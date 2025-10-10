unit TranRoad;

interface

uses
    System.Generics.Collections,
    Xml.XMLIntf,
    transportation,
    F8OpenGL,
    F8Utils,
    TrafficArea,
    PedestrianUtil;

type
    /// <summary>
    ///    PLATEAU道路モデルの道路を表すクラス
    ///    1つ以上の道路エリア(TTrafficArea)を持つ
    /// </summary>
    TTranRoad = class
        private
            p_RoadName: String;

            p_PedestrianArea: TObjectList<TTrafficArea>;
            p_BoundingArea: VertexPositions;

            procedure Make(const aCityRoad: IXMLTransportationComplexType);
            procedure MakeBoundingArea;
            constructor Create; overload;
        public
            constructor Create(const aCityRoad: IXMLRoadType; const aRoadName: String); overload;
            constructor Create(const aCitySquare: IXMLSquareType; const aRoadName: String); overload;
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function  Clone: TTranRoad;
            function  IsOverlayPedestrianArea(const aArea: VertexPositions): Boolean;
            function  IsOverlayBoundingArea(const aArea: VertexPositions): Boolean;
            function  IsInBoundingArea(const aPoint: TPoint3D): Boolean;

            procedure DoRender(const aOpenGL: TF8OpenGL);

            property  BoundingArea: VertexPositions read p_BoundingArea;
            property  RoadName    : String          read p_RoadName;
        end;

    /// <summary>
    ///    TTranRoadをUC-win/Roadのプロジェクトファイルに保存するためのクラス
    ///    このクラス自身はデータを持たない
    /// </summary>
    TTranRoadPluginData = class
        strict private
            const
                NODE_NAME_ROAD = 'TTranRoad';
                NODE_NAME_TRAFFICAREA_LIST = 'TTrafficAreaList';
                NODE_NAME_TRAFFICAREA_ITEM = 'TrafficAreaItem_';
                NODE_NAME_BOUNDING_AREA = 'BoundingArea';
                TAG_ROAD_NAME = 'RoadName';
                TAG_TRAFFIC_AREA_COUNT = 'TrafficAreaCount';
        public
            class procedure ExportPluginData(const aTranRoad: TTranRoad; const aParentNode:IXMLNode);
            class function  ImportPluginData(const aDataNode: IXMLNode; out aRes: TTranRoad; out aRoadName: String): Boolean;
        end;

implementation

uses
    Winapi.Windows,
    System.SysUtils,
    System.Classes,
    System.Math,
    geometryAggregates,
    geometryBasic2d,
    XMLPointType,
    LatLonHelper;

{ TTranRoad }

constructor TTranRoad.Create;
    begin
    p_PedestrianArea := TObjectList<TTrafficArea>.Create;
    end;

constructor TTranRoad.Create(const aCityRoad: IXMLRoadType; const aRoadName: String);
    begin
    p_PedestrianArea := TObjectList<TTrafficArea>.Create;
    p_RoadName := aRoadName;
    Make(aCityRoad);
    MakeBoundingArea;
    end;

constructor TTranRoad.Create(const aCitySquare: IXMLSquareType; const aRoadName: String);
    begin
    p_PedestrianArea := TObjectList<TTrafficArea>.Create;
    p_RoadName := aRoadName;
    Make(aCitySquare);
    MakeBoundingArea;
    end;

procedure TTranRoad.AfterConstruction;
    begin
    inherited;
    {no action}
    end;

procedure TTranRoad.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_PedestrianArea);
    end;

function TTranRoad.Clone: TTranRoad;
    var
        i: Integer;
    begin
    Result := TTranRoad.Create;
    for i := 0 to p_PedestrianArea.Count - 1 do
        Result.p_PedestrianArea.Add(p_PedestrianArea[i].Clone);
    Result.MakeBoundingArea;
    end;

function TTranRoad.IsOverlayPedestrianArea(const aArea: VertexPositions): Boolean;
    var
        area: TTrafficArea;
    begin
    Result := False;

    for area in p_PedestrianArea do
        begin
        if area.IsOverlayArea(aArea) then
            begin
            Result := True;
            Break;
            end;
        end;
    end;

function TTranRoad.IsOverlayBoundingArea(const aArea: VertexPositions): Boolean;
    begin
    Result := IsOverlay(BoundingArea, aArea);
    end;

function TTranRoad.IsInBoundingArea(const aPoint: TPoint3D): Boolean;
    begin
    Result := IsOverlay(aPoint, BoundingArea);
    end;

procedure TTranRoad.DoRender(const aOpenGL: TF8OpenGL);
    var
        i: Integer;
    begin
    for i := 0 to p_PedestrianArea.Count - 1 do
        p_PedestrianArea[i].DoRender(aOpenGL);
    end;

/// <summary>
///    PLATEAU都市モデルを読み込みデータを生成する
///    LOD1, LOD3の歩道モデルを読み込む
/// </summary>
procedure TTranRoad.Make(const aCityRoad: IXMLTransportationComplexType);
    const
        FUNC_PEDESTRIAN = 2000;
    procedure MakeLOD1PedestrianArea;
        var
            multiSurface: IXMLMultiSurfacePropertyType;
        begin
        multiSurface := aCityRoad.Lod1MultiSurface;
        if not Assigned(multiSurface) then
            Exit;

        p_PedestrianArea.Add(TTrafficArea.Create(FUNC_PEDESTRIAN));
        p_PedestrianArea.Last.ImportSurface(multiSurface);
        end;
    procedure MakeLOD3PedestrianArea;
        var
            i: Integer;
            traf: IXMLTrafficAreaType;
            func: Integer;
        begin
        for i := 0 to aCityRoad.TrafficArea.Count - 1 do
            begin
            traf := aCityRoad.TrafficArea.Items[i].TrafficArea;
            if not Assigned(traf) then
                Continue;

            if not Assigned(traf.Lod3MultiSurface) then
                Continue;

            // Function_.Countが1であると仮定
            if traf.Function_.Count < 1 then
                Continue;

            Assert(traf.Function_.Count = 1);
            if not traf.Function_.Items[0].IsTextElement then
                Continue;

            func := strToINtDef(traf.Function_.Items[0].Text, 0);

            if func <> FUNC_PEDESTRIAN then
                Continue;

            p_PedestrianArea.Add(TTrafficArea.Create(func));
            p_PedestrianArea.Last.ImportSurface(traf.Lod3MultiSurface);
            end;
        end;
    begin
    if aCityRoad.TrafficArea.Count < 1 then
        begin
        MakeLOD1PedestrianArea;
        end
    else
        MakeLOD3PedestrianArea;
    end;

procedure TTranRoad.MakeBoundingArea;
    var
        area: TTrafficArea;
        minLatLon, maxLatLon: TPoint3D;
    begin
    if p_PedestrianArea.Count < 1 then
        Exit;

    minLatLon := p_PedestrianArea[0].BoundingArea[VertexPositionType._LeftBottom];
    maxLatLon := p_PedestrianArea[0].BoundingArea[VertexPositionType._RightTop];

    for area in p_PedestrianArea do
        begin
        minLatLon.Lat := Min(minLatLon.Lat, area.BoundingArea[VertexPositionType._LeftBottom].Lat);
        minLatLon.Lon := Min(minLatLon.Lon, area.BoundingArea[VertexPositionType._LeftBottom].Lon);
        maxLatLon.Lat := Max(maxLatLon.Lat, area.BoundingArea[VertexPositionType._RightTop].Lat);
        maxLatLon.Lon := Max(maxLatLon.Lon, area.BoundingArea[VertexPositionType._RightTop].Lon);
        end;

    p_BoundingArea[VertexPositionType._LeftTop].Lat := maxLatLon.Lat;
    p_BoundingArea[VertexPositionType._LeftTop].Lon := minLatLon.Lon;

    p_BoundingArea[VertexPositionType._RightTop].Lat := maxLatLon.Lat;
    p_BoundingArea[VertexPositionType._RightTop].Lon := maxLatLon.Lon;

    p_BoundingArea[VertexPositionType._RightBottom].Lat := minLatLon.Lat;
    p_BoundingArea[VertexPositionType._RightBottom].Lon := maxLatLon.Lon;

    p_BoundingArea[VertexPositionType._LeftBottom].Lat := minLatLon.Lat;
    p_BoundingArea[VertexPositionType._LeftBottom].Lon := minLatLon.Lon;
    end;

{ TTranRoadPluginData }
class procedure TTranRoadPluginData.ExportPluginData(const aTranRoad: TTranRoad; const aParentNode: IXMLNode);
    var
        i: Integer;
        rootNode: IXMLNode;
        listNode: IXMLNode;
        itemNode: IXMLNode;
    begin
    if not Assigned(aTranRoad) then
        Exit;

    rootNode := aParentNode.AddChild(NODE_NAME_ROAD);
    rootNode.Attributes[TAG_ROAD_NAME] := aTranRoad.RoadName;
    rootNode.Attributes[TAG_TRAFFIC_AREA_COUNT] := aTranRoad.p_PedestrianArea.Count.ToString;
    listNode := rootNode.AddChild(NODE_NAME_TRAFFICAREA_LIST);
    for i := 0 to aTranRoad.p_PedestrianArea.Count - 1 do
        begin
        itemNode := listNode.AddChild(Format('%s%d', [NODE_NAME_TRAFFICAREA_ITEM, i]));
        TTraffiCAreaPluginData.ExportToPluginData(aTranRoad.p_PedestrianArea[i], itemNode);
        end;
    TVertexPositionsXMLType.ExportToXML(aTranRoad.p_BoundingArea, NODE_NAME_BOUNDING_AREA, rootNode);
    end;

class function TTranRoadPluginData.ImportPluginData(const aDataNode: IXMLNode; out aRes: TTranRoad; out aRoadName: String): Boolean;
    var
        rootNode, listNode, itemNode, boundingNode: IXMLNode;
        tmpTrafficAreaCount: Integer;
        i: Integer;
        traf: TTrafficArea;
        tmpBoundingArea: VertexPositions;
    begin
    Result := False;

    rootNode := aDataNode.ChildNodes.FindNode(NODE_NAME_ROAD);
    if not Assigned(rootNode) then
        Exit;

    if not rootNode.HasAttribute(TAG_ROAD_NAME) then
        Exit;

    aRoadName := rootNode.Attributes[TAG_ROAD_NAME];

    if not rootNode.HasAttribute(TAG_TRAFFIC_AREA_COUNT) then
        Exit;

    tmpTrafficAreaCount := StrToIntDef(rootNode.Attributes[TAG_TRAFFIC_AREA_COUNT], 0);

    listNode := rootNode.ChildNodes.FindNode(NODE_NAME_TRAFFICAREA_LIST);

    if (not Assigned(listNode)) and (listNode.ChildNodes.Count <> tmpTrafficAreaCount) then
        Exit;

    aRes := TTranRoad.Create;
    aRes.p_RoadName := aRoadName;

    for i := 0 to listNode.ChildNodes.Count - 1 do
        begin
        itemNode := listNode.ChildNodes.FindNode(Format('%s%d', [NODE_NAME_TRAFFICAREA_ITEM, i]));
        if TTraffiCAreaPluginData.ImportFromPluginData(itemNode, traf) then
            aRes.p_PedestrianArea.Add(traf);
        end;

    boundingNode := rootNode.ChildNodes.FindNode(NODE_NAME_BOUNDING_AREA);

    if not Assigned(boundingNode) then
        begin
        FreeAndNil(aRes);
        Exit;
        end;

    if not TVertexPositionsXMLType.ImportFromXML(boundingNode, tmpBoundingArea) then
        begin
        FreeAndNil(aRes);
        Exit;
        end;

    aRes.p_BoundingArea := tmpBoundingArea;

    Result := True;
    end;
end.

