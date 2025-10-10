unit TrafficArea;

interface

uses
    System.Generics.Collections,
    System.Classes,
    Xml.XMLIntf,
    F8Utils,
    F8OpenGL,
    geometryAggregates,
    MultiSurface,
    PedestrianUtil;

type
    /// <summary>
    ///    PLATEAU道路モデルの道路エリアを表すクラス
    ///    TranRoadの要素として生成・使用する
    /// </summary>
    TTrafficArea = class
        private
            p_FunctionID: Integer;
            p_MultiSurface: TMultiSurface;
            p_BoundingArea: VertexPositions;

            function  GetFunctionID: Integer;
            procedure MakeBoundingArea;
        public
            constructor Create(const aFunctionID: Integer);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure ImportSurface(const aInput: IXMLMultiSurfacePropertyType);

            function  IsOverlayArea(const aVertexPositions: VertexPositions): Boolean;
            function  IsOverlayBoundingArea(const aVertexPositions: VertexPositions): Boolean;

            function  Clone: TTrafficArea;

            procedure DoRender(const aOpenGL: TF8OpenGL);

            property  FunctionID  : Integer         read GetFunctionID;
            property  BoundingArea: VertexPositions read p_BoundingArea;
        end;

    /// <summary>
    ///    TTrafficAreaをUC-win/Roadのプロジェクトファイルに保存するためのクラス
    ///    このクラス自身はデータを持たない
    /// </summary>
    TTraffiCAreaPluginData = class
        strict private
            const
                NODE_NAME_TRAFFIC_AREA        = 'TrafficArea';
                NODE_NAME_BOUNDING_AREA       = 'BoundingArea';
                TAG_FUNCTION_ID               = 'FunctionID';
        public
            class procedure ExportToPluginData(const aTrafficArea: TTrafficArea; const aParentNode:IXMLNode);
            class function  ImportFromPluginData(const aDataNode: IXMLNode; out aRes: TTrafficArea): Boolean;
        end;

implementation

uses
    Winapi.Windows,
    System.SysUtils,
    System.Math,
    SurfaceItem,
    ParametricLine2DHelper,
    XMLPointType,
    LatLonHelper;

{TTrafficArea}
constructor TTrafficArea.Create(const aFunctionID: Integer);
    begin
    p_FunctionID := aFunctionID;
    end;

procedure TTrafficArea.AfterConstruction;
    begin
    inherited;

    p_MultiSurface := TMultiSurface.Create;
    end;

procedure TTrafficArea.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_MultiSurface);
    end;

/// <summary>
///    PLATEAU道路モデルの道路を読み込む
/// </summary>
procedure TTrafficArea.ImportSurface(const aInput: IXMLMultiSurfacePropertyType);
    begin
    p_MultiSurface.ImportCityGML(aInput);
    Assert(p_MultiSurface.Loaded);
    MakeBoundingArea;
    end;

function TTrafficArea.IsOverlayArea(const aVertexPositions: VertexPositions): Boolean;
    begin
    Result := IsOverlayBoundingArea(aVertexPositions) and p_MultiSurface.IsOverlappedMultiSurface(aVertexPositions);
    end;

function TTrafficArea.IsOverlayBoundingArea(const aVertexPositions: VertexPositions): Boolean;
    begin
    Result := IsOverlay(p_BoundingArea, aVertexPositions);
    end;

function TTrafficArea.Clone: TTrafficArea;
    var
        i: VertexPositionType;
    begin
    Result := TTrafficArea.Create(FunctionID);
    Result.p_MultiSurface.AssignFrom(p_MultiSurface);
    for i := Low(VertexPositionType) to High(VertexPositionType) do
        Result.p_BoundingArea[i] := p_BoundingArea[i];
    end;

procedure TTrafficArea.DoRender(const aOpenGL: TF8OpenGL);
    begin
    p_MultiSurface.DoRender(aOpenGL);
    end;

function TTrafficArea.GetFunctionID: Integer;
    begin
    Result := p_FunctionID;
    end;

procedure TTrafficArea.MakeBoundingArea;
    var
        edgeList: TParametricLineList;
        minLatLon, maxLatLon: TPoint3D;
        i: Integer;
        len: Integer;
    begin
    if not Assigned(p_MultiSurface) then
        Exit;

    edgeList := p_MultiSurface.GenerateOutLine;

    len := Length(edgeList);
    if len <= 0 then
        Exit;

    minLatLon := edgeList[0].a;
    maxLatLon := edgeList[0].a;

    for i := 0 to len - 1 do
        begin
        minLatLon.Lat := Min(minLatLon.Lat, Min(edgeList[i].a.Lat, edgeList[i].b.Lat));
        minLatLon.Lon := Min(minLatLon.Lon, Min(edgeList[i].a.Lon, edgeList[i].b.Lon));
        maxLatLon.Lat := Max(maxLatLon.Lat, Max(edgeList[i].a.Lat, edgeList[i].b.Lat));
        maxLatLon.Lon := Max(maxLatLon.Lon, Max(edgeList[i].a.Lon, edgeList[i].b.Lon));
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

{ TTraffiCAreaPluginData }
class procedure TTraffiCAreaPluginData.ExportToPluginData(const aTrafficArea: TTrafficArea; const aParentNode: IXMLNode);
    var
        headNode: IXMLNode;
    begin
    headNode := aParentNode.AddChild(NODE_NAME_TRAFFIC_AREA);
    headNode.Attributes[TAG_FUNCTION_ID] := aTrafficArea.p_FunctionID.ToString;
    aTrafficArea.p_MultiSurface.ExportToPluginData(headNode);
    TVertexPositionsXMLType.ExportToXML(aTrafficArea.p_BoundingArea, NODE_NAME_BOUNDING_AREA, headNode);
    end;

class function TTraffiCAreaPluginData.ImportFromPluginData(const aDataNode: IXMLNode; out aRes: TTrafficArea): Boolean;
    var
        headNode, BoundingNode: IXMLNode;
        tmpFunctionID: Integer;
        tmpBoundingArea: VertexPositions;
    begin
    Result := False;
    headNode := aDataNode.ChildNodes.FindNode(NODE_NAME_TRAFFIC_AREA);
    if not Assigned(headNode) then
        Exit;

    if not headNode.HasAttribute(TAG_FUNCTION_ID) then
        Exit;

    tmpFunctionID := StrToIntDef(headNode.Attributes[TAG_FUNCTION_ID], 0);

    boundingNode := headNOde.ChildNodes.FindNode(NODE_NAME_BOUNDING_AREA);
    if not Assigned(boundingNode) then
        Exit;

    if not TVertexPositionsXMLType.ImportFromXML(boundingNode, tmpBoundingArea) then
        Exit;

    aRes := TTrafficArea.Create(tmpFunctionID);
    aRes.p_MultiSurface.ImportFromPluginData(headNode);

    aRes.p_BoundingArea := tmpBoundingArea;

    Result := True;
    end;
end.
