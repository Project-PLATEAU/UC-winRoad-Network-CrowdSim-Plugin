unit MultiSurface;

interface

uses
    System.Generics.Collections,
    Xml.XMLIntf,
    geometryAggregates,
    F8OpenGL,
    F8Utils,
    SurfaceItem,
    PedestrianUtil;

type
    TMultiSurface = class
        private
            p_Surfaces: TObjectList<TSurfaceItem>;
            p_Loaded: Boolean;// True: データを読み込んだ

            function  GetSurface(const aIdx: Integer): TSurfaceItem;
            function  GetSurfaceCount: Integer;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure AssignFrom(const aResource: TMultiSurface);
            function  Clone: TMultiSurface;
            function  ImportCityGML(const aInputCityGML: IXMLMultiSurfacePropertyType): Boolean;  overload;
            function  ImportCityGML(const aInputCityGML: IXMLMultiSurfaceType): Boolean; overload;
            function  ImportFromPluginData(const aDataNode: IXMLNode): Boolean;
            procedure ExportToPluginData(const aParentNode: IXMLNode);

            function  GenerateOutLine: TParametricLineList;
            procedure DoRender(const aOpenGL: TF8OpenGL); overload;
            procedure DoRender(const aOpenGL: TF8OpenGL; const aColor: TPoint3D); overload;

            function  IsOverlappedMultiSurface(const aRect: VertexPositions): Boolean;

            property  Surface[const aIdx: Integer]: TSurfaceItem read GetSurface;
            property  SurfaceCount                : Integer      read GetSurfaceCount;
            property  Loaded                      : Boolean      read p_Loaded;
        end;

implementation

uses
    System.SysUtils,
    System.Classes,
    System.Math,
    geometryBasic2d,
    ParametricLine2DHelper,
    LatLonHelper;

const
    TAG_SURFACE_MEMBER_COUNT      = 'SurfaceMemberCount';
    NODE_NAME_SURFACE_MEMBER_LIST = 'SurfaceMemberList';
    NODE_NAME_SURFACE_MEMBER_ITEM = 'SurfaceMemberItem_';

/// aPointTextはスペース区切りの数値の羅列を想定している。
/// 数値は3つで一つの頂点を表し、(Lat, Lon, Height)の順になっている
/// イメージ: point1.Lat point1.Lon point1.Height point2.Lat ...
function ConvertTextToPointList(const aPointText: String): TPoint3DListType;
    var
        strList: TStringList;
        tmpList: TList<TPoint3D>;
        point: TPoint3D;
        i: Integer;
    begin
    strList := TStringList.Create;
    tmpList := TList<TPoint3D>.Create;
    try
        strList.CommaText := aPointText;
        Assert((strList.Count mod 3) = 0);
        for i := 0 to strList.Count - 1 do
            begin
            case i mod 3 of
                0: point.Lat := strToFloatDef(strList[i], 0.0);
                1: point.Lon := strToFloatDef(strList[i], 0.0);
                2:
                    begin
                    point.Height := strToFloatDef(strList[i], 0.0);
                    tmpList.Add(point);
                    end;
                end;
            end;
        SetLength(Result, tmpList.Count);
        for i := 0 to tmpList.Count - 1 do
            Result[i] := tmpList[i];
    finally
        FreeAndNil(strList);
        FreeAndNil(tmpList);
        end;
    end;

{ TMultiSurface }
procedure TMultiSurface.AfterConstruction;
    begin
    inherited;

    p_Surfaces   := TObjectList<TSurfaceItem>.Create;
    p_Loaded := False;
    end;

procedure TMultiSurface.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_Surfaces);
    end;

procedure TMultiSurface.AssignFrom(const aResource: TMultiSurface);
    var
        item: TSurfaceItem;
    begin
    p_Surfaces.Clear;

    for item in aResource.p_Surfaces do
        p_Surfaces.Add(item.Clone);

    p_Loaded := aResource.Loaded;
    end;

function TMultiSurface.Clone: TMultiSurface;
    var
        item: TSurfaceItem;
    begin
    Result := TMultiSurface.Create;
    for item in p_Surfaces do
        Result.p_Surfaces.Add(item.Clone);

    Result.p_Loaded := p_Loaded;
    end;

function TMultiSurface.ImportCityGML(const aInputCityGML: IXMLMultiSurfacePropertyType): Boolean;
    var
        multiSurface: IXMLMultiSurfaceType;
    begin
    Result := False;

    p_Surfaces.Clear;
    p_Loaded := False;

    if not Assigned(aInputCityGML) then
        Exit;
    multiSurface := aInputCityGML.MultiSurface;
    if not Assigned(multiSurface) then
        Exit;

    Result := ImportCityGML(multiSurface);
    end;

function TMultiSurface.ImportCityGML(const aInputCityGML: IXMLMultiSurfaceType): Boolean;
    var
        i: Integer;
        multiSurfaceMem: IXMLSurfacePropertyTypeList;
        pol: IXMLPolygonType;
        lil: IXMLLinearRingType;
        pointText: String;
    begin
    Result := False;

    multiSurfaceMem := aInputCityGML.SurfaceMember;
    if not Assigned(multiSurfaceMem) then
        Exit;

    for i := 0 to multiSurfaceMem.Count - 1 do
        begin
        if not Supports(multiSurfaceMem[i]._Surface, IXMLPolygonType, pol) then
            Continue;

        if not Supports(pol.Exterior._Ring, IXMLLinearRingType, lil) then
            Continue;

        if not lil.PosList.IsTextElement then
            Continue;

        pointText := lil.PosList.Text;
        p_Surfaces.Add(TSurfaceItem.Create(ConvertTextToPointList(pointText)));
        end;

    p_Loaded := (SurfaceCount >= 1);
    Result := p_Loaded;
    end;

function TMultiSurface.ImportFromPluginData(const aDataNode: IXMLNode): Boolean;
    var
        headNode, itemNode: IXMLNode;
        i, itemCount: Integer;
        surfaceItem: TSurfaceItem;
    begin
    p_Surfaces.Clear;
    p_Loaded := False;
    Result := False;

    headNode := aDataNode.ChildNodes.FindNode(NODE_NAME_SURFACE_MEMBER_LIST);
    if not Assigned(headNode) then
        Exit;

    if not headNode.HasAttribute(TAG_SURFACE_MEMBER_COUNT) then
        Exit;

    itemCount := StrToIntDef(headNode.Attributes[TAG_SURFACE_MEMBER_COUNT], 0);
    if headNode.ChildNodes.Count <> itemCount then
        Exit;

    for i := 0 to headNode.ChildNodes.Count - 1 do
        begin
        itemNode := headNode.ChildNodes.FindNode(Format('%s%d', [NODE_NAME_SURFACE_MEMBER_ITEM, i]));
        if not Assigned(itemNode) then
            Break;

        if TSurfaceItem.ImportFromPluginData(itemNode, surfaceItem) then
            p_Surfaces.Add(surfaceItem);
        end;

    p_Loaded := (p_Surfaces.Count > 1);
    Result := p_Loaded;
    end;

procedure TMultiSurface.ExportToPluginData(const aParentNode: IXMLNode);
    var
        headNode, itemNode: IXMLNode;
        i: Integer;
    begin
    headNode := aParentNode.AddChild(NODE_NAME_SURFACE_MEMBER_LIST);
    headNode.Attributes[TAG_SURFACE_MEMBER_COUNT] := SurfaceCount;
    for i := 0 to p_Surfaces.Count - 1 do
        begin
        itemNode := headNode.AddChild(Format('%s%d',[NODE_NAME_SURFACE_MEMBER_ITEM, i]));
        p_Surfaces[i].ExportToPluginData(itemNode);
        end;
    end;

///    全Surfaceをつなげた図形の外形線を出力する
///    Surface形状によっては正しく出力できない&用途によっては不要なので
///    必要な箇所で関数を呼び出し、結果を使用、保持出来るようにする
function TMultiSurface.GenerateOutLine: TParametricLineList;
    var
        i: Integer;
        idx, duplicateIdx, resIdx: Integer;
        edgeTotal: Integer;
        item: TSurfaceItem;
        edge: TParametricLine2D;
        tmpItemList, tmpDuplicateItemList: TParametricLineList;
    begin
    if SurfaceCount = 1 then
        begin
        SetLength(Result, Surface[0].EdgeCount);
        for i := 0 to Surface[0].EdgeCount - 1 do
            Result[i] := Surface[0].Edge[0];
        end
    else
        begin
        // この処理は道路LOD3の場合を想定して作成した
        edgeTotal := 0;
        for item in p_Surfaces do
            edgeTotal := edgeTotal + item.EdgeCount;

        SetLength(tmpItemList, edgeTotal);
        SetLength(tmpDuplicateItemList, (edgeTotal div 2) + 1);

        idx := 0;
        duplicateIdx := 0;
        for item in p_Surfaces do
            begin
            for i := 0 to item.EdgeCount - 1 do
                begin
                edge := item.Edge[i];
                if not edge.CheckContains(tmpItemList, idx) then
                    begin
                    tmpItemList[idx] := edge;
                    Inc(idx);
                    end
                else
                    begin
                    if not edge.CheckContains(tmpDuplicateItemList, duplicateIdx) then
                        begin
                        tmpDuplicateItemList[duplicateIdx] := edge;
                        Inc(duplicateIdx);
                        end;
                    end;
                end;
            end;

        SetLength(tmpDuplicateItemList, duplicateIdx);
        SetLength(tmpItemList, idx);
        SetLength(Result, idx);

        resIdx := 0;
        for i := 0 to idx - 1 do
            begin
            edge := tmpItemList[i];
            if edge.CheckContains(tmpDuplicateItemList, duplicateIdx) then
                Continue;

            Result[resIdx] := edge;
            Inc(resIdx);
            end;

        SetLength(Result, resIdx);
        end;
    end;

procedure TMultiSurface.DoRender(const aOpenGL: TF8OpenGL);
    begin
    DoRender(aOpenGL, Point3D(1.0, 0.5, 0.8));
    end;

procedure TMultiSurface.DoRender(const aOpenGL: TF8OpenGL; const aColor: TPoint3D);
    var
        item: TSurfaceItem;
    begin
    for item in p_Surfaces do
        item.DoRender(aOpenGL, aColor);
    end;

function TMultiSurface.IsOverlappedMultiSurface(const aRect: VertexPositions): Boolean;
    var
        item: TSurfaceItem;
    begin
    Result := False;
    for item in p_Surfaces do
        begin
        if item.IsOverlappedSurface(aRect) then
            begin
            Result := True;
            Break;
            end;
        end;
    end;

function TMultiSurface.GetSurface(const aIdx: Integer): TSurfaceItem;
    begin
    Assert(InRange(aIdx, 0, SurfaceCount - 1));
    Result := p_Surfaces[aIdx];
    end;

function TMultiSurface.GetSurfaceCount: Integer;
    begin
    Result := p_Surfaces.Count;
    end;
end.
