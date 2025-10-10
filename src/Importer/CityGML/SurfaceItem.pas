unit SurfaceItem;

interface

uses
    System.Generics.Collections,
    Xml.XMLIntf,
    F8Utils,
    F8OpenGL,
    SurfaceTriangle,
    PedestrianUtil;

type
    ///    SurfaceMember
    ///    要素としてgml:LinearRingの以下の定義に基づく3次元空間上の座標情報を読込む
    ///    > 4点以上の順列から構成され、始点と終点が一致する。
    ///    > gml:LinearRingを構成する全ての点は、始点と終点を除き、一致しない。
    ///    > 自己交差しない。
    ///    全頂点の高さの平均を計算してp_AvgElevationに保存する
    TSurfaceItem = class
        private
            const
                NODE_NAME_SURFACE_ITEM  = 'SurfaceItem';
                NODE_NAME_POINTS        = 'Points';
            var
                p_Points: TPoint3DListType;
                p_Triangles: TObjectList<TSurfaceTriangle>;
                p_AvgElevation: Double;

            function  GetPoint(const aIdx: Integer): TPoint3D;
            function  GetPointCount: Integer;
            function  GetEdge(const aIdx: Integer): TParametricLine2D;
            function  GetEdgeCount: Integer;
            function  GetSurfaceTriangle(const aIdx: Integer): TSurfaceTriangle;
            function  GetSurfaceTriangleCount: Integer;

            constructor Create; overload;
        public
            constructor Create(const aPoints: TPoint3DListType); overload;
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function  Clone: TSurfaceItem;
            class function  ImportFromPluginData(const aDataNode: IXMLNode; out aRes: TSurfaceItem): Boolean; static;
            procedure ExportToPluginData(const aParentNode: IXMLNode);

            function  IsOverlappedSurface(const aRect: VertexPositions): Boolean;
            procedure DoRender(const aOpenGL: TF8OpenGL; const aColor: TPoint3D);

            property  Point[const aIdx: Integer]          : TPoint3D          read GetPoint;
            property  PointCount                          : Integer           read GetPointCount;
            property  Edge[const aIdx: Integer]           : TParametricLine2D read GetEdge;
            property  EdgeCount                           : Integer           read GetEdgeCount;
            property  AvgElevation                        : Double            read p_AvgElevation;
            property  SurfaceTriangle[const aIdx: Integer]: TSurfaceTriangle  read GetSurfaceTriangle;
            property  SurfaceTriangleCount                : Integer           read GetSurfaceTriangleCount;
        end;

implementation

uses
    Winapi.Windows,
    System.SysUtils,
    System.Math,
    SurfaceTriangleGenerator,
    XMLPointType,
    LatLonHelper;

{ TSurfaceItem }
///    以下のチェックを行う
///    - 引数の最初の頂点と最後の頂点が一致: 当てはまらない場合、p_Pointsの長さを(引数の長さ+1)にして、配列の末尾に引数の最初の頂点を格納する
///    - p_Pointsの長さが4以上: アサーションチェックする
constructor TSurfaceItem.Create(const aPoints: TPoint3DListType);
    function  IsFirstAndLastPointsInTheSamePosition: Boolean;
        begin
        Result := aPoints[0] = aPoints[Length(aPoints) - 1];
        end;
    var
        i: Integer;
        addLastFlag: Boolean;
        sumOfElevation: Double;
        generator: TSurfaceTriangleGenerator;
    begin
    p_Triangles := TObjectList<TSurfaceTriangle>.Create;
    addLastFlag := IsFirstAndLastPointsInTheSamePosition;
    if addLastFlag then
        SetLength(p_Points, Length(aPoints))
    else
        SetLength(p_Points, Length(aPoints) + 1);

    Assert(Length(p_Points) >= 4);

    sumOfElevation := 0.0;
    for i := 0 to Length(p_Points) - 1 do
        begin
        if (i = Length(p_Points) - 1) then
            begin
            if addLastFlag then
                p_Points[i] := aPoints[0]
            else
                p_Points[i] := aPoints[i];
            end
        else
            begin
            p_Points[i] := aPoints[i];
            sumOfElevation := sumOfElevation + p_Points[i].Height; // 最後の頂点(=最初の頂点)以外の高さの合計
            end;
        end;
    p_AvgElevation := sumOfElevation / (Length(p_Points) - 1);

    generator := TSurfaceTriangleGenerator.Create;
    try
        generator.Generate(p_Points, p_Triangles);
    finally
        FreeAndNil(generator);
        end;
    end;

constructor TSurfaceItem.Create;
    begin
    SetLength(p_Points, 0);
    p_Triangles := TObjectList<TSurfaceTriangle>.Create;
    end;

procedure TSurfaceItem.AfterConstruction;
    begin
    inherited;
    {no action}
    end;

procedure TSurfaceItem.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_Triangles);
    end;

function TSurfaceItem.Clone: TSurfaceItem;
    var
        i: Integer;
    begin
    Result := TSurfaceItem.Create;
    SetLength(Result.p_Points, PointCount);
    CopyMemory(Result.p_Points, p_Points, SizeOf(TPoint3D) * PointCount);
    Result.p_AvgElevation := p_AvgElevation;
    if p_Triangles.Count > 0 then
        begin
        for i := 0 to p_Triangles.Count - 1 do
            Result.p_Triangles.Add(p_Triangles[i].Clone);
        end;
    end;

class function TSurfaceItem.ImportFromPluginData(const aDataNode: IXMLNode; out aRes: TSurfaceItem): Boolean;
    var
        headNode, pointsNode: IXMLNode;
        tmpPoints: TPoint3DListType;
    begin
    Result := False;

    headNode := aDataNode.ChildNodes.FindNode(NODE_NAME_SURFACE_ITEM);
    if not Assigned(headNode) then
        Exit;

    pointsNode := headNode.ChildNodes.FindNode(NODE_NAME_POINTS);
    if not (Assigned(pointsNode) and TPoint3DListXMLType.ImportFromXML(pointsNode, tmpPoints)) then
        Exit;

    aRes := TSurfaceItem.Create(tmpPoints);
    Result := True;
    end;

procedure TSurfaceItem.ExportToPluginData(const aParentNode: IXMLNode);
    var
        headNode: IXMLNode;
    begin
    headNode := aParentNode.AddChild(NODE_NAME_SURFACE_ITEM);
    TPoint3DListXMLType.ExportToXML(p_Points, NODE_NAME_POINTS, headNode);
    end;

///    aRectとp_Pointsの範囲が重なっているか調べる
///    主にCellと重なっているか調べるために使用する
function TSurfaceItem.IsOverlappedSurface(const aRect: VertexPositions): Boolean;
    var
        triangle: TSurfaceTriangle;
    begin
    Result := False;
    for triangle in p_Triangles do
        Result := Result or triangle.IsOverlappedTriangle(aRect);
    end;

procedure TSurfaceItem.DoRender(const aOpenGL: TF8OpenGL; const aColor: TPoint3D);
    var
        item: TSurfaceTriangle;
    begin
    for item in p_Triangles do
        item.DoRender(aOpenGL, aColor);
    end;

function TSurfaceItem.GetPoint(const aIdx: Integer): TPoint3D;
    begin
    Assert(InRange(aIdx, 0, PointCount - 1));
    Result := p_Points[aIdx];
    end;

function TSurfaceItem.GetPointCount: Integer;
    begin
    Result := Length(p_Points);
    end;

///    最初の頂点と最後の頂点の位置が同じであるため、辺の数は頂点数 - 1
///    辺aIdxは頂点[aIdx, aIdx + 1]からなる
function TSurfaceItem.GetEdge(const aIdx: Integer): TParametricLine2D;
    begin
    Assert(InRange(aIdx, 0, EdgeCount - 1));
    Result.a := p_Points[aIdx];
    Result.b := p_Points[aIdx + 1];
    end;

function TSurfaceItem.GetEdgeCount: Integer;
    begin
    Result := PointCount - 1;
    end;

function TSurfaceItem.GetSurfaceTriangle(const aIdx: Integer): TSurfaceTriangle;
    begin
    Assert(InRange(aIdx, 0, SurfaceTriangleCount - 1));
    Result := p_Triangles[aIdx];
    end;

function TSurfaceItem.GetSurfaceTriangleCount: Integer;
    begin
    Result := p_Triangles.Count;
    end;
end.
