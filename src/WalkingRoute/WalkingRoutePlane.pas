unit WalkingRoutePlane;

interface

uses
    System.Generics.Collections,
    PluginCore,
    F8GLUtils,
    F8Utils,
    SurfaceItem,
    WalkingRouteTriangle;

type
    ///    連続歩行面
    ///    連続したTWalkingRouteTriangleの集合
    TWalkingRoutePlane = class
        private
            p_Triangles: TObjectList<TWalkerNodeTriangle>;

            procedure ConnectTriangles;
            function  CheckRoute(const aItem: TWalkingRoutePlane): Boolean;
        public
            constructor Create(const aSurfaceItem: TSurfaceItem);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure CheckObstacle(const aItem: TSurfaceItem);
            function  Combine(const aItem: TWalkingRoutePlane): Boolean;
            procedure MakeRoute;
            function  GenerateCandidateWalkingPath: TParametricLineList;
        end;

    TWalkingRoutePlanes = TObjectList<TWalkingRoutePlane>;

implementation

uses
    System.SysUtils,
    ParametricLine2DHelper;

{ TWalkingRoutePlane }
constructor TWalkingRoutePlane.Create(const aSurfaceItem: TSurfaceItem);
    var
        i: Integer;
    begin
    p_Triangles := TObjectList<TWalkerNodeTriangle>.Create;
    for i := 0 to aSurfaceItem.SurfaceTriangleCount - 1 do
        p_Triangles.Add(TWalkerNodeTriangle.Create(aSurfaceItem.SurfaceTriangle[i].Clone));
    ConnectTriangles;
    end;

procedure TWalkingRoutePlane.AfterConstruction;
    begin
    inherited;
    {no action}
    end;

procedure TWalkingRoutePlane.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_Triangles);
    end;

procedure TWalkingRoutePlane.CheckObstacle(const aItem: TSurfaceItem);
    var
        i, j :Integer;
    begin
    for i := 0 to p_Triangles.Count - 1 do
        begin
        for j := 0 to aItem.SurfaceTriangleCount - 1 do
            p_Triangles[i].CheckObstacleEdgeConnected(aItem.SurfaceTriangle[j]);
        end;
    end;

function TWalkingRoutePlane.Combine(const aItem: TWalkingRoutePlane): Boolean;
    var
        i: Integer;
    begin
    Result := CheckRoute(aItem);
    if not Result then
        Exit;

    for i := 0 to aItem.p_Triangles.Count - 1 do
        p_Triangles.Add(aItem.p_Triangles[i].Clone);
    end;

procedure TWalkingRoutePlane.MakeRoute;
    var
        i: Integer;
    begin
    for i := 0 to p_Triangles.Count - 1 do
        p_Triangles[i].MakeRoute;
    end;

function TWalkingRoutePlane.GenerateCandidateWalkingPath: TParametricLineList;
    var
        triangle: TWalkerNodeTriangle;
        i: Integer;
        idx: Integer;
    begin
    SetLength(Result, p_Triangles.Count * 3);
    idx := 0;
    for triangle in p_Triangles do
        begin
        for i := 0 to 2 do
            begin
            if triangle.WalkableEdge[i] and triangle.WalkableEdge[(i + 1) mod 3] then
                begin
                Result[idx] := ParametricLine2D(triangle.EdgeGL[i].Center, triangle.EdgeGL[(i + 1) mod 3].Center);
                Inc(idx);
                end;
            end;
        end;
    SetLength(Result, idx);
    end;

procedure TWalkingRoutePlane.ConnectTriangles;
    var
        i, j: Integer;
    begin
    for i := 0 to p_Triangles.Count - 2 do
        begin
        for j := i + 1 to p_Triangles.Count - 1 do
            begin
            p_Triangles[i].CheckEdgeConnected(p_Triangles[j]);
            end;
        end;
    end;

function TWalkingRoutePlane.CheckRoute(const aItem: TWalkingRoutePlane): Boolean;
    var
        i, j: Integer;
    begin
    Result := False;
    for i := 0 to p_Triangles.Count - 1 do
        begin
        for j := 0 to aItem.p_Triangles.Count - 1 do
            Result := Result or p_Triangles[i].CheckEdgeConnected(aItem.p_Triangles[j]);
        end;
    end;
end.

