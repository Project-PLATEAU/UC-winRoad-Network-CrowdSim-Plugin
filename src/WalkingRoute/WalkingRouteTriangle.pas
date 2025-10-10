unit WalkingRouteTriangle;

interface

uses
    F8Utils,
    SurfaceTriangle;

type
    ///    三角形同士の辺ごとの状態
    ///    IsTerminateEdge: この辺は終端であるか(つまり、他のどの三角形とも辺を共有していない) True: 終端 False: 終端でない 初期値: True
    ///    hasObstacleEdge: この辺の上に障害物(内壁など)があるか。Trueの場合は辺の内側から外側に通過できない 初期値: False
    ///    (IsTerminateEdge or hasObstacleEgde) = Trueの場合その辺にノードを設置することはできない
    TWalkerNodeTriangle = class
        private
            p_Triangle: TSurfaceTriangle;
            p_IsTerminateEgde: array[0..2] of Boolean;
            p_HasObstacleEdge: array[0..2] of Boolean;

            function  GetIsTerminateEdge(const aIdx: Integer): Boolean;
            function  GetHasObstacleEdge(const aIdx: Integer): Boolean;
            function  GetWalkableEdge(const aIdx: Integer): Boolean;
            function  GetEdgeGL(const aIdx: Integer): TParametricLine2D;
        public
            constructor Create(const aTriangle: TSurfaceTriangle);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function  Clone: TWalkerNodeTriangle;

            function  CheckEdgeConnected(const aTriangle: TWalkerNodeTriangle): Boolean;
            procedure CheckObstacleEdgeConnected(const aObstacleTriangle: TSurfaceTriangle);

            procedure MakeRoute;

            property  IsTerminateEdge[const aIdx: Integer]: Boolean           read GetIsterminateEdge;
            property  HasObstacleEdge[const aIdx: Integer]: Boolean           read GetHasObstacleEdge;
            property  WalkableEdge[const aIdx: Integer]   : Boolean           read GetWalkableEdge;
            property  EdgeGL[const aIdx: Integer]         : TParametricLine2D read GetEdgeGL;
        end;


implementation

uses
    System.SysUtils,
    PluginCore,
    F8GLUtils,
    ParametricLine2DHelper;

constructor TWalkerNodeTriangle.Create(const aTriangle: TSurfaceTriangle);
    begin
    p_Triangle := aTriangle.Clone;
    end;

procedure TWalkerNodeTriangle.AfterConstruction;
    var
        i: Integer;
    begin
    inherited;
    for i := 0 to 2 do
        begin
        p_IsTerminateEgde[i] := True;
        p_HasObstacleEdge[i] := False;
        end;
    end;

procedure TWalkerNodeTriangle.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_Triangle);
    end;

function TWalkerNodeTriangle.Clone: TWalkerNodeTriangle;
    var
        i: Integer;
    begin
    Result := TWalkerNodeTriangle.Create(p_Triangle);
    for i := 0 to 2 do
        begin
        Result.p_IsTerminateEgde[i] := p_IsTerminateEgde[i];
        Result.p_HasObstacleEdge[i] := p_HasObstacleEdge[i];
        end;
    end;

function TWalkerNodeTriangle.CheckEdgeConnected(const aTriangle: TWalkerNodeTriangle): Boolean;
    var
        i, j: Integer;
    begin
    Result := False;
    for i := 0 to 2 do
        begin
        // 既にほかの辺と結合している場合はスキップ
        if not p_IsTerminateEgde[i] then
            Continue;

        for j := 0 to 2 do
            begin
            if p_Triangle.EdgeGL[i].IsMatch(aTriangle.p_Triangle.EdgeGL[j]) then
                begin
                p_IsTerminateEgde[i] := False;
                aTriangle.p_IsTerminateEgde[j] := False;
                Result := True;
                Break;
                end;
            end;
        end;
    end;

procedure TWalkerNodeTriangle.CheckObstacleEdgeConnected(const aObstacleTriangle: TSurfaceTriangle);
    var
        i, j: Integer;
    begin
    for i := 0 to 2 do
        begin
        if p_HasObstacleEdge[i] then
            Continue;

        for j := 0 to 2 do
            begin
            if p_Triangle.EdgeGL[i].IsMatch(aObstacleTriangle.EdgeGL[j]) then
                begin
                p_HasObstacleEdge[i] := True;
                Break;
                end;
            end;
        end;
    end;

procedure TWalkerNodeTriangle.MakeRoute;
    function ToGLPoint(const aPoint: TPoint3D): GLPointType;
        begin
        Result := AsGLPointType(aPoint.X, aPoint.Y, aPoint.Z);
        end;
    // これは実験用コード
    procedure MakeLinearPath(const a, b: TParametricLine2D);
        begin
        theApplicationServices.project.FlightWayNwk[1].AddLinearPath(ToGLPoint(a.Center), ToGLPoint(b.Center));
        end;
    var
        i: Integer;
    begin
    for i := 0 to 2 do
        begin
        if WalkableEdge[i] and WalkableEdge[(i + 1) mod 3] then
            MakeLinearPath(p_Triangle.EdgeGL[i], p_Triangle.EdgeGL[(i + 1) mod 3]);
        end;
    end;

function TWalkerNodeTriangle.GetIsTerminateEdge(const aIdx: Integer): Boolean;
    begin
    Assert((aIdx >= 0) and (aIdx <= 2));
    Result := p_IsTerminateEgde[aIdx];
    end;

function TWalkerNodeTriangle.GetHasObstacleEdge(const aIdx: Integer): Boolean;
    begin
    Assert((aIdx >= 0) and (aIdx <= 2));
    Result := p_HasObstacleEdge[aIdx];
    end;

function TWalkerNodeTriangle.GetWalkableEdge(const aIdx: Integer): Boolean;
    begin
    Assert((aIdx >= 0) and (aIdx <= 2));
    Result := not (IsTerminateEdge[aIdx] or HasObstacleEdge[aIdx]);
    end;

function TWalkerNodeTriangle.GetEdgeGL(const aIdx: Integer): TParametricLine2D;
    begin
    Result := p_Triangle.EdgeGL[aIdx];
    end;
end.

