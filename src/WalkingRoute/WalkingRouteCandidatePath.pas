unit WalkingRouteCandidatePath;

interface

uses
    System.Generics.Collections,
    F8Utils;

type
    TUnionNodePoint = class
        private
            p_AveragePoint: TPoint3D;
            p_Points: TList<TPoint3D>;

            procedure CalcAveragePoint;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure AddPoint(const aPoint: TPoint3D);
            function  IsContain(const aPoint: TPoint3D): Boolean;
            function  IsClosePoint(const aPoint: TPoint3D): Boolean;
            function  UniteNodePoint(const aPoint: TUnionNodePoint; out aNewPoint: TUnionNodePoint): Boolean;

            property  AveragePoint: TPoint3D read p_AveragePoint;
        end;

    TUnionNodePointList = class
        private
            p_InternalList: TObjectList<TunionNodePoint>;
            p_IsFixed: Boolean;

            function  GetAveragePointCount: Integer;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure AddNewPoint(const aPoint: TPoint3D);
            procedure FinalJoin;

            function  FindAveragePoint(const aPoint: Tpoint3D; out aRes: TPoint3D): Boolean;

            property  AveragePointCount: Integer read GetAveragePointCount;
            property  IsFixed          : Boolean read p_IsFixed;
        end;

    ///    UnionPathList 生成時に与えるパスのリストに次の操作を施した結果のリスト
    ///      - ノードの近接点をマージする
    ///      - ノードの近接点をマージした結果始点と終点が一致してしまったパスを削除する
    ///      - ノードの近接点をマージした結果他のパスと始点と終点が一致してしまったパスを削除する
    TCandidatePathList = class
        private
            p_NodeCount: Integer;
            p_UnionPathList: TList<TParametricLine2D>;

            function  GetUnionPath(const aIdx: Integer): TParametricLine2D;
            function  GetUnionPathCount: Integer;

            procedure UnitePath(const aPathList: TParametricLineList);

            constructor Create; overload;
        public
            constructor Create(const aPathList: TParametricLineList);overload;

            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function  MergeAndConnectList(const aList: TCandidatePathList): TCandidatePathList;

            property  UnionPath[const aIdx: Integer]: TParametricLine2D read GetUnionPath;
            property  UnionPathCount                : Integer           read GetUnionPathCount;
            property  NodeCount                     : Integer           read p_NodeCount;
       end;

    TCandidatePathListArray = TObjectList<TCandidatePathList>;

implementation

uses
    System.SysUtils,
    System.Math,
    ParametricLine2DHelper;

const
    CLOSE_ENOUGH_LENGTH = 1.1; // 十分近いと判定する距離(m)
    MAX_CONNECTING_LENGTH = 3.0; // TCandidatePathList.MergeAndConnectListでこの距離(m)以下のノードを結ぶパスを作成する
    MAX_CONNECTING_HEIGHT = 0.2; // TCandidatePathList.MergeAndConnectListノードの高さの差がこの値(m)以下の場合にパスを作成する

function ContainPath(const aList: TList<TParametricLine2D>; const aPath: TParametricLine2D): Boolean;
    var
        item: TParametricLine2D;
    begin
    Result := False;
    for item in aList do
        begin
        if item.IsMatch(aPath) then
            begin
            Result := True;
            Break;
            end;
        end;
    end;

{ TUnionNodePoint }
procedure TUnionNodePoint.AfterConstruction;
    begin
    inherited;

    p_AveragePoint := ZERO_POINT3D;
    p_Points := TList<TPoint3D>.Create;
    end;

procedure TUnionNodePoint.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_Points);
    end;

procedure TUnionNodePoint.AddPoint(const aPoint: TPoint3D);
    begin
    if not p_Points.Contains(aPoint) then
        begin
        p_Points.Add(aPoint);
        CalcAveragePoint;
        end;
    end;

function TUnionNodePoint.IsContain(const aPoint: TPoint3D): Boolean;
    begin
    Result := p_Points.Contains(aPoint);
    end;

function TUnionNodePoint.IsClosePoint(const aPoint: TPoint3D): Boolean;
    begin
    // aPointがこのグループの他の点と十分近いか、現在のAveragePointとの距離で評価する
    // True: 近い False: 遠い
    Result := True;
    if p_Points.Count = 0 then
        Exit;

    if IsContain(aPoint) then
        Exit;

    Result := AveragePoint.DistanceTo(aPoint) <= CLOSE_ENOUGH_LENGTH;
    end;

function TUnionNodePoint.UniteNodePoint(const aPoint: TUnionNodePoint; out aNewPoint: TUnionNodePoint): Boolean;
    var
        item: TPoint3D;
    begin
    // AveragePoint同士が十分近い場合は結合したaNewPointを作成する
    Result := AveragePoint.DistanceTo(aPoint.AveragePoint) <= CLOSE_ENOUGH_LENGTH;
    if Result then
        begin
        aNewPoint := TUnionNodePoint.Create;
        for item in p_Points do
            aNewPoint.p_Points.Add(item);

        for item in aPoint.p_Points do
            aNewPoint.p_Points.Add(item);

        aNewPoint.CalcAveragePoint;
        end
    else
        aNewPoint := nil;
    end;

procedure TUnionNodePoint.CalcAveragePoint;
    var
        sumPoint, item: TPoint3D;
    begin
    sumPoint := ZERO_POINT3D;
    for item in p_Points do
        sumPoint := sumPoint + item;

    p_AveragePoint := sumPoint / p_Points.Count;
    end;

{ TUnionNodePointList }
procedure TUnionNodePointList.AfterConstruction;
    begin
    inherited;

    p_InternalList := TObjectList<TunionNodePoint>.Create;
    p_IsFixed := True;
    end;

procedure TUnionNodePointList.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_InternalList);
    end;

procedure TUnionNodePointList.AddNewPoint(const aPoint: TPoint3D);
    var
        item: TUnionNodePoint;
        find: Boolean;
    begin
    if p_InternalList.Count = 0 then
        begin
        p_InternalList.Add(TUnionNodePoint.Create);
        p_InternalList.Last.AddPoint(aPoint);
        end
    else
        begin
        find := False;
        for item in p_InternalList do
            begin
            if item.IsClosePoint(aPoint) then
                begin
                item.AddPoint(aPoint);
                find := True;
                Break;
                end;
            end;

        if not find then
            begin
            p_InternalList.Add(TUnionNodePoint.Create);
            p_InternalList.Last.AddPoint(aPoint);
            end;
        end;

    p_IsFixed := False;
    end;

procedure TUnionNodePointList.FinalJoin;
    var
        i, j: Integer;
        newPoint: TUnionNodePoint;
        isRestart: Boolean;
    begin
    // UnionPoint同士を結合する
    // AddNewPointされるたびに結合判定をすると計算が重くなる可能性があるので
    // 全ての点をセットした後に結合処理を行う
    // 新しい点が追加された後にこの処理が行われているかどうか、p_IsFixedで判断できる

    if p_IsFixed then
        Exit;

    while True do
        begin
        isRestart := False;

        for i := 0 to p_InternalList.Count - 2 do
            begin
            for j := i + 1 to p_InternalList.Count - 1 do
                begin
                if p_InternalList[i].UniteNodePoint(p_InternalList[j], newPoint) then
                    begin
                    p_InternalList.Delete(j);
                    p_InternalList.Delete(i);
                    p_InternalList.Add(newPoint);
                    isRestart := True;
                    Break;
                    end;
                if isRestart then
                    Break;
                end;
            end;

        if not isRestart then
            Break;
        end;

    p_IsFixed := True;
    end;

function TUnionNodePointList.FindAveragePoint(const aPoint: Tpoint3D; out aRes: TPoint3D): Boolean;
    var
        item: TUnionNodePoint;
    begin
    // 点を追加するのではなく、リストにaPointを含むTUnionNodePointがあれば、そのTUnionNodePointのAveragePointを返却する
    Result := False;
    aRes := ZERO_POINT3D;

    if not IsFixed then
        FinalJoin;

    for item in p_InternalList do
        begin
        if item.IsContain(aPoint) then
            begin
            Result := True;
            aRes := item.AveragePoint;
            Break;
            end;
        end;
    end;

function TUnionNodePointList.GetAveragePointCount: Integer;
    begin
    Result := p_InternalList.Count;
    end;

{ TCandidatePathList }
constructor TCandidatePathList.Create;
    begin
    p_UnionPathList := TList<TParametricLine2D>.Create;
    end;

constructor TCandidatePathList.Create(const aPathList: TParametricLineList);
    begin
    p_UnionPathList := TList<TParametricLine2D>.Create;
    UnitePath(aPathList);
    end;

procedure TCandidatePathList.AfterConstruction;
    begin
    inherited;
    {no action}
    end;

procedure TCandidatePathList.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_UnionPathList);
    end;

///    この処理の結果壁や床を突き抜けたパスが生成されることがあるが、仕様とする
function TCandidatePathList.MergeAndConnectList(const aList: TCandidatePathList): TCandidatePathList;
    var
        nodeList: TUnionNodePointList;
        item, target: TParametricLine2D;
        isConnected: Boolean;

    procedure AddNewPathWithCurrentNodeList(const aNewPath: TParametricLine2D);
        var
            s, t: TPoint3D;
            newPath: TParametricLine2D;
        begin
        if nodeList.FindAveragePoint(aNewPath.a, s) and nodeList.FindAveragePoint(aNewPath.b, t) then
            begin
            if s = t then
                Exit;

            newPath := ParametricLine2D(s, t);
            if not ContainPath(Result.p_UnionPathList, newPath) then
                Result.p_UnionPathList.Add(newPath);
            end;
        end;

    procedure AddConnectionPath(const aPointA, aPointB: TPoint3D);
        var
            newPath: TParametricLine2D;
        begin
        if (aPointA.DistanceTo(aPointB) <= MAX_CONNECTING_LENGTH) and (Abs(aPointA.Y - aPointB.Y) <= MAX_CONNECTING_HEIGHT) then
            begin
            if isConnected then
                AddNewPathWithCurrentNodeList(ParametricLine2D(aPointA, aPointB))
            else
                begin
                newPath := ParametricLine2D(aPointA, aPointB);
                if not ContainPath(Result.p_UnionPathList, newPath) then
                    Result.p_UnionPathList.Add(newPath);
                end;
            end;
        end;

    begin
    Result := TCandidatePathList.Create;
    nodeList := TUnionNodePointList.Create;
    try
        for item in p_UnionPathList do
            begin
            nodeList.AddNewPoint(item.a);
            nodeList.AddNewPoint(item.b);
            end;
        for item in aList.p_UnionPathList do
            begin
            nodeList.AddNewPoint(item.a);
            nodeList.AddNewPoint(item.b);
            end;
        nodeList.FinalJoin;

        Result.p_NodeCount := nodeList.AveragePointCount;
        isConnected := Result.NodeCount <> (NodeCount + aList.NodeCount);
        if  isConnected then
            begin
            // 統合されたノードはない
            for item in p_UnionPathList do
                Result.p_UnionPathList.Add(item);
            for item in aList.p_UnionPathList do
                Result.p_UnionPathList.Add(item);
            end
        else
            begin
            // 統合されたノードがある
            for item in p_UnionPathList do
                AddNewPathWithCurrentNodeList(item);
            for item in aList.p_UnionPathList do
                AddNewPathWithCurrentNodeList(item);
            end;

        for item in p_UnionPathList do
            begin
            for target in aList.p_UnionPathList do
                begin
                AddConnectionPath(item.a, target.a);
                AddConnectionPath(item.a, target.b);
                AddConnectionPath(item.b, target.a);
                AddConnectionpath(item.b, target.b);
                end;
            end;
    finally
        FreeAndNil(nodeList);
        end;
    end;

function TCandidatePathList.GetUnionPath(const aIdx: Integer): TParametricLine2D;
    begin
    Assert(InRange(aIdx, 0, p_UnionPathList.Count - 1));
    Result := p_UnionPathList[aIdx];
    end;

function TCandidatePathList.GetUnionPathCount: Integer;
    begin
    Result := p_UnionPathList.Count;
    end;

procedure TCandidatePathList.UnitePath(const aPathList: TParametricLineList);
    var
        nodeList: TUnionNodePointList;
        tmpList: TList<TParametricLine2D>;

        i, len: Integer;
        path, newPath: TParametricLine2D;
        s, t: TPoint3D;
    begin
    nodeList := TUnionNodePointList.Create;
    tmpList  := TList<TParametricLine2D>.Create;
    try
        len := Length(aPathList);
        for i := 0 to len - 1 do
            begin
            path := aPathList[i];
            if not ContainPath(tmpList, path) then
                begin
                tmpList.Add(path);
                nodeList.AddNewPoint(path.a);
                nodeList.AddNewPoint(path.b);
                end;
            end;

        nodeList.FinalJoin;
        p_NodeCount := nodeList.AveragePointCount;

        p_UnionPathList.Clear;
        for i := 0 to tmpList.Count - 1 do
            begin
            path := tmpList[i];
            if nodeList.FindAveragePoint(path.a, s) and nodeList.FindAveragePoint(path.b, t) then
                begin
                if  s = t then
                    Continue;

                newPath := ParametricLine2D(s, t);
                if not ContainPath(p_UnionPathList, newPath) then
                    p_UnionPathList.Add(newPath);
                end;
            end;
    finally
        FreeAndNil(nodeList);
        FreeAndNil(tmpList);
        end;
    end;
end.
