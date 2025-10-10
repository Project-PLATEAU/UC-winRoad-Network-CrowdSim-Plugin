unit SurfaceTriangleGenerator;

interface

uses
    System.Generics.Collections,
    PluginCore,
    F8Utils,
    SurfaceTriangle;

type
    ///    SurfaceItemに格納された図形を三角形に分割する
    ///    引数はTSurfaceItemのp_Pointsを想定。そのため、最初の点と最後の点の座標が一致
    ///    処理にいくつか前提条件がある
    ///    - 重複している点があると正しく動かない
    ///      -> 最後の点を使わないようにする
    ///    - 同一平面上の頂点を対象としている
    TSurfaceTriangleGenerator = class
        private
            type
                FindTriangleProc = reference to procedure(const aIndex1, aIndex2, aIndex3: Integer);

            function  DivideToTriangle(const aVertexs: TPoint3DListType; aFindProc: FindTriangleProc): Boolean;
        public
            procedure Generate(const aPoints: TPoint3DListType; aResults: TObjectList<TSurfaceTriangle>);
        end;

implementation

uses
    System.SysUtils,
    System.Math,
    LatLonHelper;

{ TSurfaceTriangleGenerator }
procedure TSurfaceTriangleGenerator.Generate(const aPoints: TPoint3DListType; aResults: TObjectList<TSurfaceTriangle>);
    var
        horiConv: IF8WrHorizontalCoordinateConvertor;

    function LatlonToGL(const aPointLatLon: TPoint3D): TPoint3D;
        var
            src, dst: F8PointType;
        begin
        src[_x] := aPointLatLon.Lon;
        src[_y] := aPointLatLon.Lat;

        horiConv.Convert(_hctSpecifiedCS, 6668, _hctOpenGL_XZ, 0, src, dst);

        Result.X := dst[_x];
        Result.Y := aPointLatLon.Height;
        Result.Z := dst[_y];
        end;
    var
        i: Integer;
        pointGLs: TPoint3DListType;
    begin
    horiConv := theApplicationServices.GetWRCoordinateConvertor.HoirizontalCSConvertor;
    SetLength(pointGLs, Length(aPoints) - 1);

    // 頂点数が3の場合、分割処理をスキップする
    if Length(pointGLs) = 3 then
        begin
        for i := 0 to Length(pointGLs) - 1 do
            pointGLs[i] := LatlonToGL(aPoints[i]);

        aResults.Add(TSurfaceTriangle.Create(aPoints[0], aPoints[1], aPoints[2], pointGLs[0], pointGLs[1], pointGLs[2]));
        Exit;
        end;

    for i := 0 to Length(pointGLs) - 1 do
        pointGLs[i] := LatlonToGL(aPoints[i]);

    DivideToTriangle(pointGLs, procedure(const aIndex1, aIndex2, aIndex3: Integer)
        begin
        aResults.Add(TSurfaceTriangle.Create(aPoints[aIndex1], aPoints[aIndex2], aPoints[aIndex3],
                                             pointGLs[aIndex1], pointGLs[aIndex2], pointGLs[aIndex3]));

        end);
    end;

/// 三角形に分割する
function TSurfaceTriangleGenerator.DivideToTriangle(const aVertexs: TPoint3DListType; aFindProc: FindTriangleProc): Boolean;
    var
        positionTable: TDictionary<Integer, Boolean>;
        currentIndex, previousIndex, nextIndex: Integer;
        preDirection: TPoint3D;
        isIncludingIndex: Boolean;

    // ある頂点の次の頂点を探す
    // 頂点は順に並んでいる想定なので、配列の中からaStartIndexより後に始めに現れる未探索頂点を探す
    // 頂点が終端を過ぎたら最初に戻る
    function FindNextIndex(const aStartIndex: Integer): Integer;
        var
            count: Integer;
        begin
        Result := aStartIndex;
        count := Length(aVertexs);
        while True do
            begin
            Result := (Result + 1) mod count;
            if not positionTable[Result] then
                Break;
            end;
        end;

    // ある頂点より前の頂点を探す
    // FindNextIndexの探索方向が逆のバージョン
    function FindPreviousIndex(aStartIndex: Integer): Integer;
        var
            count: Integer;
        begin
        Result := aStartIndex;
        count := Length(aVertexs);
        while True do
            begin
            Result := IfThen((Result - 1) >= 0, Result - 1, count - 1);
            if not positionTable[Result] then
                Break;
            end;
        end;

    // 適当な1点から最も遠い未探索頂点を探す
    // 計算では原点(ZERO_POINT3D)から最も遠い点を探している
    procedure FindFarPoint;
        var
            key: Integer;
            farIndex: Integer;
            maxDist, dist: Double;
        begin
        farIndex := -1;
        maxDist := MinDouble;
        for key in positionTable.Keys do
            begin
            if positionTable[key] then
                Continue;

            dist := aVertexs[key].DistanceTo(ZERO_POINT3D);
            if (dist > maxDist) then
                begin
                maxDist := dist;
                farIndex := key;
                end;
            end;

        currentIndex := farIndex;
        nextIndex :=  FindNextIndex(farIndex);
        previousIndex := FindPreviousIndex(farIndex);
        end;

    // 引数で渡した点がcurrentIndex, nextIndex, previousIndexで定義した三角形の中にあるか調べる
    function CheckInPoint(const aPos: TPoint3D): Boolean;
        var
            tp: TPoint3DlistType;
            n1, n2, n3: TPoint3D;
            dot12, dot13: Double;
        begin
        Setlength(tp, 3);
        tp[0] := aVertexs[currentIndex];
        tp[1] := aVertexs[nextIndex];
        tp[2] := aVertexs[previousIndex];

        n1 := CrossProduct(tp[1] - tp[0], aPos - tp[1]).Normalise;
        n2 := CrossProduct(tp[2] - tp[1], aPos - tp[2]).Normalise;
        n3 := CrossProduct(tp[0] - tp[2], aPos - tp[0]).Normalise;

        dot12 := DotProduct(n1, n2);
        dot13 := DotProduct(n1, n3);

        Result := (dot12 > 0) and (dot13 > 0);
        end;

    // currentIndex, nextIndex, previousIndexで定義した三角形の中に他の頂点がないか調べる
    function IsIncludePoint: Boolean;
        var
            key: Integer;
        begin
        Result := False;
        for key in positionTable.Keys do
            begin
            if positionTable[key] then
                Continue;

            if (key = currentIndex) or (key = nextIndex) or (key = previousIndex) then
                Continue;

            if CheckInPoint(aVertexs[key]) then
                begin
                Result := True;
                Break;
                end;
            end;
        end;
    // 三角形が多角形の中にあるかチェックするため、現在の三角形の向きを外積で記録する
    // FindFarPointで作成した三角形は多角形の中にあるが、MoveToNextで作成した三角形はそうでないことがある
    function GetCurrentDirection: TPoint3D;
        var
            edge1, edge2: TPoint3D;
        begin
        edge1 := (aVertexs[nextIndex] - aVertexs[currentIndex]).Normalise;
        edge2 := (aVertexs[previousIndex] - aVertexs[currentIndex]).Normalise;
        Result := CrossProduct(edge2, edge1);
        end;

    // インデックスを一つずらした三角形を得る
    procedure MoveToNext;
        begin
        currentIndex  := FindNextIndex(currentIndex);
        nextIndex     := FindNextIndex(currentIndex);
        previousIndex := FindPreviousIndex(currentIndex);
        end;

    // 未探索頂点から新たに三角形を作成する
    function DetectTriangle: Boolean;
        var
            newDirection: TPoint3D;
        begin
        Result := False;
        if not isIncludingIndex then
            FindFarPoint;

        if IsIncludePoint then
            begin
            preDirection := GetCurrentDirection;
            isIncludingIndex := True;
            MoveToNext;
            Exit;
            end;

        // isIncludingIndexが有効な場合、FindFarPointで作成した三角形ではないので向きを確認する必要がある
        if isIncludingIndex then
            begin
            newDirection := GetCurrentDirection;
            if not (DotProduct(newDirection, preDirection) > 0.0) then
                begin
                MoveToNext;
                Exit;
                end;
            end;

        // 三角形が見つかった
        isIncludingIndex := False;

        positionTable[currentIndex] := True;
        Result := True;
        end;
    var
        i: Integer;
        unChecked: Integer;
        value: Boolean;
    begin
    // パラメータを初期化
    positionTable := TDictionary<Integer, Boolean>.Create;
    isIncludingIndex := False;
    preDirection := ZERO_POINT3D;
    try
        for i := 0 to Length(aVertexs) - 1 do
            positionTable.Add(i, False);

        while True do
            begin
            // 未処理の頂点を探索
            unChecked := 0;
            for value in positionTable.Values do
                begin
                if not value then
                    Inc(unChecked);
                end;

            // 未処理の頂点が3個未満: ループを抜ける
            if unChecked < 3 then
                Break;

            if DetectTriangle then
                aFindProc(currentIndex, nextIndex, previousIndex);
            end;
    finally
        FreeAndNil(positionTable);
        end;

    Result := True;
    end;
end.
