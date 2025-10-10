unit PedestrianUtil;

interface

{$SCOPEDENUMS ON}

uses
    System.Generics.Collections,
    F8Utils,
    CellID;

type
// In Japan..
//
//               North
//  _LeftTop           _RightTop
//  West               East
//  _LeftBottom        _RightBottom
//               South
//
// Lat:  North > South
// Lon:  East > West

    VertexPositionType = (
        _LeftTop,
        _RightTop,
        _RightBottom,
        _LeftBottom
        );

    VertexPositionTypeSet = set of VertexPositionType;
    VertexPositions = array[Low(VertexPositionType) .. High(VertexPositionType)] of TPoint3D;
    CornerIndex  = array[Low(VertexPositionType) .. High(VertexPositionType)] of Integer;
    CornerCellID = array[Low(VertexPositionType) .. High(VertexPositionType)] of TCellID;

    EdgePositionType = (
        _Top,
        _Right,
        _Bottom,
        _Left
        );

    EdgePositionTypeSet = set of EdgePositionType;
const
    CROSSWALK_INTERVAL_MIN = 30;
    CROSSWALK_INTERVAL_MAX = 3600;

    ALL_VERTEX_POSITION: VertexPositionTypeSet = [Low(VertexPositionType) .. High(VertexPositionType)];
    COLUMN_BLOCK_COUNT = 16;
    ROW_BLOCK_COUNT = 16;
    ALL_BLOCK_COUNT = COLUMN_BLOCK_COUNT * ROW_BLOCK_COUNT;

type
    TVertexData = packed record
        X, Y, Z: Single;
        Status: Byte;
        end;

    VertexDataArray = array of TVertexData;

    PedestrianAreaStatus = (
        _pasNoEntry, // 立入禁止
        _pasWalkable // 歩行可能
        );

    PedestrianAreaCrossingSignal = (
        _pacsCrossingGreen, // 信号(青)
        _pacsCrossingRed // 信号(赤)
        );

    PedestrianAreaStatusList = array of PedestrianAreaStatus;

    TPedestrianMatrixConfig = record
        LeftTopLatLon: TPoint3D;
        CellSize: Double; // [m]
        ColumnCount: Integer; // 列数
        RowCount: Integer; // 行数
        function  AllCellCount: Integer;
        function  RowSize: Double;
        function  ColumnSize: Double;
        class operator Equal(A, B: TPedestrianMatrixConfig): Boolean;
        class operator NotEqual(A, B: TPedestrianMatrixConfig): Boolean;
        end;

    TPointPositionData = record
        LatLon: TPoint3D;
        Local : TPoint3D;
        OpenGL: TPoint3D;
        end;

    TSeparatedAreaEdges = record
        AboveEdges: TList<integer>;
        BelowEdges: TList<integer>;
        LeftEdges : TList<integer>;
        RightEdges: TList<integer>;
        AboveLeft : TList<integer>;
        AboveRight: TList<integer>;
        BelowLeft : TList<integer>;
        BelowRight: TList<integer>;
        end;

    TArraySeparatedAreaEdges = record
        AboveEdges: TArray<integer>;
        BelowEdges: TArray<integer>;
        LeftEdges : TArray<integer>;
        RightEdges: TArray<integer>;
        AboveLeft : TArray<integer>;
        AboveRight: TArray<integer>;
        BelowLeft : TArray<integer>;
        BelowRight: TArray<integer>;
        end;

    ChangeCrossingSignalEventProc = procedure(const aSignal: PedestrianAreaCrossingSignal) of Object;
    ChangeCrossingEditingModeEventProc = procedure Of Object;

// 点を辺に変換
function  ConvertToEdge(const aVertexs: VertexPositions; const aTarget: EdgePositionType): TParametricLine2D;

function  ConvertToDiagonal_LeftTop(const aVertexs: VertexPositions): TParametricLine2D;
function  ConvertToDiagonal_RightTop(const aVertexs: VertexPositions): TParametricLine2D;
function  ConvertToDiagonal_LeftBottom(const aVertexs: VertexPositions): TParametricLine2D;
function  ConvertToDiagonal_RightBottom(const aVertexs: VertexPositions): TParametricLine2D;

// 点列を辺列に変換
function  MakeArea(const aPointList: TPoint3DListType): TParametricLineList;
// 点が面内にあるか
function IsInArea(const aPoint: TPoint3D; const aArea: TParametricLineList): Boolean;

// XY平面(LatLon)
// 凸な多角形同士とする
function  IsOverlay(const aPoint: TPoint3D; const aList: TPoint3DListType): Boolean; overload;
function  IsOverlay(const aPoint: TPoint3D; const aVertexs: Vertexpositions): Boolean; overload;
function  IsOverlay(const aEdge: TParametricLine2D; const aList: TPoint3DListType): Boolean; overload;
function  IsOverlay(const aList1, aList2: TPoint3DListType): Boolean; overload;
function  IsOverlay(const aVertexs1, aVertexs2: VertexPositions): Boolean; overload;
function  IsOverlay(const aVertexs: Vertexpositions; const aList: TPoint3DListType): Boolean; overload;

implementation

uses
    System.Math,
    LatLonHelper;

function  ConvertToEdge(const aVertexs: VertexPositions; const aTarget: EdgePositionType): TParametricLine2D;
    begin
    // 反時計回りに格納
    case aTarget of
        EdgePositionType._Top:
            begin
            Result.a := aVertexs[VertexPositionType._RightTop];
            Result.b := aVertexs[VertexPositionType._LeftTop];
            end;
        EdgePositionType._Right:
            begin
            Result.a := aVertexs[VertexPositionType._RightBottom];
            Result.b := aVertexs[VertexPositionType._RightTop];
            end;
        EdgePositionType._Bottom:
            begin
            Result.a := aVertexs[VertexPositionType._LeftBottom];
            Result.b := aVertexs[VertexPositionType._RightBottom];
            end;
        EdgePositionType._Left:
            begin
            Result.a := aVertexs[VertexPositionType._LeftTop];
            Result.b := aVertexs[VertexPositionType._LeftBottom];
            end;
        else
            {no action};
        end;
    end;

function  ConvertToDiagonal_LeftTop(const aVertexs: VertexPositions): TParametricLine2D;
    begin
    Result.a := aVertexs[VertexPositionType._RightTop];
    Result.b := aVertexs[VertexPositionType._LeftBottom];
    end;

function  ConvertToDiagonal_RightTop(const aVertexs: VertexPositions): TParametricLine2D;
    begin
    Result.a := aVertexs[VertexPositionType._RightBottom];
    Result.b := aVertexs[VertexPositionType._LeftTop];
    end;

function  ConvertToDiagonal_LeftBottom(const aVertexs: VertexPositions): TParametricLine2D;
    begin
    Result.a := aVertexs[VertexPositionType._LeftTop];
    Result.b := aVertexs[VertexPositionType._RightBottom];
    end;

function  ConvertToDiagonal_RightBottom(const aVertexs: VertexPositions): TParametricLine2D;
    begin
    Result.a := aVertexs[VertexPositionType._LeftBottom];
    Result.b := aVertexs[VertexPositionType._RightTop];
    end;

function  MakeArea(const aPointList: TPoint3DListType): TParametricLineList;
    var
        i: Integer;
        len: Integer;
    begin
    len := Length(aPointList);
    SetLength(Result, len);
    for i := 0 to len - 1 do
        begin
        if i = (len - 1) then
            begin
            Result[i].a := aPointList[i];
            Result[i].b := aPointList[0];
            end
        else
            begin
            Result[i].a := aPointList[i];
            Result[i].b := aPointList[i + 1];
            end;
        end;
    end;

function IsInArea(const aPoint: TPoint3D; const aArea: TParametricLineList): Boolean;
    var
        i: Integer;
        edg: TParametricLine2D;
        nt: Integer;
        nf: Integer;
        abvec, apvec: TDoublePoint;
    begin
    nt := 0;
    nf := 0;
    for i := 0 to Length(aArea) - 1 do
        begin
        edg := aArea[i];

        abvec := edg.b.LatLon - edg.a.LatLon;
        apvec := edg.a.LatLon - aPoint.LatLon;
        case Sign(abvec.Cross(apvec)) of
            ZeroValue:
                begin
                Inc(nt);
                Inc(nf);
                end;
            PositiveValue: Inc(nt);
            NegativeValue: Inc(nf);
            end;
        end;

//    Result := ((nt = 0) and (nf > 0)) or ((nt > 0) and (nf = 0));
    Result := (nt = Length(aArea)) or (nf = Length(aArea));
    end;

function IsOverlay(const aPoint: TPoint3D; const aList: TPoint3DListType): Boolean;
    begin
    Result := False;
    if Length(aList) <= 3 then
        Exit;

    Result := IsInArea(aPoint, MakeArea(aList));
    end;

function  IsOverlay(const aPoint: TPoint3D; const aVertexs: Vertexpositions): Boolean;
    var
        list: TPoint3DListType;
        i: Integer;
    begin
    SetLength(list, 4);
    for i := 0 to 3 do
        list[i] := aVertexs[VertexPositionType(i)];

    Result := IsOverlay(aPoint, list);
    end;

function  IsOverlay(const aEdge: TParametricLine2D; const aList: TPoint3DListType): Boolean;
    var
        area: TParametricLineList;
    begin
    Result := False;
    if Length(aList) <= 3 then
        Exit;

    area := MakeArea(aList);
    Result := IsInArea(aEdge.a, area) or IsInArea(aEdge.b, area);
    end;

function  IsOverlay(const aList1, aList2: TPoint3DListType): Boolean;
    function  IsPointsInArea(const aPoints: TPoint3DListType; const aArea: TParametricLineList): Boolean;
        var
            i: Integer;
            len: Integer;
        begin
        Result := False;
        len := Length(aPoints);
        for i := 0 to len - 1 do
            begin
            if IsInArea(aPoints[i], aArea) then
                begin
                Result := True;
                Break;
                end;
            end;
        end;
    var
        len1, len2: Integer;
    begin
    Result := False;
    len1 := Length(aList1);
    len2 := Length(aList2);

    if (len1 >= 3) and (len2 >= 3) then {1, 2がともに面}
        Result := IsPointsInArea(aList1, MakeArea(aList2)) or IsPointsInArea(aList2, MakeArea(aList1))
    else
        begin

        if (len1 < 3) and (len2 < 3) then {1, 2がともに面ではない}
            Exit
        else if len1 >= 3 then {1は面で2は面ではない}
            begin
            case len2 of
                0: Exit;
                1: Result := IsOverlay(aList2[0], aList1); {2は点}
                2: Result := IsOverlay(aList2[0], aList1) or IsOverlay(aList2[1], aList1); {2は辺}
                end;
            end
        else {len2 >= 3 1は面ではなく2は面}
            begin
            case len1 of
                0: Exit;
                1: Result := IsOverlay(aList1[0], aList2); {Aは点}
                2: Result := IsOverlay(aList1[0], aList2) or IsOverlay(aList1[1], aList2); {Aは辺}
                end;
            end;
        end;
    end;

function  IsOverlay(const aVertexs1, aVertexs2: VertexPositions): Boolean;
    var
        A, B: TPoint3DListType;
        i: Integer;
    begin
    SetLength(A, 4);
    SetLength(B, 4);
    for i := 0 to 3 do
        begin
        A[i] := aVertexs1[VertexPositionType(i)];
        B[i] := aVertexs2[VertexPositionType(i)];
        end;

    Result := IsOverlay(A, B);
    end;

function IsOverlay(const aVertexs: Vertexpositions; const aList: TPoint3DListType): Boolean;
    var
        A: TPoint3DListType;
        i: Integer;
    begin
    SetLength(A, 4);
    for i := 0 to 3 do
        begin
        A[i] := aVertexs[VertexPositionType(i)];
        end;

    Result := IsOverlay(A, aList);
    end;

{ TPedestrianMatrixConfig }
function TPedestrianMatrixConfig.AllCellCount: Integer;
    begin
    Result := RowCount * ColumnCount;
    end;

function TPedestrianMatrixConfig.RowSize: Double;
    begin
    Result := RowCount * CellSize;
    end;

function TPedestrianMatrixConfig.ColumnSize: Double;
    begin
    Result := ColumnCount * CellSize;
    end;

class operator TPedestrianMatrixConfig.Equal(A, B: TPedestrianMatrixConfig): Boolean;
    begin
    Result := ((A.LeftTopLatLon = B.LeftTopLatLon) and (A.CellSize = B.CellSize) and (A.ColumnCount = B.ColumnCount) and (A.RowCount = B.RowCount));
    end;

class operator TPedestrianMatrixConfig.NotEqual(A, B: TPedestrianMatrixConfig): Boolean;
    begin
    Result := ((A.LeftTopLatLon <> B.LeftTopLatLon) or (A.CellSize <> B.CellSize) or (A.ColumnCount <> B.ColumnCount) or (A.RowCount <> B.RowCount));
    end;
end.
