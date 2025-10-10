unit ParametricLine2DHelper;

interface

uses
    F8Utils;

type
    TParametricLine2DHelper = record helper for TParametricLine2D
        // 2つの辺が一致しているか調べる
        function  IsMatch(const aLine: TParametricLine2D): Boolean;
        // 重複する辺がaListのaEndIndex番目までに含まれているか調べる
        function CheckContains(const aList: TParametricLineList; const aEndIdx: Integer): Boolean;
        // 2つの辺が1つの頂点のみで接続しているか調べる.
        function  IsConnected_OnlyTailHead(const aLine: TParametricLine2D): Boolean;
        function  IsConnected_OnlyTailTail(const aLine: TParametricLine2D): Boolean;
        // A->BをB->Aに変換
        function  Inverse: TParametricLine2D;
        // ベクトルの長さ
        function  Magnitude: Double;
        // ベクトルの方向
        function  Direction: TPoint3D;
        // 辺の中点
        function  Center: TPoint3D;
        end;

implementation

uses
    System.Math;

{ TParametricLine2DHelper }
function TParametricLine2DHelper.IsMatch(const aLine: TParametricLine2D): Boolean;
    begin
    Result :=  ((a = aLine.a) and (b = aLine.b)) or ((a = aLine.b) and (b = aLine.a));
    end;

function TParametricLine2DHelper.CheckContains(const aList: TParametricLineList; const aEndIdx: Integer): Boolean;
    var
        i, len: Integer;
        edg: TParametricLine2D;
    begin
    Result := False;
    len := Min(Length(aList), aEndIdx);

    if len <= 0 then
        Exit;

    for i := 0 to len - 1 do
        begin
        edg := aList[i];
        if IsMatch(edg) then
            begin
            Result := True;
            Break;
            end;
        end;
    end;

function TParametricLine2DHelper.IsConnected_OnlyTailHead(const aLine: TParametricLine2D): Boolean;
    begin
    Result := (b = aLine.a) and (a <> aLine.b);
    end;

function TParametricLine2DHelper.IsConnected_OnlyTailTail(const aLine: TParametricLine2D): Boolean;
    begin
    Result := (b = aLine.b) and (a <> aLine.a);
    end;

function TParametricLine2DHelper.Inverse: TParametricLine2D;
    begin
    Result.a := b;
    Result.b := a;
    end;

function TParametricLine2DHelper.Magnitude: Double;
    begin
    Result := (b - a).Magnitude;
    end;

function TParametricLine2DHelper.Direction: TPoint3D;
    begin
    Result := (b - a).Normalise;
    end;

function TParametricLine2DHelper.Center: TPoint3D;
    begin
    Result := a + Direction * Magnitude * 0.5;
    end;
end.
