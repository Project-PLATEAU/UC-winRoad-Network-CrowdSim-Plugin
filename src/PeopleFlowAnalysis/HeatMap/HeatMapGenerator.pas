unit HeatMapGenerator;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.Math,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.Generics.Collections,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    Vcl.ComCtrls,
    PedestrianMapUser;

type
    /// <summary>
    ///    ヒートマップ描画用データを生成するクラス
    /// </summary>
    THeatMap = class
        private
            p_PedestrianMapUser : TPedestrianMapUser;

            function GetMaxMinMiddlePassedNum(const sumList: TArray<integer>; out MaxNum, MinNum: integer): double;
            procedure CalcIdxFromSpan(const StTime, EndTime: TTime; MinReso: integer; out BegIdx, EndIdx: integer);
            procedure SumDataBeginIdxToEndIdx(const begIdx, endIdx: integer; IOList: TList<TArray<integer>>; out sumList: TArray<integer>);
            procedure CreateHeatMapData(const sumList: TArray<integer>; MaxNum, MinNum: integer; MidNum: double; var Bmp: TBitmap);
            procedure SetRGB(const rVal, gVal, bVal: integer; var r, g, b: integer);

            property PedestrianMapUser : TPedestrianMapUser read p_PedestrianMapUser;
        public
            function GenerateHeatMap(const PedestrianMU: TPedestrianMapUser; CellsIOL: TList<TArray<integer>>;
                StartTime, EndTime: TTime; MinResolution: integer; out MaxNum, MiddleNum, MinNum: integer): TBitmap;
        end;

implementation

{ THeatMap }
//==============================================================================
function THeatMap.GenerateHeatMap(const PedestrianMU: TPedestrianMapUser; CellsIOL: TList<TArray<integer>>;
    StartTime, EndTime: TTime; MinResolution: integer; out MaxNum, MiddleNum, MinNum: integer): TBitmap;
    var
        beginIdx : integer;
        endIdx   : integer;
        sumIO    : TArray<integer>;
        HMBmp    : TBitmap;
        tmpMid   : double;
    begin
    p_PedestrianMapUser := PedestrianMU;
    HMBmp := TBitmap.Create;
    CalcIdxFromSpan(StartTime, EndTime, MinResolution, beginIdx, endIdx);
    SumDataBeginIdxToEndIdx(beginIdx, endIdx, CellsIOL, sumIO);
    tmpMid := GetMaxMinMiddlePassedNum(sumIO, MaxNum, MinNum);
    MiddleNum := Round(tmpMid);
    CreateHeatMapData(sumIO, MaxNum, MinNum, tmpMid, HMBmp);
    Result := HMBmp;
    end;

//==============================================================================
procedure THeatMap.CalcIdxFromSpan(const StTime, EndTime: TTime; MinReso: integer; out BegIdx, EndIdx: integer);
    var
        TTimeMR   : double;
    begin
    TTimeMR   := MinReso / SecsPerDay;
    BegIdx := Trunc(StTime / TTimeMR);
    EndIdx := Trunc(EndTime / TTimeMR) - 1;
    end;

//==============================================================================
procedure THeatMap.SumDataBeginIdxToEndIdx(const begIdx, endIdx: integer; IOList: TList<TArray<integer>>; out sumList: TArray<integer>);
    var
        tmparr : TArray<integer>;
    begin
    SetLength(sumList, PedestrianMapUser.Map.Config.AllCellCount);
    SetLength(tmparr, PedestrianMapUser.Map.Config.AllCellCount);
    for var i := begIdx to endIdx do
        begin
        if i = begIdx then
            CopyMemory(sumList, IOList[i], SizeOf(integer) * Length(IOList[i]))
        else
            begin
            CopyMemory(tmparr, IOList[i], SizeOf(integer) * Length(IOList[i]));
            for var j := 0 to Length(sumList) - 1 do
                sumList[j] := sumList[j] + tmparr[j];
            end;
        end;
    end;

//==============================================================================
function THeatMap.GetMaxMinMiddlePassedNum(const sumList: TArray<integer>; out MaxNum, MinNum: integer): double;
    var
        sortIO : TArray<integer>;
        tmpRoundMode: TRoundingMode;
    begin
    SetLength(sortIO, PedestrianMapUser.Map.Config.AllCellCount);
    CopyMemory(sortIO, sumList, SizeOf(integer) * Length(sumList));
    TArray.Sort<integer>(sortIO);
    MinNum    := sortIO[0];
    MaxNum    := sortIO[Length(sortIO) - 1];
    tmpRoundMode := GetRoundMode;
    try
        SetRoundMode(rmTruncate);
        Result := Round((MaxNum + MinNum) / 2);
    finally
        SetRoundMode(tmpRoundMode);
        end;
    end;

//==============================================================================
procedure THeatMap.SetRGB(const rVal, gVal, bVal: integer; var r, g, b: integer);
    begin
    r := rVal;
    g := gVal;
    b := bVal;
    end;

//==============================================================================
procedure THeatMap.CreateHeatMapData(const sumList: TArray<integer>; MaxNum, MinNum: integer; MidNum: double; var Bmp: TBitmap);
    var
        x, y, r, g, b : integer;
        PassedNum     : integer;
    begin
    Bmp.PixelFormat := pf24bit;
    Bmp.Width       := PedestrianMapUser.Map.Config.ColumnCount;
    Bmp.Height      := PedestrianMapUser.Map.Config.RowCount;
    for y := 0 to Bmp.Height - 1 do
        begin
        for x := 0 to Bmp.Width - 1 do
            begin
            PassedNum := sumList[(y * Bmp.Width) + x];
            if PassedNum = 0 then
                SetRGB(0, 0, 255, r, g, b)
            else if PassedNum > MidNum then
                SetRGB(255, 255 - Trunc(255 * (((PassedNum - MidNum) / (MaxNum - MidNum)))), 0, r, g, b)
            else if PassedNum >= MidNum / 2 then
                SetRGB(Trunc(255 * (((PassedNum - (MidNum / 2)) / (MaxNum - (MidNum / 2))) * 2)), 255, 0, r, g, b)
            else
                SetRGB(0, Trunc(255 * (((PassedNum - MinNum) / (MaxNum - MinNum)) * 4)),
                       255 - Trunc(255 * (((PassedNum - MinNum) / (MaxNum - MinNum)) * 4)), r, g, b);

            Bmp.Canvas.Pixels[x, y] := RGB(r, g, b);
            end;
        end;
    end;
end.
