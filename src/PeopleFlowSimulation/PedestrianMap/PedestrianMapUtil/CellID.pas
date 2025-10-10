unit CellID;

interface

uses
    System.Generics.Collections;

type
    /// <summary>
    ///    セル番号を表すデータ構造
    ///    TCellIDGeneratorで生成して使用する
    /// </summary>
    TCellID = record
        private
            p_ID       : Integer;
            p_RowIdx   : Word;
            p_ColumnIdx: Word;
            p_RightID  : Integer;
            p_LeftID   : Integer;
            p_AboveID  : Integer;
            p_BelowID  : Integer;
        public
            class operator Equal(const a, b: TCellID): Boolean;
            class operator NotEqual(const a, b: TCellID): Boolean;

            function  IsConnected(const aID: TCellID): Boolean;

            property  ID       : Integer read p_ID;
            property  RowIdx   : Word    read p_RowIdx;
            property  ColumnIdx: Word    read p_ColumnIdx;
            property  RightID  : Integer read p_RightID;
            property  LeftID   : Integer read p_LeftID;
            property  AboveID  : Integer read p_AboveID;
            property  BelowID  : Integer read p_BelowID;
        end;

    /// <summary>
    ///    TCellIDを生成するクラス
    ///    初期値でマップの行数と列数を入力する
    ///    GenerateでTCellIDを生成する。この時、引数に生成したいID(左上端からの通し番号)を渡す
    /// </summary>
    TCellIDGenerator = class
        private
            p_RowCount   : Word;
            p_ColumnCount: Word;

            function  IsInRange(const aWantedID: Integer): Boolean;
        public
            constructor Create(const aRowCount, aColumnCount: Word);
            function  Generate(const aWantedID: Integer; out aOutput: TCellID): Boolean;
        end;

implementation

uses
    system.Math;

{ TCellID }
class operator TCellID.Equal(const a, b: TCellID): Boolean;
    begin
    Result := (a.p_ID = b.p_ID);
    end;

class operator TCellID.NotEqual(const a, b: TCellID): Boolean;
    begin
    Result := (a.p_ID <> b.p_ID);
    end;

function TCellID.IsConnected(const aID: TCellID): Boolean;
    begin
    Result := ((aID.ID = RightID) or (aID.ID = LeftID) or (aID.ID = AboveID) or (aID.ID = BelowID));
    end;

{ TCellIDGenerator }
constructor TCellIDGenerator.Create(const aRowCount, aColumnCount: Word);
    begin
    p_RowCount    := aRowCount;
    p_ColumnCount := aColumnCount;
    end;

function TCellIDGenerator.Generate(const aWantedID: Integer; out aOutput: TCellID): Boolean;
    var
        rowIdx, columnIdx: Word;
    begin
    Result := IsInRange(aWantedID);
    if Result then
        begin
        rowIdx := aWantedId div p_ColumnCount;
        columnIdx := aWantedId mod p_ColumnCount;

        aOutput.p_ID        := aWantedID;
        aOutput.p_RowIdx    := rowIdx;
        aOutput.p_ColumnIdx := columnIdx;
        aOutput.p_LeftID  := IfThen(columnIdx = 0, -1, aWantedID - 1);
        aOutput.p_RightID := IfThen(columnIdx = (p_ColumnCount - 1), -1, aWantedID + 1);
        aOutput.p_AboveID := IfThen(rowIdx = 0, -1, aWantedID - p_ColumnCount);
        aOutput.p_BelowID := IfThen(rowIdx = (p_RowCount - 1), -1, aWantedID + p_ColumnCount);
        end;
    end;

function TCellIDGenerator.IsInRange(const aWantedID: Integer): Boolean;
    begin
    Result := InRange(aWantedID, 0, (p_RowCount * p_ColumnCount) - 1);
    end;
end.
