unit F_ScheduleSettings;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.DateUtils,
    System.Classes,
    System.IOUtils,
    System.Generics.Collections,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    Vcl.Grids,
    Data.DB,
    Vcl.DBGrids,
    Vcl.ComCtrls,
    System.ImageList,
    Vcl.ImgList,
    Vcl.Buttons,
    F8RealSpinEdit,
    PopOutPointList;

const
    _DateTime     = 1;
    _SeatOrPedNum = 2;
    _Capacity     = 3;
    _GetOnNum     = 4;
    _GetOffNum    = 5;

type
    TStrGridAccess = class(TStringGrid)
        private

        public
            procedure InsertRow(ARow: Longint; IsPop: boolean);
            procedure DelRow(ARow: Longint);
        end;

    TFrameScheduleSettings = class(TFrame)
        PanelSceduleIO: TPanel;
        btnImport: TButton;
        btnExport: TButton;
        PanelScheduleData: TPanel;
        sgSchedule: TStringGrid;
        PanelFotter: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        dtpScheTime: TDateTimePicker;
        btnAdd: TSpeedButton;
        btnDelete: TSpeedButton;
        ImageList1: TImageList;
        lblCaptionAboutGetOnNum: TLabel;
        OpenDialogSchedule: TOpenDialog;
        SaveDialogSchedule: TSaveDialog;
        PanelPopLblNums: TPanel;
        lbPopLbls: TListBox;
        cbbPedLbl: TComboBox;
        Label1: TLabel;
        btnPopLblAdd: TSpeedButton;
        Label2: TLabel;
        btnPopLblDel: TSpeedButton;
        Label3: TLabel;
        Label4: TLabel;
        seLblPopNum: TF8RealSpinEdit;
        seLblPopPer: TF8RealSpinEdit;
        btnDestPerSettings: TButton;
        procedure sgScheduleSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
        procedure btnAddClick(Sender: TObject);
        procedure btnDeleteClick(Sender: TObject);
        procedure dtpScheTimeExit(Sender: TObject);
        procedure sgScheduleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure btnImportClick(Sender: TObject);
        procedure btnExportClick(Sender: TObject);
        procedure lbPopLblsClick(Sender: TObject);
        procedure seLblPopNumChange(Sender: TObject);
        procedure sgScheduleSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
        private
            p_tmpCol : integer;
            p_tmpRow : integer;
            p_IsPopSche : boolean;
            p_IsGetOffSche : boolean;
            p_tmpPNumDict : TObjectDictionary<integer, TList<integer>>;
            p_tmpDateTimes : TList<TDateTime>;
            p_tmpBusID : integer;
            procedure CalcGetOnNum;
            procedure UpdateBusID;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure UpdateEditData;
            procedure UpdateEditDataDel(idx: integer);
            procedure UpdatePopNums;
            property  IsPopSche : boolean read p_IsPopSche write p_IsPopSche;
            property  IsGetOffSche : boolean read p_IsGetOffSche write p_IsGetOffSche;
            property  tmpPNumDict : TObjectDictionary<integer, TList<integer>> read p_tmpPNumDict write p_tmpPNumDict;
            property  tmpDateTimes : TList<TDateTime> read p_tmpDateTimes write p_tmpDateTimes;
        end;

implementation

{$R *.dfm}

{ TStrGridAccess }
procedure TStrGridAccess.InsertRow(ARow: Longint; IsPop: boolean);

    procedure Initialize;
        begin
        Cells[_DateTime,     RowCount - 1]  := '12:00';
        if IsPop then
            Cells[_SeatOrPedNum, RowCount - 1]  := '30'
        else
            Cells[_SeatOrPedNum, RowCount - 1]  := '50';

        if IsPop then
            Cells[_Capacity, RowCount - 1]  := (RowCount-1).ToString
        else
            Cells[_Capacity, RowCount - 1]  := '100';

        Cells[_GetOnNum,     RowCount - 1]  := '50';
        Cells[_GetOffNum,    RowCount - 1]  := (RowCount-1).ToString;
        end;

    begin
    RowCount := RowCount + 1;
    Initialize;
    end;

procedure TStrGridAccess.DelRow(ARow: Integer);
    begin
    Rows[ARow].Text := '';
    inherited DeleteRow(ARow);
    end;

{ TFrameScheduleSettings }
procedure TFrameScheduleSettings.AfterConstruction;
    begin
    inherited;
    p_IsPopSche := false;
    p_IsGetOffSche := false;
    p_tmpPNumDict := TObjectDictionary<integer, TList<integer>>.Create([doOwnsValues]);
    p_tmpDateTimes := TList<TDateTime>.Create;
    sgSchedule.ColWidths[0] := 0;
    sgSchedule.Cells[_DateTime,     0]  := '時刻';
    sgSchedule.Cells[_SeatOrPedNum, 0]  := '座席数';
    sgSchedule.Cells[_Capacity,     0]  := '最大乗車率(％)';
    sgSchedule.Cells[_GetOnNum,     0]  := '乗車(退出)可能人数';
    sgSchedule.Cells[_GetOffNum,    0]  := '降車(発生)人数';
    end;

procedure TFrameScheduleSettings.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_tmpPNumDict);
    FreeAndNil(p_tmpDateTimes);
    end;

procedure TFrameScheduleSettings.btnAddClick(Sender: TObject);
    var
        newList : TList<integer>;
        i : integer;
    begin
    TStrGridAccess(sgSchedule).InsertRow(sgSchedule.Selection.Top, p_IsPopSche);
    newList := TList<integer>.Create;
    for i := 0 to lbPopLbls.Items.Count - 1 do
        newList.Add(30);

    p_tmpPNumDict.Add(sgSchedule.RowCount - 1, newList);
    p_tmpDateTimes.Add(EncodeDateTime(2025, 12, 31, 12, 0, 0, 500));
    end;

procedure TFrameScheduleSettings.btnDeleteClick(Sender: TObject);
    var
        i : integer;
    begin
    for i := 1 to sgSchedule.RowCount - 1 do
        begin
        if sgSchedule.Selection.Top = i then
            begin
            p_tmpPNumDict.Remove(i);
            if i >= 1 then
                p_tmpDateTimes.Delete(i - 1);

            TStrGridAccess(sgSchedule).DelRow(i);
            dtpScheTime.Visible := false;
            Exit;
            end;
        end;
    end;

procedure TFrameScheduleSettings.btnImportClick(Sender: TObject);
    var
        i, j, LblCnt : integer;
        ImpStrList : TStringList;
        RowDataStr : TStringList;
        Header : TStringList;
        newList : TList<integer>;
        DateStr : TStringList;
    begin
    if OpenDialogSchedule.Execute then
        begin
        if not FileExists(OpenDialogSchedule.FileName) then
            begin
            ShowMessage('インポートに失敗しました');
            raise Exception.Create('ファイルが存在しません');
            end;

        ImpStrList := TStringList.Create;
        RowDataStr := TStringList.Create;
        Header     := TStringList.Create;
        try
            RowDataStr.Delimiter := ',';
            RowDataStr.StrictDelimiter := true;
            Header.Delimiter := ',';
            Header.StrictDelimiter := true;
            ImpStrList.LoadFromFile(OpenDialogSchedule.FileName);
            Header.DelimitedText := ImpStrList[0];
            for i := 1 to ImpStrList.Count - 1 do
                begin
                if ImpStrList[i] = '' then
                    Continue;

                RowDataStr.DelimitedText := ImpStrList[i];
                LblCnt := RowDataStr.Count - 2;
                if RowDataStr.Count <> sgSchedule.ColCount - 1 then
                    begin
                    if (p_IsPopSche) and (Header[1] = '降車(発生)人数') and (lbPopLbls.Items.Count <> LblCnt) then
                        begin
                        ShowMessage('このデータをインポートするには'+LblCnt.ToString+'個の人流ラベルが必要です');
                        raise Exception.Create('適用されている人流ラベルが不足しています');
                        end
                    else if (not p_IsPopSche) or ((p_IsPopSche) and (Header[1] <> '降車(発生)人数')) then
                        begin
                        ShowMessage('インポートに失敗しました');
                        raise Exception.Create('不適切なデータフォーマットです');
                        end;
                    end;

                if sgSchedule.RowCount < i + 1 then
                    begin
                    sgSchedule.RowCount := sgSchedule.RowCount + 1;
                    newList := TList<integer>.Create;
                    for j := 0 to lbPopLbls.Items.Count - 1 do
                        newList.Add(30);

                    tmpPNumDict.Add(i, newList);
                    p_tmpDateTimes.Add(EncodeDateTime(2025, 12, 31, 12, 0, 0, 500));
                    end;

                for j := 0 to RowDataStr.Count - 1 do
                    begin
                    if (p_IsPopSche) and (j >= RowDataStr.Count - LblCnt) then
                        tmpPNumDict[i][j - 2] := StrToIntDef(RowDataStr[j], 30)
                    else
                        begin
                        if j = 0 then
                            begin
                            DateStr := TStringList.Create;
                            try
                            DateStr.Delimiter := ':';
                            DateStr.StrictDelimiter := true;
                            DateStr.DelimitedText := RowDataStr[j];
                            p_tmpDateTimes[i - 1] := EncodeDateTime(2025, 12, 31, StrToIntDef(DateStr[0], 12), StrToIntDef(DateStr[1], 0), 0, 500)
                            finally
                                FreeAndNil(DateStr);
                                end;
                            end;

                        sgSchedule.Cells[j + 1, i] := RowDataStr[j];
                        end;
                    end;
                end;
        finally
            FreeAndNil(ImpStrList);
            FreeAndNil(RowDataStr);
            FreeAndNil(Header);
            end;
        end;
    end;

procedure TFrameScheduleSettings.btnExportClick(Sender: TObject);
    var
        i, j    : integer;
        newFile : THandle;
        writer  : TStreamWriter;
        writeStr : string;
    begin
    if SaveDialogSchedule.Execute then
        begin
        if sgSchedule.RowCount <= 1 then
            Exit;

        if not FileExists(SaveDialogSchedule.FileName) then
            begin
            newFile := FileCreate(SaveDialogSchedule.FileName);
            FileClose(newFile);
            end;

        writer := TStreamWriter.Create(SaveDialogSchedule.FileName, False, TEncoding.UTF8);
        try
            for i := 0 to sgSchedule.RowCount - 1 do
                begin
                if p_IsPopSche then
                    begin
                    if tmpPNumDict.Count < 1 then
                        Break;

                    writeStr := sgSchedule.Rows[i].CommaText.Remove(0, 1);
                    if i = 0 then
                        begin
                        for j := 0 to tmpPNumDict[1].Count - 1 do
                            writeStr := writeStr+',ラベル'+(j+1).ToString
                        end
                    else
                        begin
                        for j := 0 to tmpPNumDict[i].Count - 1 do
                            writeStr := writeStr+','+tmpPNumDict[i][j].ToString;
                        end;

                    writer.WriteLine(writeStr);
                    end
                else
                    writer.WriteLine(sgSchedule.Rows[i].CommaText.Remove(0, 1));
                end;
        finally
            writer.Free;
            end;
        end;
    end;

procedure TFrameScheduleSettings.dtpScheTimeExit(Sender: TObject);
    var
        timeStr : string;
        newTime : TDateTime;
    begin
    if sgSchedule.RowCount <= 1 then
        begin
        dtpScheTime.Visible := false;
        Exit;
        end;

    newTime := dtpScheTime.Time;
    p_tmpDateTimes[p_tmpRow - 1] := newTime;
    timeStr := Format('%.2d', [HourOf(newTime)]) + ':' + Format('%.2d', [MinuteOf(newTime)]);
    sgSchedule.Cells[p_tmpCol, p_tmpRow] := timeStr;
    dtpScheTime.Visible := false;
    end;

procedure TFrameScheduleSettings.lbPopLblsClick(Sender: TObject);
    var
        lbIdx, SumNum, i : integer;
    begin
    if (p_IsPopSche) or (p_IsGetOffSche) then
        begin
        lbIdx := lbPopLbls.ItemIndex;
        SumNum := 0;
        if lbIdx < 0 then
            Exit;

        if p_tmpPNumDict.ContainsKey(sgSchedule.Selection.Top) then
            begin
            for i := 0 to p_tmpPNumDict[sgSchedule.Selection.Top].Count - 1 do
                SumNum := SumNum + p_tmpPNumDict[sgSchedule.Selection.Top][i];

            seLblPopNum.Value := p_tmpPNumDict[sgSchedule.Selection.Top][lbIdx];
            seLblPopPer.Value := Round((p_tmpPNumDict[sgSchedule.Selection.Top][lbIdx] / SumNum) * 100);
            if p_IsPopSche then
                sgSchedule.Cells[_SeatOrPedNum, sgSchedule.Selection.Top] := SumNum.ToString
            else
                sgSchedule.Cells[_GetOffNum,    sgSchedule.Selection.Top] := SumNum.ToString;
            end;
        end;
    end;

procedure TFrameScheduleSettings.CalcGetOnNum;
    var
        SeatOrPedNum : integer;
        Capacity     : integer;
    begin
    SeatOrPedNum := StrToIntDef(sgSchedule.Cells[_SeatOrPedNum, p_tmpRow], -1);
    Capacity     := StrToIntDef(sgSchedule.Cells[_Capacity,     p_tmpRow], -1);
    if (SeatOrPedNum < 0) or (Capacity < 0) then
        Exit;

    sgSchedule.Cells[_GetOnNum, p_tmpRow] := Round(SeatOrPedNum * (Capacity / 100)).ToString;
    end;

procedure TFrameScheduleSettings.UpdateBusID;
    var
        i : integer;
    begin
    for i := 1 to sgSchedule.RowCount - 1 do
        begin
        if p_tmpRow = i then
            Continue;

        if (p_IsPopSche) and (sgSchedule.Cells[_Capacity, p_tmpRow] = sgSchedule.Cells[_Capacity, i]) then
            begin
            ShowMessage('同一のバスIDが設定されています');
            sgSchedule.Cells[_Capacity, p_tmpRow] := p_tmpBusID.ToString;
            Exit;
            end
        else if (not p_IsPopSche) and (sgSchedule.Cells[_GetOffNum, p_tmpRow] = sgSchedule.Cells[_GetOffNum, i]) then
            begin
            ShowMessage('同一のバスIDが設定されています');
            sgSchedule.Cells[_GetOffNum, p_tmpRow] := p_tmpBusID.ToString;
            Exit;
            end;
        end;
    end;

procedure TFrameScheduleSettings.seLblPopNumChange(Sender: TObject);
    var
        lbIdx, SumNum, i : integer;
    begin
    if (p_IsPopSche) or (p_IsGetOffSche) then
        begin
        lbIdx := lbPopLbls.ItemIndex;
        SumNum := 0;
        if lbIdx < 0 then
            Exit;

        if p_tmpPNumDict.ContainsKey(sgSchedule.Selection.Top) then
            begin
            p_tmpPNumDict[sgSchedule.Selection.Top][lbIdx] := seLblPopNum.iValue;
            for i := 0 to p_tmpPNumDict[sgSchedule.Selection.Top].Count - 1 do
                SumNum := SumNum + p_tmpPNumDict[sgSchedule.Selection.Top][i];

            seLblPopPer.Value := Round((p_tmpPNumDict[sgSchedule.Selection.Top][lbIdx] / SumNum) * 100);
            if p_IsPopSche then
                sgSchedule.Cells[_SeatOrPedNum, sgSchedule.Selection.Top] := SumNum.ToString
            else
                sgSchedule.Cells[_GetOffNum,    sgSchedule.Selection.Top] := SumNum.ToString;
            end;
        end;
    end;

procedure TFrameScheduleSettings.sgScheduleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
    if (Key <> VK_RETURN) then
        Exit;

    if ((p_IsPopSche) and (p_tmpCol = _Capacity)) or (p_tmpCol = _GetOffNum) then
        UpdateBusID
    else if (not p_IsPopSche) and (((p_tmpCol = _SeatOrPedNum) or (p_tmpCol = _Capacity)) and (p_tmpRow > 0)) then
        CalcGetOnNum;
    end;

procedure TFrameScheduleSettings.sgScheduleSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    var
        lbIdx, SumNum, i : integer;
    begin
    if ((p_IsPopSche) and (ACol = _Capacity)) or ((not p_IsPopSche) and (ACol = _GetOffNum)) then
        begin
        UpdateBusID;
        p_tmpBusID := StrToIntDef(sgSchedule.Cells[ACol, ARow], 0);
        end;

    if (((p_tmpCol = _SeatOrPedNum) or (p_tmpCol = _Capacity)) and (p_tmpRow > 0)) and (not p_IsPopSche) then
        CalcGetOnNum;

    if ((ACol = 0) or (ARow = 0) or (ACol = _GetOnNum)) or ((p_IsPopSche) and (ACol = _SeatOrPedNum)) or ((p_IsGetOffSche) and (ACol = _GetOffNum)) then
        sgSchedule.Options := sgSchedule.Options - [goEditing]
    else
        begin
        sgSchedule.Options := sgSchedule.Options + [goEditing];
        if ACol = _DateTime then
            begin
            if sgSchedule.Cells[ACol, ARow] <> '' then
                dtpScheTime.Time := p_tmpDateTimes[ARow - 1];

            dtpScheTime.Left    := sgSchedule.CellRect(ACol, ARow).Left + sgSchedule.Margins.Left + 3;
            dtpScheTime.Top     := sgSchedule.CellRect(ACol, ARow).Top  + sgSchedule.Margins.Top  + 6;
            dtpScheTime.Visible := true;
            end
        else
            dtpScheTime.Visible := false;

        p_tmpCol := ACol;
        p_tmpRow := ARow;
        end;

    if (p_IsPopSche) or (p_IsGetOffSche) then
        begin
        lbIdx := lbPopLbls.ItemIndex;
        if ((p_IsPopSche) and (ACol = _SeatOrPedNum) and (lbIdx >= 0) and (ARow > 0)) then
            seLblPopNum.Enabled := true
        else
            seLblPopNum.Enabled := false;

        SumNum := 0;
        if lbIdx < 0 then
            Exit;

        if p_tmpPNumDict.ContainsKey(ARow) then
            begin
            for i := 0 to p_tmpPNumDict[ARow].Count - 1 do
                SumNum := SumNum + p_tmpPNumDict[ARow][i];
            
            seLblPopNum.OnChange := nil;
            seLblPopNum.Value := p_tmpPNumDict[ARow][lbIdx];
            seLblPopPer.Value := Round((p_tmpPNumDict[ARow][lbIdx] / SumNum) * 100);
            seLblPopNum.OnChange := seLblPopNumChange;
            end;
        end;
    end;

procedure TFrameScheduleSettings.sgScheduleSetEditText(Sender: TObject; ACol,　ARow: Integer; const Value: string);
    begin
    if p_IsPopSche then
        Exit;

    if ((p_tmpCol = _SeatOrPedNum) or (p_tmpCol = _Capacity)) and (p_tmpRow > 0) then
        CalcGetOnNum;
    end;

procedure TFrameScheduleSettings.UpdateEditData;
    var
        i, j : integer;
    begin
    for i := 1 to sgSchedule.RowCount - 1 do
        begin
        if p_tmpPNumDict.ContainsKey(i) then
            begin
            for j := 0 to lbPopLbls.Items.Count - 1 do
                begin
                if j = p_tmpPNumDict[i].Count then
                     p_tmpPNumDict[i].Add(30);
                end;
            end;
        end;
    end;

procedure TFrameScheduleSettings.UpdateEditDataDel(idx: integer);
    var
        i : integer;
    begin
    for i := 1 to sgSchedule.RowCount - 1 do
        begin
        if p_tmpPNumDict.ContainsKey(i) then
            p_tmpPNumDict[i].Delete(idx);
        end;
    end;

procedure TFrameScheduleSettings.UpdatePopNums;
    var
        Idx, Lbls, SumNum : integer;
    begin
    for Idx := 1 to sgSchedule.RowCount - 1 do
        begin
        SumNum := 0;
        if p_tmpPNumDict.ContainsKey(Idx) then
            begin
            for Lbls := 0 to p_tmpPNumDict[Idx].Count - 1 do
                SumNum := SumNum + p_tmpPNumDict[Idx][Lbls];

            if p_IsPopSche then
                sgSchedule.Cells[_SeatOrPedNum, Idx] := SumNum.ToString;
            end;
        end;
    end;
end.
