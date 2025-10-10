unit FormScheduleSettings;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    F_ScheduleSettings;

type
    TFormSchedule = class(TForm)
        F_ScheSet: TFrameScheduleSettings;
        procedure btnCancelClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure btnOKClick(Sender: TObject);
        private
            procedure sgScheduleSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
        public
            procedure AfterConstruction; override;
        end;

implementation

{$R *.dfm}

procedure TFormSchedule.AfterConstruction;
    begin
    inherited;
    F_ScheSet.sgSchedule.OnSelectCell := sgScheduleSelectCell;
    end;

procedure TFormSchedule.btnCancelClick(Sender: TObject);
    begin
    Close;
    end;

procedure TFormSchedule.FormShow(Sender: TObject);
    begin
    F_ScheSet.UpdatePopNums;
    end;

procedure TFormSchedule.btnOKClick(Sender: TObject);

    function IsFoundInvalidData(const Idx: integer): boolean;
        begin
        result := false;
        if F_ScheSet.sgSchedule.ColCount = 4 then
            begin
            if (StrToIntDef(F_ScheSet.sgSchedule.Cells[_Capacity,   Idx], -1) < 0) then
                result := true;
            end
        else
            begin
            if (StrToIntDef(F_ScheSet.sgSchedule.Cells[_SeatOrPedNum,   Idx], -1) < 0)
                or (StrToIntDef(F_ScheSet.sgSchedule.Cells[_Capacity,   Idx], -1) < 0)
                or (StrToIntDef(F_ScheSet.sgSchedule.Cells[_GetOffNum,  Idx], -1) < 0) then
                result := true;
            end;
        end;

    var
        i : integer;
    begin
    for i := 1 to F_ScheSet.sgSchedule.RowCount - 1 do
        begin
        if IsFoundInvalidData(i) then
            begin
            ShowMessage('–³Œø‚È’l‚ª“ü—Í‚³‚ê‚Ä‚¢‚Ü‚·');
            Exit;
            end;
        end;

    ModalResult := mrOK;
    end;

procedure TFormSchedule.sgScheduleSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    begin
    F_ScheSet.sgScheduleSelectCell(Sender, ACol, ARow, CanSelect);
    if ARow = 0 then
        Exit;

    if ACol = _DateTime then
        ActiveControl := F_ScheSet.dtpScheTime;
    end;
end.
