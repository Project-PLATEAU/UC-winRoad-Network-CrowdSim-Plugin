unit FormPopDetail;

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
    Vcl.Grids,
    F_PopDetailSetting;

type
    TFormPopDetailSettings = class(TForm)
        F_PopDetail: TFramePopDetailSetting;
        procedure sgPopDetailSelectCell(Sender: TObject; ACol,Å@ARow: Integer; var CanSelect: Boolean);
        private

        public
            procedure AfterConstruction; override;
        end;

implementation

{$R *.dfm}

procedure TFormPopDetailSettings.AfterConstruction;

    procedure SetTimeLabel;
        var
            i : integer;
        begin
        for i := 1 to 24 do
            begin
            if i <> 24 then
                F_PopDetail.sgPopDetail.Cells[0, i] := (i - 1).ToString+':00Å`' + i.ToString+':00'
            else
                F_PopDetail.sgPopDetail.Cells[0, i] := (i - 1).ToString+':00Å`' + '0:00';
            end;

        end;

    begin
    inherited;

    F_PopDetail.sgPopDetail.Cells[0, 0] := 'éûçè';
    F_PopDetail.sgPopDetail.Cells[1, 0] := 'î≠ê∂î{ó¶(Åì)';
    SetTimeLabel;
    end;

procedure TFormPopDetailSettings.sgPopDetailSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    begin
    if (ACol = 0) or (ARow = 0) then
        F_PopDetail.sgPopDetail.Options := F_PopDetail.sgPopDetail.Options - [goEditing]
    else
        F_PopDetail.sgPopDetail.Options := F_PopDetail.sgPopDetail.Options + [goEditing];
    end;
end.
