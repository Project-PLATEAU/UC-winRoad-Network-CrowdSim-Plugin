unit F_PopDetailSetting;

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
    Vcl.ExtCtrls,
    Data.DB,
    Vcl.StdCtrls,
    Vcl.Grids,
    Vcl.DBGrids;

type
    TFramePopDetailSetting = class(TFrame)
        PanelMain: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        sgPopDetail: TStringGrid;
        PanelFotter: TPanel;
        private

        public

        end;

implementation

{$R *.dfm}

end.
