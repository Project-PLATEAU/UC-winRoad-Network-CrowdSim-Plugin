unit F_ExitInterval;

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
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    F8RealSpinEdit;

type
    TFrameExitInterval = class(TFrame)
        PanelMain: TPanel;
        grpbExitInterval: TGroupBox;
        lblExitInterval: TLabel;
        lblExitNum: TLabel;
        seExitInterval: TF8RealSpinEdit;
        seExitNum: TF8RealSpinEdit;
        private

        public

        end;

implementation

{$R *.dfm}

end.
