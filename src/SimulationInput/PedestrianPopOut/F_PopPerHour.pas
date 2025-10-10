unit F_PopPerHour;

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
    Vcl.StdCtrls,
    F8RealSpinEdit;

type
    TFramePedestrianPopPerHour = class(TFrame)
        PanelMain: TPanel;
        grpbPopPerHour: TGroupBox;
        Label1: TLabel;
        sePopPerHour: TF8RealSpinEdit;
        btnDetailSettings: TButton;
        private

        public

        end;

implementation

{$R *.dfm}

end.
