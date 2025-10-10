unit F_PedestrianPopbyInterval;

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
    F8FloatSpinEdit,
    F8RealSpinEdit;

type
    TFramePedestrianPopbyInterval = class(TFrame)
        PanelMain: TPanel;
        grpbPopInterval: TGroupBox;
        lblPopInterval: TLabel;
        lblPopNum: TLabel;
        sePopInterval: TF8RealSpinEdit;
        sePopNum: TF8RealSpinEdit;
        btnDetailSettings: TButton;
        private

        public

        end;

implementation

{$R *.dfm}

end.
