unit F_PedestrianOutbyInterval;

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
    TFramePedestrianOutbyInterval = class(TFrame)
        PanelMain: TPanel;
        grpbOutInterval: TGroupBox;
        lblOutInterval: TLabel;
        lblOutNum: TLabel;
        seOutInterval: TF8RealSpinEdit;
        seOutNum: TF8RealSpinEdit;
        private

        public

        end;

implementation

{$R *.dfm}

end.
