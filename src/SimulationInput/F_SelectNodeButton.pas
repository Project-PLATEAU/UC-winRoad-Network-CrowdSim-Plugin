unit F_SelectNodeButton;

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
    Vcl.ExtCtrls;

type
    TFrameSelectNodeButton = class(TFrame)
        PanelMain: TPanel;
        ButtonSelectNode: TButton;
        ButtonMovetoSelectedNode: TButton;
        private

        public

        end;

implementation

{$R *.dfm}

end.
