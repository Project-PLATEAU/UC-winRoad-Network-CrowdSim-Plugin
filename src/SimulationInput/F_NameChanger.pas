unit F_NameChanger;

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
    Vcl.StdCtrls;

type
    TFormNameChange = class(TForm)
        PanelMain: TPanel;
        edtNewName: TEdit;
        lblNewName: TLabel;
        btnOK: TButton;
        btnCancel: TButton;
        procedure FormShow(Sender: TObject);
        private

        public

        end;

implementation

{$R *.dfm}

procedure TFormNameChange.FormShow(Sender: TObject);
    begin
    edtNewName.SetFocus;
    end;
end.
