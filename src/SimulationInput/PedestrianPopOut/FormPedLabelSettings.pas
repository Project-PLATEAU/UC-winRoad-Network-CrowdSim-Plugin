unit FormPedLabelSettings;

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
    F_AttributesDistribution,
    Vcl.StdCtrls,
    Vcl.ExtCtrls;

type
    TFormPedestrianLabelSettings = class(TForm)
        F_AttrDist: TFrameAttributesDistribution;
        Panel1: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        procedure btnOKClick(Sender: TObject);
        procedure btnCancelClick(Sender: TObject);
        private

        public

        end;

implementation

{$R *.dfm}

procedure TFormPedestrianLabelSettings.btnOKClick(Sender: TObject);
    begin
    ModalResult := mrOK;
    end;

procedure TFormPedestrianLabelSettings.btnCancelClick(Sender: TObject);
    begin
    Close;
    end;
end.
