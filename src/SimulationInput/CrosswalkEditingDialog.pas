unit CrosswalkEditingDialog;

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
    TFormCrosswalkEditingDialog = class(TForm)
        Panel1: TPanel;
        LabelMessage: TLabel;
        ButtonOK: TButton;
        ButtonCancel: TButton;
    private
    { Private �錾 }
    public
    { Public �錾 }
    end;

implementation

{$R *.dfm}

end.
