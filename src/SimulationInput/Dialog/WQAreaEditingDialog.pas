unit WQAreaEditingDialog;

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
    TFormWQAreaEditingDialog = class(TForm)
        Panel1: TPanel;
        LabelMessage: TLabel;
        ButtonOK: TButton;
        ButtonCancel: TButton;
    private

    public

    end;

implementation

{$R *.dfm}

end.
