unit F_MainInput;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFrameMainInput = class(TFrame)
    BtnSimAreaSettings: TButton;
    BtnCrossWalkSettings: TButton;
    BtnWaitingQueueSettings: TButton;
    BtnPedestrianPopOutType: TButton;
    PanelMain: TPanel;
  private
    { Private �錾 }
  public
    { Public �錾 }
  end;

implementation

{$R *.dfm}

end.
