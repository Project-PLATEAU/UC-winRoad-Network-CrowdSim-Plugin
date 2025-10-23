unit FormPedDetailSettings;

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
    F_PedestrianDetailSettings,
    PedestrianDestinationDistribution;

type
    TFormPedestrianDetailSettings = class(TForm)
        F_PedDetail: TFramePedestrianDetailSettings;
        PanelFotter: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        procedure btnOKClick(Sender: TObject);
        procedure btnCancelClick(Sender: TObject);
        private
        public
            procedure ChangePedestrianLabel(const aPedestrianLabelName: String; const aEditingData: TPedestrianDistinationDistribution);
        end;

implementation

{$R *.dfm}

procedure TFormPedestrianDetailSettings.btnOKClick(Sender: TObject);
    begin
    modalResult := mrOk;// auto close
    end;

procedure TFormPedestrianDetailSettings.btnCancelClick(Sender: TObject);
    begin
    Close;// mrCancel
    end;

procedure TFormPedestrianDetailSettings.ChangePedestrianLabel(const aPedestrianLabelName: String; const aEditingData: TPedestrianDistinationDistribution);
    begin
    F_PedDetail.DistinationDistributionName := aPedestrianLabelName;
    F_pedDetail.EditingData := aEditingData;
    end;
end.
