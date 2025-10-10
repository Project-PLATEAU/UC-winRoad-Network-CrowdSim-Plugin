unit FormPedDetailSettings;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.Generics.Collections,
    System.SysUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    F_PedestrianDetailSettings;

type
    TFormPedestrianDetailSettings = class(TForm)
        F_PedDetail: TFramePedestrianDetailSettings;
        PanelFotter: TPanel;
        btnOK: TButton;
        btnCancel: TButton;
        procedure seDestPerChange(Sender: TObject);
        procedure cbbDestinationChange(Sender: TObject);
        private
            p_DestPerList    : TList<double>;
            p_NewDestPerList : TList<double>;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure UpdateGraph;

            property  DestPerList : TList<double> read p_DestPerList write p_DestPerList;
            property  NewDestPerList : TList<double> read p_NewDestPerList write p_NewDestPerList;
        end;

implementation

{$R *.dfm}

procedure TFormPedestrianDetailSettings.AfterConstruction;
    begin
    inherited;
    p_DestPerList    := nil;
    p_NewDestPerList := TList<double>.Create;
    end;

procedure TFormPedestrianDetailSettings.BeforeDestruction;
    begin
    inherited;
    p_DestPerList    := nil;
    FreeAndNil(p_NewDestPerList);
    end;

procedure TFormPedestrianDetailSettings.cbbDestinationChange(Sender: TObject);
    var
        SelIdx : integer;
    begin
    SelIdx := F_PedDetail.F_PedAttr.cbbDest.ItemIndex;
    if SelIdx < 0 then
        Exit;

    F_PedDetail.F_PedAttr.seDestPer.Value := p_NewDestPerList[SelIdx];
    end;

procedure TFormPedestrianDetailSettings.seDestPerChange(Sender: TObject);
    var
        SelIdx : integer;
        MaxIdx : integer;
    begin
    SelIdx := F_PedDetail.F_PedAttr.cbbDest.ItemIndex;
    if SelIdx < 0 then
        Exit;

    MaxIdx := F_PedDetail.F_PedAttr.cbbDest.Items.Count - 1;
    F_PedDetail.F_PedAttr.SeriesGoalDistribution.PieValues[MaxIdx - SelIdx] := F_PedDetail.F_PedAttr.seDestPer.Value;
    //Visibleをfalse->trueとすることでグラフの描画を更新する
    F_PedDetail.F_PedAttr.SeriesGoalDistribution.Visible := false;
    F_PedDetail.F_PedAttr.SeriesGoalDistribution.Visible := true;
    p_NewDestPerList[SelIdx] := F_PedDetail.F_PedAttr.seDestPer.iValue;
    end;

procedure TFormPedestrianDetailSettings.UpdateGraph;
    var
        i : integer;
        Value : double;
    begin
    if Assigned(p_NewDestPerList) then
        p_NewDestPerList.Clear;

    for i := 0 to p_DestPerList.Count - 1 do
        begin
        Value := p_DestPerList[i];
        p_NewDestPerList.Add(Value);
        end;

    F_PedDetail.F_PedAttr.SeriesGoalDistribution.Clear;
    for i := F_PedDetail.F_PedAttr.cbbDest.Items.Count - 1 downto 0 do
        F_PedDetail.F_PedAttr.SeriesGoalDistribution.Add(p_NewDestPerList[i], F_PedDetail.F_PedAttr.cbbDest.Items[i]);
    end;
end.
