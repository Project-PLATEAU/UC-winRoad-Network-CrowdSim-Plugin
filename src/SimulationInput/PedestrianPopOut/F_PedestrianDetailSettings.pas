unit F_PedestrianDetailSettings;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.ImageList,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    VclTee.TeeGDIPlus,
    Vcl.ImgList,
    VCLTee.TeEngine,
    VCLTee.Series,
    VCLTee.TeeProcs,
    VCLTee.Chart,
    F8RealSpinEdit,
    PedestrianDestinationDistribution;

type
    TFramePedestrianDetailSettings = class(TFrame)
        ImageList1: TImageList;
        GroupBoxAttributesSetting: TGroupBox;
        PanelDestination: TPanel;
        Label8: TLabel;
        Label9: TLabel;
        cbbDest: TComboBox;
        ChartGoalDistribution: TChart;
        SeriesGoalDistribution: TPieSeries;
        seDestPer: TF8RealSpinEdit;
        procedure cbbDestChange(Sender: TObject);
        procedure seDestPerChange(Sender: TObject);
        private
            p_DistinationDistributionName: String;
            p_EditingData: TPedestrianDistinationDistribution;
            p_IsReadOnly: Boolean;

            procedure SetPedestrianLabelName(const aValue: String);
            procedure SetEditingData(const aValue: TPedestrianDistinationDistribution);
            procedure SetIsReadOnly(const aValue: Boolean);
            function  GetPedestrianLabelName: String;
            function  GetEditingData: TPedestrianDistinationDistribution;

            procedure ClearGraph;
            procedure ChangeUIEnabled(const aEnabled: Boolean);
            procedure RebuildGraph;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            property  DistinationDistributionName: String                             read GetPedestrianLabelName write SetPedestrianLabelName;
            property  EditingData                : TPedestrianDistinationDistribution read GetEditingData         write SetEditingData;
            property  IsReadOnly                 : Boolean                            read p_IsReadOnly           write SetIsReadOnly;
        end;

implementation

const
    GROUP_DISTINATION_DISTRIBUTION_NAME = '%s: 目的地分布';

{$R *.dfm}

{ TFramePedestrianDetailSettings }
procedure TFramePedestrianDetailSettings.AfterConstruction;
    begin
    inherited;
    p_DistinationDistributionName := '';
    p_IsReadOnly := False;
    end;

procedure TFramePedestrianDetailSettings.BeforeDestruction;
    begin
    inherited;
    p_EditingData := nil;
    end;

procedure TFramePedestrianDetailSettings.cbbDestChange(Sender: TObject);
    var
        idx: Integer;
    begin
    if not Assigned(p_EditingData) then
        Exit;

    idx := cbbDest.ItemIndex;
    if idx < 0 then
        Exit;

    // detach event
    seDestPer.OnChange := nil;

    seDestPer.Value := p_EditingData[idx].Rate;
    // attach event
    seDestPer.OnChange := seDestPerChange;
    end;

procedure TFramePedestrianDetailSettings.seDestPerChange(Sender: TObject);
    var
        idx: Integer;
        newData: TPedestrianDistinationDistributionItem;
    begin
    if not Assigned(p_EditingData) then
        Exit;

    if IsReadOnly then
        Exit;

    idx := cbbDest.ItemIndex;
    if idx < 0 then
        Exit;

    newData := p_EditingData[idx];
    newData.Rate := seDestPer.iValue;
    p_EditingData.ChangeDistinationRate(idx, newData);
    RebuildGraph;
    end;

procedure TFramePedestrianDetailSettings.SetPedestrianLabelName(const aValue: String);
    begin
    p_DistinationDistributionName := aValue;
    GroupBoxAttributesSetting.Caption := Format(GROUP_DISTINATION_DISTRIBUTION_NAME, [p_DistinationDistributionName]);
    end;

procedure TFramePedestrianDetailSettings.SetEditingData(const aValue: TPedestrianDistinationDistribution);
    var
        i: Integer;
    begin
    // detach event
    cbbDest.OnChange   := nil;
    seDestPer.OnChange := nil;

    if Assigned(p_EditingData) then
        begin
        ClearGraph;
        cbbDest.Clear;
        ChangeUIEnabled(False);
        p_EditingData := nil;
        end;

    p_EditingData := aValue;
    if Assigned(p_EditingData) and (p_EditingData.ItemCount > 0) then
        begin
        for i := 0 to p_EditingData.ItemCount - 1 do
            begin
            cbbDest.Items.Add(p_EditingData[i].Name);
            if i = 0 then
                begin
                seDestPer.Value := p_EditingData[i].Rate;
                cbbDest.ItemIndex := i;
                end;
            end;

        RebuildGraph;
        ChangeUIEnabled(True);
        end;

    // attach event
    cbbDest.OnChange   := cbbDestChange;
    seDestPer.OnChange := seDestPerChange;
    end;

procedure TFramePedestrianDetailSettings.SetIsReadOnly(const aValue: Boolean);
    begin
    if p_IsReadOnly <> aValue then
        begin
        p_IsReadOnly := aValue;
        ChangeUIEnabled(p_IsReadOnly);
        end;
    end;

function TFramePedestrianDetailSettings.GetPedestrianLabelName: String;
    begin
    Result := p_DistinationDistributionName;
    end;

function TFramePedestrianDetailSettings.GetEditingData: TPedestrianDistinationDistribution;
    begin
    Result := p_EditingData;
    end;

procedure TFramePedestrianDetailSettings.ClearGraph;
    begin
    SeriesGoalDistribution.Clear;
    //Visibleをfalse->trueとすることでグラフの描画を更新する
    SeriesGoalDistribution.Visible := False;
    SeriesGoalDistribution.Visible := True;
    end;

procedure TFramePedestrianDetailSettings.ChangeUIEnabled(const aEnabled: Boolean);
    begin
    cbbDest.Enabled               := aEnabled;
    seDestPer.Enabled             := aEnabled and (not p_IsReadOnly);
    ChartGoalDistribution.Enabled := aEnabled;
    end;

procedure TFramePedestrianDetailSettings.RebuildGraph;
    var
        i: Integer;
    begin
    ClearGraph;
    if Assigned(p_EditingData) and (p_EditingData.ItemCount > 0) then
        begin
        for i := p_EditingData.ItemCount - 1 downto 0 do
            SeriesGoalDistribution.Add(p_EditingData[i].Rate, p_EditingData[i].Name);
        end;
    //Visibleをfalse->trueとすることでグラフの描画を更新する
    SeriesGoalDistribution.Visible := False;
    SeriesGoalDistribution.Visible := True;
    end;
end.
