unit F_AttributesDistribution;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.Generics.Collections,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    VclTee.TeeGDIPlus,
    Vcl.StdCtrls,
    F8RealSpinEdit,
    VCLTee.TeEngine,
    VCLTee.Series,
    VCLTee.TeeProcs,
    VCLTee.Chart,
    Vcl.ExtCtrls,
    Vcl.Buttons,
    System.ImageList,
    Vcl.ImgList,
    PluginCore,
    F_NameChanger,
    PopOutPointList,
    PedestrianProfileOptionData;

type
    TFrameAttributesDistribution = class(TFrame)
        PanelFAAMain: TPanel;
        GroupBoxAttributesSetting: TGroupBox;
        Label1: TLabel;
        Label2: TLabel;
        ChartAttrDistribution: TChart;
        SeriesAttrDistribution: TPieSeries;
        cbbAttr: TComboBox;
        seAttrPer: TF8RealSpinEdit;
        Panel1: TPanel;
        Label3: TLabel;
        btnAdd: TSpeedButton;
        btnDel: TSpeedButton;
        lbPedLabel: TListBox;
        ImageList1: TImageList;
        btnChangeLblName: TButton;
        procedure btnAddClick(Sender: TObject);
        procedure btnAddOKClick(Sender: TObject);
        procedure btnChangeCancelClick(Sender: TObject);
        procedure btnChangeNameOKClick(Sender: TObject);
        procedure lbPedLabelClick(Sender: TObject);
        procedure btnChangeLblNameClick(Sender: TObject);
        procedure btnDelClick(Sender: TObject);
        procedure cbbAttrChange(Sender: TObject);
        procedure seAttrPerChange(Sender: TObject);
        private
            p_FormNameChanger : TFormNameChange;
            p_EditingLabelList : TList<TPedestrianData>;
            procedure UpdateAddedProfile;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure SetEditingLabelData(const aList: TList<TPedestrianData>);
            procedure ReceiveLabelData;

            property  EditingLabelList : TList<TPedestrianData> read p_EditingLabelList write p_EditingLabelList;
        end;

implementation

{$R *.dfm}

procedure TFrameAttributesDistribution.AfterConstruction;
    begin
    inherited;

    p_FormNameChanger := TFormNameChange.Create(nil);
    p_EditingLabelList := TList<TPedestrianData>.Create;
    end;

procedure TFrameAttributesDistribution.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_FormNameChanger);
    FreeAndNil(p_EditingLabelList);
    end;

procedure TFrameAttributesDistribution.btnAddClick(Sender: TObject);
    var
        pedProf : IF8PedestrianProfile;
        i : Integer;
    begin
    if not Assigned(p_FormNameChanger) then
        Exit;

    p_FormNameChanger.Caption := '人流ラベルの追加';
    p_FormNameChanger.lblNewName.Caption := '追加するラベル名：';
    p_FormNameChanger.btnOK.OnClick := btnAddOKClick;
    p_FormNameChanger.btnCancel.OnClick := btnChangeCancelClick;
    p_FormNameChanger.edtNewName.Text := '';
    if p_FormNameChanger.ShowModal = mrOK then
        begin
        pedProf := theApplicationServices.project.PedestrianProfileIF[theApplicationServices.project.NumberOfPedestrianProfile];
        for i := 1 to pedProf.numberOfOptions do
            begin
            if i = 1 then
                p_EditingLabelList[p_EditingLabelList.Count - 1].AttrPerList.Add(100)
            else
                p_EditingLabelList[p_EditingLabelList.Count - 1].AttrPerList.Add(0);
            end;
        end;
    end;

procedure TFrameAttributesDistribution.btnAddOKClick(Sender: TObject);
    var
        newData : TPedestrianData;
        i : Integer;
    begin
    if p_FormNameChanger.edtNewName.Text = '' then
        begin
        ShowMessage('ラベル名を入力してください');
        Exit;
        end;

    for i := 0 to p_EditingLabelList.Count - 1 do
        begin
        if p_FormNameChanger.edtNewName.Text = p_EditingLabelList[i].LabelName then
            begin
            ShowMessage('同名のラベルは追加できません');
            Exit;
            end;
        end;

    newData := TPedestrianData.Create;
    newData.LabelName := p_FormNameChanger.edtNewName.Text;
    p_EditingLabelList.Add(newData);
    lbPedLabel.Items.Add(p_FormNameChanger.edtNewName.Text);
    p_FormNameChanger.ModalResult := mrOK;
    end;

procedure TFrameAttributesDistribution.btnChangeCancelClick(Sender: TObject);
    begin
    p_FormNameChanger.Close;
    end;

procedure TFrameAttributesDistribution.btnChangeLblNameClick(Sender: TObject);
    begin
    if (not Assigned(p_FormNameChanger)) or (lbPedLabel.ItemIndex < 0) then
        Exit;

    p_FormNameChanger.Caption := '人流ラベル名の変更';
    p_FormNameChanger.lblNewName.Caption := '変更後のラベル名：';
    p_FormNameChanger.btnOK.OnClick := btnChangeNameOKClick;
    p_FormNameChanger.btnCancel.OnClick := btnChangeCancelClick;
    p_FormNameChanger.edtNewName.Text := '';
    if p_FormNameChanger.ShowModal = mrOK then
        begin
        //no action
        end;
    end;

procedure TFrameAttributesDistribution.btnChangeNameOKClick(Sender: TObject);
    var
        SelIdx : integer;
    begin
    SelIdx := lbPedLabel.ItemIndex;
    p_EditingLabelList[SelIdx].LabelName := p_FormNameChanger.edtNewName.Text;
    lbPedLabel.Items[SelIdx] := p_FormNameChanger.edtNewName.Text;
    p_FormNameChanger.ModalResult := mrOK;
    end;

procedure TFrameAttributesDistribution.btnDelClick(Sender: TObject);
    var
        SelIdx : integer;
    begin
    SelIdx := lbPedLabel.ItemIndex;
    if SelIdx < 0 then
        Exit;

    p_EditingLabelList.Delete(SelIdx);
    lbPedLabel.Items.Delete(SelIdx);
    cbbAttr.Enabled := false;
    seAttrPer.Enabled := false;
    btnDel.Enabled := false;
    end;

procedure TFrameAttributesDistribution.cbbAttrChange(Sender: TObject);
    var
        LblIdx, SelIdx : integer;
    begin
    LblIdx := lbPedLabel.ItemIndex;
    SelIdx := cbbAttr.ItemIndex;
    if (LblIdx < 0) or (SelIdx < 0) then
        Exit;

    seAttrPer.Value := p_EditingLabelList[LblIdx].AttrPerList[SelIdx];
    end;

procedure TFrameAttributesDistribution.seAttrPerChange(Sender: TObject);
    var
        LblIdx, SelIdx, MaxIdx : integer;
    begin
    LblIdx := lbPedLabel.ItemIndex;
    SelIdx := cbbAttr.ItemIndex;
    MaxIdx := cbbAttr.Items.Count - 1;
    if (LblIdx < 0) or (SelIdx < 0) then
        Exit;

    p_EditingLabelList[LblIdx].AttrPerList[SelIdx] := seAttrPer.iValue;
    SeriesAttrDistribution.PieValues[MaxIdx - SelIdx] := p_EditingLabelList[LblIdx].AttrPerList[SelIdx];
    //Visibleをfalse->trueとすることでグラフの描画を更新する
    SeriesAttrDistribution.Visible := false;
    SeriesAttrDistribution.Visible := true;
    end;

procedure TFrameAttributesDistribution.lbPedLabelClick(Sender: TObject);
    var
        SelIdx, MaxIdx, i : integer;
    begin
    SelIdx := lbPedLabel.ItemIndex;
    if SelIdx <= 0 then
        begin
        cbbAttr.Enabled := false;
        seAttrPer.Enabled := false;
        btnDel.Enabled := false;
        end
    else
        begin
        cbbAttr.Enabled := true;
        seAttrPer.Enabled := true;
        btnDel.Enabled := true;
        end;

    if SelIdx < 0 then
        Exit;

    MaxIdx := p_EditingLabelList[SelIdx].AttrPerList.Count - 1;
    for i := p_EditingLabelList[SelIdx].AttrPerList.Count - 1 downto 0 do
        SeriesAttrDistribution.PieValues[MaxIdx - i] := p_EditingLabelList[SelIdx].AttrPerList[i];

    //Visibleをfalse->trueとすることでグラフの描画を更新する
    SeriesAttrDistribution.Visible := false;
    SeriesAttrDistribution.Visible := true;
    cbbAttr.ItemIndex := 0;
    seAttrPer.Value := p_EditingLabelList[SelIdx].AttrPerList[0];
    end;

procedure TFrameAttributesDistribution.ReceiveLabelData;
    var
        i, j   : integer;
        pedOpt : IF8PedestrianProfileOption;
    begin
    for i := 0 to lbPedLabel.Items.Count - 1 do
        begin
        if (i+1) > theApplicationServices.project.NumberOfPedestrianProfile then
            begin
            theApplicationServices.project.AddPedestrianProfileDefalt;
            UpdateAddedProfile;
            end;

        theApplicationServices.project.PedestrianProfileIF[i+1].AttributeLabel := lbPedLabel.Items[i];
        for j := 1 to theApplicationServices.project.PedestrianProfileIF[i+1].numberOfOptions do
            begin
            pedOpt := theApplicationServices.project.PedestrianProfileIF[i+1].option[j];
            pedOpt.Weight := p_EditingLabelList[i].AttrPerList[j - 1];
            theApplicationServices.project.PedestrianProfileIF[i+1].option[j] := pedOpt;
            end;
        end;
    end;

procedure TFrameAttributesDistribution.UpdateAddedProfile;
    var
        AddedProf, RefProf : IF8PedestrianProfile;
        i : integer;
    begin
    AddedProf := theApplicationServices.project.PedestrianProfileIF[theApplicationServices.project.NumberOfPedestrianProfile];
    RefProf   := theApplicationServices.project.PedestrianProfileIF[theApplicationServices.project.NumberOfPedestrianProfile - 1];
    for i := 1 to RefProf.numberOfOptions do
        begin
        if i > AddedProf.numberOfOptions then
            AddedProf.AddOptionFromGUID(RefProf.option[i].GetModel.GUID);

        AddedProf.option[i].Name := RefProf.option[i].Name;
        AddedProf.option[i].YoungerAge := RefProf.option[i].YoungerAge;
        AddedProf.option[i].OlderAge := RefProf.option[i].OlderAge;
        AddedProf.option[i].Gender := RefProf.option[i].Gender;
        AddedProf.option[i].maxSpeed := RefProf.option[i].maxSpeed;
        AddedProf.option[i].TakeBusTime := RefProf.option[i].TakeBusTime;
        AddedProf.option[i].PassThroughTicketGateSec := RefProf.option[i].PassThroughTicketGateSec;
        end;
    end;

procedure TFrameAttributesDistribution.SetEditingLabelData(const aList: TList<TPedestrianData>);
    var
        newData : TPedestrianData;
        pedProf : IF8PedestrianProfile;
        i, j, MaxIdx : integer;
    begin
    for i := 0 to aList.Count - 1 do
        begin
        newData := TPedestrianData.Create;
        newData.IsActive             := aList[i].IsActive;
        newData.LabelName            := aList[i].LabelName;
        newData.PopNum               := aList[i].PopNum;
        newData.PopPer               := aList[i].PopPer;
        for j := 0 to aList[i].AttrPerList.Count - 1 do
            newData.AttrPerList.Add(aList[i].AttrPerList[j]);

        for j := 0 to aList[i].DestPerList.Count - 1 do
            newData.DestPerList.Add(aList[i].DestPerList[j]);

        pedProf := theApplicationServices.project.PedestrianProfileIF[theApplicationServices.project.NumberOfPedestrianProfile];
        if pedProf.numberOfOptions > newData.AttrPerList.Count then
            begin
            while pedProf.numberOfOptions > newData.AttrPerList.Count do
                newData.AttrPerList.Add(0);
            end;

        p_EditingLabelList.Add(newData);
        lbPedLabel.Items.Add(newData.LabelName);
        end;

    if not Assigned(pedProf) then
        pedProf := theApplicationServices.project.PedestrianProfileIF[theApplicationServices.project.NumberOfPedestrianProfile];

    SeriesAttrDistribution.Clear;
    MaxIdx := pedProf.numberOfOptions;
    for i := pedProf.numberOfOptions downto 1 do
        begin
        cbbAttr.Items.Add(pedProf.option[(MaxIdx + 1) - i].Name);
        if p_EditingLabelList.Count > 0 then
            SeriesAttrDistribution.Add(p_EditingLabelList[0].AttrPerList[i - 1], pedProf.option[i].Name)
        else
            SeriesAttrDistribution.Add(100, pedProf.option[i].Name);
        end;

    lbPedLabel.ItemIndex := -1;
    cbbAttr.ItemIndex := -1;
    seAttrPer.Value   := 100;
    end;
end.
