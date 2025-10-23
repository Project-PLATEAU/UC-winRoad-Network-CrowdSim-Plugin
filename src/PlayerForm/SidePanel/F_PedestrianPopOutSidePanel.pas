unit F_PedestrianPopOutSidePanel;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.Generics.Collections,
    System.SysUtils,
    System.DateUtils,
    System.Variants,
    System.Classes,
    System.Math,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.ExtCtrls,
    PluginCore,
    F8GLUtils,
    CrowdSimSidePanelBaseFrame,
    F_PedestrianPopOutSettings,
    F_PedestrianPopPointSettings,
    F_PedestrianOutPointSettings,
    F_NameChanger,
    F_ScheduleSettings,
    SimulationInputUtils,
    PopOutPointList,
    PopOutSchedule,
    FormPedDetailSettings,
    FormPopDetail,
    FormScheduleSettings,
    PedestrianSettingsForm,
    PedestrianCategoryData,
    PedestrianProfileOptionData,
    Vcl.StdCtrls,
    FormPedLabelSettings;

type
    TFramePedestrianPopOutSidePanel = class(TFrame)
        F_PopOutSet: TFramePedestrianPopOutSettings;
        procedure btnAddClick(Sender: TObject);
        procedure btnDeleteClick(Sender: TObject);
        procedure btnChangePointNameClick(Sender: TObject);
        procedure btnChangeOKClick(Sender: TObject);
        procedure btnChangeCancelClick(Sender: TObject);
        procedure ListBoxInOutPointsClick(Sender: TObject);
        procedure cbbPointTypeChange(Sender: TObject);
        procedure btnLookAtClick(Sender: TObject);
        procedure btnChangePointClick(Sender: TObject);
        procedure TimerWaitMainProcTimer(Sender: TObject);
        procedure btnPedDetailClick(Sender: TObject);
        procedure btnDelLabelClick(Sender: TObject);
        procedure btnPopDetailSettingsClick(Sender: TObject);
        procedure btnPopDetailOKClick(Sender: TObject);
        procedure btnPopDetailCancelClick(Sender: TObject);
        procedure sePopIntervalChange(Sender: TObject);
        procedure sePopNumChange(Sender: TObject);
        procedure btnSetPopScheduleClick(Sender: TObject);
        procedure sePopPerHourChange(Sender: TObject);
        procedure btnPopScheSetOKClick(Sender: TObject);
        procedure cbbPopRuleChange(Sender: TObject);
        procedure btnSetOutScheduleClick(Sender: TObject);
        procedure btnOutScheSetOKClick(Sender: TObject);
        procedure seOutIntervalChange(Sender: TObject);
        procedure seOutNumChange(Sender: TObject);
        procedure cbbOutRuleChange(Sender: TObject);
        procedure btnPedSettingClick(Sender: TObject);
        procedure chbVisibleDispersionPointClick(Sender: TObject);
        procedure btnAddPopLabelClick(Sender: TObject);
        procedure btnLabelSettingClick(Sender: TObject);
        procedure btnPedLblOKClick(Sender: TObject);
        procedure btnPedLblCancelClick(Sender: TObject);
        procedure btnSchePopLblAddClick(Sender: TObject);
        procedure btnSchePopLblDelClick(Sender: TObject);
        procedure seLblPopNumChange(Sender: TObject);
        procedure btnAddOutPClick(Sender: TObject);
        procedure lbOutOnlyClick(Sender: TObject);
        procedure lbPopLblsClick(Sender: TObject);
        private
            p_TimerWaitMainProc   : TTimer;
            p_FormNameChanger     : TFormNameChange;
            p_PopOutPointList     : TPopOutPointList;
            p_AllPedestrianLabelList : TList<TPedestrianData>;
            p_IsSelectingMode     : boolean;
            p_IsChangingMode      : boolean;
            p_tmpPopNodeNum       : TNodeNumArray;
            p_tmpOutNodeNum       : TNodeNumArray;
            p_tmpChangeNodeNum    : TNodeNumArray;
            p_FormPopDetail       : TFormPopDetailSettings;
            p_FormSchedule        : TFormSchedule;
            p_FormPedSetting      : TFormPedestrianSettings;
            p_FormPedLblSetting   : TFormPedestrianLabelSettings;
            p_PedestrianProfileOptionList : TPedestrianProfileOptionList;
            p_PedestrianCategoryList      : TPedestrianCategoryList;
            p_rbSwitchedOnly              : boolean;
            p_lbSelectedOnly              : boolean;
            p_IsDispersionSelectingMode   : boolean;
            p_IsDispPopSelectingMode      : boolean;
            p_IsDispOutSelectingMode      : boolean;
            p_IsDispChangingMode          : boolean;
            p_IsOutSelectingMode : boolean;

            function  GetPOPointList: TPopOutPointList;
            procedure SetPOPointList(const aValue: TPopOutPointList);
            function  GetPedLblList: TList<TPedestrianData>;
            procedure SetPedLblList(const aValue: TList<TPedestrianData>);
            function  GetPedestrianProfileOptionList: TPedestrianProfileOptionList;
            procedure SetPedestrianProfileOptionList(const aValue: TPedestrianProfileOptionList);
            function  GetPedestrianCategoryList: TPedestrianCategoryList;
            procedure SetPedestrianCategoryList(const aValue: TPedestrianCategoryList);
            procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
            procedure OnKeyDownDispersion(Sender: TObject; var Key: Word; Shift: TShiftState);
            procedure OnKeyDownDispChange(Sender: TObject; var Key: Word; Shift: TShiftState);
            procedure OnMouseDownDispersion(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
            procedure OnMouseDownDispChange(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
            procedure AddNewData;
            procedure AddNewOutData;
            procedure ChangeData;
            procedure AddDispersionMainPoint;
            procedure ChangeDispersionMainPoint;
            procedure AfterPaintOpenGL;
            procedure AfterPaintOpenGLOnChangeMode;
            procedure Initialize;
            procedure LoadDefaultPedestrianAttribute;
            procedure FinishSelectMode;
            procedure FinishChangeMode;
            procedure UpdatePopParams;
            procedure UpdateOutParams;
            procedure FindTotalIndex(out idx: integer);
            procedure UnRegisterRoadEvent;
            procedure SetPedPopData(const target: TPopOutPoint; nwk: IF8Networks);
            procedure SetPedOutData(const target: TPopOutPoint; nwk: IF8Networks);
            procedure UpdateAddedProfile;
            procedure UpdateListBox;
            procedure UpdateComboBox;
            function  IsExistPoint(const nwkid, ndid: integer; pType: PopOutPointType): boolean;
            procedure UpdateDestPerList;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure SendRenderDispersionData(out aRenderDPList: TPopOutPointList);
            procedure SetPopOutSettingsData;
            procedure ReceiveLabelDataFromProject;
            procedure ReceivePointDataFromProject;
            function  IsNothingPopOrOut: boolean;

            property  POPointList : TPopOutPointList read GetPOPointList write SetPOPointList;
            property  AllPedestrianLabelList : TList<TPedestrianData> read GetPedLblList write SetPedLblList;
            property  PedestrianProfileOptionList : TPedestrianProfileOptionList read GetPedestrianProfileOptionList write SetPedestrianProfileOptionList;
            property  PedestrianCategoryList      : TPedestrianCategoryList      read GetPedestrianCategoryList      write SetPedestrianCategoryList;
        end;

implementation

uses
    PedestrianDestinationDistribution;

{$R *.dfm}

procedure TFramePedestrianPopOutSidePanel.AfterConstruction;

    procedure SetMenusAction;
        begin
        F_PopOutSet.PopSettingsFrame.btnLookAt.OnClick            := btnLookAtClick;
        F_PopOutSet.OutSettingsFrame.btnLookAt.OnClick            := btnLookAtClick;
        F_PopOutSet.PopSettingsFrame.btnSelectPopPoint.OnClick    := btnChangePointClick;
        F_PopOutSet.OutSettingsFrame.btnSelectOutPoint.OnClick    := btnChangePointClick;
        F_PopOutSet.PopSettingsFrame.btnAddLabel.OnClick          := btnAddPopLabelClick;
        F_PopOutSet.PopSettingsFrame.btnDelLabel.OnClick          := btnDelLabelClick;
        F_PopOutSet.PopSettingsFrame.seLabelPopNum.OnChange       := seLblPopNumChange;
        F_PopOutSet.PopSettingsFrame.lbPopLbls.OnClick            := lbPopLblsClick;
        end;

    procedure SetDetailSettingMenusAction;
        begin
        F_PopOutSet.PopSettingsFrame.F_PedPopInterval.btnDetailSettings.OnClick := btnPopDetailSettingsClick;
        F_PopOutSet.PopSettingsFrame.F_PedPopPerHour.btnDetailSettings.OnClick  := btnPopDetailSettingsClick;
        F_PopOutSet.PopSettingsFrame.btnPedDetailSettings.OnClick               := btnPedDetailClick;
        end;

    procedure SetPopParameterMenusAction;
        begin
        F_PopOutSet.PopSettingsFrame.F_PedPopInterval.sePopInterval.OnChange := sePopIntervalChange;
        F_PopOutSet.PopSettingsFrame.F_PedPopInterval.sePopNum.OnChange      := sePopNumChange;
        F_PopOutSet.PopSettingsFrame.F_ScheduleButton.btnSetSchedule.OnClick := btnSetPopScheduleClick;
        F_PopOutSet.PopSettingsFrame.F_PedPopPerHour.sePopPerHour.OnChange   := sePopPerHourChange;
        F_PopOutSet.PopSettingsFrame.cbbSelectPopRule.OnChange               := cbbPopRuleChange;
        F_PopOutSet.PopSettingsFrame.InitializeComboBox;
        end;

    procedure SetOutParameterMenusAction;
        begin
        F_PopOutSet.OutSettingsFrame.F_ScheduleButton.btnSetSchedule.OnClick := btnSetOutScheduleClick;
        F_PopOutSet.OutSettingsFrame.F_PedOutInterval.seOutInterval.OnChange := seOutIntervalChange;
        F_PopOutSet.OutSettingsFrame.F_PedOutInterval.seOutNum.OnChange      := seOutNumChange;
        F_PopOutSet.OutSettingsFrame.cbbSelectOutRule.OnChange               := cbbOutRuleChange;
        F_PopOutSet.OutSettingsFrame.InitializeComboBox;
        end;

    begin
    inherited;

    p_FormNameChanger := TFormNameChange.Create(nil);
    Initialize;
    SetMenusAction;
    SetDetailSettingMenusAction;
    SetPopParameterMenusAction;
    SetOutParameterMenusAction;
    p_FormPedSetting := TFormPedestrianSettings.Create(nil);
    p_rbSwitchedOnly := false;
    p_lbSelectedOnly := false;
    p_IsDispersionSelectingMode := false;
    p_IsDispPopSelectingMode    := false;
    p_IsDispOutSelectingMode    := false;
    p_PopOutPointList := TPopOutPointList.Create;
    p_AllPedestrianLabelList := TList<TPedestrianData>.Create;
    LoadDefaultPedestrianAttribute;
    end;

procedure TFramePedestrianPopOutSidePanel.BeforeDestruction;
    begin
    inherited;

    p_AllPedestrianLabelList := nil;
    FreeAndNil(p_FormNameChanger);
    UnRegisterRoadEvent;
    FreeAndNil(p_PopOutPointList);
    FreeAndNil(p_AllPedestrianLabelList);
    FreeAndNil(p_FormPedSetting);
    end;

procedure TFramePedestrianPopOutSidePanel.UnRegisterRoadEvent;
    var
        m : TMethod;
    begin
    FormMainKeyUpDownProc(m) := OnKeyDown;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainKeyDown, m);
    PluginNotifyProc(m) := AfterPaintOpenGL;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
    PluginNotifyProc(m) := AfterPaintOpenGLOnChangeMode;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
    FormMainOpenGLMouseUpDownProc(m) := OnMouseDownDispersion;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLMouseDown, m);
    FormMainKeyUpDownProc(m) := OnKeyDownDispersion;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainKeyDown, m);
    FormMainOpenGLMouseUpDownProc(m) := OnMouseDownDispChange;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLMouseDown, m);
    FormMainKeyUpDownProc(m) := OnKeyDownDispChange;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainKeyDown, m);
    end;

procedure TFramePedestrianPopOutSidePanel.Initialize;
    begin
    p_tmpPopNodeNum[0] := -1;
    p_tmpPopNodeNum[1] := -1;
    p_tmpOutNodeNum[0] := -1;
    p_tmpOutNodeNum[1] := -1;
    p_tmpChangeNodeNum[0] := -1;
    p_tmpChangeNodeNum[1] := -1;
    end;

procedure TFramePedestrianPopOutSidePanel.LoadDefaultPedestrianAttribute;
    begin
    p_FormPedSetting.F_PedSetting.SendLatestPedProfOptData;
    p_FormPedSetting.F_PedSetting.SetEditPedCategories;
    p_FormPedSetting.F_PedSetting.ReceivePedestrianLabelList(p_AllPedestrianLabelList);
    p_FormPedSetting.F_PedSetting.SetNewCategories;
    p_FormPedSetting.F_PedSetting.ReceiveLatestPedProfOptData;
    p_FormPedSetting.F_PedSetting.SendNewPedestrianLabelList(p_AllPedestrianLabelList);
    end;

procedure TFramePedestrianPopOutSidePanel.FindTotalIndex(out idx: integer);
    var
        i : integer;
    begin
    idx := -1;
    if F_PopOutSet.ListBoxInOutPoints.ItemIndex >= 0 then
        begin
        idx := F_PopOutSet.ListBoxInOutPoints.ItemIndex;
        for i := 0 to POPointList.Data.Count - 1 do
            begin
            if F_PopOutSet.ListBoxInOutPoints.Items[idx] = POPointList.Data[i].Name then
                begin
                idx := i;
                Break;
                end;
            end;
        end
    else if F_PopOutSet.lbOutOnly.ItemIndex >= 0 then
        begin
        idx := F_PopOutSet.lbOutOnly.ItemIndex;
        for i := 0 to POPointList.Data.Count - 1 do
            begin
            if F_PopOutSet.lbOutOnly.Items[idx] = POPointList.Data[i].Name then
                begin
                idx := i;
                Break;
                end;
            end;
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.FinishSelectMode;
    var
        m : TMethod;
    begin
    FormMainKeyUpDownProc(m) := OnKeyDown;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainKeyDown, m);
    PluginNotifyProc(m) := AfterPaintOpenGL;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
    if p_tmpPopNodeNum[0] >= 0 then
        begin
        theApplicationServices.project.FlightWayNwk[p_tmpPopNodeNum[0]].Node[p_tmpPopNodeNum[1]].ReleaseSelecting;
        theApplicationServices.project.FlightWayNwk[p_tmpPopNodeNum[0]].Node[p_tmpPopNodeNum[1]].SelectedOnCrowdSim := false;
        end;

    if p_tmpOutNodeNum[0] >= 0 then
        begin
        theApplicationServices.project.FlightWayNwk[p_tmpOutNodeNum[0]].Node[p_tmpOutNodeNum[1]].ReleaseSelecting;
        theApplicationServices.project.FlightWayNwk[p_tmpOutNodeNum[0]].Node[p_tmpOutNodeNum[1]].SelectedOnCrowdSim := false;
        end;

    Initialize;
    p_IsSelectingMode := false;
    p_IsOutSelectingMode := false;
    F_PopOutSet.Enabled := true;
    end;

procedure TFramePedestrianPopOutSidePanel.btnAddOutPClick(Sender: TObject);
    var
        m : TMethod;
        i, NwkIdx, NdIdx : integer;
    begin
    for i := 0 to POPointList.Data.Count - 1 do
        begin
        NwkIdx := POPointList.Data[i].NodeIdx[0];
        NdIdx  := POPointList.Data[i].NodeIdx[1];
        if not POPointList.Data[i].IsAssignedDispersionPoint then
            theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false
        else
            begin
            theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false;
            POPointList.Data[i].IsFocused := false;
            end;
        end;

    if F_PopOutSet.rbNormalPoint.Checked then
        begin
        F_PopOutSet.Enabled := false;
        p_IsOutSelectingMode := true;
        theApplicationServices.AddDisplayMessage(SELECT_OUT_POINT_MESSAGE_ACTION);
        theApplicationServices.AddDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);

        theApplicationServices.mainForm.SetIsNodeSelectingOnCrowdSim(true);
        FormMainKeyUpDownProc(m) := OnKeyDown;
        theApplicationServices.RegisterEventHandler(_plgFormMainKeyDown, m);
        PluginNotifyProc(m) := AfterPaintOpenGL;
        theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
        end
    else if F_PopOutSet.rbDispersionPoint.Checked then
        begin
        F_PopOutSet.Enabled := false;
        p_IsOutSelectingMode := true;
        p_IsDispersionSelectingMode := true;
        theApplicationServices.AddDisplayMessage(SELECT_DISPERSION_POINT_MESSAGE_ACTION);
        theApplicationServices.AddDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);

        theApplicationServices.mainForm.SetIsNodeSelectingOnCrowdSim(true);
        FormMainKeyUpDownProc(m) := OnKeyDownDispersion;
        theApplicationServices.RegisterEventHandler(_plgFormMainKeyDown, m);
        PluginNotifyProc(m) := AfterPaintOpenGL;
        theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.btnLabelSettingClick(Sender: TObject);
    begin
    p_FormPedLblSetting := TFormPedestrianLabelSettings.Create(nil);
    p_FormPedLblSetting.F_AttrDist.SetEditingLabelData(AllPedestrianLabelList);
    if p_FormPedLblSetting.ShowModal = mrOK then
        btnPedLblOKClick(nil)
    else
        FreeAndNil(p_FormPedLblSetting);
    end;

procedure TFramePedestrianPopOutSidePanel.btnPedLblOKClick(Sender: TObject);
    var
        i, j, k, l : integer;
        newData : TPedestrianData;
        Updated : boolean;
    begin
    p_FormPedLblSetting.F_AttrDist.ReceiveLabelData;
    if AllPedestrianLabelList.Count > 0 then
        begin
        AllPedestrianLabelList.Clear;
        F_PopOutSet.PopSettingsFrame.cbbPedLabel.Items.Clear;
        end;

    for i := 0 to p_FormPedLblSetting.F_AttrDist.EditingLabelList.Count - 1 do
        begin
        newData := TPedestrianData.Create;
        newData.LabelName            := p_FormPedLblSetting.F_AttrDist.EditingLabelList[i].LabelName;
        newData.PopNum               := p_FormPedLblSetting.F_AttrDist.EditingLabelList[i].PopNum;
        newData.PopPer               := p_FormPedLblSetting.F_AttrDist.EditingLabelList[i].PopPer;
        for j := 0 to p_FormPedLblSetting.F_AttrDist.EditingLabelList[i].AttrPerList.Count - 1 do
            newData.AttrPerList.Add(p_FormPedLblSetting.F_AttrDist.EditingLabelList[i].AttrPerList[j]);

        for j := 0 to p_FormPedLblSetting.F_AttrDist.EditingLabelList[i].DestPerList.Count - 1 do
            newData.DestPerList.Add(p_FormPedLblSetting.F_AttrDist.EditingLabelList[i].DestPerList[j]);

        AllPedestrianLabelList.Add(newData);
        F_PopOutSet.PopSettingsFrame.cbbPedLabel.Items.Add(newData.LabelName);
        end;

    for i := 0 to POPointList.Data.Count - 1 do
        begin
        for j := 0 to POPointList.Data[i].PedestrianData.Count - 1 do
            begin
            Updated := false;
            for k := 0 to AllPedestrianLabelList.Count - 1 do
                begin
                if POPointList.Data[i].PedestrianData[j].LabelName = AllPedestrianLabelList[k].LabelName then
                    begin
                    POPointList.Data[i].PedestrianData[j].AttrPerList.Clear;
                    for l := 0 to AllPedestrianLabelList[k].AttrPerList.Count - 1 do
                        POPointList.Data[i].PedestrianData[j].AttrPerList.Add(AllPedestrianLabelList[k].AttrPerList[l]);

                    Updated := true;
                    Break;
                    end;
                end;

            if Updated then
                Continue;

            for k := 0 to AllPedestrianLabelList.Count - 1 do
                begin
                if j = k then
                    begin
                    POPointList.Data[i].PedestrianData[j].LabelName := AllPedestrianLabelList[k].LabelName;
                    POPointList.Data[i].PedestrianData[j].AttrPerList.Clear;
                    for l := 0 to AllPedestrianLabelList[k].AttrPerList.Count - 1 do
                        POPointList.Data[i].PedestrianData[j].AttrPerList.Add(AllPedestrianLabelList[k].AttrPerList[l]);

                    Break;
                    end;
                end;
            end;

        if AllPedestrianLabelList.Count < POPointList.Data[i].PedestrianData.Count then
            begin
            while not (AllPedestrianLabelList.Count = POPointList.Data[i].PedestrianData.Count) do
                POPointList.Data[i].DeletePedData(POPointList.Data[i].PedestrianData.Count - 1); //更新後のリストにラベルが存在しない
            end;
        end;

    p_FormPedLblSetting.Close;
    FreeAndNil(p_FormPedLblSetting);
    end;

procedure TFramePedestrianPopOutSidePanel.btnPedLblCancelClick(Sender: TObject);
    begin
    p_FormPedLblSetting.Close;
    FreeAndNil(p_FormPedLblSetting);
    end;

procedure TFramePedestrianPopOutSidePanel.chbVisibleDispersionPointClick(Sender: TObject);
    begin
    POPointList.IsVisibleDispersionPoint := F_PopOutSet.chbVisibleDispersionPoint.Checked;
    end;

procedure TFramePedestrianPopOutSidePanel.btnPedSettingClick(Sender: TObject);
    begin
    p_FormPedSetting.F_PedSetting.SendLatestPedProfOptData;
    p_FormPedSetting.F_PedSetting.SetEditPedCategories;
    p_FormPedSetting.F_PedSetting.ReceivePedestrianLabelList(p_AllPedestrianLabelList);
    if p_FormPedSetting.ShowModal = mrOK then
        begin
        p_FormPedSetting.F_PedSetting.SetNewCategories;
        p_FormPedSetting.F_PedSetting.ReceiveLatestPedProfOptData;
        p_FormPedSetting.F_PedSetting.SendNewPedestrianLabelList(p_AllPedestrianLabelList);
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.FinishChangeMode;
    var
        m : TMethod;
    begin
    FormMainKeyUpDownProc(m) := OnKeyDown;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainKeyDown, m);
    PluginNotifyProc(m) := AfterPaintOpenGLOnChangeMode;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
    if p_tmpChangeNodeNum[0] >= 0 then
        begin
        theApplicationServices.project.FlightWayNwk[p_tmpChangeNodeNum[0]].Node[p_tmpChangeNodeNum[1]].ReleaseSelecting;
        theApplicationServices.project.FlightWayNwk[p_tmpChangeNodeNum[0]].Node[p_tmpChangeNodeNum[1]].SelectedOnCrowdSim := true;
        end;

    Initialize;
    p_IsChangingMode := false;
    F_PopOutSet.Enabled := true;
    end;

procedure TFramePedestrianPopOutSidePanel.btnAddClick(Sender: TObject);
    var
        m : TMethod;
        i, NwkIdx, NdIdx : integer;
    begin
    F_PopOutSet.ListBoxInOutPoints.ItemIndex := -1;
    F_PopOutSet.lbOutOnly.ItemIndex := -1;
    for i := 0 to POPointList.Data.Count - 1 do
        begin
        NwkIdx := POPointList.Data[i].NodeIdx[0];
        NdIdx  := POPointList.Data[i].NodeIdx[1];
        if not POPointList.Data[i].IsAssignedDispersionPoint then
            theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false
        else
            begin
            theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false;
            POPointList.Data[i].IsFocused := false;
            end;
        end;

    if F_PopOutSet.rbNormalPoint.Checked then
        begin
        F_PopOutSet.Enabled := false;
        p_IsSelectingMode := true;
        theApplicationServices.AddDisplayMessage(SELECT_POP_POINT_MESSAGE_ACTION);
        theApplicationServices.AddDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);

        theApplicationServices.mainForm.SetIsNodeSelectingOnCrowdSim(true);
        FormMainKeyUpDownProc(m) := OnKeyDown;
        theApplicationServices.RegisterEventHandler(_plgFormMainKeyDown, m);
        PluginNotifyProc(m) := AfterPaintOpenGL;
        theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
        end
    else if F_PopOutSet.rbDispersionPoint.Checked then
        begin
        F_PopOutSet.Enabled := false;
        p_IsSelectingMode := true;
        p_IsDispersionSelectingMode := true;
        theApplicationServices.AddDisplayMessage(SELECT_DISPERSION_POINT_MESSAGE_ACTION);
        theApplicationServices.AddDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);

        theApplicationServices.mainForm.SetIsNodeSelectingOnCrowdSim(true);
        FormMainKeyUpDownProc(m) := OnKeyDownDispersion;
        theApplicationServices.RegisterEventHandler(_plgFormMainKeyDown, m);
        PluginNotifyProc(m) := AfterPaintOpenGL;
        theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
    if ((p_IsSelectingMode) or (p_IsOutSelectingMode)) and (Key = VK_ESCAPE) then
        begin
        theApplicationServices.ClearDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
        theApplicationServices.ClearDisplayMessage(SELECT_POP_POINT_MESSAGE_ACTION);
        theApplicationServices.ClearDisplayMessage(SELECT_OUT_POINT_MESSAGE_ACTION);

        FinishSelectMode;
        if not Assigned(p_TimerWaitMainProc) then
            begin
            p_TimerWaitMainProc := TTimer.Create(nil);
            p_TimerWaitMainProc.Interval := 200;
            p_TimerWaitMainProc.OnTimer  := TimerWaitMainProcTimer;
            end;
        end
    else if (p_IsChangingMode) and (Key = VK_ESCAPE) then
        begin
        theApplicationServices.ClearDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
        theApplicationServices.ClearDisplayMessage(CHANGE_POINT_MESSAGE_ACTION);

        FinishChangeMode;
        if not Assigned(p_TimerWaitMainProc) then
            begin
            p_TimerWaitMainProc := TTimer.Create(nil);
            p_TimerWaitMainProc.Interval := 200;
            p_TimerWaitMainProc.OnTimer  := TimerWaitMainProcTimer;
            end;
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.OnKeyDownDispersion(Sender: TObject; var Key: Word; Shift: TShiftState);
    var
        m : TMethod;
    begin
    if ((p_IsDispersionSelectingMode) and ((not p_IsDispPopSelectingMode) and (not p_IsDispOutSelectingMode))) and (Key = VK_ESCAPE) then
        begin
        theApplicationServices.ClearDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
        theApplicationServices.ClearDisplayMessage(SELECT_DISPERSION_POINT_MESSAGE_ACTION);
        PluginNotifyProc(m) := AfterPaintOpenGL;
        theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
        p_IsDispersionSelectingMode := false;
        end
    else if (p_IsDispPopSelectingMode) and (Key = VK_ESCAPE) then
        begin
        if POPointList.Data[POPointList.Data.Count - 1].DispersionPointList.Count < 1 then
            begin
            ShowMessage('発生地点を1つ以上選択してください');
            Exit;
            end;

        theApplicationServices.project.FlightWayNwk[p_tmpPopNodeNum[0]].Node[p_tmpPopNodeNum[1]].ReleaseSelecting;
        theApplicationServices.project.FlightWayNwk[p_tmpPopNodeNum[0]].Node[p_tmpPopNodeNum[1]].SelectedOnCrowdSim := false;
        theApplicationServices.ClearDisplayMessage(EDIT_MODE_MESSAGE_FINISH);
        theApplicationServices.ClearDisplayMessage(SELECT_DISPERSION_POPOUT_POINT_MESSAGE_ACTION);
        FormMainOpenGLMouseUpDownProc(m) := OnMouseDownDispersion;
        theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLMouseDown, m);
        p_IsDispPopSelectingMode := false;
        p_IsDispersionSelectingMode := false;
        Initialize;
        F_PopOutSet.lbOutOnly.ItemIndex := -1;
        F_PopOutSet.ListBoxInOutPoints.ItemIndex := F_PopOutSet.ListBoxInOutPoints.Items.Count - 1;
        if not Assigned (F_PopOutSet.PopSettingsFrame.POPointList) then
            F_PopOutSet.PopSettingsFrame.POPointList := POPointList;

        ListBoxInOutPointsClick(nil);
        end
    else if (p_IsDispOutSelectingMode) and (Key = VK_ESCAPE) then
        begin
        if POPointList.Data[POPointList.Data.Count - 1].DispersionPointList.Count < 1 then
            begin
            ShowMessage('退出地点を1つ以上選択してください');
            Exit;
            end;

        theApplicationServices.project.FlightWayNwk[p_tmpOutNodeNum[0]].Node[p_tmpOutNodeNum[1]].ReleaseSelecting;
        theApplicationServices.project.FlightWayNwk[p_tmpOutNodeNum[0]].Node[p_tmpOutNodeNum[1]].SelectedOnCrowdSim := false;
        theApplicationServices.ClearDisplayMessage(EDIT_MODE_MESSAGE_FINISH);
        theApplicationServices.ClearDisplayMessage(SELECT_DISPERSION_POPOUT_POINT_MESSAGE_ACTION);
        FormMainOpenGLMouseUpDownProc(m) := OnMouseDownDispersion;
        theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLMouseDown, m);
        p_IsDispOutSelectingMode := false;
        p_IsDispersionSelectingMode := false;
        Initialize;
        F_PopOutSet.ListBoxInOutPoints.ItemIndex := -1;
        F_PopOutSet.lbOutOnly.ItemIndex := F_PopOutSet.lbOutOnly.Items.Count - 1;
        if not Assigned (F_PopOutSet.OutSettingsFrame.POPointList) then
            F_PopOutSet.OutSettingsFrame.POPointList := POPointList;

        lbOutOnlyClick(nil);
        end
    else
        Exit;

    FormMainKeyUpDownProc(m) := OnKeyDownDispersion;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainKeyDown, m);
    F_PopOutSet.Enabled := true;
    end;

procedure TFramePedestrianPopOutSidePanel.OnKeyDownDispChange(Sender: TObject; var Key: Word; Shift: TShiftState);
    var
        m : TMethod;
    begin
    if (p_IsDispChangingMode) and (Key = VK_ESCAPE) then
        begin
        theApplicationServices.ClearDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
        theApplicationServices.ClearDisplayMessage(CHANGE_DISPERSION_POINT_MESSAGE_ACTION);
        PluginNotifyProc(m) := AfterPaintOpenGLOnChangeMode;
        theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
        p_IsDispChangingMode := false;
        end
    else if (p_IsDispPopSelectingMode) and (Key = VK_ESCAPE) then
        begin
        if POPointList.Data[POPointList.Data.Count - 1].DispersionPointList.Count < 1 then
            begin
            ShowMessage('発生/退出地点を1つ以上選択してください');
            Exit;
            end;

        theApplicationServices.project.FlightWayNwk[p_tmpChangeNodeNum[0]].Node[p_tmpChangeNodeNum[1]].ReleaseSelecting;
        theApplicationServices.project.FlightWayNwk[p_tmpChangeNodeNum[0]].Node[p_tmpChangeNodeNum[1]].SelectedOnCrowdSim := false;
        theApplicationServices.ClearDisplayMessage(EDIT_MODE_MESSAGE_FINISH);
        theApplicationServices.ClearDisplayMessage(SELECT_DISPERSION_POPOUT_POINT_MESSAGE_ACTION);
        FormMainOpenGLMouseUpDownProc(m) := OnMouseDownDispChange;
        theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLMouseDown, m);
        p_IsDispPopSelectingMode := false;
        end;

    p_tmpChangeNodeNum[0] := -1;
    p_tmpChangeNodeNum[1] := -1;
    FormMainKeyUpDownProc(m) := OnKeyDownDispChange;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainKeyDown, m);
    if not Assigned(p_TimerWaitMainProc) then
        begin
        p_TimerWaitMainProc := TTimer.Create(nil);
        p_TimerWaitMainProc.Interval := 200;
        p_TimerWaitMainProc.OnTimer  := TimerWaitMainProcTimer;
        end;

    F_PopOutSet.Enabled := true;
    end;

procedure TFramePedestrianPopOutSidePanel.OnMouseDownDispersion(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
    if ((not p_IsDispPopSelectingMode) and (not p_IsDispOutSelectingMode)) or (Button <> mbLeft) then
        Exit;

    POPointList.Data[POPointList.Data.Count - 1].DispersionPointList.Add(theApplicationServices.mainForm.OpenGL.Find3DCoordinatesUnderMouse);
    end;

procedure TFramePedestrianPopOutSidePanel.OnMouseDownDispChange(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    var
        SelIdx : integer;
    begin
    FindTotalIndex(SelIdx);
    if ((not p_IsDispPopSelectingMode) or (SelIdx < 0)) or (Button <> mbLeft) then
        Exit;

    POPointList.Data[SelIdx].DispersionPointList.Add(theApplicationServices.mainForm.OpenGL.Find3DCoordinatesUnderMouse);
    end;

procedure TFramePedestrianPopOutSidePanel.btnDeleteClick(Sender: TObject);

    procedure DeleteDestData(const Idx, DelDataIdx: integer);
        var
            j, DestIdx : integer;
        begin
        DestIdx := -1;
        for j := 0 to POPointList.Data.Count - 1 do
            begin
            if (POPointList.Data[j].PointType = _OutPoint)
                and (not ((POPointList.Data[Idx].NodeIdx[0] = POPointList.Data[j].NodeIdx[0]) and (POPointList.Data[Idx].NodeIdx[1] = POPointList.Data[j].NodeIdx[1]))) then
                DestIdx := DestIdx + 1;

            if j = DelDataIdx then
                Break;
            end;

        if DestIdx < 0 then
            Exit;

        POPointList.Data[Idx].DeleteDestination(DestIdx);
        for j := 0 to POPointList.Data[Idx].PedestrianData.Count - 1 do
            POPointList.Data[Idx].PedestrianData[j].DeleteDestPer(DestIdx);
        end;

    var
        SelIdx, NwkIdx, NdIdx, i : integer;
    begin
    FindTotalIndex(SelIdx);
    if SelIdx < 0 then
        Exit;

    if (POPointList.Data[SelIdx].PointType = _OutPoint) then
        begin
        for i := 0 to POPointList.Data.Count - 1 do
            begin
            if (POPointList.Data[i].PointType = _PopPoint) then
                DeleteDestData(i, SelIdx);
            end;
        end;

    NwkIdx := POPointList.Data[SelIdx].NodeIdx[0];
    NdIdx  := POPointList.Data[SelIdx].NodeIdx[1];
    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false;
    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].InOut              := false;
    POPointList.DeleteData(SelIdx);
    if F_PopOutSet.ListBoxInOutPoints.ItemIndex >= 0 then
        F_PopOutSet.ListBoxInOutPoints.Items.Delete(F_PopOutSet.ListBoxInOutPoints.ItemIndex)
    else if F_PopOutSet.lbOutOnly.ItemIndex >= 0 then
        F_PopOutSet.lbOutOnly.Items.Delete(F_PopOutSet.lbOutOnly.ItemIndex);
    end;

procedure TFramePedestrianPopOutSidePanel.cbbPointTypeChange(Sender: TObject);

    procedure DeleteDestData(const Idx: integer);
        var
            i, j, DestIdx : integer;
        begin
        if p_lbSelectedOnly then
            begin
            p_lbSelectedOnly := false;
            Exit;
            end;

        DestIdx := -1;
        for i := 0 to POPointList.Data.Count - 1 do
            begin
            if (POPointList.Data[i].PointType = _OutPoint)
                and (not ((POPointList.Data[Idx].NodeIdx[0] = POPointList.Data[i].NodeIdx[0]) and (POPointList.Data[Idx].NodeIdx[1] = POPointList.Data[i].NodeIdx[1]))) then
                DestIdx := DestIdx + 1;

            if i = Idx then
                Break;
            end;

        if DestIdx < 0 then
            Exit;

        for i := 0 to POPointList.Data.Count - 1 do
            begin
            if ((POPointList.Data[i].PointType = _PopPoint) and (i <> Idx))
                and (not ((POPointList.Data[i].NodeIdx[0] = POPointList.Data[Idx].NodeIdx[0]) and (POPointList.Data[i].NodeIdx[1] = POPointList.Data[Idx].NodeIdx[1]))) then
                begin
                POPointList.Data[i].DeleteDestination(DestIdx);
                for j := 0 to POPointList.Data[i].PedestrianData.Count - 1 do
                    POPointList.Data[i].PedestrianData[j].DeleteDestPer(DestIdx);
                end;
            end;
        end;

    procedure InsertDestData(const Idx: integer);
        var
            i, j, DestIdx : integer;
        begin
        if p_lbSelectedOnly then
            begin
            p_lbSelectedOnly := false;
            Exit;
            end;

        DestIdx := -1;
        for i := 0 to POPointList.Data.Count - 1 do
            begin
            if ((POPointList.Data[i].PointType = _OutPoint) or (i = Idx))
                and (not ((POPointList.Data[Idx].NodeIdx[0] = POPointList.Data[i].NodeIdx[0]) and (POPointList.Data[Idx].NodeIdx[1] = POPointList.Data[i].NodeIdx[1])) ) then
                DestIdx := DestIdx + 1;

            if i = Idx then
                Break;
            end;

        for i := 0 to POPointList.Data.Count - 1 do
            begin
            if (((POPointList.Data[i].PointType = _PopPoint) and (i <> Idx)))
                and (not ((POPointList.Data[i].NodeIdx[0] = POPointList.Data[Idx].NodeIdx[0]) and (POPointList.Data[i].NodeIdx[1] = POPointList.Data[Idx].NodeIdx[1]))) then
                begin
                POPointList.Data[i].InsertDestination(DestIdx, POPointList.Data[Idx].Name);
                for j := 0 to POPointList.Data[i].PedestrianData.Count - 1 do
                    POPointList.Data[i].PedestrianData[j].InsertDestPer(DestIdx);
                end;
            end;
        end;

    var
        SelIdx, i, j : integer;
        mdl : IF8ThreeDeeStudio;
    begin
    FindTotalIndex(SelIdx);
    if SelIdx < 0 then
        Exit;

    if F_PopOutSet.ListBoxInOutPoints.ItemIndex >= 0 then
        begin
        F_PopOutSet.SetPopPointSettingsFrame;
        if (POPointList.Data[SelIdx].PointType <> _PopPoint) then
            DeleteDestData(SelIdx);

        POPointList.Data[SelIdx].PointType := PopOutPointType._PopPoint;
        if POPointList.Data[SelIdx].PopRule = _PopbyInterval then
            F_PopOutSet.PopSettingsFrame.cbbSelectPopRule.ItemIndex := _PByInterval
        else if POPointList.Data[SelIdx].PopRule = _PopbySchedule then
            F_PopOutSet.PopSettingsFrame.cbbSelectPopRule.ItemIndex := _PBySchedule
        else if POPointList.Data[SelIdx].PopRule = _PopPerHour then
            F_PopOutSet.PopSettingsFrame.cbbSelectPopRule.ItemIndex := _PPerHour;

        if POPointList.Data[SelIdx].DestinationList.Count < 1 then
            begin
            for i := 0 to POPointList.Data.Count - 1 do
                begin
                if (POPointList.Data[i].PointType = _OutPoint)
                    and (not ((POPointList.Data[SelIdx].NodeIdx[0] = POPointList.Data[i].NodeIdx[0]) and (POPointList.Data[SelIdx].NodeIdx[1] = POPointList.Data[i].NodeIdx[1]))) then
                    begin
                    POPointList.Data[SelIdx].AddDestination(POPointList.Data[i].Name);
                    if POPointList.Data[SelIdx].PedestrianData.Count > 0 then
                        begin
                        for j := 0 to POPointList.Data[SelIdx].PedestrianData.Count - 1 do
                            POPointList.Data[SelIdx].PedestrianData[j].AddDestPer;
                        end;
                    end;
                end;
            end;

        cbbPopRuleChange(Sender);
        if Assigned(POPointList.Data[SelIdx].BusModel) then
            begin
            for i := 0 to F_PopOutSet.PopSettingsFrame.cbbBusModel.Items.Count - 1 do
                begin
                if not Supports(F_PopOutSet.PopSettingsFrame.cbbBusModel.items.Objects[i], IF8ThreeDeeStudio, mdl) then
                    Continue;

                if (mdl.GUID = POPointList.Data[SelIdx].BusModel.GUID) then
                    begin
                    F_PopOutSet.PopSettingsFrame.cbbBusModel.ItemIndex := i;
                    Break;
                    end;
                end;
            end
        else
            F_PopOutSet.PopSettingsFrame.cbbBusModel.ItemIndex := -1;

        if POPointList.Data[SelIdx].BusRoadIndex > 0 then
            F_PopOutSet.PopSettingsFrame.cbbBusRoad.ItemIndex := POPointList.Data[SelIdx].BusRoadIndex - 1
        else
            F_PopOutSet.PopSettingsFrame.cbbBusRoad.ItemIndex := -1;
        end
    else if F_PopOutSet.lbOutOnly.ItemIndex >= 0 then
        begin
        F_PopOutSet.SetOutPointSettingsFrame;
        if (POPointList.Data[SelIdx].PointType = _PopPoint) then
            InsertDestData(SelIdx);

        POPointList.Data[SelIdx].PointType := PopOutPointType._OutPoint;
        if POPointList.Data[SelIdx].OutRule = _NoRule then
            F_PopOutSet.OutSettingsFrame.cbbSelectOutRule.ItemIndex := _NoORule
        else if POPointList.Data[SelIdx].OutRule = _OutbyInterval then
            F_PopOutSet.OutSettingsFrame.cbbSelectOutRule.ItemIndex := _OByInterval
        else if POPointList.Data[SelIdx].OutRule = _OutbySchedule then
            F_PopOutSet.OutSettingsFrame.cbbSelectOutRule.ItemIndex := _OBySchedule;

        if POPointList.Data[SelIdx].DestinationList.Count > 0 then
            begin
            POPointList.Data[SelIdx].DestinationList.Clear;
            if POPointList.Data[SelIdx].PedestrianData.Count > 0 then
                begin
                for i := 0 to POPointList.Data[SelIdx].PedestrianData.Count - 1 do
                    POPointList.Data[SelIdx].PedestrianData[i].DestPerList.Clear;
                end;
            end;

        cbbOutRuleChange(Sender);
        if Assigned(POPointList.Data[SelIdx].BusModel) then
            begin
            for i := 0 to F_PopOutSet.OutSettingsFrame.cbbBusModel.Items.Count - 1 do
                begin
                if not Supports(F_PopOutSet.OutSettingsFrame.cbbBusModel.items.Objects[i], IF8ThreeDeeStudio, mdl) then
                    Continue;

                if (mdl.GUID = POPointList.Data[SelIdx].BusModel.GUID) then
                    begin
                    F_PopOutSet.OutSettingsFrame.cbbBusModel.ItemIndex := i;
                    Break;
                    end;
                end;
            end
        else
            F_PopOutSet.OutSettingsFrame.cbbBusModel.ItemIndex := -1;

        if POPointList.Data[SelIdx].BusRoadIndex > 0 then
            F_PopOutSet.OutSettingsFrame.cbbBusRoad.ItemIndex := POPointList.Data[SelIdx].BusRoadIndex - 1
        else
            F_PopOutSet.OutSettingsFrame.cbbBusRoad.ItemIndex := -1;
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.ListBoxInOutPointsClick(Sender: TObject);
    var
        SelIdx, i, NwkIdx, NdIdx : integer;
        focus : boolean;
    begin
    F_PopOutSet.lbOutOnly.ItemIndex := -1;
    SelIdx := F_PopOutSet.ListBoxInOutPoints.ItemIndex;
    if not Assigned (F_PopOutSet.PopSettingsFrame.POPointList) then
        F_PopOutSet.PopSettingsFrame.POPointList := POPointList;

    if SelIdx < 0 then
        begin
        for i := 0 to POPointList.Data.Count - 1 do
            begin
            NwkIdx := POPointList.Data[i].NodeIdx[0];
            NdIdx  := POPointList.Data[i].NodeIdx[1];
            if not POPointList.Data[i].IsAssignedDispersionPoint then
                theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false
            else
                begin
                theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false;
                POPointList.Data[i].IsFocused := false;
                end;
            end;

        Exit;
        end
    else
        begin
        for i := 0 to POPointList.Data.Count - 1 do
            begin
            if F_PopOutSet.ListBoxInOutPoints.Items[SelIdx] = POPointList.Data[i].Name then
                begin
                SelIdx := i;
                Break;
                end;
            end;

        for i := 0 to POPointList.Data.Count - 1 do
            begin
            NwkIdx := POPointList.Data[i].NodeIdx[0];
            NdIdx  := POPointList.Data[i].NodeIdx[1];
            if not POPointList.Data[i].IsAssignedDispersionPoint then
                begin
                if i <> SelIdx then
                    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false
                else
                    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := true;
                end
            else
                begin
                if i <> SelIdx then
                    begin
                    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false;
                    focus := false;
                    end
                else
                    begin
                    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := true;
                    focus := true;
                    end;

                POPointList.Data[i].IsFocused := focus;
                end;
            end;
        end;

    F_PopOutSet.PopSettingsFrame.lbIdx := SelIdx;
    FindTotalIndex(SelIdx);
    F_PopOutSet.PopSettingsFrame.ItemName := POPointList.Data[SelIdx].Name;
    p_lbSelectedOnly := true;
    cbbPointTypeChange(Sender);
    p_lbSelectedOnly := false;
    end;

procedure TFramePedestrianPopOutSidePanel.lbOutOnlyClick(Sender: TObject);
    var
        SelIdx, i, NwkIdx, NdIdx : integer;
        focus : boolean;
    begin
    F_PopOutSet.ListBoxInOutPoints.ItemIndex := -1;
    SelIdx := F_PopOutSet.lbOutOnly.ItemIndex;
    if not Assigned (F_PopOutSet.OutSettingsFrame.POPointList) then
        F_PopOutSet.OutSettingsFrame.POPointList := POPointList;

    if SelIdx < 0 then
        begin
        for i := 0 to POPointList.Data.Count - 1 do
            begin
            NwkIdx := POPointList.Data[i].NodeIdx[0];
            NdIdx  := POPointList.Data[i].NodeIdx[1];
            if not POPointList.Data[i].IsAssignedDispersionPoint then
                theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false
            else
                begin
                theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false;
                POPointList.Data[i].IsFocused := false;
                end;
            end;

        Exit;
        end
    else
        begin
        for i := 0 to POPointList.Data.Count - 1 do
            begin
            if F_PopOutSet.lbOutOnly.Items[SelIdx] = POPointList.Data[i].Name then
                begin
                SelIdx := i;
                Break;
                end;
            end;

        for i := 0 to POPointList.Data.Count - 1 do
            begin
            NwkIdx := POPointList.Data[i].NodeIdx[0];
            NdIdx  := POPointList.Data[i].NodeIdx[1];
            if not POPointList.Data[i].IsAssignedDispersionPoint then
                begin
                if i <> SelIdx then
                    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false
                else
                    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := true;
                end
            else
                begin
                if i <> SelIdx then
                    begin
                    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false;
                    focus := false;
                    end
                else
                    begin
                    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := true;
                    focus := true;
                    end;

                POPointList.Data[i].IsFocused := focus;
                end;
            end;
        end;

    F_PopOutSet.OutSettingsFrame.lbIdx := SelIdx;
    FindTotalIndex(SelIdx);
    F_PopOutSet.OutSettingsFrame.ItemName := POPointList.Data[SelIdx].Name;
    p_lbSelectedOnly := true;
    cbbPointTypeChange(Sender);
    p_lbSelectedOnly := false;
    end;

procedure TFramePedestrianPopOutSidePanel.cbbPopRuleChange(Sender: TObject);
    begin
    F_PopOutSet.PopSettingsFrame.cbbSelectPopRuleChange(Sender);
    UpdatePopParams;
    end;

procedure TFramePedestrianPopOutSidePanel.btnChangePointNameClick(Sender: TObject);
    begin
    if not Assigned(p_FormNameChanger) then
        Exit;

    p_FormNameChanger.Caption := '人流発生・退出地点名の変更';
    p_FormNameChanger.lblNewName.Caption := '変更後の地点名：';
    p_FormNameChanger.btnOK.OnClick := btnChangeOKClick;
    p_FormNameChanger.btnCancel.OnClick := btnChangeCancelClick;
    p_FormNameChanger.edtNewName.Text := '';
    p_FormNameChanger.Show;
    end;

procedure TFramePedestrianPopOutSidePanel.btnChangeOKClick(Sender: TObject);

    procedure UpdateDestNames;
        var
            i, j : integer;
        begin
        for i := 0 to POPointList.Data.Count - 1 do
            begin
            if (POPointList.Data[i].PointType = _PopPoint) then
                begin
                POPointList.Data[i].ClearDestination;
                for j := 0 to POPointList.Data.Count - 1 do
                    begin
                    if (POPointList.Data[j].PointType = _OutPoint)
                        and (not ((POPointList.Data[j].NodeIdx[0] = POPointList.Data[i].NodeIdx[0]) and (POPointList.Data[j].NodeIdx[1] = POPointList.Data[i].NodeIdx[1]))) then
                        POPointList.Data[i].AddDestination(POPointList.Data[j].Name);
                    end;
                end;
            end;
        end;

    var
        SelIdx, i, NwkIdx, NdIdx : integer;
        newName : string;
    begin
    FindTotalIndex(SelIdx);
    if SelIdx < 0 then
        Exit;

    if F_PopOutSet.ListBoxInOutPoints.ItemIndex >= 0 then
        begin
        newName := p_FormNameChanger.edtNewName.Text + '_' +DEFAULT_POP_POINT_NAME;
        for i := 0 to POPointList.Data.Count - 1 do
            begin
            if (i <> SelIdx) and (POPointList.Data[i].Name = newName) then
                begin
                ShowMessage('同名の地点が存在します');
                Exit;
                end;
            end;

        POPointList.Data[SelIdx].Name := newName;
        F_PopOutSet.ListBoxInOutPoints.Items[F_PopOutSet.ListBoxInOutPoints.ItemIndex] := newName;
        NwkIdx := POPointList.Data[SelIdx].NodeIdx[0];
        NdIdx  := POPointList.Data[SelIdx].NodeIdx[1];
        theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].Name := p_FormNameChanger.edtNewName.Text;
        end
    else if F_PopOutSet.lbOutOnly.ItemIndex >= 0 then
        begin
        newName := p_FormNameChanger.edtNewName.Text + '_' +DEFAULT_OUT_POINT_NAME;
        for i := 0 to POPointList.Data.Count - 1 do
            begin
            if (i <> SelIdx) and (POPointList.Data[i].Name = newName) then
                begin
                ShowMessage('同名の地点が存在します');
                Exit;
                end;
            end;

        POPointList.Data[SelIdx].Name := newName;
        F_PopOutSet.lbOutOnly.Items[F_PopOutSet.lbOutOnly.ItemIndex] := newName;
        NwkIdx := POPointList.Data[SelIdx].NodeIdx[0];
        NdIdx  := POPointList.Data[SelIdx].NodeIdx[1];
        theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].Name := p_FormNameChanger.edtNewName.Text;
        end;

    UpdateDestNames;
    p_FormNameChanger.Close;
    end;

procedure TFramePedestrianPopOutSidePanel.btnChangeCancelClick(Sender: TObject);
    begin
    p_FormNameChanger.Close;
    end;

procedure TFramePedestrianPopOutSidePanel.UpdatePopParams;
    var
        PoiIdx, i : integer;
    begin
    FindTotalIndex(PoiIdx);
    if PoiIdx < 0 then
        Exit;

    if POPointList.Data[PoiIdx].PointAttr = _BusTerminal then
        F_PopOutSet.PopSettingsFrame.rbBusTerminal.Checked := true
    else if POPointList.Data[PoiIdx].PointAttr = _TicketGate then
        F_PopOutSet.PopSettingsFrame.rbTicketGate.Checked := true
    else
        F_PopOutSet.PopSettingsFrame.rbNormal.Checked := true;

    if F_PopOutSet.PopSettingsFrame.cbbSelectPopRule.ItemIndex = _PByInterval then
        begin
        POPointList.Data[PoiIdx].PopRule := _PopbyInterval;
        F_PopOutSet.PopSettingsFrame.F_PedPopInterval.sePopInterval.Value := POPointList.Data[PoiIdx].PopInterval;
        F_PopOutSet.PopSettingsFrame.F_PedPopInterval.sePopNum.Value := POPointList.Data[PoiIdx].PopNumOnInterval;
        if F_PopOutSet.PopSettingsFrame.lbPopLbls.Items.Count > 0 then
            F_PopOutSet.PopSettingsFrame.lbPopLbls.Items.Clear;

        for i := 0 to POPointList.Data[PoiIdx].PedestrianData.Count - 1 do
            F_PopOutSet.PopSettingsFrame.lbPopLbls.Items.Add(POPointList.Data[PoiIdx].PedestrianData[i].LabelName);
        end
    else if F_PopOutSet.PopSettingsFrame.cbbSelectPopRule.ItemIndex = _PBySchedule then
        begin
        POPointList.Data[PoiIdx].PopRule := _PopbySchedule;
        if F_PopOutSet.PopSettingsFrame.lbPopLbls.Items.Count > 0 then
            F_PopOutSet.PopSettingsFrame.lbPopLbls.Items.Clear;

        for i := 0 to POPointList.Data[PoiIdx].PedestrianData.Count - 1 do
            F_PopOutSet.PopSettingsFrame.lbPopLbls.Items.Add(POPointList.Data[PoiIdx].PedestrianData[i].LabelName);
        end
    else if F_PopOutSet.PopSettingsFrame.cbbSelectPopRule.ItemIndex = _PPerHour then
        begin
        POPointList.Data[PoiIdx].PopRule := _PopPerHour;
        F_PopOutSet.PopSettingsFrame.F_PedPopPerHour.sePopPerHour.Value := POPointList.Data[PoiIdx].PopPerHour;
        if F_PopOutSet.PopSettingsFrame.lbPopLbls.Items.Count > 0 then
            F_PopOutSet.PopSettingsFrame.lbPopLbls.Items.Clear;

        for i := 0 to POPointList.Data[PoiIdx].PedestrianData.Count - 1 do
            F_PopOutSet.PopSettingsFrame.lbPopLbls.Items.Add(POPointList.Data[PoiIdx].PedestrianData[i].LabelName);
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.UpdateOutParams;
    var
        SelIdx : integer;
    begin
    FindTotalIndex(SelIdx);
    if SelIdx < 0 then
        Exit;

    if POPointList.Data[SelIdx].PointAttr = _BusTerminal then
        F_PopOutSet.OutSettingsFrame.rbBusTerminal.Checked := true
    else if POPointList.Data[SelIdx].PointAttr = _TicketGate then
        F_PopOutSet.OutSettingsFrame.rbTicketGate.Checked := true
    else
        F_PopOutSet.OutSettingsFrame.rbNormal.Checked := true;

    if F_PopOutSet.OutSettingsFrame.cbbSelectOutRule.ItemIndex = _NoORule then
        begin
        POPointList.Data[SelIdx].OutRule := _NoRule;
        end
    else if F_PopOutSet.OutSettingsFrame.cbbSelectOutRule.ItemIndex = _OBySchedule then
        begin
        POPointList.Data[SelIdx].OutRule := _OutbySchedule;
        end
    else if F_PopOutSet.OutSettingsFrame.cbbSelectOutRule.ItemIndex = _OByInterval then
        begin
        POPointList.Data[SelIdx].OutRule := _OutbyInterval;
        F_PopOutSet.OutSettingsFrame.F_PedOutInterval.seOutInterval.Value := POPointList.Data[SelIdx].OutInterval;
        F_PopOutSet.OutSettingsFrame.F_PedOutInterval.seOutNum.Value := POPointList.Data[SelIdx].OutNumOnInterval;
        end;
    end;

function TFramePedestrianPopOutSidePanel.GetPOPointList: TPopOutPointList;
    begin
    Result := p_PopOutPointList;
    end;

procedure TFramePedestrianPopOutSidePanel.SetPOPointList(const aValue: TPopOutPointList);
    begin
    p_PopOutPointList := aValue;
    F_PopOutSet.PopSettingsFrame.POPointList := POPointList;
    F_PopOutSet.OutSettingsFrame.POPointList := POPointList;
    end;

function TFramePedestrianPopOutSidePanel.GetPedLblList: TList<TPedestrianData>;
    begin
    Result := p_AllPedestrianLabelList;
    end;

procedure TFramePedestrianPopOutSidePanel.SetPedLblList(const aValue: TList<TPedestrianData>);
    begin
    p_AllPedestrianLabelList := aValue;
    end;

function TFramePedestrianPopOutSidePanel.GetPedestrianProfileOptionList: TPedestrianProfileOptionList;
    begin
    result := p_PedestrianProfileOptionList;
    end;

procedure TFramePedestrianPopOutSidePanel.SetPedestrianProfileOptionList(const aValue: TPedestrianProfileOptionList);
    begin
    p_PedestrianProfileOptionList := aValue;
    end;

function TFramePedestrianPopOutSidePanel.GetPedestrianCategoryList: TPedestrianCategoryList;
    begin
    result := p_PedestrianCategoryList;
    end;

procedure TFramePedestrianPopOutSidePanel.SetPedestrianCategoryList(const aValue: TPedestrianCategoryList);
    begin
    p_PedestrianCategoryList := aValue;
    end;

procedure TFramePedestrianPopOutSidePanel.TimerWaitMainProcTimer(Sender: TObject);
    begin
    theApplicationServices.mainForm.SetIsNodeSelectingOnCrowdSim(false);
    p_TimerWaitMainProc.OnTimer := nil;
    FreeAndNil(p_TimerWaitMainProc);
    end;

procedure TFramePedestrianPopOutSidePanel.AddNewData;
    var
        newData      : TPopOutPoint;
        i, j, newNum : integer;
        newName, tmpName : string;
    begin
    if (p_tmpPopNodeNum[0] = -1) then
        Exit;

    newName := theApplicationServices.project.FlightWayNwk[p_tmpPopNodeNum[0]].Node[p_tmpPopNodeNum[1]].Name + '_' + DEFAULT_POP_POINT_NAME;
    newNum := 0;
    tmpName := newName;
    for i := 0 to F_PopOutSet.ListBoxInOutPoints.Items.Count - 1 do
        begin
        if tmpName = F_PopOutSet.ListBoxInOutPoints.Items[i] then
            begin
            newNum := newNum + 1;
            tmpName := newName + newNum.ToString;
            end;
        end;

    if newNum > 0 then
        newName := newName + newNum.ToString;

    newData := TPopOutPoint.Create(newName);
    newData.NodeIdx   := p_tmpPopNodeNum;
    newData.PointType := PopOutPointType._PopPoint;
    F_PopOutSet.ListBoxInOutPoints.Items.Add(newName);
    POPointList.Data.Add(newData);
    for i := 0 to POPointList.Data.Count - 1 do
        begin
        if (POPointList.Data[i].PointType = _OutPoint)
            and (not ((POPointList.Data[POPointList.Data.Count - 1].NodeIdx[0] = POPointList.Data[i].NodeIdx[0]) and (POPointList.Data[POPointList.Data.Count - 1].NodeIdx[1] = POPointList.Data[i].NodeIdx[1]))) then
            begin
            POPointList.Data[POPointList.Data.Count - 1].AddDestination(POPointList.Data[i].Name);
            if POPointList.Data[POPointList.Data.Count - 1].PedestrianData.Count > 0 then
                begin
                for j := 0 to POPointList.Data[POPointList.Data.Count - 1].PedestrianData.Count - 1 do
                    POPointList.Data[POPointList.Data.Count - 1].PedestrianData[j].AddDestPer;
                end;
            end;
        end;

    theApplicationServices.project.FlightWayNwk[p_tmpPopNodeNum[0]].Node[p_tmpPopNodeNum[1]].InOut := true;
    FinishSelectMode;
    if not Assigned(p_TimerWaitMainProc) then
        begin
        p_TimerWaitMainProc := TTimer.Create(nil);
        p_TimerWaitMainProc.Interval := 200;
        p_TimerWaitMainProc.OnTimer  := TimerWaitMainProcTimer;
        end;

    F_PopOutSet.ListBoxInOutPoints.ItemIndex := F_PopOutSet.ListBoxInOutPoints.Items.Count - 1;
    F_PopOutSet.lbOutOnly.ItemIndex := -1;
    ListBoxInOutPointsClick(nil);
    if not Assigned (F_PopOutSet.PopSettingsFrame.POPointList) then
        F_PopOutSet.PopSettingsFrame.POPointList := POPointList;
    end;

procedure TFramePedestrianPopOutSidePanel.AddNewOutData;
    var
        newData      : TPopOutPoint;
        i, j, newNum : integer;
        newName, tmpName : string;
    begin
    if (p_tmpOutNodeNum[0] = -1) then
        Exit;

    newName := theApplicationServices.project.FlightWayNwk[p_tmpOutNodeNum[0]].Node[p_tmpOutNodeNum[1]].Name + '_' + DEFAULT_OUT_POINT_NAME;
    newNum := 0;
    tmpName := newName;
    for i := 0 to F_PopOutSet.lbOutOnly.Items.Count - 1 do
        begin
        if tmpName = F_PopOutSet.lbOutOnly.Items[i] then
            begin
            newNum := newNum + 1;
            tmpName := newName + newNum.ToString;
            end;
        end;

    if newNum > 0 then
        newName := newName + newNum.ToString;

    newData := TPopOutPoint.Create(newName);
    newData.NodeIdx   := p_tmpOutNodeNum;
    newData.PointType := PopOutPointType._OutPoint;
    F_PopOutSet.lbOutOnly.Items.Add(newName);
    POPointList.Data.Add(newData);
    for i := 0 to POPointList.Data.Count - 1 do
        begin
        if (POPointList.Data[i].PointType = _PopPoint)
            and (not ((POPointList.Data[POPointList.Data.Count - 1].NodeIdx[0] = POPointList.Data[i].NodeIdx[0]) and (POPointList.Data[POPointList.Data.Count - 1].NodeIdx[1] = POPointList.Data[i].NodeIdx[1]))) then
            begin
            POPointList.Data[i].AddDestination(newName);
            if POPointList.Data[i].PedestrianData.Count > 0 then
                begin
                for j := 0 to POPointList.Data[i].PedestrianData.Count - 1 do
                    POPointList.Data[i].PedestrianData[j].AddDestPer;
                end;
            end;
        end;

    theApplicationServices.project.FlightWayNwk[p_tmpOutNodeNum[0]].Node[p_tmpOutNodeNum[1]].InOut := true;
    FinishSelectMode;
    if not Assigned(p_TimerWaitMainProc) then
        begin
        p_TimerWaitMainProc := TTimer.Create(nil);
        p_TimerWaitMainProc.Interval := 200;
        p_TimerWaitMainProc.OnTimer  := TimerWaitMainProcTimer;
        end;

    F_PopOutSet.ListBoxInOutPoints.ItemIndex := -1;
    F_PopOutSet.lbOutOnly.ItemIndex := F_PopOutSet.lbOutOnly.Items.Count - 1;
    lbOutOnlyClick(nil);
    if not Assigned (F_PopOutSet.OutSettingsFrame.POPointList) then
        F_PopOutSet.OutSettingsFrame.POPointList := POPointList;
    end;

procedure TFramePedestrianPopOutSidePanel.ChangeData;
    var
        SelIdx, NwkIdx, NdIdx : integer;
        ChangeIdx : TNodeNumArray;
    begin
    ChangeIdx[0] := p_tmpChangeNodeNum[0];
    ChangeIdx[1] := p_tmpChangeNodeNum[1];
    FindTotalIndex(SelIdx);
    NwkIdx := POPointList.Data[SelIdx].NodeIdx[0];
    NdIdx  := POPointList.Data[SelIdx].NodeIdx[1];
    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false;
    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].InOut := false;

    POPointList.Data[SelIdx].NodeIdx := ChangeIdx;
    NwkIdx := ChangeIdx[0];
    NdIdx  := ChangeIdx[1];
    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].InOut := true;
    FinishChangeMode;
    if not Assigned(p_TimerWaitMainProc) then
        begin
        p_TimerWaitMainProc := TTimer.Create(nil);
        p_TimerWaitMainProc.Interval := 200;
        p_TimerWaitMainProc.OnTimer  := TimerWaitMainProcTimer;
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.AddDispersionMainPoint;
    var
        newData      : TPopOutPoint;
        i, j, newNum : integer;
        newName, tmpName : string;
        m            : TMethod;
    begin
    if p_IsSelectingMode then
        begin
        newName := theApplicationServices.project.FlightWayNwk[p_tmpPopNodeNum[0]].Node[p_tmpPopNodeNum[1]].Name + '_' + DEFAULT_POP_POINT_NAME;
        newNum := 0;
        tmpName := newName;
        for i := 0 to F_PopOutSet.ListBoxInOutPoints.Items.Count - 1 do
            begin
            if tmpName = F_PopOutSet.ListBoxInOutPoints.Items[i] then
                begin
                newNum := newNum + 1;
                tmpName := newName + newNum.ToString;
                end;
            end;

        if newNum > 0 then
            newName := newName + newNum.ToString;

        newData := TPopOutPoint.Create(newName);
        newData.NodeIdx   := p_tmpPopNodeNum;
        newData.PointType := PopOutPointType._PopPoint;
        F_PopOutSet.ListBoxInOutPoints.Items.Add(newName);
        POPointList.Data.Add(newData);
        for i := 0 to POPointList.Data.Count - 1 do
            begin
            if (POPointList.Data[i].PointType = _OutPoint)
                and (not ((POPointList.Data[POPointList.Data.Count - 1].NodeIdx[0] = POPointList.Data[i].NodeIdx[0]) and (POPointList.Data[POPointList.Data.Count - 1].NodeIdx[1] = POPointList.Data[i].NodeIdx[1]))) then
                begin
                POPointList.Data[POPointList.Data.Count - 1].AddDestination(POPointList.Data[i].Name);
                if POPointList.Data[POPointList.Data.Count - 1].PedestrianData.Count > 0 then
                    begin
                    for j := 0 to POPointList.Data[POPointList.Data.Count - 1].PedestrianData.Count - 1 do
                        POPointList.Data[POPointList.Data.Count - 1].PedestrianData[j].AddDestPer;
                    end;
                end;
            end;

        POPointList.Data[POPointList.Data.Count - 1].IsAssignedDispersionPoint := true;
        theApplicationServices.project.FlightWayNwk[p_tmpPopNodeNum[0]].Node[p_tmpPopNodeNum[1]].InOut := true;
        theApplicationServices.ClearDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
        theApplicationServices.ClearDisplayMessage(SELECT_DISPERSION_POINT_MESSAGE_ACTION);
        FormMainKeyUpDownProc(m) := OnKeyDown;
        theApplicationServices.UnRegisterEventHandler(_plgFormMainKeyDown, m);
        PluginNotifyProc(m) := AfterPaintOpenGL;
        theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
        p_IsSelectingMode := false;
        end
    else if p_IsOutSelectingMode then
        begin
        newName := theApplicationServices.project.FlightWayNwk[p_tmpOutNodeNum[0]].Node[p_tmpOutNodeNum[1]].Name + '_' + DEFAULT_OUT_POINT_NAME;
        newNum := 0;
        tmpName := newName;
        for i := 0 to F_PopOutSet.lbOutOnly.Items.Count - 1 do
            begin
            if tmpName = F_PopOutSet.lbOutOnly.Items[i] then
                begin
                newNum := newNum + 1;
                tmpName := newName + newNum.ToString;
                end;
            end;

        if newNum > 0 then
            newName := newName + newNum.ToString;

        newData := TPopOutPoint.Create(newName);
        newData.NodeIdx   := p_tmpOutNodeNum;
        newData.PointType := PopOutPointType._OutPoint;
        F_PopOutSet.lbOutOnly.Items.Add(newName);
        POPointList.Data.Add(newData);
        for i := 0 to POPointList.Data.Count - 1 do
            begin
            if (POPointList.Data[i].PointType = _PopPoint)
                and (not ((POPointList.Data[POPointList.Data.Count - 1].NodeIdx[0] = POPointList.Data[i].NodeIdx[0]) and (POPointList.Data[POPointList.Data.Count - 1].NodeIdx[1] = POPointList.Data[i].NodeIdx[1]))) then
                begin
                POPointList.Data[i].AddDestination(newName);
                if POPointList.Data[i].PedestrianData.Count > 0 then
                    begin
                    for j := 0 to POPointList.Data[i].PedestrianData.Count - 1 do
                        POPointList.Data[i].PedestrianData[j].AddDestPer;
                    end;
                end;
            end;

        POPointList.Data[POPointList.Data.Count - 1].IsAssignedDispersionPoint := true;
        theApplicationServices.project.FlightWayNwk[p_tmpOutNodeNum[0]].Node[p_tmpOutNodeNum[1]].InOut := true;
        theApplicationServices.ClearDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
        theApplicationServices.ClearDisplayMessage(SELECT_DISPERSION_POINT_MESSAGE_ACTION);
        FormMainKeyUpDownProc(m) := OnKeyDown;
        theApplicationServices.UnRegisterEventHandler(_plgFormMainKeyDown, m);
        PluginNotifyProc(m) := AfterPaintOpenGL;
        theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
        p_IsOutSelectingMode := false;
        end;

    if not Assigned(p_TimerWaitMainProc) then
        begin
        p_TimerWaitMainProc := TTimer.Create(nil);
        p_TimerWaitMainProc.Interval := 200;
        p_TimerWaitMainProc.OnTimer  := TimerWaitMainProcTimer;
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.ChangeDispersionMainPoint;
    var
        SelIdx, NwkIdx, NdIdx : integer;
        ChangeIdx : TNodeNumArray;
        m : TMethod;
    begin
    FindTotalIndex(SelIdx);
    if SelIdx < 0 then
        Exit;

    ChangeIdx[0] := p_tmpChangeNodeNum[0];
    ChangeIdx[1] := p_tmpChangeNodeNum[1];
    NwkIdx := POPointList.Data[SelIdx].NodeIdx[0];
    NdIdx  := POPointList.Data[SelIdx].NodeIdx[1];
    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false;
    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].InOut := false;

    POPointList.Data[SelIdx].NodeIdx := ChangeIdx;
    NwkIdx := ChangeIdx[0];
    NdIdx  := ChangeIdx[1];
    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].InOut := true;
    POPointList.Data[SelIdx].DispersionPointList.Clear;

    theApplicationServices.ClearDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
    theApplicationServices.ClearDisplayMessage(CHANGE_DISPERSION_POINT_MESSAGE_ACTION);
    PluginNotifyProc(m) := AfterPaintOpenGLOnChangeMode;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);

    theApplicationServices.AddDisplayMessage(SELECT_DISPERSION_POPOUT_POINT_MESSAGE_ACTION);
    theApplicationServices.AddDisplayMessage(EDIT_MODE_MESSAGE_FINISH);
    FormMainOpenGLMouseUpDownProc(m) := OnMouseDownDispChange;
    theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLMouseDown, m);
    p_IsDispChangingMode := false;
    end;

procedure TFramePedestrianPopOutSidePanel.btnLookAtClick(Sender: TObject);
    const
        EYE_HEIGHT = 50;
    var
        SelIdx, NwkIdx, NdIdx : integer;
        eye, view : GLPointType;
    begin
    FindTotalIndex(SelIdx);
    if SelIdx < 0 then
        Exit;

    NwkIdx := POPointList.Data[SelIdx].NodeIdx[0];
    NdIdx  := POPointList.Data[SelIdx].NodeIdx[1];
    eye[_x]  := theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].position[_x];
    eye[_y]  := theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].position[_y] + EYE_HEIGHT;
    eye[_z]  := theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].position[_z];
    view[_x] := theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].position[_x];
    view[_y] := theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].position[_y];
    view[_z] := theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].position[_z];
    theApplicationServices.MoveCameraTo(eye, view, 0);
    theApplicationServices.mainForm.openGL.DoMouseDown(mbLeft, [], 0, 0);
    theApplicationServices.mainForm.openGL.DoMouseMove([], 1, 1);
    theApplicationServices.mainForm.openGL.DoMouseUp(mbLeft, [], 1, 1);
    end;

procedure TFramePedestrianPopOutSidePanel.btnChangePointClick(Sender: TObject);
    var
        m : TMethod;
        SelIdx : integer;
    begin
    FindTotalIndex(SelIdx);
    if SelIdx < 0 then
        Exit;

    if POPointList.Data[SelIdx].IsAssignedDispersionPoint then
        begin
        F_PopOutSet.Enabled  := false;
        p_IsChangingMode    := true;
        p_IsDispChangingMode := true;
        theApplicationServices.AddDisplayMessage(CHANGE_DISPERSION_POINT_MESSAGE_ACTION);
        theApplicationServices.AddDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);

        theApplicationServices.mainForm.SetIsNodeSelectingOnCrowdSim(true);
        FormMainKeyUpDownProc(m) := OnKeyDownDispChange;
        theApplicationServices.RegisterEventHandler(_plgFormMainKeyDown, m);
        PluginNotifyProc(m) := AfterPaintOpenGLOnChangeMode;
        theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
        Exit;
        end;

    F_PopOutSet.Enabled := false;
    p_IsChangingMode    := true;
    theApplicationServices.AddDisplayMessage(CHANGE_POINT_MESSAGE_ACTION);
    theApplicationServices.AddDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);

    theApplicationServices.mainForm.SetIsNodeSelectingOnCrowdSim(true);
    FormMainKeyUpDownProc(m) := OnKeyDown;
    theApplicationServices.RegisterEventHandler(_plgFormMainKeyDown, m);
    PluginNotifyProc(m) := AfterPaintOpenGLOnChangeMode;
    theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
    end;

procedure TFramePedestrianPopOutSidePanel.btnPedDetailClick(Sender: TObject);
    function SetData(const aEditingData: TPedestrianDistinationDistribution; out aLabelName: String): boolean;
        var
            i : integer;
            PoiIdx, LblIdx : integer;
            newItem: TPedestrianDistinationDistributionItem;
        begin
        Result := false;
        FindTotalIndex(PoiIdx);
        LblIdx := -1;
        if Assigned(p_FormSchedule) then
            begin
            LblIdx := p_FormSchedule.F_ScheSet.lbPopLbls.ItemIndex;
            if LblIdx >= 0 then
                aLabelName := p_FormSchedule.F_ScheSet.lbPopLbls.Items[lblIdx];
            end
        else if F_PopOutSet.PopSettingsFrame.Visible then
            begin
            LblIdx := F_PopOutSet.PopSettingsFrame.lbPopLbls.ItemIndex;
            if LblIdx >= 0 then
                aLabelName := F_PopOutSet.PopSettingsFrame.lbPopLbls.Items[lblIdx];
            end;

        if LblIdx < 0 then
            Exit;

        for i := 0 to POPointList.Data[PoiIdx].DestinationList.Count - 1 do
            begin
            newItem.Name := POPointList.Data[PoiIdx].DestinationList[i];
            newItem.Rate := POPointList.Data[PoiIdx].PedestrianData[LblIdx].DestPerList[i];
            aEditingData.AddDistination(newItem);
            end;
        Result := true;
        end;

    procedure ApplyData(const aEditingData: TPedestrianDistinationDistribution);
        var
            i, poiIdx, lblIdx : integer;
            newList: TList<Double>;
        begin
        FindTotalIndex(poiIdx);
        lblIdx := -1;
        if Assigned(p_FormSchedule) then
            lblIdx := p_FormSchedule.F_ScheSet.lbPopLbls.ItemIndex
        else if F_PopOutSet.PopSettingsFrame.Visible then
            lblIdx := F_PopOutSet.PopSettingsFrame.lbPopLbls.ItemIndex;

        newList := TList<Double>.Create;
        for i := 0 to aEditingData.ItemCount - 1 do
            newList.Add(aEditingData[i].Rate);

        case POPointList.Data[poiIdx].PopRule of
            _PopPerHour:
                POPointList.Data[poiIdx].PedestrianData[lblIdx].SetNewDestPers(newList);
            else
                begin
                for i := 0 to newList.Count - 1 do
                    p_PopOutPointList.Data[poiIdx].PedestrianData[lblIdx].SetNewDestPer(i, newList[i]);
                end;
            end;
        end;
    var
        labelName: String;
        editForm: TFormPedestrianDetailSettings;
        editingData: TPedestrianDistinationDistribution;
    begin
    editForm := TFormPedestrianDetailSettings.Create(nil);
    editingData := TPedestrianDistinationDistribution.Create;
    try
        if setData(editingData, labelName) then
            begin
            editForm.ChangePedestrianLabel(labelName, editingData);

            if editForm.ShowModal = mrOk then
                ApplyData(editingData);
            end;
    finally
        FreeAndNil(editForm);
        FreeAndNil(editingData);
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.btnAddPopLabelClick(Sender: TObject);
    var
        MaxPedDataIdx, PoiIdx, LblIdx, i, PopSum : integer;
    begin
    if F_PopOutSet.PopSettingsFrame.Visible then
        begin
        FindTotalIndex(PoiIdx);
        LblIdx := F_PopOutSet.PopSettingsFrame.cbbPedLabel.ItemIndex;
        if LblIdx < 0 then
            Exit;

        for i := 0 to F_PopOutSet.PopSettingsFrame.lbPopLbls.Items.Count - 1 do
            begin
            if F_PopOutSet.PopSettingsFrame.lbPopLbls.Items[i] = AllPedestrianLabelList[LblIdx].LabelName then
                begin
                ShowMessage('追加済みの人流ラベルです');
                Exit;
                end;
            end;

        POPointList.Data[PoiIdx].AddPedData(AllPedestrianLabelList[LblIdx]);
        F_PopOutSet.PopSettingsFrame.lbPopLbls.Items.Add(AllPedestrianLabelList[LblIdx].LabelName);
        MaxPedDataIdx := POPointList.Data[PoiIdx].PedestrianData.Count - 1;
        for i := 0 to POPointList.Data[PoiIdx].DestinationList.Count - 1 do
            POPointList.Data[PoiIdx].PedestrianData[MaxPedDataIdx].AddDestPer;

        for i := 0 to POPointList.Data[PoiIdx].PopSchedule.Data.Count - 1 do
            POPointList.Data[PoiIdx].PopSchedule.Data[i].LblPopNums.Add(0);

        PopSum := 0;
        for i := 0 to POPointList.Data[PoiIdx].PedestrianData.Count - 1 do
            PopSum := PopSum + POPointList.Data[PoiIdx].PedestrianData[i].PopNum;

        if POPointList.Data[PoiIdx].PopRule = _PopbyInterval then
            F_PopOutSet.PopSettingsFrame.F_PedPopInterval.sePopNum.Value := PopSum
        else if POPointList.Data[PoiIdx].PopRule = _PopPerHour then
            F_PopOutSet.PopSettingsFrame.F_PedPopPerHour.sePopPerHour.Value := PopSum;
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.btnDelLabelClick(Sender: TObject);
    var
        PoiIdx, SelIdx : integer;
    begin
    FindTotalIndex(PoiIdx);
    SelIdx := -1;
    if F_PopOutSet.PopSettingsFrame.Visible then
        SelIdx := F_PopOutSet.PopSettingsFrame.lbPopLbls.ItemIndex;

    if (PoiIdx < 0) or (SelIdx < 0) then
        Exit;

    POPointList.Data[PoiIdx].DeletePedData(SelIdx);
    if F_PopOutSet.PopSettingsFrame.Visible then
        begin
        F_PopOutSet.PopSettingsFrame.lbPopLbls.Items.Delete(SelIdx);
        F_PopOutSet.PopSettingsFrame.lbPopLbls.ItemIndex := -1;
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.btnPopDetailSettingsClick(Sender: TObject);
    var
        i, PoiIdx : integer;
        Value     : string;
    begin
    p_FormPopDetail := TFormPopDetailSettings.Create(nil);
    p_FormPopDetail.F_PopDetail.btnOK.OnClick := btnPopDetailOKClick;
    p_FormPopDetail.F_PopDetail.btnCancel.OnClick := btnPopDetailCancelClick;
    FindTotalIndex(PoiIdx);
    for i := 0 to 23 do
        begin
        Value := POPointList.Data[PoiIdx].PopDetailArray[i].ToString;
        p_FormPopDetail.F_PopDetail.sgPopDetail.Cells[1, (i + 1)] := Value;
        end;

    p_FormPopDetail.Show;
    end;

procedure TFramePedestrianPopOutSidePanel.btnPopDetailOKClick(Sender: TObject);
    var
        i, PoiIdx : integer;
        Value     : string;
    begin
    FindTotalIndex(PoiIdx);
    for i := 1 to 24 do
        begin
        Value := p_FormPopDetail.F_PopDetail.sgPopDetail.Cells[1, i];
        if (StrToIntDef(Value, -1) < 0) then
            begin
            ShowMessage('発生倍率は0以上の整数で設定してください');
            Exit;
            end;

        POPointList.Data[PoiIdx].SetNewDetailPer(i, StrToInt(Value));
        end;

    p_FormPopDetail.Close;
    FreeAndNil(p_FormPopDetail);
    end;

procedure TFramePedestrianPopOutSidePanel.btnPopDetailCancelClick(Sender: TObject);
    begin
    p_FormPopDetail.Close;
    FreeAndNil(p_FormPopDetail);
    end;

procedure TFramePedestrianPopOutSidePanel.sePopIntervalChange(Sender: TObject);
    var
        PoiIdx : integer;
    begin
    FindTotalIndex(PoiIdx);
    POPointList.Data[PoiIdx].PopInterval := F_PopOutSet.PopSettingsFrame.F_PedPopInterval.sePopInterval.iValue;
    end;

procedure TFramePedestrianPopOutSidePanel.sePopNumChange(Sender: TObject);
    var
        PoiIdx : integer;
    begin
    FindTotalIndex(PoiIdx);
    POPointList.Data[PoiIdx].PopNumOnInterval := F_PopOutSet.PopSettingsFrame.F_PedPopInterval.sePopNum.iValue;
    end;

procedure TFramePedestrianPopOutSidePanel.lbPopLblsClick(Sender: TObject);
    var
        PoiIdx, LblIdx, PNum, i, PopSum : integer;
    begin
    F_PopOutSet.PopSettingsFrame.lbPopLblsClick(Sender);
    LblIdx := -1;
    FindTotalIndex(PoiIdx);
    if F_PopOutSet.PopSettingsFrame.Visible then
        begin
        LblIdx := F_PopOutSet.PopSettingsFrame.lbPopLbls.ItemIndex;
        F_PopOutSet.PopSettingsFrame.btnPedDetailSettings.Enabled := (LblIdx >= 0);
        end;

    if LblIdx < 0 then
        Exit;

    PopSum := 0;
    PNum := POPointList.Data[PoiIdx].PedestrianData[LblIdx].PopNum;
    F_PopOutSet.PopSettingsFrame.seLabelPopNum.Value := PNum;
    for i := 0 to POPointList.Data[PoiIdx].PedestrianData.Count - 1 do
        PopSum := PopSum + POPointList.Data[PoiIdx].PedestrianData[i].PopNum;

    F_PopOutSet.PopSettingsFrame.seLabelPopPer.Value := Round((PNum / PopSum) * 100);
    if POPointList.Data[PoiIdx].PopRule = _PopbyInterval then
        F_PopOutSet.PopSettingsFrame.F_PedPopInterval.sePopNum.Value := PopSum
    else if POPointList.Data[PoiIdx].PopRule = _PopPerHour then
        F_PopOutSet.PopSettingsFrame.F_PedPopPerHour.sePopPerHour.Value := PopSum;
    end;

procedure TFramePedestrianPopOutSidePanel.seLblPopNumChange(Sender: TObject);
    var
        PoiIdx, LblIdx, PNum, i, PopSum : integer;
    begin
    LblIdx := -1;
    PNum   := 0;
    FindTotalIndex(PoiIdx);
    if F_PopOutSet.PopSettingsFrame.Visible then
        begin
        LblIdx := F_PopOutSet.PopSettingsFrame.lbPopLbls.ItemIndex;
        PNum   := F_PopOutSet.PopSettingsFrame.seLabelPopNum.iValue;
        end;

    if LblIdx < 0 then
        Exit;

    PopSum := 0;
    POPointList.Data[PoiIdx].PedestrianData[LblIdx].PopNum := PNum;
    for i := 0 to POPointList.Data[PoiIdx].PedestrianData.Count - 1 do
        PopSum := PopSum + POPointList.Data[PoiIdx].PedestrianData[i].PopNum;

    F_PopOutSet.PopSettingsFrame.seLabelPopPer.Value := Round((PNum / PopSum) * 100);
    if POPointList.Data[PoiIdx].PopRule = _PopbyInterval then
        F_PopOutSet.PopSettingsFrame.F_PedPopInterval.sePopNum.Value := PopSum
    else if POPointList.Data[PoiIdx].PopRule = _PopPerHour then
        F_PopOutSet.PopSettingsFrame.F_PedPopPerHour.sePopPerHour.Value := PopSum;
    end;

procedure TFramePedestrianPopOutSidePanel.btnSetPopScheduleClick(Sender: TObject);

    procedure SetScheduleData(const aSche: TPedestrianPopOutSchedule);
        var
            i, LatestIdx : integer;
            TimeStr      : string;
        begin
        if aSche.Data.Count = 0 then
            Exit;

        for i := 0 to aSche.Data.Count - 1 do
            begin
            p_FormSchedule.F_ScheSet.sgSchedule.RowCount := p_FormSchedule.F_ScheSet.sgSchedule.RowCount + 1;
            LatestIdx := p_FormSchedule.F_ScheSet.sgSchedule.RowCount - 1;
            p_FormSchedule.F_ScheSet.tmpDateTimes.Add(aSche.Data[i].Time);
            timeStr := Format('%.2d', [HourOf(aSche.Data[i].Time)]) + ':' + Format('%.2d', [MinuteOf(aSche.Data[i].Time)]);
            p_FormSchedule.F_ScheSet.sgSchedule.Cells[_DateTime,     LatestIdx] := TimeStr;
            p_FormSchedule.F_ScheSet.sgSchedule.Cells[_SeatOrPedNum, LatestIdx] := aSche.Data[i].SeatOrPedNum.ToString;
            p_FormSchedule.F_ScheSet.sgSchedule.Cells[_Capacity,     LatestIdx] := aSche.Data[i].BusID.ToString;
            end;
        end;

    procedure SetPedLblData(const Pidx: integer);
        var
            i, j : integer;
            newList : TList<integer>;
        begin
        if p_FormSchedule.F_ScheSet.lbPopLbls.Items.Count > 0 then
            p_FormSchedule.F_ScheSet.lbPopLbls.Items.Clear;

        for i := 0 to POPointList.Data[Pidx].PedestrianData.Count - 1 do
            p_FormSchedule.F_ScheSet.lbPopLbls.Items.Add(POPointList.Data[Pidx].PedestrianData[i].LabelName);

        for i := 0 to POPointList.Data[Pidx].PopSchedule.Data.Count - 1 do
            begin
            newList := TList<integer>.Create;
            for j := 0 to POPointList.Data[Pidx].PedestrianData.Count - 1 do
                newList.Add(POPointList.Data[Pidx].PopSchedule.Data[i].LblPopNums[j]);

            p_FormSchedule.F_ScheSet.tmpPNumDict.Add(i + 1, newList);
            end;
        end;

    var
        PoiIdx, i : integer;
    begin
    FindTotalIndex(PoiIdx);
    p_FormSchedule := TFormSchedule.Create(nil);
    p_FormSchedule.F_ScheSet.sgSchedule.ColCount := 4;
    p_FormSchedule.F_ScheSet.sgSchedule.Cells[_SeatOrPedNum, 0] := '降車(発生)人数';
    p_FormSchedule.F_ScheSet.sgSchedule.Cells[_Capacity,     0] := 'バスID';
    p_FormSchedule.F_ScheSet.IsPopSche := true;
    p_FormSchedule.F_ScheSet.btnPopLblAdd.OnClick := btnSchePopLblAddClick;
    p_FormSchedule.F_ScheSet.btnPopLblDel.OnClick := btnSchePopLblDelClick;
    p_FormSchedule.F_ScheSet.btnDestPerSettings.OnClick := btnPedDetailClick;
    for i := 0 to AllPedestrianLabelList.Count - 1 do
        p_FormSchedule.F_ScheSet.cbbPedLbl.Items.Add(AllPedestrianLabelList[i].LabelName);

    p_FormSchedule.Width := 708;
    SetScheduleData(POPointList.Data[PoiIdx].PopSchedule);
    SetPedLblData(PoiIdx);
    if p_FormSchedule.ShowModal = mrOK then
        btnPopScheSetOKClick(nil)
    else
        FreeAndNil(p_FormSchedule);
    end;

procedure TFramePedestrianPopOutSidePanel.btnSchePopLblAddClick(Sender: TObject);
    var
        MaxPedDataIdx, PoiIdx, LblIdx, i : integer;
    begin
    if F_PopOutSet.PopSettingsFrame.Visible then
        begin
        FindTotalIndex(PoiIdx);
        LblIdx := p_FormSchedule.F_ScheSet.cbbPedLbl.ItemIndex;
        if LblIdx < 0 then
            Exit;

        for i := 0 to F_PopOutSet.PopSettingsFrame.lbPopLbls.Items.Count - 1 do
            begin
            if F_PopOutSet.PopSettingsFrame.lbPopLbls.Items[i] = AllPedestrianLabelList[LblIdx].LabelName then
                begin
                ShowMessage('追加済みの人流ラベルです');
                Exit;
                end;
            end;

        POPointList.Data[PoiIdx].AddPedData(AllPedestrianLabelList[LblIdx]);
        F_PopOutSet.PopSettingsFrame.lbPopLbls.Items.Add(AllPedestrianLabelList[LblIdx].LabelName);
        MaxPedDataIdx := POPointList.Data[PoiIdx].PedestrianData.Count - 1;
        for i := 0 to POPointList.Data[PoiIdx].DestinationList.Count - 1 do
            POPointList.Data[PoiIdx].PedestrianData[MaxPedDataIdx].AddDestPer;

        for i := 0 to POPointList.Data[PoiIdx].PopSchedule.Data.Count - 1 do
            POPointList.Data[PoiIdx].PopSchedule.Data[i].LblPopNums.Add(0);

        p_FormSchedule.F_ScheSet.lbPopLbls.Items.Add(AllPedestrianLabelList[LblIdx].LabelName);
        p_FormSchedule.F_ScheSet.UpdateEditData;
        p_FormSchedule.F_ScheSet.UpdatePopNums;
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.btnSchePopLblDelClick(Sender: TObject);
    var
        PoiIdx, SelIdx : integer;
    begin
    FindTotalIndex(PoiIdx);
    SelIdx := p_FormSchedule.F_ScheSet.lbPopLbls.ItemIndex;
    if (PoiIdx < 0) or (SelIdx < 0) then
        Exit;

    POPointList.Data[PoiIdx].DeletePedData(SelIdx);
    if F_PopOutSet.PopSettingsFrame.Visible then
        begin
        F_PopOutSet.PopSettingsFrame.lbPopLbls.Items.Delete(SelIdx);
        F_PopOutSet.PopSettingsFrame.lbPopLbls.ItemIndex := -1;
        end;

    p_FormSchedule.F_ScheSet.lbPopLbls.Items.Delete(SelIdx);
    p_FormSchedule.F_ScheSet.lbPopLbls.ItemIndex := -1;
    p_FormSchedule.F_ScheSet.UpdateEditDataDel(SelIdx);
    p_FormSchedule.F_ScheSet.UpdatePopNums;
    end;

procedure TFramePedestrianPopOutSidePanel.btnPopScheSetOKClick(Sender: TObject);

    function IsFoundInvalidData(const Idx: integer): boolean;
        begin
        result := false;
        if (StrToIntDef(p_FormSchedule.F_ScheSet.sgSchedule.Cells[_Capacity,   Idx], -1) < 0) then
            result := true;
        end;

    function ReceiveScheduleData(const aSche: TPedestrianPopOutSchedule) : boolean;
        var
            i, j        : integer;
            tmpScheData : TScheduleData;
        begin
        result := false;
        for i := 1 to p_FormSchedule.F_ScheSet.sgSchedule.RowCount - 1 do
            begin
            if IsFoundInvalidData(i) then
                begin
                ShowMessage('無効な値が入力されています');
                Exit;
                end;
            end;

        aSche.ClearData;
        for i := 1 to p_FormSchedule.F_ScheSet.sgSchedule.RowCount - 1 do
            begin
            tmpScheData := TScheduleData.Create;
            tmpScheData.Time         := p_FormSchedule.F_ScheSet.tmpDateTimes[i - 1];
            tmpScheData.SeatOrPedNum := StrToInt(p_FormSchedule.F_ScheSet.sgSchedule.Cells[_SeatOrPedNum, i]);
            tmpScheData.BusID        := StrToInt(p_FormSchedule.F_ScheSet.sgSchedule.Cells[_Capacity,     i]);
            if p_FormSchedule.F_ScheSet.tmpPNumDict.ContainsKey(i) then
                begin
                for j := 0 to p_FormSchedule.F_ScheSet.tmpPNumDict[i].Count - 1 do
                    tmpScheData.LblPopNums.Add(p_FormSchedule.F_ScheSet.tmpPNumDict[i][j]);
                end;

            aSche.Data.Add(tmpScheData);
            end;

        result := true;
        end;

    var
        PoiIdx : integer;
    begin
    FindTotalIndex(PoiIdx);
    if ReceiveScheduleData(POPointList.Data[PoiIdx].PopSchedule) then
        begin
        UpdateDestPerList;
        p_FormSchedule.Close;
        FreeAndNil(p_FormSchedule);
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.sePopPerHourChange(Sender: TObject);
    var
        PoiIdx : integer;
    begin
    FindTotalIndex(PoiIdx);
    POPointList.Data[PoiIdx].PopPerHour := F_PopOutSet.PopSettingsFrame.F_PedPopPerHour.sePopPerHour.iValue;
    POPointList.Data[PoiIdx].PopFrequency := Max(Round(3600 / POPointList.Data[PoiIdx].PopPerHour), 1);
    end;

procedure TFramePedestrianPopOutSidePanel.btnSetOutScheduleClick(Sender: TObject);

    procedure SetScheduleData(const aSche: TPedestrianPopOutSchedule);
        var
            i, LatestIdx : integer;
            TimeStr      : string;
        begin
        if aSche.Data.Count = 0 then
            Exit;

        for i := 0 to aSche.Data.Count - 1 do
            begin
            p_FormSchedule.F_ScheSet.sgSchedule.RowCount := p_FormSchedule.F_ScheSet.sgSchedule.RowCount + 1;
            LatestIdx := p_FormSchedule.F_ScheSet.sgSchedule.RowCount - 1;
            p_FormSchedule.F_ScheSet.tmpDateTimes.Add(aSche.Data[i].Time);
            timeStr := Format('%.2d', [HourOf(aSche.Data[i].Time)]) + ':' + Format('%.2d', [MinuteOf(aSche.Data[i].Time)]);
            p_FormSchedule.F_ScheSet.sgSchedule.Cells[_DateTime,      LatestIdx] := TimeStr;
            p_FormSchedule.F_ScheSet.sgSchedule.Cells[_SeatOrPedNum,  LatestIdx] := aSche.Data[i].SeatOrPedNum.ToString;
            p_FormSchedule.F_ScheSet.sgSchedule.Cells[_Capacity,      LatestIdx] := aSche.Data[i].Capacity.ToString;
            p_FormSchedule.F_ScheSet.sgSchedule.Cells[_GetOnNum,      LatestIdx] := aSche.Data[i].GetOnNum.ToString;
            p_FormSchedule.F_ScheSet.sgSchedule.Cells[_GetOffNum,     LatestIdx] := aSche.Data[i].BusID.ToString;
            end;
        end;

    var
        PoiIdx : integer;
    begin
    FindTotalIndex(PoiIdx);
    p_FormSchedule := TFormSchedule.Create(nil);
    p_FormSchedule.F_ScheSet.sgSchedule.ColCount := 6;
    p_FormSchedule.F_ScheSet.sgSchedule.Cells[_GetOffNum, 0] := 'バスID';
    p_FormSchedule.F_ScheSet.PanelPopLblNums.Width   := 0;
    p_FormSchedule.F_ScheSet.PanelPopLblNums.Visible := false;
    p_FormSchedule.Width := 607;
    p_FormSchedule.F_ScheSet.lblCaptionAboutGetOnNum.Visible := true;
    SetScheduleData(POPointList.Data[PoiIdx].OutSchedule);
    if p_FormSchedule.ShowModal = mrOK then
        btnOutScheSetOKClick(nil)
    else
        FreeAndNil(p_FormSchedule);
    end;

procedure TFramePedestrianPopOutSidePanel.btnOutScheSetOKClick(Sender: TObject);

    function IsFoundInvalidData(const Idx: integer): boolean;
        begin
        result := false;
        if (StrToIntDef(p_FormSchedule.F_ScheSet.sgSchedule.Cells[_SeatOrPedNum,   Idx], -1) < 0)
            or (StrToIntDef(p_FormSchedule.F_ScheSet.sgSchedule.Cells[_Capacity,   Idx], -1) < 0)
            or (StrToIntDef(p_FormSchedule.F_ScheSet.sgSchedule.Cells[_GetOffNum,  Idx], -1) < 0) then
            result := true;
        end;

    function ReceiveScheduleData(const aSche: TPedestrianPopOutSchedule): boolean;
        var
            i             : integer;
            tmpScheData   : TScheduleData;
            tmpUpdateRule : TWaitingQueueUpdateRuleType;
        begin
        result := false;
        for i := 1 to p_FormSchedule.F_ScheSet.sgSchedule.RowCount - 1 do
            begin
            if IsFoundInvalidData(i) then
                begin
                ShowMessage('無効な値が入力されています');
                Exit;
                end;
            end;

        aSche.ClearData;
        for i := 1 to p_FormSchedule.F_ScheSet.sgSchedule.RowCount - 1 do
            begin
            tmpScheData := TScheduleData.Create;
            tmpScheData.Time         := p_FormSchedule.F_ScheSet.tmpDateTimes[i - 1];
            tmpScheData.SeatOrPedNum := StrToInt(p_FormSchedule.F_ScheSet.sgSchedule.Cells[_SeatOrPedNum,  i]);
            tmpScheData.Capacity     := StrToInt(p_FormSchedule.F_ScheSet.sgSchedule.Cells[_Capacity,      i]);
            tmpScheData.GetOnNum     := StrToInt(p_FormSchedule.F_ScheSet.sgSchedule.Cells[_GetOnNum,      i]);
            tmpScheData.BusID        := StrToInt(p_FormSchedule.F_ScheSet.sgSchedule.Cells[_GetOffNum,     i]);
            aSche.Data.Add(tmpScheData);

            tmpUpdateRule.BusID                  := StrToInt(p_FormSchedule.F_ScheSet.sgSchedule.Cells[_GetOffNum,    i]);
            tmpUpdateRule.ReleaseTime            := p_FormSchedule.F_ScheSet.tmpDateTimes[i - 1];
            tmpUpdateRule.ReleaseMaxAmountAtOnce := StrToInt(p_FormSchedule.F_ScheSet.sgSchedule.Cells[_GetOnNum,     i]);
            tmpUpdateRule.NumberOfBusSeat        := StrToInt(p_FormSchedule.F_ScheSet.sgSchedule.Cells[_SeatOrPedNum, i]);
            tmpUpdateRule.MaxPercentageGetOn     := StrToInt(p_FormSchedule.F_ScheSet.sgSchedule.Cells[_Capacity,     i]);
            aSche.DataforLink.Add(tmpUpdateRule);
            end;

        result := true;
        end;

    var
        PoiIdx : integer;
    begin
    FindTotalIndex(PoiIdx);
    if ReceiveScheduleData(POPointList.Data[PoiIdx].OutSchedule) then
        begin
        p_FormSchedule.Close;
        FreeAndNil(p_FormSchedule);
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.seOutIntervalChange(Sender: TObject);
    var
        PoiIdx : integer;
    begin
    FindTotalIndex(PoiIdx);
    POPointList.Data[PoiIdx].OutInterval := F_PopOutSet.OutSettingsFrame.F_PedOutInterval.seOutInterval.iValue;
    end;

procedure TFramePedestrianPopOutSidePanel.seOutNumChange(Sender: TObject);
    var
        PoiIdx : integer;
    begin
    FindTotalIndex(PoiIdx);
    POPointList.Data[PoiIdx].OutNumOnInterval := F_PopOutSet.OutSettingsFrame.F_PedOutInterval.seOutNum.iValue;
    end;

procedure TFramePedestrianPopOutSidePanel.cbbOutRuleChange(Sender: TObject);
    begin
    F_PopOutSet.OutSettingsFrame.cbbSelectOutRuleChange(Sender);
    UpdateOutParams;
    end;

function TFramePedestrianPopOutSidePanel.IsExistPoint(const nwkid, ndid: integer; pType: PopOutPointType): boolean;
    var
        i : integer;
    begin
    Result := false;
    for i := 0 to p_PopOutPointList.Data.Count - 1 do
        begin
        if (p_PopOutPointList.Data[i].PointType = pType) and (p_PopOutPointList.Data[i].NodeIdx[0] = nwkid) and (p_PopOutPointList.Data[i].NodeIdx[1] = ndid) then
            begin
            Result := true;
            Exit;
            end;
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.UpdateDestPerList;
    procedure DoUpdateDestPerList(const aPoiIdx: Integer);
        var
            i, j: Integer;
            newList: TList<Double>;
        begin
        for i := 0 to p_PopOutPointList.Data[aPoiIdx].PedestrianData.Count - 1 do
            begin
            newList := TList<Double>.Create;
            for j := 0 to POPointList.Data[aPoiIdx].PedestrianData[i].DestPerList.Count -1 do
                newList.Add(POPointList.Data[aPoiIdx].PedestrianData[i].DestPerList[j]);

            p_PopOutPointList.Data[aPoiIdx].PedestrianData[i].SetNewDestPers(newList);
            end;
        end;
    var
        poiIdx: Integer;
    begin
    // 画面を閉じた時の不具合対策
    FindTotalIndex(poiIdx);
    case p_PopOutPointList.Data[poiIdx].PopRule of
        _PopbySchedule:
            begin
            if Assigned(p_FormSchedule) then
                DoUpdateDestPerList(poiIdx);
            end;
        _PopbyInterval,
        _PopPerHour:
            DoUpdateDestPerList(poiIdx);
        else
            ;
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.AfterPaintOpenGL;

    procedure SetPopNode;
        var
            i, j   : integer;
            tmpNwk : IF8Networks;
            m : TMethod;
        begin
        if theApplicationServices.project.NumberOfFlightWayNwks > 0 then
            begin
            for i := 1 to theApplicationServices.project.NumberOfFlightWayNwks do
                begin
                tmpNwk := theApplicationServices.project.FlightWayNwk[i];
                if tmpNwk.NodeCount <= 0 then
                    Continue;

                for j := 0 to tmpNwk.NodeCount - 1 do
                    begin
                    if (tmpNwk.Node[j].Clicked) and (tmpNwk.Node[j].SelectedOnCrowdSim = false) then
                        begin
                        if IsExistPoint(i, j, _PopPoint) then
                            begin
                            ShowMessage('発生地点として設定済みの地点です');
                            tmpNwk.Node[j].ReleaseSelecting;
                            tmpNwk.Node[j].SelectedOnCrowdSim := false;
                            Initialize;
                            Exit;
                            end;

                        p_tmpPopNodeNum[0] := i;
                        p_tmpPopNodeNum[1] := j;
                        tmpNwk.Node[j].SelectedOnCrowdSim := true;
                        if p_IsDispersionSelectingMode then
                            begin
                            AddDispersionMainPoint;
                            p_IsDispPopSelectingMode := true;
                            theApplicationServices.AddDisplayMessage(SELECT_DISPERSION_POPOUT_POINT_MESSAGE_ACTION);
                            theApplicationServices.AddDisplayMessage(EDIT_MODE_MESSAGE_FINISH);
                            FormMainOpenGLMouseUpDownProc(m) := OnMouseDownDispersion;
                            theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLMouseDown, m);
                            FormMainKeyUpDownProc(m) := OnKeyDownDispersion;
                            theApplicationServices.RegisterEventHandler(_plgFormMainKeyDown, m);
                            Exit;
                            end;

                        theApplicationServices.ClearDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
                        theApplicationServices.ClearDisplayMessage(SELECT_POP_POINT_MESSAGE_ACTION);
                        AddNewData;
                        Break;
                        end;
                    end;
                end;
            end;
        end;

    procedure SetOutNode;
        var
            i, j   : integer;
            tmpNwk : IF8Networks;
            m : TMethod;
        begin
        if theApplicationServices.project.NumberOfFlightWayNwks > 0 then
            begin
            for i := 1 to theApplicationServices.project.NumberOfFlightWayNwks do
                begin
                tmpNwk := theApplicationServices.project.FlightWayNwk[i];
                if tmpNwk.NodeCount <= 0 then
                    Continue;

                for j := 0 to tmpNwk.NodeCount - 1 do
                    begin
                    if (tmpNwk.Node[j].Clicked) and (tmpNwk.Node[j].SelectedOnCrowdSim = false)  then
                        begin
                        if IsExistPoint(i, j, _OutPoint) then
                            begin
                            ShowMessage('退出地点として設定済みの地点です');
                            tmpNwk.Node[j].ReleaseSelecting;
                            tmpNwk.Node[j].SelectedOnCrowdSim := false;
                            Initialize;
                            Exit;
                            end;

                        p_tmpOutNodeNum[0] := i;
                        p_tmpOutNodeNum[1] := j;
                        tmpNwk.Node[j].SelectedOnCrowdSim := true;
                        if p_IsDispersionSelectingMode then
                            begin
                            AddDispersionMainPoint;
                            p_IsDispOutSelectingMode := true;
                            theApplicationServices.AddDisplayMessage(SELECT_DISPERSION_POPOUT_POINT_MESSAGE_ACTION);
                            theApplicationServices.AddDisplayMessage(EDIT_MODE_MESSAGE_FINISH);
                            FormMainOpenGLMouseUpDownProc(m) := OnMouseDownDispersion;
                            theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLMouseDown, m);
                            FormMainKeyUpDownProc(m) := OnKeyDownDispersion;
                            theApplicationServices.RegisterEventHandler(_plgFormMainKeyDown, m);
                            Exit;
                            end;

                        theApplicationServices.ClearDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
                        theApplicationServices.ClearDisplayMessage(SELECT_OUT_POINT_MESSAGE_ACTION);
                        AddNewOutData;
                        Break;
                        end;
                    end;
                end;
            end;
        end;

    begin
    if (not p_IsSelectingMode) and (not p_IsOutSelectingMode) then
        Exit;

    if (p_IsSelectingMode) and (p_tmpPopNodeNum[0] < 0) then
        SetPopNode;

    if (p_IsOutSelectingMode) and (p_tmpOutNodeNum[0] < 0) then
        SetOutNode;
    end;

procedure TFramePedestrianPopOutSidePanel.AfterPaintOpenGLOnChangeMode;

    procedure SetChangeNode;
        var
            i, j   : integer;
            tmpNwk : IF8Networks;
        begin
        if theApplicationServices.project.NumberOfFlightWayNwks > 0 then
            begin
            for i := 1 to theApplicationServices.project.NumberOfFlightWayNwks do
                begin
                tmpNwk := theApplicationServices.project.FlightWayNwk[i];
                if tmpNwk.NodeCount <= 0 then
                    Continue;

                for j := 0 to tmpNwk.NodeCount - 1 do
                    begin
                    if (tmpNwk.Node[j].Clicked) and (tmpNwk.Node[j].SelectedOnCrowdSim = false) then
                        begin
                        p_tmpChangeNodeNum[0] := i;
                        p_tmpChangeNodeNum[1] := j;
                        tmpNwk.Node[j].SelectedOnCrowdSim := true;
                        if p_IsDispChangingMode then
                            begin
                            ChangeDispersionMainPoint;
                            p_IsDispPopSelectingMode := true;
                            Exit;
                            end;

                        theApplicationServices.ClearDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
                        theApplicationServices.ClearDisplayMessage(CHANGE_POINT_MESSAGE_ACTION);
                        ChangeData;
                        Break;
                        end;
                    end;
                end;
            end;
        end;

    begin
    if not p_IsChangingMode then
        Exit;

    if p_tmpChangeNodeNum[0] < 0 then
        SetChangeNode;
    end;

procedure TFramePedestrianPopOutSidePanel.SendRenderDispersionData(out aRenderDPList: TPopOutPointList);
    var
        i, j  : integer;
        newDP : TPopOutPoint;
    begin
    if (POPointList.Data.Count < 1) then
        Exit;

    aRenderDPList.Data.Clear;
    for i := 0 to POPointList.Data.Count - 1 do
        begin
        if POPointList.Data[i].IsAssignedDispersionPoint then
            begin
            newDP := TPopOutPoint.Create(POPointList.Data[i].Name);
            newDP.IsAssignedDispersionPoint := true;
            for j := 0 to POPointList.Data[i].DispersionPointList.Count - 1 do
                newDP.DispersionPointList.Add(POPointList.Data[i].DispersionPointList[j]);

            aRenderDPList.Data.Add(newDP);
            end;
        end;

    aRenderDPList.IsVisibleDispersionPoint := POPointList.IsVisibleDispersionPoint;
    end;

procedure TFramePedestrianPopOutSidePanel.SetPopOutSettingsData;
    var
        i, j, NwkIdx, NdIdx : integer;
        nwk : IF8Networks;
        gene : IF8PedestrianGenerator;
    begin
    if (theApplicationServices.project.numberOfFlightWayNwks < 1)
        or (F_PopOutSet.ListBoxInOutPoints.Items.Count < 1)
        or (F_PopOutSet.lbOutOnly.Items.Count < 1) then
        Exit;

    nwk := theApplicationServices.project.FlightWayNwk[1];
    if nwk.Generator[nwk.NumberOfGenerator - 1].TypeOfGenerator = _pgNormal then
        begin
        nwk.AddPedestriansGeneratorIF(_pgMatrixOD);
        nwk.ChangeActivePedestriansGeneratorIndex(nwk.NumberOfGenerator - 1);
        end;

    for i := 0 to nwk.NodeCount - 1 do
        begin
        nwk.node[i].PopListIndex := -1;
        nwk.node[i].OutListIndex := -1;
        gene := nwk.Generator[nwk.NumberOfGenerator - 1];
        for j := 0 to nwk.NodeCount - 1 do
            begin
            if Assigned(gene.OptionListContainerByOD[nwk.Node[i], nwk.Node[j]]) then
                gene.DeleteOptionListContainerByOD(nwk.Node[i], nwk.Node[j]);
            end;
        end;

    if (POPointList.Data.Count < 1) then
        Exit;

    for i := 0 to F_PopOutSet.ListBoxInOutPoints.Items.Count - 1 do
        begin
        F_PopOutSet.ListBoxInOutPoints.ItemIndex := i;
        UpdateDestPerList;
        end;

    for i := 0 to POPointList.Data.Count - 1 do
        begin
        if POPointList.Data[i].IsAssignedDispersionPoint then
            begin
            NwkIdx := POPointList.Data[i].NodeIdx[0];
            NdIdx  := POPointList.Data[i].NodeIdx[1];
            for j := 0 to POPointList.Data[i].DispersionPointList.Count - 1 do
                begin
                if j <= theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].NumberOfDispersionPoints - 1 then
                    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SetDispersionPoint(j, POPointList.Data[i].DispersionPointList[j])
                else
                    theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].AddDispersionPoint(POPointList.Data[i].DispersionPointList[j]);
                end;
            end;

        if POPointList.Data[i].PointType = _PopPoint then
            SetPedPopData(POPointList.Data[i], nwk)
        else if POPointList.Data[i].PointType = _OutPoint then
            SetPedOutData(POPointList.Data[i], nwk);
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.SetPedPopData(const target: TPopOutPoint; nwk: IF8Networks);

    procedure SetPedestrianProfileData(const optCtnr: IF8PedestriansGeneratorOptionListContainer; destid: integer);
        const
            THRESHOLD = 0.001;
        var
            k, l, PPIdx, ProfCnt,
            PLSum, delta, PopSum, LblCount : integer;
            PopTime, PerSum, DestPer : double;
            pedProf : IF8PedestrianProfile;
            GeneOpt : IF8PedestriansGeneratorOption;
            PerSecPops : TList<double>;
            tmpRoundMode: TRoundingMode;
        begin
        PopTime   := 0;
        if target.PopRule = _PopbyInterval then
            PopTime   := target.PopInterval;

        PPIdx := -1;
        ProfCnt := 0;
        LblCount := p_AllPedestrianLabelList.Count;
        for k := 0 to p_PopOutPointList.Data.Count - 1 do
            begin
            if (p_PopOutPointList.Data[k].PointType = _PopPoint) and (p_PopOutPointList.Data[k].Name = target.Name) then
                begin
                if destid > 0 then
                    ProfCnt := ProfCnt + (p_PopOutPointList.Data[k].PedestrianData.Count * destid);

                PPIdx := PPIdx + 1;
                Break;
                end;

            if p_PopOutPointList.Data[k].PointType = _PopPoint then
                begin
                if p_PopOutPointList.Data[k].PedestrianData.Count < 1 then
                    Continue;

                if p_PopOutPointList.Data[k].PopRule = _PopbySchedule then
                    ProfCnt := ProfCnt + (p_PopOutPointList.Data[k].PedestrianData.Count * p_PopOutPointList.Data[k].PopSchedule.Data.Count * p_PopOutPointList.Data[k].PedestrianData[0].DestPerList.Count)
                else
                    ProfCnt := ProfCnt + (p_PopOutPointList.Data[k].PedestrianData.Count * p_PopOutPointList.Data[k].PedestrianData[0].DestPerList.Count);
                end;
            end;

        if PPIdx < 0 then
            Exit;

        for k := 0 to target.PedestrianData.Count - 1 do
            begin
            PLSum := 0;
            GeneOpt := optCtnr.OptionIF[k];
            if (ProfCnt + k+1) > (theApplicationServices.project.NumberOfPedestrianProfile - LblCount) then
                begin
                theApplicationServices.project.AddPedestrianProfileDefalt;
                UpdateAddedProfile;
                end;

            pedProf := theApplicationServices.project.PedestrianProfileIF[(ProfCnt + k+1) + LblCount];
            PerSum := 0;
            for l := 0 to target.PedestrianData[k].DestPerList.Count - 1 do
                PerSum := PerSum + target.PedestrianData[k].DestPerList[l];

            if PerSum = 0 then
                PerSum := 1;

            PopSum := 0;
            for l := 0 to target.PedestrianData.Count - 1 do
                PopSum := PopSum + target.PedestrianData[l].PopNum;

            if PopSum = 0 then
                PopSum := 1;

            DestPer := target.PedestrianData[k].DestPerList[destid];
            pedProf.AttributeLabel := target.PedestrianData[k].LabelName;
            tmpRoundMode := GetRoundMode;
            try
                SetRoundMode(rmTruncate);
                if target.PopRule = _PopPerHour then
                    begin
                    PerSecPops := TList<double>.Create;
                    for l := 0 to target.PedestrianData[k].DestPerList.Count - 1 do
                        PerSecPops.Add(target.PedestrianData[k].PopNum * (target.PedestrianData[k].DestPerList[l] / PerSum));

                    pedProf.PeopleLimit := 1;
                    PopTime := (3600 / PerSecPops[destid]);
                    FreeAndNil(PerSecPops);
                    end
                else
                    pedProf.PeopleLimit := Round(SimpleRoundTo(target.PedestrianData[k].PopNum * (DestPer / PerSum), 0));

                for l := 0 to target.PedestrianData[k].AttrPerList.Count - 1 do
                    pedProf.option[l+1].Weight := target.PedestrianData[k].AttrPerList[l];

                theApplicationServices.project.PedestrianProfileIF[(ProfCnt + k+1) + LblCount] := pedProf;
                GeneOpt.profile := pedProf;
                GeneOpt.TimeSchedule := PopTime;
                GeneOpt.Weigth := Round(((target.PedestrianData[k].PopNum / PopSum) * 100) * ((DestPer / PerSum) * 100));
                optCtnr.OptionIF[k] := GeneOpt;
                if target.PopRule = _PopbyInterval then
                    begin
                    for l := 0 to target.PedestrianData[k].DestPerList.Count - 1 do
                        PLSum := PLSum + Round(SimpleRoundTo(target.PedestrianData[k].PopNum * (target.PedestrianData[k].DestPerList[l] / PerSum), 0));

                    if PLSum <> target.PedestrianData[k].PopNum then
                        begin
                        delta := target.PedestrianData[k].PopNum - PLSum;
                        if destid = target.PedestrianData[k].DestPerList.Count - 1 then
                            optCtnr.OptionIF[k].profile.PeopleLimit := optCtnr.OptionIF[k].profile.PeopleLimit + delta;
                        end;
                    end;
            finally
                SetRoundMode(tmpRoundMode);
                end;
            end;
        end;

    procedure SetPedestrianProfileDatabyPopSchedule(const sche: TPedestrianPopOutSchedule; optCtnr: IF8PedestriansGeneratorOptionListContainer; destid: integer);
        var
            k, l, TimeDataIdx, PPIdx, ProfCnt, PopSum, PLSum, delta, LblCount : integer;
            PerSum, DestPer : double;
            pedProf : IF8PedestrianProfile;
            GeneOpt : IF8PedestriansGeneratorOption;
            tmpRoundMode: TRoundingMode;
        begin
        PPIdx := -1;
        ProfCnt := 0;
        LblCount := p_AllPedestrianLabelList.Count;
        for k := 0 to p_PopOutPointList.Data.Count - 1 do
            begin
            if (p_PopOutPointList.Data[k].PointType = _PopPoint) and (p_PopOutPointList.Data[k].Name = target.Name) then
                begin
                if destid > 0 then
                    ProfCnt := ProfCnt + (p_PopOutPointList.Data[k].PedestrianData.Count * p_PopOutPointList.Data[k].PopSchedule.Data.Count * destid);

                PPIdx := PPIdx + 1;
                Break;
                end;

            if p_PopOutPointList.Data[k].PointType = _PopPoint then
                begin
                if p_PopOutPointList.Data[k].PedestrianData.Count < 1 then
                    Continue;

                if p_PopOutPointList.Data[k].PopRule = _PopbySchedule then
                    ProfCnt := ProfCnt + (p_PopOutPointList.Data[k].PedestrianData.Count * p_PopOutPointList.Data[k].PopSchedule.Data.Count * p_PopOutPointList.Data[k].PedestrianData[0].DestPerList.Count)
                else
                    ProfCnt := ProfCnt + (p_PopOutPointList.Data[k].PedestrianData.Count * p_PopOutPointList.Data[k].PedestrianData[0].DestPerList.Count);
                end;
            end;

        if PPIdx < 0 then
            Exit;

        for k := 0 to target.PedestrianData.Count - 1 do
            begin
            for TimeDataIdx := 0 to sche.Data.Count - 1 do
                begin
                GeneOpt := optCtnr.OptionIF[(k * sche.Data.Count) + TimeDataIdx];
                if (ProfCnt + ((k * sche.Data.Count) + TimeDataIdx)+1) > (theApplicationServices.project.NumberOfPedestrianProfile - LblCount) then
                    begin
                    theApplicationServices.project.AddPedestrianProfileDefalt;
                    UpdateAddedProfile;
                    end;

                pedProf := theApplicationServices.project.PedestrianProfileIF[(ProfCnt + ((k * sche.Data.Count) + TimeDataIdx)+1) + LblCount];
                PerSum := 0;
                for l := 0 to target.PedestrianData[k].DestPerList.Count - 1 do
                    PerSum := PerSum + target.PedestrianData[k].DestPerList[l];

                if PerSum = 0 then
                    PerSum := 1;

                PopSum := 0;
                for l := 0 to target.PedestrianData.Count - 1 do
                    PopSum := PopSum + sche.Data[TimeDataIdx].LblPopNums[l];

                if PopSum = 0 then
                    PopSum := 1;

                DestPer := target.PedestrianData[k].DestPerList[destid];
                pedProf.AttributeLabel := target.PedestrianData[k].LabelName;
                tmpRoundMode := GetRoundMode;
                try
                    SetRoundMode(rmTruncate);
                    pedProf.PeopleLimit := Round(SimpleRoundTo(sche.Data[TimeDataIdx].LblPopNums[k] * (DestPer / PerSum), 0));
                    for l := 0 to target.PedestrianData[k].AttrPerList.Count - 1 do
                        pedProf.option[l+1].Weight := target.PedestrianData[k].AttrPerList[l];

                    theApplicationServices.project.PedestrianProfileIF[(ProfCnt + ((k * sche.Data.Count) + TimeDataIdx)+1) + LblCount] := pedProf;
                    GeneOpt.profile := pedProf;
                    GeneOpt.TimeSchedule := sche.Data[TimeDataIdx].Time;
                    GeneOpt.BusID := sche.Data[TimeDataIdx].BusID;
                    GeneOpt.Weigth := Round(((sche.Data[TimeDataIdx].LblPopNums[k] / PopSum) * 100) * ((DestPer / PerSum) * 100));
                    optCtnr.OptionIF[(k * sche.Data.Count) + TimeDataIdx] := GeneOpt;
                    PLSum := 0;
                    for l := 0 to target.PedestrianData[k].DestPerList.Count - 1 do
                        PLSum := PLSum + Round(SimpleRoundTo(sche.Data[TimeDataIdx].LblPopNums[k] * (target.PedestrianData[k].DestPerList[l] / PerSum), 0));

                    if (PLSum <> sche.Data[TimeDataIdx].LblPopNums[k]) then
                        begin
                        delta := sche.Data[TimeDataIdx].LblPopNums[k] - PLSum;
                        if destid = target.PedestrianData[k].DestPerList.Count - 1 then
                            optCtnr.OptionIF[(k * sche.Data.Count) + TimeDataIdx].profile.PeopleLimit := optCtnr.OptionIF[(k * sche.Data.Count) + TimeDataIdx].profile.PeopleLimit + delta;
                        end;
                finally
                    SetRoundMode(tmpRoundMode);
                    end;
                end;
            end;
        end;

    var
        i, j, k, OptNum, DestIdx, lbIdx, WeightSum : integer;
        OptListCtner          : IF8PedestriansGeneratorOptionListContainer;
        gene : IF8PedestrianGenerator;
        maxpopulation : Integer;
    begin
    DestIdx := -1;
    OptListCtner := nil;
    lbIdx := -1;
    if target.PointAttr = _BusTerminal then
        nwk.Node[target.NodeIdx[1]].LocationType := _pglBoardingGate
    else if target.PointAttr = _TicketGate then
        nwk.Node[target.NodeIdx[1]].LocationType := _pglTicketGate
    else
        nwk.Node[target.NodeIdx[1]].LocationType := _pglDefault;

    for i := 0 to F_PopOutSet.ListBoxInOutPoints.Items.Count - 1 do
        begin
        if target.Name = F_PopOutSet.ListBoxInOutPoints.Items[i] then
            begin
            lbIdx := i;
            Break;
            end;
        end;

    if lbIdx < 0 then
        Exit;

    nwk.Node[target.NodeIdx[1]].PopListIndex := lbIdx;
    nwk.Node[target.NodeIdx[1]].InOut := true;
    for i := 0 to POPointList.Data.Count - 1 do
        begin
        if (POPointList.Data[i].PointType = _OutPoint)
            and ((not ((POPointList.Data[i].NodeIdx[0] = target.NodeIdx[0]) and (POPointList.Data[i].NodeIdx[1] = target.NodeIdx[1])))) then
            begin
            DestIdx := DestIdx + 1;
            gene := nwk.Generator[nwk.NumberOfGenerator - 1];
            OptNum := 0;
            if target.PopRule = _PopbySchedule then
                OptNum := target.PedestrianData.Count * target.PopSchedule.Data.Count
            else if (target.PopRule = _PopPerHour) or (target.PopRule = _PopbyInterval) then
                OptNum := target.PedestrianData.Count;

            if Assigned(gene.OptionListContainerByOD[nwk.Node[target.NodeIdx[1]], nwk.Node[POPointList.Data[i].NodeIdx[1]]])
                and (gene.OptionListContainerByOD[nwk.Node[target.NodeIdx[1]], nwk.Node[POPointList.Data[i].NodeIdx[1]]].OptionCount > OptNum) then
                gene.DeleteOptionListContainerByOD(nwk.Node[target.NodeIdx[1]], nwk.Node[POPointList.Data[i].NodeIdx[1]]);

            OptListCtner := gene.AddOptionListContainerToOD(nwk.Node[target.NodeIdx[1]], nwk.Node[POPointList.Data[i].NodeIdx[1]]);
            end;

        if not Assigned(OptListCtner) then
            Continue;

        if target.PopRule = _PopbySchedule then
            begin
            if target.PointAttr = _BusTerminal then
                begin
                OptListCtner.BusModel     := target.BusModel;
                OptListCtner.BusRoadIndex := target.BusRoadIndex;
                end;

            OptListCtner.IntervalType := _pgiTimetable;
            maxpopulation := 1;
            for j := 0 to target.PopSchedule.Data.Count - 1 do
                begin
                for k := 0 to target.PopSchedule.Data[j].LblPopNums.Count - 1 do
                    Inc(maxpopulation, target.PopSchedule.Data[j].LblPopNums[k]);
                end;

            OptListCtner.maxPopulation := maxpopulation;

            OptNum := target.PedestrianData.Count * target.PopSchedule.Data.Count;
            for j := 1 to OptNum do
                begin
                if j > OptListCtner.OptionCount then
                    OptListCtner.AddNewOption;
                end;

            if DestIdx >= 0 then
                begin
                SetPedestrianProfileDatabyPopSchedule(target.PopSchedule, OptListCtner, DestIdx);
                gene.OptionListContainerByOD[nwk.Node[target.NodeIdx[1]], nwk.Node[POPointList.Data[i].NodeIdx[1]]] := OptListCtner;
                OptListCtner := nil;
                WeightSum := 0;
                for j := 0 to gene.OptionListContainerByOD[nwk.Node[target.NodeIdx[1]], nwk.Node[POPointList.Data[i].NodeIdx[1]]].OptionCount - 1 do
                    WeightSum := WeightSum + gene.OptionListContainerByOD[nwk.Node[target.NodeIdx[1]], nwk.Node[POPointList.Data[i].NodeIdx[1]]].OptionIF[j].Weigth;

                if WeightSum = 0 then
                    gene.DeleteOptionListContainerByOD(nwk.Node[target.NodeIdx[1]], nwk.Node[POPointList.Data[i].NodeIdx[1]]);
                end;
            end
        else if (target.PopRule = _PopPerHour) or (target.PopRule = _PopbyInterval) then
            begin
            if target.PopRule = _PopPerHour then
                OptListCtner.IntervalType := _pgiDefault
            else
                OptListCtner.IntervalType := _pginteval;

            maxpopulation := 1;
            for j := 0 to target.PedestrianData.Count - 1 do
                begin
                if target.PopRule = _PopPerHour then
                    Inc(maxpopulation, target.PedestrianData[j].PopNum)
                else
                    Inc(maxpopulation, Round((target.PedestrianData[j].PopNum / target.PopInterval) * 1800)); //30分間の発生人数
                end;

            OptListCtner.maxPopulation := maxpopulation;
            for j := 0 to 23 do
                OptListCtner.MagnificationByTime[j] := target.PopDetailArray[j];

            OptNum := target.PedestrianData.Count;
            for j := 1 to OptNum do
                begin
                if j > OptListCtner.OptionCount then
                    OptListCtner.AddNewOption;
                end;

            if DestIdx >= 0 then
                begin
                SetPedestrianProfileData(OptListCtner, DestIdx);
                gene.OptionListContainerByOD[nwk.Node[target.NodeIdx[1]], nwk.Node[POPointList.Data[i].NodeIdx[1]]] := OptListCtner;
                OptListCtner := nil;
                WeightSum := 0;
                for j := 0 to gene.OptionListContainerByOD[nwk.Node[target.NodeIdx[1]], nwk.Node[POPointList.Data[i].NodeIdx[1]]].OptionCount - 1 do
                    WeightSum := WeightSum + gene.OptionListContainerByOD[nwk.Node[target.NodeIdx[1]], nwk.Node[POPointList.Data[i].NodeIdx[1]]].OptionIF[j].Weigth;

                if WeightSum = 0 then
                    gene.DeleteOptionListContainerByOD(nwk.Node[target.NodeIdx[1]], nwk.Node[POPointList.Data[i].NodeIdx[1]]);
                end;
            end;
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.UpdateAddedProfile;
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

procedure TFramePedestrianPopOutSidePanel.SetPedOutData(const target: TPopOutPoint; nwk: IF8Networks);
    var
        i, lbIdx : integer;
        tmpWQUpdateRule : TWaitingQueueUpdateRuleType;
    begin
    if target.PointAttr = _BusTerminal then
        begin
        nwk.Node[target.NodeIdx[1]].LocationType := _pglBoardingGate;
        nwk.Node[target.NodeIdx[1]].BusModel     := target.BusModel;
        nwk.Node[target.NodeIdx[1]].BusRoadIndex := target.BusRoadIndex;
        end
    else if target.PointAttr = _TicketGate then
        nwk.Node[target.NodeIdx[1]].LocationType := _pglTicketGate
    else
        nwk.Node[target.NodeIdx[1]].LocationType := _pglDefault;

    lbIdx := -1;
    for i := 0 to F_PopOutSet.lbOutOnly.Items.Count - 1 do
        begin
        if target.Name = F_PopOutSet.lbOutOnly.Items[i] then
            begin
            lbIdx := i;
            Break;
            end;
        end;

    if lbIdx < 0 then
        Exit;

    nwk.Node[target.NodeIdx[1]].OutListIndex := lbIdx;
    nwk.Node[target.NodeIdx[1]].InOut := true;
    while nwk.Node[target.NodeIdx[1]].NumberOfWaitingQueueUpdateRule > 0 do //初期化
        nwk.Node[target.NodeIdx[1]].DeleteWaitingQueueUpdateRule(0);

    if target.OutRule = _OutbyInterval then
        begin
        nwk.Node[target.NodeIdx[1]].PedestrianExitNode := _penOByInterval;
        tmpWQUpdateRule.ReleaseTime := target.OutInterval;
        tmpWQUpdateRule.ReleaseMaxAmountAtOnce := target.OutNumOnInterval;
        nwk.Node[target.NodeIdx[1]].AddWaitingQueueUpdateRule(tmpWQUpdateRule);
        end
    else if target.OutRule = _OutbySchedule then
        begin
        nwk.Node[target.NodeIdx[1]].PedestrianExitNode := _penOBySchedule;
        for i := 0 to target.OutSchedule.DataforLink.Count - 1 do
            nwk.Node[target.NodeIdx[1]].AddWaitingQueueUpdateRule(target.OutSchedule.DataforLink[i]);
        end
    else
        nwk.Node[target.NodeIdx[1]].PedestrianExitNode := _penNoRule;
    end;

procedure TFramePedestrianPopOutSidePanel.ReceiveLabelDataFromProject;

    procedure AddandReceiveData(const pedProf : IF8PedestrianProfile);
        var
            k, WeightSum : integer;
            newData : TPedestrianData;
        begin
        newData := TPedestrianData.Create;
        newData.LabelName := pedProf.AttributeLabel;
        WeightSum := 0;
        for k := 1 to pedProf.numberOfOptions do
            WeightSum := WeightSum + pedProf.option[k].Weight;

        for k := 1 to pedProf.numberOfOptions do
            begin
            if WeightSum = pedProf.numberOfOptions then
                newData.AttrPerList.Add(100) //デフォルトは全ての属性のWeightが1になっている
            else
                newData.AttrPerList.Add(pedProf.option[k].Weight);
            end;

        p_AllPedestrianLabelList.Add(newData);
        end;

    procedure AddPedestrianLabelData(const pedProf : IF8PedestrianProfile);
        var
            j : integer;
        begin
        if p_AllPedestrianLabelList.Count = 0 then
            begin
            AddandReceiveData(pedProf);
            Exit;
            end;

        for j := 0 to p_AllPedestrianLabelList.Count - 1 do
            begin
            if (pedProf.AttributeLabel = p_AllPedestrianLabelList[j].LabelName) then
                Break;

            if j = (p_AllPedestrianLabelList.Count - 1) then
                begin
                AddandReceiveData(pedProf);
                Break;
                end;
            end;
        end;

    var
        i : integer;
    begin
    for i := 1 to theApplicationServices.project.NumberOfPedestrianProfile do
        begin
        if (theApplicationServices.project.PedestrianProfileIF[i].AttributeLabel <> '') then
            AddPedestrianLabelData(theApplicationServices.project.PedestrianProfileIF[i]);
        end;

    if p_AllPedestrianLabelList.Count > 0 then
        UpdateComboBox;
    end;

procedure TFramePedestrianPopOutSidePanel.ReceivePointDataFromProject;
    var
        i, j, PPCnt : integer;
        nwk : IF8Networks;
        gene : IF8PedestrianGenerator;
        OptListCtner : IF8PedestriansGeneratorOptionListContainer;

    procedure ReceiveOrigData(const oIdx: integer; Ctner: IF8PedestriansGeneratorOptionListContainer);
        var
            newName : string;
            newData : TPopOutPoint;
            tmpNodeIdx : TNodeNumArray;
            dispCnt : integer;

        procedure ReceiveOrigDetailData(out newData: TPopOutPoint);
            var
                pedProf : IF8PedestrianProfile;
                GeneOpt : IF8PedestriansGeneratorOption;
                newPedData : TPedestrianData;
                tmpPopDetails : TPopDetailArray;
                IsAdded : boolean;
                k, l, attrNums : integer;
            begin
            for k := 0 to 23 do
                tmpPopDetails[k] := Round(Ctner.MagnificationByTime[k]);

            newData.PopDetailArray := tmpPopDetails;
            if Ctner.OptionCount = 1 then
                begin
                if Ctner.OptionIF[0].profile.PeopleLimit = 0 then
                    Exit; //当該地点から発生する歩行者が居ない
                end;

            for k := 0 to Ctner.OptionCount - 1 do
                begin
                GeneOpt := Ctner.OptionIF[k];
                pedProf := GeneOpt.profile;
                newPedData := TPedestrianData.Create;
                IsAdded := false;
                if newData.PedestrianData.Count = 0 then
                    newPedData.LabelName := pedProf.AttributeLabel
                else
                    begin
                    for l := 0 to newData.PedestrianData.Count - 1 do
                        begin
                        if pedProf.AttributeLabel = newData.PedestrianData[l].LabelName then
                            IsAdded := true;

                        if (IsAdded = false) and (l = newData.PedestrianData.Count - 1) then
                            newPedData.LabelName := pedProf.AttributeLabel
                        end;
                    end;

                if IsAdded then
                    begin
                    FreeAndNil(newPedData);
                    Continue;
                    end;

                for l := 0 to p_AllPedestrianLabelList.Count - 1 do
                    begin
                    if newPedData.LabelName = p_AllPedestrianLabelList[l].LabelName then
                        begin
                        for attrNums := 0 to p_AllPedestrianLabelList[l].AttrPerList.Count - 1 do
                            newPedData.AttrPerList.Add(p_AllPedestrianLabelList[l].AttrPerList[attrNums]);

                        Break;
                        end;
                    end;

                newData.PedestrianData.Add(newPedData);
                end;
            end;

        begin
        newName := nwk.Node[oIdx].Name + '_' + DEFAULT_POP_POINT_NAME;
        newData := TPopOutPoint.Create(newName);
        newData.PointType := _PopPoint;
        if nwk.Node[oIdx].LocationType = _pglBoardingGate then
            newData.PointAttr := _BusTerminal
        else if nwk.Node[oIdx].LocationType = _pglTicketGate then
            newData.PointAttr := _TicketGate
        else
            newData.PointAttr := _Normal;

        tmpNodeIdx[0] := 1;
        tmpNodeIdx[1] := oIdx;
        newData.NodeIdx := tmpNodeIdx;
        if nwk.Node[oIdx].NumberOfDispersionPoints > 0 then
            begin
            newData.IsAssignedDispersionPoint := true;
            for dispCnt := 0 to nwk.Node[oIdx].NumberOfDispersionPoints - 1 do
                newData.DispersionPointList.Add(nwk.Node[oIdx].DispersionPoint[dispCnt]);
            end;

        if Ctner.IntervalType = _pginteval then
            begin
            newData.PopRule := _PopbyInterval;
            newData.PopInterval := Round(Ctner.OptionIF[0].TimeSchedule);
            end
        else if Ctner.IntervalType = _pgiTimetable then
            begin
            newData.PopRule := _PopbySchedule;
            if nwk.Node[oIdx].LocationType = _pglBoardingGate then
                begin
                newData.BusModel     := Ctner.BusModel;
                newData.BusRoadIndex := Ctner.BusRoadIndex;
                end;
            end
        else
            newData.PopRule := _PopPerHour;

        ReceiveOrigDetailData(newData);
        p_PopOutPointList.Data.Add(newData);
        end;

    procedure SortPopPointsData;
        var
            k, l, NdIdx, newNum : integer;
            tmpName : string;
            Sorted : boolean;
        begin
        Sorted := false;
        while not Sorted do
            begin
            for k := 0 to p_PopOutPointList.Data.Count - 1 do
                begin
                NdIdx := p_PopOutPointList.Data[k].NodeIdx[1];
                if (k = nwk.Node[NdIdx].PopListIndex) then
                    begin
                    if (k = p_PopOutPointList.Data.Count - 1) then
                        begin
                        Sorted := true;
                        Break;
                        end
                    else
                        Continue;
                    end;

                p_PopOutPointList.Data.Move(k, nwk.Node[NdIdx].PopListIndex);
                end;

            for k := 0 to p_PopOutPointList.Data.Count - 1 do
                begin
                NdIdx := p_PopOutPointList.Data[k].NodeIdx[1];
                if (k = nwk.Node[NdIdx].PopListIndex) then
                    begin
                    if (k = p_PopOutPointList.Data.Count - 1) then
                        begin
                        Sorted := true;
                        Break;
                        end;
                    end
                else
                    begin
                    Sorted := false;
                    Break;
                    end;
                end;
            end;

        for k := 1 to p_PopOutPointList.Data.Count - 1 do
            begin
            newNum := 0;
            tmpName := p_PopOutPointList.Data[k].Name;
            for l := 0 to p_PopOutPointList.Data.Count - 1 do
                begin
                if (tmpName = p_PopOutPointList.Data[l].Name) and (k <> l) and (k > l) then
                    begin
                    newNum := newNum + 1;
                    tmpName := p_PopOutPointList.Data[k].Name + newNum.ToString;
                    end;
                end;

            p_PopOutPointList.Data[k].Name := tmpName;
            end;
        end;

    procedure ReceiveDestData(const dIdx: integer);
        var
            newName : string;
            newData : TPopOutPoint;
            tmpNodeIdx : TNodeNumArray;
            dispCnt, k : integer;
            newScData : TScheduleData;
        begin
        newName := nwk.Node[dIdx].Name + '_' + DEFAULT_OUT_POINT_NAME;
        newData := TPopOutPoint.Create(newName);
        newData.PointType := _OutPoint;
        if nwk.Node[dIdx].LocationType = _pglBoardingGate then
            begin
            newData.PointAttr := _BusTerminal;
            newData.BusModel  := nwk.Node[dIdx].BusModel;
            newData.BusRoadIndex  := nwk.Node[dIdx].BusRoadIndex;
            end
        else if nwk.Node[dIdx].LocationType = _pglTicketGate then
            newData.PointAttr := _TicketGate
        else
            newData.PointAttr := _Normal;

        tmpNodeIdx[0] := 1;
        tmpNodeIdx[1] := dIdx;
        newData.NodeIdx := tmpNodeIdx;
        if nwk.Node[dIdx].NumberOfDispersionPoints > 0 then
            begin
            newData.IsAssignedDispersionPoint := true;
            for dispCnt := 0 to nwk.Node[dIdx].NumberOfDispersionPoints - 1 do
                newData.DispersionPointList.Add(nwk.Node[dIdx].DispersionPoint[dispCnt]);
            end;

        if nwk.Node[dIdx].PedestrianExitNode = _penOBySchedule then
            begin
            newData.OutRule := _OutbySchedule;
            for k := 0 to nwk.Node[dIdx].NumberOfWaitingQueueUpdateRule - 1 do
                begin
                newScData := TScheduleData.Create;
                newScData.Time := nwk.Node[dIdx].WaitingQueueUpdateRule[k].ReleaseTime;
                newScData.SeatOrPedNum := nwk.Node[dIdx].WaitingQueueUpdateRule[k].NumberOfBusSeat;
                newScData.Capacity := nwk.Node[dIdx].WaitingQueueUpdateRule[k].MaxPercentageGetOn;
                newScData.GetOnNum := nwk.Node[dIdx].WaitingQueueUpdateRule[k].ReleaseMaxAmountAtOnce;
                newScData.BusID := nwk.Node[dIdx].WaitingQueueUpdateRule[k].BusID;

                newData.OutSchedule.DataforLink.Add(nwk.Node[dIdx].WaitingQueueUpdateRule[k]);
                newData.OutSchedule.Data.Add(newScData);
                end;
            end
        else if nwk.Node[dIdx].PedestrianExitNode = _penOByInterval then
            begin
            newData.OutRule := _OutbyInterval;
            newData.OutInterval := Round(nwk.Node[dIdx].WaitingQueueUpdateRule[0].ReleaseTime);
            newData.OutNumOnInterval := nwk.Node[dIdx].WaitingQueueUpdateRule[0].ReleaseMaxAmountAtOnce;
            end
        else
            newData.OutRule := _NoRule;

        p_PopOutPointList.Data.Add(newData);
        end;

    procedure SortOutPointsData(const PPidx: integer);
        var
            k, l, NdIdx, newNum : integer;
            tmpName : string;
            Sorted : boolean;
        begin
        Sorted := false;
        while not Sorted do
            begin
            for k := (PPidx+1) to p_PopOutPointList.Data.Count - 1 do
                begin
                NdIdx := p_PopOutPointList.Data[k].NodeIdx[1];
                if (k = ((PPidx+1) + nwk.Node[NdIdx].OutListIndex)) then
                    begin
                    if (k = p_PopOutPointList.Data.Count - 1) then
                        begin
                        Sorted := true;
                        Break;
                        end
                    else
                        Continue;
                    end;

                p_PopOutPointList.Data.Move(k, ((PPidx+1) + nwk.Node[NdIdx].OutListIndex));
                end;

            for k := (PPidx+1) to p_PopOutPointList.Data.Count - 1 do
                begin
                NdIdx := p_PopOutPointList.Data[k].NodeIdx[1];
                if (k = ((PPidx+1) + nwk.Node[NdIdx].OutListIndex)) then
                    begin
                    if (k = p_PopOutPointList.Data.Count - 1) then
                        begin
                        Sorted := true;
                        Break;
                        end;
                    end
                else
                    begin
                    Sorted := false;
                    Break;
                    end;
                end;
            end;

        for k := (PPidx+1)+1 to p_PopOutPointList.Data.Count - 1 do
            begin
            newNum := 0;
            tmpName := p_PopOutPointList.Data[k].Name;
            for l := (PPidx+1) to p_PopOutPointList.Data.Count - 1 do
                begin
                if (tmpName = p_PopOutPointList.Data[l].Name) and (k <> l) and (k > l) then
                    begin
                    newNum := newNum + 1;
                    tmpName := p_PopOutPointList.Data[k].Name + newNum.ToString;
                    end;
                end;

            p_PopOutPointList.Data[k].Name := tmpName;
            end;
        end;

    procedure ReceiveOrigPointData(const oIdx, dIdx: integer; Ctner: IF8PedestriansGeneratorOptionListContainer);
        var
            k : integer;
            IsAddedOrig : boolean;
        begin
        IsAddedOrig := false;
        for k := 0 to p_PopOutPointList.Data.Count - 1 do
            begin
            if (oIdx = p_PopOutPointList.Data[k].NodeIdx[1]) and (p_PopOutPointList.Data[k].PointType = _PopPoint) then
                begin
                IsAddedOrig := true;
                Break;
                end;
            end;

        if not IsAddedOrig then
            ReceiveOrigData(oIdx, Ctner);
        end;

    procedure ReceiveDestPointData(const dIdx: integer);
        var
            k : integer;
            IsAddedDest : boolean;
        begin
        IsAddedDest := false;
        for k := 0 to p_PopOutPointList.Data.Count - 1 do
            begin
            if (dIdx = p_PopOutPointList.Data[k].NodeIdx[1]) and (p_PopOutPointList.Data[k].PointType = _OutPoint) then
                begin
                IsAddedDest := true;
                Break;
                end;
            end;

        if not IsAddedDest then
            ReceiveDestData(dIdx);
        end;

    procedure AddDestination(const Dest: TPopOutPoint);
        var
            k, pedDIdx : integer;
        begin
        for k := 0 to p_PopOutPointList.Data.Count - 1 do
            begin
            if (p_PopOutPointList.Data[k].PointType = _PopPoint) and (p_PopOutPointList.Data[k].NodeIdx[1] <> Dest.NodeIdx[1]) then
                begin
                p_PopOutPointList.Data[k].DestinationList.Add(Dest.Name);
                for pedDIdx := 0 to p_PopOutPointList.Data[k].PedestrianData.Count - 1 do
                    p_PopOutPointList.Data[k].PedestrianData[pedDIdx].DestPerList.Add(100);
                end;
            end;
        end;

    procedure CreatePopNumReceiver(const Ctner: IF8PedestriansGeneratorOptionListContainer; orig: TPopOutPoint; IsEmpty: boolean; out PopNums: TDictionary<string, TList<integer>>; out tmpNumList: TList<integer>);
        var
            k : integer;
            pedProf : IF8PedestrianProfile;
            GeneOpt : IF8PedestriansGeneratorOption;
            tmpRoundMode: TRoundingMode;
        begin
        for k := 0 to Ctner.OptionCount - 1 do
            begin
            GeneOpt := Ctner.OptionIF[k];
            pedProf := GeneOpt.profile;
            if not PopNums.ContainsKey(pedProf.AttributeLabel) then
                begin
                tmpNumList := TList<integer>.Create;
                PopNums.Add(pedProf.AttributeLabel, tmpNumList);
                end;
            end;

        for k := 0 to Ctner.OptionCount - 1 do
            begin
            GeneOpt := Ctner.OptionIF[k];
            pedProf := GeneOpt.profile;
            if PopNums.ContainsKey(pedProf.AttributeLabel) then
                begin
                if IsEmpty then
                    PopNums[pedProf.AttributeLabel].Add(0)
                else
                    begin
                    tmpRoundMode := GetRoundMode;
                    try
                        SetRoundMode(rmTruncate);
                        if orig.PopRule = _PopPerHour then
                            PopNums[pedProf.AttributeLabel].Add(Round(SimpleRoundTo((pedProf.PeopleLimit / GeneOpt.TimeSchedule) * 3600, 0)))
                        else
                            PopNums[pedProf.AttributeLabel].Add(pedProf.PeopleLimit);
                    finally
                        SetRoundMode(tmpRoundMode);
                        end;
                    end;
                end;
            end;
        end;

    procedure CalcOrigDetailData(const orig: TPopOutPoint);
        var
            k, destIdx, destCnt, LblIdx, Nums, Sum, DestPer, emptyNum : integer;
            pedProf : IF8PedestrianProfile;
            GeneOpt : IF8PedestriansGeneratorOption;
            Ctner : IF8PedestriansGeneratorOptionListContainer;
            PopNums : TDictionary<string, TList<integer>>;
            tmpNumList : TList<integer>;
        begin
        PopNums := TDictionary<string, TList<integer>>.Create;
        emptyNum := 0;
        for destIdx := 0 to p_PopOutPointList.Data.Count - 1 do
            begin
            if (not (p_PopOutPointList.Data[destIdx].PointType = _OutPoint)) or (orig.NodeIdx[1] = p_PopOutPointList.Data[destIdx].NodeIdx[1]) then
                Continue;

            Ctner := gene.OptionListContainerByOD[nwk.Node[orig.NodeIdx[1]], nwk.Node[p_PopOutPointList.Data[destIdx].NodeIdx[1]]];
            if not Assigned(Ctner) then
                begin
                emptyNum := emptyNum + 1;
                Continue;
                end;

            if emptyNum > 0 then
                begin
                while emptyNum > 0 do
                    begin
                    CreatePopNumReceiver(Ctner, orig, true, PopNums, tmpNumList);
                    emptyNum := emptyNum - 1;
                    end;
                end;

            CreatePopNumReceiver(Ctner, orig, false, PopNums, tmpNumList);
            end;

        for LblIdx := 0 to orig.PedestrianData.Count - 1 do
            begin
            destCnt := 0;
            orig.PedestrianData[LblIdx].PopNum := 0;
            for destIdx := 0 to p_PopOutPointList.Data.Count - 1 do
                begin
                if (not (p_PopOutPointList.Data[destIdx].PointType = _OutPoint)) or (orig.NodeIdx[1] = p_PopOutPointList.Data[destIdx].NodeIdx[1]) then
                    Continue;

                Ctner := gene.OptionListContainerByOD[nwk.Node[orig.NodeIdx[1]], nwk.Node[p_PopOutPointList.Data[destIdx].NodeIdx[1]]];
                if not Assigned(Ctner) then
                    begin
                    orig.PedestrianData[LblIdx].DestPerList[destCnt] := 0;
                    destCnt := destCnt + 1;
                    Continue;
                    end;

                GeneOpt := Ctner.OptionIF[0];
                pedProf := GeneOpt.profile;
                Sum := 0;
                for k := 0 to Ctner.OptionCount - 1 do
                    begin
                    GeneOpt := Ctner.OptionIF[k];
                    pedProf := GeneOpt.profile;
                    if orig.PedestrianData[LblIdx].LabelName = pedProf.AttributeLabel then
                        begin
                        Sum := 0;
                        for Nums := 0 to PopNums[pedProf.AttributeLabel].Count - 1 do
                            Sum := Sum + PopNums[pedProf.AttributeLabel][Nums];

                        orig.PedestrianData[LblIdx].PopNum := Sum;
                        Break;
                        end;
                    end;

                DestPer := Round((PopNums[pedProf.AttributeLabel][destCnt] / Sum) * 100);
                orig.PedestrianData[LblIdx].DestPerList[destCnt] := DestPer;
                destCnt := destCnt + 1;
                end;
            end;

        FreeAndNil(PopNums);
        end;

    procedure CalcOrigScheData(const orig: TPopOutPoint);
        var
            destIdx, destCnt, LblIdx, Sum, DestPer, targetIdx, emptyNum : integer;
            pedProf : IF8PedestrianProfile;
            GeneOpt : IF8PedestriansGeneratorOption;
            Ctner : IF8PedestriansGeneratorOptionListContainer;
            PopNums : TDictionary<string, TDictionary<TTime, TList<integer>>>;
            tmpScDataList : TDictionary<TTime, TList<integer>>;
            tmpNumList : TList<integer>;
            newScheData : TScheduleData;
            initLbl, targetLbl : string;

        procedure CreatePopNumsReceiver(const IsEmpty: boolean = false);
            var
                idx : integer;
            begin
            for idx := 0 to Ctner.OptionCount - 1 do
                begin
                GeneOpt := Ctner.OptionIF[idx];
                pedProf := GeneOpt.profile;
                if not PopNums.ContainsKey(pedProf.AttributeLabel) then
                    begin
                    tmpScDataList := TDictionary<TTime, TList<integer>>.Create;
                    tmpNumList := TList<integer>.Create;
                    tmpScDataList.Add(GeneOpt.TimeSchedule, tmpNumList);
                    PopNums.Add(pedProf.AttributeLabel, tmpScDataList);
                    end
                else
                    begin
                    if not PopNums[pedProf.AttributeLabel].ContainsKey(GeneOpt.TimeSchedule) then
                        begin
                        tmpNumList := TList<integer>.Create;
                        PopNums[pedProf.AttributeLabel].Add(GeneOpt.TimeSchedule, tmpNumList);
                        end;
                    end;
                end;

            for idx := 0 to Ctner.OptionCount - 1 do
                begin
                GeneOpt := Ctner.OptionIF[idx];
                pedProf := GeneOpt.profile;
                if PopNums.ContainsKey(pedProf.AttributeLabel) then
                    begin
                    if PopNums[pedProf.AttributeLabel].ContainsKey(GeneOpt.TimeSchedule) then
                        begin
                        if IsEmpty then
                            PopNums[pedProf.AttributeLabel][GeneOpt.TimeSchedule].Add(0)
                        else
                            PopNums[pedProf.AttributeLabel][GeneOpt.TimeSchedule].Add(pedProf.PeopleLimit);
                        end;
                    end;
                end;
            end;

        procedure CalcPopNum(out GenOpt: IF8PedestriansGeneratorOption; pdProf: IF8PedestrianProfile; aSum, atargetIdx: integer);
            var
                optIdx, PedDataIdx, PNIdx : integer;
                tmpRoundMode: TRoundingMode;
            begin
            for optIdx := 0 to Ctner.OptionCount - 1 do
                begin
                if ((optIdx+1) > orig.PopSchedule.Data.Count) and (Ctner.OptionIF[optIdx].profile.AttributeLabel = initLbl) {and (destCnt = 0)} then
                    begin
                    newScheData := TScheduleData.Create;
                    for PedDataIdx := 0 to orig.PedestrianData.Count - 1 do
                        newScheData.LblPopNums.Add(0);

                    orig.PopSchedule.Data.Add(newScheData);
                    atargetIdx := orig.PopSchedule.Data.Count - 1;
                    end
                else if Ctner.OptionIF[optIdx].profile.AttributeLabel = targetLbl then
                    begin
                    if atargetIdx = orig.PopSchedule.Data.Count - 1 then
                        atargetIdx := 0
                    else
                        atargetIdx := atargetIdx + 1;
                    end;

                GenOpt := Ctner.OptionIF[optIdx];
                pdProf := GenOpt.profile;
                if atargetIdx >= 0 then
                    begin
                    tmpRoundMode := GetRoundMode;
                    try
                        SetRoundMode(rmTruncate);
                        orig.PopSchedule.Data[atargetIdx].Time := GenOpt.TimeSchedule;
                        orig.PopSchedule.Data[atargetIdx].BusID := GenOpt.BusID;
                        if orig.PedestrianData[LblIdx].LabelName = pdProf.AttributeLabel then
                            begin
                            aSum := 0;
                            if PopNums[pdProf.AttributeLabel].ContainsKey(GenOpt.TimeSchedule) then
                                begin
                                for PNIdx := 0 to PopNums[pdProf.AttributeLabel][GenOpt.TimeSchedule].Count - 1 do
                                    aSum := aSum + PopNums[pdProf.AttributeLabel][GenOpt.TimeSchedule][PNIdx];
                                end;

                            orig.PopSchedule.Data[atargetIdx].LblPopNums[LblIdx] := aSum;
                            if aSum <> 0 then
                                begin
                                DestPer := 100;
                                if PopNums[pdProf.AttributeLabel].ContainsKey(GenOpt.TimeSchedule) then
                                    DestPer := Round((PopNums[pdProf.AttributeLabel][GenOpt.TimeSchedule][destCnt] / aSum) * 100);

                                orig.PedestrianData[LblIdx].DestPerList[destCnt] := DestPer;
                                end;
                            end;
                    finally
                        SetRoundMode(tmpRoundMode);
                        end;
                    end;
                end;
            end;

        begin
        PopNums := TDictionary<string, TDictionary<TTime, TList<integer>>>.Create;
        emptyNum := 0;
        for destIdx := 0 to p_PopOutPointList.Data.Count - 1 do
            begin
            if (not (p_PopOutPointList.Data[destIdx].PointType = _OutPoint)) or (orig.NodeIdx[1] = p_PopOutPointList.Data[destIdx].NodeIdx[1]) then
                Continue;

            Ctner := gene.OptionListContainerByOD[nwk.Node[orig.NodeIdx[1]], nwk.Node[p_PopOutPointList.Data[destIdx].NodeIdx[1]]];
            if not Assigned(Ctner) then
                begin
                emptyNum := emptyNum + 1;
                Continue;
                end;

            if emptyNum > 0 then
                begin
                while emptyNum > 0 do
                    begin
                    CreatePopNumsReceiver(true);
                    emptyNum := emptyNum - 1;
                    end;
                end;

            CreatePopNumsReceiver;
            end;

        for LblIdx := 0 to orig.PedestrianData.Count - 1 do
            begin
            destCnt := 0;
            for destIdx := 0 to p_PopOutPointList.Data.Count - 1 do
                begin
                if (not (p_PopOutPointList.Data[destIdx].PointType = _OutPoint)) or (orig.NodeIdx[1] = p_PopOutPointList.Data[destIdx].NodeIdx[1]) then
                    Continue;

                Ctner := gene.OptionListContainerByOD[nwk.Node[orig.NodeIdx[1]], nwk.Node[p_PopOutPointList.Data[destIdx].NodeIdx[1]]];
                if not Assigned(Ctner) then
                    begin
                    orig.PedestrianData[LblIdx].DestPerList[destCnt] := 0;
                    destCnt := destCnt + 1;
                    Continue;
                    end;

                GeneOpt := Ctner.OptionIF[0];
                pedProf := GeneOpt.profile;
                initLbl := pedProf.AttributeLabel;
                targetLbl := orig.PedestrianData[LblIdx].LabelName;
                Sum := 0;
                targetIdx := -1;
                CalcPopNum(GeneOpt, pedProf, Sum, targetIdx);
                destCnt := destCnt + 1;
                end;
            end;

        FreeAndNil(PopNums);
        end;

    begin
    if theApplicationServices.project.numberOfFlightWayNwks < 1 then
        Exit;

    nwk := theApplicationServices.project.FlightWayNwk[1];
    if nwk.Generator[nwk.NumberOfGenerator - 1].TypeOfGenerator = _pgNormal then
        Exit;

    gene := nwk.Generator[nwk.NumberOfGenerator - 1];
    for i := 0 to nwk.NodeCount - 1 do
        begin
        if (nwk.Node[i].PopListIndex < 0) then
            Continue;

        for j := 0 to nwk.NodeCount - 1 do
            begin
            if (i = j) then
               Continue;

            OptListCtner := gene.OptionListContainerByOD[nwk.Node[i], nwk.Node[j]];
            if Assigned(OptListCtner) then
                begin
                nwk.Node[i].InOut := true;
                nwk.Node[j].InOut := true;
                ReceiveOrigPointData(i, j, OptListCtner);
                end;
            end;
        end;

    if p_PopOutPointList.Data.Count < 1 then
        Exit;

    SortPopPointsData;
    PPCnt := p_PopOutPointList.Data.Count - 1;
    for i := 0 to nwk.NodeCount - 1 do
        begin
        if (nwk.Node[i].OutListIndex >= 0) then
            begin
            nwk.Node[i].InOut := true;
            ReceiveDestPointData(i);
            end;
        end;

    SortOutPointsData(PPCnt);
    if p_PopOutPointList.Data.Count > 0 then
        begin
        for i := 0 to p_PopOutPointList.Data.Count - 1 do
            begin
            if p_PopOutPointList.Data[i].PointType = _OutPoint then
                AddDestination(p_PopOutPointList.Data[i]);
            end;

        //各地点の目的地のリストを一通り作成した後で残りのデータを読み込み(算出)
        for i := 0 to p_PopOutPointList.Data.Count - 1 do
            begin
            if (p_PopOutPointList.Data[i].PointType = _PopPoint) and (p_PopOutPointList.Data[i].PedestrianData.Count > 0) then
                begin
                if p_PopOutPointList.Data[i].PopRule = _PopbySchedule then
                    CalcOrigScheData(p_PopOutPointList.Data[i])
                else
                    CalcOrigDetailData(p_PopOutPointList.Data[i]);
                end;
            end;

        UpdateListBox;
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.UpdateListBox;
    var
        i : integer;
    begin
    for i := 0 to p_PopOutPointList.Data.Count - 1 do
        begin
        if p_PopOutPointList.Data[i].PointType = _PopPoint then
            F_PopOutSet.ListBoxInOutPoints.Items.Add(p_PopOutPointList.Data[i].Name)
        else
            F_PopOutSet.lbOutOnly.Items.Add(p_PopOutPointList.Data[i].Name);
        end;
    end;

procedure TFramePedestrianPopOutSidePanel.UpdateComboBox;
    var
        i : integer;
    begin
    for i := 0 to p_AllPedestrianLabelList.Count - 1 do
        F_PopOutSet.PopSettingsFrame.cbbPedLabel.Items.Add(p_AllPedestrianLabelList[i].LabelName);
    end;

function TFramePedestrianPopOutSidePanel.IsNothingPopOrOut: boolean;
    begin
    if (F_PopOutSet.ListBoxInOutPoints.Items.Count < 1) or (F_PopOutSet.lbOutOnly.Items.Count < 1) then
        Result := true
    else
        Result := false;
    end;
end.
