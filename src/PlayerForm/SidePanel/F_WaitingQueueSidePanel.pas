unit F_WaitingQueueSidePanel;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
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
    F_WaitingAreaSettings,
    WQAreaEditingDialog,
    WQAreaRenderer,
    WaitingQueueAreaList,
    PopOutPointList,
    SimulationInputUtils;

type
    TFrameWaitingQueueSidePanel = class(TFrame)
        F_WaitAreaSet: TFrameWaitingAreaSettings;
        procedure chbVisibleWaitingAreaClick(Sender: TObject);
        procedure btnSetWaitingAreaClick(Sender: TObject);
        procedure btnDeleteWaitingAreaClick(Sender: TObject);
        procedure btnSelectedViewClick(Sender: TObject);
        procedure cbbExitIntervalRuleChange(Sender: TObject);
        procedure btnCreateWaitingQueueClick(Sender: TObject);
        procedure sePedMarginOnChange(Sender: TObject);
        procedure cbbSelectWaitingAreaChange(Sender: TObject);
        procedure seExitIntervalChange(Sender: TObject);
        procedure seExitNumChange(Sender: TObject);
        procedure btnSelectNodeClick(Sender: TObject);
        procedure btnMovetoNodeClick(Sender: TObject);
        procedure TimerWaitMainProcTimer(Sender: TObject);
        procedure btnSelectBusTerminalClick(Sender: TObject);
        procedure btnMovetoBusTerminalClick(Sender: TObject);
        procedure btnBusTerminalLinkFreeClick(Sender: TObject);
        procedure btnLinkWQandAreaClick(Sender: TObject);
        procedure TimerWaitResetJumpingStateTimer(Sender: TObject);
        procedure btnWQNodeChangeClick(Sender: TObject);
        procedure btnMovetoWQNodeClick(Sender: TObject);
        procedure btnLinkFreeClick(Sender: TObject);

        private
            p_WQAreaList: TWaitingQueueAreaList;
            p_IsActiveSelectMode : boolean;
            p_IsActiveExitSelectMode : boolean;
            p_FirstPoint : GLPointType;
            p_SecondPoint : GLPointType;
            p_RenderDataName : String;
            p_RenderData : TRenderDataArray;
            p_DelDataIdx : integer;
            p_ViewIdx    : integer;
            p_Dialog : TFormWQAreaEditingDialog;
            p_DisableTW : Pointer;
            p_tmpMoveMode : MoveModeType;
            p_EditFinished : boolean;
            p_IsEditing : boolean;
            p_IsSelectingMode : boolean;
            p_tmpSelectedNodeNum : TNodeNumArray;
            p_TimerWaitMainProc  : TTimer;
            p_IsChanging : boolean;
            p_IsExitSelectingMode : boolean;
            p_TimerWaitResetJumpingState : TTimer;

            procedure FinishEditMode;
            procedure OnClickDialogOK(Sender: TObject);
            procedure OnClickDialogCancel(Sender: TObject);
            procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
            procedure OnKeyDownSelectingMode(Sender: TObject; var Key: Word; Shift: TShiftState);
            procedure OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
            procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
            procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
            procedure Initialize;
            procedure SetWQAreaList(const aValue: TWaitingQueueAreaList);
            procedure SetNodeData;
            procedure AfterPaintOpenGLSelectingMode;
            procedure FinishSelectMode;
            procedure UpdatelbWQ(const nodeIdx: TNodeNumArray);
            procedure UpdatelbLinkedWQ(const nodeIdx: TNodeNumArray);
            function  GetWQAreaList: TWaitingQueueAreaList;

            procedure RegisterKeyEvents;
            procedure UnRegisterKeyEvent;

        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure UpdateWQAreaRendererData(const aRenderer: TWQAreaRenderer);
            procedure OnShow;

            property  WQAreaList  : TWaitingQueueAreaList read GetWQAreaList write SetWQAreaList;
        end;

implementation

uses
    WaitingQueueAreaRebuilder;

{$R *.dfm}

procedure TFrameWaitingQueueSidePanel.AfterConstruction;
    begin
    inherited;

    p_EditFinished := false;
    p_RenderDataName := '';
    p_RenderData[0] := NAN_POINT;
    p_RenderData[1] := NAN_POINT;
    p_DelDataIdx := -1;
    p_ViewIdx    := -1;
    p_IsEditing := false;
    p_IsActiveExitSelectMode := false;
    p_WQAreaList := nil;
    F_WaitAreaSet.sePedestrianMargin.OnChange := sePedMarginOnChange;
    p_tmpSelectedNodeNum[0] := -1;
    p_tmpSelectedNodeNum[1] := -1;
    end;

procedure TFrameWaitingQueueSidePanel.BeforeDestruction;
    begin
    p_WQAreaList := nil;
    p_RenderData[0] := NAN_POINT;
    p_RenderData[1] := NAN_POINT;
    UnRegisterKeyEvent;
    inherited;
    end;

procedure TFrameWaitingQueueSidePanel.chbVisibleWaitingAreaClick(Sender: TObject);
    begin
    F_WaitAreaSet.WaitingAreaVisible := F_WaitAreaSet.chbVisibleWaitingArea.Checked;
    end;

procedure TFrameWaitingQueueSidePanel.btnSetWaitingAreaClick(Sender: TObject);
    var
        m : TMethod;
    begin
    if F_WaitAreaSet.edtWaitingAreaName.Text = '' then
        begin
        ShowMessage('待機列形成範囲名を入力してください');
        Exit;
        end;

    p_IsExitSelectingMode := true;
    theApplicationServices.AddDisplayMessage(SELECT_EXIT_NODE_MESSAGE_ACTION);
    theApplicationServices.AddDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);

    theApplicationServices.mainForm.SetIsNodeSelectingOnCrowdSim(true);
    FormMainKeyUpDownProc(m) := OnKeyDownSelectingMode;
    theApplicationServices.RegisterEventHandler(_plgFormMainKeyDown, m);
    PluginNotifyProc(m) := AfterPaintOpenGLSelectingMode;
    theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
    F_WaitAreaSet.Enabled := false;
    end;

procedure TFrameWaitingQueueSidePanel.RegisterKeyEvents;
    var
        m : TMethod;
    begin
    p_IsActiveSelectMode := true;
    F_WaitAreaSet.btnSetWaitingArea.Enabled := false;

    FormMainOpenGLMouseUpDownProc(m) := OnMouseDown;
    theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLMouseDown, m);
    FormMainOpenGLMouseUpDownProc(m) := OnMouseUp;
    theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLMouseUp, m);
    FormMainKeyUpDownProc(m) := OnKeyDown;
    theApplicationServices.RegisterEventHandler(_plgFormMainKeyDown, m);
    FormMainKeyUpDownProc(m) := OnKeyUp;
    theApplicationServices.RegisterEventHandler(_plgFormMainKeyUp, m);
    end;

procedure TFrameWaitingQueueSidePanel.UnRegisterKeyEvent;
    var
        m : TMethod;
    begin
    FormMainKeyUpDownProc(m) := OnKeyUp;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainKeyUp, m);
    FormMainKeyUpDownProc(m) := OnKeyDown;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainKeyDown, m);
    FormMainOpenGLMouseUpDownProc(m) := OnMouseUp;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLMouseUp, m);
    FormMainOpenGLMouseUpDownProc(m) := OnMouseDown;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLMouseDown, m);

    p_IsActiveSelectMode := false;
    end;

procedure TFrameWaitingQueueSidePanel.UpdateWQAreaRendererData(const aRenderer: TWQAreaRenderer);
    var
        i, SelIdx : integer;
    begin
    if not Assigned(F_WaitAreaSet) then
        Exit;

    if F_WaitAreaSet.cbbSelectWaitingArea.Items.Count <> WQAreaList.Data.Count then
        begin
        F_WaitAreaSet.cbbSelectWaitingArea.Items.Clear;
        for i := 0 to WQAreaList.Data.Count - 1 do
            F_WaitAreaSet.cbbSelectWaitingArea.Items.Add(WQAreaList.Data[i].Name);
        end;

    aRenderer.SetIsActiveEditingRender(p_IsEditing);
    if p_ViewIdx >= 0 then
        begin
        aRenderer.MoveCameraToWQArea(p_ViewIdx);
        p_ViewIdx := -1;
        theApplicationServices.mainForm.openGL.DoMouseDown(mbLeft, [], 0, 0);
        theApplicationServices.mainForm.openGL.DoMouseMove([], 1, 1);
        theApplicationServices.mainForm.openGL.DoMouseUp(mbLeft, [], 1, 1);
        end;

    if p_RenderDataName <> '' then
        begin
        aRenderer.UpdateDrawDataList(p_RenderDataName, p_RenderData);
        p_RenderData[0] := NAN_POINT;
        p_RenderData[1] := NAN_POINT;
        p_RenderDataName := '';
        end;

    if p_DelDataIdx >= 0 then
        begin
        aRenderer.DeleteData(p_DelDataIdx);
        F_WaitAreaSet.cbbSelectWaitingArea.Items.Delete(p_DelDataIdx);
        p_DelDataIdx := -1;
        end;

    if p_IsActiveSelectMode then
        aRenderer.SetEditingPoints(p_FirstPoint, p_SecondPoint)
    else
        begin
        aRenderer.SetEditingPoints(NAN_POINT, NAN_POINT);
        SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
        aRenderer.SetFocusedPoints(SelIdx);
        end;

    if (not p_IsActiveSelectMode) and (not F_WaitAreaSet.btnSetWaitingArea.Enabled) then
        F_WaitAreaSet.btnSetWaitingArea.Enabled := true;

    if (F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex >= 0) and (p_IsActiveExitSelectMode) then
        UpdatelbWQ(p_WQAreaList.Data[F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex].WaitingQueueNodeIdx);
    end;

procedure TFrameWaitingQueueSidePanel.FinishEditMode;
    begin
    theApplicationServices.ClearDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
    theApplicationServices.ClearDisplayMessage(SELECT_MODE_MESSAGE_ACTION);
    UnRegisterKeyEvent;
    end;

procedure TFrameWaitingQueueSidePanel.seExitIntervalChange(Sender: TObject);
    var
        SelIdx, NwkIdx, NdIdx : integer;
        node : IF8NetworkNode;
    begin
    if F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex < 0 then
        Exit;

    SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    WQAreaList.Data[SelIdx].ExitInterval := F_WaitAreaSet.FrameExitInterval1.seExitInterval.iValue;
    NwkIdx := WQAreaList.Data[SelIdx].WaitingQueueNodeIdx[0];
    NdIdx  := WQAreaList.Data[SelIdx].WaitingQueueNodeIdx[1];
    if (NwkIdx >= 1) and (NdIdx >= 0) and (WQAreaList.Data[SelIdx].CurrentWQIndex > 0) then
        begin
        node := theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx];
        node.GetIntWaitingQueue(WQAreaList.Data[SelIdx].CurrentWQIndex).ConstantReleaseProcessTimelimit := F_WaitAreaSet.FrameExitInterval1.seExitInterval.iValue;
        end;
    end;

procedure TFrameWaitingQueueSidePanel.seExitNumChange(Sender: TObject);
    var
        SelIdx, NwkIdx, NdIdx : integer;
        node : IF8NetworkNode;
    begin
    if F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex < 0 then
        Exit;

    SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    WQAreaList.Data[SelIdx].ExitPedNum := F_WaitAreaSet.FrameExitInterval1.seExitNum.iValue;
    NwkIdx := WQAreaList.Data[SelIdx].WaitingQueueNodeIdx[0];
    NdIdx  := WQAreaList.Data[SelIdx].WaitingQueueNodeIdx[1];
    if (NwkIdx >= 1) and (NdIdx >= 0) and (WQAreaList.Data[SelIdx].CurrentWQIndex > 0) then
        begin
        node := theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx];
        node.GetIntWaitingQueue(WQAreaList.Data[SelIdx].CurrentWQIndex).ConstantReleaseAmountAtOnce     := F_WaitAreaSet.FrameExitInterval1.seExitNum.iValue;
        end;
    end;

procedure TFrameWaitingQueueSidePanel.cbbSelectWaitingAreaChange(Sender: TObject);
    var
        SelIdx, i, NwkIdx, NdIdx : integer;
    begin
    if F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex < 0 then
        Exit;

    SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    F_WaitAreaSet.sePedestrianMargin.Value := WQAreaList.Data[SelIdx].PedestrianMargin;
    for i := 0 to WQAreaList.Data.Count - 1 do
        begin
        NwkIdx := WQAreaList.Data[i].LinkedNodeIdx[0];
        NdIdx := WQAreaList.Data[i].LinkedNodeIdx[1];
        if (NwkIdx >= 0) and (NdIdx >= 0) then
            begin
            if i = SelIdx then
                F_WaitAreaSet.edtBusTerminal.Text := theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].Name;
            end
        else
            F_WaitAreaSet.edtBusTerminal.Text := '';

        NwkIdx := WQAreaList.Data[i].WaitingQueueNodeIdx[0];
        NdIdx := WQAreaList.Data[i].WaitingQueueNodeIdx[1];
        if (NwkIdx >= 0) and (NdIdx >= 0) then
            begin
            if i = SelIdx then
                begin
                F_WaitAreaSet.edtWQNode.Text := theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].Name;
                UpdatelbWQ(WQAreaList.Data[i].WaitingQueueNodeIdx);
                UpdatelbLinkedWQ(WQAreaList.Data[i].WaitingQueueNodeIdx);
                end;
            end
        else
            F_WaitAreaSet.edtWQNode.Text := '';

        if i = SelIdx then
            Break;
        end;

    if WQAreaList.Data[SelIdx].IsUniqueExitRule then
        begin
        F_WaitAreaSet.cbbExitIntervalRule.ItemIndex  := 1;
        F_WaitAreaSet.FrameExitInterval1.seExitInterval.Value := WQAreaList.Data[SelIdx].ExitInterval;
        F_WaitAreaSet.FrameExitInterval1.seExitNum.Value      := WQAreaList.Data[SelIdx].ExitPedNum;
        F_WaitAreaSet.FrameExitInterval1.Visible     := true;
        end
    else
        begin
        F_WaitAreaSet.cbbExitIntervalRule.ItemIndex  := 0;
        F_WaitAreaSet.FrameExitInterval1.Visible     := false;
        end;
    end;

procedure TFrameWaitingQueueSidePanel.btnCreateWaitingQueueClick(Sender: TObject);
    const
        EYE_HEIGHT = 30;
    var
        SelIdx, NwkIdx, NdIdx : integer;
        eye, view : GLPointType;
    begin
    SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    if SelIdx < 0 then
        Exit;

    NwkIdx   := WQAreaList.Data[SelIdx].WaitingQueueNodeIdx[0];
    NdIdx    := WQAreaList.Data[SelIdx].WaitingQueueNodeIdx[1];
    if (NwkIdx < 0) or (NdIdx < 0) then
        Exit;

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
    if not Assigned(p_TimerWaitResetJumpingState) then
        begin
        p_TimerWaitResetJumpingState := TTimer.Create(nil);
        p_TimerWaitResetJumpingState.Interval := 200;
        p_TimerWaitResetJumpingState.OnTimer  := TimerWaitResetJumpingStateTimer;
        end;
    end;

procedure TFrameWaitingQueueSidePanel.cbbExitIntervalRuleChange(Sender: TObject);
    var
        SelIdx, NwkIdx, NdIdx : integer;
        node : IF8NetworkNode;
    begin
    if F_WaitAreaSet.cbbExitIntervalRule.ItemIndex = 0 then
        F_WaitAreaSet.FrameExitInterval1.Visible := false
    else if F_WaitAreaSet.cbbExitIntervalRule.ItemIndex = 1 then
        F_WaitAreaSet.FrameExitInterval1.Visible := true
    else
        F_WaitAreaSet.FrameExitInterval1.Visible := false;

    if F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex < 0 then
        Exit;

    SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    WQAreaList.Data[SelIdx].IsUniqueExitRule := F_WaitAreaSet.cbbExitIntervalRule.ItemIndex.ToBoolean;
    NwkIdx := WQAreaList.Data[SelIdx].WaitingQueueNodeIdx[0];
    NdIdx  := WQAreaList.Data[SelIdx].WaitingQueueNodeIdx[1];
    if (NwkIdx >= 1) and (NdIdx >= 0) and (WQAreaList.Data[SelIdx].CurrentWQIndex > 0) then
        begin
        if WQAreaList.Data[SelIdx].IsUniqueExitRule then
            begin
            node := theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx];
            WQAreaList.Data[SelIdx].ExitInterval := F_WaitAreaSet.FrameExitInterval1.seExitInterval.iValue;
            node.GetIntWaitingQueue(WQAreaList.Data[SelIdx].CurrentWQIndex).ConstantReleaseProcessTimelimit := F_WaitAreaSet.FrameExitInterval1.seExitInterval.iValue;
            WQAreaList.Data[SelIdx].ExitPedNum := F_WaitAreaSet.FrameExitInterval1.seExitNum.iValue;
            node.GetIntWaitingQueue(WQAreaList.Data[SelIdx].CurrentWQIndex).ConstantReleaseAmountAtOnce     := F_WaitAreaSet.FrameExitInterval1.seExitNum.iValue;
            node.GetIntWaitingQueue(WQAreaList.Data[SelIdx].CurrentWQIndex).IsLinkedSchedule := false;
            end
        else
            begin
            node := theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx];
            node.GetIntWaitingQueue(WQAreaList.Data[SelIdx].CurrentWQIndex).IsLinkedSchedule := true;
            end;
        end
    else
        begin
        if WQAreaList.Data[SelIdx].IsUniqueExitRule then
            begin
            WQAreaList.Data[SelIdx].ExitInterval := F_WaitAreaSet.FrameExitInterval1.seExitInterval.iValue;
            WQAreaList.Data[SelIdx].ExitPedNum := F_WaitAreaSet.FrameExitInterval1.seExitNum.iValue;
            end;
        end;
    end;

procedure TFrameWaitingQueueSidePanel.btnSelectedViewClick(Sender: TObject);
    begin
    p_ViewIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    end;

procedure TFrameWaitingQueueSidePanel.btnDeleteWaitingAreaClick(Sender: TObject);
    begin
    p_DelDataIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex := -1;
    if p_DelDataIdx >= 0 then
        WQAreaList.DeleteData(p_DelDataIdx);
    end;

procedure TFrameWaitingQueueSidePanel.sePedMarginOnChange(Sender: TObject);
    var
        SelIdx, NwkIdx, NdIdx : integer;
        node : IF8NetworkNode;
    begin
    if F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex < 0 then
        Exit;

    SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    WQAreaList.Data[SelIdx].PedestrianMargin := F_WaitAreaSet.sePedestrianMargin.Value;
    NwkIdx := WQAreaList.Data[SelIdx].WaitingQueueNodeIdx[0];
    NdIdx  := WQAreaList.Data[SelIdx].WaitingQueueNodeIdx[1];
    if (NwkIdx >= 1) and (NdIdx >= 0) and (WQAreaList.Data[SelIdx].CurrentWQIndex > 0) then
        begin
        node := theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx];
        node.GetIntWaitingQueue(WQAreaList.Data[SelIdx].CurrentWQIndex).PedestrianMargin := F_WaitAreaSet.sePedestrianMargin.Value;
        end;
    end;

procedure TFrameWaitingQueueSidePanel.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    var
        m      : TMethod;
        SelIdx : integer;
    begin
    if (p_IsActiveSelectMode) and (Key = VK_ESCAPE) then
        begin
        FinishEditMode;
        if p_tmpSelectedNodeNum[0] >= 0 then
            begin
            theApplicationServices.project.FlightWayNwk[p_tmpSelectedNodeNum[0]].Node[p_tmpSelectedNodeNum[1]].ReleaseSelecting;
            theApplicationServices.project.FlightWayNwk[p_tmpSelectedNodeNum[0]].Node[p_tmpSelectedNodeNum[1]].SelectedOnCrowdSim := false;
            end;

        p_tmpSelectedNodeNum[0] := -1;
        p_tmpSelectedNodeNum[1] := -1;
        F_WaitAreaSet.btnSetWaitingArea.Enabled := true;
        F_WaitAreaSet.Enabled := true;
        end
    else if (p_IsActiveExitSelectMode) and (Key = VK_ESCAPE) then
        begin
        SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
        if SelIdx < 0 then
            Exit;

        WQAreaList.Data[SelIdx].PedestrianMargin := F_WaitAreaSet.sePedestrianMargin.Value;
        UpdatelbWQ(WQAreaList.Data[SelIdx].WaitingQueueNodeIdx);
        theApplicationServices.ClearDisplayMessage(EDIT_MODE_MESSAGE_FINISH);
        theApplicationServices.ClearDisplayMessage(SELECT_EXIT_NODE_MESSAGE_ACTION);
        FormMainKeyUpDownProc(m) := OnKeyDown;
        theApplicationServices.UnRegisterEventHandler(_plgFormMainKeyDown, m);
        p_IsActiveExitSelectMode := false;
        theApplicationServices.mainForm.SetIsWaitingQueueEditing(false);
        F_WaitAreaSet.btnCreateWaitingQueue.Enabled := true;
        F_WaitAreaSet.Enabled := true;
        end;
    end;

procedure TFrameWaitingQueueSidePanel.OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
    if (Key <> VK_CONTROL) or (p_EditFinished) then
        Exit;

    if (not (p_IsActiveSelectMode)) or (IsNaN(p_FirstPoint[_x])) or (Assigned(p_Dialog)) then
        Exit;

    Initialize;
    theApplicationServices.mainForm.OpenGL.Camera.MoveMode := p_tmpMoveMode;
    end;

procedure TFrameWaitingQueueSidePanel.OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
    if (not (p_IsActiveSelectMode)) or (not theApplicationServices.mainForm.openGL.CtrlKeyDown) then
        Exit;

    p_FirstPoint := theApplicationServices.mainForm.OpenGL.Find3DCoordinatesUnderMouse;
    p_IsEditing := true;
    end;

procedure TFrameWaitingQueueSidePanel.OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
    if p_EditFinished then
        Exit;

    if (not (p_IsActiveSelectMode)) or (not theApplicationServices.mainForm.openGL.CtrlKeyDown) or (IsNaN(p_FirstPoint[_x])) then
        Exit;

    p_IsEditing := false;
    p_EditFinished := true;
    theApplicationServices.mainForm.OpenGL.Camera.MoveMode := p_tmpMoveMode;
    p_Dialog := TFormWQAreaEditingDialog.Create(nil);
    p_Dialog.ButtonOK.OnClick := OnClickDialogOK;
    p_Dialog.ButtonCancel.OnClick := OnClickDialogCancel;
    p_SecondPoint := theApplicationServices.mainForm.OpenGL.Find3DCoordinatesUnderMouse;
    if p_Dialog.ShowModal = mrOK then
        begin
        p_RenderDataName := F_WaitAreaSet.edtWaitingAreaName.Text;
        p_RenderData[0] := p_FirstPoint;
        p_RenderData[1] := p_SecondPoint;
        WQAreaList.AddNewData(p_RenderDataName, p_RenderData);
        SetNodeData;
        EnableTaskWindows(p_DisableTW);
        Initialize;
        F_WaitAreaSet.edtWaitingAreaName.Text := '';
        F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex := -1;
        if p_tmpSelectedNodeNum[0] >= 0 then
            begin
            theApplicationServices.project.FlightWayNwk[p_tmpSelectedNodeNum[0]].Node[p_tmpSelectedNodeNum[1]].ReleaseSelecting;
            theApplicationServices.project.FlightWayNwk[p_tmpSelectedNodeNum[0]].Node[p_tmpSelectedNodeNum[1]].SelectedOnCrowdSim := false;
            end;

        p_tmpSelectedNodeNum[0] := -1;
        p_tmpSelectedNodeNum[1] := -1;
        FinishEditMode;
        F_WaitAreaSet.Enabled := true;

        TWaitingQueueAreaRebuilder.Rebuild(WQAreaList);
        end
    else
        Initialize;

    FreeAndNil(p_Dialog);
    end;

procedure TFrameWaitingQueueSidePanel.OnClickDialogOK(Sender: TObject);
    begin
    p_Dialog.ModalResult := mrOK;
    end;

procedure TFrameWaitingQueueSidePanel.OnClickDialogCancel(Sender: TObject);
    begin
    p_Dialog.Close;
    Initialize;
    end;

procedure TFrameWaitingQueueSidePanel.btnSelectNodeClick(Sender: TObject);
    var
        m : TMethod;
    begin
    if F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex < 0 then
        Exit;

    p_IsSelectingMode := true;
    p_IsChanging := true;
    theApplicationServices.AddDisplayMessage(SELECT_TERMINAL_MESSAGE_ACTION);
    theApplicationServices.AddDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);

    theApplicationServices.mainForm.SetIsNodeSelectingOnCrowdSim(true);
    FormMainKeyUpDownProc(m) := OnKeyDownSelectingMode;
    theApplicationServices.RegisterEventHandler(_plgFormMainKeyDown, m);
    PluginNotifyProc(m) := AfterPaintOpenGLSelectingMode;
    theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
    F_WaitAreaSet.Enabled := false;
    end;

procedure TFrameWaitingQueueSidePanel.btnMovetoNodeClick(Sender: TObject);
    const
        EYE_HEIGHT = 50;
    var
        SelIdx, NwkIdx, NdIdx : integer;
        eye, view : GLPointType;
    begin
    SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    if SelIdx < 0 then
        Exit;

    NwkIdx   := WQAreaList.Data[SelIdx].LinkedNodeIdx[0];
    NdIdx    := WQAreaList.Data[SelIdx].LinkedNodeIdx[1];
    if (NwkIdx < 0) or (NdIdx < 0) then
        begin
        ShowMessage('バス停を選択していません');
        Exit;
        end;

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

procedure TFrameWaitingQueueSidePanel.Initialize;
    begin
    p_FirstPoint   := NAN_POINT;
    p_SecondPoint  := NAN_POINT;
    p_EditFinished := false;
    end;

procedure TFrameWaitingQueueSidePanel.SetWQAreaList(const aValue: TWaitingQueueAreaList);
    begin
    p_WQAreaList := aValue;
    end;

function TFrameWaitingQueueSidePanel.GetWQAreaList: TWaitingQueueAreaList;
    begin
    Result := p_WQAreaList;
    end;

procedure TFrameWaitingQueueSidePanel.TimerWaitMainProcTimer(Sender: TObject);
    begin
    theApplicationServices.mainForm.SetIsNodeSelectingOnCrowdSim(false);
    p_TimerWaitMainProc.OnTimer := nil;
    FreeAndNil(p_TimerWaitMainProc);
    end;

procedure TFrameWaitingQueueSidePanel.TimerWaitResetJumpingStateTimer(Sender: TObject);
    var
        m       : TMethod;
        SelectI, GLHeight, GLWidth : integer;
        tmpmodelVO : boolean;
    begin
    FormMainKeyUpDownProc(m) := OnKeyDown;
    theApplicationServices.RegisterEventHandler(_plgFormMainKeyDown, m);
    p_IsActiveExitSelectMode := true;
    theApplicationServices.AddDisplayMessage(SELECT_EXIT_NODE_MESSAGE_ACTION);
    theApplicationServices.AddDisplayMessage(EDIT_MODE_MESSAGE_FINISH);
    theApplicationServices.mainForm.SetIsWaitingQueueEditing(true);
    GLHeight := theApplicationServices.mainform.opengl.Height;
    GLWidth := theApplicationServices.mainform.opengl.Width;
    tmpmodelVO := theApplicationServices.visualOptionsRoot.displayOption[_Models];
    theApplicationServices.visualOptionsRoot.displayOption[_Models] := false;
    SelectI := theApplicationServices.mainform.opengl.GetObjectAtPos(Round(GLWidth/2), Round(GLHeight/2));
    theApplicationServices.mainform.opengl.OnSelect([], SelectI);
    theApplicationServices.visualOptionsRoot.displayOption[_Models] := tmpmodelVO;
    F_WaitAreaSet.Enabled := false;
    FreeAndNil(p_TimerWaitResetJumpingState);
    end;

procedure TFrameWaitingQueueSidePanel.FinishSelectMode;
    var
        m : TMethod;
    begin
    FormMainKeyUpDownProc(m) := OnKeyDownSelectingMode;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainKeyDown, m);
    PluginNotifyProc(m) := AfterPaintOpenGLSelectingMode;
    theApplicationServices.UnRegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
    if p_tmpSelectedNodeNum[0] >= 0 then
        theApplicationServices.project.FlightWayNwk[p_tmpSelectedNodeNum[0]].Node[p_tmpSelectedNodeNum[1]].ReleaseSelecting;

    if p_IsSelectingMode then
        p_IsSelectingMode := false;
    end;

procedure TFrameWaitingQueueSidePanel.btnWQNodeChangeClick(Sender: TObject);
    var
        m : TMethod;
        SelIdx : integer;
    begin
    SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    if SelIdx < 0 then
        Exit;

    p_IsExitSelectingMode := true;
    p_IsChanging := true;
    theApplicationServices.AddDisplayMessage(SELECT_EXIT_NODE_MESSAGE_ACTION);
    theApplicationServices.AddDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);

    theApplicationServices.mainForm.SetIsNodeSelectingOnCrowdSim(true);
    FormMainKeyUpDownProc(m) := OnKeyDownSelectingMode;
    theApplicationServices.RegisterEventHandler(_plgFormMainKeyDown, m);
    PluginNotifyProc(m) := AfterPaintOpenGLSelectingMode;
    theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
    F_WaitAreaSet.Enabled := false;
    end;

procedure TFrameWaitingQueueSidePanel.btnMovetoWQNodeClick(Sender: TObject);
    const
        EYE_HEIGHT = 50;
        SPIN_ADJUSTMENT = 180;
    var
        SelIdx, NwkIdx, NdIdx : integer;
        eye, view : GLPointType;
    begin
    SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    if SelIdx < 0 then
        Exit;

    NwkIdx   := WQAreaList.Data[SelIdx].WaitingQueueNodeIdx[0];
    NdIdx    := WQAreaList.Data[SelIdx].WaitingQueueNodeIdx[1];
    if (NwkIdx < 0) or (NdIdx < 0) then
        Exit;

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

procedure TFrameWaitingQueueSidePanel.btnSelectBusTerminalClick(Sender: TObject);
    var
        m : TMethod;
        SelIdx : integer;
    begin
    SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    if SelIdx < 0 then
        Exit;

    p_IsSelectingMode := true;
    p_IsChanging := true;
    theApplicationServices.AddDisplayMessage(SELECT_TERMINAL_MESSAGE_ACTION);
    theApplicationServices.AddDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);

    theApplicationServices.mainForm.SetIsNodeSelectingOnCrowdSim(true);
    FormMainKeyUpDownProc(m) := OnKeyDownSelectingMode;
    theApplicationServices.RegisterEventHandler(_plgFormMainKeyDown, m);
    PluginNotifyProc(m) := AfterPaintOpenGLSelectingMode;
    theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLAfterPaint, m);
    F_WaitAreaSet.Enabled := false;
    end;

procedure TFrameWaitingQueueSidePanel.btnMovetoBusTerminalClick(Sender: TObject);
    const
        EYE_HEIGHT = 50;
    var
        SelIdx, NwkIdx, NdIdx : integer;
        eye, view : GLPointType;
    begin
    SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    if SelIdx < 0 then
        Exit;

    NwkIdx   := WQAreaList.Data[SelIdx].LinkedNodeIdx[0];
    NdIdx    := WQAreaList.Data[SelIdx].LinkedNodeIdx[1];
    if (NwkIdx < 0) or (NdIdx < 0) then
        begin
        ShowMessage('バス停を選択していません');
        Exit;
        end;

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

procedure TFrameWaitingQueueSidePanel.btnBusTerminalLinkFreeClick(Sender: TObject);
    var
        SelIdx : integer;
        freeIdx : TNodeNumArray;
    begin
    SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    if SelIdx < 0 then
        Exit;

    F_WaitAreaSet.edtBusTerminal.Text := '';
    freeIdx[0] := -1;
    freeIdx[1] := -1;
    WQAreaList.Data[SelIdx].LinkedNodeIdx := freeIdx;
    end;

procedure TFrameWaitingQueueSidePanel.btnLinkWQandAreaClick(Sender: TObject);
    var
        SelIdx, wqIdx, i : integer;

    function SendDatatoWQ(const aArea: TWaitingQueueArea; wqId: integer): boolean;
        var
            LNdIdx      : TNodeNumArray;
            node        : IF8NetworkNode;
        begin
        Result := false;
        node := theApplicationServices.project.FlightWayNwk[aArea.WaitingQueueNodeIdx[0]].Node[aArea.WaitingQueueNodeIdx[1]];
        if aArea.IsUniqueExitRule then
            begin
            LNdIdx := aArea.LinkedNodeIdx;
            node.GetIntWaitingQueue(wqId+1).PedestrianMargin := aArea.PedestrianMargin;
            node.GetIntWaitingQueue(wqId+1).IsLinkedSchedule := false;
            node.GetIntWaitingQueue(wqId+1).ConstantReleaseProcessTimelimit := aArea.ExitInterval;
            node.GetIntWaitingQueue(wqId+1).ConstantReleaseAmountAtOnce     := aArea.ExitPedNum;
            if not ((LNdIdx[0] <= 0) or (LNdIdx[1] < 0)) then
                node.GetIntWaitingQueue(wqId+1).LinkedNode   := theApplicationServices.project.FlightWayNwk[LNdIdx[0]].node[LNdIdx[1]];

            WQAreaList.Data[SelIdx].CurrentWQIndex := wqId+1;
            end
        else
            begin
            LNdIdx := aArea.LinkedNodeIdx;
            if (LNdIdx[0] < 0) or (LNdIdx[1] < 0) then
                begin
                ShowMessage('バス停が選択されていません');
                Exit;
                end;

            node.GetIntWaitingQueue(wqId+1).PedestrianMargin := aArea.PedestrianMargin;
            node.GetIntWaitingQueue(wqId+1).IsLinkedSchedule := true;
            node.GetIntWaitingQueue(wqId+1).LinkedNode       := theApplicationServices.project.FlightWayNwk[LNdIdx[0]].node[LNdIdx[1]];
            WQAreaList.Data[SelIdx].CurrentWQIndex := wqId+1;
            end;

        Result := true;
        end;

    begin
    SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    wqIdx  := F_WaitAreaSet.lbWQ.ItemIndex;
    if (SelIdx < 0) or (wqIdx < 0) then
        Exit;

    if not SendDatatoWQ(WQAreaList.Data[SelIdx], wqIdx) then
        Exit;

    for i := 0 to F_WaitAreaSet.lbLinkedWQ.Items.Count - 1 do
        begin
        if F_WaitAreaSet.lbWQ.Items[wqIdx] =  F_WaitAreaSet.lbLinkedWQ.Items[i] then
            Exit; //値の更新に留める
        end;

    F_WaitAreaSet.lbLinkedWQ.Items.Add(F_WaitAreaSet.lbWQ.Items[wqIdx]);
    WQAreaList.Data[SelIdx].LinkedWQList.Add(F_WaitAreaSet.lbWQ.Items[wqIdx]);
    end;

procedure TFrameWaitingQueueSidePanel.btnLinkFreeClick(Sender: TObject);
    var
        SelIdx, LwqIdx : integer;

    procedure InitializeWQ(const arId: integer);
        var
            node : IF8NetworkNode;
            i : integer;
        begin
        node := theApplicationServices.project.FlightWayNwk[WQAreaList.Data[arId].WaitingQueueNodeIdx[0]].node[WQAreaList.Data[arId].WaitingQueueNodeIdx[1]];
        for i := 1 to node.WaitingQueueCount do
            begin
            if F_WaitAreaSet.lbLinkedWQ.Items[F_WaitAreaSet.lbLinkedWQ.ItemIndex] = node.GetIntWaitingQueue(i).name then
                begin
                node.GetIntWaitingQueue(i).PedestrianMargin := 0;
                node.GetIntWaitingQueue(i).IsLinkedSchedule := false;
                node.GetIntWaitingQueue(i).ConstantReleaseProcessTimelimit := 0;
                node.GetIntWaitingQueue(i).ConstantReleaseAmountAtOnce     := 0;
                node.GetIntWaitingQueue(i).LinkedNode       := nil;
                WQAreaList.Data[SelIdx].CurrentWQIndex := -1;
                Break;
                end;
            end;
        end;

    begin
    SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    LwqIdx  := F_WaitAreaSet.lbLinkedWQ.ItemIndex;
    if (SelIdx < 0) or (LwqIdx < 0) then
        Exit;

    InitializeWQ(SelIdx);
    F_WaitAreaSet.lbLinkedWQ.Items.Delete(LwqIdx);
    WQAreaList.Data[SelIdx].LinkedWQList.Delete(LwqIdx);
    end;

procedure TFrameWaitingQueueSidePanel.OnKeyDownSelectingMode(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
    if (p_IsSelectingMode) and (Key = VK_ESCAPE) then
        begin
        theApplicationServices.ClearDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
        theApplicationServices.ClearDisplayMessage(SELECT_TERMINAL_MESSAGE_ACTION);

        FinishSelectMode;
        if not Assigned(p_TimerWaitMainProc) then
            begin
            p_TimerWaitMainProc := TTimer.Create(nil);
            p_TimerWaitMainProc.Interval := 200;
            p_TimerWaitMainProc.OnTimer  := TimerWaitMainProcTimer;
            end;

        F_WaitAreaSet.Enabled := true;
        end;
    end;

procedure TFrameWaitingQueueSidePanel.UpdatelbWQ(const nodeIdx: TNodeNumArray);
    var
        i, j, SameNum : integer;
        node : IF8NetworkNode;
    begin
    if (nodeIdx[0] = -1) or (nodeIdx[1] = -1) then
        Exit;

    if F_WaitAreaSet.lbWQ.Items.Count > 0 then
        F_WaitAreaSet.lbWQ.Items.Clear;

    node := theApplicationServices.project.FlightWayNwk[nodeIdx[0]].Node[nodeIdx[1]];
    for i := 1 to node.waitingQueueCount do
        begin
        SameNum := 0;
        for j := 0 to F_WaitAreaSet.lbWQ.Items.Count - 1 do
            begin
            if node.GetIntWaitingQueue(i).name = F_WaitAreaSet.lbWQ.Items[j] then
                SameNum := SameNum + 1;
            end;

        if SameNum > 0 then
            node.GetIntWaitingQueue(i).name := node.GetIntWaitingQueue(i).name + '_' + (SameNum + 1).ToString;

        F_WaitAreaSet.lbWQ.Items.Add(node.GetIntWaitingQueue(i).name);
        end;
    end;

procedure TFrameWaitingQueueSidePanel.UpdatelbLinkedWQ(const nodeIdx: TNodeNumArray);
    var
        i, j, SelIdx : integer;
        node : IF8NetworkNode;
    begin
    SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;
    if SelIdx < 0 then
        Exit;

    if F_WaitAreaSet.lbLinkedWQ.Items.Count > 0 then
        F_WaitAreaSet.lbLinkedWQ.Items.Clear;

    node := theApplicationServices.project.FlightWayNwk[nodeIdx[0]].Node[nodeIdx[1]];
    for i := 1 to node.waitingQueueCount do
        begin
        for j := 0 to WQAreaList.Data[SelIdx].LinkedWQList.Count - 1 do
            begin
            if node.GetIntWaitingQueue(i).name = WQAreaList.Data[SelIdx].LinkedWQList[j] then
                begin
                F_WaitAreaSet.lbLinkedWQ.Items.Add(node.GetIntWaitingQueue(i).name);
                Break;
                end;
            end;
        end;
    end;

procedure TFrameWaitingQueueSidePanel.SetNodeData;
    var
        SelIdx, NwkIdx, NdIdx : integer;
        node : IF8NetworkNode;
    begin
    SelIdx := WQAreaList.Data.Count - 1;
    if p_IsChanging then
        SelIdx := F_WaitAreaSet.cbbSelectWaitingArea.ItemIndex;

    if not p_IsExitSelectingMode then
        begin
        WQAreaList.Data[SelIdx].LinkedNodeIdx := p_tmpSelectedNodeNum;
        NwkIdx := WQAreaList.Data[SelIdx].WaitingQueueNodeIdx[0];
        NdIdx  := WQAreaList.Data[SelIdx].WaitingQueueNodeIdx[1];
        if (NwkIdx >= 1) and (NdIdx >= 0) and (WQAreaList.Data[SelIdx].CurrentWQIndex > 0) then
            begin
            node := theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx];
            node.GetIntWaitingQueue(WQAreaList.Data[SelIdx].CurrentWQIndex).LinkedNode := theApplicationServices.project.FlightWayNwk[WQAreaList.Data[SelIdx].LinkedNodeIdx[0]].Node[WQAreaList.Data[SelIdx].LinkedNodeIdx[1]];
            end;
        end
    else
        begin
        WQAreaList.Data[SelIdx].WaitingQueueNodeIdx := p_tmpSelectedNodeNum;
        UpdatelbWQ(p_tmpSelectedNodeNum);
        p_IsExitSelectingMode := false;
        end;

    if p_IsChanging then
        begin
        if p_tmpSelectedNodeNum[0] >= 0 then
            begin
            theApplicationServices.project.FlightWayNwk[p_tmpSelectedNodeNum[0]].Node[p_tmpSelectedNodeNum[1]].ReleaseSelecting;
            theApplicationServices.project.FlightWayNwk[p_tmpSelectedNodeNum[0]].Node[p_tmpSelectedNodeNum[1]].SelectedOnCrowdSim := false;
            end;

        p_tmpSelectedNodeNum[0] := -1;
        p_tmpSelectedNodeNum[1] := -1;
        if (WQAreaList.Data[SelIdx].LinkedNodeIdx[0] >= 0) and (WQAreaList.Data[SelIdx].LinkedNodeIdx[1] >= 0) then
            F_WaitAreaSet.edtBusTerminal.Text := theApplicationServices.project.FlightWayNwk[WQAreaList.Data[SelIdx].LinkedNodeIdx[0]].Node[WQAreaList.Data[SelIdx].LinkedNodeIdx[1]].Name;
        end;

    if not Assigned(p_TimerWaitMainProc) then
        begin
        p_TimerWaitMainProc := TTimer.Create(nil);
        p_TimerWaitMainProc.Interval := 200;
        p_TimerWaitMainProc.OnTimer  := TimerWaitMainProcTimer;
        end;

    F_WaitAreaSet.Enabled := true;
    end;

procedure TFrameWaitingQueueSidePanel.AfterPaintOpenGLSelectingMode;

    procedure SetSelectNode;
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
                        p_tmpSelectedNodeNum[0] := i;
                        p_tmpSelectedNodeNum[1] := j;
                        tmpNwk.Node[j].SelectedOnCrowdSim := true;
                        if p_IsExitSelectingMode then
                            begin
                            theApplicationServices.ClearDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
                            theApplicationServices.ClearDisplayMessage(SELECT_EXIT_NODE_MESSAGE_ACTION);
                            FinishSelectMode;
                            if p_IsChanging = true then
                                begin
                                SetNodeData;
                                p_IsChanging := false;
                                Exit;
                                end;

                            RegisterKeyEvents;
                            theApplicationServices.AddDisplayMessage(SELECT_MODE_MESSAGE_ACTION);
                            theApplicationServices.AddDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
                            p_tmpMoveMode := theApplicationServices.mainForm.OpenGL.Camera.MoveMode;
                            theApplicationServices.mainForm.GetForm.ActiveControl := theApplicationServices.mainForm.OpenGL;
                            Exit;
                            end;

                        theApplicationServices.ClearDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
                        theApplicationServices.ClearDisplayMessage(SELECT_TERMINAL_MESSAGE_ACTION);
                        FinishSelectMode;
                        if p_IsChanging = true then
                            begin
                            SetNodeData;
                            p_IsChanging := false;
                            Exit;
                            end;

                        RegisterKeyEvents;

                        theApplicationServices.AddDisplayMessage(SELECT_MODE_MESSAGE_ACTION);
                        theApplicationServices.AddDisplayMessage(SELECT_MODE_MESSAGE_CANCEL);
                        p_tmpMoveMode := theApplicationServices.mainForm.OpenGL.Camera.MoveMode;
                        theApplicationServices.mainForm.GetForm.ActiveControl := theApplicationServices.mainForm.OpenGL;
                        Break;
                        end;
                    end;
                end;
            end;
        end;

    begin
    if (not p_IsSelectingMode) and (not p_IsExitSelectingMode) then
        Exit;

    if p_tmpSelectedNodeNum[0] < 0 then
        SetSelectNode;
    end;

procedure TFrameWaitingQueueSidePanel.OnShow;
    var
        i, NwkIdx, NdIdx, WQIdx : integer;
        LNdIdx : TNodeNumArray;
        node : IF8NetworkNode;
    begin
    for i := 0 to p_WQAreaList.Data.Count - 1 do
        begin
        NwkIdx := p_WQAreaList.Data[i].WaitingQueueNodeIdx[0];
        NdIdx := p_WQAreaList.Data[i].WaitingQueueNodeIdx[1];
        WQIdx := p_WQAreaList.Data[i].CurrentWQIndex;
        LNdIdx := p_WQAreaList.Data[i].LinkedNodeIdx;
        if (NwkIdx > 0) and (NdIdx >= 0) and (WQIdx > 0) then
            begin
            node := theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx];
            node.GetIntWaitingQueue(WQIdx).PedestrianMargin := p_WQAreaList.Data[i].PedestrianMargin;
            node.GetIntWaitingQueue(WQIdx).IsLinkedSchedule := not p_WQAreaList.Data[i].IsUniqueExitRule;
            node.GetIntWaitingQueue(WQIdx).ConstantReleaseProcessTimelimit := p_WQAreaList.Data[i].ExitInterval;
            node.GetIntWaitingQueue(WQIdx).ConstantReleaseAmountAtOnce     := p_WQAreaList.Data[i].ExitPedNum;
            if (LNdIdx[0] > 0) and (LNdIdx[1] >= 0) then
                node.GetIntWaitingQueue(WQIdx).LinkedNode := theApplicationServices.project.FlightWayNwk[LNdIdx[0]].node[LNdIdx[1]];
            end;
        end;
    end;
end.
