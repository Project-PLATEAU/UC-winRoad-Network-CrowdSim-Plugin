unit F_CrossWalkSettingsSidePanel;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.Math,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.Generics.Collections,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    PluginCore,
    F8GLUtils,
    F_CrossWalkSettings,
    CrowdSimSidePanelBaseFrame,
    PedestrianMapList,
    PedestrianMapUser,
    PedestrianUtil,
    F8FloatSpinEdit,
    CrosswalkEditingDialog,
    SimulationInputUtils,
    F_NameChanger;

type
    TFrameCrossWalkSettingsSidePanel = class(TFrame)
        FrameCrossWalkSettings1: TFrameCrossWalkSettings;
        procedure rbCrosswalkEditTypeClick(Sender: TObject);
        procedure btnSetCrosswalkClick(Sender: TObject);
        procedure TabSheetEditCrosswalkShow(Sender: TObject);
        procedure btnlbCrosswalksUpdateClick(Sender: TObject);
        procedure lbCrosswalksClick(Sender: TObject);
        procedure OnClickDialogOK(Sender: TObject);
        procedure OnClickDialogCancel(Sender: TObject);
        procedure seValuesOnChange(Sender: TObject);
        procedure rbSignalClick(Sender: TObject);
        procedure btnChangeCWNameClick(Sender: TObject);
        procedure btnChangeCWNameOKClick(Sender: TObject);
        procedure btnChangeCWNameCancelClick(Sender: TObject);
        procedure btnDeleteCWClick(Sender: TObject);
        private
            p_MapUser: TPedestrianMapUser;
            p_seCrosswalkWidth: TF8FloatSpinEdit;
            p_IsActiveSelectMode: boolean;
            p_FirstPoint: GLPointType;
            p_SecondPoint: GLPointType;
            p_ChangePathList: TList<TPathNumArray>;
            p_Dialog : TFormCrosswalkEditingDialog;
            p_DisableTW : Pointer;
            p_tmpMoveMode : MoveModeType;
            p_EditFinished : boolean;
            p_FormNameChanger : TFormNameChange;

            procedure MoveCameraToSelectedCrosswalk(const crosswalk: IF8NetworkLink);
            procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
            procedure OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
            procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
            procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
            procedure Initialize;
            procedure SelectedRelease;
            procedure UpdateChangePathList;
            procedure SetMapUser(const aValue: TPedestrianMapUser);
            function  GetMapUser: TPedestrianMapUser;
            function  IsInEditingArea(Pathpos: GLPointType): boolean;

            procedure RegisterKeyEvents;
            procedure UnRegisterKeyEvents;

        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            property  MapUser: TPedestrianMapUser read GetMapUser write SetMapUser;
        end;

implementation

const
    PAGE_EDIT_CROSSWALK = 0;

{$R *.dfm}

procedure TFrameCrossWalkSettingsSidePanel.AfterConstruction;

    procedure CreateCrosswalkWidthEditor;
        begin
        p_seCrosswalkWidth := TF8FloatSpinEdit.Create(Self);
        p_seCrosswalkWidth.Name      := 'seCrosswalkWidth';
        p_seCrosswalkWidth.Parent    := FrameCrossWalkSettings1.PanelCrosswalkWidth;
        p_seCrosswalkWidth.Align     := alLeft;
        p_seCrosswalkWidth.Width     := 75;
        p_seCrosswalkWidth.MaxValue  := 10000.0;
        p_seCrosswalkWidth.MinValue  := 0.25;
        p_seCrosswalkWidth.Digits    := 2;
        p_seCrosswalkWidth.Increment := 0.1;
        p_seCrosswalkWidth.Tail      := ' m';
        p_seCrosswalkWidth.Value     := 2.0;
        end;

    begin
    inherited;

    FrameCrossWalkSettings1.PageControlEditMesh.ActivePageIndex := PAGE_EDIT_CROSSWALK;
    CreateCrosswalkWidthEditor;
    p_ChangePathList := TList<TPathNumArray>.Create;
    p_FormNameChanger := TFormNameChange.Create(nil);
    p_EditFinished := false;
    end;

procedure TFrameCrossWalkSettingsSidePanel.BeforeDestruction;
    begin
    UnRegisterKeyEvents;
    FreeAndNil(p_FormNameChanger);
    FreeAndNil(p_ChangePathList);
    inherited;
    end;

procedure TFrameCrossWalkSettingsSidePanel.RegisterKeyEvents;
    var
        m : TMethod;
    begin
    FrameCrossWalkSettings1.btnSetCrosswalk.Enabled := false;
    FormMainOpenGLMouseUpDownProc(m) := OnMouseDown;
    theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLMouseDown, m);
    FormMainOpenGLMouseUpDownProc(m) := OnMouseUp;
    theApplicationServices.RegisterEventHandler(_plgFormMainOpenGLMouseUp, m);
    FormMainKeyUpDownProc(m) := OnKeyDown;
    theApplicationServices.RegisterEventHandler(_plgFormMainKeyDown, m);
    FormMainKeyUpDownProc(m) := OnKeyUp;
    theApplicationServices.RegisterEventHandler(_plgFormMainKeyUp, m);

    p_IsActiveSelectMode := true;
    end;

procedure TFrameCrossWalkSettingsSidePanel.UnRegisterKeyEvents;
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
    p_MapUser.CrosswalkEditingAreaRenderer.IsActive := false;
    FrameCrossWalkSettings1.btnSetCrosswalk.Enabled := true;
    end;

procedure TFrameCrossWalkSettingsSidePanel.lbCrosswalksClick(Sender: TObject);
    var
        tmpNwk    : IF8Networks;
        i, j, idx : integer;
    begin
    if (FrameCrossWalkSettings1.lbCrosswalks.ItemIndex < 0) or (not (theApplicationServices.project.NumberOfFlightWayNwks > 0)) then
        Exit;

    idx := 0;
    for i := 1 to theApplicationServices.project.NumberOfFlightWayNwks do
        begin
        tmpNwk := theApplicationServices.project.FlightWayNwk[i];
        if tmpNwk.LinkCount <= 0 then
            Continue;

        for j := 0 to tmpNwk.LinkCount - 1 do
            begin
            if (tmpNwk.Link[j].LinkType = _nlCrosswalkBlue) or (tmpNwk.Link[j].LinkType = _nlCrosswalkRed) then
                idx := idx + 1;

            if (idx - 1) = FrameCrossWalkSettings1.lbCrosswalks.ItemIndex then
                begin
                MoveCameraToSelectedCrosswalk(tmpNwk.Link[j]);
                Break;
                end;
            end;
        end;

    end;

procedure TFrameCrossWalkSettingsSidePanel.MoveCameraToSelectedCrosswalk(const crosswalk: IF8NetworkLink);
    const
        EYE_HEIGHT = 50;
    var
        eye, view : GLPointType;
    begin
    if (not Assigned(crosswalk.startNode)) or (not Assigned(crosswalk.endNode)) then
        Exit;

    eye[_x] := crosswalk.position[_x];
    eye[_y] := crosswalk.position[_y] + EYE_HEIGHT;
    eye[_z] := crosswalk.position[_z];
    view[_x] := crosswalk.position[_x];
    view[_y] := crosswalk.position[_y];
    view[_z] := crosswalk.position[_z];
    theApplicationServices.MoveCameraTo(eye, view, 0);
    theApplicationServices.mainForm.openGL.DoMouseDown(mbLeft, [], 0, 0);
    theApplicationServices.mainForm.openGL.DoMouseMove([], 1, 1);
    theApplicationServices.mainForm.openGL.DoMouseUp(mbLeft, [], 1, 1);
    end;

procedure TFrameCrossWalkSettingsSidePanel.btnlbCrosswalksUpdateClick(Sender: TObject);
    var
        tmpNwk : IF8Networks;
        i, j   : integer;
    begin
    FrameCrossWalkSettings1.lbCrosswalks.Items.Clear;
    for i := 1 to theApplicationServices.project.NumberOfFlightWayNwks do
        begin
        tmpNwk := theApplicationServices.project.FlightWayNwk[i];
        if tmpNwk.LinkCount <= 0 then
            Continue;

        for j := 0 to tmpNwk.LinkCount - 1 do
            begin
            if (tmpNwk.Link[j].LinkType = _nlCrosswalkBlue) or (tmpNwk.Link[j].LinkType = _nlCrosswalkRed) then
                FrameCrossWalkSettings1.lbCrosswalks.Items.Add(tmpNwk.Link[j].Name);
            end;
        end;
    end;

procedure TFrameCrossWalkSettingsSidePanel.TabSheetEditCrosswalkShow(Sender: TObject);
    var
        tmpNwk : IF8Networks;
        i, j   : integer;
    begin
    if (not Assigned(FrameCrossWalkSettings1.cbbNetwork)) or (not (theApplicationServices.project.NumberOfFlightWayNwks > 0)) then
        Exit;

    FrameCrossWalkSettings1.cbbNetwork.Items.Clear;
    for i := 1 to theApplicationServices.project.NumberOfFlightWayNwks do
        FrameCrossWalkSettings1.cbbNetwork.Items.Add(theApplicationServices.project.FlightWayNwk[i].Name);

    if not Assigned(FrameCrossWalkSettings1.lbCrosswalks) then
        Exit;

    FrameCrossWalkSettings1.lbCrosswalks.Items.Clear;
    for i := 1 to theApplicationServices.project.NumberOfFlightWayNwks do
        begin
        tmpNwk := theApplicationServices.project.FlightWayNwk[i];
        if tmpNwk.LinkCount <= 0 then
            Continue;

        for j := 0 to tmpNwk.LinkCount - 1 do
            begin
            if (tmpNwk.Link[j].LinkType = _nlCrosswalkBlue) or (tmpNwk.Link[j].LinkType = _nlCrosswalkRed) then
                FrameCrossWalkSettings1.lbCrosswalks.Items.Add(tmpNwk.Link[j].Name);
            end;
        end;
    end;

procedure TFrameCrossWalkSettingsSidePanel.btnSetCrosswalkClick(Sender: TObject);
    begin
    if (not (theApplicationServices.project.NumberOfFlightWayNwks > 0)) then
        Exit;

    if FrameCrossWalkSettings1.rbAddCrosswalk.Checked then
        begin
        if FrameCrossWalkSettings1.cbbNetwork.ItemIndex < 0 then
            Exit;

        if FrameCrossWalkSettings1.rbBlueSignal.Checked then
            begin
            theApplicationServices.mainForm.OpenPathwayNewEditor(FrameCrossWalkSettings1.cbbNetwork.ItemIndex,
                                                                 _nlCrosswalkBlue,
                                                                 FrameCrossWalkSettings1.rseSignalInterval.iValue,
                                                                 FrameCrossWalkSettings1.seRedSignalInterval.iValue,
                                                                 p_seCrosswalkWidth.Value);
            end
        else if FrameCrossWalkSettings1.rbRedSignal.Checked then
            begin
            theApplicationServices.mainForm.OpenPathwayNewEditor(FrameCrossWalkSettings1.cbbNetwork.ItemIndex,
                                                                 _nlCrosswalkRed,
                                                                 FrameCrossWalkSettings1.rseSignalInterval.iValue,
                                                                 FrameCrossWalkSettings1.seRedSignalInterval.iValue,
                                                                 p_seCrosswalkWidth.Value);
            end
        else if FrameCrossWalkSettings1.rbFloor.Checked then
            begin
            theApplicationServices.mainForm.OpenPathwayNewEditor(FrameCrossWalkSettings1.cbbNetwork.ItemIndex,
                                                                 _nlFloor,
                                                                 FrameCrossWalkSettings1.rseSignalInterval.iValue,
                                                                 FrameCrossWalkSettings1.seRedSignalInterval.iValue,
                                                                 p_seCrosswalkWidth.Value);
            end;
        end
    else if FrameCrossWalkSettings1.rbToCrosswalk.Checked then
        begin
        RegisterKeyEvents;

        theApplicationServices.AddDisplayMessage(EDIT_MODE_MESSAGE_ACTION);
        theApplicationServices.AddDisplayMessage(EDIT_MODE_MESSAGE_FINISH);

        p_MapUser.CrosswalkEditingAreaRenderer.IsActive := true;
        p_tmpMoveMode := theApplicationServices.mainForm.OpenGL.Camera.MoveMode;
        theApplicationServices.mainForm.GetForm.ActiveControl := theApplicationServices.mainForm.OpenGL;
        end;
    end;

procedure TFrameCrossWalkSettingsSidePanel.btnDeleteCWClick(Sender: TObject);
    var
        tmpNwk    : IF8Networks;
        i, j, idx : integer;
    begin
    idx := 0;
    for i := 1 to theApplicationServices.project.NumberOfFlightWayNwks do
        begin
        tmpNwk := theApplicationServices.project.FlightWayNwk[i];
        if tmpNwk.LinkCount <= 0 then
            Continue;

        for j := 0 to tmpNwk.LinkCount - 1 do
            begin
            if (tmpNwk.Link[j].LinkType = _nlCrosswalkBlue) or (tmpNwk.Link[j].LinkType = _nlCrosswalkRed) then
                idx := idx + 1;

            if (idx - 1) = FrameCrossWalkSettings1.lbCrosswalks.ItemIndex then
                begin
                tmpNwk.RemoveLink(j);
                FrameCrossWalkSettings1.lbCrosswalks.Items.Delete(FrameCrossWalkSettings1.lbCrosswalks.ItemIndex);
                Break;
                end;
            end;
        end;
    end;

procedure TFrameCrossWalkSettingsSidePanel.btnChangeCWNameClick(Sender: TObject);
    begin
    if (FrameCrossWalkSettings1.lbCrosswalks.ItemIndex < 0) or (not (theApplicationServices.project.NumberOfFlightWayNwks > 0)) then
        Exit;

    if not Assigned(p_FormNameChanger) then
        Exit;

    p_FormNameChanger.Caption := 'â°ífï‡ìπñºÇÃïœçX';
    p_FormNameChanger.lblNewName.Caption := 'êVÇµÇ¢â°ífï‡ìπñºÅF';
    p_FormNameChanger.btnOK.OnClick := btnChangeCWNameOKClick;
    p_FormNameChanger.btnCancel.OnClick := btnChangeCWNameCancelClick;
    p_FormNameChanger.edtNewName.Text := '';
    p_FormNameChanger.Show;
    end;

procedure TFrameCrossWalkSettingsSidePanel.btnChangeCWNameOKClick(Sender: TObject);
    var
        tmpNwk    : IF8Networks;
        i, j, idx : integer;
    begin
    idx := 0;
    for i := 1 to theApplicationServices.project.NumberOfFlightWayNwks do
        begin
        tmpNwk := theApplicationServices.project.FlightWayNwk[i];
        if tmpNwk.LinkCount <= 0 then
            Continue;

        for j := 0 to tmpNwk.LinkCount - 1 do
            begin
            if (tmpNwk.Link[j].LinkType = _nlCrosswalkBlue) or (tmpNwk.Link[j].LinkType = _nlCrosswalkRed) then
                idx := idx + 1;

            if (idx - 1) = FrameCrossWalkSettings1.lbCrosswalks.ItemIndex then
                begin
                tmpNwk.Link[j].name := p_FormNameChanger.edtNewName.Text;
                FrameCrossWalkSettings1.lbCrosswalks.Items[FrameCrossWalkSettings1.lbCrosswalks.ItemIndex] := p_FormNameChanger.edtNewName.Text;
                Break;
                end;
            end;
        end;

    p_FormNameChanger.Close;
    end;

procedure TFrameCrossWalkSettingsSidePanel.btnChangeCWNameCancelClick(Sender: TObject);
    begin
    p_FormNameChanger.Close;
    end;

procedure TFrameCrossWalkSettingsSidePanel.rbSignalClick(Sender: TObject);
    begin
    theApplicationServices.mainForm.GetForm.ActiveControl := theApplicationServices.mainForm.OpenGL;
    end;

procedure TFrameCrossWalkSettingsSidePanel.seValuesOnChange(Sender: TObject);
    begin
    theApplicationServices.mainForm.GetForm.ActiveControl := theApplicationServices.mainForm.OpenGL;
    end;

procedure TFrameCrossWalkSettingsSidePanel.rbCrosswalkEditTypeClick(Sender: TObject);
    begin
    if FrameCrossWalkSettings1.rbAddCrosswalk.Checked then
        begin
        FrameCrossWalkSettings1.LabelNetwork.Visible := true;
        FrameCrossWalkSettings1.cbbNetwork.Visible := true;
        FrameCrossWalkSettings1.PanelNetwork.Height := 35;
        end
    else
        begin
        FrameCrossWalkSettings1.LabelNetwork.Visible := false;
        FrameCrossWalkSettings1.cbbNetwork.Visible := false;
        FrameCrossWalkSettings1.PanelNetwork.Height := 0;
        end;
    end;

function TFrameCrossWalkSettingsSidePanel.IsInEditingArea(Pathpos: GLPointType): boolean;
    begin
    Result := false;
    if (Pathpos[_x] >= p_FirstPoint[_x]) and (Pathpos[_z] <= p_FirstPoint[_z])
        and (Pathpos[_x] <= p_SecondPoint[_x]) and (Pathpos[_z] >= p_SecondPoint[_z]) then
        Result := true
    else if (Pathpos[_x] >= p_FirstPoint[_x]) and (Pathpos[_z] >= p_FirstPoint[_z])
        and (Pathpos[_x] <= p_SecondPoint[_x]) and (Pathpos[_z] <= p_SecondPoint[_z]) then
        Result := true
    else if (Pathpos[_x] <= p_FirstPoint[_x]) and (Pathpos[_z] <= p_FirstPoint[_z])
        and (Pathpos[_x] >= p_SecondPoint[_x]) and (Pathpos[_z] >= p_SecondPoint[_z]) then
        Result := true
    else if (Pathpos[_x] <= p_FirstPoint[_x]) and (Pathpos[_z] >= p_FirstPoint[_z])
        and (Pathpos[_x] >= p_SecondPoint[_x]) and (Pathpos[_z] <= p_SecondPoint[_z]) then
        Result := true;
    end;

procedure TFrameCrossWalkSettingsSidePanel.UpdateChangePathList;
    var
        i, j   : integer;
        tmpNwk : IF8Networks;
        tmpPathNumArray : TPathNumArray;
    begin
    if not Assigned(p_ChangePathList) then
        Exit;

    for i := 1 to theApplicationServices.project.NumberOfFlightWayNwks do
        begin
        tmpNwk := theApplicationServices.project.FlightWayNwk[i];
        if tmpNwk.LinkCount <= 0 then
            Continue;

        for j := 0 to tmpNwk.LinkCount - 1 do
            begin
            if IsInEditingArea(tmpNwk.Link[j].position) then
                begin
                tmpPathNumArray[0] := i;
                tmpPathNumArray[1] := j;
                tmpNwk.link[j].selected := true;
                p_ChangePathList.Add(tmpPathNumArray);
                end;
            end;
        end;
    end;

procedure TFrameCrossWalkSettingsSidePanel.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
    if (p_IsActiveSelectMode) and (Key = VK_ESCAPE) then
        begin
        theApplicationServices.ClearDisplayMessage(EDIT_MODE_MESSAGE_FINISH);
        theApplicationServices.ClearDisplayMessage(EDIT_MODE_MESSAGE_ACTION);

        UnRegisterKeyEvents;
        end;
    end;

procedure TFrameCrossWalkSettingsSidePanel.OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
    if (Key <> VK_CONTROL) or (p_EditFinished) then
        Exit;

    if (not (p_IsActiveSelectMode)) or (IsNaN(p_FirstPoint[_x])) then
        Exit;

    p_EditFinished := true;
    theApplicationServices.mainForm.OpenGL.Camera.MoveMode := p_tmpMoveMode;
    p_Dialog := TFormCrosswalkEditingDialog.Create(nil);
    p_Dialog.ButtonOK.OnClick := OnClickDialogOK;
    p_Dialog.ButtonCancel.OnClick := OnClickDialogCancel;
    p_DisableTW := DisableTaskWindows(p_Dialog.Handle);
    p_SecondPoint := theApplicationServices.mainForm.OpenGL.Find3DCoordinatesUnderMouse;
    UpdateChangePathList;
    p_Dialog.Show;
    end;

procedure TFrameCrossWalkSettingsSidePanel.OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
    if (not (p_IsActiveSelectMode)) or (not theApplicationServices.mainForm.openGL.CtrlKeyDown) then
        Exit;

    p_FirstPoint := theApplicationServices.mainForm.OpenGL.Find3DCoordinatesUnderMouse;
    p_MapUser.CrosswalkEditingAreaRenderer.SelectedPoint := p_FirstPoint;
    end;

procedure TFrameCrossWalkSettingsSidePanel.OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
    if p_EditFinished then
        Exit;

    if (not (p_IsActiveSelectMode)) or (not theApplicationServices.mainForm.openGL.CtrlKeyDown) or (IsNaN(p_FirstPoint[_x])) then
        Exit;

    p_EditFinished := true;
    theApplicationServices.mainForm.OpenGL.Camera.MoveMode := p_tmpMoveMode;
    p_Dialog := TFormCrosswalkEditingDialog.Create(nil);
    p_Dialog.ButtonOK.OnClick := OnClickDialogOK;
    p_Dialog.ButtonCancel.OnClick := OnClickDialogCancel;
    p_DisableTW := DisableTaskWindows(p_Dialog.Handle);
    p_SecondPoint := theApplicationServices.mainForm.OpenGL.Find3DCoordinatesUnderMouse;
    UpdateChangePathList;
    p_Dialog.Show;
    end;

procedure TFrameCrossWalkSettingsSidePanel.OnClickDialogOK(Sender: TObject);
    var
        i, NwkIdx, PathIdx : integer;
    begin
    if not Assigned(p_ChangePathList) then
        Exit;

    for i := 0 to p_ChangePathList.Count - 1 do
        begin
        NwkIdx  := p_ChangePathList[i][0];
        PathIdx := p_ChangePathList[i][1];
        if FrameCrossWalkSettings1.rbBlueSignal.Checked then
            theApplicationServices.project.FlightWayNwk[NwkIdx].link[PathIdx].linkType := _nlCrosswalkBlue
        else if FrameCrossWalkSettings1.rbRedSignal.Checked then
            theApplicationServices.project.FlightWayNwk[NwkIdx].link[PathIdx].linkType := _nlCrosswalkRed
        else if FrameCrossWalkSettings1.rbFloor.Checked then
            theApplicationServices.project.FlightWayNwk[NwkIdx].link[PathIdx].linkType := _nlFloor;

        theApplicationServices.project.FlightWayNwk[NwkIdx].link[PathIdx].SignalInterval := FrameCrossWalkSettings1.rseSignalInterval.iValue;
        theApplicationServices.project.FlightWayNwk[NwkIdx].link[PathIdx].RedSignalInterval := FrameCrossWalkSettings1.seRedSignalInterval.iValue;
        theApplicationServices.project.FlightWayNwk[NwkIdx].link[PathIdx].width := p_seCrosswalkWidth.Value;
        end;

    SelectedRelease;
    EnableTaskWindows(p_DisableTW);
    p_Dialog.Close;
    FreeAndNil(p_Dialog);
    Initialize;
    p_MapUser.CrosswalkEditingAreaRenderer.Initialize;
    p_ChangePathList.Clear;
    end;

procedure TFrameCrossWalkSettingsSidePanel.OnClickDialogCancel(Sender: TObject);
    begin
    SelectedRelease;
    EnableTaskWindows(p_DisableTW);
    p_Dialog.Close;
    FreeAndNil(p_Dialog);
    Initialize;
    p_MapUser.CrosswalkEditingAreaRenderer.Initialize;
    if Assigned(p_ChangePathList) then
        p_ChangePathList.Clear;
    end;

procedure TFrameCrossWalkSettingsSidePanel.Initialize;
    begin
    p_FirstPoint   := NAN_POINT;
    p_SecondPoint  := NAN_POINT;
    p_EditFinished := false;
    end;

procedure TFrameCrossWalkSettingsSidePanel.SelectedRelease;
    var
        i : integer;
    begin
    if not Assigned(p_ChangePathList) then
        Exit;

    for i := 0 to p_ChangePathList.Count - 1 do
        theApplicationServices.project.FlightWayNwk[p_ChangePathList[i][0]].link[p_ChangePathList[i][1]].selected := false;
    end;

procedure TFrameCrossWalkSettingsSidePanel.SetMapUser(const aValue: TPedestrianMapUser);
    begin
    p_MapUser := aValue;
    end;

function TFrameCrossWalkSettingsSidePanel.GetMapUser: TPedestrianMapUser;
    begin
    Result := p_MapUser;
    end;
end.
