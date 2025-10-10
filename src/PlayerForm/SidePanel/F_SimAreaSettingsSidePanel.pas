unit F_SimAreaSettingsSidePanel;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.SyncObjs,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    PluginCore,
    F8GLUtils,
    F8Utils,
    F8FloatSpinEdit,
    F_CreateSimulationArea,
    CrowdSimSidePanelBaseFrame,
    PedestrianMapList,
    PedestrianMapUser,
    PedestrianMap,
    CrowdSimBuildingList,
    WalkingRouteCandidatePath,
    PedestrianUtil;

type
    TFrameSimAreaSettingsSidePanel = class(TFrame)
        FrameCreateSimulationArea1: TFrameCreateSimulationArea;
        private
            p_API: IF8ApplicationServices;
            p_PedestrianMapList: TPedestrianMapList;
            p_PedestrianMapUser: TPedestrianMapUser;
            p_BuildingList: TCrowdSimBuildingList;

            function  GetApplication: IF8ApplicationServices;
            procedure SetApplication(const aValue: IF8ApplicationServices);
            function  GetPedestrianMapList: TPedestrianMapList;
            procedure SetPedestrianMapList(const aValue: TPedestrianMapList);
            function  GetPedestrianMapUser: TPedestrianMapUser;
            procedure SetPedestrianMapUser(const aValue: TPedestrianMapUser);
            function  GetBuildingList: TCrowdSimBuildingList;
            procedure SetBuildingList(const aValue: TCrowdSimBuildingList);
            procedure SetMeshVisible(const aValue: Boolean);
            function  GetMeshVisible: Boolean;
            procedure SetCreateMeshMode;
            procedure ChangeToEditMeshMode;
            procedure OnSyncPedestrianMapGenerateEvent(const aProgressRate: Single);
            procedure OnTerminatePedestrianMapGenerateEvent(const aMap: TPedestrianMap);
            procedure OnChangedUserPedestrianMapEvent;

            procedure ButtonCreateMapClick(Sender: TObject);
            procedure ButtonStartSelectFirstPointClick(Sender: TObject);
            procedure ButtonStartSelectSecondPointClick(Sender: TObject);
            procedure ButtonBirdEyeClick(Sender: TObject);
            procedure CheckBoxStreetMapClick(Sender: TObject);
            procedure TimerUIEnableTimer(Sender: TObject);
            procedure SpinEditMapHeightOffsetChange(Sender: TObject);
            procedure ButtonCreateNewNetworkClick(Sender: TObject);
            procedure RadioButtonWalkableClick(Sender: TObject);
            procedure RadioButtonUnwalkableClick(Sender: TObject);
            procedure TrackBarStatusEditSizeChange(Sender: TObject);
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure OnFormShow;
            procedure OnFormClose;

            property  MapList: TPedestrianMapList          read GetPedestrianMapList write SetPedestrianMapList;
            property  MapUser: TPedestrianMapUser          read GetPedestrianMapUser write SetPedestrianMapUser;
            property  BuildingList: TCrowdSimBuildingList  read GetBuildingList      write SetBuildingList;
            property  Application : IF8ApplicationServices read GetApplication       write SetApplication;
            property  MeshVisible : Boolean                read GetMeshVisible       write SetMeshVisible;
        end;

implementation

uses
    Vcl.ComCtrls,
    WalkingRouteAutomaticGenerator;

{$R *.dfm}

procedure TFrameSimAreaSettingsSidePanel.AfterConstruction;
    begin
    inherited;

    p_PedestrianMapUser := nil;
    p_PedestrianMapList := nil;

    FrameCreateSimulationArea1.btnCreateSimArea.OnClick        := ButtonCreateMapClick;
    FrameCreateSimulationArea1.btnBirdEye.OnClick              := ButtonBirdEyeClick;
    FrameCreateSimulationArea1.chbStreetMap.OnClick            := CheckBoxStreetMapClick;
    FrameCreateSimulationArea1.ButtonMeshOrigin.OnClick        := ButtonStartSelectFirstPointClick;
    FrameCreateSimulationArea1.btnSetSimArea.OnClick           := ButtonStartSelectSecondPointClick;
    FrameCreateSimulationArea1.TimerUIEnable.OnTimer           := TimerUIEnableTimer;
    FrameCreateSimulationArea1.ButtonCreateNewNetwork.OnClick  := ButtonCreateNewNetworkClick;
    FrameCreateSimulationArea1.RadioButtonWalkable.OnClick     := RadioButtonWalkableClick;
    FrameCreateSimulationArea1.RadioButtonUnwalkable.OnClick   := RadioButtonUnwalkableClick;
    FrameCreateSimulationArea1.TrackBarStatusEditSize.OnChange := TrackBarStatusEditSizeChange;

    FrameCreateSimulationArea1.RegisterOnHeightOffsetChangeEvent(SpinEditMapHeightOffsetChange);
    end;

procedure TFrameSimAreaSettingsSidePanel.BeforeDestruction;
    begin
    inherited;

    FrameCreateSimulationArea1.UnRegisterOnHeightOffsetChangeEvent(SpinEditMapHeightOffsetChange);

    p_PedestrianMapList.UnRegisterOnSynchronizeEvent(OnSyncPedestrianMapGenerateEvent);
    p_PedestrianMapList.UnRegisterOnTerminateEvent(OnTerminatePedestrianMapGenerateEvent);
    p_PedestrianMapUser.UnRegisterOnChangeUserPedestrianMapEvent(OnChangedUserPedestrianMapEvent);
    p_PedestrianMapList := nil;
    p_PedestrianMapUser := nil;
    p_API := nil;
    end;

procedure TFrameSimAreaSettingsSidePanel.OnFormShow;
    begin
    Assert(Assigned(MapList) and Assigned(MapUser));

    if Assigned(MapUser) then
        begin
        if MapUser.Enabled then
            begin
            ChangeToEditMeshMode;
            FrameCreateSimulationArea1.UpdateHeightOffset(p_PedestrianMapUser.Map.HeightOffset);
            end
        else
            SetCreateMeshMode;

        if MapUser.CellEditor.WriteStatus = PedestrianAreaStatus._pasWalkable then
            FrameCreateSimulationArea1.RadioButtonWalkable.Checked := True
        else
            FrameCreateSimulationArea1.RadioButtonUnwalkable.Checked := True;
        end;
    end;

procedure TFrameSimAreaSettingsSidePanel.OnFormClose;
    begin
    ChangeToEditMeshMode;
    if Assigned(MapList) then
        MapList.IsCreateMeshMode := False;

    if Assigned(MapUser) then
        begin
        MapUser.CellEditor.Enabled := False;
        if MapUser.Enabled then
            MapUser.Visible := True;
        end;
    end;

procedure TFrameSimAreaSettingsSidePanel.ButtonCreateMapClick(Sender: TObject);
    begin
    if not Assigned(MapList) then
        Exit;

    if MapList.GenerateNewMap(FrameCreateSimulationArea1.NetworkDetail) then
        FrameCreateSimulationArea1.btnCreateSimArea.Enabled := False;
    end;

procedure TFrameSimAreaSettingsSidePanel.SetCreateMeshMode;
    begin
    if not (Assigned(p_PedestrianMapList) and Assigned(p_PedestrianMapUser)) then
        Exit;

    p_PedestrianMapList.IsCreateMeshMode := True;
    p_PedestrianMapUser.Visible := False;
    p_PedestrianMapUser.CellEditor.Enabled := False;
    FrameCreateSimulationArea1.MeshEditorDisabled;
    end;

procedure TFrameSimAreaSettingsSidePanel.ChangeToEditMeshMode;
    begin
    if not (Assigned(p_PedestrianMapList) and Assigned(p_PedestrianMapUser)) then
        Exit;

    p_PedestrianMapList.IsCreateMeshMode := False;
    p_PedestrianMapUser.Visible := True;
    p_PedestrianMapUser.CellEditor.Enabled := p_PedestrianMapUser.Enabled;
    FrameCreateSimulationArea1.MeshEditorEnabled;
    end;

procedure TFrameSimAreaSettingsSidePanel.OnChangedUserPedestrianMapEvent;
    begin
    ChangeToEditMeshMode;
    FrameCreateSimulationArea1.UpdateHeightOffset(p_PedestrianMapUser.Map.HeightOffset);
    end;

procedure TFrameSimAreaSettingsSidePanel.SpinEditMapHeightOffsetChange(Sender: TObject);
    var
        edit: TF8FloatSpinEdit;
    begin
    if Sender is TF8FloatSpinEdit then
        begin
        edit := (Sender as TF8FloatSpinEdit);
        p_PedestrianMapUser.Map.HeightOffset := edit.Value;
        end;
    end;

procedure TFrameSimAreaSettingsSidePanel.ButtonCreateNewNetworkClick(Sender: TObject);
    var
        gen: TWalkingRouteAutomaticGenerator;
        blgList: TCandidatePathListArray;
        mpList: TCandidatePathList;
        fixedList: TCandidatePathListArray;
        path: TParametricLineList;
        item: TCandidatePathList;
    begin
    Assert(Assigned(p_BuildingList));
    gen := TWalkingRouteAutomaticGenerator.Create;
    SetLength(path, 0);
    try
        blgList := p_BuildingList.GenerateCandidateWalkingPath;
        fixedList := TCandidatePathListArray.Create;
        if p_PedestrianMapUser.Enabled then
            mpList := TCandidatePathList.Create(p_PedestrianMapUser.Map.GenerateCandidateWalkingPath)
        else
            mpList := nil;

        try
            if Assigned(mpList) then
                begin
                for item in blgList do
                    fixedList.Add(item.MergeAndConnectList(mpList));

                gen.GenerateNetworkPath(fixedList);
                end
            else
                gen.GenerateNetworkPath(blgList);
        finally
            if Assigned(mpList) then
                FreeAndNil(mpList);
            FreeAndNil(blgList);
            FreeAndNil(fixedList);
            end;
    finally
        FreeAndNil(gen);
        end;
    end;

procedure TFrameSimAreaSettingsSidePanel.RadioButtonWalkableClick(Sender: TObject);
    begin
    if Assigned(MapUser) then
        MapUser.CellEditor.WriteStatus := PedestrianAreaStatus._pasWalkable;
    end;

procedure TFrameSimAreaSettingsSidePanel.RadioButtonUnwalkableClick(Sender: TObject);
    begin
    if Assigned(MapUser) then
        MapUser.CellEditor.WriteStatus := PedestrianAreaStatus._pasNoEntry;
    end;

procedure TFrameSimAreaSettingsSidePanel.TrackBarStatusEditSizeChange(Sender: TObject);
    var
        volume: Integer;
        trackbar: TTrackBar;
    begin
    if not (Sender is TTrackBar) then
        Exit;

    trackbar := Sender as TTrackBar;

    if Assigned(MapUser) then
        MapUser.CellEditor.StatusEditSize := trackbar.Position;

    volume := trackbar.Position * 2 + 1;
    if volume < 10 then
        FrameCreateSimulationArea1.StatusEditSizeCaption := Format('  %d ~   %d', [volume, volume])
    else if volume < 100 then
        FrameCreateSimulationArea1.StatusEditSizeCaption := Format(' %d ~  %d', [volume, volume])
    else
        FrameCreateSimulationArea1.StatusEditSizeCaption := Format('%d ~ %d', [volume, volume]);
    end;

procedure TFrameSimAreaSettingsSidePanel.OnSyncPedestrianMapGenerateEvent(const aProgressRate: Single);
    begin
    FrameCreateSimulationArea1.pbCreateSimArea.Position := Trunc(aProgressRate * 100);
    end;

procedure TFrameSimAreaSettingsSidePanel.OnTerminatePedestrianMapGenerateEvent(const aMap: TPedestrianMap);
    begin
    FrameCreateSimulationArea1.btnCreateSimArea.Enabled := True;
    FrameCreateSimulationArea1.MeshEditorEnabled;
    end;

function TFrameSimAreaSettingsSidePanel.GetApplication: IF8ApplicationServices;
    begin
    Result := p_API;
    end;

procedure TFrameSimAreaSettingsSidePanel.SetApplication(const aValue: IF8ApplicationServices);
    begin
    p_API := aValue;
    if Assigned(p_API) then
        FrameCreateSimulationArea1.chbStreetMap.Checked := p_API.VisualOptionsRoot.DisplayOption[_StreetMap];
    end;

function TFrameSimAreaSettingsSidePanel.GetPedestrianMapList: TPedestrianMapList;
    begin
    Result := p_PedestrianMapList;
    end;

procedure TFrameSimAreaSettingsSidePanel.ButtonStartSelectFirstPointClick(Sender: TObject);
    begin
    SetCreateMeshMode;
    if Assigned(p_PedestrianMapList) then
        p_PedestrianMapList.StartEditFirstPoint;
    end;

procedure TFrameSimAreaSettingsSidePanel.ButtonStartSelectSecondPointClick(Sender: TObject);
    begin
    SetCreateMeshMode;
    if Assigned(p_PedestrianMapList) then
        p_PedestrianMapList.StartEditSecondPoint;
    end;

procedure TFrameSimAreaSettingsSidePanel.ButtonBirdEyeClick(Sender: TObject);
    const
        CAMERA_PITCH_ANGLE = -70; // degree
        CAMERA_DISTANCE_FROM_CENTER = -2700;
    var
        north, east: Double;
        centerOfLocal: GLPointType;
    begin
    // ’nŒ`’†S‚©‚çˆê’è‚Ì‹——£AŠp“x‚ÌˆÊ’u‚ÉƒWƒƒƒ“ƒv
    // ‹——£AŠp“x‚Í‚Æ‚è‚ ‚¦‚¸Œˆ‚ß‘Å‚¿
    north := p_API.Project.GetTerrain.lengthNorthing;
    east  := p_API.Project.GetTerrain.lengthEasting;
    centerOfLocal := AsGLPointType(east * 0.75, 0.0, north * 0.5);
    p_API.mainForm.MoveCameraToXZ(centerOfLocal);
    p_API.mainForm.CameraMoveMode := _cmmLookAbout;
    p_API.mainForm.CameraResetPitchAngle(CAMERA_PITCH_ANGLE);
    p_API.mainForm.CameraResetYawAngle(0);
    p_API.mainForm.CameraMove(dInOut, CAMERA_DISTANCE_FROM_CENTER);
    end;

procedure TFrameSimAreaSettingsSidePanel.CheckBoxStreetMapClick(Sender: TObject);
    begin
    p_API.VisualOptionsRoot.DisplayOption[_StreetMap] := FrameCreateSimulationArea1.chbStreetMap.Checked;
    end;

procedure TFrameSimAreaSettingsSidePanel.SetPedestrianMapList(const aValue: TPedestrianMapList);
    begin
    if Assigned(p_PedestrianMapList) then
        begin
        p_PedestrianMapList.UnRegisterOnSynchronizeEvent(OnSyncPedestrianMapGenerateEvent);
        p_PedestrianMapList.UnRegisterOnTerminateEvent(OnTerminatePedestrianMapGenerateEvent);
        end;

    p_PedestrianMapList := aValue;
    if Assigned(p_PedestrianMapList) then
        begin
        p_PedestrianMapList.RegisterOnSynchronizeEvent(OnSyncPedestrianMapGenerateEvent);
        p_PedestrianMapList.RegisterOnTerminateEvent(OnTerminatePedestrianMapGenerateEvent);
        SetCreateMeshMode;
        end;
    end;

function TFrameSimAreaSettingsSidePanel.GetPedestrianMapUser: TPedestrianMapUser;
    begin
    Result := p_PedestrianMapUser;
    end;

procedure TFrameSimAreaSettingsSidePanel.SetPedestrianMapUser(const aValue: TPedestrianMapUser);
    begin
    if Assigned(p_PedestrianMapUser) then
        p_PedestrianMapUser.UnRegisterOnChangeUserPedestrianMapEvent(OnChangedUserPedestrianMapEvent);

    p_PedestrianMapUser := aValue;
    if Assigned(p_PedestrianMapUser) then
        begin
        p_PedestrianMapUser.RegisterOnChangeUserPedestrianMapEvent(OnChangedUserPedestrianMapEvent);
        if p_PedestrianMapUser.Enabled then
            ChangeToEditMeshMode;

        SetCreateMeshMode;
        end;
    end;

function TFrameSimAreaSettingsSidePanel.GetBuildingList: TCrowdSimBuildingList;
    begin
    Result := p_BuildingList;
    end;

procedure TFrameSimAreaSettingsSidePanel.SetBuildingList(const aValue: TCrowdSimBuildingList);
    begin
    if Assigned(p_BuildingList) then
        p_BuildingList := nil;

    p_BuildingList := aValue;
    end;

procedure TFrameSimAreaSettingsSidePanel.SetMeshVisible(const aValue: Boolean);
    begin
    FrameCreateSimulationArea1.MeshVisible := aValue;
    end;

function TFrameSimAreaSettingsSidePanel.GetMeshVisible: Boolean;
    begin
    Result := FrameCreateSimulationArea1.MeshVisible;
    end;

procedure TFrameSimAreaSettingsSidePanel.TimerUIEnableTimer(Sender: TObject);
    var
        createMeshButtonEnabled: Boolean;
    begin
    createMeshButtonEnabled := Assigned(p_PedestrianMapList) and (not (p_PedestrianMapList.IsEditFirstPoint or p_PedestrianMapList.IsEditSecondPoint));
    FrameCreateSimulationArea1.ButtonMeshOrigin.Enabled  := createMeshButtonEnabled;
    FrameCreateSimulationArea1.btnSetSimArea.Enabled := createMeshButtonEnabled;
    end;
end.
