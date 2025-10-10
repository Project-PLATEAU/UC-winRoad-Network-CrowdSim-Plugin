unit F_SimulationAreaSettings;

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
    Vcl.ComCtrls,
    F8FloatSpinEdit,
    PluginCore,
    F8GLUtils,
    F8Utils,
    PedestrianMapList,
    PedestrianMapUser,
    PedestrianMap,
    PedestrianUtil,
    F_SimulationAreaEditMesh;

type
    /// <summary>
    ///    シミュレーションエリア生成に関する設定/操作を行うフレームとその機能を定義するクラス
    /// </summary>
    TFrameSimulationAreaSettings = class(TFrame)
        PanelCell: TPanel;
        LabelCellSize: TLabel;
        ButtonCreateMap: TButton;
        ProgressBarMakeNewMesh: TProgressBar;
        RadioButtonCreateNewMesh: TRadioButton;
        RadioButtonEditMesh: TRadioButton;
        ButtonStartSelectFirstPoint: TButton;
        ButtonStartSelectSecondPoint: TButton;
        PanelMeshPoint: TPanel;
        PanelMenu: TPanel;
        PageControlMenu: TPageControl;
        TabSheetCreateMesh: TTabSheet;
        TabSheetEditMesh: TTabSheet;
        FrameSimulationAreaEditMesh: TFrameSimulationAreaEditMesh;
        GroupBoxViewOption: TGroupBox;
        ButtonBirdEye: TButton;
        CheckBoxMeshVisible: TCheckBox;
        CheckBoxStreetMap: TCheckBox;
        TimerUIEnable: TTimer;
        procedure ButtonCreateMapClick(Sender: TObject);
        procedure RadioButtonModeChangeClick(Sender: TObject);
        procedure ButtonStartSelectFirstPointClick(Sender: TObject);
        procedure ButtonStartSelectSecondPointClick(Sender: TObject);
        procedure ButtonBirdEyeClick(Sender: TObject);
        procedure CheckBoxMeshVisibleClick(Sender: TObject);
        procedure CheckBoxStreetMapClick(Sender: TObject);
        procedure TimerUIEnableTimer(Sender: TObject);
        private
            p_CellSizeEdit: TF8FloatSpinEdit;

            p_MeshVisible: Boolean;

            p_API: IF8ApplicationServices;
            p_PedestrianMapList: TPedestrianMapList;
            p_PedestrianMapUser: TPedestrianMapUser;

            function  GetApplication: IF8ApplicationServices;
            procedure SetApplication(const aValue: IF8ApplicationServices);
            function  GetPedestrianMapList: TPedestrianMapList;
            procedure SetPedestrianMapList(const aValue: TPedestrianMapList);
            function  GetPedestrianMapUser: TPedestrianMapUser;
            procedure SetPedestrianMapUser(const aValue: TPedestrianMapUser);
            procedure UpdateMenu;
            procedure UpdateMode;
            procedure ChangeToCreateMeshMode;
            procedure ChangeToEditMeshMode;
            procedure OnSyncPedestrianMapGenerateEvent(const aProgressRate: Single);
            procedure OnTerminatePedestrianMapGenerateEvent(const aMap: TPedestrianMap);
            procedure OnChangedUserPedestrianMapEvent;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure OnFormShow;
            procedure OnChangeTab(const aActivate: Boolean);
            procedure OnFormClose;

            property  PedestrianMapList: TPedestrianMapList     read GetPedestrianMapList write SetPedestrianMapList;
            property  PedestrianMapUser: TPedestrianMapUser     read GetPedestrianMapUser write SetPedestrianMapUser;
            property  Application      : IF8ApplicationServices read GetApplication       write SetApplication;
            property  MeshVisible      : Boolean                read p_MeshVisible;
        end;

implementation

uses
    System.Math,
    MovingFeature,
    LatLonHelper;

const
    PAGE_CREATE_MESH = 0;
    PAGE_EDIT_MESH   = 1;

{$R *.dfm}

{ TFrameSimulationAreaSettings }
procedure TFrameSimulationAreaSettings.AfterConstruction;
    procedure CreateAreaTabComponent;
        begin
        p_CellSizeEdit := TF8FloatSpinEdit.Create(Self);
        p_CellSizeEdit.Name := 'SpinEditCellSize';
        p_CellSizeEdit.Parent := PanelCell;
        p_CellSizeEdit.Align := alLeft;
        p_CellSizeEdit.Width := 100;
        p_CellSizeEdit.MaxValue := 10.0;
        p_CellSizeEdit.MinValue := 0.25;
        p_CellSizeEdit.Digits := 2;
        p_CellSizeEdit.Increment := 0.1;
        p_CellSizeEdit.Tail := ' m';
        p_CellSizeEdit.Value := 8;
        end;
    begin
    inherited;

    p_MeshVisible       := True;

    PageControlMenu.ActivePageIndex := PAGE_CREATE_MESH;
    FrameSimulationAreaEditMesh.MapUser := nil;
    p_PedestrianMapUser := nil;
    CheckBoxMeshVisible.Checked := not MeshVisible;

    CreateAreaTabComponent;
    ChangeToCreateMeshMode;
    UpdateMenu;
    end;

procedure TFrameSimulationAreaSettings.BeforeDestruction;
    begin
    inherited;

    FrameSimulationAreaEditMesh.MapUser := nil;
    p_PedestrianMapList.UnRegisterOnSynchronizeEvent(OnSyncPedestrianMapGenerateEvent);
    p_PedestrianMapList.UnRegisterOnTerminateEvent(OnTerminatePedestrianMapGenerateEvent);
    p_PedestrianMapList := nil;
    p_PedestrianMapUser := nil;
    p_API := nil;
    end;

procedure TFrameSimulationAreaSettings.OnFormShow;
    begin
    if RadioButtonCreateNewMesh.Checked then
        begin
        if Assigned(PedestrianMapList) then
            PedestrianMapList.IsCreateMeshMode := True;
        if Assigned(PedestrianMapUser) and PedestrianMapUser.Enabled then
            PedestrianMapUser.Visible := False;
        end;

    if RadioButtonEditMesh.Checked then
        begin
        if Assigned(PedestrianMapUser) then
            PedestrianMapUser.CellEditor.Enabled := True;
        end;
    end;

procedure TFrameSimulationAreaSettings.OnChangeTab(const aActivate: Boolean);
    begin
    if RadioButtonCreateNewMesh.Checked then
        begin
        if Assigned(PedestrianMapList) then
            PedestrianMapList.IsCreateMeshMode := aActivate;
        if Assigned(PedestrianMapUser) and PedestrianMapUser.Enabled then
            PedestrianMapUser.Visible := False;
        end;

    if RadioButtonEditMesh.Checked then
        begin
        if Assigned(PedestrianMapUser) then
            PedestrianMapUser.CellEditor.Enabled := aActivate;
        end;
    end;

procedure TFrameSimulationAreaSettings.OnFormClose;
    begin
    if Assigned(PedestrianMapList) then
        PedestrianMapList.IsCreateMeshMode := False;

    if Assigned(PedestrianMapUser) then
        begin
        PedestrianMapUser.CellEditor.Enabled := False;
        if PedestrianMapUser.Enabled then
            PedestrianMapUser.Visible := True;
        end;
    end;

procedure TFrameSimulationAreaSettings.ButtonCreateMapClick(Sender: TObject);
    begin
    if not Assigned(PedestrianMapList) then
        Exit;

    if PedestrianMapList.GenerateNewMap(p_CellSizeEdit.Value) then
        begin
        ButtonCreateMap.Enabled := False;
        RadioButtonEditMesh.Enabled := False;
        end;
    end;

procedure TFrameSimulationAreaSettings.UpdateMenu;
    begin
    RadioButtonEditMesh.Enabled := Assigned(p_PedestrianMapList) and Assigned(p_PedestrianMapUser) and PedestrianMapUser.Enabled;
    end;

procedure TFrameSimulationAreaSettings.UpdateMode;
    begin
    if not (Assigned(p_PedestrianMapList) and Assigned(p_PedestrianMapUser)) then
        Exit;

    if RadioButtonCreateNewMesh.Checked then
        begin
        PageControlMenu.ActivePageIndex := PAGE_CREATE_MESH;
        p_PedestrianMapList.IsCreateMeshMode := True;
        p_PedestrianMapUser.Visible := False;
        p_PedestrianMapUser.CellEditor.Enabled := False;
        end
    else if RadioButtonEditMesh.Checked then
        begin
        PageControlMenu.ActivePageIndex := PAGE_EDIT_MESH;
        p_PedestrianMapList.IsCreateMeshMode := False;
        p_PedestrianMapUser.Visible := True;
        p_PedestrianMapUser.CellEditor.Enabled := p_PedestrianMapUser.Enabled;
        end;
    end;

procedure TFrameSimulationAreaSettings.ChangeToCreateMeshMode;
    begin
    if Assigned(PedestrianMapList) then
        begin
        PedestrianMapList.IsCreateMeshMode := True;
        RadioButtonCreateNewMesh.Checked := True;
        end;
    end;

procedure TFrameSimulationAreaSettings.ChangeToEditMeshMode;
    begin
    if RadioButtonEditMesh.Enabled then
        RadioButtonEditMesh.Checked := True;
    end;

procedure TFrameSimulationAreaSettings.OnSyncPedestrianMapGenerateEvent(const aProgressRate: Single);
    begin
    ProgressBarMakeNewMesh.Position := Trunc(aProgressRate * 100);
    end;

procedure TFrameSimulationAreaSettings.OnTerminatePedestrianMapGenerateEvent(const aMap: TPedestrianMap);
    begin
    ButtonCreateMap.Enabled := True;
    UpdateMenu;
    end;

procedure TFrameSimulationAreaSettings.OnChangedUserPedestrianMapEvent;
    begin
    UpdateMenu;
    ChangeToEditMeshMode;
    end;

function TFrameSimulationAreaSettings.GetApplication: IF8ApplicationServices;
    begin
    Result := p_API;
    end;

procedure TFrameSimulationAreaSettings.SetApplication(const aValue: IF8ApplicationServices);
    begin
    p_API := aValue;
    if Assigned(p_API) then
        CheckBoxStreetMap.Checked   := p_API.VisualOptionsRoot.DisplayOption[_StreetMap];
    end;

function TFrameSimulationAreaSettings.GetPedestrianMapList: TPedestrianMapList;
    begin
    Result := p_PedestrianMapList;
    end;

procedure TFrameSimulationAreaSettings.RadioButtonModeChangeClick(Sender: TObject);
    begin
    UpdateMenu;
    UpdateMode;
    end;

procedure TFrameSimulationAreaSettings.ButtonStartSelectFirstPointClick(Sender: TObject);
    begin
    if Assigned(p_PedestrianMapList) then
        p_PedestrianMapList.StartEditFirstPoint;
    end;

procedure TFrameSimulationAreaSettings.ButtonStartSelectSecondPointClick(Sender: TObject);
    begin
    if Assigned(p_PedestrianMapList) then
        p_PedestrianMapList.StartEditSecondPoint;
    end;

procedure TFrameSimulationAreaSettings.ButtonBirdEyeClick(Sender: TObject);
    const
        CAMERA_PITCH_ANGLE = -70; // degree
        CAMERA_DISTANCE_FROM_CENTER = -2700;
    var
        north, east: Double;
        centerOfLocal: GLPointType;
    begin
    // 地形中心から一定の距離、角度の位置にジャンプ
    // 距離、角度はとりあえず決め打ち
    north := p_API.Project.GetTerrain.lengthNorthing;
    east  := p_API.Project.GetTerrain.lengthEasting;
    centerOfLocal := AsGLPointType(east * 0.75, 0.0, north * 0.5);
    p_API.mainForm.MoveCameraToXZ(centerOfLocal);
    p_API.mainForm.CameraMoveMode := _cmmLookAbout;
    p_API.mainForm.CameraResetPitchAngle(CAMERA_PITCH_ANGLE);
    p_API.mainForm.CameraResetYawAngle(0);
    p_API.mainForm.CameraMove(dInOut, CAMERA_DISTANCE_FROM_CENTER);
    end;

procedure TFrameSimulationAreaSettings.CheckBoxMeshVisibleClick(Sender: TObject);
    begin
    p_MeshVisible := not CheckBoxMeshVisible.Checked;
    end;

procedure TFrameSimulationAreaSettings.CheckBoxStreetMapClick(Sender: TObject);
    begin
    p_API.VisualOptionsRoot.DisplayOption[_StreetMap] := CheckBoxStreetMap.Checked;
    end;

procedure TFrameSimulationAreaSettings.SetPedestrianMapList(const aValue: TPedestrianMapList);
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
        UpdateMode;
        end;
    end;

function TFrameSimulationAreaSettings.GetPedestrianMapUser: TPedestrianMapUser;
    begin
    Result := p_PedestrianMapUser;
    end;

procedure TFrameSimulationAreaSettings.SetPedestrianMapUser(const aValue: TPedestrianMapUser);
    begin
    if Assigned(p_PedestrianMapUser) then
        begin
        FrameSimulationAreaEditMesh.MapUser := nil;
        p_PedestrianMapUser.UnRegisterOnChangeUserPedestrianMapEvent(OnChangedUserPedestrianMapEvent);
        end;

    p_PedestrianMapUser := aValue;
    if Assigned(p_PedestrianMapUser) then
        begin
        FrameSimulationAreaEditMesh.MapUser := p_PedestrianMapUser;
        p_PedestrianMapUser.RegisterOnChangeUserPedestrianMapEvent(OnChangedUserPedestrianMapEvent);
        UpdateMenu;
        if p_PedestrianMapUser.Enabled then
            ChangeToEditMeshMode;

        UpdateMode;
        end;
    end;

procedure TFrameSimulationAreaSettings.TimerUIEnableTimer(Sender: TObject);
    var
        createMeshButtonEnabled: Boolean;
    begin
    createMeshButtonEnabled := Assigned(p_PedestrianMapList) and (not (p_PedestrianMapList.IsEditFirstPoint or p_PedestrianMapList.IsEditSecondPoint));
    ButtonStartSelectFirstPoint.Enabled  := createMEshButtonEnabled;
    ButtonStartSelectSecondPoint.Enabled := createMEshButtonEnabled;
    end;
end.
