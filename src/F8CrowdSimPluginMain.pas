//Main unit of the plug-in, it declares and implements the plug-in object
//that will be loaded by UC-win/Road.
unit F8CrowdSimPluginMain;

interface

uses
    System.Generics.Collections,
    System.SysUtils,
    System.Classes,
    Vcl.StdCtrls,
    Vcl.Dialogs,
    PluginCore,
    CrowdSimPlayerForm,
    F8OpenGL,
    F8GLUtils,
    TranRoadList,
    CrowdSimLogSensorAreaRenderer,
    WQAreaRenderer,
    PedestrianMapList,
    PedestrianMapUser,
    F8CrowdSimController,
    SensorDataLoaderSettingForm,
    TrafficSensor,
    TrafficSensorDat,
    crowdSimBuildingList,
    WaitingQueueAreaList,
    PopOutPointList,
    PedestrianMovingData,
    SimulationInputUtils,
    CrosswalkInReplay,
    PedestrianCategoryData,
    PedestrianProfileOptionData,
    WalkingRouteAutomaticGenerator,
    crowdSimCityFurnitureList;

type
    /// <summary>
    ///    人流シミュレーションシステムのメインクラス
    /// </summary>
    TF8CrowdSimPlugin = class(TF8PLuginClass, IF8OpenGLPlugin)
        private
            p_winRoadApplication : IF8ApplicationServices;
            p_ribbon : IF8Ribbon;
            p_ribbonTab : IF8RibbonTab;
            p_ribbonGroup : IF8RibbonGroup;

            ButtonCrowdSimPlayer    : TButton;
            p_formCrowdSimPlayer    : TFormCrowdSimPlayer;
            p_Controller : F8CrowdSimControllerClass;

            {CityGML}
            p_CityGMLRoadList: TTranRoadList;
            p_CityGMLBuildingList: TCrowdSimBuildingList;
            p_CityGMLCityFurnitureList: TCrowdSimCityFurnitureList;
            p_PedestrianMapList: TPedestrianMapList;
            p_PedestrianMapUser: TPedestrianMapUser;


            {WaitingQueue}
            p_WaitingQueueAreaList : TWaitingQueueAreaList;
            p_AreaSensorRenderer   : TCrowdSimLogSensorAreaRenderer;
            p_WQAreaRenderer       : TWQAreaRenderer;
            p_IsWQAreaRendering    : boolean;

            {PopOutPoint}
            p_RenderDispersionPointList   : TPopOutPointList;
            p_AllPedestrianLabelList      : TObjectList<TPedestrianData>;

            p_PedestrianMovingList        : TPedestrianMovingList;
            p_IsInOutNodeList             : TList<TNodeNumArray>;
            p_CrosswalkInReplayList       : TCrosswalkInReplayList;
            p_PedestrianProfileOptionList : TPedestrianProfileOptionList;
            p_PedestrianCategoryList      : TPedestrianCategoryList;

            p_SensorDataLoaderSettingForm : TFormSensorDataLoaderSetting;
            p_SensorData                  : TrafficSensorListClass;
            p_SensorDatData               : TrafficSensorDatClass;

            p_WalkingRouteGenerator: TWalkingRouteAutomaticGenerator;

            procedure   CreateRibbonGUI;
            function    CreateButton(const menuName, menuCaption : String; const onClick : TNotifyEvent) : TButton;
            procedure   DestroyRibbonGUI;
            procedure   DestroyViews;
            procedure   CrowdSimPlayerMenuClick(sender : Tobject);

            // IF8OpenGL implements
            procedure   SetupCamera(const opengl : TF8OpenGL; const view: ViewType);
            procedure   PaintScene(const opengl : TF8OpenGL; const view: ViewType);
            procedure   PaintHUD(const opengl : TF8OpenGL; const view: ViewType);

            procedure   ImportTrafficSensorClick(Sender: TObject);
            procedure   AbleMenus(enable: Boolean);
            procedure   FixupPluginData;
            procedure   BeforeSaveProject(const name : string);
            procedure   ProjectDestroy;

            procedure   TransientWorldAfterMove(dTimeInSeconds : Double);

            procedure   BeforeTrafficStarted;
            procedure   TrafficStarted;
            procedure   TrafficStopped;

            procedure   OnImportSensorsData;
            procedure   OnImportSensorDetails;
            procedure   OnImportInOutDat;
            procedure   OnImportGenderOldDat;

            function    OnBeforeExportXMLEvent: Boolean;
        public
            procedure   AfterConstruction; override;
            procedure   BeforeDestruction; override;

            property    winRoadApplication : IF8ApplicationServices read p_winRoadApplication;
            property    PedestrianMapList  : TPedestrianMapList     read p_PedestrianMapList;
            property    SensorData         : TrafficSensorListClass read p_SensorData;
            property    SensorDatData      : TrafficSensorDatClass  read p_SensorDatData;
        end;

    //Call back functions
    procedure RegisterUserPlugin(out optionName, apiVersion, Copyright : String);
    procedure LoadPlugin;
    procedure UnloadPlugin;

var
    F8CrowdSimPlugin : TF8CrowdSimPlugin;

implementation

uses
    Vcl.Controls,
    MovingFeature,
    MFJsonLoader,
    TrafficSensorLoader,
    GL,
    F8Utils,
    PedestrianUtil,
    LatLonHelper;

const
    PLUGIN_NAME = 'MF-Json Loader Plugin';
    COPY_RIGHT = 'FORUM8 Co., Ltd.';

    MENU_ITEM_TRAFFICSENSORDATA_IMPORT = 'Import TrafficSernsorData';

//==============================================================================
//   Register / Unregister the plugin
//==============================================================================
procedure RegisterUserPlugin(out optionName, apiVersion, Copyright : String);
    begin
    optionName := PLUGIN_NAME;
    apiVersion := PLUGIN_VERSION;
    Copyright := COPY_RIGHT;
    end;

procedure LoadPlugin;
    begin
    if not Assigned(F8CrowdSimPlugin) then
        F8CrowdSimPlugin := TF8CrowdSimPlugin.Create;
    if Assigned(F8CrowdSimPlugin) and Assigned(F8CrowdSimPlugin.winRoadApplication) then
        F8CrowdSimPlugin.winRoadApplication.RegisterPluginObject(F8CrowdSimPlugin);
    end;

procedure UnloadPlugin;
    begin
    if Assigned(F8CrowdSimPlugin) and Assigned(F8CrowdSimPlugin.winRoadApplication) then
        F8CrowdSimPlugin.winRoadApplication.UnRegisterPluginObject(F8CrowdSimPlugin);
    FreeAndNil(F8CrowdSimPlugin);
    end;

{ TAPlugin }

//==============================================================================
//  On plugin creation :
//     Gets the interface of the application.
//==============================================================================
procedure TF8CrowdSimPlugin.AfterConstruction;
    procedure RegisterAppEvents;
        var
            method : TMethod;
        begin
        PluginAbleMenusProc(method) := AbleMenus;
        winRoadApplication.RegisterEventHandler(_plgPluginAbleMenus, method);

        PluginNotifyProc(method) := FixupPluginData;
        winRoadApplication.RegisterEventHandler(_plgFixUpPluginData, method);

        NameProc(method) := BeforeSaveProject;
        winRoadApplication.RegisterEventHandler(_plgBeforeSaveProject, method);

        PluginNotifyProc(method) := ProjectDestroy;
        winRoadApplication.RegisterEventHandler(_plgProjectDestroy, method);

        PluginNotifyProc(method) := BeforeTrafficStarted;
        winRoadApplication.RegisterEventHandler(_plgBeforeTrafficStarted, method);

        PluginNotifyProc(method) := TrafficStarted;
        winRoadApplication.RegisterEventHandler(_plgTrafficStarted, method);

        PluginNotifyProc(method) := TrafficStopped;
        winRoadApplication.RegisterEventHandler(_plgTrafficStopped, method);

        TransientWorldAfterMoveProc(method) := TransientWorldAfterMove;
        winRoadApplication.RegisterEventHandler(_plgTransientWorldAfterMove, method);
        end;
    begin
    inherited;
    Supports(ApplicationServices, IF8ApplicationServices, p_winRoadApplication);

    //Add the sub-menu for simplified model to File - Import
    winRoadApplication.mainForm.AddImportExportMenuItem(MENU_ITEM_TRAFFICSENSORDATA_IMPORT,
                                                        'Import TrafficSernsorData...',
                                                        True,
                                                        ImportTrafficSensorClick);

    RegisterAppEvents;

    CreateRibbonGUI;

    p_PedestrianMovingList  := TPedestrianMovingList.Create;
    p_IsInOutNodeList       := TList<TNodeNumArray>.Create;
    p_CrosswalkInReplayList := TCrosswalkInReplayList.Create;

    p_CityGMLRoadList := TTranRoadList.Create;
    p_CityGMLBuildingList := TCrowdSimBuildingList.Create;
    p_CityGMLCityFurnitureList := TCrowdSimCityFurnitureList.Create;
    p_controller := F8CrowdSimControllerClass.Create(p_winRoadApplication, p_PedestrianMovingList, p_CrosswalkInReplayList);
    p_PedestrianMapList := TPedestrianMapList.Create(p_winRoadApplication, p_CityGMLRoadList, p_CityGMLCityFurnitureList);
    p_PedestrianMapUser := TPedestrianMapUser.Create(p_winRoadApplication, p_controller);

    p_WaitingQueueAreaList := TWaitingQueueAreaList.Create;
    p_AreaSensorRenderer   := TCrowdSimLogSensorAreaRenderer.Create;
    p_WQAreaRenderer     := TWQAreaRenderer.Create;
    p_IsWQAreaRendering  := false;

    p_RenderDispersionPointList := TPopOutPointList.Create;
    p_AllPedestrianLabelList := TObjectList<TPedestrianData>.Create;

    p_PedestrianProfileOptionList := TPedestrianProfileOptionList.Create;
    p_PedestrianCategoryList      := TPedestrianCategoryList.Create;

    p_PedestrianMapList.RegisterOnChangeActiveMapIndexEvent(p_PedestrianMapUser.OnChangeActiveMapIndex);

    p_SensorData    := TrafficSensorListClass.Create;
    p_SensorDatData := TrafficSensorDatClass.Create;

    p_WalkingRouteGenerator := TWalkingRouteAutomaticGenerator.Create;
    end;

//==============================================================================
//  On plugin destruction :
//     Releases the interface of the application.
//==============================================================================
procedure TF8CrowdSimPlugin.BeforeDestruction;
    procedure UnRegisterAppEvents;
        var
            method : TMethod;
        begin
        PluginAbleMenusProc(method) := AbleMenus;
        winRoadApplication.UnRegisterEventHandler(_plgPluginAbleMenus, method);
        PluginNotifyProc(method) := FixupPluginData;
        winRoadApplication.UnRegisterEventHandler(_plgFixUpPluginData, method);
        NameProc(method) := BeforeSaveProject;
        winRoadApplication.UnRegisterEventHandler(_plgBeforeSaveProject, method);
        PluginNotifyProc(method) := ProjectDestroy;
        winRoadApplication.UnRegisterEventHandler(_plgProjectDestroy, method);
        PluginNotifyProc(method) := BeforeTrafficStarted;
        winRoadApplication.UnRegisterEventHandler(_plgBeforeTrafficStarted, method);
        PluginNotifyProc(method) := TrafficStarted;
        winRoadApplication.UnRegisterEventHandler(_plgTrafficStarted, method);
        PluginNotifyProc(method) := TrafficStopped;
        winRoadApplication.UnRegisterEventHandler(_plgTrafficStopped, method);
        TransientWorldAfterMoveProc(method) := TransientWorldAfterMove;
        winRoadApplication.UnRegisterEventHandler(_plgTransientWorldAfterMove, method);
        end;
    begin
    inherited;

    DestroyRibbonGUI;
    DestroyViews;
    if Assigned(p_SensorDataLoaderSettingForm) then
        FreeAndNil(p_SensorDataLoaderSettingForm);

    p_SensorData    := nil;
    p_SensorDatData := nil;

    p_PedestrianMapList.UnRegisterOnChangeActiveMapIndexEvent(p_PedestrianMapUser.OnChangeActiveMapIndex);

    UnRegisterAppEvents;

    winRoadApplication.mainForm.RemoveImportExportMenuItem(MENU_ITEM_TRAFFICSENSORDATA_IMPORT, True);

    FreeAndNil(p_PedestrianMovingList);
    FreeAndNil(p_IsInOutNodeList);
    FreeAndNil(p_CrosswalkInReplayList);
    FreeAndNil(p_RenderDispersionPointList);
    FreeAndNil(p_AllPedestrianLabelList);
    FreeAndNil(p_WaitingQueueAreaList);
    FreeAndNil(p_PedestrianMapUser);
    FreeAndNil(p_PedestrianMapList);
    FreeAndNil(p_CityGMLRoadList);
    FreeAndNil(p_CityGMLBuildingList);
    FreeAndNil(p_CityGMLCityFurnitureList);
    FreeAndNil(p_AreaSensorRenderer);
    FreeAndNil(p_Controller);
    FreeAndNil(p_PedestrianProfileOptionList);
    FreeAndNil(p_PedestrianCategoryList);
    FreeAndNil(p_WalkingRouteGenerator);

    MovingFeatureListClass.BeforeDestructions;
    p_winRoadApplication := nil;
    end;

function TF8CrowdSimPlugin.CreateButton(const menuName, menuCaption: String; const onClick: TNotifyEvent): TButton;
    begin
    p_ribbonGroup.Width := p_ribbonGroup.Width + 107;

    result := TButton.Create(p_ribbonGroup.GetSelf);
    result.Name := menuName;
    result.Align := alLeft;
    result.Margins.Left := 1;
    result.Margins.Top := 1;
    result.Margins.Bottom := 4;
    result.Margins.Right := 1;
    result.AlignWithMargins := true;
    result.Width := 105;
    result.Caption := menuCaption;
    result.WordWrap := True;
    result.OnClick := onClick;
    end;

procedure TF8CrowdSimPlugin.CreateRibbonGUI;
    const
        PLUGIN_TABCONTROLNAME = 'CrowdSimTabControl';
        PLUGIN_TABCAPTION = 'Crowd Sim';
        PLUGIN_RIBBONGROUP_CONTROLNAME = 'CrowdSimPlayer';
        PLUGIN_RIBBONGROUP_NAME = 'Crowd Sim Player';
    begin
    if Assigned(p_winRoadApplication) and not Assigned(p_ribbon) then
        begin
        p_ribbon := p_winRoadApplication.mainForm.GetMainRibbonMenu;
        p_ribbonTab := p_winRoadApplication.mainForm.GetMainRibbonMenuTabByName(PLUGIN_TABCONTROLNAME);
        if Assigned(p_ribbon) and not Assigned(p_ribbonTab)then
            begin
            p_ribbonTab := p_Ribbon.CreateRibbonTab(PLUGIN_TABCONTROLNAME, 11000);
            p_ribbonTab.Caption := PLUGIN_TABCAPTION;
            end;
        end;

    p_ribbonGroup := p_ribbonTab.CreateRibbonGroup(PLUGIN_RIBBONGROUP_CONTROLNAME, 20);
    p_ribbonGroup.Caption := PLUGIN_RIBBONGROUP_NAME;
    p_ribbonGroup.AutoPositionControl := false;
    p_ribbonGroup.Width := 2;

    ButtonCrowdSimPlayer := CreateButton('ClowdSimPlayerMenu', 'Crowd Sim Player...', CrowdSimPlayerMenuClick);
    p_ribbonGroup.AddGroupControl(ButtonCrowdSimPlayer);
    end;

procedure TF8CrowdSimPlugin.CrowdSimPlayerMenuClick(sender: Tobject);

    procedure SetSimInputLists;
        begin
        p_formCrowdSimPlayer.WQAreaList  := p_WaitingQueueAreaList;
        p_formCrowdSimPlayer.RenderDispersionPointList := p_RenderDispersionPointList;
        p_formCrowdSimPlayer.AllPedLblList  := p_AllPedestrianLabelList;
        end;

    begin
    if not Assigned(p_formCrowdSimPlayer) then
        begin
        p_formCrowdSimPlayer := TFormCrowdSimPlayer.Create(nil);
        p_formCrowdSimPlayer.OnBeforeExportXMLEvent := OnBeforeExportXMLEvent;
        p_formCrowdSimPlayer.PedestrianMapList := p_PedestrianMapList;
        p_formCrowdSimPlayer.PedestrianMapUser := p_PedestrianMapUser;
        p_FormCrowdSimPlayer.BuildingList := p_CityGMLBuildingList;
        SetSimInputLists;
        p_formCrowdSimPlayer.Controller := p_Controller;
        p_formCrowdSimPlayer.PedestrianMovingList := p_PedestrianMovingList;
        p_formCrowdSimPlayer.IsInOutNodeList := p_IsInOutNodeList;
        p_formCrowdSimPlayer.CrosswalkInReplayList := p_CrosswalkInReplayList;
        p_formCrowdSimPlayer.PedestrianProfileOptionList := p_PedestrianProfileOptionList;
        p_formCrowdSimPlayer.PedestrianCategoryList := p_PedestrianCategoryList;
        p_PedestrianmapUser.RegisterOnChangeUserPedestrianMapEvent(p_formCrowdSimPlayer.OnChangedUseMap);
        end;
    p_formCrowdSimPlayer.Show;
    end;

procedure TF8CrowdSimPlugin.DestroyRibbonGUI;
    begin
    //Remove Control from the RibbonGroup
    if Assigned(p_RibbonGroup) then
        begin
        p_RibbonGroup.RemoveGroupControl(ButtonCrowdSimPlayer);
        ButtonCrowdSimPlayer := nil;
        end;

    //Remove RibbonGroup from the RibbonTab
    if Assigned(p_RibbonTab) then
        begin
        p_RibbonTab.DeleteGroup(p_RibbonGroup);
        p_RibbonGroup := nil;
        end;

    p_RibbonTab := nil;
    p_Ribbon := nil;
    end;

procedure TF8CrowdSimPlugin.DestroyViews;
    begin
    if Assigned(p_formCrowdSimPlayer) then
        begin
        p_formCrowdSimPlayer.Close;
        p_PedestrianMapUser.UnRegisterOnChangeUserPedestrianMapEvent(p_formCrowdSimPlayer.OnChangedUseMap);
        p_formCrowdSimPlayer.Free;
        p_formCrowdSimPlayer := nil;
        end;
    end;

procedure TF8CrowdSimPlugin.ImportTrafficSensorClick(Sender: TObject);
    begin
    if not Assigned(winRoadApplication.project) then
        begin
        ShowMessage('先にプロジェクトを作成してください。');
        Exit();
        end;

    if not Assigned(p_SensorDataLoaderSettingForm) then
        p_SensorDataLoaderSettingForm := TFormSensorDataLoaderSetting.Create(nil);

    p_SensorDataLoaderSettingForm.OnImportSensorsData   := OnImportSensorsData;
    p_SensorDataLoaderSettingForm.OnImportSensorDetails := OnImportSensorDetails;
    p_SensorDataLoaderSettingForm.OnImportInOutDat      := OnImportInOutDat;
    p_SensorDataLoaderSettingForm.OnImportGenderOldDat  := OnImportGenderOldDat;
    try
        p_SensorDataLoaderSettingForm.app := winRoadApplication;
        p_SensorDataLoaderSettingForm.project := winRoadApplication.project;
        p_SensorDataLoaderSettingForm.ShowModal;
    finally
        p_SensorDataLoaderSettingForm.OnImportGenderOldDat  := nil;
        p_SensorDataLoaderSettingForm.OnImportInOutDat      := nil;
        p_SensorDataLoaderSettingForm.OnImportSensorDetails := nil;
        p_SensorDataLoaderSettingForm.OnImportSensorsData   := nil;
        end;
    end;

procedure TF8CrowdSimPlugin.OnImportSensorsData;
    var
        i    : integer;
        name : string;
    begin
    if not Assigned(p_SensorDataLoaderSettingForm) then
        Exit;

    if not Assigned(p_SensorData) then
        p_SensorData := TrafficSensorListClass.Create;

    if p_SensorDataLoaderSettingForm.CheckBoxAddedData.Checked = false then
        p_SensorData.ClearSensorData;

    for i := 0 to p_SensorDataLoaderSettingForm.OpenDialogJson.Files.Count - 1 do
        begin
        name := p_SensorDataLoaderSettingForm.OpenDialogJson.Files[i];
        try
            ImportTrafficSensorJson(winRoadApplication.project, name, p_SensorData);
            p_SensorDataLoaderSettingForm.ImportedSensorData := true;
        finally
            p_SensorDataLoaderSettingForm.SensorData := p_SensorData;
            end;
        end;
    end;

procedure TF8CrowdSimPlugin.OnImportSensorDetails;
    var
        i    : integer;
        name : string;
    begin
    if not Assigned(p_SensorDataLoaderSettingForm) then
        Exit;

    if not Assigned(p_SensorData) then
        p_SensorData := TrafficSensorListClass.Create;

    if p_SensorDataLoaderSettingForm.CheckBoxAddedData.Checked = false then
        p_SensorData.ClearDetail;

    for i := 0 to p_SensorDataLoaderSettingForm.OpenDialogdetailJson.Files.Count - 1 do
        begin
        name := p_SensorDataLoaderSettingForm.OpenDialogdetailJson.Files[i];
        try
            ImportTrafficSensorDetailJson(winRoadApplication.project, name, p_SensorData);
            p_SensorDataLoaderSettingForm.ImportedDetailData := true;
        finally
            p_SensorDataLoaderSettingForm.SensorData := p_SensorData;
            end;
        end;
    end;

procedure TF8CrowdSimPlugin.OnImportInOutDat;
    var
        name : string;
    begin
    if not Assigned(p_SensorDataLoaderSettingForm) then
        Exit;

    if not Assigned(p_SensorDatData) then
        p_SensorDatData := TrafficSensorDatClass.Create;

    p_SensorDatData.ClearInOutData;
    name := p_SensorDataLoaderSettingForm.OpenDialogDat.FileName;
    try
        ImportTrafficSensorDat(winRoadApplication.project, name, p_SensorDatData);
        p_SensorDataLoaderSettingForm.ImportedDatData := true;
    finally
        p_SensorDataLoaderSettingForm.DatData := p_SensorDatData;
        end;
    end;

procedure TF8CrowdSimPlugin.OnImportGenderOldDat;
    var
        name : string;
    begin
    if not Assigned(p_SensorDataLoaderSettingForm) then
        Exit;

    if not Assigned(p_SensorDatData) then
        p_SensorDatData := TrafficSensorDatClass.Create;

    p_SensorDatData.ClearGenderOldData;
    name := p_SensorDataLoaderSettingForm.OpenDialogGenderOldDat.FileName;
    try
        ImportTrafficSensorGenderOldDat(winRoadApplication.project, name, p_SensorDatData);
        p_SensorDataLoaderSettingForm.ImportedDatData := true;
    finally
        p_SensorDataLoaderSettingForm.DatData := p_SensorDatData;
        end;
    end;

procedure TF8CrowdSimPlugin.PaintHUD(const opengl: TF8OpenGL; const view: ViewType);
    begin
    end;

procedure TF8CrowdSimPlugin.PaintScene(const opengl: TF8OpenGL; const view: ViewType);
    procedure RenderScene;
        begin
        if Assigned(p_formCrowdSimPlayer) then
            begin
            p_PedestrianMapList.Render(openGL);
            p_PedestrianMapUser.Render(OpenGL);
            end
        else
            p_PedestrianMapUser.Render(OpenGL);
        end;

    procedure RenderWQArea;
        begin
        if p_IsWQAreaRendering then
            p_WaitingQueueAreaList.Renderer.Render(opengl)
        else if Assigned(p_formCrowdSimPlayer) then
            begin
            if Assigned(p_formCrowdSimPlayer.SidePanel) then
                begin
                if Assigned(p_formCrowdSimPlayer.SidePanel.WaitingQueueSettingsFrame) then
                    p_WaitingQueueAreaList.Renderer.Render(opengl);
                end;
            end;
        end;

    procedure RenderDispersionPoint;
        begin
        if p_RenderDispersionPointList.IsVisibleDispersionPoint then
            p_RenderDispersionPointList.RenderDP(opengl)
        else if Assigned(p_formCrowdSimPlayer) then
            begin
            if Assigned(p_formCrowdSimPlayer.SidePanel) then
                begin
                if Assigned(p_formCrowdSimPlayer.SidePanel.PedestrianPopOutFrame) then
                    p_formCrowdSimPlayer.SidePanel.PedestrianPopOutFrame.POPointList.RenderDP(opengl);
                end;
            end;
        end;

    begin
    if view <> _ViewMain then
        Exit;

    RenderScene;

    if Assigned(p_formCrowdSimPlayer) then
        begin
        p_formCrowdSimPlayer.UpdateSensorAreaRendererData(p_AreaSensorRenderer);
        p_formCrowdSimPlayer.UpdateWQAreaRendererData(p_WaitingQueueAreaList.Renderer, p_IsWQAreaRendering);
        end;
    p_AreaSensorRenderer.Render(opengl);
    RenderWQArea;
    RenderDispersionPoint;
//    p_CityGMLRoadList.DoRender(opengl);
//    p_CityGMLBuildingList.DoRender(opengl);
//    p_CityGMLCityFurnitureList.DoRender(opengl);
    end;

procedure TF8CrowdSimPlugin.ProjectDestroy;
    begin
    if Assigned(p_formCrowdSimPlayer) then
        begin
        p_PedestrianMapUser.UnRegisterOnChangeUserPedestrianMapEvent(p_formCrowdSimPlayer.OnChangedUseMap);
        p_formCrowdSimPlayer.Close;
        FreeAndNil(p_formCrowdSimPlayer);
        end;
    MovingFeatureListClass.ClearMovingFeatures;

    if Assigned(p_SensorDataLoaderSettingForm) then
        FreeAndNil(p_SensorDataLoaderSettingForm);

    p_SensorData    := nil;
    p_SensorDatData := nil;
    end;

procedure TF8CrowdSimPlugin.SetupCamera(const opengl: TF8OpenGL; const view: ViewType);
    var
        glPosition: GLPointType;
    begin
    glPosition := opengl.Find3DCoordinatesUnderMouse;
    if F8IsAGLPoint(glPosition) and p_PedestrianMapUser.Enabled then
        p_PedestrianMapUser.UpdateUnderMouseGuide(Point3D(glPosition[_x], glPosition[_y], glPosition[_z]));

    if Assigned(p_formCrowdSimPlayer) then
        p_formCrowdSimPlayer.Collect;

    if p_PedestrianMapUser.Enabled then
        p_AreaSensorRenderer.HeightOffset := p_PedestrianMapUser.Map.HeightOffset;
    end;

procedure TF8CrowdSimPlugin.AbleMenus(enable: Boolean);
    begin
    enable := Assigned(p_winRoadApplication.project) and enable;
    ButtonCrowdSimPlayer.Enabled := enable;
    end;

procedure TF8CrowdSimPlugin.FixupPluginData;
    begin
    p_PedestrianMapList.ImportFromPluginData;
    p_CityGMLRoadList.ImportFromPluginData(winRoadApplication.project);
    p_CityGMLBuildingList.ImportFromPluginData(winRoadApplication.project);
    p_CityGMLCityFurnitureList.ImportFromPluginData(winRoadApplication.project);
    p_WaitingQueueAreaList.LoadData;
    p_RenderDispersionPointList.ReceiveDispersionPointData;
    p_PedestrianMapUser.ImportFromPluginData;
    p_AreaSensorRenderer.ImportFromPluginData;
    end;

procedure TF8CrowdSimPlugin.BeforeSaveProject(const name : string);
    begin
    p_PedestrianMapList.ExportToPluginData;
    p_CityGMLRoadList.ExportToPluginData(winRoadApplication.project);
    p_CityGMLBuildingList.ExportToPluginData(winRoadApplication.project);
    p_CityGMLCityFurnitureList.ExportToPluginData(winRoadApplication.project);
    p_WaitingQueueAreaList.SaveData;
    p_PedestrianMapUser.ExportToPluginData;
    p_AreaSensorRenderer.ExportToPluginData;
    end;

procedure TF8CrowdSimPlugin.BeforeTrafficStarted;
    begin
    MovingFeatureListClass.BeforeStartTraffic(winRoadApplication.project,p_Controller.IsRain);
    end;

procedure TF8CrowdSimPlugin.TrafficStarted;
    begin
    p_PedestrianMapList.TrafficStarted;
    end;

procedure TF8CrowdSimPlugin.TrafficStopped;
    begin
    //MovingFeatureListClass.StopTraffic(winRoadApplication.project);
    p_PedestrianMapList.TrafficStopped;
    end;

procedure TF8CrowdSimPlugin.TransientWorldAfterMove(dTimeInSeconds: Double);
    begin
    if not Assigned(p_Controller) then
        Exit;
    //if MovingFeatureListClass.numberOfMovingFeatures > 0 then
    p_Controller.UpdateTimer(dTimeInSeconds);
    p_PedestrianMovingList.UpdateData(winRoadApplication.project, p_Controller.currentTime);
    end;

function TF8CrowdSimPlugin.OnBeforeExportXMLEvent: Boolean;
    begin
    Result := p_PedestrianMapUser.Map.OnBeforeExportXML;
    end;
end.
