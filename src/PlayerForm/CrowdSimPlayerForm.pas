unit CrowdSimPlayerForm;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    Vcl.FileCtrl,
    System.Generics.Collections,
    System.SysUtils,
    System.Variants,
    System.Dateutils,
    System.Classes,
    Vcl.Controls,
    Vcl.Graphics,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.Buttons,
    Vcl.ToolWin,
    Vcl.ComCtrls,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    System.Math,
    PluginCore,
    F8Utils,
    F8GLUtils,
    F8CrowdSimController,
    System.Actions,
    Vcl.ActnList,
    F8OpenGL,
    F8VTEditors,
    F8RealSpinEdit,
    F8FloatSpinEdit,
    Vcl.Menus,
    System.ImageList, Vcl.ImgList,
    PedestrianMapList,
    PedestrianMapUser,
    PedestrianUtil,
    F_CrossSectionLogSettings,
    CrowdSimLogSensorAreaRenderer,
    WQAreaRenderer,
    F_TimeController,
    CrowdSimExportsForm,
    CrowdSimSidePanel,
    F_MainInput,
    WaitingQueueAreaList,
    PopOutPointList,
    PedestrianMovingData,
    SimulationInputUtils,
    CrosswalkInReplay,
    PedestrianCategoryData,
    PedestrianProfileOptionData,
    PopOutSchedule,
    CrowdSimBuildingList,
    BusTransportationResultExport,
    WaitingQueueResultExport;

type
    BeforeExportXMLEvent = function: Boolean of Object;

    /// <summary>
    ///    人流シミュレーションシステムの各種操作を行うフォームとその機能を定義するクラス
    /// </summary>
    TFormCrowdSimPlayer = class(TForm)
        ImageList: TImageList;
        PageControlClient: TPageControl;
        TabSheetArea: TTabSheet;
        TabSheetCSLog: TTabSheet;
        ButtonMFJsonExport : TButton;
        PanelExportMFJson: TPanel;
        GroupBoxExportMFJson: TGroupBox;
        SEMFJsonInterval: TF8RealSpinEdit;
    	LabelMFJsonInterval: TLabel;
        ButtonSelectExportMFJsonDirectory: TSpeedButton;
    	LabelMFJsonDirectory: TLabel;
        EditExportMFJsonDirectory: TEdit;
    	LabelMFJsonName: TLabel;
    	EditMFJsonName: TEdit;
        PanelMenu: TPanel;
        PanelButtonPedestriansSettings: TPanel;
        PanelButtonLogSettings: TPanel;
        PanelButtonAnalysis: TPanel;
        TabSheetAnalysis: TTabSheet;
        FrameControlTime1: TFrameControlTime;
        TimerUIUpdate: TTimer;
        procedure FormShow(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure ActionSelectMFJsonExportDir(Sender: TObject);
        procedure ButtonMFJsonExportClick(Sender: TObject);
        procedure PanelButtonPedestriansSettingsClick(Sender: TObject);
        procedure PanelButtonLogSettingsClick(Sender: TObject);
        procedure PanelButtonAnalysisClick(Sender: TObject);
        procedure PanelButtonsMouseEnter(Sender: TObject);
        procedure PanelButtonsMouseLeave(Sender: TObject);
        procedure btnReplayClick(Sender: TObject);
        procedure TimerUIUpdateTimer(Sender: TObject);
        private
            type
                CrowdSimMenuItemType = (
                    _csmiArea,
                    _csmiLogSettings,
                    _csmiAnalysis
                    );
                CrowdSimMenuButtons = array[CrowdSimMenuItemType] of TPanel;
                CrowdSimMenuTabs    = array[CrowdSimMenuItemType] of TTabSheet;
            var
                MainInputMenuFrame : TFrameMainInput;
                CSLogFrame         : TFrameCrossSectionLogSettings;

                p_OnBeforeExportXMLEvent: BeforeExportXMLEvent;

                p_ExportsForm : TFormCrowdSimExports;

                p_PedestrianMapList: TPedestrianMapList;
                p_PedestrianMapUser: TPedestrianMapUser;
                p_BuildingList: TCrowdSimBuildingList;

                p_WaitingQueueAreaList : TWaitingQueueAreaList;
                p_RenderDispersionPointList : TPopOutPointList;
                p_AllPedestrianLabelList : TList<TPedestrianData>;
                p_PedestrianMovingList  : TPedestrianMovingList;
                p_IsInOutNodeList       : TList<TNodeNumArray>;
                p_CrosswalkInReplayList : TCrosswalkInReplayList;
                p_PedestrianProfileOptionList : TPedestrianProfileOptionList;
                p_PedestrianCategoryList      : TPedestrianCategoryList;

                p_CurrentMenu: CrowdSimMenuItemType;
                p_MenuButtons: CrowdSimMenuButtons;
                p_MenuTabs   : CrowdSimMenuTabs;

                p_SidePanel: TCrowdSimSidePanel;
                p_MeshVisible   : boolean;
                p_WQAreaVisible : boolean;
                p_IsReplaying   : boolean;

			function    GetController: F8CrowdSimControllerClass;
            function    GetApplication: IF8ApplicationServices;
            function    GetPedestrianMapList: TPedestrianMapList;
            procedure   SetPedestrianMapList(const aValue: TPedestrianMapList);
            function    GetPedestrianMapUser: TPedestrianMapUser;
            procedure   SetPedestrianMapUser(const aValue: TPedestrianMapUser);
            function    GetBuildingList: TCrowdSimBuildingList;
            procedure   SetBuildingList(const aValue: TCrowdSimBuildingList);

            function    GetWQAreaList: TWaitingQueueAreaList;
            procedure   SetWQAreaList(const aValue: TWaitingQueueAreaList);
            function    GetRenderDispersionPointList: TPopOutPointList;
            procedure   SetRenderDispersionPointList(const aValue: TPopOutPointList);
            function    GetPedLblList: TList<TPedestrianData>;
            procedure   SetPedLblList(const aValue: TList<TPedestrianData>);
            function    GetMovingList: TPedestrianMovingList;
            procedure   SetMovingList(const aValue: TPedestrianMovingList);
            function    GetIsInOutNodeList: TList<TNodeNumArray>;
            procedure   SetIsInOutNodeList(const aValue: TList<TNodeNumArray>);
            function    GetCrosswalkInReplayList: TCrosswalkInReplayList;
            procedure   SetCrosswalkInReplayList(const aValue: TCrosswalkInReplayList);
            function    GetPedestrianProfileOptionList: TPedestrianProfileOptionList;
            procedure   SetPedestrianProfileOptionList(const aValue: TPedestrianProfileOptionList);
            function    GetPedestrianCategoryList: TPedestrianCategoryList;
            procedure   SetPedestrianCategoryList(const aValue: TPedestrianCategoryList);

            procedure   SetSimBeginningTime(sender : Tobject);
            procedure   SetController(const Value: F8CrowdSimControllerClass);
            procedure   ResetDiagramsData(sender :Tobject);
            function    AssignedLogSettingsAndMap: Boolean;
            procedure   OnStop(sender :Tobject);
            procedure   UpdateMenuItem;
            procedure   OnCloseSidePanel(Sender: TForm);
            procedure   OnClickBtnSimAreaSettings(Sender: TObject);
            procedure   OnClickBtnCrossWalkSettings(Sender: TObject);
            procedure   OnClickBtnWaitingQueueSettings(Sender: TObject);
            procedure   OnClickBtnPedestrianPopOutType(Sender: TObject);
            procedure   AllNodeInOuttoFalse;
            procedure   ResetNodeIsInOut;
            procedure   CleanTransientObjects;
            procedure   RestrictChangeAllCrosswalkSignal;
            procedure   ReleaseAllCrosswalkSignalInterval;
            function    IsNetworkExisted: Boolean;
        public
            procedure   AfterConstruction; override;
            procedure   BeforeDestruction; override;

            procedure   Collect;
            procedure   UpdateSensorAreaRendererData(const aRenderer: TCrowdSimLogSensorAreaRenderer);
            procedure   UpdateWQAreaRendererData(const aRenderer: TWQAreaRenderer; out IsVisible: boolean);
            procedure   OnChangedUseMap;

            property    application : IF8ApplicationServices read GetApplication;
            property    PedestrianMapList: TPedestrianMapList read GetPedestrianMapList write SetPedestrianMapList;
            property    PedestrianMapUser: TPedestrianMapUser read GetPedestrianMapUser write SetPedestrianMapUser;
            property    BuildingList: TCrowdSimBuildingList read GetBuildingList write SetBuildingList;
            property    OnBeforeExportXMLEvent: BeforeExportXMLEvent read p_OnBeforeExportXMLEvent write p_OnBeforeExportXMLEvent;
            property    Controller : F8CrowdSimControllerClass read GetController write SetController;
            property    SidePanel : TCrowdSimSidePanel read p_SidePanel;
            property    WQAreaList : TWaitingQueueAreaList read GetWQAreaList write SetWQAreaList;
            property    RenderDispersionPointList : TPopOutPointList read GetRenderDispersionPointList write SetRenderDispersionPointList;
            property    AllPedLblList : TList<TPedestrianData> read GetPedLblList write SetPedLblList;
            property    PedestrianMovingList : TPedestrianMovingList read GetMovingList write SetMovingList;
            property    IsInOutNodeList : TList<TNodeNumArray> read GetIsInOutNodeList write SetIsInOutNodeList;
            property    CrosswalkInReplayList : TCrosswalkInReplayList read GetCrosswalkInReplayList write SetCrosswalkInReplayList;
            property    PedestrianProfileOptionList : TPedestrianProfileOptionList read GetPedestrianProfileOptionList write SetPedestrianProfileOptionList;
            property    PedestrianCategoryList      : TPedestrianCategoryList      read GetPedestrianCategoryList      write SetPedestrianCategoryList;
        end;

implementation

uses
    System.IOUtils,
    System.Types,
    Winapi.ShellAPI,
    MovingFeature,
    LatLonHelper,
    ODTripResultExport;
{$R *.dfm}

{ TFormCrowdSimPlayer }

procedure TFormCrowdSimPlayer.ActionSelectMFJsonExportDir(Sender: TObject);
    var
        SelectFolder: TArray<String>;
    begin
    if SelectDirectory(application.UserDirectory,
                       SelectFolder,
                       [],
                       '出力先の指定',
                       '出力先フォルダ',
                       '確定') then
        begin
        EditExportMFJsonDirectory.Text := SelectFolder[0];
        end;
    end;

function TFormCrowdSimPlayer.AssignedLogSettingsAndMap: Boolean;
    begin
    Result := Assigned(PedestrianMapUser) and PedestrianMapUser.Enabled;
    end;

procedure TFormCrowdSimPlayer.AfterConstruction;

    procedure SetClickEventMainInputMenu;
        begin
        MainInputMenuFrame.BtnSimAreaSettings.OnClick      := OnClickBtnSimAreaSettings;
        MainInputMenuFrame.BtnCrossWalkSettings.OnClick    := OnClickBtnCrossWalkSettings;
        MainInputMenuFrame.BtnWaitingQueueSettings.OnClick := OnClickBtnWaitingQueueSettings;
        MainInputMenuFrame.BtnPedestrianPopOutType.OnClick := OnClickBtnPedestrianPopOutType;
        end;
    begin
    inherited;
    p_OnBeforeExportXMLEvent := nil;

    MainInputMenuFrame        := TFrameMainInput.Create(Self);
    MainInputMenuFrame.Parent := TabSheetArea;
    MainInputMenuFrame.Name   := 'FrameMainInputMenu';
    MainInputMenuFrame.Align  := alClient;
    SetClickEventMainInputMenu;

    CSLogFrame := TFrameCrossSectionLogSettings.Create(Self);
    CSLogFrame.Parent := TabSheetCSLog;
    CSLogFrame.Name := 'FrameCSLog';
    CSLogFrame.Align := alClient;

    FrameControlTime1.OnNotifyReset := ResetDiagramsData;
    FrameControlTime1.OnNotifyPlay  := SetSimBeginningTime;
    FrameControlTime1.OnNotifyStop  := OnStop;

    p_ExportsForm := TFormCrowdSimExports.Create(nil);
    p_PedestrianMapList := nil;
    p_PedestrianMapUser := nil;
    p_BuildingList := nil;

    p_WaitingQueueAreaList  := nil;
    p_RenderDispersionPointList := nil;
    p_AllPedestrianLabelList   := nil;
    p_IsInOutNodeList       := nil;
    p_CrosswalkInReplayList := nil;

    p_MenuButtons[_csmiArea]        := PanelButtonPedestriansSettings;
    p_MenuButtons[_csmiLogSettings] := PanelButtonLogSettings;
    p_MenuButtons[_csmiAnalysis]    := PanelButtonAnalysis;
    p_MenuTabs[_csmiArea]        := TabSheetArea;
    p_MenuTabs[_csmiLogSettings] := TabSheetCSLog;
    p_MenuTabs[_csmiAnalysis]    := TabSheetAnalysis;

    p_SidePanel := TCrowdSimSidePanel.Create;
    p_SidePanel.OnDockFormClose := OnCloseSidePanel;
    p_MeshVisible := true;
    end;

procedure TFormCrowdSimPlayer.BeforeDestruction;
    begin
    inherited;
    p_PedestrianMapList := nil;
    p_PedestrianMapUser := nil;
    p_BuildingList := nil;

    p_WaitingQueueAreaList := nil;
    p_RenderDispersionPointList := nil;
    p_PedestrianMovingList := nil;

    FreeAndNil(p_ExportsForm);
    FreeAndNil(CSLogFrame);
    p_SidePanel.OnDockFormClose := nil;
    FreeAndNil(p_SidePanel);
    end;

procedure TFormCrowdSimPlayer.btnReplayClick(Sender: TObject);
    var
        time : TDateTime;
    begin
    if (not Assigned(p_PedestrianMovingList)) or (p_IsReplaying) then
        Exit;

    time := FrameControlTime1.SimTimePicker.Time;
    ReplaceDate(time, FrameControlTime1.SimDatePicker.Date);
    RestrictChangeAllCrosswalkSignal;
    CrosswalkInReplayList.BeforeReplay(time);
    PedestrianMovingList.BeforeReplay;
    PedestrianMovingList.DisableTrafficGenerators;
    AllNodeInOuttoFalse;
    p_IsReplaying := true;

    FrameControlTime1.ActionReplayExecute(Sender);
    end;

procedure TFormCrowdSimPlayer.ButtonMFJsonExportClick(Sender: TObject);
    var
        filename : string;
    begin
    if DirectoryExists(EditExportMFJsonDirectory.Text) then
        begin
        if EditMFJsonName.Text <> '' then
            begin
            ButtonMFJsonExport.Enabled := false;
            filename := TPath.Combine(EditExportMFJsonDirectory.Text, EditMFJsonName.Text+'.json');
            PedestrianMovingList.ExportMFJson(filename, SEMFJsonInterval.Value, application);
            showmessage('MF-Jsonを出力しました');
            ButtonMFJsonExport.Enabled := true;
            end
        else
            showmessage('ファイル名を指定してください');
        end
    else
        showmessage('指定されているディレクトリが見つかりません');
    end;

procedure TFormCrowdSimPlayer.Collect;
    begin
    if Assigned(controller) and (controller.currentState in [csPlay,csFast,csBack]) then
        FrameControlTime1.UpdateTime;

    if not p_IsReplaying then
        PedestrianMapUser.CollectCSFlowData;

    if MilliSecondsBetween(FrameControlTime1.SimTimePicker.Time, FrameControlTime1.SimEndTimePicker.Time) < 100 then
        FrameControlTime1.ActionStopExecute(FrameControlTime1.btnSimStop);
    end;

procedure TFormCrowdSimPlayer.UpdateSensorAreaRendererData(const aRenderer: TCrowdSimLogSensorAreaRenderer);
    begin
    CSLogFrame.UpdateSensorAreaRendererData(aRenderer);
    end;

procedure TFormCrowdSimPlayer.UpdateWQAreaRendererData(const aRenderer: TWQAreaRenderer; out IsVisible: boolean);
    begin
    if not Assigned(p_SidePanel) then
        Exit;

    if Assigned(p_SidePanel.WaitingQueueSettingsFrame) then
        begin
        p_WQAreaVisible := p_SidePanel.WaitingQueueSettingsFrame.F_WaitAreaSet.WaitingAreaVisible;
        IsVisible       := p_WQAreaVisible;
        p_SidePanel.WaitingQueueSettingsFrame.UpdateWQAreaRendererData(aRenderer);
        end;
    end;

procedure TFormCrowdSimPlayer.AllNodeInOuttoFalse;
    var
        i, j       : integer;
        tmpNwk     : IF8Networks;
        IsInOutIdx : TNodeNumArray;
    begin
    if Assigned(IsInOutNodeList) then
        IsInOutNodeList.Clear;

    for i := 1 to theApplicationServices.project.NumberOfFlightWayNwks do
        begin
        tmpNwk := theApplicationServices.project.FlightWayNwk[i];
        tmpNwk.Clean;
        if tmpNwk.LinkCount <= 0 then
            Continue;

        for j := 0 to tmpNwk.NodeCount - 1 do
            begin
            if tmpNwk.Node[j].InOut then
                begin
                IsInOutIdx[0] := i;
                IsInOutIdx[1] := j;
                IsInOutNodeList.Add(IsInOutIdx);
                end;

            tmpNwk.Node[j].InOut := false;
            end;
        end;
    end;

procedure TFormCrowdSimPlayer.ResetNodeIsInOut;
    var
        i, NwkIdx, NdIdx : integer;
    begin
    for i := 0 to IsInOutNodeList.Count - 1 do
        begin
        NwkIdx := IsInOutNodeList[i][0];
        NdIdx  := IsInOutNodeList[i][1];
        theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].InOut := true;
        end;
    end;

procedure TFormCrowdSimPlayer.CleanTransientObjects;
    var
        i       : integer;
        TrsObjs : TransientObjectArray;
        proj    : IF8ProjectForRoad;
    begin
    proj := theApplicationServices.project;
    if not proj.trafficIsMoving then
        Exit;

    TrsObjs := proj.GetTransientObjectsOfEveryTypeWithinCenter(Max(proj.GetTerrain.lengthNorthing, proj.GetTerrain.lengthEasting),
                                                               GLZERO);

    for i := 0 to Length(TrsObjs) - 1 do
        TrsObjs[i].deleteMe := true;
    end;

procedure TFormCrowdSimPlayer.RestrictChangeAllCrosswalkSignal;
    var
        i, j          : integer;
        tmpNwk        : IF8Networks;
        tmpCwInReplay : TCrosswalkInReplay;
        CrosswalkIdx  : TPathNumArray;
    begin
    if Assigned(CrosswalkInReplayList) then
        CrosswalkInReplayList.CrosswalkList.Clear;

    for i := 1 to theApplicationServices.project.NumberOfFlightWayNwks do
        begin
        tmpNwk := theApplicationServices.project.FlightWayNwk[i];
        tmpNwk.Clean;
        if tmpNwk.LinkCount <= 0 then
            Continue;

        for j := 0 to tmpNwk.LinkCount - 1 do
            begin
            if (tmpNwk.Link[j].LinkType = _nlCrosswalkBlue) or (tmpNwk.Link[j].LinkType = _nlCrosswalkRed) then
                begin
                CrosswalkIdx[0] := i;
                CrosswalkIdx[1] := j;
                tmpCwInReplay   := TCrosswalkInReplay.Create(CrosswalkIdx, tmpNwk.Link[j].SignalInterval, tmpNwk.Link[j].RedSignalInterval, tmpNwk.Link[j].LinkType);
                CrosswalkInReplayList.CrosswalkList.Add(tmpCwInReplay);
                tmpNwk.Link[j].SignalInterval := MAXIMUM_SIGNAL_INTERVAL;
                tmpNwk.Link[j].RedSignalInterval := MAXIMUM_SIGNAL_INTERVAL;
                end;
            end;
        end;
    end;

procedure TFormCrowdSimPlayer.ReleaseAllCrosswalkSignalInterval;
    var
        i, NwkIdx, PathIdx : integer;
    begin
    for i := 0 to CrosswalkInReplayList.CrosswalkList.Count - 1 do
        begin
        NwkIdx   := CrosswalkInReplayList.CrosswalkList[i].Idx[0];
        PathIdx  := CrosswalkInReplayList.CrosswalkList[i].Idx[1];
        theApplicationServices.project.FlightWayNwk[NwkIdx].Link[PathIdx].SignalInterval := CrosswalkInReplayList.CrosswalkList[i].SignalInterval;
        theApplicationServices.project.FlightWayNwk[NwkIdx].Link[PathIdx].RedSignalInterval := CrosswalkInReplayList.CrosswalkList[i].RedSignalInterval;
        theApplicationServices.project.FlightWayNwk[NwkIdx].Link[PathIdx].LinkType := CrosswalkInReplayList.CrosswalkList[i].InitSignal;
        end;
    end;

function TFormCrowdSimPlayer.IsNetworkExisted: Boolean;
    begin
    Result := Assigned(Application) and Assigned(Application.project) and (Application.project.numberOfFlightWayNwks > 0);
    end;

procedure TFormCrowdSimPlayer.OnChangedUseMap;
    begin
    if Assigned(CSLogFrame) then
        CSLogFrame.OnChangeUserMap;
    end;

procedure TFormCrowdSimPlayer.FormShow(Sender: TObject);
    begin
    p_CurrentMenu := _csmiArea;
    UpdateMenuItem;
    CSLogFrame.OnChangeActivate(False);
    end;

procedure TFormCrowdSimPlayer.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
    CSLogFrame.OnChangeActivate(False);
    if p_ExportsForm.Showing then
        p_ExportsForm.Close;
    end;

function TFormCrowdSimPlayer.GetApplication: IF8ApplicationServices;
    begin
    if Assigned(FrameControlTime1.Controller) then
        Result := FrameControlTime1.Controller.application
    else
        Result := nil;
    end;

function TFormCrowdSimPlayer.GetController: F8CrowdSimControllerClass;
    begin
    Result := FrameControlTime1.Controller;
    end;

procedure TFormCrowdSimPlayer.OnStop(sender :Tobject);
    var
        busTransportationRes: TBusOperationLogsExport;
        waitingQueueRes     : TWaitingQueueResultExport;
        odTripRes           : TODTripResultExport;
    begin
    if not p_IsReplaying then
        CleanTransientObjects
    else
        begin
        PedestrianMovingList.RevertTrafficGenerators;
        PedestrianMovingList.AfterReplay;
        CrosswalkInReplayList.AfterReplay;
        ResetNodeIsInOut;
        ReleaseAllCrosswalkSignalInterval;
        p_IsReplaying := false;
        Exit;
        end;

    if controller.currentState in [csPlay, csFast, csBack] then
        begin
        if AssignedLogSettingsAndMap then
            begin
            p_ExportsForm.SetUpExportsData(PedestrianMapUser);
            PedestrianMapUser.ExportCrossSectionLog;
            busTransportationRes := TBusOperationLogsExport.Create;
            try
                if PedestrianMapUser.CrossSectionFlowLog.IsExportBusTransportLog then
                    begin
                    busTransportationRes.CollectLog(application.Project);
                    busTransportationRes.ExportToCSV(controller.StartSimTime, application.UserDirectory);
                    p_ExportsForm.UpdateBusTransportationLog(busTransportationRes);
                    end;
            finally
                FreeAndNil(busTransportationRes);
                end;
            waitingQueueRes := TWaitingQueueResultExport.Create(application.Project, controller.StartSimTime, controller.currentTime);
            try
                waitingQueueRes.ExportSumOfLogToCSV(controller.StartSimTime, application.UserDirectory);
                waitingQueueRes.ExportLogToCSV(controller.StartSimTime, application.UserDirectory);
                p_ExportsForm.UpdateWaitingQueueLog(waitingQueueRes);
            finally
                FreeAndNil(waitingQueueRes);
                end;
            odTripRes := TODTripResultExport.Create(application.Project, controller.StartSimTime, controller.currentTime);
            try
                odTripRes.ExportToCSV(controller.StartSimTime, application.UserDirectory);
                p_ExportsForm.UpdateODTripLog(odTripRes);
            finally
                FreeAndNil(odTripRes);
                end;

            p_ExportsForm.Show;
            end;
        end;

    if Assigned(CSLogFrame) then
        CSLogFrame.ChangeEnabledchbLogs(True);
    end;

procedure TFormCrowdSimPlayer.PanelButtonPedestriansSettingsClick(Sender: TObject);
    begin
    if p_CurrentMenu <> _csmiArea then
        begin
        p_CurrentMenu := _csmiArea;
        UpdateMenuItem;
        end;
    end;

procedure TFormCrowdSimPlayer.PanelButtonLogSettingsClick(Sender: TObject);
    begin
    if p_CurrentMenu <> _csmiLogSettings then
        begin
        if CSLogFrame.AbleToOpen then
            begin
            p_CurrentMenu := _csmiLogSettings;
            UpdateMenuItem;
            CSLogFrame.CSLogSettingsShow;
            end
        else
            ShowMessage('メッシュが生成されていません');
        end;
    end;


procedure TFormCrowdSimPlayer.PanelButtonAnalysisClick(Sender: TObject);
    begin
    if p_CurrentMenu <> _csmiAnalysis then
        begin
        p_CurrentMenu := _csmiAnalysis;
        UpdateMenuItem;
        end;
    end;

procedure TFormCrowdSimPlayer.PanelButtonsMouseEnter(Sender: TObject);
    var
        menuButton: TPanel;
    begin
    menuButton := (Sender as TPanel);
    menuButton.Font.Style := menuButton.Font.Style + [TFontStyle.fsUnderline];
    end;

procedure TFormCrowdSimPlayer.PanelButtonsMouseLeave(Sender: TObject);
    var
        menuButton: TPanel;
    begin
    menuButton := (Sender as TPanel);
    menuButton.Font.Style := menuButton.Font.Style - [TFontStyle.fsUnderline];
    end;

procedure TFormCrowdSimPlayer.UpdateMenuItem;
    procedure UpdateButton;
        var
            item: CrowdSimMenuItemType;
        begin
        for item := Low(CrowdSimMenuItemType) to High(CrowdSimMenuItemType) do
            begin
            if item = p_CurrentMenu then
                begin
                p_MenuButtons[item].Color := clWindow;
                p_MenuButtons[item].Font.Style := p_MenuButtons[item].Font.Style + [TFontStyle.fsBold];
                end
            else
                begin
                p_MenuButtons[item].Color := clBtnFace;
                p_MenuButtons[item].Font.Style := p_MenuButtons[item].Font.Style - [TFontStyle.fsBold];
                end;
            end;
        end;

    procedure ShowTab;
        begin
        PageControlClient.ActivePage := p_MenuTabs[p_CurrentMenu];
        end;
    begin
    UpdateButton;
    ShowTab;

    PedestrianMapUser.Visible := p_MeshVisible or ((PageControlClient.ActivePage = p_MenuTabs[_csmiArea])
                                               or (PageControlClient.ActivePage = p_MenuTabs[_csmiLogSettings]));
    CSLogFrame.OnChangeActivate(PageControlClient.ActivePage = p_MenuTabs[_csmiLogSettings]);
    end;

procedure TFormCrowdSimPlayer.OnCloseSidePanel(Sender: TForm);
    var
        i, NwkIdx, NdIdx : integer;
    begin
    if Assigned(p_SidePanel.SimAreaSettingsFrame) then
        begin
        p_PedestrianMapList := p_SidePanel.SimAreaSettingsFrame.MapList;
        p_PedestrianMapList.IsCreateMeshMode := false;
        p_PedestrianMapUser := p_SidePanel.SimAreaSettingsFrame.MapUser;
        p_MeshVisible       := p_SidePanel.SimAreaSettingsFrame.MeshVisible;
        end
    else if Assigned(p_SidePanel.CrossWalkSettingsFrame) then
        begin
        p_PedestrianMapUser := p_SidePanel.CrossWalkSettingsFrame.MapUser;
        if Assigned(p_PedestrianMapUser) then
            begin
            p_PedestrianMapUser.CellEditor.Enabled := false;
            if p_PedestrianMapUser.Enabled then
                p_PedestrianMapUser.Visible := true;
            end;
        end
    else if Assigned(p_SidePanel.WaitingQueueSettingsFrame) then
        begin

        end
    else if Assigned(p_SidePanel.PedestrianPopOutFrame) then
        begin
        p_SidePanel.PedestrianPopOutFrame.SendRenderDispersionData(p_RenderDispersionPointList);
        p_SidePanel.PedestrianPopOutFrame.SetPopOutSettingsData;
        for i := 0 to p_SidePanel.PedestrianPopOutFrame.POPointList.Data.Count - 1 do
            begin
            NwkIdx := p_SidePanel.PedestrianPopOutFrame.POPointList.Data[i].NodeIdx[0];
            NdIdx  := p_SidePanel.PedestrianPopOutFrame.POPointList.Data[i].NodeIdx[1];
            theApplicationServices.project.FlightWayNwk[NwkIdx].Node[NdIdx].SelectedOnCrowdSim := false;
            end;
        end;

    Show;
    end;

procedure TFormCrowdSimPlayer.OnClickBtnSimAreaSettings(Sender: TObject);
    begin
    p_SidePanel.ShowSimAreaPanel(p_PedestrianMapList, p_PedestrianMapUser, p_MeshVisible, p_BuildingList);
    Close;
    end;

procedure TFormCrowdSimPlayer.OnClickBtnCrossWalkSettings(Sender: TObject);
    begin
    p_SidePanel.ShowCrossWalkPanel(p_PedestrianMapUser);
    Close;
    end;

procedure TFormCrowdSimPlayer.OnClickBtnWaitingQueueSettings(Sender: TObject);
    var
        i : integer;
    begin
    p_SidePanel.ShowWaitingQueuePanel(p_WaitingQueueAreaList);
    p_SidePanel.WaitingQueueSettingsFrame.F_WaitAreaSet.WaitingAreaVisible := p_WQAreaVisible;
    p_SidePanel.WaitingQueueSettingsFrame.F_WaitAreaSet.chbVisibleWaitingArea.Checked := p_WQAreaVisible;
    for i := 0 to p_WaitingQueueAreaList.Data.Count - 1 do
        p_SidePanel.WaitingQueueSettingsFrame.F_WaitAreaSet.cbbSelectWaitingArea.Items.Add(p_WaitingQueueAreaList.Data[i].Name);

    Close;
    end;

procedure TFormCrowdSimPlayer.OnClickBtnPedestrianPopOutType(Sender: TObject);
    begin
    p_SidePanel.ShowPedestrianPopOutPanel(p_RenderDispersionPointList);
    Close;
    end;

procedure TFormCrowdSimPlayer.ResetDiagramsData(sender :Tobject);
    begin
    PedestrianMapUser.ResetCrossSectionFlowLog;
    if p_ExportsForm.Showing then
        p_ExportsForm.Close;

    if Assigned(CSLogFrame) then
        CSLogFrame.ChangeEnabledchbLogs(False);
    end;

procedure TFormCrowdSimPlayer.SetController(const Value: F8CrowdSimControllerClass);
    begin
    FrameControlTime1.Controller := Value;
    end;

function TFormCrowdSimPlayer.GetPedestrianMapList: TPedestrianMapList;
    begin
    Result := p_PedestrianMapList;
    end;

procedure TFormCrowdSimPlayer.SetPedestrianMapList(const aValue: TPedestrianMapList);
    begin
    p_PedestrianMapList := aValue;
    end;

function TFormCrowdSimPlayer.GetPedestrianMapUser: TPedestrianMapUser;
    begin
    Result := p_PedestrianMapUser;
    end;

procedure TFormCrowdSimPlayer.SetPedestrianMapUser(const aValue: TPedestrianMapUser);
    begin
    p_PedestrianMapUser := aValue;
    CSLogFrame.PedestrianMapUser := p_PedestrianMapUser;
    end;

function TFormCrowdSimPlayer.GetBuildingList: TCrowdSimBuildingList;
    begin
    Result := p_BuildingList;
    end;

procedure TFormCrowdSimPlayer.SetBuildingList(const aValue: TCrowdSimBuildingList);
    begin
    if Assigned(p_BuildingList) then
        p_BuildingList := nil;

    p_BuildingList := aValue;
    end;

function TFormCrowdSimPlayer.GetWQAreaList: TWaitingQueueAreaList;
    begin
    Result := p_WaitingQueueAreaList;
    end;

procedure TFormCrowdSimPlayer.SetWQAreaList(const aValue: TWaitingQueueAreaList);
    begin
    p_WaitingQueueAreaList := aValue;
    end;

function TFormCrowdSimPlayer.GetRenderDispersionPointList: TPopOutPointList;
    begin
    Result := p_RenderDispersionPointList;
    end;

procedure TFormCrowdSimPlayer.SetRenderDispersionPointList(const aValue: TPopOutPointList);
    begin
    p_RenderDispersionPointList := aValue;
    end;

function TFormCrowdSimPlayer.GetPedLblList: TList<TPedestrianData>;
    begin
    Result := p_AllPedestrianLabelList;
    end;

procedure TFormCrowdSimPlayer.SetPedLblList(const aValue: TList<TPedestrianData>);
    begin
    p_AllPedestrianLabelList := aValue;
    end;

function TFormCrowdSimPlayer.GetMovingList: TPedestrianMovingList;
    begin
    Result := p_PedestrianMovingList;
    end;

procedure TFormCrowdSimPlayer.SetMovingList(const aValue: TPedestrianMovingList);
    begin
    p_PedestrianMovingList := aValue;
    end;

function TFormCrowdSimPlayer.GetIsInOutNodeList: TList<TNodeNumArray>;
    begin
    Result := p_IsInOutNodeList;
    end;

procedure TFormCrowdSimPlayer.SetIsInOutNodeList(const aValue: TList<TNodeNumArray>);
    begin
    p_IsInOutNodeList := aValue;
    end;

function TFormCrowdSimPlayer.GetCrosswalkInReplayList: TCrosswalkInReplayList;
    begin
    Result := p_CrosswalkInReplayList;
    end;

procedure TFormCrowdSimPlayer.SetCrosswalkInReplayList(const aValue: TCrosswalkInReplayList);
    begin
    p_CrosswalkInReplayList := aValue;
    end;

function TFormCrowdSimPlayer.GetPedestrianProfileOptionList: TPedestrianProfileOptionList;
    begin
    result := p_PedestrianProfileOptionList;
    end;

procedure TFormCrowdSimPlayer.SetPedestrianProfileOptionList(const aValue: TPedestrianProfileOptionList);
    begin
    p_PedestrianProfileOptionList := aValue;
    end;

function TFormCrowdSimPlayer.GetPedestrianCategoryList: TPedestrianCategoryList;
    begin
    result := p_PedestrianCategoryList;
    end;

procedure TFormCrowdSimPlayer.SetPedestrianCategoryList(const aValue: TPedestrianCategoryList);
    begin
    p_PedestrianCategoryList := aValue;
    end;

procedure TFormCrowdSimPlayer.SetSimBeginningTime(sender : Tobject);
    begin
    PedestrianMapUser.PlayStart;
    PedestrianMovingList.ClearOldData;
    if FrameControlTime1.rdRain.Checked then
        begin
        theApplicationServices.visualOptionsRoot.displayOption[_Weather] := True;
        theApplicationServices.visualOptionsRoot.weather := _Raining;
        if not theApplicationServices.mainForm.IsEnvironmentStarted then
            theApplicationServices.mainForm.StartEnvironment;
        end
    else
        begin
        theApplicationServices.visualOptionsRoot.displayOption[_Weather] := False;
        theApplicationServices.GetVisualOptionsRoot.displayOption[_RainOnWindshield] := false;
        end;
    end;

procedure TFormCrowdSimPlayer.TimerUIUpdateTimer(Sender: TObject);
    var
        networkExisted: Boolean;
        onSimulation: Boolean;
    begin
    networkExisted := IsNetworkExisted;
    onSimulation := Assigned(controller) and (controller.currentState = csStop);

    MainInputMenuFrame.BtnSimAreaSettings.Enabled      := onSimulation;
    MainInputMenuFrame.BtnCrossWalkSettings.Enabled    := onSimulation and networkExisted;
    MainInputMenuFrame.BtnWaitingQueueSettings.Enabled := onSimulation and networkExisted;
    MainInputMenuFrame.BtnPedestrianPopOutType.Enabled := onSimulation and networkExisted;
    end;
end.
