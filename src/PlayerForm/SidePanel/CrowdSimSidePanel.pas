unit CrowdSimSidePanel;

interface

uses
    System.Generics.Collections,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.Controls,
    PluginCore,
    F8SingleDockPanel,
    PedestrianMapList,
    PedestrianMapUser,
    PedestrianUtil,
    WaitingQueueAreaList,
    PopOutPointList,
    PedestrianCategoryData,
    PedestrianProfileOptionData,
    CrowdSimBuildingList,
    F_SimAreaSettingsSidePanel,
    F_CrossWalkSettingsSidePanel,
    F_WaitingQueueSidePanel,
    F_PedestrianPopOutSidePanel;

type
    TCrowdSimSidePanel = class
        private
            p_DockForm: TF8CustomDockForm;

            p_SimAreaSettingsFrame:      TFrameSimAreaSettingsSidePanel;
            p_CrossWalkSettingsFrame:    TFrameCrossWalkSettingsSidePanel;
            p_WaitingQueueSettingsFrame: TFrameWaitingQueueSidePanel;
            p_PedestrianPopOutFrame:     TFramePedestrianPopOutSidePanel;

            FOnDockFormClose: TDockCallback;
            procedure DoOnDockFormClose(Sender: TForm);

        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure ShowSimAreaPanel(const aPedestrianMapList: TPedestrianMapList;
                                       const aPedestrianMapUser: TPedestrianMapUser; const aMeshVisible: boolean;
                                       const aBuildingList: TCrowdSimBuildingList);
            procedure ShowCrossWalkPanel(const aPedestrianMapUser: TPedestrianMapUser);
            procedure ShowWaitingQueuePanel(const aWQList: TWaitingQueueAreaList);
            procedure ShowPedestrianPopOutPanel(const aRenderDPList: TPopOutPointList);
            procedure ClosePanel;
            procedure PopOutCloseQuery(Sender: TObject; var CanClose: Boolean);

            property  OnDockFormClose: TDockCallback read FOnDockFormClose write FOnDockFormClose;
            property  SimAreaSettingsFrame:      TFrameSimAreaSettingsSidePanel   read p_SimAreaSettingsFrame;
            property  CrossWalkSettingsFrame:    TFrameCrossWalkSettingsSidePanel read p_CrossWalkSettingsFrame;
            property  WaitingQueueSettingsFrame: TFrameWaitingQueueSidePanel      read p_WaitingQueueSettingsFrame;
            property  PedestrianPopOutFrame:     TFramePedestrianPopOutSidePanel  read p_PedestrianPopOutFrame;
        end;

implementation

uses
    System.SysUtils;

{ TCrowdSimSidePanel }
procedure TCrowdSimSidePanel.AfterConstruction;
    begin
    inherited;
    p_SimAreaSettingsFrame      := nil;
    p_CrossWalkSettingsFrame    := nil;
    p_WaitingQueueSettingsFrame := nil;
    p_PedestrianPopOutFrame     := nil;
    end;

procedure TCrowdSimSidePanel.BeforeDestruction;
    begin
    inherited;

    ClosePanel;
    end;

procedure TCrowdSimSidePanel.ShowSimAreaPanel(const aPedestrianMapList: TPedestrianMapList;
    const aPedestrianMapUser: TPedestrianMapUser; const aMeshVisible: boolean; const aBuildingList: TCrowdSimBuildingList);
    var
        mainForm: IF8MainForm;
    begin
    if not Assigned(p_DockForm) then
        begin
        p_DockForm := CreateDockableFrameEX(TFrameSimAreaSettingsSidePanel, p_SimAreaSettingsFrame);
        p_DockForm.OnDoClose := DoOnDockFormClose;
        end;

    p_SimAreaSettingsFrame.MapList     := aPedestrianMapList;
    p_SimAreaSettingsFrame.MapUser     := aPedestrianMapUser;
    p_SimAreaSettingsFrame.Application := theApplicationServices;
    p_SimAreaSettingsFrame.MeshVisible := aMeshVisible;
    p_SimAreaSettingsFrame.BuildingList := aBuildingList;
    p_SimAreaSettingsFrame.OnFormShow;
    p_DockForm.Show;

    // auto dock
    mainForm := theApplicationServices.mainForm;
    if Assigned(mainForm) then
        begin
        // change dock panel size
        mainForm.DockSiteRight.DockSizeStrategy := dssFitToDockClient;
        mainForm.DockSiteRight.AddDockClient(p_DockForm);
        end;
    end;

procedure TCrowdSimSidePanel.ShowCrossWalkPanel(const aPedestrianMapUser: TPedestrianMapUser);
    var
        mainForm: IF8MainForm;
    begin
    if not Assigned(p_DockForm) then
        begin
        p_DockForm := CreateDockableFrameEX(TFrameCrossWalkSettingsSidePanel, p_CrossWalkSettingsFrame);
        p_DockForm.OnDoClose := DoOnDockFormClose;
        end;

    p_CrossWalkSettingsFrame.MapUser := aPedestrianMapUser;
    if Assigned(p_CrossWalkSettingsFrame.MapUser) then
        p_CrossWalkSettingsFrame.MapUser.CellEditor.Enabled := true;

    p_DockForm.Show;

    // auto dock
    mainForm := theApplicationServices.mainForm;
    if Assigned(mainForm) then
        begin
        // change dock panel size
        mainForm.DockSiteRight.DockSizeStrategy := dssFitToDockClient;
        mainForm.DockSiteRight.AddDockClient(p_DockForm);
        end;
    end;

procedure TCrowdSimSidePanel.ShowWaitingQueuePanel(const aWQList: TWaitingQueueAreaList);
    var
        mainForm: IF8MainForm;
    begin
    if not Assigned(p_DockForm) then
        begin
        p_DockForm := CreateDockableFrameEX(TFrameWaitingQueueSidePanel, p_WaitingQueueSettingsFrame);
        p_DockForm.OnDoClose := DoOnDockFormClose;
        end;

    p_WaitingQueueSettingsFrame.WQAreaList  := aWQList;
    p_WaitingQueueSettingsFrame.OnShow;
    p_DockForm.Show;

    // auto dock
    mainForm := theApplicationServices.mainForm;
    if Assigned(mainForm) then
        begin
        // change dock panel size
        mainForm.DockSiteRight.DockSizeStrategy := dssFitToDockClient;
        mainForm.DockSiteRight.AddDockClient(p_DockForm);
        end;
    end;

procedure TCrowdSimSidePanel.ShowPedestrianPopOutPanel(const aRenderDPList: TPopOutPointList);
    var
        mainForm : IF8MainForm;
    begin
    if not Assigned(p_DockForm) then
        begin
        p_DockForm := CreateDockableFrameEX(TFramePedestrianPopOutSidePanel, p_PedestrianPopOutFrame);
        p_DockForm.OnDoClose := DoOnDockFormClose;
        end;

    p_DockForm.OnCloseQuery := PopOutCloseQuery;
    p_PedestrianPopOutFrame.F_PopOutSet.chbVisibleDispersionPoint.Checked := aRenderDPList.IsVisibleDispersionPoint;
    aRenderDPList.IsVisibleDispersionPoint := false;

    p_PedestrianPopOutFrame.ReceiveLabelDataFromProject;
    p_PedestrianPopOutFrame.ReceivePointDataFromProject;

    p_DockForm.Show;

    // auto dock
    mainForm := theApplicationServices.mainForm;
    if Assigned(mainForm) then
        begin
        // change dock panel size
        mainForm.DockSiteRight.DockSizeStrategy := dssFitToDockClient;
        mainForm.DockSiteRight.AddDockClient(p_DockForm);
        end;
    end;

procedure TCrowdSimSidePanel.ClosePanel;
    begin
    if Assigned(p_DockForm) then
        p_DockForm.Close;
    end;

procedure TCrowdSimSidePanel.DoOnDockFormClose(Sender: TForm);
    begin
    if Assigned(OnDockFormClose) then
        OnDockFormClose(Sender);

    if Assigned(p_SimAreaSettingsFrame) then
        p_SimAreaSettingsFrame.OnFormClose;

    p_DockForm  := nil;
    p_SimAreaSettingsFrame      := nil;
    p_CrossWalkSettingsFrame    := nil;
    p_WaitingQueueSettingsFrame := nil;
    if Assigned(p_PedestrianPopOutFrame) then
        FreeAndNil(p_PedestrianPopOutFrame);
//    if Assigned(p_DockForm) then
//        FreeAndNil(p_DockForm);
//    if Assigned(p_DockForm) then
//        FreeAndNil(p_DockForm);
//    if Assigned(p_SimAreaSettingsFrame) then
//        p_SimAreaSettingsFrame      := nil;
//    if Assigned(p_CrossWalkSettingsFrame) then
//        FreeAndNil(p_CrossWalkSettingsFrame);
//    if Assigned(p_WaitingQueueSettingsFrame) then
//        FreeAndNil(p_WaitingQueueSettingsFrame);
//    if Assigned(p_PedestrianPopOutFrame) then
//        FreeAndNil(p_PedestrianPopOutFrame);
    end;

procedure TCrowdSimSidePanel.PopOutCloseQuery(Sender: TObject; var CanClose: Boolean);
    begin
    if Assigned(p_PedestrianPopOutFrame) then
        begin
        if p_PedestrianPopOutFrame.IsNothingPopOrOut then
            begin
            if MessageDlg('発生地点または退出地点が設定されていません。地点設定に関する編集内容が破棄されますが、画面を閉じますか?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
                CanClose := true
            else
                CanClose := false;
            end;
        end;
    end;
end.
