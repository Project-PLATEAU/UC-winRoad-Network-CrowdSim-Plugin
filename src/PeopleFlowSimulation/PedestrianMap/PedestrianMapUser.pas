unit PedestrianMapUser;

interface

uses
    System.Classes,
    System.Generics.Collections,
    XML.XMLIntf,
    PluginCore,
    F8OpenGL,
    F8Utils,
    F8CrowdSimController,
    CrowdSimLogExport,
    PedestrianMap,
    PedestrianMapEditor,
    CrosswalkEditingAreaRenderer;

type
    OnChangedUserPedestrianMapEventNotify = procedure of Object;

    /// <summary>
    ///    特定の歩行領域マップを使用/編集するクラス
    /// </summary>
    TPedestrianMapUser = class
        private
            p_Map: TPedestrianMap;

            p_API: IF8ApplicationServices;

            p_CellEditor: TPedestrianMapEditor;
            p_Controller: F8CrowdSimControllerClass;
            p_CrossSectionFlowLog: TLogCrossSectionFlow;
            p_CrosswalkEditingAreaRenderer: TCrosswalkEditingAreaRenderer;

            p_OnChangedUserPedestrianMapEvents: TList<TMethod>;

            p_Visible: Boolean;

            procedure SetMap(const aValue: TPedestrianMap);
            procedure SetVisible(const aValue: Boolean);
            function  GetMap: TPedestrianMap;
            function  GetCrossSectionFlowLog: TLogCrossSectionFlow;
            function  GetCellEditor: TPedestrianMapEditor;
            function  GetEnabled: Boolean;

            procedure OnTimeStep(dTimeInSeconds: Double);
            procedure OnChangedUserPedestrianMapEvent;
        public
            constructor Create(const aAPI: IF8ApplicationServices; const aController: F8CrowdSimControllerClass);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure ExportRoadMap(const aStrList: TStringList);
            procedure ExportVpsJson(const aDir: String);

            procedure OnChangeActiveMapIndex(const aActiveMap: TPedestrianMap);
            procedure PlayStart;
            procedure CollectCSFlowData;
            procedure ExportCrossSectionLog;
            procedure UpdateUnderMouseGuide(const aPoint: TPoint3D);
            procedure ResetCrossSectionFlowLog;

            procedure Render(const aOpenGL: TF8OpenGL);

            procedure RegisterOnChangeUserPedestrianMapEvent(const aEvent: OnChangedUserPedestrianMapEventNotify);
            procedure UnRegisterOnChangeUserPedestrianMapEvent(const aEvent: OnChangedUserPedestrianMapEventNotify);

            procedure ExportToPluginData;
            procedure ImportFromPluginData;

            property  Map                : TPedestrianMap          read GetMap                 write SetMap;
            property  CrossSectionFlowLog: TLogCrossSectionFlow    read GetCrossSectionFlowLog;
            property  CellEditor         : TPedestrianMapEditor    read GetCellEditor;
            property  Enabled            : Boolean                 read GetEnabled;
            property  Visible            : Boolean                 read p_Visible              write SetVisible;
            property  CrosswalkEditingAreaRenderer: TCrosswalkEditingAreaRenderer read p_CrosswalkEditingAreaRenderer;
        end;

implementation

uses
    System.SysUtils;

{ TPedestrianMapUser }
constructor TPedestrianMapUser.Create(const aAPI: IF8ApplicationServices; const aController: F8CrowdSimControllerClass);
    begin
    p_API := aAPI;
    p_Controller := aController;
    Assert(Assigned(p_API));
    end;

procedure TPedestrianMapUser.AfterConstruction;
    var
        m: TMethod;
    begin
    inherited;

    p_Map := nil;
    p_Visible := False;
    p_CellEditor := TPedestrianMapEditor.Create(p_API);
    p_CrossSectionFlowLog := TLogCrossSectionFlow.Create(p_API);
    p_CrosswalkEditingAreaRenderer := TCrosswalkEditingAreaRenderer.Create;

    p_OnChangedUserPedestrianMapEvents := TList<TMethod>.Create;

    TimeStepProc(m) := OnTimeStep;
    p_API.RegisterEventHandler(_plgTimeStep, m);
    end;

procedure TPedestrianMapUser.BeforeDestruction;
    var
        m: TMethod;
    begin
    inherited;

    TimeStepProc(m) := OnTimeStep;
    p_API.UnRegisterEventHandler(_plgTimeStep, m);

    FreeAndNil(p_OnChangedUserPedestrianMapEvents);

    FreeAndNil(p_CrosswalkEditingAreaRenderer);
    p_Map := nil;
    p_Controller := nil;
    FreeAndNil(p_CellEditor);
    FreeAndNil(p_CrossSectionFlowLog);
    end;

procedure TPedestrianMapUser.ExportRoadMap(const aStrList: TStringList);
    begin
    if not Enabled then
        Exit;

    p_Map.ExportRoadMap(aStrList);
    end;

procedure TPedestrianMapUser.ExportVpsJson(const aDir: String);
    begin
    if not Enabled then
        Exit;

    TPedestrianMapJSONData.ExportToJSONFile(p_Map, aDir);
    end;

procedure TPedestrianMapUser.OnChangeActiveMapIndex(const aActiveMap: TPedestrianMap);
    begin
    Map := aActiveMap;
    end;

procedure TPedestrianMapUser.PlayStart;
    begin
    if not Enabled then
        Exit;

    p_CrossSectionFlowLog.PlayStart(p_controller.currentTime);
    end;

procedure TPedestrianMapUser.CollectCSFlowData;
    begin
    if not Enabled then
        Exit;

    p_CrossSectionFlowLog.CollectData(p_controller);
    end;

procedure TPedestrianMapUser.ExportCrossSectionLog;
    begin
    if not Enabled then
        Exit;

    p_CrossSectionFlowLog.ExportLog(p_controller.currentTime, True);
    end;

procedure TPedestrianMapUser.UpdateUnderMouseGuide(const aPoint: TPoint3D);
    begin
    if not Enabled then
        Exit;

    p_CellEditor.UpdateUnderMouseGuide(aPoint);
    end;

procedure TPedestrianMapUser.ResetCrossSectionFlowLog;
    begin
    if not Enabled then
        Exit;

    p_CrossSectionFlowLog.Map := p_Map;
    end;

procedure TPedestrianMapUser.Render(const aOpenGL: TF8OpenGL);
    begin
    p_CrosswalkEditingAreaRenderer.DoRender(aOpenGL);
    if not (Enabled and Visible) then
        Exit;

    p_Map.DoRender(aOpenGL);
    p_CellEditor.DoEditingGuideRender(aOpenGL);
    end;

procedure TPedestrianMapUser.RegisterOnChangeUserPedestrianMapEvent(const aEvent: OnChangedUserPedestrianMapEventNotify);
    var
        m: TMethod;
    begin
    OnChangedUserPedestrianMapEventNotify(m) := aEvent;
    if not p_OnChangedUserPedestrianMapEvents.Contains(m) then
        p_OnChangedUserPedestrianMapEvents.Add(m);
    end;

procedure TPedestrianMapUser.UnRegisterOnChangeUserPedestrianMapEvent(const aEvent: OnChangedUserPedestrianMapEventNotify);
    var
        m: TMethod;
    begin
    OnChangedUserPedestrianMapEventNotify(m) := aEvent;
    p_OnChangedUserPedestrianMapEvents.Remove(m);
    end;

procedure TPedestrianMapUser.SetMap(const aValue: TPedestrianMap);
    begin
    p_Map := aValue;

    p_CellEditor.EditMap := p_Map;
    p_CrossSectionFlowLog.Map := p_Map;

    p_CrossSectionFlowLog.ClearSensorAreas;
    Visible := Assigned(p_Map);

    OnChangedUserPedestrianMapEvent;
    end;

procedure TPedestrianMapUser.SetVisible(const aValue: Boolean);
    begin
    p_Visible := aValue;
    end;

function TPedestrianMapUser.GetMap: TPedestrianMap;
    begin
    Result := p_Map;
    end;

function TPedestrianMapUser.GetCrossSectionFlowLog: TLogCrossSectionFlow;
    begin
    Result := p_CrossSectionFlowLog;
    end;

function TPedestrianMapUser.GetCellEditor: TPedestrianMapEditor;
    begin
    Result := p_CellEditor;
    end;

function TPedestrianMapUser.GetEnabled: Boolean;
    begin
    Result := Assigned(Map);
    end;

procedure TPedestrianMapUser.OnTimeStep(dTimeInSeconds: Double);
    begin
    if not Enabled then
        Exit;

    if not Assigned(p_Controller) then
        Exit;

    if p_Controller.CurrentState = csStop then
        Exit;

    p_Map.OnTimeStep(dTimeInSeconds, Trunc(p_Controller.dTime));
    end;

procedure TPedestrianMapUser.OnChangedUserPedestrianMapEvent;
    var
        m: TMethod;
    begin
    for m in p_OnChangedUserPedestrianMapEvents do
        OnChangedUserPedestrianMapEventNotify(m);
    end;

procedure TPedestrianMapUser.ExportToPluginData;
    begin
    CrossSectionFlowLog.ExportToPluginData;
    end;

procedure TPedestrianMapUser.ImportFromPluginData;
    begin
    CrossSectionFlowLog.ImportFromPluginData;
    end;
end.

