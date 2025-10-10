unit F8CrowdSimController;

interface

uses
    System.SyncObjs,
    System.Types,
    System.SysUtils,
    System.DateUtils,
    PluginCore,
    MovingFeature,
    PedestrianMovingData,
    CrosswalkInReplay;

type
    CrowdSimPlayerStateType = (csStop, csPlay, csPause, csFast, csBack);

    /// <summary>
    ///    シミュレーションの再生時刻や再生状況を管理するクラス
    /// </summary>
    F8CrowdSimControllerClass = class
        private
            p_application   : IF8ApplicationServices;
            p_stateCS       : TCriticalSection;
            p_currentTime   : TDateTime;
            p_playingSpeed  : Single;
            p_state         : CrowdSimPlayerStateType;
            p_IsRain        : Boolean;
            p_dtime         : double;
            p_StartSimTime  : TDateTime;
            p_EndSimTime    : TDateTime;
            p_CalculateTime : TDateTime;
            p_IsReplaying   : Boolean;

            function    GetPlayingSpeed: Single;
            procedure   SetPlayingSpeed(const Value: Single);
            function    GetcurrentTime: TDateTime;
            procedure   SetIsRain(const Value: Boolean);
            function   GetState: CrowdSimPlayerStateType;
            function    GetCalculateTime: TDateTime;

        public
            constructor Create(const application: IF8ApplicationServices; aPedestrianMovingList: TPedestrianMovingList; aCrosswalkInReplayList: TCrosswalkInReplayList);
            procedure   AfterConstruction; override;
            procedure   BeforeDestruction; override;
            procedure   Play;
            procedure   Pause;
            procedure   Stop;
            procedure   Reset;
            procedure   Replay;

            function    CheckTheState: CrowdSimPlayerStateType;
            procedure   UpdateTimer(const dTimeInSeconds : Double);
            procedure   AssignCurrentTime(const time : TDateTime);
            procedure   AssignSimlationTime(simStart,simEnd : TDateTime);

            property    playingSpeed    : Single read GetPlayingSpeed write SetPlayingSpeed;
            property    application     : IF8ApplicationServices read p_application;
            property    currentState    : CrowdSimPlayerStateType read GetState;
            property    currentTime     : TDateTime read GetcurrentTime;
            property    IsRain          : Boolean   read p_IsRain write SetIsRain;
            property    dTime           : double    read p_dTime;
            property    StartSimTime    : TDateTime read p_StartSimTime;
            property    EndSimTime      : TDateTime read p_EndSimTime;
            property    CalculateTime   : TDateTime read GetCalculateTime;
        end;

implementation

uses
    AgentSettings;

{ F8CrowdSimControllerClass }
constructor F8CrowdSimControllerClass.Create(const application: IF8ApplicationServices; aPedestrianMovingList: TPedestrianMovingList; aCrosswalkInReplayList: TCrosswalkInReplayList);
    begin
    p_application := application;
    p_playingSpeed := 1.0;
    p_currentTime := MovingFeatureListClass.beginningTime;
    MovingFeatureListClass.RegisterGetUpdatedTime(GetcurrentTime);
    aPedestrianMovingList.RegisterGetUpdatedTime(GetCalculateTime);
    aCrosswalkInReplayList.RegisterGetUpdatedTime(GetCalculateTime);
    p_dtime := 0;
    p_StartSimTime  := now;
    p_EndSimTime    := now;
    p_IsReplaying   := false;
    end;

procedure F8CrowdSimControllerClass.AfterConstruction;
    begin
    inherited;
    p_stateCS       := TCriticalSection.Create;
    end;

procedure F8CrowdSimControllerClass.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_stateCS);
    end;

procedure F8CrowdSimControllerClass.AssignCurrentTime(const time: TDateTime);
    begin
    if p_state =csStop then
        begin
        p_currentTime := time;
        p_CalculateTime := time;
        end;
    end;

procedure F8CrowdSimControllerClass.AssignSimlationTime(simStart,simEnd: TDateTime);
    begin
    p_StartSimTime := simStart;
    p_EndSimTime := simEnd;
    end;

function F8CrowdSimControllerClass.CheckTheState: CrowdSimPlayerStateType;
    begin
    p_stateCS.Enter;
    try
        if p_application.mainForm.IsTrafficStarted then
            begin
            if p_application.mainForm.IsTrafficPaused then
                begin
                p_state := csPause;
                end
            else if p_playingSpeed = 1.0 then
                begin
                p_state := csPlay;
                end
            else if p_playingSpeed > 0.0 then
                begin
                p_state := csFast;
                end
            else
                begin
                p_state := csBack;
                end;
            end
        else
            begin
            p_state := csStop;

            if p_application.mainForm.IsTrafficReset then
                if CompareDateTime(currentTime, MovingFeatureListClass.beginningTime) <> EqualsValue then
                    Reset;
            end;
        Result := p_state;
    finally
        p_stateCS.Leave;
        end;
    end;

function F8CrowdSimControllerClass.GetcurrentTime: TDateTime;
    begin
    Result := p_currentTime;
    end;

function F8CrowdSimControllerClass.GetCalculateTime: TDateTime;
    begin
    Result := p_CalculateTime;
    end;

function F8CrowdSimControllerClass.GetPlayingSpeed: Single;
    begin
    Result := p_playingSpeed;
    end;

function F8CrowdSimControllerClass.GetState: CrowdSimPlayerStateType;
    begin
    p_StateCS.Enter;
    try
        Result := p_State;
    finally
        p_StateCS.Leave;
        end;
    end;

procedure F8CrowdSimControllerClass.Pause;
    begin
    p_stateCS.Enter;
    try
        if p_state = csPause then
            begin
            if p_playingSpeed =1.0 then
                begin
                p_state := csPlay;
                p_application.mainForm.ResumeTraffic;
                end
            else if (p_playingSpeed > 0.0)  then
                begin
                p_state := csFast;
                p_application.mainForm.ResumeTraffic;
                end
            else
                begin
                p_state := csBack;
                p_application.mainForm.ResumeTraffic;
                end;
            end
        else
            begin
            p_state := csPause;
            p_application.mainForm.PauseTraffic;
            end;
    finally
        p_stateCS.Leave;
        end;
    end;

procedure F8CrowdSimControllerClass.Play;
    begin
    p_stateCS.Enter;
    try
    if p_state = csPause then
            begin
            p_state := csStop;
            p_application.mainForm.ResumeTraffic;
            p_application.mainForm.StopTraffic;
            end
        else if p_playingSpeed =1.0 then
            begin
            p_application.project.DateTime := p_currentTime;
            p_StartSimTime := p_currentTime;
            p_state := csPlay;
            p_application.mainForm.StartTraffic;
            end
        else if p_playingSpeed >0.0 then
            begin
            p_application.project.DateTime := p_currentTime;
            p_StartSimTime := p_currentTime;
            p_state := csFast;
            p_application.mainForm.StartTraffic;
            end
        else
            begin
            p_application.project.DateTime := p_currentTime;
            p_StartSimTime := p_currentTime;
            p_state := csBack;
            p_application.mainForm.StartTraffic;
            end;
        finally
        p_stateCS.Leave;
    end;

    p_dTime :=MilliSecondSpan(p_currentTime,p_StartSimTime)/1000;
    if CompareDateTime(p_currentTime,p_StartSimTime) = LessThanValue then
        p_dTime := -1*p_dTime;

    if p_IsRain then
        begin
        p_application.visualOptionsRoot.displayOption[_Weather] := True;
        p_application.visualOptionsRoot.weather := _Raining;
        if not p_application.mainForm.IsEnvironmentStarted then
            p_application.mainForm.StartEnvironment;
        end
    else
        begin
        p_application.visualOptionsRoot.displayOption[_Weather] := False;
        p_application.GetVisualOptionsRoot.displayOption[_RainOnWindshield] := false;
        end;
    end;

procedure F8CrowdSimControllerClass.Replay;
    begin
    p_stateCS.Enter;
    try
    if p_state = csPause then
            begin
            p_state := csStop;
            p_application.mainForm.ResumeTraffic;
            p_application.mainForm.StopTraffic;
            end
        else if p_playingSpeed =1.0 then
            begin
            p_IsReplaying := true;
            p_application.project.DateTime := p_currentTime;
            p_state := csPlay;
            p_application.mainForm.StartTraffic;
            end
        else if p_playingSpeed >0.0 then
            begin
            p_IsReplaying := true;
            p_application.project.DateTime := p_currentTime;
            p_state := csFast;
            p_application.mainForm.StartTraffic;
            end
        else
            begin
            p_IsReplaying := true;
            p_application.project.DateTime := p_currentTime;
            p_state := csBack;
            p_application.mainForm.StartTraffic;
            end;
        finally
        p_stateCS.Leave;
    end;

    p_dTime :=MilliSecondSpan(p_currentTime,p_StartSimTime)/1000;
    if CompareDateTime(p_currentTime,p_StartSimTime) = LessThanValue then
        p_dTime := -1*p_dTime;

    if p_IsRain then
        begin
        p_application.visualOptionsRoot.displayOption[_Weather] := True;
        p_application.visualOptionsRoot.weather := _Raining;
        if not p_application.mainForm.IsEnvironmentStarted then
            p_application.mainForm.StartEnvironment;
        end
    else
        begin
        p_application.visualOptionsRoot.displayOption[_Weather] := False;
        p_application.GetVisualOptionsRoot.displayOption[_RainOnWindshield] := false;
        end;
    end;

procedure F8CrowdSimControllerClass.Reset;
    begin
    Assert(p_state = csStop, '停止中でないとリセットできません。');
    p_stateCS.Enter;
    try
    if p_state = csStop then
        begin
        p_currentTime := MovingFeatureListClass.beginningTime;
        MovingFeatureListClass.Reset(p_application.project);
        if not p_application.mainForm.IsTrafficReset then
            p_application.mainForm.ResetTraffic;
        end;
    finally
        p_stateCS.Leave;
        end;
    end;

procedure F8CrowdSimControllerClass.SetIsRain(const Value: Boolean);
    var
        settings : TAgentSettingsType;
    begin
    p_IsRain := Value;
    settings := LoadAgentSettings;
    settings.IsRain := Value;
    SaveAgentSettings(settings)
    end;

procedure F8CrowdSimControllerClass.SetPlayingSpeed(const Value: Single);
    begin
    p_playingSpeed := Value;
    end;

procedure F8CrowdSimControllerClass.Stop;
    begin
    p_stateCS.Enter;
    try
        p_state := csStop;
        p_application.mainForm.StopTraffic;
        if p_IsReplaying then
            p_IsReplaying := false;
    finally
        p_stateCS.Leave;
        end;
    end;

procedure F8CrowdSimControllerClass.UpdateTimer(const dTimeInSeconds: Double);
    begin
    if p_application.mainForm.IsTrafficPaused then
        Exit;

    p_CalculateTime := IncMilliSecond(p_CalculateTime, Round(dTimeInSeconds * 1000 * p_playingSpeed));
    if (p_application.mainForm.IsTrafficStarted) and (p_IsReplaying) then
        begin
        p_currentTime := p_CalculateTime;
        p_application.project.DateTime := p_currentTime;
        end
    else
        p_currentTime := p_application.project.DateTime;

    p_dTime :=MilliSecondSpan(p_currentTime,p_StartSimTime)/1000;
    if CompareDateTime(p_currentTime,p_StartSimTime) = LessThanValue then
        p_dTime := -1*p_dTime;
    end;
end.
