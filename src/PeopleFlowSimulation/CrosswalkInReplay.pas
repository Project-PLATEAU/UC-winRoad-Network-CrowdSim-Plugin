unit CrosswalkInReplay;

interface

uses
    Winapi.Windows,
    System.SysUtils,
    System.DateUtils,
    System.Types,
    System.Classes,
    System.Generics.Collections,
    System.Math,
    PluginCore,
    F8GLUtils,
    SimulationInputUtils;

type
    TCrosswalkInReplay = class
        private
            FGetUpdatedTime  : TGetUpdatedtime;
            p_Idx            : TPathNumArray;
            p_SignalInterval : integer;
            p_RedSignalInterval : integer;
            p_SigStartTime   : TDateTime;
            p_InitSignal     : NetworkLinkType;
            p_API            : IF8ApplicationServices;
            p_IsReplay       : boolean;

            procedure   ChangeSignal(dTimeInSeconds : Double);
        public
            constructor Create(const idx: TPathNumArray; interval: integer; intervalRed: integer; nltype: NetworkLinkType);
            procedure   AfterConstruction; override;
            procedure   BeforeDestruction; override;
            procedure   RegisterGetUpdatedtime(value : TGetUpdatedtime);
            procedure   UnRegisterGetUpdatedtime;

            property    Idx            : TPathNumArray   read p_Idx write p_Idx;
            property    SignalInterval : integer         read p_SignalInterval write p_SignalInterval;
            property    RedSignalInterval : integer      read p_RedSignalInterval write p_RedSignalInterval;
            property    SigStartTime   : TDateTime       read p_SigStartTime write p_SigStartTime;
            property    InitSignal     : NetworkLinkType read p_InitSignal write p_InitSignal;
            property    IsReplay       : boolean         read p_IsReplay write p_IsReplay;
        end;

    TCrosswalkInReplayList = class
        private
            FGetUpdatedTime : TGetUpdatedtime;
            p_CrosswalkList : TList<TCrosswalkInReplay>;
            p_IsReplay      : boolean;
        public
            procedure   AfterConstruction; override;
            procedure   BeforeDestruction; override;
            procedure   RegisterGetUpdatedtime(value : TGetUpdatedtime);
            procedure   UnRegisterGetUpdatedtime;
            procedure   BeforeReplay(const starttime: TDateTime);
            procedure   AfterReplay;

            property    CrosswalkList : TList<TCrosswalkInReplay> read p_CrosswalkList write p_CrosswalkList;
            property    IsReplay      : boolean                   read p_IsReplay write p_IsReplay;
        end;

implementation

{ TCrosswalkInReplay }
constructor TCrosswalkInReplay.Create(const idx: TPathNumArray; interval: integer; intervalRed: integer; nltype: NetworkLinkType);
    begin
    p_Idx            := idx;
    p_SignalInterval := interval;
    p_RedSignalInterval := intervalRed;
    p_InitSignal     := nltype;
    p_API            := theApplicationServices;
    end;

procedure TCrosswalkInReplay.AfterConstruction;
    var
        m : TMethod;
    begin
    inherited;
    TransientWorldAfterMoveProc(m) := ChangeSignal;
    p_API.RegisterEventHandler(_plgTransientWorldAfterMove, m);
    end;

procedure TCrosswalkInReplay.BeforeDestruction;
    var
        m : TMethod;
    begin
    inherited;
    TransientWorldAfterMoveProc(m) := ChangeSignal;
    p_API.UnRegisterEventHandler(_plgTransientWorldAfterMove, m);
    end;

procedure TCrosswalkInReplay.RegisterGetUpdatedtime(value: TGetUpdatedtime);
    begin
    FGetUpdatedTime := value;
    end;

procedure TCrosswalkInReplay.UnRegisterGetUpdatedtime;
    begin
    FGetUpdatedTime := nil;
    end;

procedure TCrosswalkInReplay.ChangeSignal(dTimeInSeconds : Double);
    var
        now        : TDateTime;
        tmpSigTime : TDateTime;
    begin
    if not IsReplay then
        Exit;

    now        := FGetUpdatedTime;
    tmpSigTime := p_SigStartTime;
    if ((p_API.project.FlightWayNwk[Idx[0]].Link[Idx[1]].LinkType = _nlCrosswalkBlue)
        and (CompareDateTime(now, IncSecond(tmpSigTime, p_SignalInterval)) = GreaterThanValue))
        or ((p_API.project.FlightWayNwk[Idx[0]].Link[Idx[1]].LinkType = _nlCrosswalkRed)
        and (CompareDateTime(now, IncSecond(tmpSigTime, p_RedSignalInterval)) = GreaterThanValue)) then
        begin
        if p_API.project.FlightWayNwk[Idx[0]].Link[Idx[1]].LinkType = _nlCrosswalkBlue then
            p_API.project.FlightWayNwk[Idx[0]].Link[Idx[1]].LinkType := _nlCrosswalkRed
        else if p_API.project.FlightWayNwk[Idx[0]].Link[Idx[1]].LinkType = _nlCrosswalkRed then
            p_API.project.FlightWayNwk[Idx[0]].Link[Idx[1]].LinkType := _nlCrosswalkBlue;

        p_SigStartTime := now;
        end
    else if CompareDateTime(now, tmpSigTime) = LessThanValue then //Šª‚«–ß‚µ
        begin
        if p_API.project.FlightWayNwk[Idx[0]].Link[Idx[1]].LinkType = _nlCrosswalkBlue then
            p_API.project.FlightWayNwk[Idx[0]].Link[Idx[1]].LinkType := _nlCrosswalkRed
        else if p_API.project.FlightWayNwk[Idx[0]].Link[Idx[1]].LinkType = _nlCrosswalkRed then
            p_API.project.FlightWayNwk[Idx[0]].Link[Idx[1]].LinkType := _nlCrosswalkBlue;

        if p_API.project.FlightWayNwk[Idx[0]].Link[Idx[1]].LinkType = _nlCrosswalkBlue then
            p_SigStartTime := IncSecond(now, -p_SignalInterval)
        else
            p_SigStartTime := IncSecond(now, -p_RedSignalInterval)
        end;
    end;

{ TCrosswalkInReplayList }
procedure TCrosswalkInReplayList.AfterConstruction;
    begin
    inherited;
    p_CrosswalkList := TList<TCrosswalkInReplay>.Create;
    end;

procedure TCrosswalkInReplayList.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_CrosswalkList);
    end;

procedure TCrosswalkInReplayList.RegisterGetUpdatedtime(value : TGetUpdatedtime);
    begin
    FGetUpdatedTime := value;
    end;

procedure TCrosswalkInReplayList.UnRegisterGetUpdatedtime;
    begin
    FGetUpdatedTime := nil;
    end;

procedure TCrosswalkInReplayList.BeforeReplay(const starttime: TDateTime);
    var
        i : integer;
    begin
    if (p_IsReplay) or (p_CrosswalkList.Count = 0) or (not Assigned(FGetUpdatedTime)) then
        Exit;

    for i := 0 to p_CrosswalkList.Count - 1 do
        begin
        p_CrosswalkList[i].RegisterGetUpdatedtime(FGetUpdatedTime);
        p_CrosswalkList[i].SigStartTime := starttime;
        p_CrosswalkList[i].IsReplay     := true;
        end;

    p_IsReplay := true;
    end;

procedure TCrosswalkInReplayList.AfterReplay;
    var
        i : integer;
    begin
    if (not p_IsReplay) or (p_CrosswalkList.Count = 0) or (not Assigned(FGetUpdatedTime)) then
        Exit;

    for i := 0 to p_CrosswalkList.Count - 1 do
        begin
        p_CrosswalkList[i].UnRegisterGetUpdatedtime;
        p_CrosswalkList[i].IsReplay := false;
        end;

    p_IsReplay := false;
    end;
end.
