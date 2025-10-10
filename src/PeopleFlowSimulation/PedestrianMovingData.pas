unit PedestrianMovingData;

interface

uses
    Winapi.Windows,
    System.SysUtils,
    System.DateUtils,
    System.Types,
    System.Classes,
    System.Generics.Collections,
    System.Math,
    System.JSON,
    System.JSON.Readers,
    System.JSON.Builders,
    System.JSON.Serializers,
    PluginCore,
    F8GLUtils,
    SimulationInputUtils;

type
    MoveProc = procedure(dTimeInSeconds : Double; Instance : IF8MovingObjectInstance) of Object;

    TMovingData = record
        time     : TDateTime;
        position : GLPointType;
        angle    : GLPointType;
        end;

    TTrafficGeneratorState = record
        StartTo : boolean;
        EndTo   : boolean;
        end;

    TPedestrianMovingData = class
        private
            FGetUpdatedTime   : TGetUpdatedtime;
            p_CurrentIdx      : integer;
            p_CurrentPosition : GLPointType;
            p_MovingLog       : TList<TMovingData>;
            [weak] p_Instance        : IF8CharacterInstance;
            [weak] p_Model           : IF8QuakeIII;
            p_Age             : integer;
            p_Gender          : integer;
            p_IsReplay        : boolean;
            p_LogInterval     : double;
            procedure Move(dTimeInSeconds : Double; Instance : IF8MovingObjectInstance);
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure RegisterGetUpdatedtime(value : TGetUpdatedtime);
            procedure UnRegisterGetUpdatedtime;
            procedure InstanceCreate;
            procedure InitializeCurrentParams;

            property  CurrentIdx  : integer              read p_CurrentIdx  write p_CurrentIdx;
            property  MovingLog   : TList<TMovingData>   read p_MovingLog   write p_MovingLog;
            property  Instance    : IF8CharacterInstance read p_Instance    write p_Instance;
            property  Model       : IF8QuakeIII          read p_Model       write p_Model;
            property  Age         : integer              read p_Age         write p_Age;
            property  Gender      : integer              read p_Gender      write p_Gender;
            property  IsReplay    : boolean              read p_IsReplay    write p_IsReplay;
            property  LogInterval : double               read p_LogInterval write p_LogInterval;
        end;

    TVehicleMovingData = class
        private
            FGetUpdatedTime   : TGetUpdatedtime;
            p_CurrentIdx      : integer;
            p_CurrentPosition : GLPointType;
            p_MovingLog       : TList<TMovingData>;
            [weak] p_Instance        : IF8MovingObjectInstance;
            [weak] p_Model           : IF8ThreeDeeStudio;
            p_ID              : integer;
            p_IsReplay        : boolean;
            p_LogInterval     : double;
            procedure Move(dTimeInSeconds : Double; Instance : IF8MovingObjectInstance);
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure RegisterGetUpdatedtime(value : TGetUpdatedtime);
            procedure UnRegisterGetUpdatedtime;
            procedure InstanceCreate;
            procedure InitializeCurrentParams;

            property  CurrentIdx  : integer                 read p_CurrentIdx  write p_CurrentIdx;
            property  MovingLog   : TList<TMovingData>      read p_MovingLog   write p_MovingLog;
            property  Instance    : IF8MovingObjectInstance read p_Instance    write p_Instance;
            property  Model       : IF8ThreeDeeStudio       read p_Model       write p_Model;
            property  ID          : integer                 read p_ID          write p_ID;
            property  IsReplay    : boolean                 read p_IsReplay    write p_IsReplay;
            property  LogInterval : double                  read p_LogInterval write p_LogInterval;
        end;

    TPedestrianMovingList = class
        private
            FGetUpdatedTime : TGetUpdatedtime;
            p_List          : TObjectList<TPedestrianMovingData>;
            p_IDList        : TList<integer>;
            p_VehicleList   : TObjectList<TVehicleMovingData>;
            p_VehicleIDList : TList<integer>;
            p_CurrentTime   : TDateTime;
            p_IsReplay      : boolean;
            p_preTrsObjNum  : integer;
            p_TGStates      : TList<TTrafficGeneratorState>;

            procedure MoveNwkPed(dTimeInSeconds : Double; Instance : IF8MovingObjectInstance);
            procedure MoveVehicle(dTimeInSeconds : Double; Instance : IF8MovingObjectInstance);
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure RegisterGetUpdatedtime(value : TGetUpdatedtime);
            procedure UnRegisterGetUpdatedtime;
            procedure BeforeReplay;
            procedure AfterReplay;
            procedure ClearOldData;
            procedure DisableTrafficGenerators;
            procedure RevertTrafficGenerators;
            procedure ExportMFJson(const path: string; expInterval: double; api: IF8ApplicationServices);

            procedure UpdateData(const proj: IF8ProjectForRoad; time: TDateTime);
            property  Data : TObjectList<TPedestrianMovingData> read p_List;
            property  IDs  : TList<integer> read p_IDList;
            property  VehicleData : TObjectList<TVehicleMovingData> read p_VehicleList;
            property  VehicleIDs  : TList<integer> read p_VehicleIDList;
        end;

implementation

uses
    MFJsonExportHelper;

{ TPedestrianMovingData }
procedure TPedestrianMovingData.AfterConstruction;
    begin
    inherited;

    p_MovingLog       := TList<TMovingData>.Create;
    p_Instance        := nil;
    p_Model           := nil;
    p_CurrentIdx      := 0;
    p_CurrentPosition := GLZERO;
    p_LogInterval     := 0;
    end;

procedure TPedestrianMovingData.BeforeDestruction;
    begin
    inherited;

    UnRegisterGetUpdatedtime;
    FreeAndNil(p_MovingLog);
    end;

procedure TPedestrianMovingData.RegisterGetUpdatedtime(value: TGetUpdatedtime);
    begin
    FGetUpdatedTime := value;
    end;

procedure TPedestrianMovingData.UnRegisterGetUpdatedtime;
    begin
    FGetUpdatedTime := nil;
    end;

procedure TPedestrianMovingData.InstanceCreate;
    begin
    if (Assigned(p_Instance)) or (not Assigned(p_Model)) then
        Exit;

    p_Instance := theApplicationServices.project.CreateCharacterInstance(p_Model, true);
    p_Instance.onMove := Move;
    p_Instance.objectVisibility := false;
    end;

procedure TPedestrianMovingData.InitializeCurrentParams;
    begin
    p_CurrentIdx      := 0;
    p_CurrentPosition := GLZERO;
    if Assigned(p_Instance) then
        p_Instance.objectVisibility := false;
    end;

procedure TPedestrianMovingData.Move(dTimeInSeconds : Double; Instance : IF8MovingObjectInstance);
    var
        i      : Integer;
        t1, t2 : Double;
        x2, z2 : Double;
        now    : TDateTime;
    begin
    if (not IsReplay) or (not Assigned(p_Instance)) then
        Exit;

    if not Assigned(FGetUpdatedTime) then
        Exit;

    try
        if not(Assigned(MovingLog)) or (MovingLog.Count <= 0) then
            Exit;

        now := FGetUpdatedTime;
        if CompareDateTime(now, MovingLog[0].time) = LessThanValue then
            begin
            if p_Instance.objectVisibility then
                begin
                p_CurrentIdx := -1;
                p_instance.instancePosition := MovingLog[0].position;
                p_instance.yawAngle         := MovingLog[0].angle[_y];
                p_Instance.objectVisibility := false;
                end;
            end
        else if CompareDateTime(now, MovingLog[MovingLog.Count - 1].time) = GreaterThanValue then
            begin
            if p_Instance.objectVisibility then
                begin
                p_currentIdx := MovingLog.Count;
                i := MovingLog.Count - 1;
                p_instance.instancePosition := MovingLog[i].position;
                p_instance.yawAngle         := MovingLog[i].angle[_y];
                p_Instance.objectVisibility := false;
                end;
            end
        else
            begin
            if p_Instance.objectVisibility = false then
                p_Instance.objectVisibility := true;

            if (p_currentIdx<(MovingLog.Count - 2)) and (p_currentIdx > 0) then//再生
                begin
                if (CompareDateTime(now, MovingLog[p_currentIdx].time) <> LessThanValue) then
                    begin
                    for i := p_currentIdx to MovingLog.Count - 2 do
                        begin
                        if (CompareDateTime(now, MovingLog[i].time) <> LessThanValue) and (CompareDateTime(now, MovingLog[i+1].time) <> GreaterThanValue) then
                            begin
                            p_currentIdx := i;
                            break;
                            end;
                        end;
                    end
                else
                    begin//巻き戻し
                    for i := p_currentIdx downto 1 do
                        begin
                        if (CompareDateTime(now, MovingLog[i-1].time) <> LessThanValue) and (CompareDateTime(now, MovingLog[i].time) <> GreaterThanValue) then
                            begin
                            p_currentIdx := i;
                            break;
                            end;
                        end;
                    end;
                end
            else
                begin
                p_currentIdx := 0;
                for i := 0 to MovingLog.Count - 2 do
                    begin
                    if (CompareDateTime(now, MovingLog[i].time) <> LessThanValue) and (CompareDateTime(now, MovingLog[i+1].time) <> GreaterThanValue) then
                        begin
                        p_currentIdx := i;
                        break;
                        end;
                    end;
                end;

            i  := p_currentIdx;
            t1 := MilliSecondSpan(MovingLog[i].time, now);
            t2 := MilliSecondSpan(MovingLog[i].time, MovingLog[i + 1].time);
            x2 := MovingLog[i + 1].position[_x] - MovingLog[i].position[_x];
            z2 := MovingLog[i + 1].position[_z] - MovingLog[i].position[_z];
            if t2 > 0.0 then
                begin
                p_currentPosition[_x] := MovingLog[i].position[_x] + ((t1 * x2) / t2);
                p_currentPosition[_y] := MovingLog[i].position[_y];
                p_currentPosition[_z] := MovingLog[i].position[_z] + ((t1 * z2) / t2);
                end
            else
                p_currentPosition     := MovingLog[i].position;

            p_instance.instancePosition := p_currentPosition;
            p_instance.yawAngle         := MovingLog[i].angle[_y];
            end;
    finally
        //no action
        end;
    end;

{ TVehicleMovingData }
procedure TVehicleMovingData.AfterConstruction;
    begin
    inherited;
    p_MovingLog       := TList<TMovingData>.Create;
    p_Instance        := nil;
    p_Model           := nil;
    p_CurrentIdx      := 0;
    p_CurrentPosition := GLZERO;
    p_LogInterval     := 0;
    end;

procedure TVehicleMovingData.BeforeDestruction;
    begin
    inherited;
    UnRegisterGetUpdatedtime;
    FreeAndNil(p_MovingLog);
    end;

procedure TVehicleMovingData.RegisterGetUpdatedtime(value: TGetUpdatedtime);
    begin
    FGetUpdatedTime := value;
    end;

procedure TVehicleMovingData.UnRegisterGetUpdatedtime;
    begin
    FGetUpdatedTime := nil;
    end;

procedure TVehicleMovingData.InstanceCreate;
    begin
    if (Assigned(p_Instance)) or (not Assigned(p_Model)) then
        Exit;

    p_Instance := theApplicationServices.project.CreateMovingObjectInstance(p_Model, true);
    p_Instance.onMove := Move;
    p_Instance.objectVisibility := false;
    end;

procedure TVehicleMovingData.InitializeCurrentParams;
    begin
    p_CurrentIdx      := 0;
    p_CurrentPosition := GLZERO;
    if Assigned(p_Instance) then
        p_Instance.objectVisibility := false;
    end;

procedure TVehicleMovingData.Move(dTimeInSeconds : Double; Instance : IF8MovingObjectInstance);
    var
        i      : Integer;
        t1, t2 : Double;
        x2, z2 : Double;
        now    : TDateTime;
    begin
    if (not IsReplay) or (not Assigned(p_Instance)) then
        Exit;

    if not Assigned(FGetUpdatedTime) then
        Exit;

    try
        if not(Assigned(MovingLog)) or (MovingLog.Count <= 0) then
            Exit;

        now := FGetUpdatedTime;
        if CompareDateTime(now, MovingLog[0].time) = LessThanValue then
            begin
            p_CurrentIdx := -1;
            p_instance.instancePosition := MovingLog[0].position;
            p_instance.yawAngle         := MovingLog[0].angle[_y];
            if p_Instance.objectVisibility then
                p_Instance.objectVisibility := false;
            end
        else if CompareDateTime(now, MovingLog[MovingLog.Count - 1].time) = GreaterThanValue then
            begin
            p_currentIdx := MovingLog.Count;
            i := MovingLog.Count - 1;
            p_instance.instancePosition := MovingLog[i].position;
            p_instance.yawAngle         := MovingLog[i].angle[_y];
            if p_Instance.objectVisibility then
                p_Instance.objectVisibility := false;
            end
        else
            begin
            if p_Instance.objectVisibility = false then
                p_Instance.objectVisibility := true;

            if (p_currentIdx<(MovingLog.Count - 2)) and (p_currentIdx > 0) then//再生
                begin
                if (CompareDateTime(now, MovingLog[p_currentIdx].time) <> LessThanValue) then
                    begin
                    for i := p_currentIdx to MovingLog.Count - 2 do
                        begin
                        if (CompareDateTime(now, MovingLog[i].time) <> LessThanValue) and (CompareDateTime(now, MovingLog[i+1].time) <> GreaterThanValue) then
                            begin
                            p_currentIdx := i;
                            break;
                            end;
                        end;
                    end
                else
                    begin//巻き戻し
                    for i := p_currentIdx downto 1 do
                        begin
                        if (CompareDateTime(now, MovingLog[i-1].time) <> LessThanValue) and (CompareDateTime(now, MovingLog[i].time) <> GreaterThanValue) then
                            begin
                            p_currentIdx := i;
                            break;
                            end;
                        end;
                    end;
                end
            else
                begin
                p_currentIdx := 0;
                for i := 0 to MovingLog.Count - 2 do
                    begin
                    if (CompareDateTime(now, MovingLog[i].time) <> LessThanValue) and (CompareDateTime(now, MovingLog[i+1].time) <> GreaterThanValue) then
                        begin
                        p_currentIdx := i;
                        break;
                        end;
                    end;
                end;

            i  := p_currentIdx;
            t1 := MilliSecondSpan(MovingLog[i].time, now);
            t2 := MilliSecondSpan(MovingLog[i].time, MovingLog[i + 1].time);
            x2 := MovingLog[i + 1].position[_x] - MovingLog[i].position[_x];
            z2 := MovingLog[i + 1].position[_z] - MovingLog[i].position[_z];
            if t2 > 0.0 then
                begin
                p_currentPosition[_x] := MovingLog[i].position[_x] + ((t1 * x2) / t2);
                p_currentPosition[_y] := MovingLog[i].position[_y];
                p_currentPosition[_z] := MovingLog[i].position[_z] + ((t1 * z2) / t2);
                end
            else
                p_currentPosition     := MovingLog[i].position;

            p_instance.instancePosition := p_currentPosition;
            p_instance.yawAngle         := MovingLog[i].angle[_y];
            end;
    finally
        //no action
        end;
    end;

{ TPedestrianMovingList }
procedure TPedestrianMovingList.AfterConstruction;
    begin
    inherited;
    p_List   := TObjectList<TPedestrianMovingData>.Create;
    p_IDList := TList<integer>.Create;
    p_VehicleList   := TObjectList<TVehicleMovingData>.Create;
    p_VehicleIDList := TList<integer>.Create;
    p_TGStates := TList<TTrafficGeneratorState>.Create;
    p_preTrsObjNum := 0;
    end;

procedure TPedestrianMovingList.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_List);
    FreeAndNil(p_IDList);
    FreeAndNil(p_VehicleList);
    FreeAndNil(p_VehicleIDList);
    FreeAndNil(p_TGStates);
    end;

procedure TPedestrianMovingList.RegisterGetUpdatedtime(value : TGetUpdatedtime);
    begin
    FGetUpdatedTime := value;
    end;

procedure TPedestrianMovingList.UnRegisterGetUpdatedtime;
    begin
    FGetUpdatedTime := nil;
    end;

procedure TPedestrianMovingList.BeforeReplay;
    var
        i : integer;
    begin
    if (p_IsReplay) or (p_List.Count = 0) or (not Assigned(FGetUpdatedTime)) then
        Exit;

    for i := 0 to p_List.Count - 1 do
        begin
        p_List[i].InstanceCreate;
        p_List[i].RegisterGetUpdatedtime(FGetUpdatedTime);
        p_List[i].IsReplay := true;
        end;

    if p_VehicleList.Count > 0 then
        begin
        for i := 0 to p_VehicleList.Count - 1 do
            begin
            p_VehicleList[i].InstanceCreate;
            p_VehicleList[i].RegisterGetUpdatedtime(FGetUpdatedTime);
            p_VehicleList[i].IsReplay := true;
            end;
        end;

    p_IsReplay := true;
    end;

procedure TPedestrianMovingList.AfterReplay;
    var
        i : integer;
    begin
    if (not p_IsReplay) or (p_List.Count = 0) or (not Assigned(FGetUpdatedTime)) then
        Exit;

    for i := 0 to p_List.Count - 1 do
        begin
        p_List[i].InitializeCurrentParams;
        p_List[i].UnRegisterGetUpdatedtime;
        p_List[i].IsReplay := false;
        end;

    if p_VehicleList.Count > 0 then
        begin
        for i := 0 to p_VehicleList.Count - 1 do
            begin
            p_VehicleList[i].InitializeCurrentParams;
            p_VehicleList[i].UnRegisterGetUpdatedtime;
            p_VehicleList[i].IsReplay := false;
            end;
        end;

    p_IsReplay := false;
    end;

procedure TPedestrianMovingList.ClearOldData;
    begin
    p_List.Clear;
    p_IDList.Clear;
    end;

procedure TPedestrianMovingList.MoveNwkPed(dTimeInSeconds : Double; Instance : IF8MovingObjectInstance);
    var
        newHistory : TMovingData;
    begin
    if p_IsReplay then
        Exit;

    p_List[p_IDList.IndexOf(Instance.ID)].LogInterval := p_List[p_IDList.IndexOf(Instance.ID)].LogInterval + dTimeInSeconds;
    if p_List[p_IDList.IndexOf(Instance.ID)].LogInterval < MOVING_LOG_INTERVAL then
        Exit;

    newHistory.time      := FGetUpdatedTime;
    newHistory.position  := Instance.savedPosition;
    newHistory.angle[_x] := Instance.rollAngle;
    newHistory.angle[_y] := Instance.yawAngle;
    newHistory.angle[_z] := Instance.pitchAngle;
    p_List[p_IDList.IndexOf(Instance.ID)].MovingLog.Add(newHistory);
    p_List[p_IDList.IndexOf(Instance.ID)].LogInterval := 0;
    end;

procedure TPedestrianMovingList.MoveVehicle(dTimeInSeconds : Double; Instance : IF8MovingObjectInstance);
    var
        newHistory : TMovingData;
    begin
    if p_IsReplay then
        Exit;

    p_VehicleList[p_VehicleIDList.IndexOf(Instance.ID)].LogInterval := p_VehicleList[p_VehicleIDList.IndexOf(Instance.ID)].LogInterval + dTimeInSeconds;
    if p_VehicleList[p_VehicleIDList.IndexOf(Instance.ID)].LogInterval < MOVING_LOG_INTERVAL then
        Exit;

    newHistory.time      := FGetUpdatedTime;
    newHistory.position  := Instance.savedPosition;
    newHistory.angle[_x] := Instance.rollAngle;
    newHistory.angle[_y] := Instance.yawAngle;
    newHistory.angle[_z] := Instance.pitchAngle;
    p_VehicleList[p_VehicleIDList.IndexOf(Instance.ID)].MovingLog.Add(newHistory);
    p_VehicleList[p_VehicleIDList.IndexOf(Instance.ID)].LogInterval := 0;
    end;

procedure TPedestrianMovingList.UpdateData(const proj: IF8ProjectForRoad; time: TDateTime);
    var
        i           : integer;
        newData     : TPedestrianMovingData;
        newVehiData : TVehicleMovingData;
        TrsObjs     : TransientObjectArray;
        tmpCh       : IF8CharacterInstance;
        tmpVehi     : IF8MovingObjectInstance;
    begin
    if (not proj.trafficIsMoving) or (p_IsReplay) then
        Exit;

    p_CurrentTime := time;
    TrsObjs := proj.GetTransientObjectsOfEveryTypeWithinCenter(Max(proj.GetTerrain.lengthNorthing, proj.GetTerrain.lengthEasting),
                                                               GLZERO);

    if Length(TrsObjs) <= p_preTrsObjNum then //TransientObjectの数が直前と同じor直前より少ない=更新不要
        begin
        p_preTrsObjNum := Length(TrsObjs);
        Exit;
        end;

    for i := 0 to Length(TrsObjs) - 1 do
        begin
        if (TrsObjs[i].IsCarInstance) or (TrsObjs[i].IsPlaneInstance) then
            begin
            tmpVehi := IF8MovingObjectInstance(TrsObjs[i]);
            if (not p_VehicleIDList.Contains(tmpVehi.ID)) then
                begin
                tmpVehi.RegisterOnAfterCalculateMovementProc(MoveVehicle);
                newVehiData       := TVehicleMovingData.Create;
                newVehiData.Model := tmpVehi.Model;
                p_VehicleList.Add(newVehiData);
                p_VehicleIDList.Add(tmpVehi.ID);
                end;

            Continue;
            end;

        tmpCh := IF8CharacterInstance(TrsObjs[i]);
        if (not p_IDList.Contains(tmpCh.ID)) then
            begin
            tmpCh.RegisterOnAfterCalculateMovementProc(MoveNwkPed);
            newData       := TPedestrianMovingData.Create;
            newData.Model := tmpCh.characterModel;
            p_List.Add(newData);
            p_IDList.Add(tmpCh.ID);
            end;
        end;

    p_preTrsObjNum := Length(TrsObjs);
    end;

procedure TPedestrianMovingList.DisableTrafficGenerators;
    var
        i : integer;
        tgState : TTrafficGeneratorState;
    begin
    p_TGStates.Clear;
    for i := 1 to theApplicationServices.project.numberOfRoads do
        begin
        if Assigned(theApplicationServices.project.road[i].trafficGeneratorStart) then
            begin
            tgState.StartTo := theApplicationServices.project.road[i].trafficGeneratorStart.enabled;
            theApplicationServices.project.road[i].trafficGeneratorStart.enabled := false;
            end;

        if Assigned(theApplicationServices.project.road[i].trafficGeneratorFinish) then
            begin
            tgState.EndTo := theApplicationServices.project.road[i].trafficGeneratorFinish.enabled;
            theApplicationServices.project.road[i].trafficGeneratorFinish.enabled := false;
            end;

        p_TGStates.Add(tgState);
        end;
    end;

procedure TPedestrianMovingList.RevertTrafficGenerators;
    var
        i : integer;
    begin
    for i := 1 to theApplicationServices.project.numberOfRoads do
        begin
        if Assigned(theApplicationServices.project.road[i].trafficGeneratorStart) then
            theApplicationServices.project.road[i].trafficGeneratorStart.enabled := p_TGStates[i - 1].StartTo;

        if Assigned(theApplicationServices.project.road[i].trafficGeneratorFinish) then
            theApplicationServices.project.road[i].trafficGeneratorFinish.enabled := p_TGStates[i - 1].EndTo;
        end;
    end;

procedure TPedestrianMovingList.ExportMFJson(const path: string; expInterval: double; api: IF8ApplicationServices);
    var
        JsonSerializer : TJsonSerializer;
        ExportHelper   : TMFJsonExportHelper;
        Writer         : TStreamWriter;
        Json           : string;
        exList         : TList<TPedestrianMovingData>;
    begin
    ExportHelper   := TMFJsonExportHelper.Create;
    JsonSerializer := TJsonSerializer.Create;
    exList         := TList<TPedestrianMovingData>.Create;
    exList.AddRange(p_list);
    try
        Writer := TStreamWriter.Create(path, False, TEncoding.UTF8);
        try
            Json := ExportHelper.SetMFJsonData(api, exList, expInterval, JsonSerializer);
            Writer.Write(Json);
        finally
            Writer.Free;
        end;
    finally
        FreeAndNil(ExportHelper);
        end;
    end;
end.
