unit MovingFeature;

interface

uses
    System.SysUtils,
    System.DateUtils,
    System.Types,
    System.Classes,
    System.JSON,
    System.JSON.Readers,
    System.JSON.Builders,
    System.JSON.Serializers,
    System.Generics.Collections,
    System.Math,
    TemporalProperty,
    PluginCore,
    F8GLUtils,
    MFJsonLoaderOptions;


type
    TGetUpdatedtime = function : TDateTime of Object;

    TCoordinate = TArray<Double>;
    TProperties = record
        name   : String;
        age    : String;
        gender : String;
        weather: String;
        procedure ReadJson(const ait: TJSONIterator);
        end;

    TTemporalGeometry = record
        interpolations  : String;
        atype           : String;
        coordinates     : TArray<TCoordinate>;
        datetimes       : TArray<String>;
        procedure ReadJson(const ait: TJSONIterator);
        end;

    TimeAndPositionType = record
        time    : TDateTime;
        position: GLPointType;
        end;

    /// <summary>
    ///    シミュレーション計算後のAgentを管理するクラス
    /// </summary>
    MovingFeatureClass = class
        private
            FGetUpdatedTime : TGetUpdatedtime;

            p_properties    : TProperties;
            // 読み込むが追加分は保持しない
            p_temporalGeometry      : TTemporalGeometry;
            // 読み込むが追加分は保持しない
            p_temporalProperties    : TObjectList<TemporalPropertyClass>;

            p_character : IF8QuakeIII;
            p_characterInRain : IF8QuakeIII;
            p_instance  : IF8CharacterInstance;

            // 読み込まれた後、下記のリストに時系列としてまとめる
            p_histories : TList<TimeAndPositionType>;
            // 上の時系列の結果から今回Focusしたエリアにあるリスト
            p_focusedHistories  : TList<TimeAndPositionType>;
            p_temporalHistories : TList<TimeAndPositionType>;

            p_startTime : TDateTime;
            p_goalTime  : TDateTime;
            p_currentIndex      : Integer;
            p_currentPosition   : GLPointType;
            p_currentTime       : TDateTime;

            p_communicateMENGE   : Boolean;

            p_startPoint    : GLPointType;
            p_goalPoint     : GLPointType;
            p_lastStateIndex  : NativeInt;
            p_lastStateName   : String;

            procedure   SetInstance(const Value: IF8CharacterInstance);
            procedure   CheckAndSetCharacter(const aproject: IF8ProjectForRoad; const options: MFJsonLoaderOptionsClass);
            procedure   CreateCharacterInstance(const aproject: IF8ProjectForRoad; isRain : Boolean);

            procedure   CalculatePosition(const aproject: IF8ProjectForRoad; const now: TDateTime);
            procedure   FocusTheHistoryOfArea(const topLeftPos, downRightPos: GLPointType);

            function    FindProp(const name: String): TemporalPropertyClass;
            function    GetFocusedHistory(index: integer): TimeAndPositionType;
            function    GetNumberOfFocusedHistories: integer;
            procedure   SetMyCharacter(const Value: IF8QuakeIII);
            function    GetLastStateIndex : NativeInt;
            procedure   SetLastStateIndex(const Value: NativeInt);
            function    GetLastStateName: String;
            procedure   SetLastStatename(const Value: String);

        public
            procedure   AfterConstruction; override;
            procedure   BeforeDestruction; override;
            procedure   ReadJson(const ait: TJSONIterator);

            function    Clone : MovingFeatureClass;

            procedure   Parse(const aproject: IF8ProjectForRoad);
            procedure   MergeFrom(const amf: MovingFeatureClass);

            procedure   CalcTargetTime(const simStartTime,simEndTime : TDatetime; per : integer);
            procedure   CalcTargetArea(const topLeftPos, downRightPos: GLPointType);
            procedure   InitialiseHistories;
            procedure   InitialiseFocusedHistories;

            procedure   BeforeStart(const startTime: TDateTime; const aproject: IF8ProjectForRoad; isRain : Boolean);
            procedure   Pause;
            procedure   Stop(const aproject: IF8ProjectForRoad);
            procedure   Reset(const aproject: IF8ProjectForRoad);
            procedure   Move(dTimeInSeconds : Double; Instance : IF8MovingObjectInstance);
            procedure   InstanceDestroy(Instance : IF8DBObject);


            property    myInstance : IF8CharacterInstance read p_instance write SetInstance;
            property    myCharacter: IF8QuakeIII read p_character write SetMyCharacter;

            procedure   RegisterGetUpdatedtime(value : TGetUpdatedtime);
            procedure   UnRegisterGetUpdatedtime;
            property    communicateMenge: Boolean read p_communicateMENGE write p_communicateMENGE;

            property    startTime   : TDateTime   read p_startTime;
            property    startPoint  : GLPointType read p_startPoint write p_startPoint;
            property    goalPoint   : GLPointType read p_goalPoint  write p_goalPoint;

            property    properties       : TProperties       read p_properties;
            property    temporalGeometry : TTemporalGeometry read p_temporalGeometry;
            property    numberOfFocusedHistories : integer read GetNumberOfFocusedHistories;
            property    focusedHistory[index : integer] : TimeAndPositionType read GetFocusedHistory;

            property    lastStateIndex : NativeInt read GetLastStateIndex write SetLastStateindex;
            property    lastStateName  : String    read GetLastStateName  write SetLastStatename;
        end;

    /// <summary>
    ///    MovingFeatureClassのリスト
    ///    リストに加えて、保持している全てのMovingFeatureClassに対して行う処理等を配置している
    /// </summary>
    MovingFeatureListClass = class
        private
            class var FGetUpdatedTime : TGetUpdatedtime;
            class var p_list    : TObjectList<MovingFeatureClass>;
            class var p_Maglist    : TObjectList<MovingFeatureClass>;
            class var p_beginningTime   : TDateTime;
            class var p_currentStateList: TStringList;

            class function GetList: TObjectList<MovingFeatureClass>;
            class function GetMovingFeature(const index: Integer): MovingFeatureClass; static;
            class function GetNumberOfMovingFeatures: Integer; static;
        public
            class procedure ReadJson(const ait: TJSONIterator; const project: IF8ProjectForRoad; const options: MFJsonLoaderOptionsClass);
            procedure AfterConstruction; override;
            class procedure Reset(const aproject: IF8ProjectForRoad);
            class property beginningTime : TDateTime read p_beginningTime;
            class property numberOfMovingFeatures: Integer read GetNumberOfMovingFeatures;
            class property movingFeature[const index: Integer]  : MovingFeatureClass read GetMovingFeature;

            class procedure BeforeDestructions;
            class procedure ClearMovingFeatures;
            class procedure ClearFocusedHistories;
            class procedure ClearMagnificateFeature;

            class procedure BeforeStartTraffic(const aproject: IF8ProjectForRoad; isRain : boolean);
            class procedure PauseTraffic;
            class procedure StopTraffic(const project: IF8ProjectForRoad);

            class function  MakeCurrentStateList(const targetDir: String): Boolean;
            class function  GetCurrentState(const stateId: NativeUInt): String;

            class function  CreateMagnificateFeature(mf : MovingFeatureClass) : MovingFeatureClass;

            class Procedure RegisterGetUpdatedTime(value : TGetUpdatedtime);
            class Procedure UnRegisterGetUpdatedTime;

            // Debug用
            class function AppendNewMovingFeature: MovingFeatureClass;
        end;


implementation

uses
    winapi.Windows,
    MFJsonExportHelper;

const
    STATE_NAME_NONE     = 'none';
    STATE_NAME_WAIT     = 'wait_';
    STATE_NAME_WALK     = 'walk_';
    STATE_NAME_GOAL     = 'goal_';
    STATE_NAME_TELEPORT = 'telepo_';

{ MovingFeatureListClass }

procedure MovingFeatureListClass.AfterConstruction;
    begin
    inherited;
    raise Exception.Create('This class is not expected to be created.');
    end;

class function MovingFeatureListClass.AppendNewMovingFeature: MovingFeatureClass;
    begin
    Result := MovingFeatureClass.Create;
    p_list.add( Result );
    end;

class procedure MovingFeatureListClass.BeforeDestructions;
    var
        i   : Integer;
    begin
    for i := 0 to numberOfMovingFeatures - 1 do
        movingFeature[i].myInstance := nil;
    end;

class procedure MovingFeatureListClass.ClearFocusedHistories;
    var
        i   : Integer;
    begin
    for i := 0 to numberOfMovingFeatures - 1 do
        begin
        movingFeature[i].InitialiseFocusedHistories;
        end;
    end;

class procedure MovingFeatureListClass.ClearMagnificateFeature;
    begin
    if Assigned(p_MagList) then
        p_MagList.Clear;
    end;

class procedure MovingFeatureListClass.ClearMovingFeatures;
    begin
    if Assigned(p_list) then
        p_list.Clear;
    if Assigned(p_MagList) then
        p_MagList.Clear;
    end;

class function MovingFeatureListClass.CreateMagnificateFeature(mf: MovingFeatureClass) : MovingFeatureClass;
    var
        clone : MovingFeatureClass;
    begin
    clone := mf.Clone;
    if not Assigned(p_Maglist) then
        p_Maglist := TObjectList<MovingFeatureClass>.Create;
    p_Maglist.Add(clone);
    Result := clone;
    end;

class function MovingFeatureListClass.GetCurrentState(const stateId: NativeUInt): String;
    begin
    {$ifdef Debug}
    Assert( (stateID =-1)or(InRange(stateID,0,p_currentStateList.Count-1)),'Receive Invalid StateIndex');
    {$endif}

    if stateID <0 then
        begin
        Result := STATE_NAME_NONE;
        end
    else if stateID<=p_currentStateList.Count-1 then
        begin
        Result := p_currentStateList[ stateId ];
        end;
    end;

class function MovingFeatureListClass.GetList: TObjectList<MovingFeatureClass>;
    begin
    if not Assigned(p_list) then
        p_list := TObjectList<MovingFeatureClass>.Create;

    Result := p_list;
    end;

class function MovingFeatureListClass.GetMovingFeature(const index: Integer): MovingFeatureClass;
    begin
    if Assigned(p_list) and (index < p_list.Count) then
        Result := p_list[ index ]
    else if Assigned(p_MagList) and InRange(index,p_list.Count,p_list.Count+p_Maglist.Count-1) then
        Result := p_MagList[ index- p_list.Count]
    else
        Result := nil;
    end;

class function MovingFeatureListClass.GetNumberOfMovingFeatures: Integer;
    begin
    if Assigned(p_MagList) then
        Result := p_list.Count + p_MagList.Count
    else if Assigned(p_list) then
        Result := p_list.Count
    else
        Result := 0;
    end;

class function MovingFeatureListClass.MakeCurrentStateList(const targetDir: String): Boolean;
    var
        csvList : TStringList;
        line    : String;
        pair    : TArray<String>;
        index   : NativeInt;
        i       : NativeInt;
    begin
    if not Assigned(p_currentStateList) then
        p_currentStateList := TStringList.Create
    else
        p_currentStateList.Clear;

    if DirectoryExists(targetDir) and FileExists(targetDir + '\' + 'StatusList.csv') then
        begin
        csvList := TStringList.Create;
        try
            csvList.LoadFromFile(targetDir + '\' + 'StatusList.csv');
            for line in csvList do
                begin
                pair := line.Split([',']);
                if Length(pair) = 2 then
                    begin
                    index := StrtoIntDef(pair[0], -1);
                    if index > -1 then
                        begin
                        if p_currentStateList.Count <= index then
                            for i := p_currentStateList.Count to index+1 do
                                p_currentStateList.Add('');

                        p_currentStateList[index] := pair[1];
                        end;
                    end;
                end;
            Result := True;
        finally
            FreeAndNil(csvList);
            end;
        end
    else
        Result := False;
    end;

class procedure MovingFeatureListClass.PauseTraffic;
    var
        i   : Integer;
    begin
    for i := 0 to numberOfMovingFeatures - 1 do
        movingFeature[i].Pause;
    end;

class procedure MovingFeatureListClass.ReadJson(const ait: TJSONIterator; const project: IF8ProjectForRoad; const options: MFJsonLoaderOptionsClass);
    var
        alist   : TObjectList<MovingFeatureClass>;
        amf     : MovingFeatureClass;
        i       : Integer;
        adding  : Boolean;
    begin
    alist := GetList;
    adding := alist.Count > 0;

    amf := nil;
    while ait.Next do
        begin
        ait.Recurse;

        amf := MovingFeatureClass.Create;
        amf.ReadJson(ait);

        if adding then
            begin
            for i := 0 to alist.Count-1 do
                if alist[i].p_properties.name = amf.p_properties.name then
                    begin
                    amf.Parse(project);

                    alist[i].MergeFrom(amf);
                    FreeAndNil(amf);
                    break;
                    end;
            end;

        if Assigned(amf) then
            alist.Add(amf);

        ait.Return;
        end;

    p_beginningTime := IncYear(Now, 30);
    for i := 0 to alist.Count - 1 do
        begin
        if alist[i].p_histories.Count = 0 then
            alist[i].Parse(project);
        if CompareDateTime(p_beginningTime, alist[i].p_histories[0].time) = GreaterThanValue  then
            p_beginningTime := alist[i].p_histories[0].time;
        end;

    for i := 0 to alist.Count - 1 do
        alist[i].CheckAndSetCharacter(project, options);

    end;

class procedure MovingFeatureListClass.RegisterGetUpdatedTime(value: TGetUpdatedtime);
    begin
    FGetUpdatedTime := Value;
    end;

class procedure MovingFeatureListClass.Reset(const aproject: IF8ProjectForRoad);
    var
        i   : Integer;
    begin
    if not Assigned(p_list) then
        exit;
    for i := 0 to p_list.Count - 1 do
        p_list[i].Reset(aproject);

    if not Assigned(p_Maglist) then
        exit;
    for i := 0 to p_Maglist.Count - 1 do
        p_Maglist[i].Reset(aproject);
    end;

class procedure MovingFeatureListClass.BeforeStartTraffic(const aproject: IF8ProjectForRoad; isRain : boolean);
    var
        i   : Integer;
    begin

    for i := 0 to numberOfMovingFeatures - 1 do
        begin
        movingFeature[i].BeforeStart(p_beginningTime, aproject,isRain);
        movingFeature[i].RegisterGetUpdatedtime(FGetUpdatedTime);
        end;
    end;

class procedure MovingFeatureListClass.StopTraffic(const project: IF8ProjectForRoad);
    var
        i   : Integer;
    begin
    for i := 0 to p_list.Count - 1 do
        p_list[i].Stop(project);

    if Assigned(p_Maglist) then
        begin
        for i := 0 to p_Maglist.Count-1 do
            p_Maglist[i].Stop(project);
        end;
    end;

class procedure MovingFeatureListClass.UnRegisterGetUpdatedTime;
    begin
    FGetUpdatedTime := nil;
    end;

{ MovingFeatureClass }

procedure MovingFeatureClass.AfterConstruction;
    begin
    inherited;
    p_lastStateName := STATE_NAME_NONE;
    p_temporalProperties := TObjectList<TemporalPropertyClass>.Create(True);
    p_histories := TList<TimeAndPositionType>.Create;
    p_temporalHistories := nil;
    p_focusedHistories := nil;
    p_communicateMENGE := True;
    end;

procedure MovingFeatureClass.BeforeDestruction;
    begin
    if Assigned(p_instance) then
        InstanceDestroy(p_instance);
    if Assigned(p_temporalProperties) then
        FreeAndNil(p_temporalProperties);
    if Assigned(p_histories) then
        FreeAndNil(p_histories);
    inherited;
    end;

procedure MovingFeatureClass.CalcTargetArea(const topLeftPos, downRightPos: GLPointType);
    begin
    if p_histories.Count > 0 then
        FocusTheHistoryOfArea(topLeftPos, downRightPos);
    end;

procedure MovingFeatureClass.CalcTargetTime(const simStartTime,simEndTime: TDatetime; per : integer);
    //==========================================================================
    function ForwordSeekTime(targetTime : TDatetime; start,last : integer) : integer;
        var
            i : integer;
        begin
        Result := start;
        for I := start to last-1 do
            begin
            if InRange(targetTime,p_focusedHistories[i].time,p_focusedHistories[i+1].time) then
                begin
                Exit(i);
                end;
            end;
        end;
    //==========================================================================
    function BackwordSeekTime(targetTime : TDatetime; last,start : integer) : integer;
        var
            i : integer;
        begin
        Result := last;
        for I := last downto start+1 do
            begin
            if InRange(targetTime,p_focusedHistories[i-1].time,p_focusedHistories[i].time) then
                begin
                Exit(i);
                end;
            end;
        end;
    //==========================================================================
    function QuickSeekTime(targetTime : TDatetime; start,last : integer) : integer;
        var
            ave : double;
            index : integer;
        begin
        Result := 0;
        if last-start>4 then //計算回数　quickSeek=log要素数=log4=2 ForwordSeek=要素数/2=4/2=2 　
            begin
            ave := SecondsBetween(p_focusedHistories[start].time,p_focusedHistories[last].time)/(last-start);
            index := Floor(SecondsBetween(p_focusedHistories[start].time,targetTime)/ave)+start;
            if  InRange(index,0,p_focusedHistories.Count-1) then
                begin
                if (index <> last) and InRange(targetTime,p_focusedHistories[index].time,p_focusedHistories[index+1].time) then
                    begin
                    if p_focusedHistories[index+1].time = targetTime  then
                        begin
                        Result := index+1;
                        end
                    else
                        begin
                        Result := index;
                        end;
                    end
                else if (index <> start) and InRange(targetTime,p_focusedHistories[index-1].time,p_focusedHistories[index].time) then
                    begin
                    if p_focusedHistories[index].time = targetTime  then
                        begin
                        Result := index;
                        end
                    else
                        begin
                        Result := index-1;
                        end;
                    end
                else if index = last then
                    begin
                    Result := BackwordSeekTime(targetTime,last,start);
                    end
                else if index = start then
                    begin
                    Result := ForwordSeekTime(targetTime,start,last);
                    end
                else
                    begin
                    if (targetTime - p_focusedHistories[index].time)>=0  then
                        begin
                        Result := QuickSeekTime(targetTime,index,last);
                        end
                    else
                        begin
                        Result := QuickSeekTime(targetTime,start,index);
                        end;
                    end;
                end;
            end
        else
            begin
            if (targetTime - p_focusedHistories[start].time)<(p_focusedHistories[last].time-targetTime)  then
                begin
                Result := ForwordSeekTime(targetTime,start,last);
                end
            else
                begin
                Result := BackwordSeekTime(targetTime,last,start);
                end;
            end;
        end;
    //==========================================================================
    var
        si,ei : Integer;
        I: Integer;
        newList : TList<TimeAndPositionType>;
        lastTime : TDateTime;
        fullTime,dTime : double;
    begin
    if not Assigned(p_focusedHistories) then
        begin
        Exit;
        end;
    if p_focusedHistories.Count<=1 then
        begin
        p_focusedHistories.Clear;
        Exit;
        end;


    fullTime := SecondsBetween(simEndTime,simStartTime);

    dTime := fullTime/(fullTime*(per/100));

    if (p_goalTime<simStartTime)then
        begin
        p_focusedHistories.Clear;
        end
    else if  (simEndTime<p_startTime)then//out of SimTime
        begin
        p_focusedHistories.Clear;
        end
    else
        begin
        if simStartTime<=p_startTime then //already in SimArea at startSimeTime
            begin
            si := 0;
            end
        else //Enter SimArea During SimTime
            begin
            si := QuickSeekTime(simStartTime,0,p_focusedHistories.Count-1);
            end;

        if p_goalTime<=simEndTime then //Agent in SimArea at SimEnd
            begin
            ei := p_focusedHistories.Count-1;
            end
        else //Exit SimArea Before SimEnd
            begin
            ei := QuickSeekTime(simEndTime,  0,p_focusedHistories.Count-1);//here
            end;

        lastTime := 0;
        newList := TList<TimeAndPositionType>.Create;

        for I := si to ei do
            begin
            if (i >= 0) and (i< p_focusedHistories.Count) then
                begin
                if (SecondsBetween(p_focusedHistories[i].time,lastTime)>=dTime) then
                    begin
                    newList.Add(p_focusedHistories[i]);
                    lastTime := p_focusedHistories[i].time;
                    end
                else
                    begin
                    newList.Add(p_focusedHistories[i]);
                    break;
                    end;
                end;
            end;

        p_focusedHistories.Clear;
        p_focusedHistories := newList;
        end;
    end;

procedure MovingFeatureClass.CalculatePosition(const aproject: IF8ProjectForRoad; const now: TDateTime);
    var
        i   : Integer;
        t1, t2  : Double;
        x2, z2  : Double;
        h   : Double;
        histories  : TList<TimeAndPositionType>;
        prepos : GLPointType;
        curpos : GLPointType;
        yaw : Double;
        if8api  : IF8ApplicationServices;
    begin
    histories  := TList<TimeAndPositionType>.Create;
    try
        if Assigned(p_focusedHistories) and (p_focusedHistories.Count > 0) then
            histories.AddRange( p_focusedHistories);

        if not(Assigned(histories)) or (histories.Count <= 0) then
            Exit;

        if CompareDateTime(now, histories[0].time) = LessThanValue then
            begin
            p_currentIndex := -1;
            p_currentPosition := histories[0].position;
            end
        else if CompareDateTime(now, histories[histories.Count - 1].time) = GreaterThanValue then
            begin
            p_currentIndex := histories.Count;
            i := histories.Count - 1;
            p_currentPosition := histories[i].position;
            end
        else if histories.Count = 1 then
            begin
            p_currentIndex := 0;
            p_currentPosition[_x] := histories[0].position[_x];
            p_currentPosition[_z] := histories[0].position[_z];
            end
        else
            begin
            if (p_currentIndex<(histories.Count - 2)) and (p_currentIndex > 0) then//再生
                begin
                if (CompareDateTime(now, histories[p_currentIndex].time) <> LessThanValue) then
                    begin
                    for i := p_currentIndex to histories.Count - 2 do
                        begin
                        if (CompareDateTime(now, histories[i].time) <> LessThanValue) and (CompareDateTime(now, histories[i+1].time) <> GreaterThanValue) then
                            begin
                            p_currentIndex := i;
                            break;
                            end;
                        end;
                    end
                else
                    begin//巻き戻し
                    for i := p_currentIndex downto 1 do
                        begin
                        if (CompareDateTime(now, histories[i-1].time) <> LessThanValue) and (CompareDateTime(now, histories[i].time) <> GreaterThanValue) then
                            begin
                            p_currentIndex := i;
                            break;
                            end;
                        end;
                    end;
                end
            else
                begin
                p_currentIndex := 0;
                for i := 0 to histories.Count - 2 do
                    begin
                    if (CompareDateTime(now, histories[i].time) <> LessThanValue) and (CompareDateTime(now, histories[i+1].time) <> GreaterThanValue) then
                        begin
                        p_currentIndex := i;
                        break;
                        end;
                    end;
                end;

            Assert((p_currentIndex >= 0) and (p_currentIndex < histories.Count-1), 'The p_currentIndex is ' + IntToStr(p_currentIndex) + ' histories.Count is ' + IntToStr(histories.Count));

            if p_temporalGeometry.interpolations = 'Linear' then
                begin
                i := p_currentIndex;
                t1 := MilliSecondSpan(histories[i].time, now);
                t2 := MilliSecondSpan(histories[i].time, histories[i + 1].time);
                x2 := histories[i + 1].position[_x] - histories[i].position[_x];
                z2 := histories[i + 1].position[_z] - histories[i].position[_z];
                if t2 > 0.0 then
                    begin
                    p_currentPosition[_x] := histories[i].position[_x] + t1 * x2 / t2;
                    p_currentPosition[_z] := histories[i].position[_z] + t1 * z2 / t2;
                    end
                else
                    begin
                    p_currentPosition[_x] := histories[i].position[_x];
                    p_currentPosition[_z] := histories[i].position[_z];
                    end;
                end;
            end;

        if (CompareDateTime(p_currenttime,now)=LessThanValue) and (p_currentIndex>0) then        begin
            for i := p_currentIndex to histories.Count - 1 do
                begin
                if SecondsBetween(histories[p_currentIndex].time,histories[i].time)>5 then
                    begin
                    x2 := histories[p_currentIndex].position[_x] - histories[i].position[_x];
                    z2 := histories[p_currentIndex].position[_z] - histories[i].position[_z];

                    if (x2*x2+z2*z2)<1 then
                        begin//移動距離が短いので止まっているものとみなして計算処理中断
                        Exit;
                        end
                    else
                        begin
                        break;
                        end;
                    end;
                end;
            end
        else if p_currentIndex<histories.Count  then
            begin
            for i := p_currentIndex downto 1 do
                begin
                if SecondsBetween(histories[p_currentIndex].time,histories[i].time)>5 then
                    begin
                    x2 := histories[p_currentIndex].position[_x] - histories[i].position[_x];
                    z2 := histories[p_currentIndex].position[_z] - histories[i].position[_z];

                    if (x2*x2+z2*z2)<1 then
                        begin//移動距離が短いので止まっているものとみなして計算処理中断
                        Exit;
                        end
                    else
                        begin
                        break;
                        end;
                    end;
                end;
            end;

        if Supports(ApplicationServices, IF8ApplicationServices, if8api) and Assigned(p_instance) then
            begin
            prepos := p_instance.instancePosition;
            curpos := AsGLPointType(p_currentPosition[_x], 0, if8api.distanceNorth - p_currentPosition[_z]);

            h := aproject.GetHeightForObjectInstanceAt(curpos[_x], curpos[_z]);
            curpos := AsGLPointType(curpos[_x], h, curpos[_z]);
            p_instance.instancePosition := curpos;
            if F8Distance2D(curpos, prepos) > 0 then
                begin
                if abs(prepos[_z] - curpos[_z]) > 1E-6 then
                    yaw := ArcTan2(curpos[_x] - prepos[_x], curpos[_z] - prepos[_z] )
                else
                    begin
                    if curpos[_x] - prepos[_x] < 0 then
                        yaw := - Pi / 2
                    else
                        yaw := Pi / 2;
                    end;

                if CompareDateTime(p_currenttime,now)=LessThanValue  then
                    p_instance.yawAngle := yaw
                else//巻き戻し
                    p_instance.yawAngle := yaw+PI;

                end;
            end;
    finally
        FreeAndNil(histories);
        end;

    end;

procedure MovingFeatureClass.CheckAndSetCharacter(const aproject: IF8ProjectForRoad; const options: MFJsonLoaderOptionsClass);
    var
        ci  : Integer;
        age : Integer;
        gender  : Integer;
        ageProp,
        genderProp,
        weatherProp  : TemporalPropertyClass;
        ar  : AgeRangeType;
    begin
    ci := aproject.numberOfCharacters;
    if (ci < 1) then
        raise Exception.Create('キャラクターを先に作成してください。');

    ageProp := FindProp('age');
    genderProp := FindProp('gender');
    weatherProp := FindProp('weather');

    if Assigned(weatherProp) then
        p_properties.weather  := weatherProp.value.AsString;

    if Assigned(ageProp) and Assigned(genderProp) then
        begin
        age := ageProp.value.AsInteger;
        gender := genderProp.value.AsInteger;
        p_properties.age     := age.ToString;
        p_properties.gender  := gender.ToString;

        if age < 20 then
            begin
            ar := under20
            end
        else if age < 60 then
            begin
            ar := under60
            end
        else
            begin
            ar := over60;
            end;

        if gender = 0 then
            begin
            p_character := options.male[ar];
            p_characterInRain := options.maleinRain[ar];
            end
        else
            begin
            p_character := options.female[ar];
            p_characterInRain := options.femaleinRain[ar];
            end;
        end
    else
        begin
        age := Random(60);
        gender := Random(2);

        if age < 20 then
            begin
            ar := under20
            end
        else if age < 40 then
            begin
            ar := under60
            end
        else
            begin
            ar := over60;
            end;

        if gender = 0 then
            begin
            p_character := options.male[ar];
            p_characterInRain := options.maleinRain[ar];
            end
        else
            begin
            p_character := options.female[ar];
            p_characterInRain := options.femaleinRain[ar];
            end;
        end;


    end;

function MovingFeatureClass.Clone: MovingFeatureClass;
    var
        i: Integer;
    begin
    Result := MovingFeatureClass.Create;

    Result.p_properties    := p_properties;
    Result.p_temporalGeometry      := p_temporalGeometry;
    for I := 0 to p_temporalProperties.Count-1 do
        begin
        Result.p_temporalProperties.Add(p_temporalProperties[i].clone);
        end;

    Result.p_character := p_character;
    Result.p_histories.AddRange(p_histories);


    Result.p_startTime := p_startTime;
    Result.p_goalTime  := p_goalTime;
    Result.p_currentIndex      :=  p_currentIndex;
    Result.p_currentPosition   :=  p_currentPosition;
    Result.p_currentTime       :=  p_currentTime;

    Result.p_focusedHistories :=TList<TimeAndPositionType>.Create;
    Result.p_focusedHistories.AddRange(p_focusedHistories);

    Result.p_communicateMENGE   :=  p_communicateMENGE;

    Result.p_startPoint    := p_startPoint;
    Result.p_goalPoint     :=  p_goalPoint;
    Result.p_lastStateIndex  :=  0;
    Result.p_lastStateName   := p_lastStateName;
    end;

procedure MovingFeatureClass.CreateCharacterInstance(const aproject: IF8ProjectForRoad; isRain : Boolean);
    var
        ci  : Integer;
    begin
    ci := aproject.numberOfCharacters;
    if (ci < 1) then
        raise Exception.Create('キャラクターを先に作成してください。');

    if not isRain then
        begin
        if Assigned(p_character) then
            begin
            p_instance := aproject.CreateCharacterInstance(p_character, true);
            p_instance.onMove := Move;
            p_instance.RegisterOnBeforeDestructionProc(InstanceDestroy);
            end;
        end
    else
       begin
        if Assigned(p_characterinRain) then
            begin
            p_instance := aproject.CreateCharacterInstance(p_character, true);
            p_instance.onMove := Move;
            p_instance.RegisterOnBeforeDestructionProc(InstanceDestroy);
            end;
       end;
    end;

function MovingFeatureClass.FindProp(const name: String): TemporalPropertyClass;
    var
        i   : Integer;
    begin
    Result := nil;
    for i := 0 to p_temporalProperties.count-1 do
        begin
        if p_temporalProperties[i].name = name then
            begin
            Result := p_temporalProperties[i];
            break;
            end;
        end;
    end;

procedure MovingFeatureClass.FocusTheHistoryOfArea(const topLeftPos, downRightPos: GLPointType);
    var
        i   : Integer;
        gp  : GLPointType;
        firstIndex,
        finishIndex : Integer;
    begin
    firstIndex := -1;
    finishIndex := -1;
    if not Assigned(p_focusedHistories) then
        p_focusedHistories := TList<TimeAndPositionType>.Create;
    p_focusedHistories.Clear;

    for i := 0 to p_histories.Count - 1 do
        begin
        gp := p_histories[i].position;
        if InRange(gp[_x], topLeftPos[_x],downRightPos[_x]) and InRange(gp[_z], downRightPos[_z],topLeftPos[_z]) then
            begin
            firstIndex := i;
            break;
            end;
        end;

    for i := p_histories.Count - 1 downto 0 do
        begin
        gp := p_histories[i].position;
        if InRange(gp[_x], topLeftPos[_x],downRightPos[_x]) and InRange(gp[_z], downRightPos[_z],topLeftPos[_z]) then
            begin
            finishIndex := i;
            break;
            end;
        end;

    if (firstIndex =-1) or (finishIndex=-1) then
        Exit;

    for i := firstIndex to finishIndex do
        p_focusedHistories.Add(p_histories[i]);

    if p_focusedHistories.Count>0 then
        begin
        p_startTime := p_focusedHistories[0].time;
        p_startPoint := p_focusedHistories[0].position;
        p_goalPoint := p_focusedHistories[p_focusedHistories.Count - 1].position;
        p_goalTime := p_focusedHistories[p_focusedHistories.Count - 1].time;
        end;
    end;


function MovingFeatureClass.GetFocusedHistory(index: integer): TimeAndPositionType;
    begin
    if not Assigned(p_focusedHistories) then
        Exit;

    if p_focusedHistories.count> index then
        Result := p_focusedHistories[index];
    end;

function MovingFeatureClass.GetLastStateIndex: NativeInt;
    begin
    Result := p_lastStateIndex;
    end;


function MovingFeatureClass.GetLastStateName: String;
    begin
    Result := p_lastStateName;
    end;

function MovingFeatureClass.GetNumberOfFocusedHistories: integer;
    begin
    if not Assigned(p_focusedHistories) then
        Result := -1
    else
        Result := p_focusedHistories.count;
    end;

procedure MovingFeatureClass.InitialiseFocusedHistories;
    begin
    p_focusedHistories.Clear;
    if Assigned(p_temporalHistories) then
        p_temporalHistories.Clear;
    end;

procedure MovingFeatureClass.InitialiseHistories;
    begin
    p_histories.Clear;
    p_focusedHistories.Clear;
    if Assigned(p_temporalHistories) then
        p_temporalHistories.Clear;
    end;

procedure MovingFeatureClass.InstanceDestroy(Instance: IF8DBObject);
    var
        method : procedure(Instance: IF8DBObject) of Object;
    begin
    if Assigned(p_instance) then
        begin
        method := InstanceDestroy;
        if Assigned(method) then
            p_instance.UnregisterOnBeforeDestructionProc(method);

        p_instance.onMove := nil;
        end;
    p_instance := nil;
    end;

procedure MovingFeatureClass.MergeFrom(const amf: MovingFeatureClass);
    var
        i   : Integer;
    begin
    Assert(p_properties.name = amf.p_properties.name, 'The both name are not same.');

    if p_histories[0].time < amf.p_histories[0].time then
        begin
        if p_histories[p_histories.Count - 1].time < amf.p_histories[0].time then
            p_histories.AddRange(amf.p_histories)
        else
            begin
            for i := 0 to p_histories.Count - 1 do
                begin
                if p_histories[i].time > amf.p_histories[0].time then
                    begin
                    p_histories.InsertRange(i - 1, amf.p_histories);
                    break;
                    end;
                end;
            end;
        end
    else
        p_histories.InsertRange(0, amf.p_histories);
    end;

procedure MovingFeatureClass.Move(dTimeInSeconds: Double; Instance: IF8MovingObjectInstance);
    var
        now : TDatetime;
    begin
    if p_lastStateName = STATE_NAME_NONE then //シミュレーションされていないMF
        begin
        Instance.objectVisibility := false;
        Exit;
        end;

    if Assigned(FGetUpdatedTime) then
        begin
        now :=FGetUpdatedTime();

        if (CompareDateTime(p_startTime, now) = LessThanValue) and
           (CompareDateTime(now, p_goalTime) = LessThanValue) then
            begin
            Instance.objectVisibility := True;
            CalculatePosition(Instance.GetProject, now);
            end
        else
            begin//歩行時間外のMF
            Instance.objectVisibility := False;
            end;
        p_currentTime := now;
        end;
    end;

procedure MovingFeatureClass.Parse(const aproject: IF8ProjectForRoad);
    var
        i   : Integer;
        tap : TimeAndPositionType;
    begin
    Assert(Length(p_temporalGeometry.coordinates) = Length(p_temporalGeometry.datetimes), '座標と時刻の数があっていないです。');

    for i := Low(p_temporalGeometry.coordinates) to High(p_temporalGeometry.datetimes) do
        begin
        tap.time     := ISO8601ToDate(p_temporalGeometry.datetimes[i]);
        aproject.ConvertLatLongToPosition(p_temporalGeometry.coordinates[i][0], p_temporalGeometry.coordinates[i][1], tap.position);
        if Length(p_temporalGeometry.coordinates[i]) = 3 then
            begin
            tap.position[_y] := p_temporalGeometry.coordinates[i][2];
            end;
        p_histories.Add(tap);
        end;
    p_currentIndex := -1;

    if p_histories.Count > 0 then
        begin
        p_startPoint := p_histories[0].position;
        p_goalPoint := p_histories[p_histories.Count - 1].position;
        end;
    end;

procedure MovingFeatureClass.Pause;
    begin
    end;

procedure MovingFeatureClass.ReadJson(const ait: TJSONIterator);
    var
        p : TemporalPropertyClass;
    begin
    p_communicateMENGE := True;
    while ait.Next do
        begin
        if ait.Key = 'properties' then
            begin
            ait.Recurse;
            try
                p_properties.ReadJson(ait);
            finally
                ait.Return;
                end;
            end
        else if ait.Key = 'type' then
            Assert(ait.AsString = 'MovingFeatures', 'This type is MovingFeatures')
        else if ait.Key = 'temporalGeometry' then
            begin
            ait.Recurse;
            try
                p_temporalGeometry.ReadJson(ait);
            finally
                ait.Return;
                end;
            end
        else if ait.Key = 'temporalProperties' then
            begin
            ait.Recurse;
            try
                while ait.Next do
                    begin
                    ait.Recurse;
                    try
                        p := TemporalPropertyClass.ReadJson(ait);
                        p_temporalProperties.Add(p);
                    finally
                        ait.Return;
                        end;
                    end;
            finally
                ait.Return;
                end;
            end;

        end;

    end;

procedure MovingFeatureClass.RegisterGetUpdatedtime(value: TGetUpdatedtime);
    begin
    FGetUpdatedTime := value;
    end;

procedure MovingFeatureClass.UnRegisterGetUpdatedtime;
    begin
    FGetUpdatedTime := nil;
    end;

procedure MovingFeatureClass.Reset(const aproject: IF8ProjectForRoad);
    begin
    p_currentTime := p_startTime;

    CalculatePosition(aproject, p_startTime);
    end;

procedure MovingFeatureClass.SetInstance(const Value: IF8CharacterInstance);
    begin
    if Assigned(p_instance) then
        p_instance.UnRegisterOnBeforeDestructionProc(InstanceDestroy);
    p_instance := Value;
    if Assigned(p_instance) then
        p_instance.RegisterOnBeforeDestructionProc(InstanceDestroy);
    end;

procedure MovingFeatureClass.SetLastStateIndex(const Value: NativeInt);
    begin
    p_lastStateIndex := Value;
    end;

procedure MovingFeatureClass.SetLastStatename(const Value: String);
    begin
    p_LastStateName := value;
    end;

procedure MovingFeatureClass.SetMyCharacter(const Value: IF8QuakeIII);
    var
        project : IF8ProjectForRoad;
    begin
    p_character := Value;
    if Assigned(p_instance) then
        begin
        project := p_instance.GetProject;
        InstanceDestroy(p_instance);
        if Assigned(p_character) then
            CreateCharacterInstance(project,False);
        end;
    end;

procedure MovingFeatureClass.BeforeStart(const startTime: TDateTime; const aproject: IF8ProjectForRoad;isRain : Boolean);
    begin
    p_currentTime := startTime;
    CreateCharacterInstance(aproject,isRain);

    CalculatePosition(aproject, startTime);
    end;

procedure MovingFeatureClass.Stop(const aproject: IF8ProjectForRoad);
    begin
    InstanceDestroy(p_instance);
    aproject.DeleteCharacterInstance(p_instance);
    p_instance := nil;
    end;

{ TProperties }

procedure TProperties.ReadJson(const ait: TJSONIterator);
    begin
    if ait.Next('name') then
        name := ait.AsString;
    end;

{ TTemporalGeometry }

procedure TTemporalGeometry.ReadJson(const ait: TJSONIterator);
    var
        adlist  : TList<Double>;
        aval    : TCoordinate;
        aclist  : TList<TCoordinate>;
        aslist  : TStringList;
    begin
    while ait.Next do
        begin
        if ait.Key = 'interpolations' then
            interpolations := ait.AsString
        else if ait.Key = 'type' then
            atype := ait.AsString
        else if ait.Key = 'coordinates' then
            begin
            aclist := TList<TCoordinate>.Create;
            adlist := TList<Double>.Create;
            try
                ait.Recurse;
                while ait.Next do
                    begin
                    ait.Recurse;
                    while ait.Next do
                        adlist.Add(ait.AsDouble);
                    aval := adlist.ToArray;
                    acList.Add(aval);
                    adlist.Clear;
                    ait.Return;
                    end;

                coordinates := acList.ToArray;
            finally
                ait.Return;
                FreeAndNil(aclist);
                FreeAndNil(adlist);
                end;
            end
        else if ait.Key = 'datetimes' then
            begin
            ait.Recurse;
            aslist := TStringList.Create;
            try
                while ait.Next do
                    aslist.Add(ait.AsString);
                datetimes := aslist.ToStringArray;
            finally
                FreeAndNil(aslist);
                ait.Return;
                end;
            end;
        end;
    end;

initialization
    MovingFeatureListClass.p_list := nil;
    MovingFeatureListClass.p_Maglist := nil;
    MovingFeatureListClass.p_currentStateList := nil;

finalization
    if Assigned(MovingFeatureListClass.p_list) then
        FreeAndNil(MovingFeatureListClass.p_list);
    if Assigned(MovingFeatureListClass.p_Maglist) then
        FreeAndNil(MovingFeatureListClass.p_Maglist);
    if Assigned(MovingFeatureListClass.p_currentStateList) then
        FreeAndNil(MovingFeatureListClass.p_currentStateList);

end.
