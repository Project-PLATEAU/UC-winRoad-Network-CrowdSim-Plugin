unit AgentSettings;

interface

uses
    PluginCore,
    F8CrowdSimPluginDataConstant;

type
    TAgentSettingsType = Record
        simPerPedestrainData: Integer;
        pedestrainRadiusInSunny: double;
        pedestrainRadiusInRainny: double;
        maxWalkSpeed: double;
        positionTolerance : double;

        IsUseMagnificationFactor : Boolean;
        UseMagnificationType : Integer;
        PopulationValue : Integer;
        UUValue : Integer;
        RealMagnification : Integer;

        IsRain : Boolean;
        procedure Initialize;

    end;
const
    MALE = 0;
    FEMALE = 1;
    YOUNG = 0;
    ADULT = 1;
    OLD   = 2;
    SUNNY = 0;
    RAINNY = 1;

type
    TModelSettingType = record
        UnknownModel  : TGUID;
        Models : Array [MALE..FEMALE] of Array [YOUNG..OLD] of Array [SUNNY..RAINNY] of TGUID;
        procedure Initialize;
    end;

    MangnificationType = (Population,RealUU,CrossTraffic);



function  LoadAgentSettings :  TAgentSettingsType;
procedure SaveAgentSettings(data : TAgentSettingsType);
function  LoadModelSettings : TModelSettingType;
procedure SaveModelSettings(data : TModelSettingType);

implementation

uses
    System.SysUtils;

//==============================================================================
function  ConvertToGUID(const aValue : String; out guid : TGUID) : Boolean;
    var
        l : integer;
    begin
    Result := true;
    l := Length(TGUID.Empty.ToString);

    if l <> Length(aValue) then
        begin
        guid := TGUID.Empty;
        Exit(false);
        end;

    try
        guid := StringToGUID(aValue);
    except
        on E : EConvertError do
            begin
            guid := TGUID.Empty;
            Result := false;
            end;
        end;
    end;

//==============================================================================
function  LoadAgentSettings :  TAgentSettingsType;
    var
        project : IF8ProjectForRoad;
    begin
    project :=  theApplicationServices.project;
    Result.Initialize;
    if Assigned(project) then
        begin
        Result.simPerPedestrainData     := StrToIntDef(project.pluginData[F8_CROWD_SIM_PLUGIN_ID,   SIM_PER_PEDESTRAIN],  Result.simPerPedestrainData);
        Result.pedestrainRadiusInSunny  := StrToFloatDef(project.pluginData[F8_CROWD_SIM_PLUGIN_ID, SUNNY_PEDESTRAIN_RADIUS],   Result.pedestrainRadiusInSunny);
        Result.pedestrainRadiusInRainny := StrToFloatDef(project.pluginData[F8_CROWD_SIM_PLUGIN_ID, RAINNY_PEDESTRAIN_RADIUS],   Result.pedestrainRadiusInRainny);
        Result.maxWalkSpeed             := StrToFloatDef(project.pluginData[F8_CROWD_SIM_PLUGIN_ID, MAX_WALK_SPEED],      Result.maxWalkSpeed);
        Result.positionTolerance        := StrToFloatDef(project.pluginData[F8_CROWD_SIM_PLUGIN_ID, POS_TORLERANCE],      Result.positionTolerance);
        Result.IsUseMagnificationFactor := StrToBoolDef(project.pluginData[F8_CROWD_SIM_PLUGIN_ID, IS_USE_MAG_FACTOR],False);
        Result.UseMagnificationType     := StrToIntDef(project.pluginData[F8_CROWD_SIM_PLUGIN_ID, MAG_VALUE_TYPE],0);
        Result.PopulationValue          := StrToIntDef(project.pluginData[F8_CROWD_SIM_PLUGIN_ID, PopulationValue],0);
        Result.UUValue                  := StrToIntDef(project.pluginData[F8_CROWD_SIM_PLUGIN_ID, UU_VALUE],10000);
        Result.RealMagnification        := StrToIntDef(project.pluginData[F8_CROWD_SIM_PLUGIN_ID, REAL_MAG],10);
        Result.IsRain                   := StrToBoolDef(project.pluginData[F8_CROWD_SIM_PLUGIN_ID, IS_RAIN],False);
        end;
    end;

//==============================================================================
procedure SaveAgentSettings(data : TAgentSettingsType);
    var
        project : IF8ProjectForRoad;
    begin
    project :=  theApplicationServices.project;
    if Assigned(project) then
        begin
        project.pluginData[F8_CROWD_SIM_PLUGIN_ID,   SIM_PER_PEDESTRAIN] := IntToStr(data.simPerPedestrainData);
        project.pluginData[F8_CROWD_SIM_PLUGIN_ID, SUNNY_PEDESTRAIN_RADIUS]    := FloatToStr(data.pedestrainRadiusInSunny);
        project.pluginData[F8_CROWD_SIM_PLUGIN_ID, RAINNY_PEDESTRAIN_RADIUS]    := FloatToStr(data.pedestrainRadiusInRainny);
        project.pluginData[F8_CROWD_SIM_PLUGIN_ID, MAX_WALK_SPEED]       := FloatToStr(data.maxWalkSpeed);
        project.pluginData[F8_CROWD_SIM_PLUGIN_ID, POS_TORLERANCE]       := FloatToStr(data.positionTolerance);

        project.pluginData[F8_CROWD_SIM_PLUGIN_ID, IS_USE_MAG_FACTOR] := BoolToStr(data.IsUseMagnificationFactor);
        project.pluginData[F8_CROWD_SIM_PLUGIN_ID, MAG_VALUE_TYPE]    := IntToStr(data.UseMagnificationType);
        project.pluginData[F8_CROWD_SIM_PLUGIN_ID, PopulationValue]   := intToStr(data.PopulationValue);
        project.pluginData[F8_CROWD_SIM_PLUGIN_ID, UU_VALUE]          :=IntToStr(data.UUValue);
        project.pluginData[F8_CROWD_SIM_PLUGIN_ID, REAL_MAG]          := IntToStr(data.RealMagnification);

        project.pluginData[F8_CROWD_SIM_PLUGIN_ID, IS_RAIN]              := BoolToStr(data.IsRain);
        end;
    end;

//==============================================================================
procedure SaveModelSettings(data : TModelSettingType);
    //==============================================================================
    var
        project : IF8ProjectForRoad;
    procedure SaveModelSetting(mf,age,wth : Integer; id : TGUID);
        begin
        project.pluginData[F8_CROWD_SIM_PLUGIN_ID,MODEL_SETTING+IntToStr(mf)+IntToStr(age)+IntToStr(wth)] := GUIDtoString(id);
        end;
    //==============================================================================
    var
        i,j,k : Integer;
    begin
    project :=  theApplicationServices.project;
    if Assigned(project) then
        begin
        project.pluginData[F8_CROWD_SIM_PLUGIN_ID,MODEL_SETTING+UNKNOWN] := GUIDtoString(data.UnknownModel);
        for I := MALE to FEMALE do
            for j := YOUNG to OLD do
                for k := SUNNY to RAINNY do
                    begin
                    SaveModelSetting(i,j,k,data.Models[i,j,k]);
                    end;
        end;
    end;

//==============================================================================
function  LoadModelSettings : TModelSettingType;
    //==========================================================================
    var
        project : IF8ProjectForRoad;
    function LoadModelSetting(mf,age,wth : Integer) : TGUID;
        var
            id : TGUID;
            str : String;
        begin
        str :=project.pluginData[F8_CROWD_SIM_PLUGIN_ID,MODEL_SETTING+IntToStr(mf)+IntToStr(age)+IntToStr(wth)];
        if ConvertToGUID(str,id) then
            Result := id
        else
            Result := TGUID.Empty;
        end;
    //==========================================================================
    var
        i,j,k : Integer;
    begin
    project :=  theApplicationServices.project;
    if Assigned(project) then
        begin
        ConvertToGUID(project.pluginData[F8_CROWD_SIM_PLUGIN_ID,MODEL_SETTING+UNKNOWN],Result.UnknownModel);

        for I := MALE to FEMALE do
            for j := YOUNG to OLD do
                for k := SUNNY to RAINNY do
                    begin
                    Result.Models[i,j,k] := LoadModelSetting(i,j,k);
                    end;
        end;
    end;

{ TAgentSettingsType }
//==============================================================================
procedure TAgentSettingsType.Initialize;
    begin
    simPerPedestrainData := 100;
    pedestrainRadiusInSunny:= 0.5;
    pedestrainRadiusInRainny:= 1.0;
    maxWalkSpeed := 4.0;
    positionTolerance := 8.0;
    IsRain := False;
    end;

{ TModelSettingType }
//==============================================================================
procedure TModelSettingType.Initialize;
    var
        eGUID : TGUID;
        i,j,k : Integer;
    begin
    eGUID := TGUID.Empty;
    self.UnknownModel := eGUID;
    for I := MALE to FEMALE do
            for j := YOUNG to OLD do
                for k := SUNNY to RAINNY do
                    begin
                    Self.Models[i,j,k] := eGUID;
                    end;
    end;

end.
