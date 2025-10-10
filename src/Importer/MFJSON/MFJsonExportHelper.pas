unit MFJsonExportHelper;

interface

uses
    System.SysUtils,
    System.DateUtils,
    System.Types,
    System.Classes,
    System.JSON,
    System.JSON.Types,
    System.JSON.Readers,
    System.JSON.Builders,
    System.JSON.Serializers,
    System.Generics.Collections,
    System.Math,
    TemporalProperty,
    PluginCore,
    F8Utils,
    MFJsonLoaderOptions,
    MovingFeature,
    MFJsonExportUtils,
    PedestrianMovingData;

type
    /// <summary>
    ///    シミュレーション計算結果に基づいてMF-Jsonを出力するクラス
    /// </summary>
    TMFJsonExportHelper = class
        private
            p_HoriConv : IF8WrHorizontalCoordinateConvertor;
            p_ExportID : integer;
            function  SetExportPMData(const MFData: TList<TPedestrianMovingData>; expInterval: double;
                var ExpData: TArray<TMFJsonExportData>; Serializer: TJsonSerializer): string;
            procedure SetPMProperties(const MF: TPedestrianMovingData; var ExportProp: TExportProperties);
            procedure SetPMtemporalGeometry(const MF: TPedestrianMovingData; expInterval: double; var ExporttmpG: TExporttemporalGeometry);
        public
            function SetMFJsonData(const api: IF8ApplicationServices; MFData: TList<TPedestrianMovingData>; expInterval: double; var Serializer: TJsonSerializer): string;
        end;

implementation

uses
    AgentSettings;

{ TMFJsonExportHelper }
//==============================================================================
function TMFJsonExportHelper.SetMFJsonData(const api: IF8ApplicationServices; MFData: TList<TPedestrianMovingData>; expInterval: double; var Serializer: TJsonSerializer): string;
    var
        ExportData : TArray<TMFJsonExportData>;
    begin
    p_HoriConv := api.GetWRCoordinateConvertor.HoirizontalCSConvertor;
    Serializer.Formatting := TJsonFormatting.Indented;
    p_ExportID := 0;
    if MFData.Count < 1 then
        Exit;

    Result := SetExportPMData(MFData, expInterval, ExportData, Serializer);
    end;

//==============================================================================
function TMFJsonExportHelper.SetExportPMData(const MFData: TList<TPedestrianMovingData>; expInterval: double;
    var ExpData: TArray<TMFJsonExportData>; Serializer: TJsonSerializer): string;
    var
        i : integer;
    begin
    SetLength(ExpData, MFData.Count);
    for i := 0 to MFData.Count - 1 do
        begin
        SetPMProperties(MFData[i], ExpData[i].properties);
        ExpData[i]._type := 'MovingFeatures';
        SetPMtemporalGeometry(MFData[i], expInterval, ExpData[i].temporalGeometry);
        end;

    Result := Serializer.Serialize<TArray<TMFJsonExportData>>(ExpData);
    end;

//==============================================================================
procedure TMFJsonExportHelper.SetPMProperties(const MF: TPedestrianMovingData; var ExportProp: TExportProperties);
    var
        AgentSetting : TAgentSettingsType;
    begin
    ExportProp.name := p_ExportID.ToString;
    AgentSetting := LoadAgentSettings;
    if AgentSetting.IsRain then
        ExportProp.weather := 'rainny'
    else
        ExportProp.weather := 'sunny';

    ExportProp.gender := MF.Gender.ToString;
    ExportProp.age := MF.Age.ToString;
    p_ExportID := p_ExportID + 1;
    end;

//==============================================================================
procedure TMFJsonExportHelper.SetPMtemporalGeometry(const MF: TPedestrianMovingData; expInterval: double; var ExporttmpG: TExporttemporalGeometry);

    //--------------------------------------------------------------------------
    procedure ConvertAndSet(const aMF: TPedestrianMovingData; ExpIdx, MFIdx: integer; var exptmpG: TExporttemporalGeometry);
        var
            GLCoord, LatLon : F8PointType;
        begin
        GLCoord[_x] := aMF.MovingLog[MFIdx].position[_x];
        GLCoord[_y] := aMF.MovingLog[MFIdx].position[_z];
        p_HoriConv.Convert(_hctOpenGL_XZ, 0, _hctWGS84_LonLat, 6668, GLCoord, LatLon);
        exptmpG.coordinates[ExpIdx][0] := LatLon[_y];
        exptmpG.coordinates[ExpIdx][1] := LatLon[_x];
        exptmpG.coordinates[ExpIdx][2] := aMF.MovingLog[MFIdx].position[_y];
        exptmpG.datetimes[ExpIdx]      := DateToISO8601(aMF.MovingLog[MFIdx].time);
        end;

    //--------------------------------------------------------------------------
    procedure SetPositionAndTimeByInterval(const aMF: TPedestrianMovingData; interval: double; var exptmpG: TExporttemporalGeometry);
        var
            i, ExportIdx : integer;
            DataLength   : integer;
            preTime      : TDateTime;
        begin
        DataLength := 0;
        preTime    := 0;
        for i := 0 to MF.MovingLog.Count - 1 do
            begin
            if (i = 0) or (i = MF.MovingLog.Count - 1) then
                begin
                DataLength := DataLength + 1;
                preTime    := aMF.MovingLog[i].time;
                end
            else
                begin
                if SecondsBetween(aMF.MovingLog[i].time, preTime) >= Trunc(interval) then
                    begin
                    DataLength := DataLength + 1;
                    preTime    := aMF.MovingLog[i].time;
                    end;
                end;
            end;

        preTime   := 0;
        ExportIdx := 0;
        SetLength(exptmpG.coordinates, DataLength);
        SetLength(exptmpG.datetimes,   DataLength);
        for i := 0 to MF.MovingLog.Count - 1 do
            begin
            if i = 0 then
                begin
                ExportIdx := 0;
                ConvertAndSet(aMF, ExportIdx, i, exptmpG);
                preTime   := aMF.MovingLog[i].time;
                ExportIdx := ExportIdx + 1;
                end
            else if i = MF.MovingLog.Count - 1 then
                begin
                ExportIdx  := Length(exptmpG.coordinates) - 1;
                ConvertAndSet(aMF, ExportIdx, i, exptmpG);
                preTime    := aMF.MovingLog[i].time;
                end
            else
                begin
                if SecondsBetween(aMF.MovingLog[i].time, preTime) >= Trunc(interval) then
                    begin
                    ConvertAndSet(aMF, ExportIdx, i, exptmpG);
                    preTime    := aMF.MovingLog[i].time;
                    ExportIdx := ExportIdx + 1;
                    end;
                end;
            end;
        end;

    //--------------------------------------------------------------------------
    procedure SetAllPositionAndTime(const aMF: TPedestrianMovingData; var exptmpG: TExporttemporalGeometry);
        var
            i : integer;
        begin
        SetLength(exptmpG.coordinates, aMF.MovingLog.Count);
        SetLength(exptmpG.datetimes, aMF.MovingLog.Count);
        for i := 0 to MF.MovingLog.Count - 1 do
            ConvertAndSet(aMF, i, i, exptmpG);
        end;

    //--------------------------------------------------------------------------
    begin
    ExporttmpG.interpolations := 'Linear';
    ExporttmpG._type          := 'MovingPoint';
    if Trunc(expInterval) <> 0 then
        SetPositionAndTimeByInterval(MF, expInterval, ExporttmpG)
    else
        SetAllPositionAndTime(MF, ExporttmpG);
    end;
end.
