unit TrafficSensorDat;

interface

uses
    System.SysUtils,
    System.DateUtils,
    System.Types,
    System.Classes,
    System.JSON,
    System.JSON.Readers,
    System.JSON.Builders,
    System.Generics.Collections,
    Vcl.Dialogs,
    PluginCore;

const
    INOUT_DATA_HEADER = 'time,in,out,all';
    _time  = 0;
    _in    = 1;
    _out   = 2;
    _IOall = 3;

    GENDEROLD_DATA_HEADER = 'time,0m,0f,10m,10f,20m,20f,30m,30f,40m,40f,50m,50f,60m,60f';
    _0m    = 1;
    _0f    = 2;
    _10m   = 3;
    _10f   = 4;
    _20m   = 5;
    _20f   = 6;
    _30m   = 7;
    _30f   = 8;
    _40m   = 9;
    _40f   = 10;
    _50m   = 11;
    _50f   = 12;
    _60m   = 13;
    _60f   = 14;

type
    TLoadedInOutValues = record
        time      : string;
        peoplein  : integer;
        peopleout : integer;
        peopleall : integer;
        end;

    TLoadedGenderOldValues = record
        time         : string;
        maleOver0    : integer;
        femaleOver0  : integer;
        maleOver10   : integer;
        femaleOver10 : integer;
        maleOver20   : integer;
        femaleOver20 : integer;
        maleOver30   : integer;
        femaleOver30 : integer;
        maleOver40   : integer;
        femaleOver40 : integer;
        maleOver50   : integer;
        femaleOver50 : integer;
        maleOver60   : integer;
        femaleOver60 : integer;
        peopleall    : integer;
        end;

    /// <summary>
    ///    読み込んだ断面交通流データのうち、FieldAnalyst forGateVer.8フォーマットのデータを管理するクラス
    /// </summary>
    TrafficSensorDatClass = class
        private
            p_inoutvalues     : TList<TLoadedInOutValues>;
            p_genderoldvalues : TList<TLoadedGenderOldValues>;

            procedure ReadInOutDataLine(const asr: TStreamReader; var datarec: TLoadedInOutValues);
            procedure ReadGenderOldDataLine(const asr: TStreamReader; var datarec: TLoadedGenderOldValues);
            function  GetInOutValues : TList<TLoadedInOutValues>;
            function  GetGenderOldValues : TList<TLoadedGenderOldValues>;
            function  SumPeople(var datarec: TLoadedGenderOldValues): integer;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure ClearInOutData;
            procedure ClearGenderOldData;
            procedure ReadInOutDat(const asr: TStreamReader; project: IF8ProjectForRoad);
            procedure ReadGenderOldDat(const asr: TStreamReader; project: IF8ProjectForRoad);

            property  InOutData       : TList<TLoadedInOutValues> read p_inoutvalues;
            property  GenderOldData   : TList<TLoadedGenderOldValues> read p_genderoldvalues;
        end;

implementation

{ TrafficSensorDatClass }
//==============================================================================
procedure TrafficSensorDatClass.AfterConstruction;
    begin
    inherited;
    p_inoutvalues := TList<TLoadedInOutValues>.Create;
    p_genderoldvalues := TList<TLoadedGenderOldValues>.Create;
    end;

//==============================================================================
procedure TrafficSensorDatClass.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_genderoldvalues);
    FreeAndNil(p_inoutvalues);
    end;

//==============================================================================
procedure TrafficSensorDatClass.ClearInOutData;
    begin
    if p_inoutvalues.Count > 0 then
        p_inoutvalues.Clear;
    end;

//==============================================================================
procedure TrafficSensorDatClass.ClearGenderOldData;
    begin
    if p_genderoldvalues.Count > 0 then
        p_genderoldvalues.Clear;
    end;

//==============================================================================
function TrafficSensorDatClass.GetInOutValues : TList<TLoadedInOutValues>;
    begin
    Result := p_inoutvalues;
    end;

//==============================================================================
function TrafficSensorDatClass.GetGenderOldValues : TList<TLoadedGenderOldValues>;
    begin
    Result := p_genderoldvalues;
    end;

//==============================================================================
procedure TrafficSensorDatClass.ReadInOutDat(const asr: TStreamReader; project: IF8ProjectForRoad);
    var
        a_inout  : TList<TLoadedInOutValues>;
        iorecord : TLoadedInOutValues;
        dataheader : string;
    begin
    a_inout := GetInOutValues;
    asr.ReadLine;
    dataheader := asr.ReadLine;

    if dataheader <> INOUT_DATA_HEADER then
        begin
        ShowMessage('インポートに失敗しました');
        raise Exception.Create('不適切なデータフォーマットです');
        end;

    while not asr.EndOfStream do
        begin
        ReadInOutDataLine(asr, iorecord);
        a_inout.Add(iorecord);
        end;

    p_inoutvalues := a_inout;
    end;

//==============================================================================
procedure TrafficSensorDatClass.ReadGenderOldDat(const asr: TStreamReader; project: IF8ProjectForRoad);
    var
        a_genderold : TList<TLoadedGenderOldValues>;
        gorecord    : TLoadedGenderOldValues;
        dataheader : string;
    begin
    a_genderold := GetGenderOldValues;
    asr.ReadLine;
    dataheader := asr.ReadLine;

    if dataheader <> GENDEROLD_DATA_HEADER then
        begin
        ShowMessage('インポートに失敗しました');
        raise Exception.Create('不適切なデータフォーマットです');
        end;

    while not asr.EndOfStream do
        begin
        ReadGenderOldDataLine(asr, gorecord);
        a_genderold.Add(gorecord);
        end;

    p_genderoldvalues := a_genderold;
    end;

//==============================================================================
procedure TrafficSensorDatClass.ReadInOutDataLine(const asr: TStreamReader; var datarec: TLoadedInOutValues);
    var
        linestr : string;
    begin
    linestr := asr.ReadLine;
    datarec.time      := linestr.Split([','])[_time];
    datarec.peoplein  := StrToInt(linestr.Split([','])[_in]);
    datarec.peopleout := StrToInt(linestr.Split([','])[_out]);
    datarec.peopleall := StrToInt(linestr.Split([','])[_IOall]);
    end;

//==============================================================================
procedure TrafficSensorDatClass.ReadGenderOldDataLine(const asr: TStreamReader; var datarec: TLoadedGenderOldValues);
    var
        linestr : string;
    begin
    linestr := asr.ReadLine;
    datarec.time      := linestr.Split([','])[_time];
    datarec.maleOver0  := StrToInt(linestr.Split([','])[_0m]);
    datarec.femaleOver0 := StrToInt(linestr.Split([','])[_0f]);
    datarec.maleOver10  := StrToInt(linestr.Split([','])[_10m]);
    datarec.femaleOver10 := StrToInt(linestr.Split([','])[_10f]);
    datarec.maleOver20  := StrToInt(linestr.Split([','])[_20m]);
    datarec.femaleOver20 := StrToInt(linestr.Split([','])[_20f]);
    datarec.maleOver30  := StrToInt(linestr.Split([','])[_30m]);
    datarec.femaleOver30 := StrToInt(linestr.Split([','])[_30f]);
    datarec.maleOver40  := StrToInt(linestr.Split([','])[_40m]);
    datarec.femaleOver40 := StrToInt(linestr.Split([','])[_40f]);
    datarec.maleOver50  := StrToInt(linestr.Split([','])[_50m]);
    datarec.femaleOver50 := StrToInt(linestr.Split([','])[_50f]);
    datarec.maleOver60  := StrToInt(linestr.Split([','])[_60m]);
    datarec.femaleOver60 := StrToInt(linestr.Split([','])[_60f]);
    datarec.peopleall := SumPeople(datarec);
    end;

//==============================================================================
function TrafficSensorDatClass.SumPeople(var datarec: TLoadedGenderOldValues): integer;
    begin
    Result := datarec.maleOver0 + datarec.femaleOver0 +
              datarec.maleOver10 + datarec.femaleOver10 +
              datarec.maleOver20 + datarec.femaleOver20 +
              datarec.maleOver30 + datarec.femaleOver30 +
              datarec.maleOver40 + datarec.femaleOver40 +
              datarec.maleOver50 + datarec.femaleOver50 +
              datarec.maleOver60 + datarec.femaleOver60;
    end;
end.
