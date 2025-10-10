unit TrafficSensor;

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
    PluginCore;

type
    TnumberInfo = record
        parentid  : string;
        id        : string;
        attrName  : string;
        attrType  : string;
        attrValue : integer;
        recvTime  : TDateTime;
        procedure initialize;
        end;

    TdoubleInfo = record
        parentid  : string;
        id        : string;
        attrName  : string;
        attrType  : string;
        attrValue : double;
        recvTime  : TDateTime;
        procedure initialize;
        end;

    TstringInfo = record
        parentid  : string;
        id        : string;
        attrName  : string;
        attrType  : string;
        attrValue : string;
        recvTime  : TDateTime;
        procedure initialize;
        end;

    TDateTimeInfo = record
        parentid  : string;
        id        : string;
        attrName  : string;
        attrType  : string;
        attrValue : TDateTime;
        recvTime  : TDateTime;
        procedure initialize;
        end;

    TLatestEventDetails = record
        LatestdataCreated     : TDateTimeInfo;
        LatestdateModified    : TDateTimeInfo;
        LatestdetailedUrl     : TstringInfo;
        LateststartDate       : TstringInfo;
        LatestendDate         : TstringInfo;
        Latestimage           : TstringInfo;
        Latestname            : TstringInfo;
        Latestno              : TstringInfo;
        Latestsummary         : TstringInfo;
        Latestlatitude        : TdoubleInfo;
        Latestlongitude       : TdoubleInfo;
        LatestlocationName    : TstringInfo;
        LatestlocationAddress : TstringInfo;
        end;

    TLoadedEventValues = record
        id              : string;
        dataCreated     : TList<TDateTimeInfo>;
        dateModified    : TList<TDateTimeInfo>;
        detailedUrl     : TList<TstringInfo>;
        startDate       : TList<TstringInfo>;
        endDate         : TList<TstringInfo>;
        image           : TList<TstringInfo>;
        name            : TList<TstringInfo>;
        no              : TList<TstringInfo>;
        summary         : TList<TstringInfo>;
        latitude        : TList<TdoubleInfo>;
        longitude       : TList<TdoubleInfo>;
        locationName    : TList<TstringInfo>;
        locationAddress : TList<TstringInfo>;
        latestdetails   : TLatestEventDetails;
        procedure ReadDateTime(const ait: TJSONIterator; var date: TDateTime);
        procedure ReadDateTimeList(const ait: TJSONIterator; pid, valname: string; recv: TDateTime; var dtinfo: TDateTimeInfo);
        procedure ReadStringList(const ait: TJSONIterator; pid, valname: string; recv: TDateTime; var sinfo: TstringInfo);
        procedure ReadDoubleList(const ait: TJSONIterator; pid, valname: string; recv: TDateTime; var dninfo: TdoubleInfo);
        procedure ReadTSJson(const ait: TJSONIterator; Sid: string);
        procedure SetDetailLists(const details; ImportedSensor, ImportedDetail: boolean);
        procedure CreateDetailLists;
        procedure ClearDetailLists;
        end;

    TLatestBlesensorDetails = record
        LatestdateObservedFrom        : TDateTimeInfo;
        LatestdateObservedTo          : TDateTimeInfo;
        LatestdateRetrieved           : TDateTimeInfo;
        Latestidentifcation           : TstringInfo;
        LatestpeopleCountNear         : TnumberInfo;
        LatestpeopleCountFar          : TnumberInfo;
        LatestpeopleCountImmedate     : TnumberInfo;
        LatestpeopleOccupancyNear     : TdoubleInfo;
        LatestpeopleOccupancyFar      : TdoubleInfo;
        LatestpeopleOccupancyImmedate : TdoubleInfo;
        end;

    TLoadedBlesensorValues = record
        id                      : string;
        dateObservedFrom        : TList<TDateTimeInfo>;
        dateObservedTo          : TList<TDateTimeInfo>;
        dateRetrieved           : TList<TDateTimeInfo>;
        identifcation           : TList<TstringInfo>;
        latitude                : double;
        longitude               : double;
        locationName            : string;
        peopleCountNear         : TList<TnumberInfo>;
        peopleCountFar          : TList<TnumberInfo>;
        peopleCountImmedate     : TList<TnumberInfo>;
        peopleOccupancyNear     : TList<TdoubleInfo>;
        peopleOccupancyFar      : TList<TdoubleInfo>;
        peopleOccupancyImmedate : TList<TdoubleInfo>;
        latestdetails           : TLatestBlesensorDetails;
        procedure ReadDateTime(const ait: TJSONIterator; var date: TDateTime);
        procedure ReadDateTimeInfo(const ait: TJSONIterator; pid, valname: string; recv: TDateTime; var dtinfo: TDateTimeInfo);
        procedure ReadStringInfo(const ait: TJSONIterator; pid, valname: string; recv: TDateTime; var sinfo: TstringInfo);
        procedure ReadNumberInfo(const ait: TJSONIterator; pid, valname: string; recv: TDateTime; var ninfo: TnumberInfo);
        procedure ReadDoubleNumInfo(const ait: TJSONIterator; pid, valname: string; recv: TDateTime; var dninfo: TdoubleInfo);
        procedure ReadString(const ait: TJSONIterator; var strdata: string);
        procedure ReadDouble(const ait: TJSONIterator; var valdata: double);
        procedure ReadTSJson(const ait: TJSONIterator; Sid: string);
        procedure SetDetailLists(const details; ImportedSensor, ImportedDetail: boolean);
        procedure CreateDetailLists;
        procedure ClearDetailLists;
        end;

    /// <summary>
    ///    読み込んだ断面交通流データをセンサのIDごとに管理するクラス
    /// </summary>
    TrafficSensorClass = class
        private
            p_loadedeventvalues     : TLoadedEventValues;
            p_loadedblesensorvalues : TLoadedBlesensorValues;
            p_ID                    : string;

            p_dataCreated           : TList<TDateTimeInfo>;
            p_dateModified          : TList<TDateTimeInfo>;
            p_detailedUrl           : TList<TstringInfo>;
            p_startDate             : TList<TstringInfo>;
            p_endDate               : TList<TstringInfo>;
            p_image                 : TList<TstringInfo>;
            p_name                  : TList<TstringInfo>;
            p_no                    : TList<TstringInfo>;
            p_summary               : TList<TstringInfo>;
            p_latitude              : TList<TdoubleInfo>;
            p_longitude             : TList<TdoubleInfo>;
            p_locationName          : TList<TstringInfo>;
            p_locationAddress       : TList<TstringInfo>;

            p_dateObservedFrom        : TList<TDateTimeInfo>;
            p_dateObservedTo          : TList<TDateTimeInfo>;
            p_dateRetrieved           : TList<TDateTimeInfo>;
            p_identifcation           : TList<TstringInfo>;
            p_peopleCountFar          : TList<TnumberInfo>;
            p_peopleCountNear         : TList<TnumberInfo>;
            p_peopleCountImmedate     : TList<TnumberInfo>;
            p_peopleOccupancyFar      : TList<TdoubleInfo>;
            p_peopleOccupancyNear     : TList<TdoubleInfo>;
            p_peopleOccupancyImmedate : TList<TdoubleInfo>;

            procedure   MergeFrom(const ats: TrafficSensorClass);
            procedure   Setdetails(const ait: TJSONIterator);
            procedure   SetDateTimeInfo(const ait: TJSONIterator; var datedata: TList<TDateTimeInfo>; IsUTC: boolean = true);
            procedure   SetStringInfo(const ait: TJSONIterator; var strdata: TList<TStringInfo>);
            procedure   SetDoubleInfo(const ait: TJSONIterator; var ddata: TList<TDoubleInfo>);
            procedure   SetNumberInfo(const ait: TJSONIterator; var ndata: TList<TNumberInfo>);
            procedure   SetDoubleNumberInfo(const ait: TJSONIterator; var dndata: TList<TDoubleInfo>);
            procedure   InitializeDetails;
            procedure   FreeDetails;
        public
            procedure   AfterConstruction; override;
            procedure   BeforeDestruction; override;
            procedure   ReadTSJson(const ait: TJSONIterator; sensorID: string);
            procedure   ReadTSDetailJson(const ait: TJSONIterator);

            property    ID            : string read p_ID;
            property    EventData     : TLoadedEventValues read p_loadedeventvalues;
            property    BleSensorData : TLoadedBlesensorValues read p_loadedblesensorvalues;
        end;

    /// <summary>
    ///    TrafficSensorClassのリスト
    ///    リストに加えて、保持している全てのTrafficSensorClassに対して行う処理等を配置している
    /// </summary>
    TrafficSensorListClass = class
        private
            p_ImportedSensorsData  : boolean;
            p_ImportedDetailData   : boolean;
            p_ImportedID           : TList<string>;

            p_list            : TObjectList<TrafficSensorClass>;
            p_detaillist      : TObjectList<TrafficSensorClass>;

            function GetList: TObjectList<TrafficSensorClass>;
            function GetDetailList: TObjectList<TrafficSensorClass>;
            function GetTrafficSensorData(const idx: Integer): TrafficSensorClass;
            function GetNumberOfTrafficSensorData: Integer;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure ReadTSJson(const ait: TJSONIterator; const project: IF8ProjectForRoad);
            procedure ReadTSDetailJson(const ait: TJSONIterator; const project: IF8ProjectForRoad);

            procedure ClearSensorData;
            procedure ClearDetail;

            property numberOfTrafficSensorData: Integer read GetNumberOfTrafficSensorData;
            property TrafficSensorData[const idx: Integer]  : TrafficSensorClass read GetTrafficSensorData;
        end;

implementation

{ TrafficSensorClass }
//==============================================================================
procedure TrafficSensorClass.AfterConstruction;
    begin
    inherited;
    InitializeDetails;
    end;

//==============================================================================
procedure TrafficSensorClass.BeforeDestruction;
    begin
    inherited;
    FreeDetails;
    end;

//==============================================================================
procedure TrafficSensorClass.MergeFrom(const ats: TrafficSensorClass);
    begin
    if Assigned(p_dataCreated) and Assigned(ats.p_dataCreated) then
        p_dataCreated.AddRange(ats.p_dataCreated);
    if Assigned(p_dateModified) and Assigned(ats.p_dateModified) then
        p_dateModified.AddRange(ats.p_dateModified);
    if Assigned(p_detailedUrl) and Assigned(ats.p_detailedUrl) then
        p_detailedUrl.AddRange(ats.p_detailedUrl);
    if Assigned(p_startDate) and Assigned(ats.p_startDate) then
        p_startDate.AddRange(ats.p_startDate);
    if Assigned(p_endDate) and Assigned(ats.p_endDate) then
        p_endDate.AddRange(ats.p_endDate);
    if Assigned(p_image) and Assigned(ats.p_image) then
        p_image.AddRange(ats.p_image);
    if Assigned(p_name) and Assigned(ats.p_name) then
        p_name.AddRange(ats.p_name);
    if Assigned(p_no) and Assigned(ats.p_no) then
        p_no.AddRange(ats.p_no);
    if Assigned(p_summary) and Assigned(ats.p_summary) then
        p_summary.AddRange(ats.p_summary);
    if Assigned(p_latitude) and Assigned(ats.p_latitude) then
        p_latitude.AddRange(ats.p_latitude);
    if Assigned(p_longitude) and Assigned(ats.p_longitude) then
        p_longitude.AddRange(ats.p_longitude);
    if Assigned(p_locationName) and Assigned(ats.p_locationName) then
        p_locationName.AddRange(ats.p_locationName);
    if Assigned(p_locationAddress) and Assigned(ats.p_locationAddress) then
        p_locationAddress.AddRange(ats.p_locationAddress);

    if Assigned(p_dateObservedFrom) and Assigned(ats.p_dateObservedFrom) then
        p_dateObservedFrom.AddRange(ats.p_dateObservedFrom);
    if Assigned(p_dateObservedTo) and Assigned(ats.p_dateObservedTo) then
        p_dateObservedTo.AddRange(ats.p_dateObservedTo);
    if Assigned(p_dateRetrieved) and Assigned(ats.p_dateRetrieved) then
        p_dateRetrieved.AddRange(ats.p_dateRetrieved);
    if Assigned(p_identifcation) and Assigned(ats.p_identifcation) then
        p_identifcation.AddRange(ats.p_identifcation);
    if Assigned(p_peopleCountFar) and Assigned(ats.p_peopleCountFar) then
        p_peopleCountFar.AddRange(ats.p_peopleCountFar);
    if Assigned(p_peopleCountNear) and Assigned(ats.p_peopleCountNear) then
        p_peopleCountNear.AddRange(ats.p_peopleCountNear);
    if Assigned(p_peopleCountImmedate) and Assigned(ats.p_peopleCountImmedate) then
        p_peopleCountImmedate.AddRange(ats.p_peopleCountImmedate);
    if Assigned(p_peopleOccupancyFar) and Assigned(ats.p_peopleOccupancyFar) then
        p_peopleOccupancyFar.AddRange(ats.p_peopleOccupancyFar);
    if Assigned(p_peopleOccupancyNear) and Assigned(ats.p_peopleOccupancyNear) then
        p_peopleOccupancyNear.AddRange(ats.p_peopleOccupancyNear);
    if Assigned(p_peopleOccupancyImmedate) and Assigned(ats.p_peopleOccupancyImmedate) then
        p_peopleOccupancyImmedate.AddRange(ats.p_peopleOccupancyImmedate);
    end;

//==============================================================================
procedure TrafficSensorClass.ReadTSJson(const ait: TJSONIterator; sensorID: string);
    begin
    p_ID := sensorID;
    while ait.Next do
        begin
        if ait.Key = 'type' then
            begin
            if ait.AsString = 'null' then
                Exit
            else if ait.AsString = 'Event' then
                begin
                p_loadedeventvalues.ReadTSJson(ait, sensorID);
                end
            else
                begin
                p_loadedblesensorvalues.ReadTSJson(ait, sensorID);
                end;
            end;
        end;
    end;

//==============================================================================
procedure TrafficSensorClass.ReadTSDetailJson(const ait: TJSONIterator);
    begin
    ait.Rewind;
    while ait.Next do
        begin
        ait.Recurse;
        if ait.Key = 'attributes' then
            begin
            ait.Return;
            while ait.Next do
                begin
                if ait.Key = 'id' then
                    p_id := ait.AsString;
                end;
            end;
        end;

    ait.Rewind;
    while ait.Next do
        begin
        ait.Recurse;
        if ait.Key = 'attributes' then
            Setdetails(ait);
        end;
    end;

//==============================================================================
procedure TrafficSensorClass.Setdetails(const ait: TJSONIterator);
    begin
    ait.Recurse;
    while ait.Next do
        begin
        ait.Recurse;
        if ait.Key = 'name' then
            begin
            if ait.AsString = 'dateObservedFrom' then
                SetDateTimeInfo(ait, p_dateObservedFrom)
            else if ait.AsString = 'dateObservedTo' then
                SetDateTimeInfo(ait, p_dateObservedTo)
            else if ait.AsString = 'dateRetrieved' then
                SetDateTimeInfo(ait, p_dateRetrieved)
            else if ait.AsString = 'identifcation' then
                SetStringInfo(ait, p_identifcation)
            else if ait.AsString = 'peopleCount_far' then
                SetNumberInfo(ait, p_peopleCountfar)
            else if ait.AsString = 'peopleCount_near' then
                SetNumberInfo(ait, p_peopleCountnear)
            else if ait.AsString = 'peopleCount_immedate' then
                SetNumberInfo(ait, p_peopleCountImmedate)
            else if ait.AsString = 'peopleOccupancy_far' then
                SetDoubleNumberInfo(ait, p_peopleOccupancyFar)
            else if ait.AsString = 'peopleOccupancy_near' then
                SetDoubleNumberInfo(ait, p_peopleOccupancyNear)
            else if ait.AsString = 'peopleOccupancy_immedate' then
                SetDoubleNumberInfo(ait, p_peopleOccupancyImmedate)
            else if ait.AsString = 'dataCreated' then
                SetDateTimeInfo(ait, p_dataCreated, false)
            else if ait.AsString = 'dateModified' then
                SetDateTimeInfo(ait, p_dateModified, false)
            else if ait.AsString = 'detailedUrl' then
                SetStringInfo(ait, p_detailedUrl)
            else if ait.AsString = 'startDate' then
                SetStringInfo(ait, p_startDate)
            else if ait.AsString = 'endDate' then
                SetStringInfo(ait, p_endDate)
            else if ait.AsString = 'image' then
                SetStringInfo(ait, p_image)
            else if ait.AsString = 'name' then
                SetStringInfo(ait, p_name)
            else if ait.AsString = 'no' then
                SetStringInfo(ait, p_no)
            else if ait.AsString = 'summary' then
                SetStringInfo(ait, p_summary)
            else if ait.AsString = 'latitude' then
                SetDoubleInfo(ait, p_latitude)
            else if ait.AsString = 'longitude' then
                SetDoubleInfo(ait, p_longitude)
            else if ait.AsString = 'locationName' then
                SetStringInfo(ait, p_locationName)
            else if ait.AsString = 'locationAddress' then
                SetStringInfo(ait, p_locationAddress)
            end;
        end;
    end;

//==============================================================================
procedure TrafficSensorClass.SetDateTimeInfo(const ait: TJSONIterator; var datedata: TList<TDateTimeInfo>; IsUTC: boolean = true);
    var
        emptyrecord : TDateTimeInfo;
    begin
    emptyrecord.initialize;
    ait.Recurse;
    while ait.Next do
        begin
        ait.Recurse;
        emptyrecord.parentid := p_id;

        if ait.Key = '_id' then
            emptyrecord.id := ait.AsString
        else if ait.Key = 'attrName' then
            emptyrecord.attrName := ait.AsString
        else if ait.Key = 'attrType' then
            emptyrecord.attrType := ait.AsString
        else if ait.Key = 'attrValue' then
            begin
            if IsUTC then
                emptyrecord.attrValue := IncHour(ISO8601ToDate(ait.AsString), 9)
            else
                emptyrecord.attrValue := ISO8601ToDate(ait.AsString, false);
            end
        else if ait.Key = 'recvTime' then
            begin
            emptyrecord.recvTime := IncHour(ISO8601ToDate(ait.AsString), 9);
            datedata.add(emptyrecord);
            ait.Return;
            end;
        end;
    end;

//==============================================================================
procedure TrafficSensorClass.SetStringInfo(const ait: TJSONIterator; var strdata: TList<TStringInfo>);
    var
        emptyrecord : TStringInfo;
    begin
    emptyrecord.initialize;
    ait.Recurse;
    while ait.Next do
        begin
        ait.Recurse;
        emptyrecord.parentid := p_id;

        if ait.Key = '_id' then
            emptyrecord.id := ait.AsString
        else if ait.Key = 'attrName' then
            emptyrecord.attrName := ait.AsString
        else if ait.Key = 'attrType' then
            emptyrecord.attrType := ait.AsString
        else if ait.Key = 'attrValue' then
            emptyrecord.attrValue := ait.AsString
        else if ait.Key = 'recvTime' then
            begin
            emptyrecord.recvTime := IncHour(ISO8601ToDate(ait.AsString), 9);
            strdata.add(emptyrecord);
            ait.Return;
            end;
        end;
    end;

//==============================================================================
procedure TrafficSensorClass.SetDoubleInfo(const ait: TJSONIterator; var ddata: TList<TDoubleInfo>);
    var
        emptyrecord : TDoubleInfo;
    begin
    emptyrecord.initialize;
    ait.Recurse;
    while ait.Next do
        begin
        ait.Recurse;
        emptyrecord.parentid := p_id;

        if ait.Key = '_id' then
            emptyrecord.id := ait.AsString
        else if ait.Key = 'attrName' then
            emptyrecord.attrName := ait.AsString
        else if ait.Key = 'attrType' then
            emptyrecord.attrType := ait.AsString
        else if ait.Key = 'attrValue' then
            emptyrecord.attrValue := StrToFloat(ait.AsString)
        else if ait.Key = 'recvTime' then
            begin
            emptyrecord.recvTime := IncHour(ISO8601ToDate(ait.AsString), 9);
            ddata.add(emptyrecord);
            ait.Return;
            end;
        end;
    end;

//==============================================================================
procedure TrafficSensorClass.SetNumberInfo(const ait: TJSONIterator; var ndata: TList<TNumberInfo>);
    var
        emptyrecord : TNumberInfo;
    begin
    emptyrecord.initialize;
    ait.Recurse;
    while ait.Next do
        begin
        ait.Recurse;
        emptyrecord.parentid := p_id;

        if ait.Key = '_id' then
            emptyrecord.id := ait.AsString
        else if ait.Key = 'attrName' then
            emptyrecord.attrName := ait.AsString
        else if ait.Key = 'attrType' then
            emptyrecord.attrType := ait.AsString
        else if ait.Key = 'attrValue' then
            emptyrecord.attrValue := ait.AsInteger
        else if ait.Key = 'recvTime' then
            begin
            emptyrecord.recvTime := IncHour(ISO8601ToDate(ait.AsString), 9);
            ndata.add(emptyrecord);
            ait.Return;
            end;
        end;
    end;

//==============================================================================
procedure TrafficSensorClass.SetDoubleNumberInfo(const ait: TJSONIterator; var dndata: TList<TDoubleInfo>);
    var
        emptyrecord : TDoubleInfo;
    begin
    emptyrecord.initialize;
    ait.Recurse;
    while ait.Next do
        begin
        ait.Recurse;
        emptyrecord.parentid := p_id;

        if ait.Key = '_id' then
            emptyrecord.id := ait.AsString
        else if ait.Key = 'attrName' then
            emptyrecord.attrName := ait.AsString
        else if ait.Key = 'attrType' then
            emptyrecord.attrType := ait.AsString
        else if ait.Key = 'attrValue' then
            emptyrecord.attrValue := ait.AsDouble
        else if ait.Key = 'recvTime' then
            begin
            emptyrecord.recvTime := IncHour(ISO8601ToDate(ait.AsString), 9);
            dndata.add(emptyrecord);
            ait.Return;
            end;
        end;
    end;

//==============================================================================
procedure TrafficSensorClass.InitializeDetails;
    begin
    p_dataCreated      := TList<TDateTimeInfo>.Create;
    p_dateModified     := TList<TDateTimeInfo>.Create;
    p_detailedUrl      := TList<TstringInfo>.Create;
    p_startDate        := TList<TstringInfo>.Create;
    p_endDate          := TList<TstringInfo>.Create;
    p_image            := TList<TstringInfo>.Create;
    p_name             := TList<TstringInfo>.Create;
    p_no               := TList<TstringInfo>.Create;
    p_summary          := TList<TstringInfo>.Create;
    p_latitude         := TList<TdoubleInfo>.Create;
    p_longitude        := TList<TdoubleInfo>.Create;
    p_locationName     := TList<TstringInfo>.Create;
    p_locationAddress  := TList<TstringInfo>.Create;

    p_dateObservedFrom        := TList<TDateTimeInfo>.Create;
    p_dateObservedTo          := TList<TDateTimeInfo>.Create;
    p_dateRetrieved           := TList<TDateTimeInfo>.Create;
    p_identifcation           := TList<TstringInfo>.Create;
    p_peopleCountNear         := TList<TnumberInfo>.Create;
    p_peopleCountFar          := TList<TnumberInfo>.Create;
    p_peopleCountImmedate     := TList<TnumberInfo>.Create;
    p_peopleOccupancyFar      := TList<TdoubleInfo>.Create;
    p_peopleOccupancyNear     := TList<TdoubleInfo>.Create;
    p_peopleOccupancyImmedate := TList<TdoubleInfo>.Create;
    end;

//==============================================================================
procedure TrafficSensorClass.FreeDetails;
    begin
    FreeAndNil(p_dataCreated);
    FreeAndNil(p_dateModified);
    FreeAndNil(p_detailedUrl);
    FreeAndNil(p_startDate);
    FreeAndNil(p_endDate);
    FreeAndNil(p_image);
    FreeAndNil(p_name);
    FreeAndNil(p_no);
    FreeAndNil(p_summary);
    FreeAndNil(p_latitude);
    FreeAndNil(p_longitude);
    FreeAndNil(p_locationName);
    FreeAndNil(p_locationAddress);

    FreeAndNil(p_dateObservedFrom);
    FreeAndNil(p_dateObservedTo);
    FreeAndNil(p_dateRetrieved);
    FreeAndNil(p_identifcation);
    FreeAndNil(p_peopleCountNear);
    FreeAndNil(p_peopleCountFar);
    FreeAndNil(p_peopleCountImmedate);
    FreeAndNil(p_peopleOccupancyFar);
    FreeAndNil(p_peopleOccupancyNear);
    FreeAndNil(p_peopleOccupancyImmedate);
    end;

{ TrafficSensorListClass }
//==============================================================================
procedure TrafficSensorListClass.AfterConstruction;
    begin
    inherited;
    p_ImportedSensorsData := false;
    p_ImportedDetailData  := false;
    end;

//==============================================================================
procedure TrafficSensorListClass.BeforeDestruction;
    begin
    inherited;

    end;

//==============================================================================
function TrafficSensorListClass.GetList: TObjectList<TrafficSensorClass>;
    begin
    if not Assigned(p_list) then
        p_list := TObjectList<TrafficSensorClass>.Create;

    Result := p_list;
    end;

//==============================================================================
function TrafficSensorListClass.GetDetailList: TObjectList<TrafficSensorClass>;
    begin
    if not Assigned(p_detaillist) then
        p_detaillist := TObjectList<TrafficSensorClass>.Create;

    Result := p_detaillist;
    end;

//==============================================================================
function TrafficSensorListClass.GetTrafficSensorData(const idx: Integer): TrafficSensorClass;
    begin
    if Assigned(p_list) and (idx < p_list.Count) then
        Result := p_list[idx]
    else
        Result := nil;
    end;

//==============================================================================
function TrafficSensorListClass.GetNumberOfTrafficSensorData: Integer;
    begin
    if Assigned(p_list) then
        Result := p_list.Count
    else
        Result := 0;
    end;

//==============================================================================
procedure TrafficSensorListClass.ReadTSJson(const ait: TJSONIterator; const project: IF8ProjectForRoad);
    var
        alist      : TObjectList<TrafficSensorClass>;
        ats        : TrafficSensorClass;
        i          : Integer;
        sensorID   : string;
        imported   : Boolean;
    begin
    alist := GetList;
    if not Assigned(p_ImportedID) then
        p_importedID := TList<string>.Create;

    imported := false;
    while ait.Next do
        begin
        ait.Recurse;
        if ait.Key = 'id' then
            begin
            sensorID := ait.AsString;
            if p_importedID.Count >= 1 then
                begin
                for i := 0 to p_importedID.Count - 1 do
                    begin
                    if sensorID = p_importedID[i] then
                        begin
                        imported := true;
                        Break;
                        end;
                    end;
                end;

            if imported then
                begin
                imported := false;
                ait.Return;
                Continue;
                end;

            ats := TrafficSensorClass.Create;
            ats.ReadTSJson(ait, sensorID);
            if Assigned(ats) then
                alist.Add(ats);

            p_importedID.add(sensorID);
            ait.Return;
            ait.Rewind;
            end;
        end;

    p_list := alist;
    p_ImportedSensorsData := true;
    for i := 0 to numberOfTrafficSensorData - 1 do
        begin
        TrafficSensorData[i].p_loadedblesensorvalues.SetDetailLists(p_detaillist, p_ImportedSensorsData, p_ImportedDetailData);
        TrafficSensorData[i].p_loadedeventvalues.SetDetailLists(p_detaillist, p_ImportedSensorsData, p_ImportedDetailData);
        end;
    end;

//==============================================================================
procedure TrafficSensorListClass.ReadTSDetailJson(const ait: TJSONIterator; const project: IF8ProjectForRoad);
    var
        alist   : TObjectList<TrafficSensorClass>;
        ats     : TrafficSensorClass;
        i       : Integer;
        adding  : Boolean;
    begin
    alist := GetDetailList;
    adding := alist.Count > 0;
    ats := nil;
    while ait.Next do
        begin
        ats := TrafficSensorClass.Create;
        ats.ReadTSDetailJson(ait);

        if adding then
            begin
            for i := 0 to alist.Count - 1 do
                begin
                if alist[i].ID = ats.ID then
                    begin
                    alist[i].MergeFrom(ats);
                    FreeAndNil(ats);
                    break;
                    end;
                end;
            end;

        if Assigned(ats) then
            alist.Add(ats);
        end;

    p_detaillist := alist;
    p_ImportedDetailData := true;
    for i := 0 to numberOfTrafficSensorData - 1 do
        begin
        TrafficSensorData[i].p_loadedblesensorvalues.SetDetailLists(p_detaillist, p_ImportedSensorsData, p_ImportedDetailData);
        TrafficSensorData[i].p_loadedeventvalues.SetDetailLists(p_detaillist, p_ImportedSensorsData, p_ImportedDetailData);
        end;
    end;

//==============================================================================
procedure TrafficSensorListClass.ClearSensorData;
    begin
    if not Assigned(p_list) then
        p_list := TObjectList<TrafficSensorClass>.Create;
    if not Assigned(p_ImportedID) then
        p_ImportedID := TList<string>.Create;

    p_list.Clear;
    p_ImportedID.Clear;
    end;

//==============================================================================
procedure TrafficSensorListClass.ClearDetail;
    begin
    if not Assigned(p_detaillist) then
        p_detaillist := TObjectList<TrafficSensorClass>.Create;

    p_detaillist.Clear;
    for var i := 0 to numberOfTrafficSensorData - 1 do
        begin
        TrafficSensorData[i].p_loadedblesensorvalues.ClearDetailLists;
        TrafficSensorData[i].p_loadedeventvalues.ClearDetailLists;
        end;
    end;

{ TLoadedEventValues }
//==============================================================================
procedure TLoadedEventValues.ReadTSJson(const ait: TJSONIterator; Sid: string);
    var
        created         : TDateTime;
        tmpDateTimeInfo : TDateTimeInfo;
        tmpStringInfo   : TStringInfo;
        tmpDoubleInfo   : TDoubleInfo;
    begin
    CreateDetailLists;
    created := 0;
    id := Sid;
    while ait.Next do
        begin
        if ait.Key = 'dataCreated' then
            ReadDateTime(ait, created);
        end;

    ait.Rewind;
    ait.Next;
    ait.Recurse;
    while ait.Next do
        begin
        if ait.Key = 'id' then
            begin
            if ait.AsString <> Sid then
                begin
                ait.Return;
                ait.Next;
                ait.Recurse;
                Continue;
                end;
            end;

        if ait.Key = 'dataCreated' then
            begin
            ReadDateTimeList(ait, id, ait.AsString, created, tmpDateTimeInfo);
            latestdetails.LatestdataCreated := tmpDateTimeInfo;
            end
        else if ait.Key = 'dateModified' then
            begin
            ReadDateTimeList(ait, id, ait.AsString, created, tmpDateTimeInfo);
            latestdetails.LatestdateModified := tmpDateTimeInfo;
            end
        else if ait.Key = 'detailedUrl' then
            begin
            ReadStringList(ait, id, ait.AsString, created, tmpStringInfo);
            latestdetails.LatestdetailedUrl := tmpStringInfo;
            end
        else if ait.Key = 'startDate' then
            begin
            ReadStringList(ait, id, ait.AsString, created, tmpStringInfo);
            latestdetails.LateststartDate := tmpStringInfo;
            end
        else if ait.Key = 'endDate' then
            begin
            ReadStringList(ait, id, ait.AsString, created, tmpStringInfo);
            latestdetails.LatestendDate := tmpStringInfo;
            end
        else if ait.Key = 'image' then
            begin
            ReadStringList(ait, id, ait.AsString, created, tmpStringInfo);
            latestdetails.Latestimage := tmpStringInfo;
            end
        else if ait.Key = 'name' then
            begin
            ReadStringList(ait, id, ait.AsString, created, tmpStringInfo);
            latestdetails.Latestname := tmpStringInfo;
            end
        else if ait.Key = 'no' then
            begin
            ReadStringList(ait, id, ait.AsString, created, tmpStringInfo);
            latestdetails.Latestno := tmpStringInfo;
            end
        else if ait.Key = 'summary' then
            begin
            ReadStringList(ait, id, ait.AsString, created, tmpStringInfo);
            latestdetails.Latestsummary := tmpStringInfo;
            end
        else if ait.Key = 'latitude' then
            begin
            ReadDoubleList(ait, id, ait.AsString, created, tmpDoubleInfo);
            latestdetails.Latestlatitude := tmpDoubleInfo;
            end
        else if ait.Key = 'longitude' then
            begin
            ReadDoubleList(ait, id, ait.AsString, created, tmpDoubleInfo);
            latestdetails.Latestlongitude := tmpDoubleInfo;
            end
        else if ait.Key = 'locationName' then
            begin
            ReadStringList(ait, id, ait.AsString, created, tmpStringInfo);
            latestdetails.LatestlocationName := tmpStringInfo;
            end
        else if ait.Key = 'locationAddress' then
            begin
            ReadStringList(ait, id, ait.AsString, created, tmpStringInfo);
            latestdetails.LatestlocationAddress := tmpStringInfo;
            end;
        end;
    end;

//==============================================================================
procedure TLoadedEventValues.ReadDateTime(const ait: TJSONIterator; var date: TDateTime);
    begin
    ait.Recurse;
    while ait.Next do
        begin
        if ait.Key = 'value' then
            date := IncHour(ISO8601ToDate(ait.AsString), 9);
        end;
    ait.Return;
    end;

//==============================================================================
procedure TLoadedEventValues.ReadDateTimeList(const ait: TJSONIterator;
    pid, valname: string; recv: TDateTime; var dtinfo: TDateTimeInfo);
    begin
    dtinfo.parentid := pid;
    dtinfo.attrName := valname;
    dtinfo.attrType := 'DateTime';
    dtinfo.recvTime := recv;

    ait.Recurse;
    while ait.Next do
        begin
        if ait.Key = 'value' then
            dtinfo.attrValue := ISO8601ToDate(ait.AsString, false);
        end;
    ait.Return;
    end;

//==============================================================================
procedure TLoadedEventValues.ReadStringList(const ait: TJSONIterator;
    pid, valname: string; recv: TDateTime; var sinfo: TstringInfo);
    begin
    sinfo.parentid := pid;
    sinfo.attrName := valname;
    sinfo.attrType := 'Text';
    sinfo.recvTime := recv;

    ait.Recurse;
    while ait.Next do
        begin
        if ait.Key = 'value' then
            sinfo.attrValue := ait.AsString;
        end;
    ait.Return;
    end;

//==============================================================================
procedure TLoadedEventValues.ReadDoubleList(const ait: TJSONIterator;
    pid, valname: string; recv: TDateTime; var dninfo: TdoubleInfo);
    begin
    dninfo.parentid := pid;
    dninfo.attrName := valname;
    dninfo.attrType := 'Text';
    dninfo.recvTime := recv;

    ait.Recurse;
    while ait.Next do
        begin
        if ait.Key = 'value' then
            dninfo.attrValue := StrToFloat(ait.AsString);
        end;
    ait.Return;
    end;

//==============================================================================
procedure TLoadedEventValues.SetDetailLists(const details; ImportedSensor, ImportedDetail: boolean);
    var
        tmp : TObjectList<TrafficSensorClass>;
        i   : integer;
    begin
    if not (ImportedSensor and ImportedDetail) then
        Exit;

    tmp := TObjectList<TrafficSensorClass>(details);

    for i := 0 to tmp.Count - 1 do
        begin
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_dataCreated)) then
            begin
            if tmp[i].p_dataCreated.Count > 0 then
                dataCreated := tmp[i].p_dataCreated;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_dateModified)) then
            begin
            if tmp[i].p_dateModified.Count > 0 then
                dateModified := tmp[i].p_dateModified;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_detailedUrl)) then
            begin
            if tmp[i].p_detailedUrl.Count > 0 then
                detailedUrl := tmp[i].p_detailedUrl;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_startDate)) then
            begin
            if tmp[i].p_startDate.Count > 0 then
                startDate := tmp[i].p_startDate;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_endDate)) then
            begin
            if tmp[i].p_endDate.Count > 0 then
                endDate := tmp[i].p_endDate;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_image)) then
            begin
            if tmp[i].p_image.Count > 0 then
                image := tmp[i].p_image;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_name)) then
            begin
            if tmp[i].p_name.Count > 0 then
                name := tmp[i].p_name;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_no)) then
            begin
            if tmp[i].p_no.Count > 0 then
                no := tmp[i].p_no;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_summary)) then
            begin
            if tmp[i].p_summary.Count > 0 then
                summary := tmp[i].p_summary;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_latitude)) then
            begin
            if tmp[i].p_latitude.Count > 0 then
                latitude := tmp[i].p_latitude;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_longitude)) then
            begin
            if tmp[i].p_longitude.Count > 0 then
                longitude := tmp[i].p_longitude;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_locationName)) then
            begin
            if tmp[i].p_locationName.Count > 0 then
                locationName := tmp[i].p_locationName;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_locationAddress)) then
            begin
            if tmp[i].p_locationAddress.Count > 0 then
                locationAddress := tmp[i].p_locationAddress;
            end;
        end;
    end;

//==============================================================================
procedure TLoadedEventValues.CreateDetailLists;
    begin
    dataCreated      := TList<TDateTimeInfo>.Create;
    dateModified     := TList<TDateTimeInfo>.Create;
    detailedUrl      := TList<TstringInfo>.Create;
    startDate        := TList<TstringInfo>.Create;
    endDate          := TList<TstringInfo>.Create;
    image            := TList<TstringInfo>.Create;
    name             := TList<TstringInfo>.Create;
    no               := TList<TstringInfo>.Create;
    summary          := TList<TstringInfo>.Create;
    latitude         := TList<TdoubleInfo>.Create;
    longitude        := TList<TdoubleInfo>.Create;
    locationName     := TList<TstringInfo>.Create;
    locationAddress  := TList<TstringInfo>.Create;
    end;

//==============================================================================
procedure TLoadedEventValues.ClearDetailLists;
    begin
    if Assigned(dataCreated) then
        begin
        dataCreated.Clear;
        dataCreated := TList<TDateTimeInfo>.Create;
        end;
    if Assigned(dateModified) then
        begin
        dateModified.Clear;
        dateModified     := TList<TDateTimeInfo>.Create;
        end;
    if Assigned(detailedUrl) then
        begin
        detailedUrl.Clear;
        detailedUrl := TList<TstringInfo>.Create;
        end;
    if Assigned(startDate) then
        begin
        startDate.Clear;
        startDate := TList<TstringInfo>.Create;
        end;
    if Assigned(endDate) then
        begin
        endDate.Clear;
        endDate := TList<TstringInfo>.Create;
        end;
    if Assigned(image) then
        begin
        image.Clear;
        image := TList<TstringInfo>.Create;
        end;
    if Assigned(name) then
        begin
        name.Clear;
        name := TList<TstringInfo>.Create;
        end;
    if Assigned(no) then
        begin
        no.Clear;
        no := TList<TstringInfo>.Create;
        end;
    if Assigned(summary) then
        begin
        summary.Clear;
        summary := TList<TstringInfo>.Create;
        end;
    if Assigned(latitude) then
        begin
        latitude.Clear;
        latitude := TList<TdoubleInfo>.Create;
        end;
    if Assigned(longitude) then
        begin
        longitude.Clear;
        longitude := TList<TdoubleInfo>.Create;
        end;
    if Assigned(locationName) then
        begin
        locationName.Clear;
        locationName := TList<TstringInfo>.Create;
        end;
    if Assigned(locationAddress) then
        begin
        locationAddress.Clear;
        locationAddress := TList<TstringInfo>.Create;
        end;
    end;

{ TLoadedBlesensorValues }
//==============================================================================
procedure TLoadedBlesensorValues.ReadTSJson(const ait: TJSONIterator; Sid: string);
    var
        retrievedDate   : TDateTime;
        tmpDateTimeInfo : TDateTimeInfo;
        tmpStringInfo : TStringInfo;
        tmpNumberInfo : TNumberInfo;
        tmpDoubleInfo : TDoubleInfo;
    begin
    CreateDetailLists;
    retrievedDate := 0;
    id := Sid;
    while ait.Next do
        begin
        if ait.Key = 'dateRetrieved' then
            ReadDateTime(ait, retrievedDate);
        end;

    ait.Rewind;
    ait.Next;
    ait.Recurse;
    while ait.Next do
        begin
        if ait.Key = 'id' then
            begin
            if ait.AsString <> Sid then
                begin
                ait.Return;
                ait.Next;
                ait.Recurse;
                Continue;
                end;
            end;
        if ait.Key = 'locationName' then
            ReadString(ait, locationName)
        else if ait.Key = 'latitude' then
            ReadDouble(ait, latitude)
        else if ait.Key = 'longitude' then
            ReadDouble(ait, longitude)
        else if ait.Key = 'dateObservedFrom' then
            begin
            ReadDateTimeInfo(ait, id, ait.AsString, retrievedDate, tmpDateTimeInfo);
            latestdetails.LatestdateObservedFrom := tmpDateTimeInfo;
            end
        else if ait.Key = 'dateObservedTo' then
            begin
            ReadDateTimeInfo(ait, id, ait.AsString, retrievedDate, tmpDateTimeInfo);
            latestdetails.LatestdateObservedTo := tmpDateTimeInfo;
            end
        else if ait.Key = 'dateRetrieved' then
            begin
            ReadDateTimeInfo(ait, id, ait.AsString, retrievedDate, tmpDateTimeInfo);
            latestdetails.LatestdateRetrieved := tmpDateTimeInfo;
            end
        else if ait.Key = 'identifcation' then
            begin
            ReadStringInfo(ait, id, ait.AsString, retrievedDate, tmpStringInfo);
            latestdetails.Latestidentifcation := tmpStringInfo;
            end
        else if ait.Key = 'peopleCount_far' then
            begin
            ReadNumberInfo(ait, id, ait.AsString, retrievedDate, tmpNumberInfo);
            latestdetails.LatestpeopleCountFar := tmpNumberInfo;
            end
        else if ait.Key = 'peopleCount_immedate' then
            begin
            ReadNumberInfo(ait, id, ait.AsString, retrievedDate, tmpNumberInfo);
            latestdetails.LatestpeopleCountImmedate := tmpNumberInfo;
            end
        else if ait.Key = 'peopleCount_near' then
            begin
            ReadNumberInfo(ait, id, ait.AsString, retrievedDate, tmpNumberInfo);
            latestdetails.LatestpeopleCountNear := tmpNumberInfo;
            end
        else if ait.Key = 'peopleOccupancy_far' then
            begin
            ReadDoubleNumInfo(ait, id, ait.AsString, retrievedDate, tmpDoubleInfo);
            latestdetails.LatestpeopleOccupancyFar := tmpDoubleInfo;
            end
        else if ait.Key = 'peopleOccupancy_immedate' then
            begin
            ReadDoubleNumInfo(ait, id, ait.AsString, retrievedDate, tmpDoubleInfo);
            latestdetails.LatestpeopleOccupancyImmedate := tmpDoubleInfo;
            end
        else if ait.Key = 'peopleOccupancy_near' then
            begin
            ReadDoubleNumInfo(ait, id, ait.AsString, retrievedDate, tmpDoubleInfo);
            latestdetails.LatestpeopleOccupancyNear := tmpDoubleInfo;
            end;
        end;
    end;

//==============================================================================
procedure TLoadedBlesensorValues.ReadDateTime(const ait: TJSONIterator; var date: TDateTime);
    begin
    ait.Recurse;
    while ait.Next do
        begin
        if ait.Key = 'value' then
            date := IncHour(ISO8601ToDate(ait.AsString), 9);
        end;
    ait.Return;
    end;

//==============================================================================
procedure TLoadedBlesensorValues.ReadDateTimeInfo(const ait: TJSONIterator;
    pid, valname: string; recv: TDateTime; var dtinfo: TDateTimeInfo);
    begin
    dtinfo.parentid := pid;
    dtinfo.attrName := valname;
    dtinfo.attrType := 'DateTime';
    dtinfo.recvTime := recv;

    ait.Recurse;
    while ait.Next do
        begin
        if ait.Key = 'value' then
            dtinfo.attrValue := IncHour(ISO8601ToDate(ait.AsString), 9);
        end;
    ait.Return;
    end;

//==============================================================================
procedure TLoadedBlesensorValues.ReadStringInfo(const ait: TJSONIterator;
    pid, valname: string; recv: TDateTime; var sinfo: TstringInfo);
    begin
    sinfo.parentid := pid;
    sinfo.attrName := valname;
    sinfo.attrType := 'Text';
    sinfo.recvTime := recv;

    ait.Recurse;
    while ait.Next do
        begin
        if ait.Key = 'value' then
            sinfo.attrValue := ait.AsString;
        end;
    ait.Return;
    end;

//==============================================================================
procedure TLoadedBlesensorValues.ReadNumberInfo(const ait: TJSONIterator;
    pid, valname: string; recv: TDateTime; var ninfo: TnumberInfo);
    begin
    ninfo.parentid := pid;
    ninfo.attrName := valname;
    ninfo.attrType := 'number';
    ninfo.recvTime := recv;

    ait.Recurse;
    while ait.Next do
        begin
        if ait.Key = 'value' then
            ninfo.attrValue := ait.AsInteger;
        end;
    ait.Return;
    end;

//==============================================================================
procedure TLoadedBlesensorValues.ReadDoubleNumInfo(const ait: TJSONIterator;
    pid, valname: string; recv: TDateTime; var dninfo: TdoubleInfo);
    begin
    dninfo.parentid := pid;
    dninfo.attrName := valname;
    dninfo.attrType := 'number';
    dninfo.recvTime := recv;

    ait.Recurse;
    while ait.Next do
        begin
        if ait.Key = 'value' then
            dninfo.attrValue := ait.AsDouble;
        end;
    ait.Return;
    end;

//==============================================================================
procedure TLoadedBlesensorValues.ReadString(const ait: TJSONIterator; var strdata: string);
    begin
    ait.Recurse;
    while ait.Next do
        begin
        if ait.Key = 'value' then
            strdata := ait.AsString;
        end;
    ait.Return;
    end;

//==============================================================================
procedure TLoadedBlesensorValues.ReadDouble(const ait: TJSONIterator; var valdata: double);
    begin
    ait.Recurse;
    while ait.Next do
        begin
        if ait.Key = 'value' then
            valdata := StrToFloat(ait.AsString);
        end;
    ait.Return;
    end;

//==============================================================================
procedure TLoadedBlesensorValues.SetDetailLists(const details; ImportedSensor, ImportedDetail: boolean);
    var
        tmp : TObjectList<TrafficSensorClass>;
        i   : integer;
    begin
    if not (ImportedSensor and ImportedDetail) then
        Exit;

    tmp := TObjectList<TrafficSensorClass>(details);

    for i := 0 to tmp.Count - 1 do
        begin
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_dateObservedFrom)) then
            begin
            if tmp[i].p_dateObservedFrom.Count > 0 then
                dateObservedFrom := tmp[i].p_dateObservedFrom;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_dateObservedTo)) then
            begin
            if tmp[i].p_dateObservedTo.Count > 0 then
                dateObservedTo := tmp[i].p_dateObservedTo;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_dateRetrieved)) then
            begin
            if tmp[i].p_dateRetrieved.Count > 0 then
                dateRetrieved := tmp[i].p_dateRetrieved;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_identifcation)) then
            begin
            if tmp[i].p_identifcation.Count > 0 then
                identifcation := tmp[i].p_identifcation;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_peopleCountFar)) then
            begin
            if tmp[i].p_peopleCountFar.Count > 0 then
                peopleCountFar := tmp[i].p_peopleCountFar;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_peopleCountNear)) then
            begin
            if tmp[i].p_peopleCountNear.Count > 0 then
                peopleCountNear := tmp[i].p_peopleCountNear;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_peopleCountImmedate)) then
            begin
            if tmp[i].p_peopleCountImmedate.Count > 0 then
                peopleCountImmedate := tmp[i].p_peopleCountImmedate;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_peopleOccupancyFar)) then
            begin
            if tmp[i].p_peopleOccupancyFar.Count > 0 then
                peopleOccupancyFar := tmp[i].p_peopleOccupancyFar;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_peopleOccupancyNear)) then
            begin
            if tmp[i].p_peopleOccupancyNear.Count > 0 then
                peopleOccupancyNear := tmp[i].p_peopleOccupancyNear;
            end;
        if (id = tmp[i].ID) and (Assigned(tmp[i].p_peopleOccupancyImmedate)) then
            begin
            if tmp[i].p_peopleOccupancyImmedate.Count > 0 then
                peopleOccupancyImmedate := tmp[i].p_peopleOccupancyImmedate;
            end;
        end;
    end;

//==============================================================================
procedure TLoadedBlesensorValues.CreateDetailLists;
    begin
    dateObservedFrom := TList<TDateTimeInfo>.Create;
    dateObservedTo := TList<TDateTimeInfo>.Create;
    dateRetrieved := TList<TDateTimeInfo>.Create;
    identifcation := TList<TStringInfo>.Create;
    peopleCountfar := TList<TNumberInfo>.Create;
    peopleCountimmedate := TList<TNumberInfo>.Create;
    peopleCountnear := TList<TNumberInfo>.Create;
    peopleOccupancyfar := TList<TDoubleInfo>.Create;
    peopleOccupancyimmedate := TList<TDoubleInfo>.Create;
    peopleOccupancynear := TList<TDoubleInfo>.Create;
    end;

//==============================================================================
procedure TLoadedBlesensorValues.ClearDetailLists;
    begin
    if Assigned(dateObservedFrom) then
        begin
        dateObservedFrom.Clear;
        dateObservedFrom := TList<TDateTimeInfo>.Create;
        end;
    if Assigned(dateObservedTo) then
        begin
        dateObservedTo.Clear;
        dateObservedTo := TList<TDateTimeInfo>.Create;
        end;
    if Assigned(dateRetrieved) then
        begin
        dateRetrieved.Clear;
        dateRetrieved := TList<TDateTimeInfo>.Create;
        end;
    if Assigned(identifcation) then
        begin
        identifcation.Clear;
        identifcation := TList<TStringInfo>.Create;
        end;
    if Assigned(peopleCountFar) then
        begin
        peopleCountFar.Clear;
        peopleCountfar := TList<TNumberInfo>.Create;
        end;
    if Assigned(peopleCountNear) then
        begin
        peopleCountNear.Clear;
        peopleCountnear := TList<TNumberInfo>.Create;
        end;
    if Assigned(peopleCountImmedate) then
        begin
        peopleCountImmedate.Clear;
        peopleCountimmedate := TList<TNumberInfo>.Create;
        end;
    if Assigned(peopleOccupancyFar) then
        begin
        peopleOccupancyFar.Clear;
        peopleOccupancyfar := TList<TDoubleInfo>.Create;
        end;
    if Assigned(peopleOccupancyNear) then
        begin
        peopleOccupancyNear.Clear;
        peopleOccupancynear := TList<TDoubleInfo>.Create;
        end;
    if Assigned(peopleOccupancyImmedate) then
        begin
        peopleOccupancyImmedate.Clear;
        peopleOccupancyimmedate := TList<TDoubleInfo>.Create;
        end;
    end;

{ TDateTimeInfo }
//==============================================================================
procedure TDateTimeInfo.initialize;
    begin
    parentid  := '';
    id        := '';
    attrName  := '';
    attrType  := '';
    attrValue := 0;
    recvTime  := 0;
    end;

{ TStringInfo }
//==============================================================================
procedure TStringInfo.initialize;
    begin
    parentid  := '';
    id        := '';
    attrName  := '';
    attrType  := '';
    attrValue := '';
    recvTime  := 0;
    end;

{ TDoubleInfo }
//==============================================================================
procedure TDoubleInfo.initialize;
    begin
    parentid  := '';
    id        := '';
    attrName  := '';
    attrType  := '';
    attrValue := 0;
    recvTime  := 0;
    end;

{ TNumberInfo }
//==============================================================================
procedure TNumberInfo.initialize;
    begin
    parentid  := '';
    id        := '';
    attrName  := '';
    attrType  := '';
    attrValue := 0;
    recvTime  := 0;
    end;
end.
