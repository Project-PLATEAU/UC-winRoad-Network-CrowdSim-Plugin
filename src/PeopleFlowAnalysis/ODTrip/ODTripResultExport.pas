unit ODTripResultExport;

interface

uses
    System.Classes,
    System.Generics.Collections,
    PluginCore;

type
    TODIdentifyData = record
        OriginNodeName: String;
        DestinationNodeName: String;
        OriginNodeGUID: TGUID;
        DestinationNodeGUID: TGUID;
        end;

    TODAggregateResult = record
        NumberOfPerson: Integer;
        TripTimeMinimum: Double;
        TripTimeMaximum: Double;
        TripTimeAverage: Double;
        TripTimeMedian : Double;
        end;

    ///    出発地点(O: IF8NetworkNode), 目的地点(D: IF8NetworkNode)別データ
    ///    個別のデータが時刻(開始時刻, 終了時刻, 通過時間)とラベル文字列を持つ
    ///    関数で期間別の集計を行える。
    ///    期間は開始時刻の幅とする(例 9:00～9:01の間)
    ///    集計結果は次の項目とする
    ///    到着人数, 通過時間(最小), 通過時間(最大), 通過時間(平均), 通過時間(中央)
    TODTripLogData = class
        private
            p_ID: TODIdentifyData;
            p_LogList: TList<PedestrianSimResultType>;
            p_PedestrianLabelList: TStringList;

            function  GetPedestrianLabelCount: Integer;
            function  GetPedestrianLabel(const aIdx: Integer): String;
        public
            constructor Create(const aID: TODIdentifyData; aDataContainer: IF8PedestriansGeneratorOptionListContainer);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function  CalcSpecificPeriod(const aLabel: String; const aStartTime: TDateTime; const aStartTimePeriod: Integer = 1): TODAggregateResult;

            property  PedestrianLabelCount                : Integer         read GetPedestrianLabelCount;
            property  PedestrianLabel[const aIdx: Integer]: String          read GetPedestrianLabel;
            property  ID                                  : TODIdentifyData read p_ID;
        end;

    TODTripResultExport = class
        private
            p_ODLogList: TObjectList<TODTripLogData>;
            p_SimulationStartTime: TDateTime;
            p_SimulationEndTime: TDateTime;

            function  GetODLogCount: Integer;
            function  GetODLog(const aIdx: Integer): TODTripLogData;
        public
            constructor Create(const aProject: IF8ProjectForRoad;
                               const aSimulationStartTime: TDateTime;
                               const aSimulationEndTime: TDateTime;
                               const aNetworkIndex: Integer = 1);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure ExportToCSV(const aDate: TDateTime; const aDataFolderPath: String);

            property  SimulationStartTime       : TDateTime      read p_SimulationStartTime;
            property  SimulationEndTime         : TDateTime      read p_SimulationEndTime;
            property  ODLogCount                : Integer        read GetODLogCount;
            property  ODLog[const aIdx: Integer]: TODTripLogData read GetODLog;
        end;

implementation

uses
    System.Types,
    System.SysUtils,
    System.DateUtils,
    System.IOUtils,
    System.Math,
    System.Generics.Defaults;

{ TODTripLogData }
constructor TODTripLogData.Create(const aID: TODIdentifyData; aDataContainer: IF8PedestriansGeneratorOptionListContainer);
    var
        i: Integer;
        data: PedestrianSimResultType;
    begin
    Assert(Assigned(aDataContainer));
    p_ID := aID;
    p_LogList   := TList<PedestrianSimResultType>.Create;
    p_PedestrianLabelList := TStringList.Create;

    // データ収集
    for i := 0 to aDataContainer.GetNumberOfLog - 1 do
        begin
        data := aDataContainer.GetLog(i);
        p_LogList.Add(data);
        if p_PedestrianLabelList.IndexOf(data.PedestrianLabel) = -1 then
            p_PedestrianLabelList.Add(data.PedestrianLabel);
        end;
    end;

procedure TODTripLogData.AfterConstruction;
    begin
    inherited;
    {no action}
    end;

procedure TODTripLogData.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_LogList);
    FreeAndNil(p_PedestrianLabelList);
    end;

function TODTripLogData.CalcSpecificPeriod(const aLabel: String; const aStartTime: TDateTime; const aStartTimePeriod: Integer): TODAggregateResult;
    var
        dataList: TList<PedestrianSimResultType>;
        i: Integer;
        startRes, endRes: TValueRelationShip;
        cnt: Integer;
        sumOfElapsed: Double;
    begin
    // Initilize
    Result.NumberOfPerson := 0;
    Result.TripTimeMinimum := 1e-6;
    Result.TripTimeMaximum := 1e-6;
    Result.TripTimeAverage := 1e-6;
    Result.TripTimeMedian  := 1e-6;

    if aStartTimePeriod < 1 then
        Exit;

    dataList := TList<PedestrianSimResultType>.Create;
    sumOfElapsed := 0.0;
    try
        // Extract item
        for i := 0 to p_LogList.Count - 1 do
            begin
            if aLabel <> p_LogList[i].PedestrianLabel then
                Continue;

            // Check startTime < log.EnterTime < startTime + aStartTimePeriod
            startRes := CompareTime(p_LogList[i].EnterTime, aStartTime);
            endRes   := CompareTime(p_LogList[i].EnterTime, IncMinute(aStartTime, aStartTimePeriod));

            if (startRes <> LessThanValue) and (endRes <> GreaterThanValue) then
                begin
                dataList.Add(p_LogList[i]);
                sumOfElapsed := sumOfElapsed + p_LogList[i].ElapsedTime;
                end;
            end;

        if datalist.Count <> 0 then
            begin
            // Sort
            dataList.Sort(TComparer<PedestrianSimResultType>.Construct(
                function(const L, R: PedestrianSimResultType): Integer
                    begin
                    Result := Trunc(L.ElapsedTime * 100 - R.ElapsedTime * 100);
                    end));

            cnt := dataList.Count;

            Result.NumberOfPerson := cnt;
            Result.TripTimeMinimum := dataList.First.ElapsedTime;
            Result.TripTimeMaximum := dataList.Last.ElapsedTime;
            // average
            Result.TripTimeAverage := sumOfElapsed / cnt;
            // median
            if (cnt mod 2) = 1 then
                Result.TripTimeMedian := dataList[cnt div 2].ElapsedTime
            else
                Result.TripTimeMedian := (dataList[(cnt div 2) - 1].ElapsedTime + dataList[cnt div 2].ElapsedTime) / 2;
            end;
    finally
        FreeAndNil(dataList);
        end;
    end;

function TODTripLogData.GetPedestrianLabelCount: Integer;
    begin
    Result := p_PedestrianLabelList.Count;
    end;

function TODTripLogData.GetPedestrianLabel(const aIdx: Integer): String;
    begin
    Assert(InRange(aIdx, 0, p_PedestrianLabelList.Count - 1));
    Result := p_PedestrianLabelList[aIdx];
    end;

{ TODTripResultExport }
constructor TODTripResultExport.Create(const aProject: IF8ProjectForRoad;
                                       const aSimulationStartTime: TDateTime;
                                       const aSimulationEndTime: TDateTime;
                                       const aNetworkIndex: Integer);
    var
        network: IF8Networks;
        inoutNodeList: TList<IF8NetworkNode>;
        gen: IF8PedestrianGenerator;
        container: IF8PedestriansGeneratorOptionListContainer;
        odId: TODIdentifyData;
        i: Integer;
        oriIdx, destIdx: Integer;
    begin
    p_SimulationStartTime := aSimulationStartTime;
    p_SimulationEndTime   := aSimulationEndTime;
    p_ODLogList := TObjectList<TODTripLogData>.Create;

    if aProject.NumberOfFlightWayNwks < aNetworkIndex then
        Exit;

    network := aProject.FlightWayNwk[aNetworkIndex];
    // get generator is start 0
    // 現在のシステムではネットワーク生成時にODマトリックスジェネレータを追加し、アクティブにして使用している
    // この仕様を前提としてリストの末尾からログを取得する
    gen := network.Generator[network.NumberOfGenerator - 1];
    if gen.TypeOfGenerator = _pgNormal then
        Exit;

    inoutNodeList := TList<IF8NetworkNode>.Create;
    try
        for i := 0 to network.NodeCount - 1 do
            begin
            if network.Node[i].InOut then
                inoutNodeList.Add(network.Node[i]);
            end;

        for oriIdx := 0 to inoutNodeList.Count - 1 do
            begin
            for destIdx := 0 to inoutNodeList.Count - 1 do
                begin
                if oriIdx = destIdx then
                    Continue;

                container := gen.GetOptionListContainerByOD(inoutNodeList[oriIdx], inoutNodeList[destIdx]);
                if not Assigned(container) then
                    Continue;

                odId.OriginNodeName      := inoutNodeList[oriIdx].name;
                odId.DestinationNodeName := inoutNodeList[destIdx].name;
                odId.OriginNodeGUID      := inoutNodeList[oriIdx].GUID;
                odId.DestinationNodeGUID := inoutNodeList[destIdx].GUID;

                p_ODLogList.Add(TODTripLogData.Create(odId, container));
                end;
            end;
    finally
        inoutNodeList.Clear;
        FreeAndNil(inoutNodeList);
        end;
    end;

procedure TODTripResultExport.AfterConstruction;
    begin
    inherited;
    {no action}
    end;

procedure TODTripResultExport.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_ODLogList);
    end;

procedure TODTripResultExport.ExportToCSV(const aDate: TDateTime; const aDataFolderPath: String);
    const
        LOG_PATH = '\Log';
        DATE_TIME_FILE_NAME_FORMAT = 'yyyy年mm月dd日_hh時_nn分ss秒';
        HEADER_STRING = '出現開始時刻,出現終了時刻,出発地点,目的地点,ラベル,到着人数(人),通過時間_最小(秒),通過時間_最大(秒),通過時間_平均(秒),通過時間_中央(秒)';

    function CreateEmptyFile(const aOutputPath: String): String;
        var
            timeStr: String;
            idx: Integer;
            newHandle: THandle;
        begin
        DateTimeToString(timeStr, DATE_TIME_FILE_NAME_FORMAT, aDate);
        idx := 0;
        while True do
            begin
            Result := 'Log_OD通過_' + timeStr + Format('%d', [idx]) + '.csv';

            if not FileExists(TPath.Combine(aOutputPath, Result)) then
                begin
                newHandle := FileCreate(TPath.Combine(aOutputPath, Result));
                FileClose(newHandle);
                Break;
                end;
            Inc(idx);
            end;
        end;

    var
        outputPath, fileName: String;
        startStr, endStr: String;
        writer : TStreamWriter;
        logData: TODTripLogData;
        logLabel: String;
        logStartTime: TDateTime;
        logRes: TODAggregateResult;
        txt: String;
        i, j: Integer;
    begin
    outputPath := aDataFolderPath + LOG_PATH;
    fileName := CreateEmptyFile(outputPath);
    writer := TStreamWriter.Create(TPath.Combine(outputPath, fileName), False, TEncoding.UTF8);
    try
        writer.WriteLine(HEADER_STRING);
        logStartTime := p_SimulationStartTime;
        while True do
            begin
            for i := 0 to p_ODLogList.Count - 1 do
                begin
                logData := p_ODLogList[i];
                for j := 0 to logData.PedestrianLabelCount - 1 do
                    begin
                    // 出力間隔は1分固定
                    logLabel := logData.PedestrianLabel[j];
                    logRes := logData.CalcSpecificPeriod(logLabel, logStartTime);
                    DateTimeToString(startStr, DATE_TIME_FILE_NAME_FORMAT, logStartTime);
                    DateTimeToString(endStr, DATE_TIME_FILE_NAME_FORMAT, IncMinute(logStartTime));
                    txt := Format('%s, %s, %s, %s, %s, %d, %.3f, %.3f, %.3f, %.3f',
                                    [startStr, endStr,
                                     logData.ID.OriginNodeName, logData.ID.DestinationNodeName,
                                     logLabel,
                                     logRes.NumberOfPerson,
                                     logRes.TripTimeMinimum, logRes.TripTimeMaximum, logRes.TripTimeAverage, logRes.TripTimeMedian
                                    ]);
                    writer.WriteLine(txt);
                    end;
                end;
            logStartTime := IncMinute(logStartTime, 1);
            if CompareTime(logStartTime, p_SimulationEndTime) = GreaterThanValue then
                Break;
            end;
    finally
        writer.Close;
        writer.Free;
        end;
    end;

function TODTripResultExport.GetODLogCount: Integer;
    begin
    Result := p_ODLogList.Count;
    end;

function TODTripResultExport.GetODLog(const aIdx: Integer): TODTripLogData;
    begin
    Assert(InRange(aIdx, 0, p_ODLogList.Count - 1));
    Result := p_ODLogList[aIdx];
    end;
end.

