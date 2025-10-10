unit WaitingQueueResultExport;

interface

uses
    System.Generics.Collections,
    PluginCore;

type
    TWaitingQueueSimResultArray = array of TWaitingQueueSimResultType;

    TWaitingQueueIdentifyData = record
        NodeName        : String;
        WaitingQueueName: String;
        NodeGUID        : TGUID;
        WaitingQueueGUID: TGUID;
        end;

    TWaitingQueueAllPeriodLogData = record
        WaitAverageTime: Double; // 平均待機時間(分)
        WaitAverageNum: Integer; // 平均待機人数(人)
        ReleaseNum : Integer; // 総退出人数(人)
        MaxWaitTime: Double; // 最大待機時間(分)
        MaxWaitNum: Integer; // 最大待機人数(人)
        end;

    TWaitingQueueLogData = class
        private
            p_ID          : TWaitingQueueIdentifyData;
            p_Log         : TWaitingQueueSimResultArray;
            p_AllPeriodLog: TWaitingQueueAllPeriodLogData;

            function  GetLogCount: Integer;
            function  GetLog(const aIdx: Integer): TWaitingQueueSimResultType;

            function  Clone: TWaitingQueueLogData;
        public
            constructor Create(const aID: TWaitingQueueIdentifyData; const aLog: TWaitingQueueSimResultArray; const aAllLog: TWaitingQueueAllPeriodLogData);

            property  ID                      : TWaitingQueueIdentifyData     read p_ID;
            property  Log[const aIdx: Integer]: TWaitingQueueSimResultType    read GetLog;
            property  LogCount                : Integer                       read GetLogCount;
            property  AllPeriodLog            : TWaitingQueueAllPeriodLogData read p_AllPeriodLog;
        end;

    ///    - シミュレーション終了時にログデータを収集する
    ///    - 1分間隔でCSVファイルに出力する
    ///        - 収集したログは1分単位で計測されているので加工は不要
    ///    - 合計ログをCSVファイルに出力する
    ///    - 任意の期間の合計ログをCSVファイルに出力する
    ///    - 任意の間隔の時間単位ログをCSVファイルに出力する
    ///    - グラフ表示用のデータを取得できる
    TWaitingQueueResultExport = class
        private
            // ログの計測間隔(分): 0以上とする
            p_OutputIntervalMinutes: Cardinal;
            // シミュレーション開始時刻, 終了時刻
            p_SimulationStartTime: TDateTime;
            p_SimulationEndTime  : TDateTime;
            p_WaitingQueueLogList: TObjectList<TWaitingQueueLogData>;

            function  GetWaitingQueueLogCount: Integer;
            function  GetWaitingQueueLog(const aIdx: Integer): TWaitingQueueLogData;

            constructor Create; overload;
        public
            constructor Create(const aProject: IF8ProjectForRoad;
                               const aSimulationStartTime: TDateTime; const aSimulationEndTime: TDateTime;
                               const aNetworkIndex: Integer = 1); overload;
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            function  Clone: TWaitingQueueResultExport;

            function  ExportSumOfLogToCSV(const aDate: TDateTime; const aDataFolderPath: String): Boolean;
            function  ExportLogToCSV(const aDate: TDateTime; const aDataFolderPath: String): Boolean;

            function  ConvertIntervalMinutes(const aNewIntervalMinutes: Cardinal): TWaitingQueueResultExport;

            property  OutputIntervalMinutes               : Cardinal             read p_OutputIntervalMinutes;
            property  SimulationStartTime                 : TDateTime            read p_SimulationStartTime;
            property  SimulationEndTime                   : TDateTime            read p_SimulationEndTime;
            property  WaitingQueueLogCount                : Integer              read GetWaitingQueueLogCount;
            property  WaitingQueueLog[const aIdx: Integer]: TWaitingQueueLogData read GetWaitingQueueLog;
        end;

implementation

uses
    Winapi.Windows,
    System.SysUtils,
    System.Types,
    System.Classes,
    System.DateUtils,
    System.IOUtils,
    System.Math;

const
    LOG_PATH = '\Log';
    DATE_TIME_FILE_NAME_FORMAT = 'yyyy年mm月dd日_hh時nn分ss秒';

{ TWaitingQueueLogData }
constructor TWaitingQueueLogData.Create(const aID: TWaitingQueueIdentifyData; const aLog: TWaitingQueueSimResultArray; const aAllLog: TWaitingQueueAllPeriodLogData);
    begin
    p_ID := aID;
    SetLength(p_Log, Length(aLog));
    CopyMemory(p_Log, aLog, SizeOf(TWaitingQueueSimResultType) * Length(aLog));
    p_AllPeriodLog := aAllLog;
    end;

function TWaitingQueueLogData.GetLogCount: Integer;
    begin
    Result := Length(p_Log);
    end;

function TWaitingQueueLogData.GetLog(const aIdx: Integer): TWaitingQueueSimResultType;
    begin
    Assert(InRange(aIdx, 0, Length(p_Log) - 1));
    Result := p_Log[aIdx];
    end;

function TWaitingQueueLogData.Clone: TWaitingQueueLogData;
    begin
    Result := TWaitingQueueLogData.Create(p_ID, p_Log, p_AllPeriodLog);
    end;

{ TWaitingQueueResultExport }
constructor TWaitingQueueResultExport.Create;
    begin
    p_OutputIntervalMinutes := 1;
    p_WaitingQueueLogList := TObjectList<TWaitingQueueLogData>.Create;
    end;

constructor TWaitingQueueResultExport.Create(const aProject: IF8ProjectForRoad;
                                             const aSimulationStartTime: TDateTime; const aSimulationEndTime: TDateTime;
                                             const aNetworkIndex: Integer);
    type
        TWaitingQueueCheckFunc = reference to procedure(const aID: TWaitingQueueIdentifyData; const aWaitingQueue: IF8WaitingQueue);

    procedure CheckWaitingQueue(proc: TWaitingQueueCheckFunc);
        var
            network: IF8Networks;
            node   : IF8NetworkNode;
            wait   : IF8WaitingQueue;
            id     : TWaitingQueueIdentifyData;

            i, nodeIdx: Integer;
        begin
        if aProject.NumberOfFlightWayNwks < aNetworkIndex then
            Exit;

        network := aProject.FlightWayNwk[aNetworkIndex];
        for nodeIdx := 0 to network.NodeCount - 1 do
            begin
            node := network.Node[nodeIdx];
            if node.waitingQueueCount < 1 then
                Continue;

            for i := 1 to node.waitingQueueCount do
                begin
                wait := node.GetIntWaitingQueue(i);
                if wait.NumberOfSimResult < 1 then
                    Continue;

                id.NodeName         := node.name;
                id.WaitingQueueName := wait.name;
                id.NodeGUID         := node.GUID;
                id.WaitingQueueGUID := wait.GUID;

                proc(id, wait);
                end;
            end;
        end;

    const
        MAX_WAIT_NUM = 100000;
    begin
    p_WaitingQueueLogList := TObjectList<TWaitingQueueLogData>.Create;
    p_OutputIntervalMinutes := 1; // 本体ログは1分間隔で出力されている
    p_SimulationStartTime := aSimulationStartTime;
    p_SimulationEndTime   := aSimulationEndTime;

    CheckWaitingQueue(procedure(const aID: TWaitingQueueIdentifyData; const aWaitingQueue: IF8WaitingQueue)
        function TruncSeconds(const aDateTime: TDateTime): TDateTime;
            begin
            // 秒、ミリ秒を切り捨てる
            Result := RecodeMilliSecond(RecodeSecond(aDateTime, 0), 0);
            end;

        function StagnantResult(const aLastResult: TWaitingQueueSimResultType; const aElapsedMinutes: Integer):TWaitingQueueSimResultType;
            begin
            Result.Time            := IncMinute(aLastResult.Time, aElapsedMinutes);
            if aLastResult.WaitNum > 0 then
                Result.AverageWait := aLastResult.AverageWait + aElapsedMinutes * 60
            else
                Result.AverageWait := 0;
            Result.WaitNum         := aLastResult.WaitNum;
            Result.EnterNum        := 0;
            Result.ReleaseNum      := 0;
            Result.TotalReleaseNum := aLastResult.TotalReleaseNum;
            Result.remain          := aLastResult.remain; // 使わないけど一応
            end;
        var
            resArr: TWaitingQueueSimResultArray;
            resAllLog: TWaitingQueueAllPeriodLogData;

            i: Integer;
            logLength: Integer;
            sumOfWaitNum: Integer;
            lastLog: TWaitingQueueSimResultType;
        begin
        // シミュレーション開始が12:00:00, シミュレーション終了が12:06:00の場合7ログ出力する
        logLength := MinutesBetween(aSimulationStartTime, aSimulationEndTime) + 1;

        SetLength(resArr, logLength);

        // シミュレーション開始時に自動でログが入るのでシミュレーション開始時刻とログ開始時刻は一致する
        // ログ終了が12:04, シミュレーション終了が12:06:00の場合、12:05, 12:06のログを追加する
        sumOfWaitNum := 0;
        for i := 0 to logLength - 1 do
            begin
            if i < aWaitingQueue.NumberOfSimResult then
                begin
                // ログ
                resArr[i] := aWaitingQueue.SimResult[i];
                lastLog := aWaitingQueue.SimResult[i];
                end
            else
                begin
                // ログ終了-シミュレーション終了
                resArr[i] := StagnantResult(lastLog, (i + 1) - aWaitingQueue.NumberOfSimResult);
                end;

            if (resArr[i].WaitNum > 0) or (resArr[i].WaitNum < MAX_WAIT_NUM) then
                sumOfWaitNum := sumOfWaitNum + resArr[i].WaitNum;
            end;

        resAllLog.WaitAverageTime := Trunc(aWaitingQueue.WaitAverage / 60);
        resAllLog.WaitAverageNum  := Trunc(sumOfWaitNum / Length(resArr));
        resAllLog.ReleaseNum      := aWaitingQueue.ReleaseNum;
        resAllLog.MaxWaitTime     := Trunc(aWaitingQueue.MaxWaitTimeSec / 60);
        resAllLog.MaxWaitNum      := aWaitingQueue.MaxWaitNum;

        p_WaitingQueueLogList.Add(TWaitingQueueLogData.Create(aID, resArr, resAllLog));
        end);
    end;

procedure TWaitingQueueResultExport.AfterConstruction;
    begin
    inherited;
    {no action}
    end;

procedure TWaitingQueueResultExport.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_WaitingQueueLogList);
    end;

function TWaitingQueueResultExport.Clone: TWaitingQueueResultExport;
    begin
    Result := ConvertIntervalMinutes(OutputIntervalMinutes);
    end;

function TWaitingQueueResultExport.ExportSumOfLogToCSV(const aDate: TDateTime; const aDataFolderPath: String): Boolean;
    const
        FILE_NAME = '待機列全期間';
        HEADER_STRING = '待機列名,平均待ち時間(分),最大待ち時間(分),平均待機人数(人),最大待機人数(人),累計退出人数(人)';
    var
        outputPath, fileName: String;
        timeStr: String;
        writer: TStreamWriter;
        newFile: THandle;

        idx: Integer;
        item: TWaitingQueueLogData;
        itemAll: TWaitingQueueAllPeriodLogData;
    begin
    outputPath := aDataFolderPath + LOG_PATH;
    DateTimeToString(timeStr, DATE_TIME_FILE_NAME_FORMAT, aDate);
    idx := 0;
    while True do
        begin
        fileName := FILE_NAME + timeStr + Format('_%d', [idx]) + '.csv';
        if not FileExists(TPath.Combine(outputPath, fileName)) then
            begin
            newFile := FileCreate(TPath.Combine(outputPath, fileName));
            FileClose(newFile);
            Break;
            end;
        Inc(idx);
        end;

    writer := TStreamWriter.Create(TPath.Combine(outputPath, fileName), False, TEncoding.UTF8);
    try
        writer.WriteLine(HEADER_STRING);
        for item in p_WaitingQueueLogList do
            begin
            itemAll := item.AllPeriodLog;
            writer.WriteLine(Format('%s, %.3f, %.3f, %d, %d, %d', [item.ID.WaitingQueueName,
                                                                 itemAll.WaitAverageTime,
                                                                 itemAll.MaxWaitTime,
                                                                 itemAll.WaitAverageNum,
                                                                 itemAll.MaxWaitNum,
                                                                 itemAll.ReleaseNum]));
            end;
    finally
        writer.Close;
        writer.Free;
        end;
    Result := True;
    end;

function TWaitingQueueResultExport.ExportLogToCSV(const aDate: TDateTime; const aDataFolderPath: String): Boolean;
    const
        FILE_NAME = '待機列時刻歴';
        HEADER_STRING = '待機列名,集計開始時刻,平均待ち時間(分),待機列人数(人),追加人数(人),退出人数(人),累計退出人数(人)';

    var
        outputPath, fileName: String;
        timeStr: String;
        writer: TStreamWriter;
        newFile: THandle;

        i: Integer;
        idx: Integer;
        item: TWaitingQueueLogData;
        l: TWaitingQueueSimResultType;
    begin
    outputPath := aDataFolderPath + LOG_PATH;
    DateTimeToString(timeStr, DATE_TIME_FILE_NAME_FORMAT, aDate);
    idx := 0;
    while True do
        begin
        fileName := FILE_NAME + timeStr + Format('_%d', [idx]) + '.csv';
        if not FileExists(TPath.Combine(outputPath, fileName)) then
            begin
            newFile := FileCreate(TPath.Combine(outputPath, fileName));
            FileClose(newFile);
            Break;
            end;
        Inc(idx);
        end;

    writer := TStreamWriter.Create(TPath.Combine(outputPath, fileName), False, TEncoding.UTF8);
    try
        writer.WriteLine(HEADER_STRING);
        for item in p_WaitingQueueLogList do
            begin
            for i := 0 to item.LogCount - 1 do
                begin
                l := item.Log[i];
                DateTimeToString(timeStr, DATE_TIME_FILE_NAME_FORMAT, l.Time);
                writer.WriteLine(Format('%s, %s, %d, %d, %d, %d, %d', [item.ID.WaitingQueueName,
                                                                       timeStr,
                                                                       Trunc(l.AverageWait / 60),
                                                                       l.WaitNum,
                                                                       l.EnterNum,
                                                                       l.ReleaseNum,
                                                                       l.TotalReleaseNum]));
                end;
            end;
    finally
        writer.Close;
        writer.Free;
        end;
    Result := True;
    end;

function TWaitingQueueResultExport.ConvertIntervalMinutes(const aNewIntervalMinutes: Cardinal): TWaitingQueueResultExport;
    var
        i, j: Integer;
        newIntervalMinutes: Cardinal;
        res      : TWaitingQueueSimResultType;
        resID    : TWaitingQueueIdentifyData;
        resArr   : TWaitingQueueSimResultArray;
        resAllLog: TWaitingQueueAllPeriodLogData;
    begin
    // 実装中...

    Result := TWaitingQueueResultExport.Create;
    newIntervalMinutes := Max(aNewIntervalMinutes, 1);
    Result.p_OutputIntervalMinutes := newIntervalMinutes;
    Result.p_SimulationStartTime := p_SimulationStartTime;
    Result.p_SimulationEndTime   := p_SimulationEndTime;

    if OutputIntervalMinutes = newIntervalMinutes then
        begin
        // 実質Clone
        for i := 0 to p_WaitingQueueLogList.Count - 1 do
            Result.p_WaitingQueueLogList.Add(p_WaitingQueueLogList[i].Clone);
        end
    else
        begin
        for i := 0 to p_WaitingQueueLogList.Count - 1 do
            begin
            if p_WaitingQueueLogList[i].LogCount < 1 then
                Continue;

            resID     := p_WaitingQueueLogList[i].ID;
            resAllLog := p_WaitingQueueLogList[i].AllPeriodLog;

            SetLength(resArr, ((Integer(p_OutputIntervalMinutes) * p_WaitingQueueLogList[i].LogCount) div Integer(newIntervalMinutes)) + 1);
            for j := 0 to Length(resArr) - 1 do
                begin
                resArr[j] := res;
                end;

            Result.p_WaitingQueueLogList.Add(TWaitingQueueLogData.Create(resID, resArr, resAllLog));
            end;
        end;
    end;

function TWaitingQueueResultExport.GetWaitingQueueLogCount: Integer;
    begin
    Result := p_WaitingQueueLogList.Count;
    end;

function TWaitingQueueResultExport.GetWaitingQueueLog(const aIdx: Integer): TWaitingQueueLogData;
    begin
    Assert(InRange(aIdx, 0, p_WaitingQueueLogList.Count - 1));
    Result := p_WaitingQueueLogList[aIdx];
    end;
end.
