unit BusTransportationResultExport;

interface

uses
    System.Generics.Collections,
    PluginCore;

type
    BusInOutLog = record
        BusStopGUID: TGUID;
        BusLog     : BusTransportationResultType;

        class operator Equal(const a, b: BusInOutLog): Boolean;
        class operator NotEqual(const a, b: BusInOutLog): Boolean;

        procedure SetData(const aNodeGUID: TGUID; const aLog: BusTransportationResultType);
        end;

    BusStopInfo = record
        name: String;
        GUID: TGUID;

        procedure SetData(const aName: String; const aGUID: TGUID);
        end;

    ///    時刻別のバスの乗降者ログ。BusIdが同じログをまとめる
    ///    複数のノード(=バス停)で乗降が発生する場合、各ノード(バス停)からログをAddLogDataで追加する
    ///    1ノード1バス停
    ///
    ///    BusStop: バス停ごとのログ
    ///        NodeName
    ///            下記をリスト化し、SimTimeでソートしておく
    ///             NodeId: ノードのID。詳細未定
    ///            SimTime: そのバス停を出発した時間
    ///            GetOff: そのバス停で、SimTimeの時に降りた人数
    ///            RideOn: そのバス停で、SimTimeの時に乗った人数
    ///            Per   : その時点の乗車率
    ///    TotalGetOff: 全てのバス停で降りた人数の総数
    ///    TotalRideOn: 全てのバス停で乗った人数の総数
    TBusOperationLog = class
        private
            p_BusId  : Integer;

            p_LogList: TList<BusInOutLog>;
            p_TotalGetOff: Integer;
            p_TotalRideOn: Integer;

            function GetInOutLog(const aIdx: Integer): BusInOutLog;
            function GetInOutLogCount: Integer;
        public
            constructor Create(const aBusId: Integer);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure AddLogData(const aNodeGUID: TGUID; const aData: BusTransportationResultType);

            property  BusId                        : Integer   read p_BusId;
            property  InOutLog[const aIdx: Integer]: BusInOutLog read GetInOutLog;
            property  InOutLogCount                : Integer     read GetInOutLogCount;
            property  TotalGetOff                  : Integer   read p_TotalGetOff;
            property  TotalRideOn                  : Integer   read p_TotalRideOn;
        end;

    TBusOperationLogsExport = class
        private
            p_BusLogList : TObjectList<TBusOperationLog>;
            p_BusStopList: TList<BusStopInfo>;

            procedure AddLogData(const aNodeGUID: TGUID; const aData: BusTransportationResultType);
            procedure CollectLogFromBusStop(const aBusStop: IF8NetworkNode);

            function  GetLog(const aIdx: Integer): TBusOperationLog;
            function  GetLogCount: Integer;
            function  GetBusStopInfo(const aIdx: Integer): BusStopInfo;
            function  GetBusStopInfoCount: Integer;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure CollectLog(const aProject: IF8ProjectForRoad);
            procedure ExportToCSV(const aDate: TDateTime; const aDataFolderPath: String);

            property  BusLog[const aIdx: Integer] : TBusOperationLog read GetLog;
            property  BusLogCount                 : Integer          read GetLogCount;
            property  BusStop[const aIdx: Integer]: BusStopInfo      read GetBusStopInfo;
            property  BusStopCount                : Integer          read GetBusStopInfoCount;
        end;


implementation

uses
    System.SysUtils,
    System.Classes,
    System.Math,
    System.DateUtils,
    System.IOUtils,
    System.Generics.Defaults,
    Vcl.Dialogs;

{ BusInOutLog }
class operator BusInOutLog.Equal(const a, b: BusInOutLog): Boolean;
    begin
    Result := (a.BusLog.BusId = b.BusLog.BusId) and IsEqualGUID(a.BusStopGUID, b.BusStopGUID) and (a.BusLog.SimTime = b.BusLog.SimTime);
    end;

class operator BusInOutLog.NotEqual(const a, b: BusInOutLog): Boolean;
    begin
    Result := not ((a.BusLog.BusId = b.BusLog.BusId) and IsEqualGUID(a.BusStopGUID, b.BusStopGUID) and (a.BusLog.SimTime = b.BusLog.SimTime));
    end;

procedure BusInOutLog.SetData(const aNodeGUID: TGUID; const aLog: BusTransportationResultType);
    begin
    BusStopGUID := aNodeGUID;
    BusLog      := aLog;
    end;

{ BusStopInfo }

procedure BusStopInfo.SetData(const aName: String; const aGUID: TGUID);
    begin
    name := aName;
    GUID := aGUID;
    end;

{ TBusOperationLog }
constructor TBusOperationLog.Create(const aBusId: Integer);
    begin
    p_BusId   := aBusId;
    end;

procedure TBusOperationLog.AfterConstruction;
    begin
    inherited;
    p_LogList := TList<BusInOutLog>.Create;

    p_TotalGetOff := 0;
    p_TotalRideOn := 0;
    end;

procedure TBusOperationLog.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_LogList);
    end;

procedure TBusOperationLog.AddLogData(const aNodeGUID: TGUID; const aData: BusTransportationResultType);
    var
        i: Integer;
        find: Boolean;
        newData: BusInOutLog;
    begin
    if not (aData.BusId = BusId) then
        Exit;

    newData.SetData(aNodeGUID, aData);

    find := False;
    for i := 0 to p_LogList.Count - 1 do
        begin
        if newData = p_LogList[i] then
            begin
            find := True;
            Break;
            end;
        end;

    if find then
        Exit;

    p_TotalGetOff := p_TotalGetOff + newData.BusLog.GetOffNum;
    p_TotalRideOn := p_TotalRideOn + newData.BusLog.RideOnNum;

    p_LogList.Add(newData);
    end;

function TBusOperationLog.GetInOutLog(const aIdx: Integer): BusInOutLog;
    begin
    Assert(InRange(aIdx, 0, p_LogList.Count - 1));
    Result := p_LogList[aIdx];
    end;

function TBusOperationLog.GetInOutLogCount: Integer;
    begin
    Result := p_LogList.Count;
    end;

{ TBusOperationLogsExport }
procedure TBusOperationLogsExport.AfterConstruction;
    begin
    inherited;
    p_BusLogList  := TObjectList<TBusOperationLog>.Create;
    p_BusStopList := TList<BusStopInfo>.Create;
    end;

procedure TBusOperationLogsExport.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_BusLogList);
    FreeAndNil(p_BusStopList);
    end;

procedure TBusOperationLogsExport.CollectLog(const aProject: IF8ProjectForRoad);
    var
        i: Integer;
        targetNetwork: IF8Networks;
        targetNode: IF8NetworkNode;
        newBusStop: BusStopInfo;
    begin
    // 既存データをクリア
    p_BusLogList.Clear;
    p_BusStopList.Clear;

    if not Assigned(aProject) then
        Exit;

    if aProject.NumberOfFlightWayNwks < 1 then
        Exit;

    // データ収集
    targetNetwork := aProject.FlightWayNwk[1];

    for i := 0 to targetNetwork.NodeCount - 1 do
        begin
        targetNode := targetNetwork.Node[i];
        if targetNode.NumberofBusTransportationResult > 0 then
            begin
            newBusStop.SetData(targetNode.name, targetNode.GUID);
            p_BusStopList.Add(newBusStop);
            CollectLogFromBusStop(targetNode);
            end;
        end;
    end;

procedure TBusOperationLogsExport.ExportToCSV(const aDate: TDateTime; const aDataFolderPath: String);
    const
        LOG_PATH = '\Log';
        DATE_TIME_FILE_NAME_FORMAT = 'yyyy年mm月dd日_hh時nn分ss秒';
        HEADER_STRING = 'バスID,バス出発時シミュレーション経過時間(秒),到着時刻,出発時刻,降車人数(人),乗車人数(人),乗車率(%)';
    var
        outputPath: String;
        timeStr   : String;
        fileName  : String;
        newFile   : THandle;
        writer    : TStreamWriter;

        tmpList: TList<BusInOutLog>;
        tmpItem: BusInOutLog;
        log: BusTransportationResultType;

        i: Integer;
        idx: Integer;
        item: TBusOperationLog;
        arrivalStr, departureStr: String;
        txt: String;

    begin
    tmpList := TList<BusInOutLog>.Create;
    try
        for item in p_BusLogList do
            begin
            for i := 0 to item.InOutLogCount - 1 do
                tmpList.Add(item.InOutLog[i]);
            end;

        tmpList.Sort(TComparer<BusInOutLog>.Construct(
            function(const L, R: BusInOutLog): Integer
                begin
                Result := L.BusLog.SimTime - R.BusLog.SimTime;
                end));

        outputPath := aDataFolderPath + LOG_PATH;
        DateTimeToString(timeStr, DATE_TIME_FILE_NAME_FORMAT, aDate);
        idx := 0;
        while True do
            begin
            fileName := 'Log_バス乗降ログ_' + timeStr + Format('_%d', [idx]) + '.csv';
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
            for tmpItem in tmpList do
                begin
                log := tmpItem.BusLog;
                DateTimeToString(arrivalStr,   DATE_TIME_FILE_NAME_FORMAT, log.ArrivalTime);
                DateTimeToString(departureStr, DATE_TIME_FILE_NAME_FORMAT, log.DepartureTime);
                txt := Format('%d, %d, %s, %s, %d, %d, %.3f',
                  [log.BusId, log.SimTime, arrivalStr, departureStr, log.GetOffNum, log.RideOnNum, log.OccupancyPer]);
                writer.WriteLine(txt);
                end;
        finally
            writer.Close;
            writer.Free;
            end;
    finally
        FreeAndNil(tmpList);
        end;
    end;

procedure TBusOperationLogsExport.AddLogData(const aNodeGUID: TGUID; const aData: BusTransportationResultType);
    var
        item: TBusOperationLog;
        findItem: Boolean;
    begin
    findItem := False;
    for item in p_BusLogList do
        begin
        if (item.BusId = aData.BusId) then
            begin
            findItem := True;
            item.AddLogData(aNodeGUID, aData);
            Break;
            end;
        end;

    if not findItem then
        begin
        p_BusLogList.Add(TBusOperationLog.Create(aData.BusId));
        p_BusLogList.Last.AddLogData(aNodeGUID, aData);
        end;
    end;

procedure TBusOperationLogsExport.CollectLogFromBusStop(const aBusStop: IF8NetworkNode);
    var
        i: Integer;
    begin
    if not Assigned(aBusStop) then
        Exit;

    for i := 0 to aBusStop.NumberofBusTransportationResult - 1 do
        AddLogData(aBusStop.GUID, aBusStop.BusTransportationResult[i]);
    end;

function TBusOperationLogsExport.GetLog(const aIdx: Integer): TBusOperationLog;
    begin
    Assert(InRange(aIdx, 0, p_BusLogList.Count - 1));
    Result := p_BusLogList[aIdx];
    end;

function TBusOperationLogsExport.GetLogCount: Integer;
    begin
    Result := p_BusLogList.Count;
    end;

function TBusOperationLogsExport.GetBusStopInfo(const aIdx: Integer): BusStopInfo;
    begin
    Assert(InRange(aIdx, 0, p_BusStopList.Count - 1));
    Result := p_BusStopList[aIdx];
    end;

function TBusOperationLogsExport.GetBusStopInfoCount: Integer;
    begin
    Result := p_BusStopList.Count;
    end;
end.

