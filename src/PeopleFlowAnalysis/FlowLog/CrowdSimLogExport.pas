unit CrowdSimLogExport;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.IOUtils,
    System.SysUtils,
    System.StrUtils,
    System.DateUtils,
    System.Generics.Collections,
    System.Classes,
    System.Types,
    Vcl.StdCtrls,
    Vcl.Dialogs,
    Vcl.Forms,
    PluginCore,
    F8OpenGL,
    PedestrianMap,
    PedestrianCell,
    CrowdSimLogUtils,
    F8CrowdSimController,
    VCLTee.Series,
    VCLTee.Chart;

type
    /// <summary>
    ///    人流シミュレーションのログデータを集計するクラス
    /// </summary>
    TLogCrossSectionFlow = class
        private
            p_API              : IF8ApplicationServices;

            p_Map              : TPedestrianMap;
            p_SensorAreaList   : TList<TSensorAreaData>;
            p_CharacterCellList: TList<TCharacterAndCell>;
            p_CharacterIDs     : TList<integer>;
            p_CellsInOutArr    : TArray<integer>;
            p_InOutZEROArr     : TArray<integer>;
            p_CellsInOutList   : TList<TArray<integer>>;
            p_LogDataList      : TList<string>;

            p_Logbeginningtime : TDateTime;//ログ収集のたびに変動
            p_Colbeginningtime : TDateTime; //通過データ収集のたびに変動
            p_SimBeginningTime : TDateTime; //シミュレーション開始時間で固定

            p_LogExportInterval    : integer;
            p_AddCellsDataInterval : integer;

            p_IsExportCrossSectionLog : boolean;
            p_IsExportBusTransportLog : boolean;
            p_IsExportWaitingQueueLog : boolean;
            p_IsExportPedestrianODLog : boolean;

            procedure CollectCSFLog(const SimBegTime, CurrTime, BeginTime: TDateTime);
            procedure GetCurrentCellOnCharacter(const ch: IF8CharacterInstance; controller: F8CrowdSimControllerClass; var ccRecord: TCharacterAndCell);
            procedure GetSensorAreaInOut(const ch: IF8CharacterInstance; preCC: TCharacterAndCell; controller: F8CrowdSimControllerClass; var currCC: TCharacterAndCell);
            procedure CharacterOutFromMesh(const ch: IF8CharacterInstance; controller: F8CrowdSimControllerClass; var ccRecord: TCharacterAndCell);
            procedure SetMap(const currmap: TPedestrianMap);
            procedure AddNewCharacter(const ch: IF8CharacterInstance; var ccRecord: TCharacterAndCell);
            procedure CollectAndResetCellPassData;
            procedure ResetSAPassedNum(const sa_i: TSensorAreaData; var rsa: TSensorAreaData);
            procedure ResetAssignedData;
            procedure SetDefaultInterval;
            procedure CollectLogAndPassedData(const CurrTime: TDateTime);
            procedure ExecuteCollect(const SimBegTime, CurrTime: TDateTime);
            procedure SetLogHeader;

            function CalcLatLonDiffAndSARange(const sa: TSensorAreaData; var LatD, LonD: double): double;
            function CountUpPassedNum(const Idx: integer; controller: F8CrowdSimControllerClass): boolean;
            function IsCharacterSkipCell(const xdiff, zdiff: double): boolean;
            function IsCharacterOutOfSensorArea(const aCharacterInst: IF8CharacterInstance; const aSensorNumber: Integer): Boolean;
            function IsCharacterOutOfMesh(const ch: IF8CharacterInstance; meshLT, meshRB: TPedestrianCell): boolean;
        public
            constructor Create(const aAPI: IF8ApplicationServices);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure AddSensorAreaData(const saData: TSensorAreaData);
            procedure DeleteSensorAreaData(const idx: integer);
            procedure SetIntervals(const LogInterval, CellDataInterval: integer);
            procedure CollectData(const controller: F8CrowdSimControllerClass);
            procedure ExportLog(const SimEndTime: TDateTime; SimStopped: boolean);
            procedure SetGraphData(var Graph: TChart);
            procedure ClearSensorAreas;
            function  IsExistSameName(const aName: String): Boolean;
            procedure PlayStart(const aStartTime: TDateTime);
            procedure ExportToPluginData;
            procedure ImportFromPluginData;

            property  Map             : TPedestrianMap               read p_Map            write SetMap;
            property  CellsInOutList  : TList<TArray<integer>>       read p_CellsInOutList;
            property  SensorAreas     : TList<TSensorAreaData>       read p_SensorAreaList;
            property  AddDataInterval : integer                      read p_AddCellsDataInterval;
            property  IsExportCrossSectionLog : boolean read p_IsExportCrossSectionLog write p_IsExportCrossSectionLog;
            property  IsExportBusTransportLog : boolean read p_IsExportBusTransportLog write p_IsExportBusTransportLog;
            property  IsExportWaitingQueueLog : boolean read p_IsExportWaitingQueueLog write p_IsExportWaitingQueueLog;
            property  IsExportPedestrianODLog : boolean read p_IsExportPedestrianODLog write p_IsExportPedestrianODLog;
        end;

implementation

uses
    System.Math,
    Vcl.Controls,
    F8GLUtils,
    PedestrianUtil,
    MovingFeature,
    F8CrowdSimPluginDataConstant,
    F8Utils;

const
    COLLECT_WAIT_MILLISEC = 100; //CharacterInstanceが初期座標に再配置されるまで待機

{ TLogCrossSectionFlow }
//==============================================================================
constructor TLogCrossSectionFlow.Create(const aAPI: IF8ApplicationServices);
    begin
    p_API := aAPI;
    end;

//==============================================================================
procedure TLogCrossSectionFlow.AfterConstruction;
    begin
    inherited;
    p_SensorAreaList     := TList<TSensorAreaData>.Create;
    p_CharacterCellList  := TList<TCharacterAndCell>.Create;
    p_CharacterIDs       := TList<integer>.Create;
    p_CellsInOutList     := TList<TArray<integer>>.Create;
    SetLength(p_CellsInOutArr, 0);
    p_LogDataList        := TList<string>.Create;
    SetLogHeader;
    p_LogExportInterval    := -1;
    p_AddCellsDataInterval := -1;
    p_IsExportCrossSectionLog := true;
    p_IsExportBusTransportLog := true;
    p_IsExportWaitingQueueLog := true;
    p_IsExportPedestrianODLog := true;
    end;

//==============================================================================
procedure TLogCrossSectionFlow.BeforeDestruction;
    begin
    inherited;
    p_Map := nil;
    FreeAndNil(p_CharacterCellList);
    FreeAndNil(p_SensorAreaList);
    FreeAndNil(p_CharacterIDs);
    FreeAndNil(p_CellsInOutList);
    FreeAndNil(p_LogDataList);
    end;

//==============================================================================
procedure TLogCrossSectionFlow.ResetSAPassedNum(const sa_i: TSensorAreaData; var rsa: TSensorAreaData);
    begin
    rsa.SensorAreaName := sa_i.SensorAreaName;
    rsa.AreaOrigin     := sa_i.AreaOrigin;
    rsa.AreaEnd        := sa_i.AreaEnd;
    rsa.LatLonOrigin   := sa_i.LatLonOrigin;
    rsa.LatLonEnd      := sa_i.LatLonEnd;
    rsa.PassedNum      := 0;
    end;

//==============================================================================
procedure TLogCrossSectionFlow.CollectCSFLog(const SimBegTime, CurrTime, BeginTime: TDateTime);
    var
        currTstr : string;
        passMstr : string;
        strArr   : TStringDynArray;
        i        : integer;
        resetSA  : TSensorAreaData;
        LatDiff  : double;
        LonDiff  : double;
        SARange  : double;
    begin
    if (not Assigned(p_LogDataList)) or (not p_IsExportCrossSectionLog) then
        Exit;

    passMstr := (MinutesBetween(BeginTime, SimBegTime) + p_LogExportInterval).ToString;
    DateTimeToString(currTstr, 'yyyy年mm月dd日 hh:nn:ss', CurrTime);
    SetLength(strArr, 12);
    for i := 0 to p_SensorAreaList.Count - 1 do
        begin
        SARange := CalcLatLonDiffAndSARange(p_SensorAreaList[i], LatDiff, LonDiff);
        strArr[_saName]      := p_SensorAreaList[i].SensorAreaName;
        strArr[_simPassTime] := passMstr;
        strArr[_simCurrTime] := currTstr;
        strArr[_saCenLat]    := (p_SensorAreaList[i].LatLonOrigin.X + (LatDiff / 2)).ToString;
        strArr[_saCenLon]    := (p_SensorAreaList[i].LatLonOrigin.Y + (LonDiff / 2)).ToString;
        strArr[_saRange]     := FormatFloat('0.00', SARange);
        strArr[_saOriLat]    := p_SensorAreaList[i].LatLonOrigin.X.ToString;
        strArr[_saOriLon]    := p_SensorAreaList[i].LatLonOrigin.Y.ToString;
        strArr[_saEndLat]    := p_SensorAreaList[i].LatLonEnd.X.ToString;
        strArr[_saEndLon]    := p_SensorAreaList[i].LatLonEnd.Y.ToString;
        strArr[_saPassedNum] := p_SensorAreaList[i].PassedNum.ToString;
        strArr[_saDensity]   := FormatFloat('0.00', p_SensorAreaList[i].PassedNum / (PI * SARange * SARange));
        p_LogDataList.Add(String.Join(',', strArr));
        ResetSAPassedNum(p_SensorAreaList[i], resetSA);
        p_SensorAreaList[i] := resetSA;
        end;
    end;

//==============================================================================
function TLogCrossSectionFlow.CalcLatLonDiffAndSARange(const sa: TSensorAreaData; var LatD, LonD: double): double;
    var
        AreaXDiff, AreaZDiff : double;
    begin
    LatD := sa.LatLonEnd.X - sa.LatLonOrigin.X;
    LonD := sa.LatLonEnd.Y - sa.LatLonOrigin.Y;
    AreaXDiff := Abs(sa.AreaEnd.X - sa.AreaOrigin.X);
    AreaZDiff := Abs(sa.AreaEnd.Z - sa.AreaOrigin.Z);
    if AreaXDiff >= AreaZDiff then
        Result := AreaXDiff / 2
    else
        Result := AreaZDiff / 2;
    end;

//==============================================================================
procedure TLogCrossSectionFlow.CollectAndResetCellPassData;
    var
        tmpArr : TArray<integer>;
    begin
    SetLength(tmpArr, Length(p_CellsInOutArr));
    CopyMemory(tmpArr, p_CellsInOutArr, SizeOf(integer) * Length(p_CellsInOutArr));
    CellsInOutList.Add(tmpArr);
    CopyMemory(p_CellsInOutArr, p_InOutZEROArr, SizeOf(integer) * Length(p_InOutZEROArr));
    end;

//==============================================================================
procedure TLogCrossSectionFlow.AddSensorAreaData(const saData: TSensorAreaData);
    begin
    p_SensorAreaList.Add(saData);
    end;

//==============================================================================
procedure TLogCrossSectionFlow.DeleteSensorAreaData(const idx: integer);
    begin
    p_SensorAreaList.Delete(idx);
    end;

//==============================================================================
procedure TLogCrossSectionFlow.SetIntervals(const LogInterval, CellDataInterval: integer);
    begin
    p_LogExportInterval := LogInterval;
    p_AddCellsDataInterval := CellDataInterval;
    end;

//==============================================================================
procedure TLogCrossSectionFlow.SetMap(const currmap: TPedestrianMap);
    begin
    ResetAssignedData;
    p_Map := currmap;
    if not Assigned(p_Map) then
        Exit;

    if Length(p_CellsInOutArr) <> p_Map.Config.AllCellCount then
        begin
        SetLength(p_CellsInOutArr, p_Map.Config.AllCellCount);
        SetLength(p_InOutZEROArr, p_Map.Config.AllCellCount);
        for var i := 0 to Length(p_InOutZEROArr) - 1 do
            p_InOutZEROArr[i] := 0;
        end;

    CopyMemory(p_CellsInOutArr, p_InOutZEROArr, SizeOf(integer) * Length(p_InOutZEROArr));
    end;

//==============================================================================
procedure TLogCrossSectionFlow.ResetAssignedData;
    var
        ResetSA : TSensorAreaData;
    begin
    if Assigned(p_CellsInOutList) then
        p_CellsInOutList.Clear;

    if Assigned(p_CharacterCellList) then
        p_CharacterCellList.Clear;

    if Assigned(p_CharacterIDs) then
        p_CharacterIDs.Clear;

    if Assigned(p_LogDataList) then
        begin
        p_LogDataList.Clear;
        SetLogHeader;
        end;

    if Assigned(p_SensorAreaList) then
        begin
        if p_SensorAreaList.Count > 0 then
            begin
            for var i := 0 to p_SensorAreaList.Count - 1 do
                begin
                ResetSAPassedNum(p_SensorAreaList[i], resetSA);
                p_SensorAreaList[i] := resetSA;
                end;
            end;
        end;
    end;

//==============================================================================
procedure TLogCrossSectionFlow.ClearSensorAreas;
    begin
    if Assigned(p_SensorAreaList) then
        p_SensorAreaList.Clear;
    end;

//==============================================================================
function TLogCrossSectionFlow.CountUpPassedNum(const Idx: integer; controller: F8CrowdSimControllerClass): boolean;
    begin
    Result := false;
    if Idx = -1 then
        Exit;

    if controller.playingSpeed > 0 then
        p_CellsInOutArr[Idx] := p_CellsInOutArr[Idx] + 1
    else
        begin
        if p_CellsInOutArr[Idx] < 1 then
            p_CellsInOutArr[Idx] := 0
        else
            p_CellsInOutArr[Idx] := p_CellsInOutArr[Idx] - 1;
        end;

    Result := true;
    end;

//==============================================================================
function TLogCrossSectionFlow.IsCharacterOutOfSensorArea(const aCharacterInst: IF8CharacterInstance; const aSensorNumber: Integer): Boolean;
    begin
    Result := (aCharacterInst.position[_x] < p_SensorAreaList[aSensorNumber].AreaOrigin.X)
            or (aCharacterInst.position[_z] < p_SensorAreaList[aSensorNumber].AreaOrigin.Z)
            or (aCharacterInst.position[_x] > p_SensorAreaList[aSensorNumber].AreaEnd.X)
            or (aCharacterInst.position[_z] > p_SensorAreaList[aSensorNumber].AreaEnd.Z)
    end;

//==============================================================================
procedure TLogCrossSectionFlow.AddNewCharacter(const ch: IF8CharacterInstance; var ccRecord: TCharacterAndCell);
    begin
    ccRecord.Character  := ch;
    ccRecord.CellIdx    := -1;
    ccRecord.IntoSAName := '';
    p_CharacterCellList.Add(ccRecord);
    p_CharacterIDs.Add(ch.id);
    end;

//==============================================================================
procedure TLogCrossSectionFlow.GetCurrentCellOnCharacter(const ch: IF8CharacterInstance; controller: F8CrowdSimControllerClass; var ccRecord: TCharacterAndCell);

    //--------------------------------------------------------------------------
    function IsCounted(const Idx: integer; var CurrCell: TPedestrianCell): boolean;
        begin
        Result := false;
//        if not (CurrCell.Status = PedestrianAreaStatus._pasNoEntry) then
//            begin
            if not CountUpPassedNum(Idx, controller) then
                Exit
            else
                Result := true;
//            end;

        if not p_map.RequireCellInfo(Idx, CurrCell) then
            Result := false;
        end;

    //--------------------------------------------------------------------------
    /// <summary>行方向→列方向の順に、Agentがスキップしたセルを直線軌道で補完する関数</summary>
    procedure CountUpRowtoCol(const PreCell: TPedestrianCell; var CurrCell: TPedestrianCell; RowDiff, ColDiff: integer);
        var
            AddRatio, remain : integer;
        begin
        if ColDiff <> 0 then
            AddRatio := Trunc((RowDiff - ColDiff) / ColDiff)
        else
            AddRatio := RowDiff;

        remain   := AddRatio;
        while RowDiff > 0 do
            begin
            if CurrCell.CellID.RowIdx > PreCell.CellID.RowIdx then
                begin
                if not IsCounted(CurrCell.CellID.AboveID, CurrCell) then
                    Exit;
                end
            else
                begin
                if not IsCounted(CurrCell.CellID.BelowID, CurrCell) then
                    Exit;
                end;

            if (ColDiff > 0) and (remain = 0) then
                begin
                if CurrCell.CellID.ColumnIdx > PreCell.CellID.ColumnIdx then
                    begin
                    if not IsCounted(CurrCell.CellID.LeftID, CurrCell) then
                        Exit;
                    end
                else
                    begin
                    if not IsCounted(CurrCell.CellID.RightID, CurrCell) then
                        Exit;
                    end;

                ColDiff := ColDiff - 1;
                end;

            RowDiff := RowDiff - 1;
            if remain > 0 then
                remain := remain - 1
            else
                remain := AddRatio;
            end;
        end;

    //--------------------------------------------------------------------------
    /// <summary>列方向→行方向の順に、Agentがスキップしたセルを直線軌道で補完する関数</summary>
    procedure CountUpColtoRow(const PreCell: TPedestrianCell; var CurrCell: TPedestrianCell; RowDiff, ColDiff: integer);
        var
            AddRatio, remain : integer;
        begin
        if RowDiff <> 0 then
            AddRatio := Trunc((ColDiff - RowDiff) / RowDiff)
        else
            AddRatio := ColDiff;

        remain := AddRatio;
        while ColDiff > 1 do
            begin
            if CurrCell.CellID.ColumnIdx > PreCell.CellID.ColumnIdx then
                begin
                if not IsCounted(CurrCell.CellID.LeftID, CurrCell) then
                    Exit;
                end
            else
                begin
                if not IsCounted(CurrCell.CellID.RightID, CurrCell) then
                    Exit;
                end;

            if (RowDiff > 0) and (remain = 0) then
                begin
                if CurrCell.CellID.RowIdx > PreCell.CellID.RowIdx then
                    begin
                    if not IsCounted(CurrCell.CellID.AboveID, CurrCell) then
                        Exit;
                    end
                else
                    begin
                    if not IsCounted(CurrCell.CellID.BelowID, CurrCell) then
                        Exit;
                    end;

                RowDiff := RowDiff - 1;
                end;

            ColDiff := ColDiff - 1;
            if remain > 0 then
                remain := remain - 1
            else
                remain := AddRatio;
            end;
        end;

    //--------------------------------------------------------------------------
    procedure CountUpSkipCells(const currID: integer; PreCell: TPedestrianCell);
        var
            nextidx  : integer;
            RowDiff  : integer;
            ColDiff  : integer;
            CurrCell : TPedestrianCell;
        begin
        nextidx := currID;
        if not p_map.RequireCellInfo(nextidx, CurrCell) then
            Exit;

        RowDiff := Abs(CurrCell.CellID.RowIdx - PreCell.CellID.RowIdx);
        ColDiff := Abs(CurrCell.CellID.ColumnIdx - PreCell.CellID.ColumnIdx);
        if RowDiff >= ColDiff then
            CountUpRowtoCol(PreCell, CurrCell, RowDiff, ColDiff)
        else
            CountUpColtoRow(PreCell, CurrCell, RowDiff, ColDiff);
        end;

    //--------------------------------------------------------------------------
    function CalcCurrentCellIndex(const ch: IF8CharacterInstance; ccRecord: TCharacterAndCell): integer;
        var
            PreviousCell : TPedestrianCell;
            Xdifference  : double;
            Zdifference  : double;
            XCellStep    : integer;
            ZCellStep    : integer;
            idx          : integer;
        begin
        Result := ccRecord.CellIdx;
        if ccRecord.CellIdx = -1 then
            begin
            if not p_map.RequireCellInfo(0, PreviousCell) then
                Exit;
            {$ifdef Debug}
            outputDebugString(PWideChar(Format('inoutLog,%d:%d,%d,in,,%f,%f,%s',[MinuteOf(theApplicationServices.Project.DateTime),
                                                                 SecondOf(theApplicationServices.Project.DateTime),
                                                                 PreviousCell.CellID.ID,
                                                                 ch.position[_x],
                                                                 ch.position[_z],
                                                                 ch.GUID.ToString])));
            {$endif}
            Xdifference := ch.position[_x] - PreviousCell.AreaGL[VertexPositionType._LeftTop].X;
            Zdifference := ch.position[_z] - PreviousCell.AreaGL[VertexPositionType._LeftTop].Z;
            XCellStep := Trunc(Xdifference / p_Map.Config.CellSize);
            ZCellStep := Trunc(Zdifference / p_Map.Config.CellSize);
            Result := (p_Map.Config.ColumnCount * ZCellStep) + XCellStep;
            end
        else
            begin
            if not p_map.RequireCellInfo(ccRecord.CellIdx, PreviousCell) then
                Exit;

            Xdifference := ch.position[_x] - PreviousCell.AreaGL[VertexPositionType._LeftTop].X;
            Zdifference := ch.position[_z] - PreviousCell.AreaGL[VertexPositionType._LeftTop].Z;
            if IsCharacterSkipCell(Xdifference, Zdifference) then
                begin
                XCellStep := Trunc(Xdifference / p_Map.Config.CellSize);
                ZCellStep := Trunc(Zdifference / p_Map.Config.CellSize);
                idx    := ccRecord.CellIdx + (p_Map.Config.ColumnCount * ZCellStep) + XCellStep;
                Result := idx;
//                if PreviousCell.Status = PedestrianAreaStatus._pasNoEntry then
//                    begin
//                    if controller.playingSpeed > 0 then
//                        begin
//                        if p_CellsInOutArr[ccRecord.CellIdx] < 1 then
//                            p_CellsInOutArr[ccRecord.CellIdx] := 0
//                        else
//                            p_CellsInOutArr[ccRecord.CellIdx] := p_CellsInOutArr[ccRecord.CellIdx] - 1;
//                        end
//                    else
//                        p_CellsInOutArr[ccRecord.CellIdx] := p_CellsInOutArr[ccRecord.CellIdx] + 1;
//                    end
//                else
                    CountUpSkipCells(idx, PreviousCell);
                Exit;
                end;

            if Xdifference >= p_Map.Config.CellSize then
                begin
                Result := PreviousCell.CellID.RightID;
                {$ifdef Debug}
                outputDebugString(PWideChar(Format('inoutLog,%d:%d,%d,,out,%f,%f,%s',[MinuteOf(theApplicationServices.Project.DateTime),
                                                                 SecondOf(theApplicationServices.Project.DateTime),
                                                                 PreviousCell.CellID.ID,
                                                                 ch.position[_x],
                                                                 ch.position[_z],
                                                                 ch.GUID.ToString])));
                outputDebugString(PWideChar(Format('inoutLog,%d:%d,%d,in,,%f,%f,%s',[MinuteOf(theApplicationServices.Project.DateTime),
                                                                 SecondOf(theApplicationServices.Project.DateTime),
                                                                 PreviousCell.CellID.RightID,
                                                                 ch.position[_x],
                                                                 ch.position[_z],
                                                                 ch.GUID.ToString])));
                {$endif}
                end
            else if Xdifference < 0 then
                begin
                Result := PreviousCell.CellID.LeftID;
                {$ifdef Debug}
                outputDebugString(PWideChar(Format('inoutLog,%d:%d,%d,,out,%f,%f,%s',[MinuteOf(theApplicationServices.Project.DateTime),
                                                                 SecondOf(theApplicationServices.Project.DateTime),
                                                                 PreviousCell.CellID.ID,
                                                                 ch.position[_x],
                                                                 ch.position[_z],
                                                                 ch.GUID.ToString])));
                outputDebugString(PWideChar(Format('inoutLog,%d:%d,%d,in,,%f,%f,%s',[MinuteOf(theApplicationServices.Project.DateTime),
                                                                 SecondOf(theApplicationServices.Project.DateTime),
                                                                 PreviousCell.CellID.LeftID,
                                                                 ch.position[_x],
                                                                 ch.position[_z],
                                                                 ch.GUID.ToString])));
                {$endif}
                end
            else if Zdifference >= p_Map.Config.CellSize then
                begin
                Result := PreviousCell.CellID.BelowID;
                {$ifdef Debug}
                outputDebugString(PWideChar(Format('inoutLog,%d:%d,%d,,out,%f,%f,%s',[MinuteOf(theApplicationServices.Project.DateTime),
                                                                 SecondOf(theApplicationServices.Project.DateTime),
                                                                 PreviousCell.CellID.ID,
                                                                 ch.position[_x],
                                                                 ch.position[_z],
                                                                 ch.GUID.ToString])));
                outputDebugString(PWideChar(Format('inoutLog,%d:%d,%d,in,,%f,%f,%s',[MinuteOf(theApplicationServices.Project.DateTime),
                                                                 SecondOf(theApplicationServices.Project.DateTime),
                                                                 PreviousCell.CellID.BelowID,
                                                                 ch.position[_x],
                                                                 ch.position[_z],
                                                                 ch.GUID.ToString])));
                {$endif}
                end
            else if Zdifference < 0 then
                begin
                Result := PreviousCell.CellID.AboveID;
                {$ifdef Debug}
                outputDebugString(PWideChar(Format('inoutLog,%d:%d,%d,,out,%f,%f,%s',[MinuteOf(theApplicationServices.Project.DateTime),
                                                                 SecondOf(theApplicationServices.Project.DateTime),
                                                                 PreviousCell.CellID.ID,
                                                                 ch.position[_x],
                                                                 ch.position[_z],
                                                                 ch.GUID.ToString])));
                outputDebugString(PWideChar(Format('inoutLog,%d:%d,%d,in,,%f,%f,%s',[MinuteOf(theApplicationServices.Project.DateTime),
                                                                 SecondOf(theApplicationServices.Project.DateTime),
                                                                 PreviousCell.CellID.AboveID,
                                                                 ch.position[_x],
                                                                 ch.position[_z],
                                                                 ch.GUID.ToString])));
                {$endif}
                end
            else
                Exit;

//            if PreviousCell.Status = PedestrianAreaStatus._pasNoEntry then
//                begin
//                if controller.playingSpeed > 0 then
//                    begin
//                    if p_CellsInOutArr[ccRecord.CellIdx] < 1 then
//                        p_CellsInOutArr[ccRecord.CellIdx] := 0
//                    else
//                        p_CellsInOutArr[ccRecord.CellIdx] := p_CellsInOutArr[ccRecord.CellIdx] - 1;
//                    end
//                else
//                    p_CellsInOutArr[ccRecord.CellIdx] := p_CellsInOutArr[ccRecord.CellIdx] + 1;
//                end;
            end;
        end;

    //--------------------------------------------------------------------------
    var
        CurrIdx     : integer;
    begin
    if not Assigned(p_Map) then
        Exit;

    CurrIdx := CalcCurrentCellIndex(ch, ccRecord);
    if CurrIdx = ccRecord.CellIdx then
        Exit;

    if (ccRecord.CellIdx <> -1) and (ccRecord.CellIdx <> CurrIdx) then
        CountUpPassedNum(ccRecord.CellIdx, controller);

    ccRecord.CellIdx := CurrIdx;
    p_CharacterCellList[p_CharacterIDs.IndexOf(ch.id)] := ccRecord;
    end;

//==============================================================================
function TLogCrossSectionFlow.IsCharacterSkipCell(const xdiff, zdiff: double): boolean;
    begin
    Result := false;
    if (xdiff >= p_Map.Config.CellSize * 2) or (zdiff >= p_Map.Config.CellSize * 2)
        or (xdiff < -p_Map.Config.CellSize) or (zdiff < -p_Map.Config.CellSize) then
        Result := true;
    end;

//==============================================================================
function TLogCrossSectionFlow.IsCharacterOutOfMesh(const ch: IF8CharacterInstance; meshLT, meshRB: TPedestrianCell): boolean;
    begin
    if not Assigned(ch) then
        Exit(false);
    Result := (ch.position[_x] < meshLT.AreaGL[VertexPositionType._LeftTop].X)
              or (ch.position[_z] < meshLT.AreaGL[VertexPositionType._LeftTop].Z)
              or (ch.position[_x] > meshRB.AreaGL[VertexPositionType._RightBottom].X)
              or (ch.position[_z] > meshRB.AreaGL[VertexPositionType._RightBottom].Z);
    end;

//==============================================================================
procedure TLogCrossSectionFlow.CollectData(const controller: F8CrowdSimControllerClass);
    var
        traArray: TransientObjectArray;
        charaAndCell : TCharacterAndCell;
        previousCC   : TCharacterAndCell;
        chara        : IF8CharacterInstance;
        mapLeftTop, mapRightBottom: TPedestrianCell;
        currentTime: TDateTime;
        i: Integer;
    begin
    if not Assigned(controller) then
        Exit;

    if not (controller.currentState in [csPlay, csFast, csBack]) then
        Exit;

    currentTime := controller.currentTime;

    if not (MilliSecondsBetween(currentTime, p_SimBeginningTime) >= COLLECT_WAIT_MILLISEC) then
        Exit;

    SetDefaultInterval;
    if not (Assigned(p_API) and Assigned(p_API.project)) then
        Exit;

    if not p_Map.RequireCellInfo(0, mapLeftTop) then
        Exit;

    if not p_Map.RequireCellInfo(p_Map.Config.AllCellCount -1, mapRightBottom) then
        begin
        Exit;
        end;

    traArray := p_API.project.GetTransientObjectsOfEveryTypeWithinCenter(MaxDouble, AsGLPointType(0, 0, 0));

    for i := 0 to Length(traArray) - 1 do
        begin

        if traArray[i].IsCarInstance then
            Continue;

        if not Supports(traArray[i], IF8CharacterInstance, chara) then
            continue;

        if chara.objectVisibility = false then
            continue;

        if IsCharacterOutOfMesh(chara, mapLeftTop, mapRightBottom) then
            begin
            if p_CharacterIDs.IndexOf(chara.id) <> -1 then
                begin
                charaAndCell := p_CharacterCellList[p_CharacterIDs.IndexOf(chara.id)];
                CharacterOutFromMesh(chara, controller, charaAndCell);
                Continue;
                end
            else
                Continue;
            end;

        if p_CharacterIDs.IndexOf(chara.id) = -1 then
            AddNewCharacter(chara, charaAndCell);

        charaAndCell := p_CharacterCellList[p_CharacterIDs.IndexOf(chara.id)];
        previousCC := charaAndCell;
        GetCurrentCellOnCharacter(chara, controller, charaAndCell);

        if p_SensorAreaList.Count > 0 then
            GetSensorAreaInOut(chara, previousCC, controller, charaAndCell);

        CollectLogAndPassedData(currentTime);

        end;
    end;

//==============================================================================
procedure TLogCrossSectionFlow.CollectLogAndPassedData(const CurrTime: TDateTime);
    begin
    if (SecondsBetween(CurrTime, p_Logbeginningtime) >= (p_LogExportInterval * 60)) and (p_SensorAreaList.Count > 0) then
        begin
        CollectCSFLog(p_SimBeginningTime, CurrTime, p_Logbeginningtime);
        p_Logbeginningtime := IncSecond(p_Logbeginningtime, p_LogExportInterval * 60);
        end;

    if SecondsBetween(CurrTime, p_Colbeginningtime) >= (p_AddCellsDataInterval * 60) then
        begin
        CollectAndResetCellPassData;
        p_Colbeginningtime := IncSecond(p_Colbeginningtime, p_AddCellsDataInterval * 60);
        end;
    end;

//==============================================================================
procedure TLogCrossSectionFlow.ExecuteCollect(const SimBegTime, CurrTime: TDateTime);
    begin
    CollectCSFLog(SimBegTime, CurrTime, p_Logbeginningtime);
    CollectAndResetCellPassData;
    end;

//==============================================================================
procedure TLogCrossSectionFlow.ExportLog(const SimEndTime: TDateTime; SimStopped: boolean);
    var
        EndTstr    : string;
        BegTstr    : string;
        outputpath : string;
        fileName   : string;
        i          : integer;
        newFile    : THandle;
        writer     : TStreamWriter;
    begin
    if (not Assigned(p_API)) or (not p_IsExportCrossSectionLog) then
        Exit;

    if SimStopped then
        ExecuteCollect(p_SimBeginningTime, SimEndTime);

    if p_LogDataList.Count <= 1 then
        Exit;

    outputpath := p_API.UserDirectory+CSLOG_PATH;
    DateTimeToString(BegTstr, 'yyyy年mm月dd日_hh時nn分ss秒', p_SimBeginningTime);
    DateTimeToString(EndTstr, 'yyyy年mm月dd日_hh時nn分ss秒', SimEndTime);
    fileName := 'Log_断面交通流_'+BegTstr+'-'+EndTstr+'.csv';
    if FileExists(TPath.Combine(outputpath, fileName)) = false then
        begin
        newFile := FileCreate(TPath.Combine(outputpath, fileName));
        FileClose(newFile);
        end;

    writer := TStreamWriter.Create(TPath.Combine(outputpath, fileName), False, TEncoding.UTF8);
    try
        for i := 0 to p_LogDataList.Count - 1 do
            writer.WriteLine(p_LogDataList[i]);
    finally
        writer.Free;
        end;
    end;

//==============================================================================
procedure TLogCrossSectionFlow.SetDefaultInterval;
    const
        DEFAULT_INTERVAL = 5;
    begin
    if (p_LogExportInterval = -1) then
        p_LogExportInterval := DEFAULT_INTERVAL;

    if (p_AddCellsDataInterval = -1) then
        p_AddCellsDataInterval := DEFAULT_INTERVAL;
    end;

//==============================================================================
procedure TLogCrossSectionFlow.SetLogHeader;
    var
        strArr    : TStringDynArray;
    begin
    if (not Assigned(p_LogDataList)) or (not p_IsExportCrossSectionLog) then
        Exit;

    SetLength(strArr, 12);
    strArr[_saName]      := '計測範囲名';
    strArr[_simPassTime] := 'シミュレーション経過時間(分)';
    strArr[_simCurrTime] := 'シミュレーション時刻';
    strArr[_saCenLat]    := '計測範囲中心：北緯';
    strArr[_saCenLon]    := '計測範囲中心：東経';
    strArr[_saRange]     := '計測半径(m)';
    strArr[_saOriLat]    := '計測範囲原点：北緯';
    strArr[_saOriLon]    := '計測範囲原点：東経';
    strArr[_saEndLat]    := '計測範囲終点：北緯';
    strArr[_saEndLon]    := '計測範囲終点：東経';
    strArr[_saPassedNum] := '通過人数';
    strArr[_saDensity]   := '人流密度(人/m^2)';
    p_LogDataList.Add(String.Join(',', strArr));
    end;

//==============================================================================
function TLogCrossSectionFlow.IsExistSameName(const aName: String): Boolean;
    var
        i: Integer;
    begin
    Result := False;

    for i := 0 to p_SensorAreaList.Count - 1 do
        begin
        if aName = p_SensorAreaList[i].SensorAreaName then
            begin
            Result := True;
            Exit;
            end;
        end;
    end;

procedure TLogCrossSectionFlow.PlayStart(const aStartTime: TDateTime);
    begin
    p_Logbeginningtime := aStartTime;
    p_Colbeginningtime := aStartTime;
    p_SimBeginningTime := aStartTime;
    end;

//==============================================================================
procedure TLogCrossSectionFlow.CharacterOutFromMesh(const ch: IF8CharacterInstance; controller: F8CrowdSimControllerClass; var ccRecord: TCharacterAndCell);
    var
        previousCell : TPedestrianCell;
        updateSAData : TSensorAreaData;
        xDifference  : double;
        zDifference  : double;
    begin
    if ccRecord.CellIdx = -1 then
        Exit;

    if not p_Map.RequireCellInfo(ccRecord.CellIdx, previousCell) then
        Exit;

    {$ifdef Debug}
    outputDebugString(PWideChar(Format('inoutLog,%d:%d,%d,,out,%f,%f,%s',[MinuteOf(theApplicationServices.Project.DateTime),
                                                                 SecondOf(theApplicationServices.Project.DateTime),
                                                                 PreviousCell.CellID.ID,
                                                                 ch.position[_x],
                                                                 ch.position[_z],
                                                                 ch.GUID.ToString])));
    {$endif}
    xDifference := ch.position[_x] - previousCell.AreaGL[VertexPositionType._LeftTop].X;
    zDifference := ch.position[_z] - previousCell.AreaGL[VertexPositionType._LeftTop].Z;

    if (xDifference >= p_Map.Config.CellSize) or (xDifference < 0)
        or (zDifference >= p_Map.Config.CellSize) or (zDifference < 0) then
        begin
        CountUpPassedNum(ccRecord.CellIdx, controller);
        ccRecord.CellIdx := -1;
        p_CharacterCellList[p_CharacterIDs.IndexOf(ch.id)] := ccRecord;
        end
    else
        Exit;

    if ccRecord.IntoSAName <> '' then
        begin
        for var i := 0 to p_SensorAreaList.Count - 1 do
            begin
            if IsCharacterOutOfSensorArea(ch, i) and (ccRecord.IntoSAName = p_SensorAreaList[i].SensorAreaName) then
                begin
                updateSAData := p_SensorAreaList[i];
                if controller.playingSpeed > 0 then
                    updateSAData.PassedNum := updateSAData.PassedNum + 1
                else
                    begin
                    if updateSAData.PassedNum < 1 then
                        updateSAData.PassedNum := 0
                    else
                        updateSAData.PassedNum := updateSAData.PassedNum - 1;
                    end;

                p_SensorAreaList[i] := updateSAData;
                ccRecord.IntoSAName := '';
                p_CharacterCellList[p_CharacterIDs.IndexOf(ch.id)] := ccRecord;
                end
            else
                Continue;
            end;
        end
    end;

//==============================================================================
procedure TLogCrossSectionFlow.GetSensorAreaInOut(const ch: IF8CharacterInstance; preCC: TCharacterAndCell; controller: F8CrowdSimControllerClass; var currCC: TCharacterAndCell);
    var
        updateSAData : TSensorAreaData;
    begin
    if preCC.IntoSAName <> '' then
        begin
        for var i := 0 to p_SensorAreaList.Count - 1 do
            begin
            if IsCharacterOutOfSensorArea(ch, i) and (preCC.IntoSAName = p_SensorAreaList[i].SensorAreaName) then
                begin
                updateSAData := p_SensorAreaList[i];
                if controller.playingSpeed > 0 then
                    updateSAData.PassedNum := updateSAData.PassedNum + 1
                else
                    begin
                    if updateSAData.PassedNum < 1 then
                        updateSAData.PassedNum := 0
                    else
                        updateSAData.PassedNum := updateSAData.PassedNum - 1;
                    end;

                p_SensorAreaList[i] := updateSAData;
                currCC.IntoSAName := '';
                p_CharacterCellList[p_CharacterIDs.IndexOf(ch.id)] := currCC;
                end
            else
                Continue;
            end;
        end
    else
        begin
        for var i := 0 to p_SensorAreaList.Count - 1 do
            begin
            if IsCharacterOutOfSensorArea(ch, i) then
                Continue
            else
                begin
                currCC.IntoSAName := p_SensorAreaList[i].SensorAreaName;
                p_CharacterCellList[p_CharacterIDs.IndexOf(ch.id)] := currCC;
                end;
            end;
        end;
    end;

//==============================================================================
procedure TLogCrossSectionFlow.SetGraphData(var Graph: TChart);

    //--------------------------------------------------------------------------
    function IsLogCollected(const LogList: TList<string>): boolean;
        begin
        Result := false;
        if Assigned(LogList) then
            Result := LogList.Count > 0;
        end;

    //--------------------------------------------------------------------------
    procedure AddData(const DataStr: string);
        var
            i      : integer;
            Data   : TStringList;
        begin
        Data := TStringList.Create;
        try
        Data.Delimiter       := ',';
        Data.StrictDelimiter := true;
        Data.DelimitedText   := DataStr;
        if Graph.SeriesCount = 0 then
            begin
            Graph.AddSeries(TLineSeries.Create(Graph));
            Graph.Series[0].Title     := Data[_saName];
            Graph.Series[0].Pen.Width := 1;
            Graph.Series[0].AddXY(0, 0);
            end;

        for i := 0 to Graph.SeriesCount - 1 do
            begin
            if Graph.Series[i].Title = Data[_saName] then
                begin
                Graph.Series[i].AddXY(StrToInt(Data[_simPassTime]), StrToInt(Data[_saPassedNum]));
                Break;
                end;

            if (i = Graph.SeriesCount - 1) then
                begin
                Graph.AddSeries(TLineSeries.Create(Graph));
                Graph.Series[i + 1].Title     := Data[_saName];
                Graph.Series[i + 1].Pen.Width := 1;
                Graph.Series[i + 1].AddXY(0, 0);
                Graph.Series[i + 1].AddXY(StrToInt(Data[_simPassTime]), StrToInt(Data[_saPassedNum]));
                end;
                end;
        finally
            FreeAndNil(Data);
            end;
        end;

    //--------------------------------------------------------------------------
    var
        i : integer;
    begin
    if Assigned(Graph) then
        begin
        while Graph.SeriesCount > 0 do
            Graph.RemoveSeries(0);

        if IsLogCollected(p_LogDataList) then
            begin
            for i := 1 to p_LogDataList.Count - 1 do
                AddData(p_LogDataList[i]);
            end;
        end;
    end;

procedure TLogCrossSectionFlow.ExportToPluginData;
    var
        i          : integer;
        tmpProject : IF8ProjectForRoad;
    begin
    tmpProject := theApplicationServices.project;
    if Assigned(tmpProject) then
        begin
        tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, 'SACount'] := p_SensorAreaList.Count.ToString;
        for i := 0 to p_SensorAreaList.Count - 1 do
            begin
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.Name']           := p_SensorAreaList[i].SensorAreaName;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.AreaOrigin_X']   := p_SensorAreaList[i].AreaOrigin.X.ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.AreaOrigin_Y']   := p_SensorAreaList[i].AreaOrigin.Y.ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.AreaOrigin_Z']   := p_SensorAreaList[i].AreaOrigin.Z.ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.AreaEnd_X']      := p_SensorAreaList[i].AreaEnd.X.ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.AreaEnd_Y']      := p_SensorAreaList[i].AreaEnd.Y.ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.AreaEnd_Z']      := p_SensorAreaList[i].AreaEnd.Z.ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.LatLonOrigin_X'] := p_SensorAreaList[i].LatLonOrigin.X.ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.LatLonOrigin_Y'] := p_SensorAreaList[i].LatLonOrigin.Y.ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.LatLonOrigin_Z'] := p_SensorAreaList[i].LatLonOrigin.Z.ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.LatLonEnd_X']    := p_SensorAreaList[i].LatLonEnd.X.ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.LatLonEnd_Y']    := p_SensorAreaList[i].LatLonEnd.Y.ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.LatLonEnd_Z']    := p_SensorAreaList[i].LatLonEnd.Z.ToString;
            end;
        end;

    tmpProject := nil;
    end;

procedure TLogCrossSectionFlow.ImportFromPluginData;
    var
        tmpCount, i : integer;
        tmpProject  : IF8ProjectForRoad;
        tmpSA : TSensorAreaData;
        tmpPoint : TPoint3D;
    begin
    tmpProject := theApplicationServices.project;
    if Assigned(tmpProject) then
        begin
        tmpCount := StrToIntDef(tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, 'SACount'], 0);
        if tmpCount < 1 then
            begin
            tmpProject := nil;
            Exit;
            end;

        for i := 0 to tmpCount - 1 do
            begin
            tmpSA.SensorAreaName := tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.Name'];
            tmpPoint.X           := StrToFloat(tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.AreaOrigin_X']);
            tmpPoint.Y           := StrToFloat(tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.AreaOrigin_Y']);
            tmpPoint.Z           := StrToFloat(tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.AreaOrigin_Z']);
            tmpSA.AreaOrigin     := tmpPoint;

            tmpPoint.X           := StrToFloat(tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.AreaEnd_X']);
            tmpPoint.Y           := StrToFloat(tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.AreaEnd_Y']);
            tmpPoint.Z           := StrToFloat(tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.AreaEnd_Z']);
            tmpSA.AreaEnd        := tmpPoint;

            tmpPoint.X           := StrToFloat(tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.LatLonOrigin_X']);
            tmpPoint.Y           := StrToFloat(tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.LatLonOrigin_Y']);
            tmpPoint.Z           := StrToFloat(tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.LatLonOrigin_Z']);
            tmpSA.LatLonOrigin   := tmpPoint;

            tmpPoint.X           := StrToFloat(tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.LatLonEnd_X']);
            tmpPoint.Y           := StrToFloat(tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.LatLonEnd_Y']);
            tmpPoint.Z           := StrToFloat(tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('CrossSectionSensorArea[%d]', [i]) + '.LatLonEnd_Z']);
            tmpSA.LatLonEnd      := tmpPoint;

            p_SensorAreaList.Add(tmpSA);
            end;
        end;

    tmpProject := nil;
    end;
end.
