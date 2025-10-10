unit PedestrianMap;

interface

uses
    System.Generics.Collections,
    system.SyncObjs,
    System.Classes,
    System.JSON,
    XML.XMLIntf,
    PluginCore,
    F8OpenGL,
    F8Utils,
    PedestrianCell,
    PedestrianCellGroup_CrossWalk,
    PedestrianCellGroup_Area,
    MapOrigin,
    MapRenderer,
    CellID,
    TranRoadList,
    TranRoad,
    crowdSimCityFurniture,
    PedestrianUtil,
    PedestrianMapUnificator;

type
    /// <summary>
    ///    歩行マップを表すクラス
    /// </summary>
    TPedestrianMap = class
        private
            p_MapName: String;
            p_Config  : TPedestrianMatrixConfig;
            p_HeightOffset: Double;
            p_LeftTopLocal: TPoint3D;
            p_Renderer: TMapRenderer;
            p_CellList: TObjectList<TPedestrianCell>;
            p_CrossWalks: TPedestrianCrossWalkList;
            p_CrossWalkIntervalSecond: Integer;
            p_CellListCS: TCriticalSection;
            p_Unificator: TPedestrianMapUnificator;
            p_RendererChangeQueue: TQueue<Integer>;

            p_NoEntrySet : TPedestrianCellGroupAreaMap;

            procedure SetName(const aValue: String);
            procedure SetHeightOffset(const aValue: Double);
            function  GetHeightOffset: Double;
            procedure SetCrossWalkIntervalSecond(const aValue: Integer);
            function  ExtractRenderData: VertexDataArray;
            procedure MakeCell(const aMapOrigin: TMatrixOrigin; const aStatusList: PedestrianAreaStatusList;
              out aRenderingData: VertexDataArray);
            procedure OnUpdateCrossWalkEvent(const aCell: TPedestrianCell);
            procedure OnBeforeEndCrossingEditEvent;
            procedure UpdateRenerer;

            function  FindAreaCornerCellID(const aHoriConv: IF8WrHorizontalCoordinateConvertor; const aArea: VertexPositions;
              out aCorner: CornerCellID): Boolean;
            function  FindNextCell(const aCellID: TCellID; const aCorner: CornerCellID): Integer;

            property  LeftTopLocal: TPoint3D read p_LeftTopLocal;
            constructor Create; overload;
        public
            constructor Create(const aHoriConv: IF8WrHorizontalCoordinateConvertor; const aConfig: TPedestrianMatrixConfig;
              const aStatusList: PedestrianAreaStatusList = nil); overload;
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            function  Clone: TPedestrianMap;
            // 道路の歩道エリアを塗りつぶす(歩行可能にする)
            procedure OverlayRoad(const aHoriConv: IF8WrHorizontalCoordinateConvertor; const aRoad: TTranRoad); overload;
            // 都市構造物で塗りつぶす(歩行不可能にする)
            procedure OverlayCityFurniture(const aHoriConv: IF8WrHorizontalCoordinateConvertor; const aCityFurniture: TCrowdSimCityFurniture);
            // 描画関数
            procedure DoRender(const aOpenGL: TF8OpenGL);
            // 編集用
            function  RequireCellInfo(const aLocalXZ: TPoint3D; out aCell: TPedestrianCell; out aDiff: TPoint3D): Boolean; overload;
            function  RequireCellInfo(const aLocalXZ: TPoint3D; out aID: TCellID; out aStatus: PedestrianAreaStatus): Boolean; overload;
            function  RequireCellInfo(const aLocalXZ: TPoint3D; out aID: TCellID; out aStatus: PedestrianAreaStatus; out aDiff: TPoint3D): Boolean; overload;
            function  RequireCellInfo(const aRowIdx, ColumnIdx: Integer; out aCell: TPedestrianCell): Boolean; overload;
            function  RequireCellInfo(const aIdx: Integer; out aCell: TPedestrianCell): Boolean; overload;
            function  RequireCellsInfo(const aLocalXZ: TPoint3D; const aRange: Cardinal; out aCells: TObjectList<TPedestrianCell>): Boolean;
            procedure ChangeCellState(const aID: Integer; const aNewState: PedestrianAreaStatus);

            function  NearbySearch(const Range: Integer; centerID: TCellID; out res: TPedestrianCell) : Boolean;
            function  OnBeforeExportXML: Boolean;
            procedure ExportMapAreaWithMargin(out aLeftTop, aRightBottom: TPoint3D);
            procedure ExportRoadMap(const aStrList: TStringList);

            procedure ResetSignals;
            procedure OnTimeStep(const dTimeInSeconds: Double; const dTimeFromStart: Integer);
            function  GenerateCandidateWalkingPath: TParametricLineList;

            property  MapName                : String                   read p_MapName                 write SetName;
            property  Config                 : TPedestrianMatrixConfig  read p_Config;
            property  HeightOffset           : Double                   read GetHeightOffset write SetHeightOffset;
            property  CrossWalks             : TPedestrianCrossWalkList read p_CrossWalks;
            property  CrossWalkIntervalSecond: Integer                  read p_CrossWalkIntervalSecond write SetCrossWalkIntervalSecond;
        end;

    TPedestrianMapPluginData = class
        private
            class procedure ExportConfigData(const aConfig: TPedestrianMatrixConfig; const aParentNode: IXMLNode);
            class procedure ExportAreaStatusList(const aMap: TPedestrianMap; const aParentNode: IXMLNode);
            class function  ImportConfigdata(const aDataNode: IXMLNode; out aConfig: TPedestrianMatrixConfig): Boolean;
            class function  ImportAreaStatusList(const aDataNode: IXMLNode; const aListLength: Integer; out aStatusList: PedestrianAreaStatusList): Boolean;
        public
            class procedure ExportPluginData(const aMap: TPedestrianMap; const aParentNode: IXMLNode);
            class procedure ExportToXMLFile(const aMap: TPedestrianMap; const aDir: String);
            class function  ImportPluginData(const aAPI: IF8ApplicationServices; const aDataNode: IXMLNode; out aRes: TPedestrianMap): Boolean;
        end;

    TPedestrianMapJSONData = class
        private
            class procedure ExportConfigData(const aConfig: TPedestrianMatrixConfig; const aParentNode: TJSONObject);
            class procedure ExportAreaStatusList(const aMap: TPedestrianMap; const aParentNode: TJSONObject);

        public
            class procedure ExportData(const aMap: TPedestrianMap; const aParentNode: TJSONObject);
            class procedure ExportToJSONFile(const aMap: TPedestrianMap; const aDir: String);
        end;

implementation

uses
    System.SysUtils,
    System.Math,
    Xml.xmldom,
    Xml.XMLDoc,
    LatLonHelper;

const
    MARGIN_OF_MAP = 1.0;

    NODE_NAME_MAP = 'TPedestrianMap';
    NODE_NAME_CONFIG = 'TPedestrianMatrixConfig';
    NODE_NAME_STATUS_LIST = 'PedestrianAreaStatusList';
    NODE_NAME_CROSSWALK_LIST = 'CrossWalks';
    TAG_MAP_NAME = 'MapName';
    TAG_LEFT_TOP_LAT = 'LeftTopLat';
    TAG_LEFT_TOP_LON = 'LeftTopLon';
    TAG_CELL_SIZE    = 'CellSize';
    TAG_COLUMN_COUNT = 'ColumnCount';
    TAG_ROW_COUNT    = 'RowCount';
    TAG_HEIGHT_OFFSET = 'MapHeightOffset';
    TAG_CROSSWALK_INTERVAL_SECOND = 'CrossWwalk_Interval_Second';
    VALUE_NO_ENTRY        = 0;
    VALUE_WALKABLE        = 1;


type
    TNavMeshLinkPair = record
        A, B: Integer;// A < B
        procedure SetPair(const aA, aB: Integer);
        end;

    TNavMeshBlock = class
        strict private
            p_RepresentativePoint: TPoint3D;
            p_List: TObjectList<TPedestrianCell>;
        public
            constructor Create(const aList: TObjectList<TPedestrianCell>);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function  CheckRightSide(const aBlock: TNavMeshBlock): Boolean;
            function  CheckBelowSide(const aBlock: TNavMeshBlock): Boolean;

            property  RepresentativePoint: TPoint3D read p_RepresentativePoint;
        end;

{ TPedestrianMap }
constructor TPedestrianMap.Create;
    begin
    p_MapName := '';
    p_CrossWalkIntervalSecond := CROSSWALK_INTERVAL_MIN;
    p_CellListCS  := TCriticalSection.Create;
    p_CellList    := TObjectList<TPedestrianCell>.Create;
    p_Renderer    := TMapRenderer.Create;
    p_CrossWalks  := TPedestrianCrossWalkList.Create;
    p_NoEntrySet  := TPedestrianCellGroupAreaMap.Create(PedestrianAreaStatus._pasNoEntry);
    p_Unificator  := TPedestrianMapUnificator.Create;
    p_RendererChangeQueue := TQueue<Integer>.Create;

    p_CrossWalks.RegisterOnUpdateCrossWalkEvent(OnUpdateCrossWalkEvent);
    p_CrossWalks.OnBeforeChangeEditingEvent := OnBeforeEndCrossingEditEvent;
    p_HeightOffset := 0.0;
    end;

constructor TPedestrianMap.Create(const aHoriConv: IF8WrHorizontalCoordinateConvertor; const aConfig: TPedestrianMatrixConfig;
  const aStatusList: PedestrianAreaStatusList);
    var
        renderingData: VertexDataArray;
        mapOrigin: TMatrixOrigin;
        statusList: PedestrianAreaStatusList;
    begin
    Assert(aConfig.CellSize > 0.0);
    Assert(aConfig.ColumnCount * aConfig.RowCount > 0);

    if not Assigned(aStatusList) then
        SetLength(statuslist, aConfig.ColumnCount * aConfig.RowCount)
    else
        Assert((aConfig.ColumnCount * aConfig.RowCount) = Length(aStatusList));

    Create;
    mapOrigin := TMatrixOrigin.Create(aHoriConv, aConfig.LeftTopLatLon);

    p_Config := aConfig;
    p_LeftTopLocal := mapOrigin.OriginLocal;

    if not Assigned(aStatusList) then
        MakeCell(mapOrigin, statusList, renderingData)
    else
        MakeCell(mapOrigin, aStatusList, renderingData);

    p_Renderer.Initialize(renderingData, aConfig.CellSize);
    end;

procedure TPedestrianMap.AfterConstruction;
    begin
    inherited;
    {no action}
    end;

procedure TPedestrianMap.BeforeDestruction;
    begin
    inherited;

    p_CrossWalks.UnRegisterOnUpdateCrossWalkEvent(OnUpdateCrossWalkEvent);
    p_CrossWalks.OnBeforeChangeEditingEvent := nil;

    FreeAndNil(p_Renderer);
    FreeAndNil(p_CellList);
    FreeAndNil(p_CellListCS);
    FreeAndNil(p_CrossWalks);
    FreeAndNil(p_NoEntrySet);
    FreeAndNil(p_Unificator);
    FreeAndNil(p_RendererChangeQueue);
    end;

function TPedestrianMap.Clone: TPedestrianMap;
    var
        i: Integer;
    begin
    Result := TPedestrianMap.Create;
    Result.p_MapName := p_MapName;
    Result.p_CrossWalkIntervalSecond := p_CrossWalkIntervalSecond;
    Result.p_Config  := p_Config;
    Result.p_LeftTopLocal := p_LeftTopLocal;
    for i := 0 to p_CellList.Count - 1 do
        Result.p_CellList.Add(p_CellList[i].Clone);
    Result.CrossWalks.AssignFrom(CrossWalks, p_CellList);
    Result.p_Renderer.Initialize(ExtractRenderData, p_Config.CellSize);
    end;

procedure TPedestrianMap.OverlayRoad(const aHoriConv: IF8WrHorizontalCoordinateConvertor; const aRoad: TTranRoad);
    var
        i: Integer;
        roadCorner: CornerCellID;
    begin
    p_CellListCS.Enter;
    try
        if not FindAreaCornerCellID(aHoriConv, aRoad.BoundingArea, roadCorner) then
            Exit;

        i := roadCorner[VertexPositionType._LeftTop].ID;
        while i <= roadCorner[VertexPositionType._RightBottom].ID do
            begin
            if p_CellList[i].Status = PedestrianAreaStatus._pasNoEntry then
                begin
                if aRoad.IsOverlayPedestrianArea(p_CellList[i].Area) then
                    begin
                    p_CellList[i].Status := PedestrianAreaStatus._pasWalkable;
                    p_RendererChangeQueue.Enqueue(i);
                    end;
                end;

            i := FindNextCell(p_CellList[i].CellID, roadCorner);
            end;
    finally
        p_CellListCS.Leave;
        end;
    end;

procedure TPedestrianMap.OverlayCityFurniture(const aHoriConv: IF8WrHorizontalCoordinateConvertor; const aCityFurniture: TCrowdSimCityFurniture);
    var
        i: Integer;
        furnitureCorner: CornerCellID;
    begin
    p_CellListCS.Enter;
    try
        if not FindAreaCornerCellID(aHoriConv, aCityFurniture.BoundingArea, furnitureCorner) then
            Exit;

        i := furnitureCorner[VertexPositionType._LeftTop].ID;
        while i <= furnitureCorner[VertexPositionType._RightBottom].ID do
            begin
            if p_CellList[i].Status = PedestrianAreaStatus._pasWalkable then
                begin
                if aCityFurniture.IsOverlayPedestrianArea(p_CellList[i].Area) then
                    begin
                    p_CellList[i].Status := PedestrianAreaStatus._pasNoEntry;
                    p_RendererChangeQueue.Enqueue(i);
                    end;
                end;

            i := FindNextCell(p_CellList[i].CellID, furnitureCorner);
            end;
    finally
        p_CellListCS.Leave;
        end;
    end;

procedure TPedestrianMap.DoRender(const aOpenGL: TF8OpenGL);
    begin
    UpdateRenerer;
    p_Renderer.DoRender(aOpenGL, Config.CellSize);
    end;

function  TPedestrianMap.RequireCellInfo(const aLocalXZ: TPoint3D; out aCell: TPedestrianCell; out aDiff: TPoint3D): Boolean;
    var
        rowIndex, columnIndex: Integer;
    begin
    Result := False;

    p_CellListCS.Enter;
    try
        aDiff := (aLocalXZ - LeftTopLocal);
        rowIndex    := Trunc( -aDiff.Z / Config.CellSize);
        columnIndex := Trunc( aDiff.X / Config.CellSize);

        if not (InRange(rowIndex, 0, Config.RowCount - 1) and InRange(columnIndex, 0, Config.ColumnCount - 1)) then
            Exit;

        aCell := p_CellList[rowIndex * Config.ColumnCount + columnIndex];
    finally
        p_CellListCS.Leave;
        end;
    Result := True;
    end;

function TPedestrianMap.RequireCellInfo(const aLocalXZ: TPoint3D; out aID: TCellID; out aStatus: PedestrianAreaStatus): Boolean;
    var
        diff: TPoint3D;
    begin
    Result := RequireCellInfo(aLocalXZ, aID, aStatus, diff);
    end;

function TPedestrianMap.RequireCellInfo(const aLocalXZ: TPoint3D; out aID: TCellID; out aStatus: PedestrianAreaStatus; out aDiff: TPoint3D): Boolean;
    var
        aCell: TPedestrianCell;
    begin
    Result := RequireCellInfo(aLocalXZ, aCell, aDiff);
    if Result then
        begin
        aID     := aCell.CellID;
        aStatus := aCell.Status;
        end;
    end;

function TPedestrianMap.RequireCellInfo(const aRowIdx, ColumnIdx: Integer; out aCell: TPedestrianCell): Boolean;
    var
        idx: Integer;
    begin
    idx := aRowIdx * Config.ColumnCount + ColumnIdx;
    Result := RequireCellInfo(idx, aCell);
    end;

function TPedestrianMap.RequireCellInfo(const aIdx: Integer; out aCell: TPedestrianCell): Boolean;
    begin
    Result := False;
    p_CellListCS.Enter;
    try
        if InRange(aIdx, 0, Config.AllCellCount - 1) then
            begin
            aCell := p_CellList[aIdx];
            Result := True;
            end
        else
            aCell := nil;
    finally
        p_CellListCS.Leave;
        end;
    end;

function TPedestrianMap.RequireCellsInfo(const aLocalXZ: TPoint3D; const aRange: Cardinal; out aCells: TObjectList<TPedestrianCell>): Boolean;
    // aRange = 0: 選択した地点のマスのみ aRange = 1: 選択した地点のマスと周囲1マス(計9マス)
    // aLocalXZ地点のマスを中心とする. aLocalXZ地点にマスがない場合は失敗とする
    var
        centerCell, cell: TPedestrianCell;
        diff: TPoint3D;
        cellCount: Integer;
        rowIdx, columnIdx: Integer;
        ri, ci: Cardinal;
    begin
    Result := RequireCellInfo(aLocalXZ, centerCell, diff);

    if not Result then
        Exit;

    aCells := TObjectList<TPedestrianCell>.Create(False);

    if aRange = 0 then
        begin
        aCells.Add(centerCell);
        end
    else
        begin
        // function  RequireCellInfo(const aRowIdx, ColumnIdx: Integer; out aCell: TPedestrianCell): Boolean; overload;
        cellCount := aRange * 2 + 1;
        for ri := 0 to cellCount - 1 do
            begin
            rowIdx := centerCell.CellID.RowIdx - (Integer(ri) - Integer(aRange));
            for ci := 0 to cellCount - 1 do
                begin
                columnIdx := centerCell.CellID.ColumnIdx - (Integer(ci) - Integer(aRange));
                if RequireCellInfo(rowIdx, columnIdx, cell) then
                    aCells.Add(cell);
                end;
            end;
        end;
    end;

procedure TPedestrianMap.ChangeCellState(const aID: Integer; const aNewState: PedestrianAreaStatus);
    begin
    p_CellListCS.Enter;
    try
        if InRange(aID, 0, p_CellList.Count - 1) then
            begin
            // 最外周辺は編集不可
            if (p_CellList[aID].CellID.RowIdx = 0)
            or (p_CellList[aID].CellID.ColumnIdx = 0)
            or (p_CellList[aID].CellID.RowIdx = Config.RowCount - 1)
            or (p_CellList[aID].CellID.ColumnIdx = Config.ColumnCount - 1) then
                Exit;

            p_CellList[aID].Status := aNewState;
            p_RendererChangeQueue.Enqueue(aID);
            end;
    finally
        p_CellListCS.Leave;
        end;
    end;

function TPedestrianMap.OnBeforeExportXML: Boolean;
    var
        walkableSet      : TPedestrianCellGroupAreaMap;
        idxRow, idxColumn: Integer;
        unificated       : boolean;

    function ConnectSeparatedAreas: boolean;
        begin
        Result := p_Unificator.UnificateWalkableMap(p_Config, walkableSet, p_NoEntrySet, p_CellList, p_Renderer);
        end;

    begin
    p_CellListCS.Enter;
    unificated    := true;
    walkableSet := TPedestrianCellGroupAreaMap.Create(PedestrianAreaStatus._pasWalkable);
    try
        p_NoEntrySet.ClearList;
        for idxRow := 0 to Config.RowCount - 1 do
            begin
            for idxColumn := 0 to Config.ColumnCount - 1 do
                begin
                p_NoEntrySet.InputCell(idxRow, p_CellList[idxRow * Config.ColumnCount + idxColumn]);
                walkableSet.InputCell(idxRow, p_CellList[idxRow * Config.ColumnCount + idxColumn]);
                end;
            end;
        p_NoEntrySet.MergeLines;
        walkableSet.MergeLines;
        if WalkableSet.GroupCount > 1 then
            unificated := ConnectSeparatedAreas;

        Result := unificated;
    finally
        FreeAndNil(walkableSet);
        p_CellListCS.Leave;
        end;
    end;

procedure TPedestrianMap.ExportMapAreaWithMargin(out aLeftTop, aRightBottom: TPoint3D);
    var
        t: TPoint3D;
    begin
    t := p_CellList[0].AreaLocal[VertexPositionType._LeftTop];
    t.X := t.X - MARGIN_OF_MAP;
    t.Z := t.Z + MARGIN_OF_MAP;
    aLeftTop := t;
    t := p_CellList[p_CellList.Count - 1].AreaLocal[VertexPositionType._RightBottom];
    t.X := t.X + MARGIN_OF_MAP;
    t.Z := t.Z - MARGIN_OF_MAP;
    aRightBottom := t;
    end;

procedure TPedestrianMap.ExportRoadMap(const aStrList: TStringList);
    const
        // N < N2とする
        N  = 2;
        N2 = 3;

    function  RequireWalkableLine(const aStartID: Integer; const aIDs: TObjectList<TPedestrianCell>): Boolean;
        var
            count: Integer;
        begin
        // aStartIDから始まる横一行の歩行可能エリアを抽出
        aIDs.Clear;
        count := 0;

        while True do
            begin
            if p_CellList[aStartID + count].Status = PedestrianAreaStatus._pasWalkable then
                aIDs.Add(p_CellList[aStartID + count])
            else
                Break;

            if p_CellList[aStartID + count].CellID.RightID = -1 then
                Break;

            Inc(count);
            end;

        Result := aIDs.Count > 0;
        end;

    function  DivideWalkableLine(const aIDs: TObjectList<TPedestrianCell>; const aDividedCells: TObjectList<TObjectList<TPedestrianCell>>): Boolean;
        var
            modN, modN2, modBlockLength: Integer;
            blockLength: Integer;
            blockCount: Integer;
            blockFirstID: Integer;
            i, j: Integer;
        begin
        aDividedCells.Clear;
        // aIDsを分割する
        // (aIDs.Count - 2)をNとN2で割る。余りが偶数と奇数の場合は偶数余りの方を採用. どちらも偶数または奇数の場合は余りが小さい方を採用
        if (aIDs.Count - 2) < N then
            begin
            for i := 0 to aIDs.Count - 1 do
                begin
                aDividedCells.Add(TObjectList<TPedestrianCell>.Create(False));
                aDividedCells.Last.Add(aIDs[i]);
                end;
            end
        else
            begin
            modN  := (aIDs.Count - 2) mod N;
            modN2 := (aIDs.Count - 2) mod N2;

            if Odd(modN) = Odd(modN2) then
                blockLength := IfThen(modN < modN2, N, N2)
            else
                blockLength := IfThen(not Odd(modN), N, N2);

            modBlockLength := (aIDs.Count - 2) mod blockLength;
            blockCount     := (aIDs.Count - 2) div blockLength;

            if modBlockLength <= 1 then
                blockFirstID := 1
            else
                blockFirstID := (modBlockLength div 2) + 1;

            i := 0;
            while i <= aIDs.Count - 1 do
                begin
                if (i < blockFirstID) or (i = aIDs.Count - 1) then
                    begin
                    aDividedCells.Add(TObjectList<TPedestrianCell>.Create(False));
                    aDividedCells.Last.Add(aIDs[i]);
                    Inc(i);
                    end
                else
                    begin
                    if blockCount = 0 then
                        begin
                        aDividedCells.Add(TObjectList<TPedestrianCell>.Create(False));
                        aDividedCells.Last.Add(aIDs[i]);
                        Inc(i);
                        end
                    else
                        begin
                        aDividedCells.Add(TObjectList<TPedestrianCell>.Create(False));
                        for j := i to (i + blockLength) - 1 do
                            aDividedCells.Last.Add(aIDs[j]);
                        i := i + blockLength;
                        Dec(blockCount);
                        end;
                    end;
                end;
            end;

        Result := aDividedCells.Count > 0;
        end;
    var
        i, j: Integer;
        ri, ci: Integer;
        id: Integer;
        checkedColumn: Integer;
        tmpCellList: TList<TPoint3D>;
        tmpLinkCountTable: TDictionary<TPoint3D, Integer>;
        tmpLinkList: TList<TNavMeshLinkPair>;// Link間のインデックス。tmpCellListのインデックスを使用
        linkPair: TNavMeshLinkPair;
        newLine: TObjectList<TPedestrianCell>;
        newBlocks: TObjectList<TObjectList<TPedestrianCell>>;
        cellTable: TObjectDictionary<TPoint3D, TList<Integer>>; // セル/ブロックの中点, 所属セルリスト
        navMeshBlocks: TObjectDictionary<Integer, TObjectList<TNavMeshBlock>>; // 行数, 所属ブロック
        
    procedure RegisterPair(const aKey, aTarget: TPoint3D);
        begin
        // 接続しているセル(ブロック)があれば、その代表点とペアの情報をリストに登録する
        // 接続しているセル(ブロック)をcellPointとする
        if not tmpCellList.Contains(aKey) then
            begin
            tmpCellList.Add(aKey);
            tmpLinkCountTable.Add(aKey, 0);
            end;

        if not tmpCellList.Contains(aTarget) then
            begin
            tmpCellList.Add(aTarget);
            tmpLinkCountTable.Add(aTarget, 0);
            end;

        linkPair.SetPair(tmpCellList.IndexOf(aKey), tmpCellList.IndexOf(aTarget));
        if not tmpLinkList.Contains(linkPair) then
            begin
            tmpLinkList.Add(linkPair);
            tmpLinkCountTable.AddOrSetValue(aKey, tmpLinkCountTable[aKey] + 1);
            tmpLinkCountTable.AddOrSetValue(aTarget, tmpLinkCountTable[aTarget] + 1);
            end;
        end;
    begin
    tmpCellList := TList<TPoint3D>.Create;
    tmpLinkCountTable := TDictionary<TPoint3D, Integer>.Create;
    tmpLinkList := TList<TNavMeshLinkPair>.Create;
    cellTable := TObjectDictionary<TPoint3D, TList<Integer>>.Create([doOwnsValues]);
    navMeshBlocks := TObjectDictionary<Integer, TObjectList<TNavMeshBlock>>.Create([doOwnsValues]);
    try
        newLine      := TObjectList<TPedestrianCell>.Create(False);
        newBlocks    := TObjectList<TObjectList<TPedestrianCell>>.Create(False);
        try
        for ri := 0 to Config.RowCount - 1 do
            begin
            navMeshBlocks.Add(ri, TObjectList<TNavMeshBlock>.Create);
            checkedColumn := -1;
            for ci := 0 to Config.ColumnCount - 1 do
                begin
                if  ci <= checkedColumn then
                    Continue;
                    
                id := ri * Config.ColumnCount + ci;
                if p_CellList[id].Status <> PedestrianAreaStatus._pasWalkable then
                    Continue;

                if not RequireWalkableLine(id, newLine) then
                    Continue;

                if DivideWalkableLine(newLine, newBlocks) then
                    begin
                    for i := 0 to newBlocks.Count - 1 do
                        navMeshBlocks[ri].Add(TNavMeshBlock.Create(newBlocks[i]));
                    end;

                checkedColumn := newLine.Last.CellID.ColumnIdx;
                end;
            end;
        finally
            FreeAndNil(newLine);
            FreeAndNil(newBlocks);
        end;

        for ri := 0 to Config.RowCount - 1 do
            begin
            for i := 0 to navMeshBlocks[ri].Count - 1 do
                begin
                if i <> navMeshBlocks[ri].Count - 1 then
                    begin
                    if navMeshBlocks[ri][i].CheckRightSide(navMeshBlocks[ri][i + 1]) then
                        RegisterPair(navMeshBlocks[ri][i].RepresentativePoint, navMeshBlocks[ri][i + 1].RepresentativePoint);
                    end;
                    
                if ri <> Config.RowCount - 1 then
                    begin
                    for j := 0 to navMeshBlocks[ri + 1].Count - 1 do
                        begin
                        if navMeshBlocks[ri][i].CheckBelowSide(navMeshBlocks[ri + 1][j]) then
                            RegisterPair(navMeshBlocks[ri][i].RepresentativePoint, navMeshBlocks[ri + 1][j].RepresentativePoint);
                        end;
                    end;
                end;
            end;

        aStrList.Add(tmpCellList.Count.ToString);
        for i := 0 to tmpCellList.Count - 1 do
            aStrList.Add(Format('%d %f %f', [tmpLinkCountTable[tmpCellList[i]], tmpCellList[i].X, tmpCellList[i].Z]));

        aStrList.Add(tmpLinkList.Count.ToString);
        for i := 0 to tmpLinkList.Count - 1 do
            aStrList.Add(Format('%d %d', [tmpLinkList[i].A, tmpLinkList[i].B]));
    finally
        FreeAndNil(tmpCellList);
        FreeAndNil(tmpLinkCountTable);
        FreeAndNil(tmpLinkList);
        FreeAndNil(cellTable);
        FreeAndNil(navMeshBlocks);
        end;
    end;

procedure TPedestrianMap.ResetSignals;
    var
        i: Integer;
    begin
    for i := 0 to CrossWalks.ListCount - 1 do
        CrossWalks.CrossWalk[i].ResetSimSignal;
    end;

procedure TPedestrianMap.OnTimeStep(const dTimeInSeconds: Double; const dTimeFromStart: Integer);
    var
        i: Integer;
    begin
    // dTimeInSeconds = RoadのOnTimeStepで取得できる時間(秒)
    // dTimeFromStart = F8CrowdSimControllerClass.dTimeでスタートからの経過時間(秒)

    for i := 0 to CrossWalks.ListCount - 1 do
        CrossWalks.CrossWalk[i].UpdateSimSignal(dTimeFromStart);
    end;

function TPedestrianMap.GenerateCandidateWalkingPath: TParametricLineList;
    const
        PATH_INTERVAL_MINIMUM = 5.0; // m
        WALKABLE = PedestrianAreaStatus._pasWalkable;

    function ApplyHeightOffset(const aPoint: TPoint3D): TPoint3D;
        begin
        Result := aPoint;
        Result.Y := aPoint.Y + HeightOffset;
        end;
    var
        i: Integer;
        ri, ci: Integer;
        idx: Integer;
        tmpList: TList<TParametricLine2D>;
        newLine: TParametricLine2D;
    begin
    SetLength(Result, 0);
    p_CellListCS.Enter;
    tmpList := TList<TParametricLine2D>.Create;
    try
        for ri := 1 to p_Config.RowCount - 2 do
            begin
            for ci := 1 to p_Config.ColumnCount - 2 do
                begin
                idx := ri * p_Config.ColumnCount + ci;
                if p_CellList[idx].Status = WALKABLE then
                    begin
                    if (idx <> 1) and (p_CellList[idx - 1].Status = WALKABLE) then
                        begin
                        newLine.a := ApplyHeightOffset(p_CellList[idx].Center.OpenGL);
                        newLine.b := ApplyHeightOffset(p_CellList[idx - 1].Center.OpenGL);
                        tmpList.Add(newLine);
                        end;
                    if (idx <> p_Config.ColumnCount - 2) and (p_CellList[idx + 1].Status = WALKABLE) then
                        begin
                        newLine.a := ApplyHeightOffset(p_CellList[idx].Center.OpenGL);
                        newLine.b := ApplyHeightOffset(p_CellList[idx + 1].Center.OpenGL);
                        tmpList.Add(newLine);
                        end;
                    if (ri <> 1) and (p_CellList[idx - p_Config.ColumnCount].Status = WALKABLE) then
                        begin
                        newLine.a := ApplyHeightOffset(p_CellList[idx].Center.OpenGL);
                        newLine.b := ApplyHeightOffset(p_CellList[idx - p_Config.ColumnCount].Center.OpenGL);
                        tmpList.Add(newLine);
                        end;
                    if (ri <> p_Config.RowCount - 1) and (p_CellList[idx + p_Config.ColumnCount].Status = WALKABLE) then
                        begin
                        newLine.a := ApplyHeightOffset(p_CellList[idx].Center.OpenGL);
                        newLine.b := ApplyHeightOffset(p_CellList[idx + p_Config.ColumnCount].Center.OpenGL);
                        tmpList.Add(newLine);
                        end;
                    end;
                end;
            end;

        SetLength(Result, tmpList.Count);
        for i := 0 to tmpList.Count - 1 do
            Result[i] := tmpList[i];
    finally
        FreeAndNil(tmpList);
        p_CellListCS.Leave;
        end;
    end;

procedure TPedestrianMap.SetName(const aValue: String);
    begin
    if (aValue = '') or (aValue = p_MapName) then
        Exit;

    p_MapName := aValue;
    end;

procedure TPedestrianMap.SetHeightOffset(const aValue: Double);
    begin
    if p_HeightOffset <> aValue then
        begin
        p_HeightOffset := aValue;
        p_Renderer.HeightOffset := p_HeightOffset;
        end;
    end;

function TPedestrianMap.GetHeightOffset: Double;
    begin
    Result := p_HeightOffset;
    end;

procedure TPedestrianMap.SetCrossWalkIntervalSecond(const aValue: Integer);
    var
        i: Integer;
    begin
    if not InRange(aValue, CROSSWALK_INTERVAL_MIN, CROSSWALK_INTERVAL_MAX) then
        Exit;

    if p_CrossWalkIntervalSecond <> aValue then
        begin
        p_CrossWalkIntervalSecond := aValue;
        // 信号のインターバルも変更
        p_CrossWalks.SignalIntervalSecond := p_CrossWalkIntervalSecond;
        for i := 0 to p_CrossWalks.ListCount - 1 do
            begin
            p_CrossWalks.CrossWalk[i].SignalIntervalSecond := p_CrossWalkIntervalSecond;
            end;
        end;
    end;

function TPedestrianMap.ExtractRenderData: VertexDataArray;
    var
        i: Integer;
        renderData: TVertexData;
    begin
    SetLength(Result, p_CellList.Count);
    for i := 0 to p_CellList.Count - 1 do
        begin
        Assert(p_CellList[i].CellID.ID = i);
        renderData.X := p_CellList[i].AreaGL[VertexPositionType._LeftTop].X;
        renderData.Y := p_CellList[i].AreaGL[VertexPositionType._LeftTop].Y;
        renderData.Z := p_CellList[i].AreaGL[VertexPositionType._LeftTop].Z;

        case p_CellList[i].Status of
            PedestrianAreaStatus._pasNoEntry  : renderData.Status := 0;
            PedestrianAreaStatus._pasWalkable : renderData.Status := 1;
            end;
        Result[i] := renderData;
        end;
    end;

procedure TPedestrianMap.MakeCell(const aMapOrigin: TMatrixOrigin; const aStatusList: PedestrianAreaStatusList;
  out aRenderingData: VertexDataArray);
    var
        vertexs, vertexsLocal, vertexsGL: VertexPositions;

    procedure WritePointData(const aTarget: VertexPositionType; const aPositionData: TPointPositionData);
        begin
        vertexs[aTarget]      := aPositionData.LatLon;
        vertexsLocal[aTarget] := aPositionData.Local;
        vertexsGL[aTarget]    := aPositionData.OpenGL;
        end;

    // 複数マスで共有している頂点は初回のみ計算する。2回目以降は以前の計算結果を利用する
    // 左上座標: 最初の行の左端マスでは計算する 最初の行の左端マス以外では左隣のマスの右上 2行目以降では1行上の左下
    // 右上座標: 最初の行では常に計算する 2行目以降では1行上の右下
    // 左下座標: 左端マスでは計算する 左端マス以外では左隣のマスの右下
    // 右下座標: 常に計算する
    // 計算は左から右かつ上から下

    var
        idGenerator: TCellIDGenerator;
        newID: TCellID;
        idxRow, idxColumn, idxCell{, i, idx}: Integer;
        southOffset, eastOffset: Double;
        cellSize: Double;
        centerPosition: TPointPositionData;
        status: PedestrianAreaStatus;
        rowCount, ColumnCount: Integer;
        cell: TPedestrianCell;
        renderData: TVertexData;
    begin
    p_CellListCS.Enter;
    try
        p_CellList.Clear;
        SetLength(aRenderingData, p_Config.AllCellCount);

        cellSize    := Config.CellSize;
        rowCount    := Config.RowCount;
        columnCount := Config.ColumnCount;

        idGenerator := TCellIDGenerator.Create(Config.RowCount, Config.ColumnCount);
        try
            for idxRow := 0 to rowCount - 1 do
                begin
                southOffset := idxRow * cellSize;

                for idxColumn := 0 to columnCount - 1 do
                    begin
                    eastOffset  := idxColumn * cellSize;
                    idxCell := (idxRow * columnCount) + idxColumn;

                    // Top
                    if idxRow = 0 then
                        begin
                        // LeftTop
                        if idxColumn = 0 then
                            WritePointData(VertexPositionType._LeftTop, aMapOrigin.MoveTo(southOffset, eastOffset))
                        else
                            WritePointData(VertexPositionType._LeftTop, p_CellList[idxCell - 1].Corner[VertexPositionType._RightTop]);

                        // RightTop
                        WritePointData(VertexPositionType._RightTop, aMapOrigin.MoveTo(southOffset, eastOffset + cellSize));
                        end
                    else
                        begin
                        // 1列前
                        cell := p_CellList[idxCell - columnCount];
                        WritePointData(VertexPositionType._LeftTop, cell.Corner[VertexPositionType._LeftBottom]);
                        WritePointData(VertexPositionType._RightTop, cell.Corner[VertexPositionType._RightBottom]);
                        end;

                    // Bottom
                    if idxColumn = 0 then
                        WritePointData(VertexPositionType._LeftBottom, aMapOrigin.MoveTo(southOffset + cellSize, eastOffset))
                    else
                        WritePointData(VertexPositionType._LeftBottom, p_CellList[idxCell - 1].Corner[VertexPositionType._RightBottom]);

                    WritePointData(VertexPositionType._RightBottom, aMapOrigin.MoveTo(southOffset + cellSize, eastOffset + cellSize));

                    // Create Cell
                    if idGenerator.Generate(idxCell, newID) then
                        begin
                        // 最外周辺は必ず歩行可能
                        if (newID.RowIdx = 0)
                        or (newID.ColumnIdx = 0)
                        or (newID.RowIdx = Config.RowCount - 1)
                        or (newID.ColumnIdx = Config.ColumnCount - 1) then
                            status := PedestrianAreaStatus._pasWalkable
                        else
                            status := aStatusList[idxCell];

                        centerPosition := aMapOrigin.MoveTo(southOffset + cellSize / 2, eastOffset + cellSize / 2);

                        p_CellList.Add(TPedestrianCell.Create(newID, vertexs, vertexsLocal, vertexsGL, centerPosition, status));
                        // 描画用データの作成
                        renderData.X := p_CellList.Last.AreaGL[VertexPositionType._LeftTop].X;
                        renderData.Y := p_CellList.Last.AreaGL[VertexPositionType._LeftTop].Y;
                        renderData.Z := p_CellList.Last.AreaGL[VertexPositionType._LeftTop].Z;
                        case p_CellList.Last.Status of
                            PedestrianAreaStatus._pasNoEntry      : renderData.Status := 0;
                            PedestrianAreaStatus._pasWalkable     : renderData.Status := 1;
                            end;
                        aRenderingData[newID.ID] := renderData;
                        end;
                    end;
                end;
        finally
            FreeAndNil(idGenerator);
            end;
    finally
        p_CellListCS.Leave;
        end;
    end;

procedure TPedestrianMap.OnUpdateCrossWalkEvent(const aCell: TPedestrianCell);
    begin
    p_RendererChangeQueue.Enqueue(aCell.CellID.ID);
    end;

procedure TPedestrianMap.OnBeforeEndCrossingEditEvent;
    procedure FillInternalArea(const aCrossWalk: TPedestrianCrossWalk);
        var
            area: TPedestrianCellGroupMap;
            rowIdx, rowMin, columnIdx, idx: Integer;
            externalCell: TPedestrianCell;
            isFirst: Boolean;
            i, j, k: Integer;
            cellIDs: TList<Integer>;
        begin
        if aCrossWalk.CellCount = 0 then
            Exit;

        isFirst := True;
        externalCell := nil;
        area := TPedestrianCellGroupMap.Create;
        cellIDs := TList<Integer>.Create;
        try
            // 空洞があるか調査
            rowMin := aCrossWalk.TopRow - 1;
            for rowIdx := rowMin to aCrossWalk.BottomRow + 1 do
                begin
                for columnIdx := aCrossWalk.LeftColumn - 1 to aCrossWalk.RightColumn + 1 do
                    begin
                    idx := rowIdx * Config.ColumnCount + columnIdx;
                    // 最初のマスは必ず横断歩道ではない
                    if isFirst then
                        begin
                        externalCell := p_CellList[idx];
                        isFirst := False;
                        end;

                    if not aCrossWalk.ContainsCell(p_CellList[idx]) then
                        area.InputCell(rowIdx - rowMin, p_CellList[idx]);
                    end;
                end;
            area.MergeLines;

            // 空洞がある場合
            if area.GroupCount > 1 then
                begin
                for i := 0 to area.GroupAreaCount - 1 do
                    begin
                    for j := 0 to area.GroupAreaItem[i].Count - 1 do
                        begin
                        if Assigned(externalCell) and area.GroupAreaItem[i].Item[j].ContainsCell(externalCell) then
                            Continue;

                        cellIDs.Clear;
                        area.GroupAreaItem[i].Item[j].ExtractCellIDs(cellIDs);
                        for k in cellIDs do
                            p_CrossWalks.AddOrRemoveCell(area.GroupAreaItem[i].Item[j].CellItem[k]);
                        end;
                    end;
                end;
        finally
            FreeAndNil(area);
            FreeAndNil(cellIDs);
            end;
        end;
    begin
    FillInternalArea(p_CrossWalks.EditCrossWalk);
    end;

procedure TPedestrianMap.UpdateRenerer;
    var
        cell: TPedestrianCell;
        crossWalkIdx: Integer;
    begin
    while p_RendererChangeQueue.Count > 0 do
        begin
        cell := p_CellList[p_RendererChangeQueue.Dequeue];
        if p_CrossWalks.ContainsCell(cell, crossWalkIdx) then
            begin
            case p_CrossWalks.CrossWalk[crossWalkIdx].SimSignal of
                PedestrianAreaCrossingSignal._pacsCrossingGreen: p_Renderer.SetWalkable(cell.CellID.ID, 2);
                PedestrianAreaCrossingSignal._pacsCrossingRed  : p_Renderer.SetWalkable(cell.CellID.ID, 3);
                end;
            end
        else
            begin
            case cell.Status of
                PedestrianAreaStatus._pasNoEntry : p_Renderer.SetWalkable(cell.CellID.ID, 0);
                PedestrianAreaStatus._pasWalkable: p_Renderer.SetWalkable(cell.CellID.ID, 1);
                end;
            end;
        end;
    end;

function TPedestrianMap.FindAreaCornerCellID(const aHoriConv: IF8WrHorizontalCoordinateConvertor; const aArea: VertexPositions;
  out aCorner: CornerCellID): Boolean;
    function  ConvertLatLonToLocal(const aLatLon: TPoint3D): TPoint3D;
        var
            src, dst: F8PointType;
        begin
        src[_x] := aLatLon.Lon;
        src[_y] := aLatLon.Lat;
        aHoriConv.Convert(_hctSpecifiedCS, 6668, _hctLocal_XY, 0, src, dst);
        Result.Assign(dst[_x], aLatLon.Y, dst[_y]);
        end;

    function  IsLeftOfMesh(const aDiffLocalXZ: TPoint3D): Boolean;
        begin
        Result := (aDiffLocalXZ.X < 0);
        end;

    function  IsRightOfMesh(const aDiffLocalXZ: TPoint3D): Boolean;
        begin
        Result := (Config.ColumnSize < aDiffLocalXZ.X);
        end;

    function  IsAboveOfMesh(const aDiffLocalXZ: TPoint3D): Boolean;
        begin
        Result := (0 < aDiffLocalXZ.Z);
        end;

    function  IsBelowOfMesh(const aDiffLocalXZ: TPoint3D): Boolean;
        begin
        Result := (Config.RowSize < -aDiffLocalXZ.Z);
        end;

    function  GetRowIndex(const aDiffLocalXZ: TPoint3D): Integer;
        begin
        Result := Trunc( -aDiffLocalXZ.Z / Config.CellSize);
        end;

    function  GetColumnIndex(const aDiffLocalXZ: TPoint3D): Integer;
        begin
        Result := Trunc( aDiffLocalXZ.X / Config.CellSize);
        end;
    var
        status: PedestrianAreaStatus;
        cellIDLT, cellIDRT, cellIDRB, cellIDLB: TCellID;
        diffLT, diffRT, diffRB, diffLB: TPoint3D;
        findLT, findRT, findRB, findLB: Boolean;
    begin
    // 道路のBoundingAreaの頂点がメッシュのどのマスに位置するか判定
    // 結果はroadCornerに格納
    // 道路が部分的にメッシュと重なっている場合にも対応
    // メッシュと道路のBoundingAreaは回転していない四角形とする
    // Result True: メッシュと道路で重複している箇所がある False: メッシュ外
    // 最初はメッシュ外判定なし
    Result := False;
    findLT := RequireCellInfo(ConvertLatLonToLocal(aArea[VertexPositionType._LeftTop]), cellIDLT, status, diffLT);
     // (左上)道路とメッシュは全く重なっていない
    if IsRightOfMesh(diffLT) or IsBelowOfMesh(diffLT) then
        Exit;
    findRB := RequireCellInfo(ConvertLatLonToLocal(aArea[VertexPositionType._RightBottom]), cellIDRB, status, diffRB);
     // (右下)道路とメッシュは全く重なっていない
    if IsLeftOfMesh(diffRB) or IsAboveOfMesh(diffRB) then
        Exit;
    findRT := RequireCellInfo(ConvertLatLonToLocal(aArea[VertexPositionType._RightTop]), cellIDRT, status, diffRT);
    findLB := RequireCellInfo(ConvertLatLonToLocal(aArea[VertexPositionType._LeftBottom]), cellIDLB, status, diffLB);

    if not (findLT or findRT or findRB or findLB) then
        Exit;

    // cellIDをセット
    // 道路とメッシュの位置関係で場合分け
    // 3点がメッシュ内(4パターン)、対角の頂点のみがメッシュ内(2パターン)はありえないため除外
    if findLT and findRT and findRB and findLB then
        begin
        // メッシュ内
        aCorner[VertexPositionType._LeftTop] := cellIDLT;
        aCorner[VertexPositionType._RightTop] := cellIDRT;
        aCorner[VertexPositionType._LeftBottom] := cellIDLB;
        aCorner[VertexPositionType._RightBottom] := cellIDRB;
        end
    else if findLB and findRB then
        begin
        // 上
        aCorner[VertexPositionType._LeftTop] := p_CellList[GetColumnIndex(diffLT)].CellID;
        aCorner[VertexPositionType._RightTop]  := p_CellList[GetColumnIndex(diffRT)].CellID;
        aCorner[VertexPositionType._LeftBottom] := cellIDLB;
        aCorner[VertexPositionType._RightBottom] := cellIDRB;
        end
    else if findLT and findRT then
        begin
        // 下
        aCorner[VertexPositionType._LeftTop] := CellIDLT;
        aCorner[VertexPositionType._RightTop]  := CellIDRT;
        aCorner[VertexPositionType._LeftBottom] := p_CellList[(Config.RowCount - 1) * Config.ColumnCount + GetColumnIndex(diffLB)].CellID;
        aCorner[VertexPositionType._RightBottom] := p_CellList[(Config.RowCount - 1) * Config.ColumnCount + GetColumnIndex(diffRB)].CellID;
        end
    else if findRT and findRB then
        begin
        // 左
        aCorner[VertexPositionType._LeftTop] := p_CellList[GetRowIndex(diffLT) * Config.ColumnCount].CellID;
        aCorner[VertexPositionType._RightTop]  := CellIDRT;
        aCorner[VertexPositionType._LeftBottom] := p_CellList[GetRowIndex(diffLB) * Config.ColumnCount].CellID;
        aCorner[VertexPositionType._RightBottom] := cellIDRB;
        end
    else if findLT and findLB then
        begin
        // 右
        aCorner[VertexPositionType._LeftTop] := CellIDLT;
        aCorner[VertexPositionType._RightTop] := p_CellList[GetRowIndex(diffRT) * Config.ColumnCount + (Config.ColumnCount - 1)].CellID;
        aCorner[VertexPositionType._LeftBottom] := cellIDLB;
        aCorner[VertexPositionType._RightBottom] := p_CellList[GetRowIndex(diffRB) * Config.ColumnCount + (Config.ColumnCount - 1)].CellID;
        end
    else if findRB then
        begin
        // 左上
        aCorner[VertexPositionType._LeftTop] := p_CellList[0].CellID;
        aCorner[VertexPositionType._RightTop]  := p_CellList[GetColumnIndex(diffRT)].CellID;
        aCorner[VertexPositionType._LeftBottom] := p_CellList[GetRowIndex(diffLB) * Config.ColumnCount].CellID;
        aCorner[VertexPositionType._RightBottom] := cellIDRB;
        end
    else if findLB then
        begin
        // 右上
        aCorner[VertexPositionType._LeftTop] := p_CellList[GetColumnIndex(diffLT)].CellID;
        aCorner[VertexPositionType._RightTop]  := p_CellList[Config.ColumnCount - 1].CellID;
        aCorner[VertexPositionType._LeftBottom] := cellIDLB;
        aCorner[VertexPositionType._RightBottom] := p_CellList[GetRowIndex(diffRB) * Config.ColumnCount + (Config.ColumnCount - 1)].CellID;
        end
    else if findRT then
        begin
        // 左下
        aCorner[VertexPositionType._LeftTop] := p_CellList[GetRowIndex(diffLT) * Config.ColumnCount].CellID;
        aCorner[VertexPositionType._RightTop]  := CellIDRT;
        aCorner[VertexPositionType._LeftBottom] := p_CellList[(Config.RowCount - 1) * Config.ColumnCount].CellID;
        aCorner[VertexPositionType._RightBottom] := p_CellList[(Config.RowCount - 1) * Config.ColumnCount + GetColumnIndex(diffRB)].CellID;
        end
    else if findLT then
        begin
        // 右下
        aCorner[VertexPositionType._LeftTop] := CellIDLT;
        aCorner[VertexPositionType._RightTop]  := p_CellList[GetRowIndex(diffRT) * Config.ColumnCount + (Config.ColumnCount - 1)].CellID;
        aCorner[VertexPositionType._LeftBottom] := p_CellList[(Config.RowCount - 1) * Config.ColumnCount + GetColumnIndex(diffLB)].CellID;
        aCorner[VertexPositionType._RightBottom] := p_CellList[Config.AllCellCount - 1].CellID;
        end
    else
        begin
        // ありえない組み合わせ. 多分何かミスがある
        Assert(False);
        Exit;
        end;

    Result := True;
    end;

function TPedestrianMap.FindNextCell(const aCellID: TCellID; const aCorner: CornerCellID): Integer;
    begin
    // 道路のBoundingAreaと重なっている次のマスを検索する
    // 右端 or Not
    if aCellID.ColumnIdx = aCorner[VertexPositionType._RightBottom].ColumnIdx then
        begin
        // 最後
        if aCellID.ID = aCorner[VertexPositionType._RightBottom].ID then
            begin
            Result := aCellID.ID + 1;
            Exit;
            end;
        // 1行下の左下
        Result := (aCellID.RowIdx + 1) * Config.ColumnCount + (aCorner[VertexPositionType._LeftBottom].ColumnIdx - 1);
        end
    else
        Result := aCellID.ID + 1; // 横
    end;

//==============================================================================
function TPedestrianMap.NearbySearch(const Range: Integer; centerID: TCellID; out res: TPedestrianCell): Boolean;
    //==========================================================================
    function CheckCell(id : Integer) : Boolean;
        var
            cell : TPedestrianCell;
        begin
        Result := False;
        if RequireCellInfo(id,cell) then
            begin
            if (cell.Status= PedestrianAreaStatus._pasWalkable) then
                begin
                res := cell;
                Result := True;
                end;
            end;
        end;
    //==========================================================================
    var
        cell,above,right,left,below : TPedestrianCell;
        i,j : integer;
    begin
    Result := False;
    if not RequireCellInfo(centerID.ID,cell) then
        Exit;

    above := cell.Clone;
    right := cell.Clone;
    left  := cell.Clone;
    below := cell.Clone;


    for i := 1 to Range do//中心マスからI離れたセルから検索
        begin
        //check RightColumn
        if Assigned(right) and RequireCellInfo(right.cellID.RightID, cell) then//[i,0]
            begin
            right := cell;
            if CheckCell(right.cellID.ID) then
                Exit(True);
            cell := right;
            for j := 1 to i do
                begin
                if not RequireCellInfo(cell.cellID.aboveID,cell) then//[i,j]
                    begin
                    break;
                    end;
                if CheckCell(cell.cellID.ID) then
                    begin
                    Exit(True);
                    end;
                end;
            cell := right;
            for j := 1 to i do
                begin
                if not RequireCellInfo(cell.cellID.belowID,cell) then//[i,-j]
                    begin
                    break;
                    end;
                if CheckCell(cell.cellID.ID) then
                    begin
                    Exit(True);
                    end;
                end;
            end;

        //check LeftColumn
        if Assigned(left) and RequireCellInfo(left.cellID.LeftID, cell) then//[-i,0]
            begin
            left := cell;
            if CheckCell(left.cellID.ID) then
                Exit(True);
            cell := left;
            for j := 1 to i do
                begin
                if not RequireCellInfo(cell.cellID.aboveID,cell) then//[-i,j]
                    begin
                    break;
                    end;
                if CheckCell(cell.cellID.ID) then
                    begin
                    Exit(True);
                    end;
                end;
            cell := left;
            for j := 1 to i do
                begin
                if not RequireCellInfo(cell.cellID.belowID,cell) then//[-i,-j]
                    begin
                    break;
                    end;
                if CheckCell(cell.cellID.ID) then
                    begin
                    Exit(True);
                    end;
                end;
            end;


        //check aboveRow
        if Assigned(above) and RequireCellInfo(above.cellID.AboveID, cell) then//[0,i]
            begin
            above := cell;
            if CheckCell(above.cellID.ID) then
                Exit(True);

            cell := above;
            for j := 1 to i - 1 do//検索個所が1マス重複する分省略する
                begin
                if not RequireCellInfo(cell.cellID.rightID,cell) then//[j,i]
                    begin
                    break;
                    end;
                if CheckCell(cell.cellID.ID) then
                    begin
                    Exit(True);
                    end;
                end;
            cell := above;
            for j := 1 to i - 1 do//検索個所が1マス重複する分省略する
                begin
                if not RequireCellInfo(cell.cellID.leftID,cell) then//[-j,i]
                    begin
                    break;
                    end;
                if CheckCell(cell.cellID.ID) then
                    begin
                    Exit(True);
                    end;
                end;
            end;

        //check belowRow
        if Assigned(below) and RequireCellInfo(below.cellID.BelowID, cell) then//[0,-i]
            begin
            below := cell;
            if CheckCell(below.cellID.ID) then
                Exit(True);

            cell := below;
            for j := 1 to i - 1 do//検索個所が1マス重複する分省略する
                begin
                if not RequireCellInfo(cell.cellID.rightID,cell) then//[j,-i]
                    begin
                    break;
                    end;
                if CheckCell(cell.cellID.ID) then
                    begin
                    Exit(True);
                    end;
                end;
            cell := below;
            for j := 1 to i - 1 do//検索個所が1マス重複する分省略する
                begin
                if not RequireCellInfo(cell.cellID.leftID,cell) then//[-j,-i]
                    begin
                    break;
                    end;
                if CheckCell(cell.cellID.ID) then
                    begin
                    Exit(True);
                    end;
                end;
            end;
        end;
    end;

{ TNavMeshLinkPair }

procedure TNavMeshLinkPair.SetPair(const aA, aB: Integer);
    begin
    if aA <= aB then
        begin
        A := aA;
        B := aB;
        end
    else
        begin
        A := aB;
        B := aA;
        end;
    end;

{ TPedestrianMapPluginData }
class procedure TPedestrianMapPluginData.ExportPluginData(const aMap: TPedestrianMap; const aParentNode: IXMLNode);
    var
        headNode, crossWalkNode: IXMLNode;
    begin
    if not Assigned(aMap) then
        Exit;

    headNode := aParentNode.AddChild(NODE_NAME_MAP);
    headNode.Attributes[TAG_MAP_NAME] := aMap.MapName;
    headNode.Attributes[TAG_CROSSWALK_INTERVAL_SECOND] := aMap.CrosswalkIntervalSecond;
    headNode.Attributes[TAG_HEIGHT_OFFSET] := aMap.HeightOffset;
    ExportConfigData(aMap.Config, headNode);
    ExportAreaStatusList(aMap, headNode);
    crossWalkNode := headNode.AddChild(NODE_NAME_CROSSWALK_LIST);
    aMap.CrossWalks.ExportPluginData(crossWalkNode);
    end;

class procedure TPedestrianMapPluginData.ExportToXMLFile(const aMap: TPedestrianMap; const aDir: String);
    const
        FILE_NAME = 'pedestrianMap.xml';
    var
        xmlFile: IXMLDocument;
        node: IXMLNode;
    begin
    xmlFile := NewXMLDocument();
    node := xmlFile.AddChild('root');
    ExportPluginData(aMap, node);
    xmlFile.SaveToFile(aDir + '\' + FILE_NAME);
    end;

class function TPedestrianMapPluginData.ImportPluginData(const aAPI: IF8ApplicationServices; const aDataNode: IXMLNode; out aRes: TPedestrianMap): Boolean;
    var
        headNode, configNode, statusListNode, crossWalkNode: IXMLNode;
        tmpMapName: String;
        tmpCrosswalkIntervalSecond: Integer;
        tmpHeightOffset: Double;
        tmpConfig: TPedestrianMatrixConfig;
        tmpStatusList: PedestrianAreaStatusList;
        i: Integer;
    begin
    Result := False;

    headNode := aDataNode.ChildNodes.FindNode(NODE_NAME_MAP);
    if not Assigned(headNode) then
        Exit;

    if not headNode.HasAttribute(TAG_MAP_NAME) then
        Exit;

    tmpMapName := headNode.Attributes[TAG_MAP_NAME];

    if not headNode.HasAttribute(TAG_CROSSWALK_INTERVAL_SECOND) then
        Exit;

    tmpCrosswalkIntervalSecond := StrToIntDef(headNode.Attributes[TAG_CROSSWALK_INTERVAL_SECOND], CROSSWALK_INTERVAL_MIN);

    if not headNode.HasAttribute(TAG_HEIGHT_OFFSET) then
        Exit;

    tmpHeightOffset := StrToFloatDef(headNode.Attributes[TAG_HEIGHT_OFFSET], 0.0);

    configNode := headNode.ChildNodes.FindNode(NODE_NAME_CONFIG);

    if not Assigned(configNode) then
        Exit;

    if not ImportConfigdata(configNode, tmpConfig) then
        Exit;

    statusListNode := headNode.ChildNodes.FindNode(NODE_NAME_STATUS_LIST);

    if not Assigned(statusListNode) then
        Exit;

    if not ImportAreaStatusList(statusListNode, tmpConfig.AllCellCount, tmpStatusList) then
        Exit;

    aRes := TPedestrianMap.Create(aAPI.GetWRCoordinateConvertor.HoirizontalCSConvertor, tmpConfig, tmpStatusList);
    aRes.p_MapName := tmpMapName;
    aRes.p_CrossWalkIntervalSecond := tmpCrosswalkIntervalSecond;
    aRes.p_HeightOffset := tmpHeightOffset;

    // Import crossWalk
    crossWalkNode := headNode.ChildNodes.FindNode(NODE_NAME_CROSSWALK_LIST);
    if not Assigned(crossWalkNode) then
        Exit;

    if not aRes.CrossWalks.ImportPluginData(aRes.p_CellList, crossWalkNode) then
        Exit;

    aRes.CrossWalks.SignalIntervalSecond := tmpCrosswalkIntervalSecond;

    // 全てのセルを再描画
    for i := 0 to aRes.p_CellList.Count - 1 do
        aRes.p_RendererChangeQueue.Enqueue(i);

    Result := True;
    end;

class procedure TPedestrianMapPluginData.ExportConfigData(const aConfig: TPedestrianMatrixConfig; const aParentNode: IXMLNode);
    var
        node: IXMLNode;
    begin
    node := aParentNode.AddChild(NODE_NAME_CONFIG);
    node.SetAttribute(TAG_LEFT_TOP_LAT, aConfig.LeftTopLatLon.Lat.ToString);
    node.SetAttribute(TAG_LEFT_TOP_LON, aConfig.LeftTopLatLon.Lon.ToString);
    node.SetAttribute(TAG_CELL_SIZE,    aConfig.CellSize.ToString);
    node.SetAttribute(TAG_COLUMN_COUNT, aConfig.ColumnCount.ToString);
    node.SetAttribute(TAG_ROW_COUNT,    aConfig.RowCount.ToString);
    end;

class procedure TPedestrianMapPluginData.ExportAreaStatusList(const aMap: TPedestrianMap; const aParentNode: IXMLNode);
    function  StatusToInteger(const aCell: TPedestrianCell): Integer;
        begin
        case aCell.Status of
            PedestrianAreaStatus._pasNoEntry      : Result := 0;
            PedestrianAreaStatus._pasWalkable     : Result := 1;
            else
                begin
                Assert(False);
                Result := 0;
                end;
            end;
        end;
    var
        node: IXMLNode;
        txt: String;
        first: Integer;
        data: Byte;
        i: Integer;
    begin
    node := aParentNode.AddChild(NODE_NAME_STATUS_LIST);
    txt := '';
    first := aMap.Config.AllCellCount - 1;
    while first > 0 do
        begin
        data := 0;
        for i := 0 to 7 do
            begin
            if first - i > 0 then
                data := data or StatusToInteger(aMap.p_CellList[first - i]) shl (7 - i)
            else
                data := data or 0 shl (7 - i);
            end;
        txt := txt + data.ToHexString;
        first := first - 8;
        end;

    node.Text := txt;
    end;

class function TPedestrianMapPluginData.ImportConfigdata(const aDataNode: IXMLNode; out aConfig: TPedestrianMatrixConfig): Boolean;
    begin
    Result := False;
    if aDataNode.NodeName <> NODE_NAME_CONFIG then
        Exit;

    if aDataNode.HasAttribute(TAG_LEFT_TOP_LAT) then
        aConfig.LeftTopLatLon.Lat := StrToFloatDef(aDataNode.Attributes[TAG_LEFT_TOP_LAT], 0.0)
    else
        Exit;

    if aDataNode.HasAttribute(TAG_LEFT_TOP_LON) then
        aConfig.LeftTopLatLon.Lon := StrToFloatDef(aDataNode.Attributes[TAG_LEFT_TOP_LON], 0.0)
    else
        Exit;

    if aDataNode.HasAttribute(TAG_CELL_SIZE) then
        aConfig.CellSize := StrToFloatDef(aDataNode.Attributes[TAG_CELL_SIZE], 0.0)
    else
        Exit;

    if aDataNode.HasAttribute(TAG_COLUMN_COUNT) then
        aConfig.ColumnCount := StrToIntDef(aDataNode.Attributes[TAG_COLUMN_COUNT], 0)
    else
        Exit;

    if aDataNode.HasAttribute(TAG_ROW_COUNT) then
        aConfig.RowCount := StrToIntDef(aDataNode.Attributes[TAG_ROW_COUNT], 0)
    else
        Exit;

    Result := True;
    end;

class function TPedestrianMapPluginData.ImportAreaStatusList(const aDataNode: IXMLNode; const aListLength: Integer; out aStatusList: PedestrianAreaStatusList): Boolean;
    function  IntToStatus(const aValue: Integer): PedestrianAreaStatus;
        begin
        case aValue of
            VALUE_NO_ENTRY     : Result := PedestrianAreaStatus._pasNoEntry;
            VALUE_WALKABLE     : Result := PedestrianAreaStatus._pasWalkable;
            else
                Result := PedestrianAreaStatus._pasNoEntry;
            end;
        end;
    var
        text: String;
        i, j: Integer;
        str: String;
        data: Byte;
        param: Byte;
        idx: Integer;
    begin
    Result := False;
    if aDataNode.NodeName <> NODE_NAME_STATUS_LIST then
        Exit;

    text := aDataNode.Text;
    SetLength(aStatusList, aListLength);
    idx := aListLength - 1;
    for i := 1 to Length(text) - 1 do
        begin
        // 2文字ずつ処理
        if not Odd(i) then
            Continue;

        str := text[i] + text[i + 1];
        data := StrToIntDef('$' + str, 0);
        for j := 7 downto 0 do
            begin
            param := (data shr j) and 1;
            aStatusList[idx] := IntToStatus(param);
            Dec(idx);
            if idx < 0 then
                Break;
            end;

        if idx < 0 then
            Break;
        end;

    Result := True;
    end;

{ TPedestrianMapJSONData }
class procedure TPedestrianMapJSONData.ExportData(const aMap: TPedestrianMap; const aParentNode: TJSONObject);
    var
        root, map, crossWalk: TJSONObject;
    begin
    root := TJSONObject.Create;
    map := TJSONObject.Create;
    crossWalk := TJSONObject.Create;
    try
        map.AddPair(TAG_MAP_NAME, aMap.MapName);
        map.AddPair(TAG_CROSSWALK_INTERVAL_SECOND, aMap.CrossWalkIntervalSecond.ToString);
        map.AddPair(TAG_HEIGHT_OFFSET, aMap.HeightOffset.ToString);

        ExportConfigData(aMap.Config, map);
        ExportAreaStatusList(aMap, map);
        aMap.CrossWalks.ExportJSONData(crossWalk);

        map.AddPair(NODE_NAME_CROSSWALK_LIST, crossWalk.Clone as TJSONObject);
        root.AddPair(NODE_NAME_MAP, map.Clone as TJSONObject);
        aParentNode.AddPair('root', root.Clone as TJSONObject);
    finally
        FreeAndNil(root);
        FreeAndNil(map);
        FreeAndNil(crossWalk);
        end;
    end;

class procedure TPedestrianMapJSONData.ExportToJSONFile(const aMap: TPedestrianMap; const aDir: String);
    const
        FILE_NAME = 'pedestrianMap.json';
    var
        jsonObj: TJSONObject;
        strList: TStringList;
    begin
    jsonObj := TJSONObject.Create;
    strList := TStringList.Create;
    try
        ExportData(aMap, jsonObj);
        strList.Add(jsonObj.ToString);
        strList.SaveToFile(aDir + '\' + FILE_NAME, TEncoding.UTF8);
    finally
        FreeAndNil(jsonObj);
        FreeAndNil(strList);
        end;
    end;

class procedure TPedestrianMapJSONData.ExportConfigData(const aConfig: TPedestrianMatrixConfig; const aParentNode: TJSONObject);
    var
        config: TJSONObject;
    begin
    config := TJSONObject.Create;
    try
        config.AddPair(TAG_LEFT_TOP_LAT, aConfig.LeftTopLatLon.Lat.ToString);
        config.AddPair(TAG_LEFT_TOP_LON, aConfig.LeftTopLatLon.Lon.ToString);
        config.AddPair(TAG_CELL_SIZE, aConfig.CellSize.ToString);
        config.AddPair(TAG_COLUMN_COUNT, aConfig.ColumnCount.ToString);
        config.AddPair(TAG_ROW_COUNT, aConfig.RowCount.ToString);

        aParentNode.AddPair(NODE_NAME_CONFIG, config.Clone as TJSONObject);
    finally
        FreeAndNil(config);
        end;
    end;

class procedure TPedestrianMapJSONData.ExportAreaStatusList(const aMap: TPedestrianMap; const aParentNode: TJSONObject);
    function  StatusToInteger(const aCell: TPedestrianCell): Integer;
        begin
        case aCell.Status of
            PedestrianAreaStatus._pasNoEntry      : Result := 0;
            PedestrianAreaStatus._pasWalkable     : Result := 1;
            else
                begin
                Assert(False);
                Result := 0;
                end;
            end;
        end;
    var
        txt: String;
        first: Integer;
        data: Byte;
        i: Integer;
    begin
    txt := '';
    first := aMap.Config.AllCellCount - 1;
    while first > 0 do
        begin
        data := 0;
        for i := 0 to 7 do
            begin
            if first - i > 0 then
                data := data or StatusToInteger(aMap.p_CellList[first - i]) shl (7 - i)
            else
                data := data or 0 shl (7 - i);
            end;
        txt := txt + data.ToHexString;
        first := first - 8;
        end;

    aParentNode.AddPair(NODE_NAME_STATUS_LIST, txt);
    end;
    
{ TNavMeshBlock }
constructor TNavMeshBlock.Create(const aList: TObjectList<TPedestrianCell>);
    var
        i: Integer;
    begin
    p_List := TObjectList<TPedestrianCell>.Create(False);

    for i := 0 to aList.Count - 1 do
        begin
        p_List.Add(aList[i]);
        end;
    end;

procedure TNavMeshBlock.AfterConstruction;
    begin
    inherited;
    // ブロックの中心を調べる
    if Odd(p_List.Count) then
        p_RepresentativePoint := p_List[p_List.Count div 2].Center.Local
    else
        p_RepresentativePoint := p_List[(p_List.Count div 2) - 1].Center.Local;
    end;

procedure TNavMeshBlock.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_List);
    end;

function TNavMeshBlock.CheckRightSide(const aBlock: TNavMeshBlock): Boolean;
    begin
    Result := p_List.Last.CellID.RightID = aBlock.p_List.First.CellID.ID;
    end;

function TNavMeshBlock.CheckBelowSide(const aBlock: TNavMeshBlock): Boolean;
    var
        i, j: Integer;
    begin
    Result := False;
    for i := 0 to p_List.Count - 1 do
        begin
        for j := 0 to aBlock.p_List.Count - 1 do
            begin
            if p_List[i].CellID.BelowID = aBlock.p_List[j].CellID.ID then
                begin
                Result := True;
                Break;
                end;
            end;
        end;
    end;
end.

