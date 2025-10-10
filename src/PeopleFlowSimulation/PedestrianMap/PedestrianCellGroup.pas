unit PedestrianCellGroup;

interface

uses
    System.Generics.Collections,
    System.JSON,
    XML.XMLIntf,
    F8Utils,
    PedestrianCell,
    CellID,
    PedestrianUtil,
    MapRenderer,
    winapi.Windows;

type
    /// <summary>
    ///    地続きのTPedestrianCellの集合を表すクラス
    /// </summary>
    /// <remark>
    ///    このクラスはTPedestrianMapが所有するTPedestrianCellの部分集合を表す
    ///    このクラス自身が独自にTPedestrianCellを生成・管理することは想定していない
    /// </remark>
    TPedestrianCellGroup = class
        private
            const
                NODE_NAME_PEDESTRIAN_CELL_GROUP = 'PedestrianCellGroup';
                NODE_NAME_CELL_TABLE_LIST       = 'CellTableList';
                NODE_NAME_CELL_TABLE_ITEM       = 'CellTableItem_';
                TAG_NAME_TOP_ROW                = 'TopRow';
                TAG_NAME_BOTTOM_ROW             = 'BottomRow';
                TAG_NAME_LEFT_COLUMN            = 'LeftColumn';
                TAG_NAME_RIGHT_COLUMN           = 'RightColumn';
                TAG_NAME_CELL_ID                = 'CellID';
            var
                p_CellTable: TObjectDictionary<Integer, TPedestrianCell>;
                p_TopRow: Integer;
                p_BottomRow: Integer;
                p_LeftColumn: Integer;
                p_RightColumn: Integer;
                p_ConnectToIdx: integer;
                p_NearEdges   : TList<integer>;

            function  GetCellItem(const aCellID: Integer): TPedestrianCell;
            function  GetCellCount: Integer;
            procedure SetTopRow(const aValue: Integer);
            procedure SetBottomRow(const aValue: Integer);
            procedure SetLeftColumn(const aValue: Integer);
            procedure SetRightColumn(const aValue: Integer);
            procedure ExecuteUnification(const aGroup: TPedestrianCellGroup);
        protected
            function  HasConnectableCell(const aCell: TPedestrianCell): Boolean;
            function  HasOverlayGroup(const aGroup: TPedestrianCellGroup): Boolean;
            function  HasConnectableGroup(const aGroup: TPedestrianCellGroup): Boolean;

            procedure OnAddCellEvent(const aCell: TPedestrianCell); virtual;
            procedure OnRemoveCellEvent(const aCell: TPedestrianCell); virtual;

            function  SearchNearestWalkableCell(const aGroup: TPedestrianCellGroup; Config: TPedestrianMatrixConfig; Width: integer; Limit: double;
                var Edges: TSeparatedAreaEdges; CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;
            function  ConnectNearestWalkableCell(const Config: TPedestrianMatrixConfig; Width, ConnectIdx: integer; FromEdges: TList<integer>;
                var CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;
            function  ChangeBetweenCellsToWalkable(const Config: TPedestrianMatrixConfig; Width, ConnectIdx, EdgeIdx: integer; RowFarther: boolean;
                var RowDiff, ColDiff: integer; CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;

            property  _TopRow           : Integer read p_TopRow      write SetTopRow;
            property  _BottomRow        : Integer read p_BottomRow   write SetBottomRow;
            property  _LeftColumn       : Integer read p_LeftColumn  write SetLeftColumn;
            property  _RightColumn      : Integer read p_RightColumn write SetRightColumn;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function  AddCell(const aCell: TPedestrianCell): Boolean; virtual;
            function  RemoveCell(const aCell: TPedestrianCell): Boolean; virtual;
            procedure RemoveAll;
            function  ContainsCell(const aCell: TPedestrianCell): Boolean; virtual;
            function  UniteGroup(const aGroup: TPedestrianCellGroup): Boolean; virtual;
            procedure ExtractCellIDs(const aCellIDs: TList<Integer>);
            function  Clone: TPedestrianCellGroup;
            // PluginData入出力
            procedure ExportPluginData(const aParentNode: IXMLNode); virtual;
            function  ImportPluginData(const aList: TObjectList<TPedestrianCell>; const aParentNode: IXMLNode): Boolean; virtual;
            // JSON出力
            procedure ExportJSONData(const aParentNode: TJSONObject); virtual;

            procedure ExecuteRemoveCell(const CellIdx: integer);
            function  ConnectGroupFrom(const aGroup: TPedestrianCellGroup; Config: TPedestrianMatrixConfig;
                Width: integer; Limit: double; var CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;
            function  ReConnectGroup(const aGroup: TPedestrianCellGroup; Config: TPedestrianMatrixConfig; NearEdges: TList<integer>;
                ConnectToIdx, Width: integer; var CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;

            property  CellTable        : TObjectDictionary<Integer, TPedestrianCell> read p_CellTable;
            property  CellItem[const aCellID: Integer]: TPedestrianCell read GetCellItem;
            property  CellCount                       : Integer         read GetCellCount;
            property  TopRow                          : Integer         read p_TopRow;
            property  BottomRow                       : Integer         read p_BottomRow;
            property  LeftColumn                      : Integer         read p_LeftColumn;
            property  RightColumn                     : Integer         read p_RightColumn;
            property  ConnectToIdx                    : integer         read p_ConnectToIdx;
            property  NearEdges                       : TList<integer>  read p_NearEdges;
        end;

implementation

uses
    System.SysUtils,
    System.Generics.Defaults,
    System.Math,
    ParametricLine2DHelper,
    PedestrianMapUnificator;

{ TPedestrianCellGroup }
procedure TPedestrianCellGroup.AfterConstruction;
    begin
    inherited;

    // TPedestrianCellGroupではTPedestrianCellを所有しない
    p_CellTable := TObjectDictionary<Integer, TPedestrianCell>.Create([]);

    p_TopRow     := 0;
    p_BottomRow  := 0;
    p_LeftColumn := 0;
    p_RightColumn:= 0;

    p_ConnectToIdx := 0;
    p_NearEdges := TList<integer>.Create;
    end;

procedure TPedestrianCellGroup.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_CellTable);
    end;

function TPedestrianCellGroup.AddCell(const aCell: TPedestrianCell): Boolean;
    begin
    Result := False;

    if CellTable.Keys.Count = 0 then
        begin
        CellTable.Add(aCell.CellID.ID, aCell);
        Result := True;

        p_TopRow      := aCell.CellID.RowIdx;
        p_BottomRow   := aCell.CellID.RowIdx;
        p_LeftColumn  := aCell.CellID.ColumnIdx;
        p_RightColumn := aCell.CellID.ColumnIdx;

        OnAddCellEvent(aCell);
        Exit;
        end;

    if CellTable.ContainsKey(aCell.CellID.ID) then
        begin
        Result := True; // 追加は出来た(既に出来ている)
        Exit;
        end;

    if HasConnectableCell(aCell) then
        begin
        CellTable.Add(aCell.CellID.ID, aCell);
        Result := True;

        p_TopRow      := Min(p_TopRow, aCell.CellID.RowIdx);
        p_BottomRow   := Max(p_BottomRow, aCell.CellID.RowIdx);
        p_LeftColumn  := Min(p_LeftColumn, aCell.CellID.ColumnIdx);
        p_RightColumn := Max(p_RightColumn, aCell.CellID.ColumnIdx);

        OnAddCellEvent(aCell);
        Exit;
        end;
    end;

function TPedestrianCellGroup.RemoveCell(const aCell: TPedestrianCell): Boolean;
    var
        checkedList: TList<Integer>;

    procedure CheckCell(const aCell: TPedestrianCell);
        begin
        if checkedList.Contains(aCell.CellID.ID) then
            Exit;

        checkedList.Add(aCell.CellID.ID);

        if CellTable.ContainsKey(aCell.CellID.AboveID) then
            CheckCell(CellTable[aCell.CellID.AboveID]);
        if CellTable.ContainsKey(aCell.CellID.BelowID) then
            CheckCell(CellTable[aCell.CellID.BelowID]);
        if CellTable.ContainsKey(aCell.CellID.RightID) then
            CheckCell(CellTable[aCell.CellID.RightID]);
        if CellTable.ContainsKey(aCell.CellID.LeftID) then
            CheckCell(CellTable[aCell.CellID.LeftID]);
        end;
    begin
    if not CellTable.ContainsKey(aCell.CellID.ID) then
        begin
        Result := False;
        Exit;
        end;

    CellTable.Remove(aCell.CellID.ID);

    // グループが1つの領域になっているか判定
    // 適当なセルから全てのセルに到達できればOK
    // グループが1つの領域になっていない場合、削除は実行しない

    if p_CellTable.Keys.Count < 1 then
        begin
        Result := True;
        end
    else
        begin
        checkedList := TList<Integer>.Create;
        try
            CheckCell(CellTable[CellTable.Keys.ToArray[0]]);
            Result := (checkedList.Count = CellTable.Keys.Count);
        finally
            FreeAndNil(checkedList);
            end;
        end;

    if Result then
        OnRemoveCellEvent(aCell)
    else
        CellTable.Add(aCell.CellID.ID, aCell);
    end;

procedure TPedestrianCellGroup.RemoveAll;
    var
        key: Integer;
    begin
    for key in p_CellTable.Keys do
        OnRemoveCellEvent(p_CellTable[key]);

    p_CellTable.Clear;
    end;

function TPedestrianCellGroup.ContainsCell(const aCell: TPedestrianCell): Boolean;
    begin
    Result := Assigned(aCell) and p_CellTable.ContainsKey(aCell.CellID.ID);
    end;

function TPedestrianCellGroup.UniteGroup(const aGroup: TPedestrianCellGroup): Boolean;
    var
        key: Integer;
    begin
    Result := HasConnectableGroup(aGroup);

    if not Result then
        Exit;

    for key in aGroup.CellTable.Keys do
        begin
        if CellTable.ContainsKey(key) then
            Continue;

        CellTable.Add(key, aGroup.CellTable[key]);

        p_TopRow      := Min(p_TopRow,      CellTable[key].CellID.RowIdx);
        p_BottomRow   := Max(p_BottomRow,   CellTable[key].CellID.RowIdx);
        p_LeftColumn  := Min(p_LeftColumn,  CellTable[key].CellID.ColumnIdx);
        p_RightColumn := Max(p_RightColumn, CellTable[key].CellID.ColumnIdx);
        end;
    end;

procedure TPedestrianCellGroup.ExtractCellIDs(const aCellIDs: TList<Integer>);
    var
        key: Integer;
    begin
    aCellIDs.Clear;

    for key in p_CellTable.Keys do
        aCellIDs.Add(key);
    end;

function TPedestrianCellGroup.Clone: TPedestrianCellGroup;
    var
        key: Integer;
    begin
    Result := TPedestrianCellGroup.Create;
    for key in p_CellTable.Keys do
        Result.p_CellTable.Add(key, p_CellTable[key]);

    Result.p_TopRow      := p_TopRow;
    Result.p_BottomRow   := p_BottomRow;
    Result.p_LeftColumn  := p_LeftColumn;
    Result.p_RightColumn := p_RightColumn;
    end;

procedure TPedestrianCellGroup.ExportPluginData(const aParentNode: IXMLNode);
    var
        rootNode, ListNode, itemNode: IXMLNode;
        key, cnt: Integer;
    begin
    rootNode := aParentNode.AddChild(NODE_NAME_PEDESTRIAN_CELL_GROUP);
    rootNode.Attributes[TAG_NAME_TOP_ROW]      := p_TopRow.ToString;
    rootNode.Attributes[TAG_NAME_BOTTOM_ROW]   := p_BottomRow.ToString;
    rootNode.Attributes[TAG_NAME_LEFT_COLUMN]  := p_LeftColumn.ToString;
    rootNode.Attributes[TAG_NAME_RIGHT_COLUMN] := p_RightColumn.ToString;
    listNode := rootNode.AddChild(NODE_NAME_CELL_TABLE_LIST);
    cnt := 0;
    for key in p_CellTable.Keys do
        begin
        itemNode := listNode.AddChild(Format('%s%d', [NODE_NAME_CELL_TABLE_ITEM, cnt]));
        itemNode.Attributes[TAG_NAME_CELL_ID] := key;
        Inc(cnt);
        end;
    end;

function  TPedestrianCellGroup.ImportPluginData(const aList: TObjectList<TPedestrianCell>; const aParentNode: IXMLNode): Boolean;
    var
        rootNode, ListNode, itemNode: IXMLNode;
        i, key: Integer;
    begin
    Result := False;
    rootNode := aParentNode.ChildNodes.FindNode(NODE_NAME_PEDESTRIAN_CELL_GROUP);
    if not Assigned(rootNode) then
        Exit;

    if not rootNode.HasAttribute(TAG_NAME_TOP_ROW) then
        Exit;
    p_TopRow := StrToIntDef(rootNode.Attributes[TAG_NAME_TOP_ROW], 0);
    if not rootNode.HasAttribute(TAG_NAME_BOTTOM_ROW) then
        Exit;
    p_BottomRow := StrToIntDef(rootNode.Attributes[TAG_NAME_BOTTOM_ROW], 0);
    if not rootNode.HasAttribute(TAG_NAME_LEFT_COLUMN) then
        Exit;
    p_LeftColumn := StrToIntDef(rootNode.Attributes[TAG_NAME_LEFT_COLUMN], 0);
    if not rootNode.HasAttribute(TAG_NAME_RIGHT_COLUMN) then
        Exit;
    p_RightColumn := StrToIntDef(rootNode.Attributes[TAG_NAME_RIGHT_COLUMN], 0);

    listNode := rootNode.ChildNodes.FindNode(NODE_NAME_CELL_TABLE_LIST);
    if not Assigned(listNode) then
        Exit;

    for i := 0 to listNode.ChildNodes.Count - 1 do
        begin
        itemNode := listNode.ChildNodes.FindNode(Format('%s%d', [NODE_NAME_CELL_TABLE_ITEM, i]));
        if not Assigned(itemNode) then
            Continue;

        if not itemNode.HasAttribute(TAG_NAME_CELL_ID) then
            Continue;

        key := StrToIntDef(itemNode.Attributes[TAG_NAME_CELL_ID], -1);
        if not InRange(key, 0, aList.Count - 1) then
            Continue;

        Assert(key = aList[key].CellID.ID);
        p_CellTable.Add(key, aList[key]);
        end;

    Result := True;
    end;

procedure TPedestrianCellGroup.ExportJSONData(const aParentNode: TJSONObject);
    var
        root, list, item: TJSONObject;
        key, cnt: Integer;
    begin
    root := TJSONObject.Create;
    list := TJSONObject.Create;
    try
        root.AddPair(TAG_NAME_TOP_ROW, p_TopRow.ToString);
        root.AddPair(TAG_NAME_BOTTOM_ROW, p_BottomRow.ToString);
        root.AddPair(TAG_NAME_LEFT_COLUMN, p_LeftColumn.ToString);
        root.AddPair(TAG_NAME_RIGHT_COLUMN, p_RightColumn.ToString);
        cnt := 0;
        for key in p_CellTable.Keys do
            begin
            item := TJSONObject.Create;
            try
                item.AddPair(TAG_NAME_CELL_ID, key.ToString);
                list.AddPair(Format('%s%d', [NODE_NAME_CELL_TABLE_ITEM, cnt]), item.Clone as TJSONObject);
                Inc(cnt);
            finally
                FreeAndNil(item);
                end;
            end;

        root.AddPair(NODE_NAME_CELL_TABLE_LIST, list.Clone as TJSONObject);
        aParentNode.AddPair(NODE_NAME_PEDESTRIAN_CELL_GROUP, root.Clone as TJSONObject);
    finally
        FreeAndNil(root);
        FreeAndNil(list);
        end;
    end;

//==============================================================================
procedure TPedestrianCellGroup.ExecuteUnification(const aGroup: TPedestrianCellGroup);
    var
        key : Integer;
    begin
    for key in aGroup.CellTable.Keys do
        begin
        if CellTable.ContainsKey(key) then
            Continue;

        CellTable.Add(key, aGroup.CellTable[key]);

        p_TopRow      := Min(p_TopRow,      CellTable[key].CellID.RowIdx);
        p_BottomRow   := Max(p_BottomRow,   CellTable[key].CellID.RowIdx);
        p_LeftColumn  := Min(p_LeftColumn,  CellTable[key].CellID.ColumnIdx);
        p_RightColumn := Max(p_RightColumn, CellTable[key].CellID.ColumnIdx);
        end;
    end;

//==============================================================================
procedure TPedestrianCellGroup.ExecuteRemoveCell(const CellIdx: integer);
    begin
    if not CellTable.ContainsKey(CellIdx) then
        Exit;

    CellTable.Remove(CellIdx);
    end;

//==============================================================================
function TPedestrianCellGroup.ConnectGroupFrom(const aGroup: TPedestrianCellGroup; Config: TPedestrianMatrixConfig;
    Width: integer; Limit: double; var CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;

    //--------------------------------------------------------------------------
    procedure SetUpEdgesData(var EdgesData: TSeparatedAreaEdges);
        begin
        EdgesData.AboveEdges := TList<integer>.Create;
        EdgesData.BelowEdges := TList<integer>.Create;
        EdgesData.LeftEdges  := TList<integer>.Create;
        EdgesData.RightEdges := TList<integer>.Create;
        EdgesData.AboveLeft  := TList<integer>.Create;
        EdgesData.AboveRight := TList<integer>.Create;
        EdgesData.BelowLeft  := TList<integer>.Create;
        EdgesData.BelowRight := TList<integer>.Create;
        end;

    //--------------------------------------------------------------------------
    procedure CollectEdgeIndex(var EdgesData: TSeparatedAreaEdges);
        var
            key : Integer;
        begin
        for key in aGroup.CellTable.Keys do
            begin
            if not aGroup.CellTable.ContainsKey(aGroup.CellTable[key].CellID.AboveID) then
                begin
                EdgesData.AboveEdges.Add(CellList[key].CellID.ID);

                if not aGroup.CellTable.ContainsKey(aGroup.CellTable[key].CellID.LeftID)then
                    EdgesData.AboveLeft.Add(CellList[key].CellID.ID);

                if not aGroup.CellTable.ContainsKey(aGroup.CellTable[key].CellID.RightID)then
                    EdgesData.AboveRight.Add(CellList[key].CellID.ID);
                end;

            if not aGroup.CellTable.ContainsKey(aGroup.CellTable[key].CellID.BelowID) then
                begin
                EdgesData.BelowEdges.Add(CellList[key].CellID.ID);

                if not aGroup.CellTable.ContainsKey(aGroup.CellTable[key].CellID.LeftID)then
                    EdgesData.BelowLeft.Add(CellList[key].CellID.ID);

                if not aGroup.CellTable.ContainsKey(aGroup.CellTable[key].CellID.RightID)then
                    EdgesData.BelowRight.Add(CellList[key].CellID.ID);
                end;

            if not aGroup.CellTable.ContainsKey(aGroup.CellTable[key].CellID.LeftID) then
                EdgesData.LeftEdges.Add(CellList[key].CellID.ID);

            if not aGroup.CellTable.ContainsKey(aGroup.CellTable[key].CellID.RightID) then
                EdgesData.RightEdges.Add(CellList[key].CellID.ID);
            end;
        end;

    //--------------------------------------------------------------------------
    var
        SepAreaEdges : TSeparatedAreaEdges;
    begin
    SetUpEdgesData(SepAreaEdges);
    CollectEdgeIndex(SepAreaEdges);
    Result := SearchNearestWalkableCell(aGroup, Config, Width, Limit, SepAreaEdges, CellList, Renderer);
    if Result then
        ExecuteUnification(aGroup);
    end;

//==============================================================================
function TPedestrianCellGroup.SearchNearestWalkableCell(const aGroup: TPedestrianCellGroup; Config: TPedestrianMatrixConfig; Width: integer; Limit: double;
    var Edges: TSeparatedAreaEdges; CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;

    //--------------------------------------------------------------------------
    procedure SetUpAndCopyEdgesData(const SourceEdges: TSeparatedAreaEdges; var EdgesData: TArraySeparatedAreaEdges);
        begin
        SetLength(EdgesData.AboveEdges, SourceEdges.AboveEdges.Count);
        SetLength(EdgesData.BelowEdges, SourceEdges.BelowEdges.Count);
        SetLength(EdgesData.LeftEdges,  SourceEdges.LeftEdges.Count);
        SetLength(EdgesData.RightEdges, SourceEdges.RightEdges.Count);
        SetLength(EdgesData.AboveLeft,  SourceEdges.AboveLeft.Count);
        SetLength(EdgesData.AboveRight, SourceEdges.AboveRight.Count);
        SetLength(EdgesData.BelowLeft,  SourceEdges.BelowLeft.Count);
        SetLength(EdgesData.BelowRight, SourceEdges.BelowRight.Count);

        CopyMemory(EdgesData.AboveEdges, SourceEdges.AboveEdges.ToArray, SizeOf(integer) * SourceEdges.AboveEdges.Count);
        CopyMemory(EdgesData.BelowEdges, SourceEdges.BelowEdges.ToArray, SizeOf(integer) * SourceEdges.BelowEdges.Count);
        CopyMemory(EdgesData.LeftEdges,  SourceEdges.LeftEdges.ToArray,  SizeOf(integer) * SourceEdges.LeftEdges.Count);
        CopyMemory(EdgesData.RightEdges, SourceEdges.RightEdges.ToArray, SizeOf(integer) * SourceEdges.RightEdges.Count);
        CopyMemory(EdgesData.AboveLeft,  SourceEdges.AboveLeft.ToArray,  SizeOf(integer) * SourceEdges.AboveLeft.Count);
        CopyMemory(EdgesData.AboveRight, SourceEdges.AboveRight.ToArray, SizeOf(integer) * SourceEdges.AboveRight.Count);
        CopyMemory(EdgesData.BelowLeft,  SourceEdges.BelowLeft.ToArray,  SizeOf(integer) * SourceEdges.BelowLeft.Count);
        CopyMemory(EdgesData.BelowRight, SourceEdges.BelowRight.ToArray, SizeOf(integer) * SourceEdges.BelowRight.Count);
        end;

    //--------------------------------------------------------------------------
    procedure SetReturnData(out Edge: TList<integer>; Idx: integer);
        begin
        p_NearEdges    := Edge;
        p_ConnectToIdx := Idx;
        end;

    //--------------------------------------------------------------------------
    var
        Dist      : double;
        ConnectIdx: integer;
        NextEdges : TArraySeparatedAreaEdges;
    begin
    Dist       := 0;
    ConnectIdx := -1;
    Result     := false;
    SetUpAndCopyEdgesData(Edges, NextEdges);
    while Result = false do
        begin
        if Dist > Limit then //探索上限を超えた時点で失敗とする
            Break;

        if TPedestrianMapUnificator.SearchAbove(aGroup, NextEdges, CellList, ConnectIdx) then //上方向探索
            begin
            Result := ConnectNearestWalkableCell(Config, Width, ConnectIdx, Edges.AboveEdges, CellList, Renderer);
            SetReturnData(Edges.AboveEdges, ConnectIdx);
            Break;
            end;

        if TPedestrianMapUnificator.SearchBelow(aGroup, NextEdges, CellList, ConnectIdx) then //下方向探索
            begin
            Result := ConnectNearestWalkableCell(Config, Width, ConnectIdx, Edges.BelowEdges, CellList, Renderer);
            SetReturnData(Edges.BelowEdges, ConnectIdx);
            Break;
            end;

        if TPedestrianMapUnificator.SearchLeft(aGroup, NextEdges, CellList, ConnectIdx) then //左方向探索
            begin
            Result := ConnectNearestWalkableCell(Config, Width, ConnectIdx, Edges.LeftEdges, CellList, Renderer);
            SetReturnData(Edges.LeftEdges, ConnectIdx);
            Break;
            end;

        if TPedestrianMapUnificator.SearchRight(aGroup, NextEdges, CellList, ConnectIdx) then //右方向探索
            begin
            Result := ConnectNearestWalkableCell(Config, Width, ConnectIdx, Edges.RightEdges, CellList, Renderer);
            SetReturnData(Edges.RightEdges, ConnectIdx);
            Break;
            end;

        if TPedestrianMapUnificator.SearchAboveLeft(aGroup, NextEdges, CellList, ConnectIdx) then //左上方向探索
            begin
            Result := ConnectNearestWalkableCell(Config, Width, ConnectIdx, Edges.AboveLeft, CellList, Renderer);
            SetReturnData(Edges.AboveLeft, ConnectIdx);
            Break;
            end;

        if TPedestrianMapUnificator.SearchAboveRight(aGroup, NextEdges, CellList, ConnectIdx) then //右上方向探索
            begin
            Result := ConnectNearestWalkableCell(Config, Width, ConnectIdx, Edges.AboveRight, CellList, Renderer);
            SetReturnData(Edges.AboveRight, ConnectIdx);
            Break;
            end;

        if TPedestrianMapUnificator.SearchBelowLeft(aGroup, NextEdges, CellList, ConnectIdx) then //左下方向探索
            begin
            Result := ConnectNearestWalkableCell(Config, Width, ConnectIdx, Edges.BelowLeft, CellList, Renderer);
            SetReturnData(Edges.BelowLeft, ConnectIdx);
            Break;
            end;

        if TPedestrianMapUnificator.SearchBelowRight(aGroup, NextEdges, CellList, ConnectIdx) then //右下方向探索
            begin
            Result := ConnectNearestWalkableCell(Config, Width, ConnectIdx, Edges.BelowRight, CellList, Renderer);
            SetReturnData(Edges.BelowRight, ConnectIdx);
            Break;
            end;

        Dist := Dist + Config.CellSize;
        end;
    end;

//==============================================================================
function TPedestrianCellGroup.ConnectNearestWalkableCell(const Config: TPedestrianMatrixConfig; Width, ConnectIdx: integer; FromEdges: TList<integer>;
    var CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;
    var
        i           : integer;
        mindist     : double;
        mindistEIdx : integer;
        RowDiff     : integer;
        ColDiff     : integer;
    begin
    mindist     := 99999;
    mindistEIdx := -1;
    for i := 0 to FromEdges.Count - 1 do
        begin
        if mindist > PointDistance(CellList[ConnectIdx].AreaLocal[VertexPositionType._LeftTop], CellList[FromEdges[i]].AreaLocal[VertexPositionType._LeftTop]) then
            begin
            mindist     := PointDistance(CellList[ConnectIdx].AreaLocal[VertexPositionType._LeftTop], CellList[FromEdges[i]].AreaLocal[VertexPositionType._LeftTop]);
            mindistEIdx := FromEdges[i];
            end;
        end;

    RowDiff := Abs(CellList[ConnectIdx].CellID.RowIdx - CellList[mindistEIdx].CellID.RowIdx);
    ColDiff := Abs(CellList[ConnectIdx].CellID.ColumnIdx - CellList[mindistEIdx].CellID.ColumnIdx);
    if RowDiff >= ColDiff then
        Result := ChangeBetweenCellsToWalkable(Config, Width, ConnectIdx, mindistEIdx, true, RowDiff, ColDiff, CellList, Renderer)
    else
        Result := ChangeBetweenCellsToWalkable(Config, Width, ConnectIdx, mindistEIdx, false, RowDiff, ColDiff, CellList, Renderer);
    end;

//==============================================================================
function TPedestrianCellGroup.ChangeBetweenCellsToWalkable(const Config: TPedestrianMatrixConfig; Width, ConnectIdx, EdgeIdx: integer; RowFarther: boolean;
    var RowDiff, ColDiff: integer; CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;

    //--------------------------------------------------------------------------
    function ChangeABCellToWalkable(const ChangeIdx: integer): boolean;
        var
            i : integer;
        begin
        Result := false;
        for i := 0 to Width - 1 do
            begin
            if (i mod 2 = 0) then
                begin
                if CellList[ChangeIdx - (Trunc(i / 2) * Config.ColumnCount)].CellID.RowIdx <= 0 then
                    Continue;

                CellList[ChangeIdx - (Trunc(i / 2) * Config.ColumnCount)].Status := PedestrianAreaStatus._pasWalkable;
                Renderer.SetWalkable(ChangeIdx - (Trunc(i / 2) * Config.ColumnCount), 1);
                if AddCell(CellList[ChangeIdx - (Trunc(i / 2) * Config.ColumnCount)]) then
                    Result := true
                else
                    Exit;
                end
            else
                begin
                if CellList[ChangeIdx + (Trunc((i + 1) / 2) * Config.ColumnCount)].CellID.RowIdx > Config.RowCount - 1 then
                    Continue;

                CellList[ChangeIdx + (Trunc((i + 1) / 2) * Config.ColumnCount)].Status := PedestrianAreaStatus._pasWalkable;
                Renderer.SetWalkable(ChangeIdx + (Trunc((i + 1) / 2) * Config.ColumnCount), 1);
                if AddCell(CellList[ChangeIdx + (Trunc((i + 1) / 2) * Config.ColumnCount)]) then
                    Result := true
                else
                    Exit;
                end;
            end;
        end;

    //--------------------------------------------------------------------------
    function ChangeLRCellToWalkable(const ChangeIdx: integer): boolean;
        var
            i : integer;
        begin
        Result := false;
        for i := 0 to Width - 1 do
            begin
            if (i mod 2 = 0) then
                begin
                if CellList[ChangeIdx - Trunc(i / 2)].CellID.ColumnIdx <= 0 then
                    Continue;

                CellList[ChangeIdx - Trunc(i / 2)].Status := PedestrianAreaStatus._pasWalkable;
                Renderer.SetWalkable(ChangeIdx - Trunc(i / 2), 1);
                if AddCell(CellList[ChangeIdx - Trunc(i / 2)]) then
                    Result := true
                else
                    Exit;
                end
            else
                begin
                if CellList[ChangeIdx + Trunc((i + 1) / 2)].CellID.ColumnIdx > Config.ColumnCount - 1 then
                    Continue;

                CellList[ChangeIdx + Trunc((i + 1) / 2)].Status := PedestrianAreaStatus._pasWalkable;
                Renderer.SetWalkable(ChangeIdx + Trunc((i + 1) / 2), 1);
                if AddCell(CellList[ChangeIdx + Trunc((i + 1) / 2)]) then
                    Result := true
                else
                    Exit;
                end;
            end;
        end;

    //--------------------------------------------------------------------------
    function ConnectedRowToCol(var ChangeIdx: integer): boolean;
        begin
        Result := false;
        while RowDiff >= 0 do
            begin
            if CellList[ChangeIdx].CellID.RowIdx > CellList[EdgeIdx].CellID.RowIdx then
                ChangeIdx := CellList[ChangeIdx].CellID.AboveID
            else
                ChangeIdx := CellList[ChangeIdx].CellID.BelowID;

            if not ChangeLRCellToWalkable(ChangeIdx) then
                Exit;

            if ColDiff >= 0 then
                begin
                if CellList[ChangeIdx].CellID.ColumnIdx > CellList[EdgeIdx].CellID.ColumnIdx then
                    ChangeIdx := CellList[ChangeIdx].CellID.LeftID
                else
                    ChangeIdx := CellList[ChangeIdx].CellID.RightID;

                if not ChangeLRCellToWalkable(ChangeIdx) then
                    Exit;

                ColDiff := ColDiff - 1;
                end;

            RowDiff := RowDiff - 1;
            end;

        Result := true;
        end;

    //--------------------------------------------------------------------------
    function ConnectedColToRow(var ChangeIdx: integer): boolean;
        begin
        Result := false;
        while ColDiff >= 0 do
            begin
            if CellList[ChangeIdx].CellID.ColumnIdx > CellList[EdgeIdx].CellID.ColumnIdx then
                ChangeIdx := CellList[ChangeIdx].CellID.LeftID
            else
                ChangeIdx := CellList[ChangeIdx].CellID.RightID;

            if not ChangeABCellToWalkable(ChangeIdx) then
                Exit;

            if RowDiff >= 0 then
                begin
                if CellList[ChangeIdx].CellID.RowIdx > CellList[EdgeIdx].CellID.RowIdx then
                    ChangeIdx := CellList[ChangeIdx].CellID.AboveID
                else
                    ChangeIdx := CellList[ChangeIdx].CellID.BelowID;

                if not ChangeABCellToWalkable(ChangeIdx) then
                    Exit;

                RowDiff := RowDiff - 1;
                end;

            ColDiff := ColDiff - 1;
            end;

        Result := true;
        end;

    //--------------------------------------------------------------------------
    var
        ChangeIdx   : integer;
    begin
    Result := true;
    ChangeIdx := ConnectIdx;
    if RowFarther then
        begin
        if not ConnectedRowToCol(ChangeIdx) then
            begin
            Result := false;
            Exit;
            end;
        end
    else
        begin
        if not ConnectedColToRow(ChangeIdx) then
            begin
            Result := false;
            Exit;
            end;
        end;
    end;

//==============================================================================
function TPedestrianCellGroup.ReConnectGroup(const aGroup: TPedestrianCellGroup; Config: TPedestrianMatrixConfig; NearEdges: TList<integer>;
    ConnectToIdx, Width: integer; var CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;
    begin
    Result := false;
    if ConnectNearestWalkableCell(Config, Width, ConnectToIdx, NearEdges, CellList, Renderer) then
        begin
        Result := true;
        ExecuteUnification(aGroup);
        end;
    end;

function TPedestrianCellGroup.HasConnectableCell(const aCell: TPedestrianCell): Boolean;
    begin
    // True: リストに接続可能なセルがある False: ない
    Result := CellTable.ContainsKey(aCell.CellID.AboveID)
              or CellTable.ContainsKey(aCell.CellID.BelowID)
              or CellTable.ContainsKey(aCell.CellID.RightID)
              or CellTable.ContainsKey(aCell.CellID.LeftID);
    end;

function TPedestrianCellGroup.HasOverlayGroup(const aGroup: TPedestrianCellGroup): Boolean;
    var
        key: Integer;
    begin
    // True : このリストとaGroupは重複しているセルがある
    // False: 重複していない
    Result := False;

    if (CellTable.Keys.Count = 0) or (aGroup.CellTable.Keys.Count = 0) then
        Exit;

    // 地続きでないパターンは、以下のいずれかを満たす
    // 1. aGroupがこのリストより1行以上、上
    if (0 < _TopRow) and (aGroup._BottomRow < (_TopRow - 1)) then
        Exit;
    // 2. aGroupがこのリストより1行以上、下
    if (_BottomRow + 1) < aGroup._TopRow then
        Exit;
    // 3. aGroupがこのリストより1列以上、左
    if (0 < _LeftColumn) and (aGroup._RightColumn < (_LeftColumn - 1)) then
        Exit;

    // 4. aGroupがこのリストより1列以上、右
    if (_RightColumn + 1) < aGroup._LeftColumn then
        Exit;

    for key in aGroup.CellTable.Keys do
        begin
        if CellTable.ContainsKey(key) then
            begin
            Result := True;
            Break;
            end;
        end;
    end;

function TPedestrianCellGroup.HasConnectableGroup(const aGroup: TPedestrianCellGroup): Boolean;
    var
        key: Integer;
    begin
    // True : このリストとaGroupは地続き(両方が同じセルを含む、または接点となる隣同士のセルがある)
    // False: 地続きではない
    Result := False;

    if (CellTable.Keys.Count = 0) or (aGroup.CellTable.Keys.Count = 0) then
        Exit;

    // 地続きでないパターンは、以下のいずれかを満たす
    // 1. aGroupがこのリストより1行以上、上
    if (0 < _TopRow) and (aGroup._BottomRow < (_TopRow - 1)) then
        Exit;
    // 2. aGroupがこのリストより1行以上、下
    if (_BottomRow + 1) < aGroup._TopRow then
        Exit;
    // 3. aGroupがこのリストより1列以上、左
    if (0 < _LeftColumn) and (aGroup._RightColumn < (_LeftColumn - 1)) then
        Exit;

    // 4. aGroupがこのリストより1列以上、右
    if (_RightColumn + 1) < aGroup._LeftColumn then
        Exit;

    for key in aGroup.CellTable.Keys do
        begin
        if CellTable.ContainsKey(key) then
            begin
            Result := True;
            Break;
            end;

        if not InRange(aGroup.CellTable[key].CellID.RowIdx, _TopRow - 1, _BottomRow + 1) then
            Continue;

        if not InRange(aGroup.CellTable[key].CellID.ColumnIdx, _LeftColumn - 1, _RightColumn + 1) then
            Continue;

        if HasConnectableCell(aGroup.CellTable[key]) then
            begin
            Result := True;
            Break;
            end;
        end;
    end;

procedure TPedestrianCellGroup.OnAddCellEvent(const aCell: TPedestrianCell);
    begin
    {no action}
    end;

procedure TPedestrianCellGroup.OnRemoveCellEvent(const aCell: TPedestrianCell);
    begin
    {no action}
    end;

function TPedestrianCellGroup.GetCellItem(const aCellID: Integer): TPedestrianCell;
    begin
    if p_CellTable.ContainsKey(aCellID) then
        Result := p_CellTable[aCellID]
    else
        Result := nil;
    end;

function TPedestrianCellGroup.GetCellCount: Integer;
    begin
    Result := p_CellTable.Keys.Count;
    end;

procedure TPedestrianCellGroup.SetTopRow(const aValue: Integer);
    begin
    if aValue <> p_TopRow then
        p_TopRow := aValue;
    end;

procedure TPedestrianCellGroup.SetBottomRow(const aValue: Integer);
    begin
    if aValue <> p_BottomRow then
        p_BottomRow := aValue;
    end;

procedure TPedestrianCellGroup.SetLeftColumn(const aValue: Integer);
    begin
    if aValue <> p_LeftColumn then
        p_LeftColumn := aValue;
    end;

procedure TPedestrianCellGroup.SetRightColumn(const aValue: Integer);
    begin
    if aValue <> p_RightColumn then
        p_RightColumn := aValue;
    end;
end.

