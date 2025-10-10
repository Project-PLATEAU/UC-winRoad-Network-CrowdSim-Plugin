unit PedestrianCellGroup_CrossWalk;

interface

uses
    System.Generics.Collections,
    System.JSON,
    XML.XMLIntf,
    PedestrianCell,
    PedestrianCellGroup,
    CellID,
    PedestrianUtil;

type
    /// <summary>
    ///    横断歩道を表すクラス
    /// </summary>
    /// <remark>
    ///    横断歩道は複数定義可能である
    ///    横断歩道はそれぞれが地続きのTPedestrianCellで構成されている必要がある
    ///    横断歩道同士は地理的に排他である(=あるTPedestrianCellが2つ以上の横断歩道に所属していることはない)
    /// </remark>
    TPedestrianCrossWalk = class(TPedestrianCellGroup)
        strict private
            const
                NODE_NAME_CROSS_WALK = 'PedestrianCrossWalk';
                TAG_NAME_FIRST_SIGNAL = 'FirstSignal';
                TAG_NAME_SIGNAL_INTERVAL_SECOND = 'SignalIntervalSecond';
                TAG_NAME_CROSSWALK_NAME = 'CrosswalkName';
                VALUE_SIGNAL_GREEN    = 'SignalGreen';
                VALUE_SIGNAL_RED      = 'SignalRed';
            var
                p_FirstSignal : PedestrianAreaCrossingSignal;
                p_SimSignal   : PedestrianAreaCrossingSignal;
                p_Name        : String;
                p_SignalIntervalSecond: Integer;
                // 編集中フラグ Trueの場合編集中
                // True -> False時にグループをチェックする
                // 次の場合、グループとして不整合なので対策する
                // 1. グループのマスを上下左右につなげて領域とした場合に複数の領域に分かれる
                // 1.の対策 最初の追加以降、繋がるようにしか追加(削除)できない
                // 2. 内周辺がある
                // 2.の対策 内周マスをグループに追加する
                // 3. マスのステータスが_pasNoEntry
                // 3.の対策 _pasWalkableに変更する
                p_IsEditing: Boolean;
                p_OnChangeCrossWalkCell: ChangeCellNotifyProc;

            procedure SetFirstStatus(const aValue: PedestrianAreaCrossingSignal);
            procedure SetSignalIntervalSecond(const aValue: Integer);
            procedure SetIsEditing(const aValue: Boolean);
            procedure SetCrosswalkName(const aValue: String);
            function  GetCrosswalkName: String;
        private
            procedure BeginEditCrossWalk;
            procedure EndEditCrossWalk;
            procedure DeleteAllCell;

            function  GetCurrentSignal(const aElapsedTime: Integer): PedestrianAreaCrossingSignal;

            property  _IsEditing           : Boolean              read p_IsEditing             write SetIsEditing;
            property  OnChangeCrossWalkCell: ChangeCellNotifyProc read p_OnChangeCrossWalkCell write p_OnChangeCrossWalkCell;

        protected
            procedure OnAddCellEvent(const aCell: TPedestrianCell); override;
            procedure OnRemoveCellEvent(const aCell: TPedestrianCell); override;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function  Clone(const aClonedList: TObjectList<TPedestrianCell>): TPedestrianCrossWalk;

            procedure UpdateSimSignal(const aElapsedTime: Integer);
            procedure ResetSimSignal;

            // PluginData入出力
            procedure ExportPluginData(const aParentNode: IXMLNode); override;
            function  ImportPluginData(const aList: TObjectList<TPedestrianCell>; const aParentNode: IXMLNode): Boolean; override;

            // JSON出力
            procedure ExportJSONData(const aParentNode: TJSONObject); override;

            property  FirstSignal         : PedestrianAreaCrossingSignal read p_FirstSignal          write SetFirstStatus;
            property  SimSignal           : PedestrianAreaCrossingSignal read p_SimSignal;
            property  IsEditing           : Boolean                      read p_IsEditing;
            property  SignalIntervalSecond: Integer                      read p_SignalIntervalSecond write SetSignalIntervalSecond;
            property  CrosswalkName       : String                       read GetCrosswalkName       write SetCrosswalkName;
        end;

    UpdateCrossWalkProc = procedure(const aCrossWalk: TPedestrianCrossWalk) of Object;
    ChangeCellGroupCellProc = procedure(const aCell: TPedestrianCell) of Object;
    /// <summary>
    ///    TPedestrianCrossWalkのリスト
    /// </summary>
    TPedestrianCrossWalkList = class
        private
            const
                NODE_NAME_PEDESTRIAN_CROSSWALK_LIST = 'PedestrianCrossWalkList';
                NODE_NAME_CROSSWALK_LIST            = 'CrossWalkList';
                NODE_NAME_CROSSWALK_ITEM            = 'CrossWalkItem_';
            type
                CrosswalkEditingMode = (
                    cemAdd,
                    cemRemove);
            var
                p_List: TObjectList<TPedestrianCrossWalk>;
                p_EditingIdx: Integer;
                p_EditingMode: CrosswalkEditingMode;
                p_OnUpdateCrossWalkEvents: TList<TMethod>;
                p_SignalIntervalSecond   : Integer;
                p_OnBeforeChangeEditingEvent: ChangeCrossingEditingModeEventProc;

            function  GetCrossing(const aIdx: Integer): TPedestrianCrossWalk;
            function  GetListCount: Integer;
            function  GetEditCrossWalk: TPedestrianCrossWalk;
            function  GetEditingIdx: Integer;
            function  GetIsEditing: Boolean;
            procedure SetSignalIntervalSecond(const aValue: Integer);

            procedure DoOnChangeCrossWalkCellEvent(const aCell: TPedestrianCell);
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure RegisterOnUpdateCrossWalkEvent(const aEvent: ChangeCellNotifyProc);
            procedure UnRegisterOnUpdateCrossWalkEvent(const aEvent: ChangeCellNotifyProc);

            procedure AddNewCrossWalk;
            procedure RemoveCrossWalk(const aCrossWalk: TPedestrianCrossWalk);
            procedure DeleteCrossWalk(const aIdx: Integer);
            procedure BeginEditCrossWalk(const aIdx: Integer);
            procedure ChangeToAddCellMode;
            procedure ChangeToRemoveCellMode;
            function  AddOrRemoveCell(const aCell: TPedestrianCell): Boolean;
            procedure AddOrRemoveCells(const aCells: TObjectList<TPedestrianCell>);
            function  ContainsCell(const aCell: TPedestrianCell; out aCrossWalkIdx: Integer): Boolean;
            function  EndEditCrossWalk: Boolean;
            procedure AssignFrom(const aSrc: TPedestrianCrossWalkList; aList: TObjectList<TPedestrianCell>);

            procedure ExportPluginData(const aParentNode: IXMLNode);
            function  ImportPluginData(const aList: TObjectList<TPedestrianCell>; const aParentNode: IXMLNode): Boolean;

            procedure ExportJSONData(const aParentNode: TJSONObject);

            property OnBeforeChangeEditingEvent: ChangeCrossingEditingModeEventProc read p_OnBeforeChangeEditingEvent write p_OnBeforeChangeEditingEvent;
            property  CrossWalk[const aIdx: Integer]: TPedestrianCrossWalk read GetCrossing;
            property  ListCount                     : Integer              read GetListCount;
            property  EditCrossWalk                 : TPedestrianCrossWalk read GetEditCrossWalk;
            property  EditingIdx                    : Integer              read GetEditingIdx;
            property  SignalIntervalSecond          : Integer              read p_SignalIntervalSecond write SetSignalIntervalSecond;
            property  IsEditing                     : Boolean              read GetIsEditing;
        end;

implementation

uses
    System.SysUtils,
    System.Math,
    PedestrianCellGroup_Area;

{ TPedestrianCrossWalk }
procedure TPedestrianCrossWalk.AfterConstruction;
    begin
    inherited;

    p_FirstSignal := PedestrianAreaCrossingSignal._pacsCrossingGreen;
    p_IsEditing   := False;

    p_SignalIntervalSecond := CROSSWALK_INTERVAL_MIN;
    p_OnChangeCrossWalkCell := nil;
    end;

procedure TPedestrianCrossWalk.BeforeDestruction;
    begin
    inherited;
    {no action}
    end;

/// <summary>
/// クラスを複製する.プラグインデータの保存・読込時に使用する
/// </summary>
/// <remark>
///    プラグインデータの保存・読込以外で使用した場合、
///    1つのTPedestrianCellが複数の横断歩道に登録される状況が発生し、不具合の原因となる可能性がある
/// </remark>
function TPedestrianCrossWalk.Clone(const aClonedList: TObjectList<TPedestrianCell>): TPedestrianCrossWalk;
    var
        key: Integer;
    begin
    Result := TPedestrianCrossWalk.Create;
    Result.p_FirstSignal := FirstSignal;
    Result._TopRow        := _TopRow;
    Result._BottomRow     := _BottomRow;
    Result._LeftColumn    := _LeftColumn;
    Result._RightColumn   := _RightColumn;

    for key in CellTable.Keys do
        begin
        if not InRange(key, 0, aClonedList.Count - 1) then
            begin
            FreeAndNil(Result);
            Break;
            end;

        if not (key = aClonedList[key].CellID.ID) then
            begin
            FreeAndNil(Result);
            Break;
            end;

        Result.CellTable.Add(key, aClonedList[key]);
        end;
    end;

procedure TPedestrianCrossWalk.UpdateSimSignal(const aElapsedTime: Integer);
    var
        preSignal: PedestrianAreaCrossingSignal;
        cell: TPedestrianCell;
    begin
    preSignal := p_SimSignal;
    p_SimSignal := GetCurrentSignal(aElapsedTime);
    if preSignal <> p_SimSignal then
        begin
        if Assigned(p_OnChangeCrossWalkCell) then
            begin
            for cell in CellTable.Values do
                p_OnChangeCrossWalkCell(cell);
            end;
        end;
    end;

procedure TPedestrianCrossWalk.ResetSimSignal;
    var
        preSignal: PedestrianAreaCrossingSignal;
        cell: TPedestrianCell;
    begin
    preSignal := p_SimSignal;
    p_SimSignal := p_FirstSignal;
    if preSignal <> p_SimSignal then
        begin
        if Assigned(p_OnChangeCrossWalkCell) then
            begin
            for cell in CellTable.Values do
                p_OnChangeCrossWalkCell(cell);
            end;
        end;
    end;

procedure TPedestrianCrossWalk.ExportPluginData(const aParentNode: IXMLNode);
    var
        paramNode: IXMLNode;
    begin
    inherited;

    paramNode := aParentNode.AddChild(NODE_NAME_CROSS_WALK);
    case FirstSignal of
        PedestrianAreaCrossingSignal._pacsCrossingGreen: paramNode.Attributes[TAG_NAME_FIRST_SIGNAL] := VALUE_SIGNAL_GREEN;
        PedestrianAreaCrossingSignal._pacsCrossingRed  : paramNode.Attributes[TAG_NAME_FIRST_SIGNAL] := VALUE_SIGNAL_RED;
        end;
    paramNode.Attributes[TAG_NAME_SIGNAL_INTERVAL_SECOND] := p_SignalIntervalSecond.ToString;
    paramNode.Attributes[TAG_NAME_CROSSWALK_NAME] := p_Name;
    end;

procedure TPedestrianCrossWalk.ExportJSONData(const aParentNode: TJSONObject);
    var
        param: TJSONObject;
    begin
    inherited;

    param := TJSONObject.Create;
    try
        case FirstSignal of
            PedestrianAreaCrossingSignal._pacsCrossingGreen: param.AddPair(TAG_NAME_FIRST_SIGNAL, VALUE_SIGNAL_GREEN);
            PedestrianAreaCrossingSignal._pacsCrossingRed  : param.AddPair(TAG_NAME_FIRST_SIGNAL, VALUE_SIGNAL_RED);
            end;
        param.AddPair(TAG_NAME_SIGNAL_INTERVAL_SECOND, p_SignalIntervalSecond.ToString);

        aParentNode.AddPair(NODE_NAME_CROSS_WALK, param.Clone as TJSONObject);
    finally
        FreeAndNil(param);
        end;
    end;

function TPedestrianCrossWalk.ImportPluginData(const aList: TObjectList<TPedestrianCell>; const aParentNode: IXMLNode): Boolean;
    var
        paramNode: IXMLNode;
        firstSignalValue: String;
    begin
    Result := False;
    if not inherited then
        Exit;

    paramNode := aParentNode.ChildNodes.FindNode(NODE_NAME_CROSS_WALK);
    if not Assigned(paramNode) then
        Exit;

    if not paramNode.HasAttribute(TAG_NAME_FIRST_SIGNAL) then
        Exit;

    firstSignalValue := paramNode.Attributes[TAG_NAME_FIRST_SIGNAL];
    if firstSignalValue = VALUE_SIGNAL_GREEN then
        p_FirstSignal := PedestrianAreaCrossingSignal._pacsCrossingGreen
    else if firstSignalValue = VALUE_SIGNAL_RED then
        p_FirstSignal := PedestrianAreaCrossingSignal._pacsCrossingRed
    else
        p_FirstSignal := PedestrianAreaCrossingSignal._pacsCrossingGreen; // Default

    p_SimSignal := p_FirstSignal;

    if not paramNode.HasAttribute(TAG_NAME_SIGNAL_INTERVAL_SECOND) then
        Exit;

    p_SignalIntervalSecond := StrToIntDef(paramNode.Attributes[TAG_NAME_SIGNAL_INTERVAL_SECOND], CROSSWALK_INTERVAL_MIN);

    if not paramNode.HasAttribute(TAG_NAME_CROSSWALK_NAME) then
        Exit;

    p_Name := paramNode.Attributes[TAG_NAME_CROSSWALK_NAME];

    Result := True;
    end;

procedure TPedestrianCrossWalk.OnAddCellEvent(const aCell: TPedestrianCell);
    begin
    inherited;
    if Assigned(p_OnChangeCrossWalkCell) then
        p_OnChangeCrossWalkCell(aCell);
    end;

procedure TPedestrianCrossWalk.OnRemoveCellEvent(const aCell: TPedestrianCell);
    begin
    inherited;
    if Assigned(p_OnChangeCrossWalkCell) then
        p_OnChangeCrossWalkCell(aCell);
    end;

procedure TPedestrianCrossWalk.BeginEditCrossWalk;
    begin
    _IsEditing := True;
    end;

procedure TPedestrianCrossWalk.EndEditCrossWalk;
    begin
    _IsEditing := False;
    end;

procedure TPedestrianCrossWalk.DeleteAllCell;
    begin
    RemoveAll;
    end;

function TPedestrianCrossWalk.GetCurrentSignal(const aElapsedTime: Integer): PedestrianAreaCrossingSignal;
    var
        isDiffOfFirst: Boolean;
    begin
    isDiffOfFirst := Odd(aElapsedTime div SignalIntervalSecond);
    if isDiffOfFirst then
        begin
        case FirstSignal of
            PedestrianAreaCrossingSignal._pacsCrossingGreen: Result := PedestrianAreaCrossingSignal._pacsCrossingRed;
            PedestrianAreaCrossingSignal._pacsCrossingRed: Result := PedestrianAreaCrossingSignal._pacsCrossingGreen;
            else
                Result := PedestrianAreaCrossingSignal._pacsCrossingGreen;
            end;
        end
    else
        Result := FirstSignal;
    end;

procedure TPedestrianCrossWalk.SetFirstStatus(const aValue: PedestrianAreaCrossingSignal);
    var
        cell: TPedestrianCell;
    begin
    if p_FirstSignal <> aValue then
        begin
        p_FirstSignal := aValue;
        p_SimSignal := p_FirstSignal;
        if Assigned(p_OnChangeCrossWalkCell) then
            begin
            for cell in CellTable.Values do
                p_OnChangeCrossWalkCell(cell);
            end;
        end;
    end;

procedure TPedestrianCrossWalk.SetSignalIntervalSecond(const aValue: Integer);
    begin
    if not InRange(aValue, CROSSWALK_INTERVAL_MIN, CROSSWALK_INTERVAL_MAX) then
        Exit;

    if p_SignalIntervalSecond <> aValue then
        p_SignalIntervalSecond := aValue;
    end;

procedure TPedestrianCrossWalk.SetIsEditing(const aValue: Boolean);
    procedure FixNoEntryCell;
        var
            key: Integer;
        begin
        for key in CellTable.Keys do
            begin
            if CellTable[key].Status = PedestrianAreaStatus._pasNoEntry then
                CellTable[key].Status := PedestrianAreaStatus._pasWalkable;
            end;
        end;
    begin
    if p_IsEditing <> aValue then
        begin
        if aValue then // False -> True
            p_IsEditing := aValue
        else
            begin

            FixNoEntryCell;
            p_IsEditing := aValue;
            end;
        end;
    end;

procedure TPedestrianCrossWalk.SetCrosswalkName(const aValue: String);
    begin
    p_Name := aValue;
    end;

function TPedestrianCrossWalk.GetCrosswalkName: String;
    begin
    Result := p_Name;
    end;

{ TPedestrianCrossWalkList }
procedure TPedestrianCrossWalkList.AfterConstruction;
    begin
    inherited;

    p_List := TObjectList<TPedestrianCrossWalk>.Create;
    p_EditingIdx := -1;
    p_EditingMode := cemAdd;
    p_SignalIntervalSecond    := CROSSWALK_INTERVAL_MIN;
    p_OnUpdateCrossWalkEvents := TList<TMethod>.Create;
    end;

procedure TPedestrianCrossWalkList.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_List);
    FreeAndNil(p_OnUpdateCrossWalkEvents);
    end;

procedure TPedestrianCrossWalkList.RegisterOnUpdateCrossWalkEvent(const aEvent: ChangeCellNotifyProc);
    var
        m: TMethod;
    begin
    ChangeCellNotifyProc(m) := aEvent;
    if not p_OnUpdateCrossWalkEvents.Contains(m) then
        p_OnUpdateCrossWalkEvents.Add(m);
    end;

procedure TPedestrianCrossWalkList.UnRegisterOnUpdateCrossWalkEvent(const aEvent: ChangeCellNotifyProc);
    var
        m: TMethod;
    begin
    ChangeCellNotifyProc(m) := aEvent;
    p_OnUpdateCrossWalkEvents.Remove(m);
    end;

procedure TPedestrianCrossWalkList.AddNewCrossWalk;
    function  FindExistName(const aName: String): Boolean;
        var
            item: TPedestrianCrosswalk;
        begin
        Result := False;
        for item in p_List do
            begin
            if item.crossWalkName = aName then
                begin
                Result := True;
                Break;
                end;
            end;
        end;
    const
        BASE_NAME = 'Crosswalk_';
    var
        num: Integer;
        newName: String;
    begin
    p_List.Add(TPedestrianCrossWalk.Create);
    p_List.Last.OnChangeCrossWalkCell := DoOnChangeCrossWalkCellEvent;
    p_List.Last.SignalIntervalSecond := SignalIntervalSecond;

    num := 0;

    while FindExistName(newName) do
        begin
        Inc(num);
        newName := Format('%s%d', [BASE_NAME, num]);
        end;

    p_List.Last.CrosswalkName := newName;
    end;

procedure TPedestrianCrossWalkList.RemoveCrossWalk(const aCrossWalk: TPedestrianCrossWalk);
    var
        idx: Integer;
    begin
    if not Assigned(aCrossWalk) then
        Exit;

    idx := p_List.IndexOf(aCrossWalk);
    DeleteCrosswalk(idx);
    end;

procedure TPedestrianCrossWalkList.DeleteCrossWalk(const aIdx: Integer);
    begin
    if IsEditing then
        EndEditCrossWalk;

    if aIdx >= 0 then
        begin
        p_List[aIdx].DeleteAllCell;
        p_List[aIdx].OnChangeCrossWalkCell := nil;
        p_List.Delete(aIdx);
        p_EditingIdx := -1;
        end;
    end;

procedure TPedestrianCrossWalkList.BeginEditCrossWalk(const aIdx: Integer);
    begin
    Assert(InRange(aIdx, 0, p_List.Count - 1));
    if IsEditing then
        Exit;

    if aIdx < 0 then
        Exit;

    p_List[aIdx].BeginEditCrossWalk;
    p_EditingIdx := aIdx;
    end;

procedure TPedestrianCrossWalkList.ChangeToAddCellMode;
    begin
    p_EditingMode := cemAdd;
    end;

procedure TPedestrianCrossWalkList.ChangeToRemoveCellMode;
    begin
    p_EditingMode := cemRemove;
    end;

function TPedestrianCrossWalkList.AddOrRemoveCell(const aCell: TPedestrianCell): Boolean;
    function AddCell: Boolean;
        var
            i: Integer;
            find: Boolean;
        begin
        Result := False;
        if EditCrossWalk.ContainsCell(aCell) then
            Exit;

        find := False;
        for i := 0 to p_List.Count - 1 do
            begin
            if i = p_EditingIdx then
                Continue;

            if p_List[i].ContainsCell(aCell) then
                begin
                find := True;
                Break;
                end;
            end;

        if find then
            Exit;

        Result := EditCrossWalk.AddCell(aCell);
        end;

    function RemoveCell: Boolean;
        begin
        Result := False;
        if EditCrossWalk.ContainsCell(aCell) then
            Result := p_List[p_EditingIdx].RemoveCell(aCell);
        end;
    begin
    Result := False;
    if not IsEditing then
        Exit;

    case p_EditingMode of
        cemAdd   : Result := AddCell;
        cemRemove: Result := RemoveCell;
        end;
    end;

procedure TPedestrianCrossWalkList.AddOrRemoveCells(const aCells: TObjectList<TPedestrianCell>);
    var
        i: Integer;
        doneCellCount: Integer;
        changed: Boolean;
    begin
    if not (Assigned(aCells) and (aCells.Count > 0)) then
        Exit;

    doneCellCount := 0;
    while True do
        begin
        changed := False;
        for i := 0 to aCells.Count - 1 do
            begin
            if AddOrRemoveCell(aCells[i]) then
                begin
                Inc(doneCellCount);
                changed := True;
                end;
            end;

        if aCells.Count = doneCellCount then
            Break;

        if not changed then
            Break;
        end;
    end;

function TPedestrianCrossWalkList.ContainsCell(const aCell: TPedestrianCell; out aCrossWalkIdx: Integer): Boolean;
    var
        i: Integer;
    begin
    Result := False;
    for i := 0 to p_List.Count - 1 do
        begin
        if p_List[i].ContainsCell(aCell) then
            begin
            Result := True;
            aCrossWalkIdx := i;
            Break;
            end;
        end;
    end;

function TPedestrianCrossWalkList.EndEditCrossWalk: Boolean;
    begin
    Result := False;
    if not IsEditing then
        Exit;

    if Assigned(p_OnBeforeChangeEditingEvent) then
        p_OnBeforeChangeEditingEvent;

    p_List[p_EditingIdx].EndEditCrossWalk;
    Result := not p_List[p_EditingIdx].IsEditing;
    if Result then
        p_EditingIdx := -1;
    end;

procedure TPedestrianCrossWalkList.AssignFrom(const aSrc: TPedestrianCrossWalkList; aList: TObjectList<TPedestrianCell>);
    var
        i: Integer;
    begin
    p_EditingIdx := -1;
    p_List.Clear;

    for i := 0 to aSrc.p_List.Count - 1 do
        begin
        p_List.Add(aSrc.p_List[i].Clone(aList));
        end;
    end;

procedure TPedestrianCrossWalkList.ExportPluginData(const aParentNode: IXMLNode);
    var
        rootNode, listNode, itemNode: IXMLNode;
        i: Integer;
    begin
    rootNode := aParentNode.AddChild(NODE_NAME_PEDESTRIAN_CROSSWALK_LIST);

    listNode := rootNode.AddChild(NODE_NAME_CROSSWALK_LIST);
    for i := 0 to p_List.Count - 1 do
        begin
        itemNode := listNode.AddChild(Format('%s%d', [NODE_NAME_CROSSWALK_ITEM, i]));
        p_List[i].ExportPluginData(itemNode);
        end;
    end;

function TPedestrianCrossWalkList.ImportPluginData(const aList: TObjectList<TPedestrianCell>; const aParentNode: IXMLNode): Boolean;
    var
        rootNode, listNode, itemNode: IXMLNode;
        item: TPedestrianCrossWalk;
        i: Integer;
    begin
    Result := False;
    rootNode := aParentNode.ChildNodes.FindNode(NODE_NAME_PEDESTRIAN_CROSSWALK_LIST);
    if not Assigned(rootNode) then
        Exit;

    listNode := rootNode.ChildNodes.FindNode(NODE_NAME_CROSSWALK_LIST);
    if not Assigned(listNode) then
        Exit;

    for i := 0 to listNode.ChildNodes.Count - 1 do
        begin
        itemNode := listNode.ChildNodes.FindNode(Format('%s%d', [NODE_NAME_CROSSWALK_ITEM, i]));
        if not Assigned(itemNode) then
            Continue;

        item := TPedestrianCrossWalk.Create;
        item.ImportPluginData(aList, itemNode);
        item.OnChangeCrossWalkCell := DoOnChangeCrossWalkCellEvent;
        p_List.Add(item);
        end;
    Result := True;
    end;

procedure TPedestrianCrossWalkList.ExportJSONData(const aParentNode: TJSONObject);
    var
        root, list, item: TJSONObject;
        i: Integer;
    begin
    root := TJSONObject.Create;
    list := TJSONObject.Create;
    try
    for i := 0 to p_List.Count - 1 do
        begin
        item := TJSONObject.Create;
        try
            p_List[i].ExportJSONData(item);
            list.AddPair(Format('%s%d', [NODE_NAME_CROSSWALK_ITEM, i]), item.Clone as TJSONObject);
        finally
            FreeAndNil(item);
            end;
        end;

    root.AddPair(NODE_NAME_CROSSWALK_LIST, list.Clone as TJSONObject);
    aParentNode.AddPair(NODE_NAME_PEDESTRIAN_CROSSWALK_LIST, root.Clone as TJSONObject);
    finally
        FreeAndNil(root);
        FreeAndNil(list);
        end;
    end;

function TPedestrianCrossWalkList.GetCrossing(const aIdx: Integer): TPedestrianCrossWalk;
    begin
    Assert(InRange(aIdx, 0, p_List.Count - 1));
    Result := p_List[aIdx];
    end;

function TPedestrianCrossWalkList.GetListCount: Integer;
    begin
    Result := p_List.Count;
    end;

function TPedestrianCrossWalkList.GetEditCrossWalk: TPedestrianCrossWalk;
    begin
    if IsEditing then
        Result := p_List[p_EditingIdx]
    else
        Result := nil;
    end;

function TPedestrianCrossWalkList.GetEditingIdx: Integer;
    begin
    Result := p_EditingIdx;
    end;

function TPedestrianCrossWalkList.GetIsEditing: Boolean;
    begin
    Result := (p_EditingIdx >= 0);
    end;

procedure TPedestrianCrossWalkList.SetSignalIntervalSecond(const aValue: Integer);
    begin
    if p_SignalIntervalSecond <> aValue then
        p_SignalIntervalSecond := aValue;
    end;

procedure TPedestrianCrossWalkList.DoOnChangeCrossWalkCellEvent(const aCell: TPedestrianCell);
    var
        m: TMethod;
    begin
    for m in p_OnUpdateCrossWalkEvents do
        ChangeCellNotifyProc(m)(aCell);
    end;
end.

