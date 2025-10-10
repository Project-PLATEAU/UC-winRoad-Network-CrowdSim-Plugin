unit PedestrianCellGroup_Area;

interface

uses
    System.Generics.Collections,
    Xml.XMLIntf,
    PedestrianCellGroup,
    PedestrianCell,
    MapRenderer,
    PedestrianUtil,
    winapi.windows;

type
    /// <summary>
    ///    TPedestrianCellGroupのリスト
    /// </summary>
    TPedestrianCellGroupBase = class
        strict protected
            p_Groups: TObjectList<TPedestrianCellGroup>;

            function  GetItem(const aIdx: Integer): TPedestrianCellGroup;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function  GetCount: Integer;
            procedure InputCell(const aCell: TPedestrianCell); virtual;
            function  InputGroup(const aGroup: TPedestrianCellGroup): Boolean; virtual;
            procedure UniteLine(const aLine: TPedestrianCellGroupBase); virtual;
            procedure RemoveGroup(const aIdx: Integer); virtual;

            property  Item[const aIdx: Integer]: TPedestrianCellGroup read GetItem;
            property  Count                    : Integer              read GetCount;
        end;

    TPedestrianCellGroupArea = class(TPedestrianCellGroupBase)
        private
            p_TargetStatus: PedestrianAreaStatus;

            function  ConnectArea(const GroupIdx: integer; Config: TPedestrianMatrixConfig; Width: integer; Limit: double;
                var CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;
            function  ReConnectArea(const aGroup: TPedestrianCellGroup; Config: TPedestrianMatrixConfig; NearEdges: TList<integer>;
                ConnectToIdx, Width: integer; var CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;
        public
            constructor Create(const aTargetStatus: PedestrianAreaStatus);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure InputCell(const aCell: TPedestrianCell); override;

            procedure DeleteDuplicatedCells(const aArea: TPedestrianCellGroupArea);
            procedure AddOtherLineGroup(const OtherLine: TPedestrianCellGroupArea);
            procedure ClearGroup;
            function  ConnectGroup(const Config: TPedestrianMatrixConfig; Width: integer; Limit: double;
                var CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;

            property  Item[const aIdx: Integer]: TPedestrianCellGroup read GetItem;
            property  Count                    : Integer              read GetCount;
            property  TargetStatus             : PedestrianAreaStatus read p_TargetStatus;
        end;

    /// <summary>
    ///    TPedestrianCellGroupのリストのリスト
    /// </summary>
    TPedestrianCellGroupAreaMapBase<T: TPedestrianCellGroupBase> = class
        private
            p_Lines: TObjectList<T>;// Indexは行番号を表す

            function  GetGroupCount: Integer;
            function  GetGroupAreaCount: Integer;
            function  GetGroupAreaItem(const aIdx: Integer): T;
        protected
            function  GetNew: T; virtual; abstract;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure ClearList;
            procedure InputCell(const aRow: Integer; const aCell: TPedestrianCell); virtual;
            procedure MergeLines;

            property  GroupAreaItem[const aIdx: Integer] : T       read GetGroupAreaItem;
            property  GroupAreaCount                     : Integer read GetGroupAreaCount;
            property  GroupCount                         : Integer read GetGroupCount;
        end;

    TPedestrianCellGroupMap = class(TPedestrianCellGroupAreaMapBase<TPedestrianCellGroupBase>)
        protected
            function  GetNew: TPedestrianCellGroupBase; override;
        end;

    TPedestrianCellGroupAreaMap = class(TPedestrianCellGroupAreaMapBase<TPedestrianCellGroupArea>)
        private
            p_TargetStatus: PedestrianAreaStatus;
        protected
            function  GetNew: TPedestrianCellGroupArea; override;
        public
            constructor Create(const aTargetStatus: PedestrianAreaStatus);

            function  ConnectWalkableMap(const Config: TPedestrianMatrixConfig; Width: integer; Limit: double;
                var CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer; UnWalkableMap: TPedestrianCellGroupAreaMap): boolean;
            procedure DeleteDuplications(const aArea: TPedestrianCellGroupArea);

            property  TargetStatus: PedestrianAreaStatus read p_TargetStatus;
        end;

implementation

uses
    System.SysUtils;

{ TPedestrianCellGroupBase }

procedure TPedestrianCellGroupBase.AfterConstruction;
    begin
    inherited;

    p_Groups := TObjectList<TPedestrianCellGroup>.Create;
    end;

procedure TPedestrianCellGroupBase.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_Groups);
    end;

procedure TPedestrianCellGroupBase.InputCell(const aCell: TPedestrianCell);
    procedure CreateNewGroupAndAddCell;
        var
            idx: Integer;
        begin
        idx := p_Groups.Add(TPedestrianCellGroup.Create);
        p_Groups[idx].AddCell(aCell);
        end;
    var
        i: Integer;
        registered: Boolean;
    begin
    if p_Groups.Count = 0 then
        CreateNewGroupAndAddCell
    else
        begin
        registered := False;
        for i := 0 to p_Groups.Count - 1 do
            begin
            if p_Groups[i].AddCell(aCell) then
                begin
                registered := True;
                Break;
                end;
            end;

        if not registered then
            CreateNewGroupAndAddCell;
        end;
    end;

function TPedestrianCellGroupBase.InputGroup(const aGroup: TPedestrianCellGroup): Boolean;
    var
        i: Integer;
    begin
    Result := False;

    if p_Groups.Count = 0 then
        Exit;

    for i := 0 to p_Groups.Count - 1 do
        begin
        if p_Groups[i].UniteGroup(aGroup) then
            begin
            Result := True;
            Break;
            end;
        end;
    end;

procedure TPedestrianCellGroupBase.UniteLine(const aLine: TPedestrianCellGroupBase);
    var
        i, j: Integer;
        isChecked: Integer;
        isContinue: Boolean;
    begin
    if (aLine.Count = 0) then
        Exit;

    for i := aLine.p_Groups.Count - 1 downto 0 do
        begin
        if InputGroup(aLine.p_Groups[i]) then
            aLine.RemoveGroup(i)
        else
            begin
            p_Groups.Add(aLine.p_Groups[i].Clone);
            aLine.RemoveGroup(i);
            end;
        end;

    isChecked := 0;
    while True do
        begin
        if Count = 1 then
            Break;

        isContinue := False;
        for i := isChecked to p_Groups.Count - 2 do
            begin
            for j := i + 1 to p_Groups.Count - 1 do
                begin
                if p_Groups[i].UniteGroup(p_Groups[j]) then
                    begin
                    p_Groups.Delete(j);
                    isContinue := True;
                    Break;
                    end;
                end;

            if isContinue then
                Break
            else
                Inc(isChecked);
            end;

        if not isContinue then
            Break;
        end;
    end;

procedure TPedestrianCellGroupBase.RemoveGroup(const aIdx: Integer);
    begin
    p_Groups.Delete(aIdx);
    end;

function TPedestrianCellGroupBase.GetItem(const aIdx: Integer): TPedestrianCellGroup;
    begin
    Result := p_Groups[aIdx];
    end;

function TPedestrianCellGroupBase.GetCount: Integer;
    begin
    Result := p_Groups.Count;
    end;


{ TPedestrianCellGroupArea }
constructor TPedestrianCellGroupArea.Create(const aTargetStatus: PedestrianAreaStatus);
    begin
    p_TargetStatus := aTargetStatus;
    end;

procedure TPedestrianCellGroupArea.AfterConstruction;
    begin
    inherited;
    {no action}
    end;

procedure TPedestrianCellGroupArea.BeforeDestruction;
    begin
    inherited;
    {no action}
    end;

procedure TPedestrianCellGroupArea.InputCell(const aCell: TPedestrianCell);
    begin
    if aCell.Status <> TargetStatus then
        Exit;

    inherited;
    end;

//==============================================================================
procedure TPedestrianCellGroupArea.DeleteDuplicatedCells(const aArea: TPedestrianCellGroupArea);
    var
        key, i : integer;
    begin
    for key in aArea.p_Groups[0].CellTable.Keys do
        begin
        for i := 0 to p_Groups.Count - 1 do
            begin
            if p_Groups[i].CellTable.Keys.Count < 1 then
                Continue;

            if p_Groups[i].CellTable.ContainsKey(key) then
                begin
                p_Groups[i].ExecuteRemoveCell(key);
                Break;
                end;
            end;
        end;
    end;

//==============================================================================
procedure TPedestrianCellGroupArea.AddOtherLineGroup(const OtherLine: TPedestrianCellGroupArea);
    var
        i      : integer;
        tmp    : TPedestrianCellGroup;
        source : TPedestrianCellGroup;
    begin
    tmp    := TPedestrianCellGroup.Create;
    source := TPedestrianCellGroup.Create;
    for i := 0 to OtherLine.GetCount - 1 do
        begin
        source := OtherLine.p_Groups[i];
        Move(source, tmp, SizeOf(TPedestrianCellGroup));
        p_Groups.Add(tmp);
        end;
    end;

//==============================================================================
function TPedestrianCellGroupArea.ConnectGroup(const Config: TPedestrianMatrixConfig; Width: integer; Limit: double;
    var CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;
    var
        i         : integer;
        failed    : boolean;
    begin
    Result := false;
    failed := false;
    while p_Groups.Count > 1 do
        begin
        for i := 0 to p_Groups.Count - 2 do
            begin
            if (p_Groups[i].CellTable.keys.Count = 0) or (p_Groups[i + 1].CellTable.keys.Count = 0) then
                begin
                failed := true;
                Continue;
                end;

            if ConnectArea(i, Config, Width, Limit, CellList, Renderer) then
                begin
                failed := false;
                Break;
                end
            else
                failed := true;
            end;

        if failed then
            Exit;
        end;

    if p_Groups.Count = 1 then
        Result := true;
    end;

//==============================================================================
function TPedestrianCellGroupArea.ConnectArea(const GroupIdx: integer; Config: TPedestrianMatrixConfig; Width: integer; Limit: double;
    var CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;
    var
        connect : boolean;
    begin
    if not p_Groups[GroupIdx].CellTable.ContainsKey(0) then
        begin
        connect := p_Groups[GroupIdx + 1].ConnectGroupFrom(p_Groups[GroupIdx], Config, Width, Limit, CellList, Renderer);
        if connect then
            begin
            RemoveGroup(GroupIdx);
            Result := true;
            end
        else
            begin
            Result := ReConnectArea(p_Groups[GroupIdx], Config, p_Groups[GroupIdx + 1].NearEdges, p_Groups[GroupIdx + 1].ConnectToIdx, Width, CellList, Renderer);
            if Result = true then
                RemoveGroup(GroupIdx);
            end;
        end
    else
        begin
        connect := p_Groups[GroupIdx].ConnectGroupFrom(p_Groups[GroupIdx + 1], Config, Width, Limit, CellList, Renderer);
        if connect then
            begin
            RemoveGroup(GroupIdx + 1);
            Result := true;
            end
        else
            begin
            Result := ReConnectArea(p_Groups[GroupIdx + 1], Config, p_Groups[GroupIdx].NearEdges, p_Groups[GroupIdx].ConnectToIdx, Width, CellList, Renderer);
            if Result = true then
                RemoveGroup(GroupIdx + 1);
            end;
        end;
    end;

//==============================================================================
function TPedestrianCellGroupArea.ReConnectArea(const aGroup: TPedestrianCellGroup; Config: TPedestrianMatrixConfig; NearEdges: TList<integer>;
    ConnectToIdx, Width: integer; var CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;
    var
        i : integer;
    begin
    Result := false;
    for i := 0 to p_Groups.Count - 1 do
        begin
        if not p_Groups[i].CellTable.ContainsKey(ConnectToIdx) then
            Continue;

        if p_Groups[i].ReConnectGroup(aGroup, Config, NearEdges, ConnectToIdx, Width, CellList, Renderer) then
            begin
            Result := true;
            Break;
            end;
        end;
    end;

procedure TPedestrianCellGroupArea.ClearGroup;
    begin
    if Assigned(p_Groups) then
        p_Groups.Clear;
    end;

{ TPedestrianCellGroupAreaMapBase }
procedure TPedestrianCellGroupAreaMapBase<T>.AfterConstruction;
    begin
    inherited;

    p_Lines := TObjectList<T>.Create;
    end;

procedure TPedestrianCellGroupAreaMapBase<T>.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_Lines);
    end;

procedure TPedestrianCellGroupAreaMapBase<T>.ClearList;
    begin
    p_Lines.Clear;
    end;

procedure TPedestrianCellGroupAreaMapBase<T>.InputCell(const aRow: Integer; const aCell: TPedestrianCell);
    var
        newLine: TPedestrianCellGroupBase;
    begin
    if aRow < 0 then
        Exit;

    if p_Lines.Count < aRow + 1 then
        begin
        while p_Lines.Count < aRow + 1 do
            p_Lines.Add(GetNew);
        end;

    p_Lines[aRow].InputCell(aCell);
    end;

procedure TPedestrianCellGroupAreaMapBase<T>.MergeLines;
    var
        i: Integer;

    begin
    if p_Lines.Count < 2 then
        Exit;

    for i := 1 to p_Lines.Count - 1 do
        p_Lines[0].UniteLine(p_Lines[i]);
    end;

function TPedestrianCellGroupAreaMapBase<T>.GetGroupCount: Integer;
    var
        l: TPedestrianCellGroupBase;
    begin
    Result := 0;
    for l in p_Lines do
        Result := Result + l.Count;
    end;

function TPedestrianCellGroupAreaMapBase<T>.GetGroupAreaCount: Integer;
    begin
    Result := p_Lines.Count;
    end;

function TPedestrianCellGroupAreaMapBase<T>.GetGroupAreaItem(const aIdx: Integer): T;
    begin
    Result := p_Lines[aIdx];
    end;

constructor TPedestrianCellGroupAreaMap.Create(const aTargetStatus: PedestrianAreaStatus);
    begin
    p_TargetStatus := aTargetStatus;
    end;

//==============================================================================
procedure TPedestrianCellGroupAreaMap.DeleteDuplications(const aArea: TPedestrianCellGroupArea);
    var
        l : TPedestrianCellGroupArea;
    begin
    for l in p_lines do
        begin
        if l.GetCount > 0 then
            l.DeleteDuplicatedCells(aArea);
        end;
    end;

//==============================================================================
function TPedestrianCellGroupAreaMap.ConnectWalkableMap(const Config: TPedestrianMatrixConfig; Width: integer; Limit: double;
    var CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer; UnWalkableMap: TPedestrianCellGroupAreaMap): boolean;
    var
        l, AllGroups : TPedestrianCellGroupArea;
    begin
    AllGroups := TPedestrianCellGroupArea.Create(TargetStatus);
    Result := false;
    for l in p_Lines do
        begin
        if l.GetCount > 0 then
            AllGroups.AddOtherLineGroup(l);
        end;

    if AllGroups.ConnectGroup(Config, Width, Limit, CellList, Renderer) then
        begin
        Result := true;
        UnWalkableMap.DeleteDuplications(AllGroups);
        end;
    end;

function TPedestrianCellGroupAreaMap.GetNew: TPedestrianCellGroupArea;
    begin
    Result := TPedestrianCellGroupArea.Create(TargetStatus);
    end;

{ TPedestrianCellGroupMap }
function TPedestrianCellGroupMap.GetNew: TPedestrianCellGroupBase;
    begin
    Result := TPedestrianCellGroupBase.Create;
    end;
end.
