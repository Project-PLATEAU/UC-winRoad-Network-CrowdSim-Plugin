unit PedestrianMapUnificator;

interface

uses
    System.Generics.Collections,
    PedestrianCell,
    PedestrianCellGroup_Area,
    MapRenderer,
    CellID,
    PedestrianUtil,
    PedestrianCellGroup;

type
    /// <summary>
    ///    シミュレーション計算時に孤立している歩行可能領域を自動接続するクラス
    /// </summary>
    TPedestrianMapUnificator = class
        private
            p_ConnectCellNum : integer;
            p_SearchLimit    : double;

            class function AssignedAbove(const aGroup: TPedestrianCellGroup; CellList: TObjectList<TPedestrianCell>; var Idx: integer): boolean;
            class function AssignedBelow(const aGroup: TPedestrianCellGroup; CellList: TObjectList<TPedestrianCell>; var Idx: integer): boolean;
            class function AssignedLeft(const aGroup: TPedestrianCellGroup; CellList: TObjectList<TPedestrianCell>; var Idx: integer): boolean;
            class function AssignedRight(const aGroup: TPedestrianCellGroup; CellList: TObjectList<TPedestrianCell>; var Idx: integer): boolean;
        public
            procedure AfterConstruction; override;
            function UnificateWalkableMap(const Config: TPedestrianMatrixConfig;
                var WalkableMap, UnWalkableMap: TPedestrianCellGroupAreaMap; CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;

            class function SearchAbove(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
                CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
            class function SearchBelow(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
                CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
            class function SearchLeft(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
                CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
            class function SearchRight(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
                CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
            class function SearchAboveLeft(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
                CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
            class function SearchAboveRight(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
                CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
            class function SearchBelowLeft(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
                CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
            class function SearchBelowRight(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
                CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
        end;

implementation

uses
    System.Math,
    AgentSettings;

const
    SEARCH_LIMIT_DISTANCE = 1000.0;

{ TPedestrianMapUnificator }
//==============================================================================
procedure TPedestrianMapUnificator.AfterConstruction;
    begin
    inherited;
    p_ConnectCellNum := 1;
    p_SearchLimit    := SEARCH_LIMIT_DISTANCE;
    end;

//==============================================================================
function TPedestrianMapUnificator.UnificateWalkableMap(const Config: TPedestrianMatrixConfig;
    var WalkableMap, UnWalkableMap: TPedestrianCellGroupAreaMap; CellList: TObjectList<TPedestrianCell>; Renderer: TMapRenderer): boolean;
    var
        AgentSetting : TAgentSettingsType;
        AgentRadius  : double;
    begin
    AgentSetting := LoadAgentSettings;
    if AgentSetting.IsRain then
        AgentRadius := AgentSetting.pedestrainRadiusInRainny
    else
        AgentRadius := AgentSetting.pedestrainRadiusInSunny;

    if Config.CellSize < (AgentRadius * 2) then
        p_ConnectCellNum := Ceil((AgentRadius * 3) / Config.CellSize);

    if (Config.CellSize * 5) > p_SearchLimit then
        p_SearchLimit := Config.CellSize * 5;

    Result := WalkableMap.ConnectWalkableMap(Config, p_ConnectCellNum, p_SearchLimit, CellList, Renderer, UnWalkableMap);
    end;

//==============================================================================
class function TPedestrianMapUnificator.AssignedAbove(const aGroup: TPedestrianCellGroup; CellList: TObjectList<TPedestrianCell>; var Idx: integer): boolean;
    begin
    Result := true;
    if Idx = -1 then
        Result := false
    else if aGroup.CellTable.ContainsKey(CellList[Idx].CellID.AboveID) then
        begin
        Idx := -1;
        Result := false;
        end;
    end;

//==============================================================================
class function TPedestrianMapUnificator.AssignedBelow(const aGroup: TPedestrianCellGroup; CellList: TObjectList<TPedestrianCell>; var Idx: integer): boolean;
    begin
    Result := true;
    if Idx = -1 then
        Result := false
    else if aGroup.CellTable.ContainsKey(CellList[Idx].CellID.BelowID) then
        begin
        Idx := -1;
        Result := false;
        end;
    end;

//==============================================================================
class function TPedestrianMapUnificator.AssignedLeft(const aGroup: TPedestrianCellGroup; CellList: TObjectList<TPedestrianCell>; var Idx: integer): boolean;
    begin
    Result := true;
    if Idx = -1 then
        Result := false
    else if aGroup.CellTable.ContainsKey(CellList[Idx].CellID.LeftID) then
        begin
        Idx := -1;
        Result := false;
        end;
    end;

//==============================================================================
class function TPedestrianMapUnificator.AssignedRight(const aGroup: TPedestrianCellGroup; CellList: TObjectList<TPedestrianCell>; var Idx: integer): boolean;
    begin
    Result := true;
    if Idx = -1 then
        Result := false
    else if aGroup.CellTable.ContainsKey(CellList[Idx].CellID.RightID) then
        begin
        Idx := -1;
        Result := false;
        end;
    end;

//==============================================================================
class function TPedestrianMapUnificator.SearchAbove(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
    CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
    var
        i   : integer;
        idx : integer;
    begin
    Result := false;
    for i := 0 to Length(Edges.AboveEdges) - 1 do
        begin
        idx := Edges.AboveEdges[i];
        if not AssignedAbove(aGroup, CellList, idx) then
            begin
            Edges.AboveEdges[i] := idx;
            Continue;
            end;

        if CellList[Edges.AboveEdges[i]].CellID.AboveID = -1 then
            begin
            ConnectIdx          := CellList[Edges.AboveEdges[i]].CellID.ID;
            Edges.AboveEdges[i] := ConnectIdx;
            Result              := true;
            Break;
            end;

        if (CellList[CellList[Edges.AboveEdges[i]].CellID.AboveID].Status = PedestrianAreaStatus._pasWalkable)
            and (aGroup.CellTable.ContainsKey(CellList[Edges.AboveEdges[i]].CellID.AboveID) = false) then
            begin
            ConnectIdx          := CellList[Edges.AboveEdges[i]].CellID.AboveID;
            Edges.AboveEdges[i] := ConnectIdx;
            Result              := true;
            Break;
            end
        else
            begin
            if CellList[Edges.AboveEdges[i]].CellID.AboveID = -1 then
                begin
                ConnectIdx          := CellList[Edges.AboveEdges[i]].CellID.ID;
                Edges.AboveEdges[i] := ConnectIdx;
                Result              := true;
                Break;
                end
            else
                Edges.AboveEdges[i] := CellList[Edges.AboveEdges[i]].CellID.AboveID;
            end;
        end;
    end;

//==============================================================================
class function TPedestrianMapUnificator.SearchBelow(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
    CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
    var
        i   : integer;
        idx : integer;
    begin
    Result := false;
    for i := 0 to Length(Edges.BelowEdges) - 1 do
        begin
        idx := Edges.BelowEdges[i];
        if not AssignedBelow(aGroup, CellList, idx) then
            begin
            Edges.BelowEdges[i] := idx;
            Continue;
            end;

        if (CellList[idx].CellID.RowIdx = 0) or (CellList[idx].CellID.ColumnIdx = 0) then
            begin
            ConnectIdx          := CellList[idx].CellID.ID;
            Edges.BelowEdges[i] := ConnectIdx;
            Result              := true;
            Break;
            end;

        if (CellList[CellList[Edges.BelowEdges[i]].CellID.BelowID].Status = PedestrianAreaStatus._pasWalkable)
            and (aGroup.CellTable.ContainsKey(CellList[Edges.BelowEdges[i]].CellID.BelowID) = false) then
            begin
            ConnectIdx          := CellList[Edges.BelowEdges[i]].CellID.BelowID;
            Edges.BelowEdges[i] := ConnectIdx;
            Result              := true;
            Break;
            end
        else
            begin
            if CellList[Edges.BelowEdges[i]].CellID.BelowID = -1 then
                begin
                ConnectIdx          := CellList[Edges.BelowEdges[i]].CellID.ID;
                Edges.BelowEdges[i] := ConnectIdx;
                Result              := true;
                Break;
                end
            else
                Edges.BelowEdges[i] := CellList[Edges.BelowEdges[i]].CellID.BelowID;
            end;
        end;
    end;

//==============================================================================
class function TPedestrianMapUnificator.SearchLeft(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
    CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
    var
        i   : integer;
        idx : integer;
    begin
    Result := false;
    for i := 0 to Length(Edges.LeftEdges) - 1 do
        begin
        idx := Edges.LeftEdges[i];
        if not AssignedLeft(aGroup, CellList, idx) then
            begin
            Edges.LeftEdges[i] := idx;
            Continue;
            end;

        if (CellList[idx].CellID.RowIdx = 0) or (CellList[idx].CellID.ColumnIdx = 0) then
            begin
            ConnectIdx         := CellList[idx].CellID.ID;
            Edges.LeftEdges[i] := ConnectIdx;
            Result             := true;
            Break;
            end;

        if (CellList[CellList[Edges.LeftEdges[i]].CellID.LeftID].Status = PedestrianAreaStatus._pasWalkable)
            and (aGroup.CellTable.ContainsKey(CellList[Edges.LeftEdges[i]].CellID.LeftID) = false) then
            begin
            ConnectIdx          := CellList[Edges.LeftEdges[i]].CellID.LeftID;
            Edges.LeftEdges[i]  := ConnectIdx;
            Result              := true;
            Break;
            end
        else
            begin
            if CellList[Edges.LeftEdges[i]].CellID.LeftID = -1 then
                begin
                ConnectIdx          := CellList[Edges.LeftEdges[i]].CellID.ID;
                Edges.LeftEdges[i]  := ConnectIdx;
                Result              := true;
                Break;
                end
            else
                Edges.LeftEdges[i] := CellList[Edges.LeftEdges[i]].CellID.LeftID;
            end;
        end;
    end;

//==============================================================================
class function TPedestrianMapUnificator.SearchRight(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
    CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
    var
        i   : integer;
        idx : integer;
    begin
    Result := false;
    for i := 0 to Length(Edges.RightEdges) - 1 do
        begin
        idx := Edges.RightEdges[i];
        if not AssignedRight(aGroup, CellList, idx) then
            begin
            Edges.RightEdges[i] := idx;
            Continue;
            end;

        if (CellList[idx].CellID.RowIdx = 0) or (CellList[idx].CellID.ColumnIdx = 0) then
            begin
            ConnectIdx          := CellList[idx].CellID.ID;
            Edges.RightEdges[i] := ConnectIdx;
            Result              := true;
            Break;
            end;

        if (CellList[CellList[Edges.RightEdges[i]].CellID.RightID].Status = PedestrianAreaStatus._pasWalkable)
            and (aGroup.CellTable.ContainsKey(CellList[Edges.RightEdges[i]].CellID.RightID) = false) then
            begin
            ConnectIdx          := CellList[Edges.RightEdges[i]].CellID.RightID;
            Edges.RightEdges[i] := ConnectIdx;
            Result              := true;
            Break;
            end
        else
            begin
            if CellList[Edges.RightEdges[i]].CellID.RightID = -1 then
                begin
                ConnectIdx          := CellList[Edges.RightEdges[i]].CellID.ID;
                Edges.RightEdges[i] := ConnectIdx;
                Result              := true;
                Break;
                end
            else
                Edges.RightEdges[i] := CellList[Edges.RightEdges[i]].CellID.RightID;
            end;
        end;
    end;

//==============================================================================
class function TPedestrianMapUnificator.SearchAboveLeft(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
    CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
    var
        i      : integer;
        idx    : integer;
    begin
    Result := false;
    for i := 0 to Length(Edges.AboveLeft) - 1 do
        begin
        idx := Edges.AboveLeft[i];
        if not AssignedLeft(aGroup, CellList, idx) then
            begin
            Edges.AboveLeft[i] := idx;
            Continue;
            end;

        if (CellList[idx].CellID.RowIdx = 0) or (CellList[idx].CellID.ColumnIdx = 0) then
            begin
            ConnectIdx         := CellList[idx].CellID.ID;
            Edges.AboveLeft[i] := ConnectIdx;
            Result             := true;
            Break;
            end;

        idx := CellList[idx].CellID.LeftID;
        if not AssignedAbove(aGroup, CellList, idx) then
            begin
            Edges.AboveLeft[i] := idx;
            Continue;
            end;

        if (CellList[CellList[idx].CellID.AboveID].Status = PedestrianAreaStatus._pasWalkable)
            and (aGroup.CellTable.ContainsKey(CellList[idx].CellID.AboveID) = false)
            and (CellList[idx].CellID.AboveID <> -1) then
            begin
            ConnectIdx         := CellList[idx].CellID.AboveID;
            Edges.AboveLeft[i] := ConnectIdx;
            Result             := true;
            Break;
            end
        else
            begin
            if CellList[idx].CellID.AboveID = -1 then
                begin
                ConnectIdx         := CellList[idx].CellID.ID;
                Edges.AboveLeft[i] := ConnectIdx;
                Result             := true;
                Break;
                end
            else
                Edges.AboveLeft[i] := CellList[idx].CellID.AboveID;
            end;
        end;
    end;

//==============================================================================
class function TPedestrianMapUnificator.SearchAboveRight(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
    CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
    var
        i      : integer;
        idx    : integer;
    begin
    Result := false;
    for i := 0 to Length(Edges.AboveRight) - 1 do
        begin
        idx := Edges.AboveRight[i];
        if not AssignedRight(aGroup, CellList, idx) then
            begin
            Edges.AboveRight[i] := idx;
            Continue;
            end;

        if (CellList[idx].CellID.RowIdx = 0) or (CellList[idx].CellID.ColumnIdx = 0) then
            begin
            ConnectIdx          := CellList[idx].CellID.ID;
            Edges.AboveRight[i] := ConnectIdx;
            Result              := true;
            Break;
            end;

        idx := CellList[idx].CellID.RightID;
        if not AssignedAbove(aGroup, CellList, idx) then
            begin
            Edges.AboveRight[i] := idx;
            Continue;
            end;

        if (CellList[CellList[idx].CellID.AboveID].Status = PedestrianAreaStatus._pasWalkable)
            and (aGroup.CellTable.ContainsKey(CellList[idx].CellID.AboveID) = false)
            and (CellList[idx].CellID.AboveID <> -1) then
            begin
            ConnectIdx          := CellList[idx].CellID.AboveID;
            Edges.AboveRight[i] := ConnectIdx;
            Result              := true;
            Break;
            end
        else
            begin
            if CellList[idx].CellID.AboveID = -1 then
                begin
                ConnectIdx          := CellList[idx].CellID.ID;
                Edges.AboveRight[i] := ConnectIdx;
                Result              := true;
                Break;
                end
            else
                Edges.AboveRight[i] := CellList[idx].CellID.AboveID;
            end;
        end;
    end;

//==============================================================================
class function TPedestrianMapUnificator.SearchBelowLeft(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
    CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
    var
        i      : integer;
        idx    : integer;
    begin
    Result := false;
    for i := 0 to Length(Edges.BelowLeft) - 1 do
        begin
        idx := Edges.BelowLeft[i];
        if not AssignedLeft(aGroup, CellList, idx) then
            begin
            Edges.BelowLeft[i] := idx;
            Continue;
            end;

        if (CellList[idx].CellID.RowIdx = 0) or (CellList[idx].CellID.ColumnIdx = 0) then
            begin
            ConnectIdx         := CellList[idx].CellID.ID;
            Edges.BelowLeft[i] := ConnectIdx;
            Result             := true;
            Break;
            end;

        idx := CellList[idx].CellID.LeftID;
        if not AssignedBelow(aGroup, CellList, idx) then
            begin
            Edges.BelowLeft[i] := idx;
            Continue;
            end;

        if (CellList[CellList[idx].CellID.BelowID].Status = PedestrianAreaStatus._pasWalkable)
            and (aGroup.CellTable.ContainsKey(CellList[idx].CellID.BelowID) = false)
            and (CellList[idx].CellID.BelowID <> -1) then
            begin
            ConnectIdx         := CellList[idx].CellID.BelowID;
            Edges.BelowLeft[i] := ConnectIdx;
            Result             := true;
            Break;
            end
        else
            begin
            if CellList[idx].CellID.BelowID = -1 then
                begin
                ConnectIdx         := CellList[idx].CellID.ID;
                Edges.BelowLeft[i] := ConnectIdx;
                Result             := true;
                Break;
                end
            else
                Edges.BelowLeft[i] := CellList[idx].CellID.BelowID;
            end;
        end;
    end;

//==============================================================================
class function TPedestrianMapUnificator.SearchBelowRight(const aGroup: TPedestrianCellGroup; var Edges: TArraySeparatedAreaEdges;
    CellList: TObjectList<TPedestrianCell>; out ConnectIdx: integer): boolean;
    var
        i      : integer;
        idx    : integer;
    begin
    Result := false;
    for i := 0 to Length(Edges.BelowRight) - 1 do
        begin
        idx := Edges.BelowRight[i];
        if not AssignedRight(aGroup, CellList, idx) then
            begin
            Edges.BelowRight[i] := idx;
            Continue;
            end;

        if (CellList[idx].CellID.RowIdx = 0) or (CellList[idx].CellID.ColumnIdx = 0) then
            begin
            ConnectIdx          := CellList[idx].CellID.ID;
            Edges.BelowRight[i] := ConnectIdx;
            Result              := true;
            Break;
            end;

        idx := CellList[idx].CellID.RightID;
        if not AssignedBelow(aGroup, CellList, idx) then
            begin
            Edges.BelowRight[i] := idx;
            Continue;
            end;

        if (CellList[CellList[idx].CellID.BelowID].Status = PedestrianAreaStatus._pasWalkable)
            and (aGroup.CellTable.ContainsKey(CellList[idx].CellID.BelowID) = false)
            and (CellList[idx].CellID.BelowID <> -1) then
            begin
            ConnectIdx          := CellList[idx].CellID.BelowID;
            Edges.BelowRight[i] := ConnectIdx;
            Result              := true;
            Break;
            end
        else
            begin
            if CellList[idx].CellID.BelowID = -1 then
                begin
                ConnectIdx          := CellList[idx].CellID.ID;
                Edges.BelowRight[i] := ConnectIdx;
                Result              := true;
                Break;
                end
            else
                Edges.BelowRight[i] := CellList[idx].CellID.BelowID;
            end;
        end;
    end;
end.
