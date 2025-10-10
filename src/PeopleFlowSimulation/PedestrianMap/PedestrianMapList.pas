unit PedestrianMapList;

interface

uses
    System.Generics.Collections,
    System.Classes,
    XML.XMLIntf,
    PluginCore,
    F8OpenGL,
    F8GLUtils,
    F8Utils,
    TranRoadList,
    CrowdSimCityFurnitureList,
    PedestrianMap,
    PedestrianMapGenerator,
    PedestrianMapPlaneRenderer,
    PedestrianUtil;

type
    OnChangeActiveMapIndexNotify = procedure(const aActiveMap: TPedestrianMap) of Object;
    OnBeforeRenderingNotify = function(out aVertexs: VertexPositions): Boolean of Object;

    /// <summary>
    ///    PedestrianMapのリスト.
    ///    PedestrianMapデータをプラグインデータに保存/読込する.また、新規PedestrianMapデータを作成する.
    /// </summary>
    TPedestrianMapList = class
        private
            p_API: IF8ApplicationServices;

            p_List: TObjectList<TPedestrianMap>;

            p_RoadList: TTranRoadList;
            p_CityFurnitureList: TCrowdSimCityFurnitureList;
            p_Generator: TPedestrianMapGenerator;
            p_IsCreateMeshMode: Boolean;
            p_NewMapVertex: TParametricLine2D;
            p_IsEditFirstPoint: Boolean;
            p_IsEditSecondPoint: Boolean;
            p_newMapArea: VertexPositions;
            p_ConfigRenderer: TPedestrianMapPlaneRenderer;

            p_OnChangeActiveMapIndexEvents: TList<TMethod>;

            // 描画, シミュレーションに使うMapのインデクス
            p_ActiveMapIdx: Integer;

            function  GetMap(const aIdx: Integer): TPedestrianMap;
            function  GetMapCount: Integer;
            procedure SetActiveMapIdx(const aValue: Integer);
            procedure SetIsCreateMeshMode(const aValue: Boolean);

            procedure AddNewMap(const aMap: TPedestrianMap);
            procedure DoOnChangeActiveMapIndexEvents;

            procedure MakeNewMapVertexs;
            procedure OnFormMainOpenGLMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
            procedure OnFormMainTerrainClick(const aClickPoint : GLPointType; const aClickTri : F8TriangleType; var aClickCommand : ModelSelectCommandType);
        public
            constructor Create(const aAPI: IF8ApplicationServices; const aRoadList: TTranRoadList; const aCityFurnitureList: TCrowdSimCityFurnitureList);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure TrafficStarted;
            procedure TrafficStopped;

            function  GenerateNewMap(const aCellSize: Double): Boolean;
            // 描画用
            procedure Render(const aOpenGL: TF8OpenGL);

            procedure RegisterOnChangeActiveMapIndexEvent(const aEvent: OnChangeActiveMapIndexNotify);
            procedure UnRegisterOnChangeActiveMapIndexEvent(const aEvent: OnChangeActiveMapIndexNotify);

            procedure RegisterOnSynchronizeEvent(const aEvent: OnSynchronizeEventProc);
            procedure UnRegisterOnSynchronizeEvent(const aEvent: OnSynchronizeEventProc);
            procedure RegisterOnTerminateEvent(const aEvent: OnTerminateEventProc);
            procedure UnRegisterOnTerminateEvent(const aEvent: OnTerminateEventProc);

            procedure StartEditFirstPoint;
            procedure StartEditSecondPoint;

            procedure ExportToPluginData;
            procedure ImportFromPluginData;

            property  ActiveMapIdx             : Integer                 read p_ActiveMapIdx      write SetActiveMapIdx;
            property  Item[const aIdx: Integer]: TPedestrianMap          read GetMap;
            property  MapCount                 : Integer                 read GetMapCount;
            property  IsCreateMeshMode         : Boolean                 read p_IsCreateMeshMode  write SetIsCreateMeshMode;
            property  IsEditFirstPoint         : Boolean                 read p_IsEditFirstPoint;
            property  IsEditSecondPoint        : Boolean                 read p_IsEditSecondPoint;
        end;

implementation

uses
    System.SysUtils,
    System.Math,
    XML.XMLDoc,
    XML.xmldom,
    LatLonHelper,
    F8CrowdSimPluginDataConstant;

const
    NODE_NAME_ROOT = 'PluginDataPedestrianMapList';
    TAG_VERSION = 'Version'; // データバージョン
    TAG_EDITING_MAP_INDEX = 'EditingMapIndex'; // 編集中のメッシュインデクス
    TAG_MAP_COUNT = 'MapCount'; // メッシュの数
    NODE_NAME_MAP_LIST = 'MapList'; // メッシュデータの集合ノード
    NODE_NAME_MAP_ITEM = 'MapItem_'; // メッシュデータノード
    CURRENT_VERSION = 1;


{ TPedestrianMapList }
constructor TPedestrianMapList.Create(const aAPI: IF8ApplicationServices; const aRoadList: TTranRoadList; const aCityFurnitureList: TCrowdSimCityFurnitureList);
    begin
    p_API := aAPI;
    p_RoadList := aRoadList;
    p_CityFurnitureList := aCityFurnitureList;
    end;

procedure TPedestrianMapList.AfterConstruction;
    procedure RegisterAppEvents;
        var
            m: TMethod;
        begin
        FormMainOpenGLMouseMoveProc(m) := OnFormMainOpenGLMouseMove;
        p_API.RegisterEventHandler(_plgFormMainOpenGLMouseMove, m);

        FormMainTerrainClickProc(m) := OnFormMainTerrainClick;
        p_API.RegisterEventHandler(_plgFormMainTerrainClick, m);
        end;
    begin
    inherited;

    p_List := TObjectList<TPedestrianMap>.Create;
    p_Generator := TPedestrianMapGenerator.Create;
    p_ConfigRenderer := TPedestrianMapPlaneRenderer.Create;
    p_OnChangeActiveMapIndexEvents := TList<TMethod>.Create;
    p_IsCreateMeshMode := False;
    p_ActiveMapIdx := -1;
    p_IsEditFirstPoint  := False;
    p_IsEditSecondPoint := False;
    p_NewMapVertex.A  := ORIGIN_TPOINT3D;
    p_NewMapVertex.B  := ORIGIN_TPOINT3D;

    p_Generator.RegisterOnTerminateEvent(AddNewMap);
    p_RoadList.RegisterOnAddRoadEvent(p_Generator.AddRoad);
    p_RoadList.RegisterOnRemoveRoadEvent(p_Generator.RemoveRoad);
    p_CityFurnitureList.RegisterOnAddCityFurnitureEvent(p_Generator.AddCityFurniture);
    p_CityFurnitureList.RegisterOnRemoveCityFurnitureEvent(p_Generator.RemoveCityFurniture);

    RegisterAppEvents;
    end;

procedure TPedestrianMapList.BeforeDestruction;
    procedure UnRegisterAppEvents;
        var
            m: TMethod;
        begin
        FormMainOpenGLMouseMoveProc(m) := OnFormMainOpenGLMouseMove;
        p_API.UnRegisterEventHandler(_plgFormMainOpenGLMouseMove, m);

        FormMainTerrainClickProc(m) := OnFormMainTerrainClick;
        p_API.UnRegisterEventHandler(_plgFormMainTerrainClick, m);
        end;
    begin
    inherited;

    UnRegisterAppEvents;

    p_Generator.UnRegisterOnTerminateEvent(AddNewMap);
    p_RoadList.UnRegisterOnAddRoadEvent(p_Generator.AddRoad);
    p_RoadList.UnRegisterOnRemoveRoadEvent(p_Generator.RemoveRoad);
    p_CityFurnitureList.UnRegisterOnAddCityFurnitureEvent(p_Generator.AddCityFurniture);
    p_CityFurnitureList.UnRegisterOnRemoveCityFurnitureEvent(p_Generator.RemoveCityFurniture);


    p_RoadList := nil;
    p_CityFurnitureList := nil;
    FreeAndNil(p_List);
    FreeAndNil(p_Generator);
    FreeAndNil(p_ConfigRenderer);
    FreeAndNil(p_OnChangeActiveMapIndexEvents);
    end;

procedure TPedestrianMapList.TrafficStarted;
    var
        i: Integer;
    begin
    for i := 0 to p_List.Count - 1 do
        p_List[i].ResetSignals;
    end;

procedure TPedestrianMapList.TrafficStopped;
    var
        i: Integer;
    begin
    for i := 0 to p_List.Count - 1 do
        p_List[i].ResetSignals;
    end;

/// <summary>
///    新規歩行マップを生成する.
///    p_Generator.Generateを実行して別スレッドで生成処理を開始する
///    この関数の実行終了時に別スレッドでの新規歩行マップ生成処理が完了しているか未定である点に注意
///    生成中に処理を行いたい場合はRegisterOnSynchronizeEventでイベントを登録する
///    生成終了時に処理を行いたい場合はRegisterOnTerminateEventでイベントを登録する
/// </summary>
function TPedestrianMapList.GenerateNewMap(const aCellSize: Double): Boolean;
    begin
    Result := p_Generator.Generate(p_API, p_newMapVertex.A, p_newMapVertex.B, aCellSize);
    end;

procedure TPedestrianMapList.Render(const aOpenGL: TF8OpenGL);
    begin
    if not IsCreateMeshMode then
        Exit;

    if not ((p_NewMapVertex.A <> ORIGIN_TPOINT3D) and (p_NewMapVertex.B <> ORIGIN_TPOINT3D)) then
        Exit;

    p_ConfigRenderer.Render(p_newMapArea, aOpenGL);
    end;

procedure TPedestrianMapList.RegisterOnChangeActiveMapIndexEvent(const aEvent: OnChangeActiveMapIndexNotify);
    var
        m: TMethod;
    begin
    OnChangeActiveMapIndexNotify(m) := aEvent;
    if not p_OnChangeActiveMapIndexEvents.Contains(m) then
        p_OnChangeActiveMapIndexEvents.Add(m);
    end;

procedure TPedestrianMapList.UnRegisterOnChangeActiveMapIndexEvent(const aEvent: OnChangeActiveMapIndexNotify);
    var
        m: TMethod;
    begin
    OnChangeActiveMapIndexNotify(m) := aEvent;
    p_OnChangeActiveMapIndexEvents.Remove(m);
    end;

procedure TPedestrianMapList.RegisterOnSynchronizeEvent(const aEvent: OnSynchronizeEventProc);
    begin
    p_Generator.RegisterOnSynchronizeEvent(aEvent);
    end;

procedure TPedestrianMapList.UnRegisterOnSynchronizeEvent(const aEvent: OnSynchronizeEventProc);
    begin
    p_Generator.UnRegisterOnSynchronizeEvent(aEvent);
    end;

procedure TPedestrianMapList.RegisterOnTerminateEvent(const aEvent: OnTerminateEventProc);
    begin
    p_Generator.RegisterOnTerminateEvent(aEvent);
    end;

procedure TPedestrianMapList.UnRegisterOnTerminateEvent(const aEvent: OnTerminateEventProc);
    begin
    p_Generator.UnRegisterOnTerminateEvent(aEvent);
    end;

procedure TPedestrianMapList.StartEditFirstPoint;
    begin
    if not IsCreateMeshMode then
        Exit;

    if p_IsEditSecondPoint then
        Exit;

    p_IsEditFirstPoint := True;
    end;

procedure TPedestrianMapList.StartEditSecondPoint;
    begin
    if not IsCreateMeshMode then
        Exit;

    if p_IsEditFirstPoint then
        Exit;

    p_IsEditSecondPoint := True;
    end;

procedure TPedestrianMapList.ExportToPluginData;
    var
        i: Integer;
        project: IF8ProjectForRoad;
        xmlDoc: IXMLDocument;
        rootNode, listNode, itemNode: IXMLNode;
        saveText, tmpText: String;
    begin
    if not (Assigned(p_API) and (Assigned(p_API.project))) then
        Exit;

    project := p_API.project;
    project.DeletePluginData(F8_CROWD_SIM_PLUGIN_ID, PEDESTRIAN_MAP_LIST_PLUGIN_DATA);

    xmlDoc := NewXMLDocument;
    rootNode := xmlDoc.AddChild(NODE_NAME_ROOT);
    rootNode.Attributes[TAG_VERSION]           := CURRENT_VERSION.ToString;
    rootNode.Attributes[TAG_EDITING_MAP_INDEX] := ActiveMapIdx.ToString;
    rootNode.Attributes[TAG_MAP_COUNT]         := MapCount.ToString;

    listNode := rootNode.AddChild(NODE_NAME_MAP_LIST);
    for i := 0 to MapCount - 1 do
        begin
        itemNode := listNode.AddChild(Format('%s%d', [NODE_NAME_MAP_ITEM, i]));
        TPedestrianMapPluginData.ExportPluginData(p_List[i], itemNode);
        end;
    saveText := '';
    for i := 0 to xmlDoc.XML.Count - 1 do
        begin
        tmpText := Trim(xmlDoc.XML[i]);
        saveText := saveText + tmpText;
        end;

    project.pluginData[F8_CROWD_SIM_PLUGIN_ID, PEDESTRIAN_MAP_LIST_PLUGIN_DATA] := saveText;
    end;

procedure TPedestrianMapList.ImportFromPluginData;
    var
        i: Integer;
        project: IF8ProjectForRoad;
        savedText: String;
        xmlDoc: IXMLDocument;
        rootNode, listNode, itemNode: IXMLNode;
        tmpEditingMapIndex, tmpMapCount: Integer;
        newMap: TPedestrianMap;
    begin
    if not (Assigned(p_API) and (Assigned(p_API.project))) then
        Exit;

    project := p_API.project;
    savedText := project.pluginData[F8_CROWD_SIM_PLUGIN_ID, PEDESTRIAN_MAP_LIST_PLUGIN_DATA];

    if savedText = '' then
        Exit;

    xmlDoc := NewXMLDocument;
    xmlDoc.LoadFromXML(savedText);

    if xmlDoc.IsEmptyDoc then
        Exit;

    if xmlDoc.ChildNodes.Count < 0 then
        Exit;

    rootNode := xmlDoc.ChildNodes.FindNode(NODE_NAME_ROOT);

    if not Assigned(rootNode) then
        Exit;

    if not rootNode.HasAttribute(TAG_EDITING_MAP_INDEX) then
        Exit;

    tmpEditingMapIndex := StrToIntDef(rootNode.Attributes[TAG_EDITING_MAP_INDEX], -1);

    if not rootNode.HasAttribute(TAG_MAP_COUNT) then
        Exit;

    tmpMapCount := StrToIntDef(rootNode.Attributes[TAG_MAP_COUNT], 0);

    listNode := rootNode.ChildNodes.FindNode(NODE_NAME_MAP_LIST);

    if (not Assigned(listNode)) and (listNode.ChildNodes.Count <> tmpMapCount) then
        Exit;

    p_List.Clear;
    for i := 0 to listNode.ChildNodes.Count - 1 do
        begin
        itemNode := listNode.ChildNodes.FindNode(Format('%s%d', [NODE_NAME_MAP_ITEM, i]));
        if not Assigned(itemNode) then
            Break;

        if TPedestrianMapPluginData.ImportPluginData(p_API, itemNode, newMap) then
            p_List.Add(newMap);
        end;
    if MapCount > tmpEditingMapIndex then
        ActiveMapIdx := tmpEditingMapIndex;
    end;

function TPedestrianMapList.GetMap(const aIdx: Integer): TPedestrianMap;
    begin
    Assert(InRange(aIdx, 0, p_List.Count - 1));
    Result := p_List[aIdx];
    end;

function TPedestrianMapList.GetMapCount: Integer;
    begin
    Result := p_List.Count;
    end;

procedure TPedestrianMapList.SetActiveMapIdx(const aValue: Integer);
    begin
    if InRange(aValue, -1, MapCount - 1) and (p_ActiveMapIdx <> aValue) then
        begin
        p_ActiveMapIdx := aValue;
        DoOnChangeActiveMapIndexEvents;
        end;
    end;

procedure TPedestrianMapList.SetIsCreateMeshMode(const aValue: Boolean);
    begin
    p_IsCreateMeshMode := aValue;

    p_IsEditFirstPoint  := False;
    p_IsEditSecondPoint := False;
    end;

procedure TPedestrianMapList.AddNewMap(const aMap: TPedestrianMap);
    function  FindExistName(const aName: String): Boolean;
        var
            item: TPedestrianMap;
        begin
        Result := False;
        for item in p_List do
            begin
            if item.MapName = aName then
                begin
                Result := True;
                Break;
                end;
            end;
        end;
    const
        BASE_NAME = 'map_';
    var
        idx: Integer;
        num: Integer;
        newName: String;
    begin
    idx := p_List.Add(aMap.Clone);
    num := 0;
    newName := Format('%s%d', [BASE_NAME, num]);
    while FindExistName(newName) do
        begin
        Inc(num);
        newName := Format('%s%d', [BASE_NAME, num]);
        end;

    p_List[idx].MapName := newName;

    // 作成したマップを編集対象にする
    ActiveMapIdx := MapCount - 1;
    end;

procedure TPedestrianMapList.DoOnChangeActiveMapIndexEvents;
    var
        m: TMethod;
    begin
    for m in p_OnChangeActiveMapIndexEvents do
        OnChangeActiveMapIndexNotify(m)(p_List[ActiveMapIdx]);
    end;

procedure TPedestrianMapList.MakeNewMapVertexs;
    function MakePoint(const aPosX, aPosZ: Double): TPoint3D;
        var
            hr: HeightResultArrayType;
            i: Integer;
            maxH: Single;
        begin
        Result.X := aPosX;
        Result.Z := aPosZ;
        hr := p_API.Project.GetHeightsAt(Result.X, Result.Z, [_hTerrain, _hRoad, _hInterSection]);
        maxH := 0;
        if Length(hr) > 0 then
            begin
            for i := 0 to Length(hr) - 1 do
                maxH := Max(maxH, hr[i].hHeight);

            Result.Y := maxH;
            end
        else
            Result.Y := p_NewMapVertex.A.Y;
        end;

    var
        isRight, isBottom: Boolean;
    begin
    isRight   := (p_NewMapVertex.B.X < p_NewMapVertex.A.X);
    isBottom  := (p_NewMapVertex.A.Z < p_NewMapVertex.B.Z);

    if isRight then
        begin
        if isBottom then
            begin
            p_NewMapArea[VertexPositionType._LeftTop]     := p_NewMapVertex.B;
            p_NewMapArea[VertexPositionType._RightTop]    := MakePoint(p_NewMapVertex.A.X, p_NewMapVertex.B.Z);
            p_NewMapArea[VertexPositionType._RightBottom] := p_NewMapVertex.A;
            p_NewMapArea[VertexPositionType._LeftBottom]  := MakePoint(p_NewMapVertex.B.X, p_NewMapVertex.A.Z);
            end
        else
            begin
            p_NewMapArea[VertexPositionType._LeftTop]     := MakePoint(p_NewMapVertex.B.X, p_NewMapVertex.A.Z);
            p_NewMapArea[VertexPositionType._RightTop]    := p_NewMapVertex.A;
            p_NewMapArea[VertexPositionType._RightBottom] := MakePoint(p_NewMapVertex.A.X, p_NewMapVertex.B.Z);
            p_NewMapArea[VertexPositionType._LeftBottom]  :=  p_NewMapVertex.B;
            end;
        end
    else
        begin
        if isBottom then
            begin
            p_NewMapArea[VertexPositionType._LeftTop]     := MakePoint(p_NewMapVertex.A.X, p_NewMapVertex.B.Z);
            p_NewMapArea[VertexPositionType._RightTop]    := p_NewMapVertex.B;
            p_NewMapArea[VertexPositionType._RightBottom] := MakePoint(p_NewMapVertex.B.X, p_NewMapVertex.A.Z);
            p_NewMapArea[VertexPositionType._LeftBottom]  :=  p_NewMapVertex.A;
            end
        else
            begin
            p_NewMapArea[VertexPositionType._LeftTop]     := p_NewMapVertex.A;
            p_NewMapArea[VertexPositionType._RightTop]    := MakePoint(p_NewMapVertex.B.X, p_NewMapVertex.A.Z);
            p_NewMapArea[VertexPositionType._RightBottom] := p_NewMapVertex.B;
            p_NewMapArea[VertexPositionType._LeftBottom]  := MakePoint(p_NewMapVertex.A.X, p_NewMapVertex.B.Z);
            end;
        end;
    end;

procedure TPedestrianMapList.OnFormMainOpenGLMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    var
        i: Integer;
        point: GLPointType;
        validPoint: Boolean;
    begin
    if not IsCreateMeshMode then
        Exit;

    if not (p_IsEditFirstPoint or p_IsEditSecondPoint) then
        Exit;

    validPoint := True;
    point := p_API.MainForm.OpenGL.Find3DCoordinatesAtPos(X, Y);
    for i := _x to _w do
        begin
        if isNan(point[i]) then
            begin
            validPoint := False;
            Break;
            end;
        end;

    if validPoint then
        begin
        if p_IsEditFirstPoint then
            p_NewMapVertex.A.Assign(point[_x], point[_y], point[_z])
        else if p_IsEditSecondPoint then
            p_NewMapVertex.B.Assign(point[_x], point[_y], point[_z]);
        MakeNewMapVertexs;
        p_API.MainForm.OpenGL.Changed;
        end;
    end;

procedure TPedestrianMapList.OnFormMainTerrainClick(const aClickPoint : GLPointType; const aClickTri : F8TriangleType; var aClickCommand : ModelSelectCommandType);
    begin
    aClickCommand := _msc_none;
    if not IsCreateMeshMode then
        Exit;

    if p_IsEditFirstPoint then
        begin
        p_NewMapVertex.A.Assign(aClickPoint[_x], aClickPoint[_y], aClickPoint[_z]);
        p_IsEditFirstPoint := False;
        end
    else if p_IsEditSecondPoint then
        begin
        p_NewMapVertex.B.Assign(aClickPoint[_x], aClickPoint[_y], aClickPoint[_z]);
        p_IsEditSecondPoint := False;
        end;
    end;
end.
