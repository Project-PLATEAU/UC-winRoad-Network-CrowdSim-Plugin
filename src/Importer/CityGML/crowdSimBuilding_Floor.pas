unit crowdSimBuilding_Floor;

interface

uses
    System.Generics.Collections,
    Xml.XMLIntf,
    F8OpenGL,
    F8Utils,
    crowdSimBuilding_InteriorRoom,
    WalkingRouteCandidatePath;

type
    TBuildingFloor = class
        private
            p_FloorIndex: Integer;
            p_MinimumElevation: Double;
            p_Rooms: TObjectList<TCrowdSimBuilding_InteriorRoom>;

            function  GetRoom(const aIdx: Integer): TCrowdSimBuilding_InteriorRoom;
            function  GetRoomCount: Integer;
        public
            constructor Create(const aMinimumElevation: Double; const aFloorIndex: Integer);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure AddRoom(const aRoom: TCrowdSimBuilding_InteriorRoom);
            procedure GenerateCandidateWalkingPath(const aList: TCandidatePathListArray);

            procedure DoRender(const aOpenGL: TF8OpenGL);

            property  FloorIndex               : Integer                        read p_FloorIndex;
            property  MinimumElevation         : Double                         read p_MinimumElevation;
            property  Room[const aIdx: Integer]: TCrowdSimBuilding_InteriorRoom read GetRoom;
            property  RoomCount                : Integer                        read GetRoomCount;
        end;

    TBuildingFloorPluginData = class
        strict private
            const
                NODE_NAME_FLOOR     = 'CrowdSimFloor';
                NODE_NAME_ROOM_LIST = 'CrowdSimRooms';
                NODE_NAME_ROOM_ITEM = 'CrowdSimRoom_';

                TAG_FLOOR_INDEX       = 'FloorIndex';
                TAG_MINIMUM_ELEVATION = 'MinimumElevation';
                TAG_ROOM_COUNT        = 'RoomCount';
        public
            class procedure ExportToPluginData(const aBuildingFloor: TBuildingFloor; const aParentNode: IXMLNode);
            class function  ImportFromPluginData(const aDataNode: IXMLNode; out aRes: TBuildingFloor; out aFloorIndex: Integer): Boolean;
        end;

implementation

uses
    System.SysUtils,
    System.Math,
    WalkingRoutePlane;

{ TBuildingFloor }
constructor TBuildingFloor.Create(const aMinimumElevation: Double; const aFloorIndex: Integer);
    begin
    p_MinimumElevation := aMinimumElevation;
    p_FloorIndex := aFloorIndex;
    end;

procedure TBuildingFloor.AfterConstruction;
    begin
    inherited;

    p_Rooms := TObjectList<TCrowdSimBuilding_InteriorRoom>.Create;
    end;

procedure TBuildingFloor.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_Rooms);
    end;

procedure TBuildingFloor.AddRoom(const aRoom: TCrowdSimBuilding_InteriorRoom);
    begin
    p_Rooms.Add(aRoom);
    end;

procedure TBuildingFloor.GenerateCandidateWalkingPath(const aList: TCandidatePathListArray);
    var
        room: TCrowdSimBuilding_InteriorRoom;
    begin
    for room in p_Rooms do
        aList.Add(room.GenerateCandidateWalkingPath);
    end;

procedure TBuildingFloor.DoRender(const aOpenGL: TF8OpenGL);
    var
        room: TCrowdSimBuilding_InteriorRoom;
    begin
    for room in p_Rooms do
        room.DoRender(aOpenGL);
    end;

function TBuildingFloor.GetRoom(const aIdx: Integer): TCrowdSimBuilding_InteriorRoom;
    begin
    Assert(InRange(aIdx, 0, RoomCount - 1));
    Result := p_Rooms[aIdx];
    end;

function TBuildingFloor.GetRoomCount: Integer;
    begin
    Result := p_Rooms.Count;
    end;

{ TBuildingFloorPluginData }
class procedure TBuildingFloorPluginData.ExportToPluginData(const aBuildingFloor: TBuildingFloor; const aParentNode: IXMLNode);
    var
        rootNode, listNode, itemNode: IXMLNode;
        i: Integer;
    begin
    if not Assigned(aBuildingFloor) then
        Exit;

    rootNode := aParentNode.AddChild(NODE_NAME_FLOOR);
    rootNode.Attributes[TAG_FLOOR_INDEX] := aBuildingFloor.FloorIndex.ToString;
    rootNode.Attributes[TAG_MINIMUM_ELEVATION] := aBuildingFloor.MinimumElevation.ToString;
    rootNode.Attributes[TAG_ROOM_COUNT] := aBuildingFloor.RoomCount.ToString;
    listNode := rootNode.AddChild(NODE_NAME_ROOM_LIST);
    for i := 0 to aBuildingFloor.p_Rooms.Count - 1 do
        begin
        itemNode := listNode.AddChild(Format('%s%d', [NODE_NAME_ROOM_ITEM, i]));
        TCrowdSimBuilding_InteriorRoomPluginData.ExportPluginData(aBuildingFloor.p_rooms[i], itemNode);
        end;
    end;

class function TBuildingFloorPluginData.ImportFromPluginData(const aDataNode: IXMLNode; out aRes: TBuildingFloor; out aFloorIndex: Integer): Boolean;
    var
        rootNode, listNode, itemNode: IXMLNode;
        tmpMinimumElevation: Double;
        tmpRoomCount: Integer;

        i: Integer;
        newRoom: TCrowdSimBuilding_InteriorRoom;
    begin
    Result := False;

    rootNode := aDataNode.ChildNodes.FindNode(NODE_NAME_FLOOR);
    if not Assigned(rootNode) then
        Exit;

    if not rootNode.HasAttribute(TAG_FLOOR_INDEX) then
        Exit;
    aFloorIndex := StrToIntDef(rootNode.Attributes[TAG_FLOOR_INDEX], 0);

    if not rootNode.HasAttribute(TAG_MINIMUM_ELEVATION) then
        Exit;
    tmpMinimumElevation := rootNode.Attributes[TAG_MINIMUM_ELEVATION];

    if not rootNode.HasAttribute(TAG_ROOM_COUNT) then
        Exit;
    tmpRoomCount := StrToIntDef(rootnode.Attributes[TAG_ROOM_COUNT], 0);

    listNode := rootNode.ChildNodes.FindNode(NODE_NAME_ROOM_LIST);
    if not (Assigned(listNode) and (listNode.ChildNodes.Count = tmpRoomCount)) then
        Exit;

    aRes := TBuildingFloor.Create(tmpMinimumElevation, aFloorIndex);
    for i := 0 to listNode.ChildNodes.Count - 1 do
        begin
        itemNode := listNode.ChildNodes.FindNode(Format('%s%d', [NODE_NAME_ROOM_ITEM, i]));
        if TCrowdSimBuilding_InteriorRoomPluginData.ImportPluginData(itemNode, newRoom) then
            aRes.p_Rooms.Add(newRoom);
        end;

    Result := True;
    end;
end.
