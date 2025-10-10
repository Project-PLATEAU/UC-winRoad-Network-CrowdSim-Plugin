unit crowdSimBuilding;

interface

uses
    System.Generics.Collections,
    Xml.XMLIntf,
    PluginCore,
    F8OpenGL,
    F8Utils,
    CityGMLBase,
    Building,
    crowdSimBuilding_Floor,
    crowdSimBuilding_InteriorRoom,
    WalkingRouteCandidatePath;

type
    // LOD4 Building Only
    TCrowdSimBuilding = class
        private
            p_BuildingName: String;
            p_Floors: TObjectDictionary<Integer, TBuildingFloor>;
        public
            constructor Create(const aBuildingName: String); overload;
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure OnAfterImportBuilding(const aBuilding: IXMLAbstractBuildingType);
            procedure GenerateCandidateWalkingPath(const aList: TCandidatePathListArray);

            procedure DoRender(const aOpenGL: TF8OpenGL);

            property  BuildingName: String read p_BuildingName;
        end;

    TCrowdSimBuildingPluginData = class
        strict private
            const
                NODE_NAME_BUILDING = 'CrowdSimBuilding';
                NODE_NAME_FLOOR_LIST = 'CrowdSimFloors';
                NODE_NAME_FLOOR_ITEM = 'CrowdSimFloor_';
                TAG_BUILDING_NAME  = 'BuildingName';
                TAG_FLOORS_COUNT   = 'FloorsCount';
        public
            class procedure ExportPluginData(const aBuilding: TCrowdSimBuilding; const aParentNode: IXMLNode);
            class function  ImportPluginData(const aDataNode: IXMLNode; out aRes: TCrowdSimBuilding; out aBuildingName: String): Boolean;
        end;


implementation

uses
    System.SysUtils,
    System.Math,
    System.Generics.Defaults,
    geometryBasic0d1d,
    geometryAggregates,
    geometryBasic2d;

const
    ///    部屋同士で床面の標高(m)の差がこの数値以下なら同じ階の部屋として扱う
    FLOOR_HEIGHT_THRESHOLD = 0.8;

{ TCrowdSimBuilding }
constructor TCrowdSimBuilding.Create(const aBuildingName: String);
    begin
    p_BuildingName := aBuildingName;
    end;

procedure TCrowdSimBuilding.AfterConstruction;
    begin
    inherited;

    p_Floors := TObjectDictionary<Integer, TBuildingFloor>.Create;
    end;

procedure TCrowdSimBuilding.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_Floors);
    end;

procedure TCrowdSimBuilding.OnAfterImportBuilding(const aBuilding: IXMLAbstractBuildingType);
    var
        tmpRooms: TObjectList<TCrowdSimBuilding_InteriorRoom>;
        roomPropList: IXMLInteriorRoomPropertyTypeList;
        roomProp: IXMLInteriorRoomPropertyType;
        room: IXMLRoomType;

        i: Integer;
        floorIndex: Integer;
        isFirstRoom: Boolean;
        minimumFloorElevation: Double;
    begin
    tmpRooms := TObjectList<TCrowdSimBuilding_InteriorRoom>.Create(False);
    try
        roomPropList := aBuilding.InteriorRoom;
        if roomPropList.Count < 1 then
            Exit;

        for i := 0 to roomPropList.Count - 1 do
            begin
            roomProp := roomPropList[i];
            if not Assigned(roomProp) then
                Continue;

            room := roomProp.Room;
            if not Assigned(room) then
                Continue;

            tmpRooms.Add(TCrowdSimBuilding_InteriorRoom.Create);
            tmpRooms.Last.ImportGML(room);
            end;

        // cm単位で降順ソート
        tmpRooms.Sort(TComparer<TCrowdSimBuilding_InteriorRoom>.Construct(
            function(const L, R:TCrowdSimBuilding_InteriorRoom):Integer
                begin
                Result := Trunc(R.AvgFloorElevation * 100 - L.AvgFloorElevation * 100);
                end));

        isFirstRoom := True;
        floorIndex := 0;
        minimumFloorElevation := 0.0;
        for i := tmpRooms.Count - 1 downto 0 do
            begin
            if not tmpRooms[i].hasFloorSurface then
                begin
                tmpRooms.Delete(i);
                Continue;
                end;

            if isFirstRoom then
                begin
                minimumFloorElevation := tmpRooms[i].AvgFloorElevation;
                p_Floors.Add(floorIndex, TBuildingFloor.Create(minimumFloorElevation, floorIndex));
                isFirstRoom := False;
                end
            else
                begin
                if tmpRooms[i].AvgFloorElevation > (minimumFloorElevation + FLOOR_HEIGHT_THRESHOLD) then
                    begin
                    minimumFloorElevation := tmpRooms[i].AvgFloorElevation;
                    Inc(FloorIndex);
                    p_Floors.Add(floorIndex, TBuildingFloor.Create(minimumFloorElevation, floorIndex));
                    end;
                end;

            p_Floors[floorIndex].AddRoom(tmpRooms[i]);
            end;
    finally
        FreeAndNil(tmpRooms);
        end;
    end;

procedure TCrowdSimBuilding.GenerateCandidateWalkingPath(const aList: TCandidatePathListArray);
    var
        key: Integer;
    begin
    for key in p_Floors.Keys do
        p_Floors[key].GenerateCandidateWalkingPath(aList);
    end;

procedure TCrowdSimBuilding.DoRender(const aOpenGL: TF8OpenGL);
    var
        key: Integer;
    begin
    for key in p_Floors.Keys do
        p_Floors[key].DoRender(aOpenGL);
    end;

{ TCrowdSimBuildingPluginData }
class procedure TCrowdSimBuildingPluginData.ExportPluginData(const aBuilding: TCrowdSimBuilding; const aParentNode: IXMLNode);
    var
        rootNode, listNode, itemNode: IXMLNode;
        key: Integer;
    begin
    if not Assigned(aBuilding) then
        Exit;

    rootNode := aParentNode.AddChild(NODE_NAME_BUILDING);
    rootNode.Attributes[TAG_BUILDING_NAME] := aBuilding.BuildingName;
    rootNode.Attributes[TAG_FLOORS_COUNT]  := aBuilding.p_Floors.Keys.Count.ToString;
    listNode := rootNode.AddChild(NODE_NAME_FLOOR_LIST);
    for key in aBuilding.p_Floors.Keys do
        begin
        itemNode := listNode.AddChild(Format('%s%d', [NODE_NAME_FLOOR_ITEM, key]));
        TBuildingFloorPluginData.ExportToPluginData(aBuilding.p_Floors[key], itemNode);
        end;
    end;

class function TCrowdSimBuildingPluginData.ImportPluginData(const aDataNode: IXMLNode; out aRes: TCrowdSimBuilding; out aBuildingName: String): Boolean;
    var
        rootNode, listNode, itemNode: IXMLNode;
        tmpFloorsCount: Integer;
        i: Integer;
        newFloor: TBuildingFloor;
        newFloorIndex: Integer;
    begin
    Result := False;

    rootNode := aDataNode.ChildNodes.FindNode(NODE_NAME_BUILDING);
    if not Assigned(rootNode) then
        Exit;

    if not rootNode.HasAttribute(TAG_BUILDING_NAME) then
        Exit;
    aBuildingName := rootNode.Attributes[TAG_BUILDING_NAME];

    if not rootNode.HasAttribute(TAG_FLOORS_COUNT) then
        Exit;
    tmpFloorsCount := StrToIntDef(rootNode.Attributes[TAG_FLOORS_COUNT], 0);

    listNode := rootNode.ChildNodes.FindNode(NODE_NAME_FLOOR_LIST);
    if not (Assigned(listNode) and (listNode.ChildNodes.Count = tmpFloorsCount)) then
        Exit;

    aRes := TCrowdSimBuilding.Create(aBuildingName);
    for i := 0 to listNode.ChildNodes.Count - 1 do
        begin
        itemNode := listNode.ChildNodes.FindNode(Format('%s%d', [NODE_NAME_FLOOR_ITEM, i]));
        if TBuildingFloorPluginData.ImportFromPluginData(itemNode, newFloor, newFloorIndex) then
            aRes.p_Floors.Add(newFloorIndex, newFloor);
        end;
    Result := True;
    end;
end.

