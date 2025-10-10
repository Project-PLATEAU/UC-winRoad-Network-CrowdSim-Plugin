unit crowdSimBuilding_InteriorRoom;

interface

uses
    System.Generics.Collections,
    Xml.XMLIntf,
    F8OpenGL,
    F8Utils,
    CityGMLBase,
    Building,
    basicTypes,
    MultiSurface,
    WalkingRoutePlane,
    WalkingRouteCandidatePath;

type
    TCrowdSimBuilding_InteriorRoom = class
        private
            p_FloorMultiSurfaces : TObjectList<TMultiSurface>;
            p_InteriorWallMultiSurfaces: TObjectList<TMultiSurface>;

            p_RoomName: String;
            p_AvgFloorElevation: Double;

            function  GetHasFloorSurface: Boolean;

            procedure ImportFloorSurface(const aFloorSurface: IXMLFloorSurfaceType);
            procedure ImportInteriorWallSurface(const aInteriorWallSurface: IXMLInteriorWallSurfaceType);
            procedure ImportRoomName(const aRoomName: IXMLCodeTypeList);
            procedure CalcAvgFloorElevation;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure ImportGML(const aRoom: IXMLRoomType);
            procedure DoRender(const aOpenGL: TF8OpenGL);
            function  GenerateCandidateWalkingPath: TCandidatePathList;

            property  RoomName         : String  read p_RoomName;
            property  hasFloorSurface  : Boolean read GetHasFloorSurface;
            property  AvgFloorElevation: Double  read p_AvgFloorElevation;
        end;

    TCrowdSimBuilding_InteriorRoomPluginData = class
        strict private
            const
                NODE_NAME_INTERIOR_ROOM                   = 'CrowdSimInteriorRoom';
                NODE_NAME_FLOOR_MULTISURFACE_LIST         = 'CrowdSimFloorMultiSurfaces';
                NODE_NAME_FLOOR_MULTISURFACE              = 'CrowdSimFloorMultiSurface_';
                NODE_NAME_INTERIOR_WALL_MULTISURFACE_LIST = 'CrowdSimInteriorWallMultiSurfaces';
                NODE_NAME_INTERIOR_WALL_MULTISURFACE      = 'CrowdSimInteriorWallMultiSurface_';

                TAG_ROOM_NAME           = 'RoomName';
                TAG_AVG_FLOOR_ELEVATION = 'Avg_Floor_Elevation';
                TAG_FLOOR_MULTISURFACE_COUNT = 'FloorMultiSurfaceCount';
                TAG_INTERIOR_WALL_MULTISURFACE_COUNT = 'InteriorWallMultiSurfaceCount';
        public
            class procedure ExportPluginData(const aInteriorRoom: TCrowdSimBuilding_InteriorRoom; const aPrentNode: IXMLNode);
            class function  ImportPluginData(const aDataNode: IXMLNode; out aRes: TCrowdSimBuilding_InteriorRoom): Boolean;
        end;

implementation

uses
    System.SysUtils,
    geometryBasic0d1d,
    geometryAggregates,
    geometryBasic2d;

{ TCrowdSimBuilding_InteriorRoom }
procedure TCrowdSimBuilding_InteriorRoom.AfterConstruction;
    begin
    inherited;

    p_FloorMultiSurfaces  := TObjectList<TMultiSurface>.Create;
    p_InteriorWallMultiSurfaces := TObjectList<TMultiSurface>.Create;
    p_RoomName := '';
    p_AvgFloorElevation := 0.0;
    end;

procedure TCrowdSimBuilding_InteriorRoom.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_FloorMultiSurfaces);
    FreeAndNil(p_InteriorWallMultiSurfaces);
    end;

procedure TCrowdSimBuilding_InteriorRoom.ImportGML(const aRoom: IXMLRoomType);
    var
        baundarySurfacePropList: IXMLBoundarySurfacePropertyTypeList;
        baundarySurface: IXMLBoundarySurfacePropertyType;
        baundaryAbstract: IXMLAbstractBoundarySurfaceType;
        floorSurface: IXMLFloorSurfaceType;
        interiorWallSurface: IXMLInteriorWallSurfaceType;
        roomName: IXMLCodeTypeList;

        i: Integer;
    begin
    baundarySurfacePropList := aRoom.BoundedBy;
    if not Assigned(baundarySurfacePropList)  then
        Exit;

    for i := 0 to baundarySurfacePropList.Count - 1 do
        begin
        baundarySurface := baundarySurfacePropList[i];
        if not Assigned(baundarySurface) then
            Continue;

        baundaryAbstract := baundarySurface._BoundarySurface;
        if not Assigned(baundaryAbstract) then
            Continue;

        if Supports(baundaryAbstract, IXMLFloorSurfaceType, floorSurface) then
            ImportFloorSurface(floorSurface)
        else if Supports(baundaryAbstract, IXMLInteriorWallSurfaceType, interiorWallSurface) then
            ImportInteriorWallSurface(interiorWallSurface);
        end;

    roomName := aRoom.Name;
    if Assigned(roomName) then
        ImportRoomName(roomName);
    CalcAvgFloorElevation;
    end;

procedure TCrowdSimBuilding_InteriorRoom.DoRender(const aOpenGL: TF8OpenGL);
    var
        item: TMultiSurface;
    begin
    for item in p_FloorMultiSurfaces do
        item.DoRender(aOpenGL, Point3D(1.0, 0.5, 0.8));
    for item in p_InteriorWallMultiSurfaces do
        item.DoRender(aOpenGL, Point3D(0.5, 1.0, 0.8));
    end;

function TCrowdSimBuilding_InteriorRoom.GenerateCandidateWalkingPath: TCandidatePathList;
    var
        planes: TWalkingRoutePlanes;
        i, j, k: Integer;
        fixedCount: Integer;
        restart: Boolean;
        tmpPathList: TParametricLIneList;
    begin
    Result := nil;
    planes := TWalkingRoutePlanes.Create;
    try
        // SurfaceItemごとにWalkingRoutePlaneを作る
        for i := 0 to p_FloorMultiSurfaces.Count - 1 do
            begin
            for j := 0 to p_FloorMultiSurfaces[i].SurfaceCount - 1 do
                planes.Add(TWalkingRoutePlane.Create(p_FloorMultiSurfaces[i].Surface[j]));
            end;

        if planes.Count < 1 then
            Exit;

        // 内壁面と接続判定を行う
        for i := 0 to planes.Count - 1 do
            begin
            for j := 0 to p_InteriorWallMultiSurfaces.Count - 1 do
                begin
                for k := 0 to p_InteriorWallMultiSurfaces[j].SurfaceCount - 1 do
                    planes[i].CheckObstacle(p_InteriorWallMultiSurfaces[j].Surface[k]);
                end;
            end;
        // 結合する
        fixedCount := 0;
        while True do
            begin
            if fixedCount = planes.Count - 1 then
                Break;

            restart := False;
            for i := fixedCount + 1 to planes.Count - 1 do
                begin
                if planes[fixedCount].Combine(planes[i]) then
                    begin
                    planes.Delete(i);
                    restart := True;
                    Break;
                    end;
                end;

            if restart then
                fixedCount := 0
            else
                Inc(fixedCount);
            end;

        SetLength(tmpPathList, 0);
        for i := 0 to planes.Count - 1 do
            tmpPathList := tmpPathList + planes[i].GenerateCandidateWalkingPath;

        Result := TCandidatePathList.Create(tmpPathList);
    finally
        FreeAndNil(planes);
        end;
    end;

function TCrowdSimBuilding_InteriorRoom.GetHasFloorSurface: Boolean;
    begin
    Result := (p_FloorMultiSurfaces.Count > 0);
    end;

procedure TCrowdSimBuilding_InteriorRoom.ImportFloorSurface(const aFloorSurface: IXMLFloorSurfaceType);
    var
        lod4MultiSurfaceProp: IXMLMultiSurfacePropertyType;
    begin
    lod4MultiSurfaceProp := aFloorSurface.Lod4MultiSurface;
    if not Assigned(lod4MultiSurfaceProp) then
        Exit;

    p_FloorMultiSurfaces.Add(TMultiSurface.Create);
    p_FloorMultiSurfaces.Last.ImportCityGML(lod4MultiSurfaceProp);
    end;

procedure TCrowdSimBuilding_InteriorRoom.ImportInteriorWallSurface(const aInteriorWallSurface: IXMLInteriorWallSurfaceType);
    var
        lod4MultiSurfaceProp: IXMLMultiSurfacePropertyType;
    begin
    lod4MultiSurfaceProp := aInteriorWallSurface.Lod4MultiSurface;

    if not Assigned(lod4MultiSurfaceProp) then
        Exit;

    p_InteriorWallMultiSurfaces.Add(TMultiSurface.Create);
    p_InteriorWallMultiSurfaces.Last.ImportCityGML(lod4MultiSurfaceProp);
    end;

procedure TCrowdSimBuilding_InteriorRoom.ImportRoomName(const aRoomName: IXMLCodeTypeList);
    var
        i: Integer;
    begin
    if aRoomName.Count < 1 then
        Exit;

    for i := 0 to aRoomName.Count - 1 do
        begin
        if aRoomName.Items[i].IsTextElement then
            begin
            p_RoomName := aRoomName.Items[i].Text;
            Break;
            end;
        end;
    end;

procedure TCrowdSimBuilding_InteriorRoom.CalcAvgFloorElevation;
    var
        sum, sums: Double;
        i, j: Integer;
    begin
    if not hasFloorSurface then
        Exit;

    sums := 0;
    for i := 0 to p_FloorMultiSurfaces.Count - 1 do
        begin
        sum := 0.0;
        for j := 0 to p_FloorMultiSurfaces[i].SurfaceCount - 1 do
            sum := sum + p_FloorMultiSurfaces[i].Surface[j].AvgElevation;
        sums := sums + (sum / p_FloorMultiSurfaces[i].SurfaceCount);
        end;
    p_AvgFloorElevation := sums / p_FloorMultiSurfaces.Count;
    end;

{ TCrowdSimBuilding_InteriorRoomPluginData }
class procedure TCrowdSimBuilding_InteriorRoomPluginData.ExportPluginData(const aInteriorRoom: TCrowdSimBuilding_InteriorRoom; const aPrentNode: IXMLNode);
    var
        rootNode, floorSurfaceListNode, floorSurfaceItemNode, interiorWallSurfaceListNode, interiorWallSurfaceItemNode: IXMLNode;
        i: Integer;
    begin
    if not Assigned(aInteriorRoom) then
        Exit;

    rootNode := aPrentNode.AddChild(NODE_NAME_INTERIOR_ROOM);
    rootNode.Attributes[TAG_ROOM_NAME] := aInteriorRoom.RoomName;
    rootNode.Attributes[TAG_AVG_FLOOR_ELEVATION] := aInteriorRoom.AvgFloorElevation.ToString;
    rootNode.Attributes[TAG_FLOOR_MULTISURFACE_COUNT] := aInteriorRoom.p_FloorMultiSurfaces.Count.ToString;
    rootNode.Attributes[TAG_INTERIOR_WALL_MULTISURFACE_COUNT] := aInteriorRoom.p_InteriorWallMultiSurfaces.Count.ToString;

    floorSurfaceListNode := rootNode.AddChild(NODE_NAME_FLOOR_MULTISURFACE_LIST);
    for i := 0 to aInteriorRoom.p_FloorMultiSurfaces.Count - 1 do
        begin
        floorSurfaceItemNode := floorSurfaceListNode.AddChild(Format('%s%d', [NODE_NAME_FLOOR_MULTISURFACE, i]));
        aInteriorRoom.p_FloorMultiSurfaces[i].ExportToPluginData(floorSurfaceItemNode);
        end;

    interiorWallSurfaceListNode := rootNode.AddChild(NODE_NAME_INTERIOR_WALL_MULTISURFACE_LIST);
    for i := 0 to aInteriorRoom.p_InteriorWallMultiSurfaces.Count - 1 do
        begin
        interiorWallSurfaceItemNode := interiorWallSurfaceListNode.AddChild(Format('%s%d', [NODE_NAME_INTERIOR_WALL_MULTISURFACE, i]));
        aInteriorRoom.p_InteriorWallMultiSurfaces[i].ExportToPluginData(interiorWallSurfaceItemNode);
        end;
    end;

class function TCrowdSimBuilding_InteriorRoomPluginData.ImportPluginData(const aDataNode: IXMLNode; out aRes: TCrowdSimBuilding_InteriorRoom): Boolean;
    var
        rootNode, floorSurfaceListNode, floorSurfaceItemNode, interiorWallSurfaceListNode, interiorWallSurfaceItemNode: IXMLNode;

        i: Integer;
        tmpRoomName: String;
        tmpRoom_FloorMultiSurfaceCount, tmpRoom_InteriorWallMultiSurfaceCount: Integer;
        tmpAvgFloorElevation: Double;
        tmpMultiSurface: TMultiSurface;
    begin
    Result := False;

    rootNode := aDataNode.ChildNodes.FindNode(NODE_NAME_INTERIOR_ROOM);
    if not Assigned(rootNode) then
        Exit;

    if not rootNode.HasAttribute(TAG_ROOM_NAME) then
        Exit;
    tmpRoomName := rootNode.Attributes[TAG_ROOM_NAME];

    if not rootNode.HasAttribute(TAG_AVG_FLOOR_ELEVATION) then
        Exit;
    tmpAvgFloorElevation := StrToFloatDef(rootNode.Attributes[TAG_AVG_FLOOR_ELEVATION], 0);

    if not rootNode.HasAttribute(TAG_FLOOR_MULTISURFACE_COUNT) then
        Exit;
    tmpRoom_FloorMultiSurfaceCount := StrToIntDef(rootNode.Attributes[TAG_FLOOR_MULTISURFACE_COUNT], 0);

    if not rootNode.HasAttribute(TAG_INTERIOR_WALL_MULTISURFACE_COUNT) then
        Exit;
    tmpRoom_InteriorWallMultiSurfaceCount := StrToIntDef(rootNode.Attributes[TAG_INTERIOR_WALL_MULTISURFACE_COUNT], 0);

    floorSurfaceListNode := rootNode.ChildNodes.FindNode(NODE_NAME_FLOOR_MULTISURFACE_LIST);
    if not (Assigned(floorSurfaceListNode) and (floorSurfaceListNode.ChildNodes.Count = tmpRoom_FloorMultiSurfaceCount)) then
        Exit;

    interiorWallSurfaceListNode := rootNode.ChildNodes.FindNode(NODE_NAME_INTERIOR_WALL_MULTISURFACE_LIST);
    if not (Assigned(interiorWallSurfaceListNode) and (interiorWallSurfaceListNode.ChildNodes.Count = tmpRoom_InteriorWallMultiSurfaceCount)) then
        Exit;

    aRes := TCrowdSimBuilding_InteriorRoom.Create;
    aRes.p_RoomName := tmpRoomName;
    aRes.p_AvgFloorElevation := tmpAvgFloorElevation;

    for i := 0 to floorSurfaceListNode.ChildNodes.Count - 1 do
        begin
        floorSurfaceItemNode := floorSurfaceListNode.ChildNodes.FindNode(Format('%s%d', [NODE_NAME_FLOOR_MULTISURFACE, i]));
        if not Assigned(floorSurfaceItemNode) then
            Break;

        tmpMultiSurface := TMultiSurface.Create;
        if tmpMultiSurface.ImportFromPluginData(floorSurfaceItemNode) then
            aRes.p_FloorMultiSurfaces.Add(tmpMultiSurface);
        end;

    for i := 0 to interiorWallSurfaceListNode.ChildNodes.Count - 1 do
        begin
        interiorWallSurfaceItemNode := interiorWallSurfaceListNode.ChildNodes.FindNode(Format('%s%d', [NODE_NAME_INTERIOR_WALL_MULTISURFACE, i]));
        if not Assigned(interiorWallSurfaceItemNode) then
            Break;

        tmpMultiSurface := TMultiSurface.Create;
        if tmpMultiSurface.ImportFromPluginData(interiorWallSurfaceItemNode) then
            aRes.p_InteriorWallMultiSurfaces.Add(tmpMultiSurface);
        end;

    Result := True;
    end;
end.

