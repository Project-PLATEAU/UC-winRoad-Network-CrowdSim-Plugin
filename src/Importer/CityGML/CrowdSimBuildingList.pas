unit CrowdSimBuildingList;

interface

uses
    System.Generics.Collections,
    XML.XMLIntf,
    PluginCore,
    F8OpenGL,
    F8Utils,
    CityGMLBase,
    CrowdSimCityGMLImporterModel,
    CrowdSimBuilding,
    WalkingRouteCandidatePath;

type
    TCrowdSimBuildingList = class(TCrowdSimCityGMLImporterModel)
        private
            p_InternalTable: TObjectDictionary<String, TCrowdSimBuilding>;
        protected
            function  GetModelType: TCrowdSimCityGMLModelType; override;
            procedure OnAfterImportModel(const aObj: IXMLAbstractCityObjectType); override;

            procedure DoExportToPluginData(const aRootNode: IXMLNode); override;
            procedure DoImportFromPluginData(const aRootNode: IXMLNode); override;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function  GenerateCandidateWalkingPath: TCandidatePathListArray;

            procedure DoRender(const aOpenGL: TF8OpenGL); override;
        end;

implementation

uses
    System.SysUtils,
    Building;

const
    NODE_NAME_BUILDING_LIST = 'BuildingList';
    NODE_NAME_BUILGIND_ITEM = 'Building_';
    TAG_VERSION             = 'BuildingList_Version';
    TAG_BUILDING_COUNT      = 'BuildingCount';
    CURRENT_VERSION = 1;

{ TCrowdSimBuildingList }
procedure TCrowdSimBuildingList.AfterConstruction;
    begin
    inherited;

    p_InternalTable := TObjectDictionary<String, TCrowdSimBuilding>.Create;
    end;

procedure TCrowdSimBuildingList.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_InternalTable);
    end;

function TCrowdSimBuildingList.GenerateCandidateWalkingPath: TCandidatePathListArray;
    var
        key: String;
    begin
    Result := TCandidatePathListArray.Create;
    for key in p_InternalTable.Keys do
        p_InternalTable[key].GenerateCandidateWalkingPath(Result);
    end;

procedure TCrowdSimBuildingList.DoRender(const aOpenGL: TF8OpenGL);
    var
        key: String;
    begin
    inherited;
    for key in p_InternalTable.Keys do
        p_InternalTable[key].DoRender(aOpenGL);
    end;

function TCrowdSimBuildingList.GetModelType: TCrowdSimCityGMLModelType;
    begin
    Result := _cscgBuilding;
    end;

procedure TCrowdSimBuildingList.OnAfterImportModel(const aObj: IXMLAbstractCityObjectType);
    var
        abstractBuilding: IXMLAbstractBuildingType;
        gmlId: String;
    begin
    if Supports(aObj, IXMLAbstractBuildingType, abstractBuilding) then
        begin
        gmlId := abstractBuilding.Id;
        if not p_InternalTable.ContainsKey(gmlId) then
            begin
            p_InternalTable.Add(gmlId, TCrowdSimBuilding.Create(gmlId));
            p_InternalTable[gmlId].OnAfterImportBuilding(abstractBuilding);
            end;
        end;
    end;

procedure TCrowdSimBuildingList.DoExportToPluginData(const aRootNode: IXMLNode);
    var
        listNode, itemNode: IXMLNode;
        count: Integer;
        key: String;
    begin
    aRootNode.Attributes[TAG_VERSION] := CURRENT_VERSION.ToString;
    aRootNode.Attributes[TAG_BUILDING_COUNT] := p_InternalTable.Keys.Count;

    listNode := aRootNode.AddChild(NODE_NAME_BUILDING_LIST);
    count := 0;
    for key in p_InternalTable.Keys do
        begin
        itemNode := listNode.AddChild(Format('%s%d', [NODE_NAME_BUILGIND_ITEM, count]));
        TCrowdSimBuildingPluginData.ExportPluginData(p_InternalTable[key], itemNode);
        Inc(count);
        end;
    end;

procedure TCrowdSimBuildingList.DoImportFromPluginData(const aRootNode: IXMLNode);
    var
        listNode, itemNode: IXMLNode;

        i: Integer;
        tmpBuildingCount: Integer;
        newBuilding: TCrowdSimBuilding;
        newBuildingName: String;
    begin
    if not aRootNode.HasAttribute(TAG_BUILDING_COUNT) then
        Exit;

    tmpBuildingCount := StrToIntDef(aRootNode.Attributes[TAG_BUILDING_COUNT], 0);
    listNode := aRootNode.ChildNodes.FindNode(NODE_NAME_BUILDING_LIST);

    if not (Assigned(listNode) and (listNode.ChildNodes.Count = tmpBuildingCount)) then
        Exit;

    p_InternalTable.Clear;
    for i := 0 to listNode.ChildNodes.Count - 1 do
        begin
        itemNode := listNode.ChildNodes.FindNode(Format('%s%d', [NODE_NAME_BUILGIND_ITEM, i]));
        if not Assigned(itemNode) then
            Break;

        if TCrowdSimBuildingPluginData.ImportPluginData(itemNode, newBuilding, newBuildingName) then
            p_InternalTable.Add(newBuildingName, newBuilding);
        end;
    end;
end.
