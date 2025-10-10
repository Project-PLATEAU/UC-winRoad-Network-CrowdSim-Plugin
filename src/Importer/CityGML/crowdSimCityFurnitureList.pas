unit crowdSimCityFurnitureList;

interface

uses
    System.Generics.Collections,
    System.Classes,
    XML.XMLIntf,
    PluginCore,
    CityGMLBase,
    CityFurniture,
    F8OpenGL,
    CrowdSimCityGMLImporterModel,
    crowdSimCityFurniture,
    PedestrianUtil;

type
    ChangeCityFurnitureProc = procedure(const aCityFurnitureName: String; const aCityFurniture: TcrowdSimCityFurniture) of Object;
    TCrowdSimCityFurnitureList = class(TCrowdSimCityGMLImporterModel)
        private
            p_InternalTable: TObjectDictionary<string, TCrowdSimCityFurniture>;

            p_OnAddCityFurnitureEvent   : TList<TMethod>;
            p_OnRemoveCityFurnitureEvent: TList<TMethod>;

            function  GetCityFurnitureCount: Integer;
            function  GetCityFurniture(const aCityFurnitureID: String): TCrowdSimCityFurniture;

            procedure OnChangeValueNotify(aSender: TObject; const aItem: TCrowdSimCityFurniture; aAction: TCollectionNotification);
            procedure FireOnAddCityFurniture(const aCityFurniture: TCrowdSimCityFurniture);
            procedure FireOnRemoveCityFurniture(const aCityFurniture: TCrowdSimCityFurniture);
        protected
            function  GetModelType: TCrowdSimCityGMLModelType; override;
            procedure OnAfterImportModel(const aObj: IXMLAbstractCityObjectType); override;
            procedure DoExportToPluginData(const aRootNode: IXMLNode); override;
            procedure DoImportFromPluginData(const aRootNode: IXMLNode); override;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure AddCityFurniture(const aCityFurnitureName: String; const aCityFurniture: TCrowdSimCityFurniture);
            procedure RemoveCityFurniture(const aCityFurnitureName: String);

            function  IsOverlayPedestrianArea(const aArea: VertexPositions): Boolean;

            procedure DoRender(const aOpenGL: TF8OpenGL); override;

            function  ExtractAllCityFurnitureID(const aList: TStringList): Boolean;

            procedure RegisterOnAddCityFurnitureEvent(const aEvent: ChangeCityFurnitureProc);
            procedure UnRegisterOnAddCityFurnitureEvent(const aEvent: ChangeCityFurnitureProc);
            procedure RegisterOnRemoveCityFurnitureEvent(const aEvent: ChangeCityFurnitureProc);
            procedure UnRegisterOnRemoveCityFurnitureEvent(const aEvent: ChangeCityFurnitureProc);

            property  CityFurnitureCount                           : Integer                read GetCityFurnitureCount;
            property  CityFurniture[const aCityFurnitureID: String]: TCrowdSimCityFurniture read GetCityFurniture;
        end;

implementation

uses
    System.SysUtils;

const
    NODE_NAME_CITY_FURNITURE_LIST = 'CityFurnitureList';
    NODE_NAME_CITY_FURNITURE_ITEM = 'CityFurniture_';
    TAG_VERSION = 'Version';
    TAG_CITY_FURNITURE_COUNT = 'CityFurnitureCount';
    CURRENT_VERSION = 1;

{ TCrowdSimCityFurnitureList }
procedure TCrowdSimCityFurnitureList.AfterConstruction;
    begin
    inherited;

    p_InternalTable := TObjectDictionary<String, TCrowdSimCityFurniture>.Create;
    p_OnAddCityFurnitureEvent   := TList<TMethod>.Create;
    p_OnRemoveCityFurnitureEvent:= TList<TMethod>.Create;

    p_InternalTable.OnValueNotify := OnChangeValueNotify;
    end;

procedure TCrowdSimCityFurnitureList.BeforeDestruction;
    begin
    inherited;

    p_OnAddCityFurnitureEvent.Clear;
    p_OnRemoveCityFurnitureEvent.Clear;

    FreeAndNil(p_InternalTable);
    FreeAndNil(p_OnAddCityFurnitureEvent);
    FreeAndNil(p_OnRemoveCityFurnitureEvent);
    end;

procedure TCrowdSimCityFurnitureList.AddCityFurniture(const aCityFurnitureName: String; const aCityFurniture: TCrowdSimCityFurniture);
    begin
    if not p_InternalTable.ContainsKey(aCityFurnitureName) then
        p_InternalTable.Add(aCityFurnitureName, aCityFurniture);
    end;

procedure TCrowdSimCityFurnitureList.RemoveCityFurniture(const aCityFurnitureName: String);
    begin
    p_InternalTable.Remove(aCityFurnitureName);
    end;

function TCrowdSimCityFurnitureList.IsOverlayPedestrianArea(const aArea: VertexPositions): Boolean;
    var
        key: String;
    begin
    Result := False;

    for key in p_InternalTable.Keys do
        begin
        if p_InternalTable[key].IsOverlayPedestrianArea(aArea) then
            begin
            Result := True;
            Break;
            end;
        end;
    end;

procedure TCrowdSimCityFurnitureList.DoRender(const aOpenGL: TF8OpenGL);
    var
        key: String;
    begin
    inherited;
    for key in p_InternalTable.Keys do
        p_InternalTable[key].DoRender(aOpenGL);
    end;

function TCrowdSimCityFurnitureList.ExtractAllCityFurnitureID(const aList: TStringList): Boolean;
    begin
    Result := Assigned(aList);
    if not Result then
        Exit;

    Assert(aList.Count = 0);
    aList.AddStrings(p_InternalTable.Keys.ToArray);
    end;

procedure TCrowdSimCityFurnitureList.RegisterOnAddCityFurnitureEvent(const aEvent: ChangeCityFurnitureProc);
    var
        m: TMethod;
    begin
    ChangeCityFurnitureProc(m) := aEvent;
    if not p_OnAddCityFurnitureEvent.Contains(m) then
        p_OnAddCityFurnitureEvent.Add(m);
    end;

procedure TCrowdSimCityFurnitureList.UnRegisterOnAddCityFurnitureEvent(const aEvent: ChangeCityFurnitureProc);
    var
        m: TMethod;
    begin
    ChangeCityFurnitureProc(m) := aEvent;
    p_OnAddCityFurnitureEvent.Remove(m);
    end;

procedure TCrowdSimCityFurnitureList.RegisterOnRemoveCityFurnitureEvent(const aEvent: ChangeCityFurnitureProc);
    var
        m: TMethod;
    begin
    ChangeCityFurnitureProc(m) := aEvent;
    if not p_OnRemoveCityFurnitureEvent.Contains(m) then
        p_OnRemoveCityFurnitureEvent.Add(m);
    end;

procedure TCrowdSimCityFurnitureList.UnRegisterOnRemoveCityFurnitureEvent(const aEvent: ChangeCityFurnitureProc);
    var
        m: TMethod;
    begin
    ChangeCityFurnitureProc(m) := aEvent;
    p_OnRemoveCityFurnitureEvent.Remove(m);
    end;

function TCrowdSimCityFurnitureList.GetModelType: TCrowdSimCityGMLModelType;
    begin
    Result := _cscgCityFurniture;
    end;

procedure TCrowdSimCityFurnitureList.OnAfterImportModel(const aObj: IXMLAbstractCityObjectType);
    var
        abstractCityFurniture: IXMLCityFurnitureType;
        gmlId: String;
    begin
    if Supports(aObj, IXMLCityFurnitureType, abstractCityFurniture) then
        begin
        gmlId := abstractCityFurniture.Id;
        if not p_InternalTable.ContainsKey(gmlId) then
            begin
            p_InternalTable.Add(gmlId, TCrowdSimCityFurniture.Create(abstractCityFurniture));
            end;
        end;
    end;

procedure TCrowdSimCityFurnitureList.DoExportToPluginData(const aRootNode: IXMLNode);
    var
        listNode, itemNode: IXMLNode;
        count: Integer;
        key: String;
    begin
    aRootNode.Attributes[TAG_VERSION]              := CURRENT_VERSION.ToString;
    aRootNode.Attributes[TAG_CITY_FURNITURE_COUNT] := p_InternalTable.Keys.Count.ToString;

    listNode := aRootNode.AddChild(NODE_NAME_CITY_FURNITURE_LIST);
    count := 0;
    for key in p_InternalTable.Keys do
        begin
        itemNode := listNode.AddChild(Format('%s%d', [NODE_NAME_CITY_FURNITURE_ITEM, count]));
        TCrowdSimCityFurniturePluginData.ExportToPluginData(p_InternalTable[key], itemNode);
        Inc(count);
        end;
    end;

procedure TCrowdSimCityFurnitureList.DoImportFromPluginData(const aRootNode: IXMLNode);
    var
        listNode, itemNode: IXMLNode;
        newCityFurniture: TCrowdSimCityFurniture;
        tmpItemCount: Integer;
        i: Integer;
    begin
    if not aRootNode.HasAttribute(TAG_CITY_FURNITURE_COUNT) then
        Exit;

    tmpItemCount := StrToIntDef(aRootNode.Attributes[TAG_CITY_FURNITURE_COUNT], 0);
    listNode := aRootNode.ChildNodes.FindNode(NODE_NAME_CITY_FURNITURE_LIST);
    if (not Assigned(listNode)) and (listNode.ChildNodes.Count <> tmpItemCount) then
        Exit;

    p_InternalTable.Clear;
    for i := 0 to listNode.ChildNodes.Count - 1 do
        begin
        itemNode := listNode.ChildNodes.FindNode(Format('%s%d', [NODE_NAME_CITY_FURNITURE_ITEM, i]));
        if not Assigned(itemNode) then
            Break;

        if TCrowdSimCityFurniturePluginData.ImportFromPluginData(itemNode, newCityFurniture) then
            p_InternalTable.Add(newCityFurniture.Name, newCityFurniture);
        end;
    end;

function TCrowdSimCityFurnitureList.GetCityFurnitureCount: Integer;
    begin
    Result := p_InternalTable.Count;
    end;

function TCrowdSimCityFurnitureList.GetCityFurniture(const aCityFurnitureID: String): TCrowdSimCityFurniture;
    begin
    Assert(p_InternalTable.ContainsKey(aCityFurnitureID));
    Result := p_InternalTable[aCityFurnitureID];
    end;

procedure TCrowdSimCityFurnitureList.OnChangeValueNotify(aSender: TObject; const aItem: TCrowdSimCityFurniture; aAction: TCollectionNotification);
    begin
    case aAction of
        cnAdded  : FireOnAddCityFurniture(aItem);
        cnRemoved: FireOnRemoveCityFurniture(aItem);
        end;
    end;

procedure TCrowdSimCityFurnitureList.FireOnAddCityFurniture(const aCityFurniture: TCrowdSimCityFurniture);
    var
        m: TMethod;
    begin
    for m in p_OnAddCityFurnitureEvent do
        ChangeCityFurnitureProc(m)(aCityFurniture.Name, aCityFurniture);
    end;

procedure TCrowdSimCityFurnitureList.FireOnRemoveCityFurniture(const aCityFurniture: TCrowdSimCityFurniture);
    var
        m: TMethod;
    begin
    for m in p_OnRemoveCityFurnitureEvent do
        ChangeCityFurnitureProc(m)(aCityFurniture.Name, aCityFurniture);
    end;
end.
