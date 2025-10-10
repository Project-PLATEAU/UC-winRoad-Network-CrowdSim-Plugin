unit TranRoadList;

interface

uses
    System.SyncObjs,
    System.Generics.Collections,
    System.Classes,
    XML.XMLIntf,
    cityGMLBase,
    transportation,
    PluginCore,
    F8OpenGL,
    CrowdSimCityGMLImporterModel,
    TranRoad,
    PedestrianUtil;

type
    ChangeRoadProc = procedure(const aRoadName: String; const aRoad: TTranRoad) of object;
    /// <summary>
    ///    PLATEAU道路モデルの道路の集合を表すクラス
    ///    PLATEAU道路関連を管理するルートクラス
    /// </summary>
    TTranRoadList = class(TCrowdSimCityGMLImporterModel)
        private
            p_Table: TObjectDictionary<String, TTranRoad>;

            p_OnAddRoadEvent   : TList<TMethod>;
            p_OnRemoveRoadEvent: TList<TMethod>;

            function  GetRoadCount: Integer;
            function  GetRoad(const aRoadID: String): TTranRoad;

            procedure OnChangeValueNotify(aSender: TObject; const aItem: TTranRoad; aAction: TCollectionNotification);
            procedure FireOnAddRoad(const aRoad: TTranRoad);
            procedure FireOnRemoveRoad(const aRoad: TTranRoad);
        protected
            function  GetModelType: TCrowdSimCityGMLModelType; override;
            procedure OnAfterImportModel(const aObj: IXMLAbstractCityObjectType); override;
            procedure DoExportToPluginData(const aRootNode: IXMLNode); override;
            procedure DoImportFromPluginData(const aRootNode: IXMLNode); override;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure AddRoad(const aRoadName: String; const aRoad: TTranRoad);
            procedure RemoveRoad(const aRoadName: String);

            procedure DoRender(const aOpenGL: TF8OpenGL); override;

            function  ExtractAllRoadID(const aList: TStringList): Boolean;

            procedure RegisterOnAddRoadEvent(const aEvent: ChangeRoadProc);
            procedure UnRegisterOnAddRoadEvent(const aEvent: ChangeRoadProc);
            procedure RegisterOnRemoveRoadEvent(const aEvent: ChangeRoadProc);
            procedure UnRegisterOnRemoveRoadEvent(const aEvent: ChangeRoadProc);

            property  RoadCount                  : Integer   read GetRoadCount;
            property  Road[const aRoadID: String]: TTranRoad read GetRoad;
        end;

implementation

uses
    System.SysUtils;

const
    NODE_NAME_ROAD_LIST = 'RoadList';
    NODE_NAME_ROAD_ITEM = 'Road_';
    TAG_VERSION = 'Version';
    TAG_TRAN_ROAD_COUNT = 'TranRoadCount';
    CURRENT_VERSION = 1;

{ TTranRoadList }
procedure TTranRoadList.AfterConstruction;
    begin
    inherited;

    p_Table := TObjectDictionary<String, TTranRoad>.Create([doOwnsValues]);
    p_Table.OnValueNotify := OnChangeValueNotify;

    p_OnAddRoadEvent := TList<TMethod>.Create;
    p_OnRemoveRoadEvent := TList<TMethod>.Create;
    end;

procedure TTranRoadList.BeforeDestruction;
    begin
    inherited;

    p_Table.OnKeyNotify := nil;
    FreeAndNil(p_OnAddRoadEvent);
    FreeAndNil(p_OnRemoveRoadEvent);
    FreeAndNil(p_Table);
    end;

procedure TTranRoadList.AddRoad(const aRoadName: String; const aRoad: TTranRoad);
    begin
    if not p_Table.ContainsKey(aRoadName) then
        p_Table.Add(aRoadName, aRoad);
    end;

procedure TTranRoadList.RemoveRoad(const aRoadName: String);
    begin
    p_Table.Remove(aRoadName);
    end;

procedure TTranRoadList.DoRender(const aOpenGL: TF8OpenGL);
    var
        key: String;
    begin
    inherited;
    for key in p_Table.Keys do
        p_Table[key].DoRender(aOpenGL);
    end;

function TTranRoadList.ExtractAllRoadID(const aList: TStringList): Boolean;
    begin
    Result := Assigned(aList);
    if not Result then
        Exit;

    Assert(aList.Count = 0);
    aList.AddStrings(p_Table.Keys.ToArray);
    end;

procedure TTranRoadList.RegisterOnAddRoadEvent(const aEvent: ChangeRoadProc);
    var
        m: TMethod;
    begin
    ChangeRoadProc(m) := aEvent;
    if not p_OnAddRoadEvent.Contains(m) then
        p_OnAddRoadEvent.Add(m);
    end;

procedure TTranRoadList.UnRegisterOnAddRoadEvent(const aEvent: ChangeRoadProc);
    var
        m: TMethod;
    begin
    ChangeRoadProc(m) := aEvent;
    p_OnAddRoadEvent.Remove(m);
    end;

procedure TTranRoadList.RegisterOnRemoveRoadEvent(const aEvent: ChangeRoadProc);
    var
        m: TMethod;
    begin
    ChangeRoadProc(m) := aEvent;
    if not p_OnRemoveRoadEvent.Contains(m) then
        p_OnRemoveRoadEvent.Add(m);
    end;

procedure TTranRoadList.UnRegisterOnRemoveRoadEvent(const aEvent: ChangeRoadProc);
    var
        m: TMethod;
    begin
    ChangeRoadProc(m) := aEvent;
    p_OnRemoveRoadEvent.Remove(m);
    end;

function TTranRoadList.GetModelType: TCrowdSimCityGMLModelType;
    begin
    Result := _cscgRoad;
    end;

procedure TTranRoadList.OnAfterImportModel(const aObj: IXMLAbstractCityObjectType);
    var
        cityRoad: IXMLRoadType;
        citySquare: IXMLSquareType;
        gmlId: String;
    begin
    if Supports(aObj, IXMLRoadType, cityRoad) then
        begin
        gmlId := cityRoad.Id;
        if not p_Table.ContainsKey(gmlId) then
            p_Table.Add(gmlId, TTranRoad.Create(cityRoad, gmlId));
        end
    else if Supports(aObj, IXMLSquareType, citySquare) then
        begin
        gmlId := citySquare.Id;
        if not p_Table.ContainsKey(gmlId) then
            p_Table.Add(gmlId, TTranRoad.Create(citySquare, gmlId));
        end;
    end;

procedure TTranRoadList.DoExportToPluginData(const aRootNode: IXMLNode);
    var
        listNode, itemNode: IXMLNode;
        key: String;
        count: Integer;
    begin
    aRootNode.Attributes[TAG_VERSION]         := CURRENT_VERSION.ToString;
    aRootNode.Attributes[TAG_TRAN_ROAD_COUNT] := p_Table.Keys.Count;

    listNode := aRootNode.AddChild(NODE_NAME_ROAD_LIST);
    count := 0;
    for key in p_Table.Keys do
        begin
        itemNode := listNode.AddChild(Format('%s%d', [NODE_NAME_ROAD_ITEM, count]));
        TTranRoadPluginData.ExportPluginData(p_Table[key], itemNode);
        Inc(count);
        end;
    end;

procedure TTranRoadList.DoImportFromPluginData(const aRootNode: IXMLNode);
    var
        listNode, itemNode: IXMLNode;
        i: Integer;
        newRoad: TTranRoad;
        newRoadName: String;
        tmpRoadCount: Integer;
    begin
    if not aRootNode.HasAttribute(TAG_TRAN_ROAD_COUNT) then
        Exit;

    tmpRoadCount := StrToIntDef(aRootNode.Attributes[TAG_TRAN_ROAD_COUNT], 0);
    listNode := aRootNode.ChildNodes.FindNode(NODE_NAME_ROAD_LIST);

    if (not Assigned(listNode)) and (listNode.ChildNodes.Count <> tmpRoadCount) then
        Exit;

    p_Table.Clear;
    for i := 0 to listNode.ChildNodes.Count - 1 do
        begin
        itemNode := listNode.ChildNodes.FindNode(Format('%s%d', [NODE_NAME_ROAD_ITEM, i]));
        if not Assigned(itemNode) then
            Break;

        if TTranRoadPluginData.ImportPluginData(itemNode, newRoad, newRoadName) then
            p_Table.Add(newRoadName, newRoad);
        end;
    end;

function TTranRoadList.GetRoadCount: Integer;
    begin
    Result := p_Table.Keys.Count;
    end;

function TTranRoadList.GetRoad(const aRoadID: String): TTranRoad;
    begin
    Assert(p_Table.ContainsKey(aRoadID));
    Result := p_Table[aRoadID];
    end;

procedure TTranRoadList.OnChangeValueNotify(aSender: TObject; const aItem: TTranRoad; aAction: TCollectionNotification);
    begin
    case aAction of
        cnAdded  : FireOnAddRoad(aItem);
        cnRemoved: FireOnRemoveRoad(aItem);
        end;
    end;

procedure TTranRoadList.FireOnAddRoad(const aRoad: TTranRoad);
    var
        m: TMethod;
    begin
    if not Assigned(p_OnAddRoadEvent) then
        Exit;

    for m in p_OnAddRoadEvent do
        ChangeRoadProc(m)(aRoad.RoadName, aRoad);
    end;

procedure TTranRoadList.FireOnRemoveRoad(const aRoad: TTranRoad);
    var
        m: TMethod;
    begin
    if not Assigned(p_OnRemoveRoadEvent) then
        Exit;

    for m in p_OnRemoveRoadEvent do
        ChangeRoadProc(m)(aRoad.RoadName, aRoad);
    end;
end.

