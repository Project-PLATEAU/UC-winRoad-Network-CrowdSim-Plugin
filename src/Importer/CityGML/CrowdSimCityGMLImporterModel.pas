unit CrowdSimCityGMLImporterModel;

interface

uses
    PluginCore,
    CityGMLBase,
    Xml.XMLIntf,
    F8OpenGL;

type
    TCrowdSimCityGMLModelType = (
        _cscgRoad,
        _cscgBuilding,
        _cscgBridge,
        _cscgCityFurniture
        );

    TCrowdSimCityGMLImporterModel = class
        protected
            function  GetModelType: TCrowdSimCityGMLModelType; virtual; abstract;
            procedure OnAfterImportModel(const aObj: IXMLAbstractCityObjectType); virtual; abstract;
            procedure DoExportToPluginData(const aRootNode: IXMLNode); virtual; abstract;
            procedure DoImportFromPluginData(const aRootNode: IXMLNode); virtual; abstract;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure ExportToPluginData(const aProject: IF8ProjectForRoad);
            procedure ImportFromPluginData(const aProject: IF8ProjectForRoad);

            procedure DoRender(const aOpenGL: TF8OpenGL); virtual;

            property  ModelType: TCrowdSimCityGMLModelType read GetModelType;
        end;

implementation

uses
    System.SysUtils,
    XML.XMLDoc,
    XML.xmldom,
    F8CrowdSimPluginDataConstant;

const
    SAVE_POINT_NAME: array[Low(TCrowdSimCityGMLModelType)..High(TCrowdSimCityGMLModelType)] of String = (
        TRAN_ROAD_LIST_PLUGIN_DATA,
        BUILDING_LIST_PLUGIN_DATA,
        'BridgePluginData',
        CITY_FURNITURE_LIST_PLUGIN_DATA
        );
    SAVE_XML_NODE_ROOT_NAME: array[Low(TCrowdSimCityGMLModelType)..High(TCrowdSimCityGMLModelType)] of String = (
        'TranRoadList',
        'CrowdSimBuildingList',
        'CrowdSimBridgeList',
        'CrowdSimCityFurnitureList'
    );

{ TCrowdSimCityGMLImporterModel }
procedure TCrowdSimCityGMLImporterModel.AfterConstruction;
    var
        api: IF8ApplicationServices;
        citygmlPlugin: IF8CityGMLPlugin;
    begin
    inherited;

    api := theApplicationServices;
    if api.GetPlugInThatSupports(IF8CityGMLPlugin, citygmlPlugin) then
        begin
        case ModelType of
            _cscgRoad         : citygmlPlugin.RegisterOnAfterImportRoadEvent(OnAfterImportModel);
            _cscgBuilding     : citygmlPlugin.RegisterOnAfterImportBuildingEvent(OnAfterImportModel);
            _cscgBridge       : citygmlPlugin.RegisterOnAfterImportBridgeEvent(OnAfterImportModel);
            _cscgCityFurniture: citygmlPlugin.RegisterOnAfterImportCityFurnitureEvent(OnAfterImportModel);
            else
                ;
            end;
        end;
    end;

procedure TCrowdSimCityGMLImporterModel.BeforeDestruction;
    var
        api: IF8ApplicationServices;
        citygmlPlugin: IF8CityGMLPlugin;
    begin
    inherited;
    api := theApplicationServices;
    if api.GetPlugInThatSupports(IF8CityGMLPlugin, citygmlPlugin) then
        begin
        case ModelType of
            _cscgRoad         : citygmlPlugin.UnRegisterOnAfterImportRoadEvent(OnAfterImportModel);
            _cscgBuilding     : citygmlPlugin.UnRegisterOnAfterImportBuildingEvent(OnAfterImportModel);
            _cscgBridge       : citygmlPlugin.UnRegisterOnAfterImportBridgeEvent(OnAfterImportModel);
            _cscgCityFurniture: citygmlPlugin.UnRegisterOnAfterImportCityFurnitureEvent(OnAfterImportModel);
            else
                ;
            end;
        end;
    end;

procedure TCrowdSimCityGMLImporterModel.ExportToPluginData(const aProject: IF8ProjectForRoad);
    var
        xmlDoc: IXMLDocument;
        rootNode: IXMLNode;
        saveText, tmpText: String;
        i: Integer;
    begin
    if not Assigned(aProject) then
        Exit;

    aProject.DeletePluginData(F8_CROWD_SIM_PLUGIN_ID, SAVE_POINT_NAME[ModelType]);

    xmlDoc := NewXMLDocument;
    rootNode := xmlDoc.AddChild(SAVE_XML_NODE_ROOT_NAME[ModelType]);
    DoExportToPluginData(rootNode);

    saveText := '';
    for i := 0 to xmlDoc.XML.Count - 1 do
        begin
        tmpText := Trim(xmlDoc.XML[i]);
        saveText := saveText + tmpText;
        end;

    aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, SAVE_POINT_NAME[ModelType]] := saveText;
    end;

procedure TCrowdSimCityGMLImporterModel.ImportFromPluginData(const aProject: IF8ProjectForRoad);
    var
        savedText: String;
        xmlDoc: IXMLDocument;
        rootNode: IXMLNode;
    begin
    if not Assigned(aProject) then
        Exit;

    savedText := aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, SAVE_POINT_NAME[ModelType]];
    if savedText = '' then
        Exit;

    xmlDoc := NewXMLDocument;
    xmlDoc.LoadFromXML(savedText);

    if xmlDoc.IsEmptyDoc then
        Exit;

    if xmlDoc.ChildNodes.Count < 0 then
        Exit;

    rootNode := xmlDoc.ChildNodes.FindNode(SAVE_XML_NODE_ROOT_NAME[ModelType]);
    if not Assigned(rootNode) then
        Exit;
    DoImportFromPluginData(rootNode);
    end;

procedure TCrowdSimCityGMLImporterModel.DoRender(const aOpenGL: TF8OpenGL);
    begin
    {no action}
    end;
end.
