unit crowdSimCityFurniture;

interface

uses
    XML.XMLIntf,
    CityFurniture,
    geometryBasic0d1d,
    geometryBasic2d,
    geometryAggregates,
    F8OpenGL,
    MultiSurface,
    PedestrianUtil;

type
    TCityFurnitureFunctionType = (
        _cffRoadMarkings, // 道路標示
        _cffLotLine, // 区画線
        _cffCenterLine, // 車道中央線
        _cffRoadBoundaryLine, // 車道境界線
        _cffRoadOutsideLine, // 車道外側線
        _cffInstructionDisplay, // 指示表示
        _cffPedestrianCrossing, // 横断歩道
        _cffStopLine, // 停止線
        _cffRegulatoryInformation, // 規制表示
        _cffFence, // 柵・壁
        _cffRoadSign, // 道路標識
        _cffGuideSigns, // 案内標識
        _cffWarningSign, // 警戒標識
        _cffRegulatorySigns, // 規制標識
        _cffInstructionalSign, // 指示標識
        _cffAuxiliarySign, // 補助標識
        _cffBuilding, // 建造物
        _cffShed, // 上屋
        _cffUndergroundEntrance, // 地下出入口
        _cffAcade, // アーケード
        _cffLineOfSightGuide, // 視線誘導標
        _cffRoadRefrector, // 道路反射鏡
        _cffLightingFacility, // 照明施設
        _cffTombstone, // 墓碑
        _cffMonument, // 記念碑
        _cffStatue, // 立像
        _cffRoadsideShrine, // 路傍祠
        _cffLantern, // 灯ろう
        _cffTorii, // 鳥居
        _cffNaturalDisasterMemorialMonument, // 自然災害伝承碑
        _cffFountain, // 噴水
        _cffWell, // 井戸
        _cffOilWell, // 油井・ガス井
        _cffHoist, // 起重機
        _cffTank, // タンク
        _cffChimney, // 煙突
        _cffHighTower, // 高塔
        _cffRadioTower, // 電波塔
        _cffWindmill, // 風車
        _cffLighthouse, // 灯台
        _cffBeacon, // 灯標
        _cffHeliport, // ヘリポート
        _cffWaterLevelObservationStation, // 水位観測所
        _cffTrafficInfoHub, // 道路情報管理施設
        _cffDisasterDetector, // 災害検知器
        _cffWeatherSensor, // 気象観測装置
        _cffInfoSign, // 道路情報板
        _cffFiberOptic, // 光ファイバー
        _cffPole, // 柱
        _cffRoadside, // 路側
        _cffSinglePost, // 片持
        _cffGateType, // 門型
        _cffUtilityPole, // 電柱
        _cffTrafficLight, // 交通信号機
        _cffStairs, // 階段
        _cffCorridor, // 通路
        _cffElevator, // エレベータ
        _cffEscalator, // エスカレータ
        _cffGroundControl, // 管理用地上施設
        _cffPowerTrench, // 電線共同溝
        _cffCableBox, // CAB
        _cffInfoBox, // 情報BOX
        _cffPipeLane, // 管路
        _cffAccessOpening, // 管理用開口部
        _cffManhole, // マンホール
        _cffHandhole, // ハンドホール
        _cffInletHole, // 入孔
        _cffDistanceMarker, // 距離標
        _cffBoundarySign, // 境界標識
        _cffMilepost, // 道路元標・里程標
        _cffTollBooth, // 料金徴収施設
        _cffSnowMelt, // 融雪施設
        _cffDrainageSystem, // 排水施設
        _cffCollectionPit, // 集水桝
        _cffDrainTrench, // 排水溝
        _cffSideDitch, // 側溝
        _cffDrainPipe, // 排水管
        _cffDrainPump, // 排水ポンプ
        _cffBusStop, // 停留所
        _cffFireHydrant, // 消火栓
        _cffMailbox, // 郵便ポスト
        _cffPhoneBox, // 電話ボックス
        _cffTransportPipe, // 輸送管
        _cffTrack, // 軌道
        _cffOverheadLine, // 架空線
        _cffVendingMachine, // 自動販売機
        _cffBulletinBoard, // 掲示板
        _cffBrailleBlocks, // 点字ブロック
        _cffBench, // ベンチ
        _cffTable, // テーブル
        _cffOther, // その他
        _cffSignborrd, // 看板
        _cffWaterStation // 水飲み
    );
    TCityFurnitureFunctionTypeSet = set of TCityFurnitureFunctionType;
    TCityFurnitureFunctionTypeValue = array[Low(TCityFurnitureFunctionType) .. High(TCityFurnitureFunctionType)] of Integer;

    ///    都市設備モデル
    ///    瀬谷駅データにはLOD3しかないのでとりあえずLOD3だけ考える
    TCrowdSimCityFurniture = class
        private
            p_Name: String; // gml:id
            p_FunctionSet: TCityFurnitureFunctionTypeSet;
            p_IsPassThrough: Boolean;

            p_Exterior    : TMultiSurface;
            p_BoundingArea: VertexPositions;

            procedure MakeBoundingArea;
            constructor Create; overload;
        public
            constructor Create(const aCityFurniture: IXMLCityFurnitureType); overload;
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function  Clone: TCrowdSimCityFurniture;
            procedure DoRender(const aOpenGL: TF8OpenGL);
            function  IsOverlayPedestrianArea(const aArea: VertexPositions): Boolean;
            function  IsOverlayBoundingArea(const aArea: VertexPositions): Boolean;

            property  Name         : String                        read p_Name;
            property  _Function    : TCityFurnitureFunctionTypeSet read p_FunctionSet;
            property  IsPassThrough: Boolean                       read p_IsPassThrough;
            property  BoundingArea : VertexPositions               read p_BoundingArea;
        end;

    TCrowdSimCityFurniturePluginData = class
        strict private
            const
                NODE_NAME_CITY_FURNITURE = 'CityFurniture';
                NODE_NAME_BOUNDING_AREA  = 'BoundingArea';
                NODE_NAME_FUNCTION_ID = 'FunctionID';
                TAG_NAME = 'Name';
                TAG_FUNCTION_ID_COUNT = 'FunctionIDCount';
                TAG_FUNCTION_ID       = 'FunctionID_';
        public
            class procedure ExportToPluginData(const aCityFurniture: TCrowdSimCityFurniture; const aParentNode: IXMLNode);
            class function  ImportFromPluginData(const aDataNode: IXMLNode; out aRes: TCrowdSimCityFurniture): Boolean;
        end;

const
    ALL_CITY_FURNITURE_FUNCTIONS: TCityFurnitureFunctionTypeSet = [Low(TCityFurnitureFunctionType) .. High(TCityFurnitureFunctionType)];
    CityFurnitureFunction_Values:TCityFurnitureFunctionTypeValue = (
    1000, 1010, 1020, 1030, 1040, 1100, 1110, 1120, 1200,
    2000,
    3000, 3110, 3120, 3130, 3140, 3150,
    4000, 4010, 4020, 4030,
      4100, 4120,
      4200, 4201, 4202, 4203, 4204, 4205, 4207, 4208, 4223, 4224, 4225, 4228, 4231, 4234, 4235, 4236, 4239, 4241, 4243, 4245, 4251,
      4300,
      4400,
      4500,
      4600,
      4700,
      4800, 4810, 4820, 4830, 4840,
      4900,
    5000, 5010, 5020, 5030, 5100, 5200, 5300, 5400, 5500, 5600, 5610, 5620, 5630,
    6000, 6010, 6020, 6100, 6200,
    7000, 7100, 7200, 7300, 7400, 7500,
    8010, 8020, 8030, 8040, 8050, 8060, 8070, 8080, 8140, 8150, 8160, 8170,
    9000, 9001, 9002);
    // 構造物上を歩行可能とするアイテム
    CITY_FURNITURE_PASS_THROUGH_STRUCTURES: TCityFurnitureFunctionTypeSet = [
        _cffRoadMarkings, // 道路標示
        _cffLotLine, // 区画線
        _cffCenterLine, // 車道中央線
        _cffRoadBoundaryLine, // 車道境界線
        _cffRoadOutsideLine, // 車道外側線
        _cffInstructionDisplay, // 指示表示
        _cffPedestrianCrossing, // 横断歩道
        _cffStopLine, // 停止線
        _cffRegulatoryInformation, // 規制表示
        _cffUndergroundEntrance, // 地下出入口
        _cffRoadside, // 路側
        _cffSinglePost, // 片持
        _cffGateType, // 門型
        _cffStairs, // 階段
        _cffCorridor, // 通路
        _cffElevator, // エレベータ
        _cffEscalator, // エスカレータ
        _cffPowerTrench, // 電線共同溝
        _cffCableBox, // CAB
        _cffInfoBox, // 情報BOX
        _cffPipeLane, // 管路
        _cffAccessOpening, // 管理用開口部
        _cffManhole, // マンホール
        _cffHandhole, // ハンドホール
        _cffInletHole, // 入孔
        _cffDrainTrench, // 排水溝
        _cffSideDitch, // 側溝
        _cffDrainPipe, // 排水管
        _cffDrainPump, // 排水ポンプ
        _cffTransportPipe, // 輸送管
        _cffTrack, // 軌道
        _cffOverheadLine, // 架空線
        _cffBrailleBlocks // 点字ブロック
    ];

implementation

uses
    System.SysUtils,
    System.Math,
    F8Utils,
    basicTypes,
    XMLPointType,
    LatLonHelper;

{ TCrowdSimCityFurniture }
constructor TCrowdSimCityFurniture.Create;
    begin
    p_Name          := '';
    p_FunctionSet   := [];
    p_IsPassThrough := True;
    p_Exterior := TMultiSurface.Create;
    end;

constructor TCrowdSimCityFurniture.Create(const aCityFurniture: IXMLCityFurnitureType);
    function  GetFunction(const aFunc: IXMLCodeTypeList): TCityFurnitureFunctionTypeSet;
        var
            func: IXMLCodeType;
            funcNum: Integer;
            i: Integer;
            iFunc: TCityFurnitureFunctionType;
        begin
        Result := [];
        for i := 0 to aFunc.Count - 1 do
            begin
            func := aFunc[i];
            if not Assigned(func) then
                Break;

            if not func.IsTextElement then
                Continue;

            funcNum := StrToIntDef(func.Text, 0);
            for iFunc in ALL_CITY_FURNITURE_FUNCTIONS do
                begin
                if funcNum = CityFurnitureFunction_Values[iFunc] then
                    Result := Result + [iFunc];
                end;
            end;
        end;
    var
        func: IXMLCodeTypeList;
        exterior: IXMLGeometryPropertyType;
        exteriorAbstract: IXMLAbstractGeometryType;
        exteriorMultiSurface: IXMLMultiSurfaceType;
    begin
    p_Exterior := TMultiSurface.Create;

    if not Assigned(aCityFurniture) then
        Exit;

    p_Name := aCityFurniture.Id;

    func := aCityFurniture.Function_;
    if not Assigned(func) then
        Exit;

    p_FunctionSet   := GetFunction(func);
    p_IsPassThrough := (p_FunctionSet * CITY_FURNITURE_PASS_THROUGH_STRUCTURES) <> [];

    // まずはLOD3限定
    exterior := aCityFurniture.Lod3Geometry;
    if not Assigned(exterior) then
        Exit;

    exteriorAbstract := exterior._Geometry;
    if not Assigned(exteriorAbstract) then
        Exit;

    if not Supports(exteriorAbstract, IXMLMultiSurfaceType, exteriorMultiSurface) then
        Exit;

    p_Exterior.ImportCityGML(exteriorMultiSurface);
    MakeBoundingArea;
    end;

procedure TCrowdSimCityFurniture.AfterConstruction;
    begin
    inherited;
    {no action}
    end;

procedure TCrowdSimCityFurniture.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_Exterior);
    end;

function TCrowdSimCityFurniture.Clone: TCrowdSimCityFurniture;
    var
        i: VertexPositionType;
    begin
    Result := TCrowdSimCityFurniture.Create;
    Result.p_Name          := p_Name;
    Result.p_FunctionSet   := p_FunctionSet;
    Result.p_IsPassThrough := p_IsPassThrough;
    Result.p_Exterior      := p_Exterior.Clone;

    for i in ALL_VERTEX_POSITION do
        Result.p_BoundingArea[i] := p_BoundingArea[i];
    end;

procedure TCrowdSimCityFurniture.DoRender(const aOpenGL: TF8OpenGL);
    begin
    p_Exterior.DoRender(aOpenGL);
    end;

function TCrowdSimCityFurniture.IsOverlayPedestrianArea(const aArea: VertexPositions): Boolean;
    begin
    Result := (not IsPassThrough) and  IsOverlayBoundingArea(aArea) and p_Exterior.IsOverlappedMultiSurface(aArea);
    end;

function TCrowdSimCityFurniture.IsOverlayBoundingArea(const aArea: VertexPositions): Boolean;
    begin
    Result := IsOverlay(p_BoundingArea, aArea);
    end;

procedure TCrowdSimCityFurniture.MakeBoundingArea;
    var
        edge: TParametricLine2D;
        minLatLon, maxLatLon: TPoint3D;
        i, j: Integer;
    begin
    if not Assigned(p_Exterior) then
        Exit;

    minLatLon := Point3D(MaxDouble, MaxDouble, MaxDouble);
    maxLatLon := Point3D(MinDouble, MinDouble, MinDouble);
    // TMultiSurface.GenerateOutLineで正しく外形線を取れないので地道にBoundingBoxを作成することにする
    for i := 0 to p_Exterior.SurfaceCount - 1 do
        begin
        for j := 0 to p_Exterior.Surface[i].EdgeCount - 1 do
            begin
            edge := p_Exterior.Surface[i].Edge[j];

            minLatLon.Lat := Min(minLatLon.Lat, Min(edge.a.Lat, edge.b.Lat));
            minLatLon.Lon := Min(minLatLon.Lon, Min(edge.a.Lon, edge.b.Lon));
            maxLatLon.Lat := Max(maxLatLon.Lat, Max(edge.a.Lat, edge.b.Lat));
            maxLatLon.Lon := Max(maxLatLon.Lon, Max(edge.a.Lon, edge.b.Lon));
            end;
        end;

    p_BoundingArea[VertexPositionType._LeftTop].Lat := maxLatLon.Lat;
    p_BoundingArea[VertexPositionType._LeftTop].Lon := minLatLon.Lon;

    p_BoundingArea[VertexPositionType._RightTop].Lat := maxLatLon.Lat;
    p_BoundingArea[VertexPositionType._RightTop].Lon := maxLatLon.Lon;

    p_BoundingArea[VertexPositionType._RightBottom].Lat := minLatLon.Lat;
    p_BoundingArea[VertexPositionType._RightBottom].Lon := maxLatLon.Lon;

    p_BoundingArea[VertexPositionType._LeftBottom].Lat := minLatLon.Lat;
    p_BoundingArea[VertexPositionType._LeftBottom].Lon := minLatLon.Lon;
    end;

{ TCrowdSimCityFurniturePluginData }
class procedure TCrowdSimCityFurniturePluginData.ExportToPluginData(const aCityFurniture: TCrowdSimCityFurniture; const aParentNode: IXMLNode);
    var
        headNode, functionIDNode: IXMLNode;
        count: Integer;
        f: TCityFurnitureFunctionType;
    begin
    headnode := aParentNode.AddChild(NODE_NAME_CITY_FURNITURE);
    headNode.Attributes[TAG_NAME] := aCityFurniture.Name;

    functionIDNode := headNode.AddChild(NODE_NAME_FUNCTION_ID);
    count := 0;
    for f in aCityFurniture._Function do
        begin
        functionIDNode.Attributes[Format('%s%d', [TAG_FUNCTION_ID, count])] := IntToStr(CityFurnitureFunction_Values[f]);
        Inc(count);
        end;
    functionIDNode.Attributes[TAG_FUNCTION_ID_COUNT] := IntToStr(count);

    aCityFurniture.p_Exterior.ExportToPluginData(headNode);
    TVertexPositionsXMLType.ExportToXML(aCityFurniture.p_BoundingArea, NODE_NAME_BOUNDING_AREA, headNode);
    end;

class function TCrowdSimCityFurniturePluginData.ImportFromPluginData(const aDataNode: IXMLNode; out aRes: TCrowdSimCityFurniture): Boolean;
    var
        headNode, functionIDNode, boundingAreaNode: IXMLNode;
        i: Integer;
        tmpID: Integer;
        iID: TCityFurnitureFunctionType;
        tmpName: String;
        tmpFunctionIDSet: TCityFurnitureFunctionTypeSet;
        tmpFunctionIDCount: Integer;
        tmpBoundingArea: VertexPositions;
    begin
    Result := False;
    headNode := aDataNode.ChildNodes.FindNode(NODE_NAME_CITY_FURNITURE);
    if not Assigned(headNode) then
        Exit;

    if not headNode.HasAttribute(TAG_NAME) then
        Exit;

    tmpName := headNode.Attributes[TAG_NAME];

    functionIDNode := headNode.ChildNodes.FindNode(NODE_NAME_FUNCTION_ID);
    if not Assigned(functionIDNode) then
        Exit;

    tmpFunctionIDSet := [];
    if not functionIDNode.HasAttribute(TAG_FUNCTION_ID_COUNT) then
        Exit;

    tmpFunctionIDCount := StrToIntDef(functionIDNode.Attributes[TAG_FUNCTION_ID_COUNT], 0);
    for i := 0 to tmpFunctionIDCount - 1 do
        begin
        if not functionIDNode.HasAttribute(Format('%s%d', [TAG_FUNCTION_ID, i])) then
            Exit;

        tmpID := StrToIntDef(functionIDNode.Attributes[Format('%s%d', [TAG_FUNCTION_ID, i])], 0);
        for iID in ALL_CITY_FURNITURE_FUNCTIONS do
            begin
            if tmpID = CityFurnitureFunction_Values[iID] then
                begin
                tmpFunctionIDSet := tmpFunctionIDSet + [iID];
                Break;
                end;
            end;
        end;

    boundingAreaNode := headNode.ChildNodes.FindNode(NODE_NAME_BOUNDING_AREA);
    if not Assigned(boundingAreaNode) then
        Exit;

    if not TVertexPositionsXMLType.ImportFromXML(boundingAreaNode, tmpBoundingArea) then
        Exit;

    aRes := TCrowdSimCityFurniture.Create;
    aRes.p_Name := tmpName;

    if not aRes.p_Exterior.ImportFromPluginData(headNode) then
        Exit;

    aRes.p_BoundingArea := tmpBoundingArea;
    aRes.p_FunctionSet := tmpFunctionIDSet;
    aRes.p_IsPassThrough := (CITY_FURNITURE_PASS_THROUGH_STRUCTURES * aRes.p_FunctionSet) <> [];
    Result := True;
    end;
end.

