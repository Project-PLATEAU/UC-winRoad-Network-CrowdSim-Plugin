unit PedestrianMapGenerator;

interface

uses
    System.Classes,
    System.Generics.Collections,
    System.SyncObjs,
    PluginCore,
    F8Utils,
    F8GLUtils,
    TranRoadList,
    TranRoad,
    crowdSimCityFurnitureList,
    crowdSimCityFurniture,
    PedestrianMap,
    MapOrigin,
    PedestrianUtil;

type
    OnSynchronizeEventProc = procedure(const aProgressRate: Single) of object;
    OnTerminateEventProc = procedure(const aMap: TPedestrianMap) of object;

    /// <summary>
    ///    TPedestrianMapを生成するクラス
    ///    生成処理は独自スレッドで実行する.生成中と生成終了時にイベントを実行できる
    /// </summary>
    TPedestrianMapGenerator = class
        private
            type
            TPedestrianMapGeneratorThread = class(TThread)
                private

                    p_Map: TPedestrianMap;
                    p_Config: TPedestrianMatrixConfig;
                    p_horiConv: IF8WrHorizontalCoordinateConvertor;
                    p_RefRoadList: TTranRoadList;
                    p_RefCityFurnitureList: TCrowdSimCityFurnitureList;

                    p_SynchronizeEvent: OnSynchronizeEventProc;
                    p_TerminateEvent  : OnTerminateEventProc;

                    procedure DoExecute;
                    procedure DoSynchronizeEvent(const aProgressRate: Double);
                    procedure DoTerminateEvent(aSender: TObject);
                protected
                    procedure Execute; override;
                public
                    constructor Create(const aHoriConv: IF8WrHorizontalCoordinateConvertor; const aConfig: TPedestrianMatrixConfig;
                      const aRoadList: TTranRoadList; const aCityFurnitureList: TCrowdSimCityFurnitureList);
                    procedure AfterConstruction; override;
                    procedure BeforeDestruction; override;

                    property  SynchronizeEvent: OnSynchronizeEventProc read p_SynchronizeEvent write p_SynchronizeEvent;
                    property  TerminateEvent  : OnTerminateEventProc   read p_TerminateEvent   write p_TerminateEvent;
                end;

            var
                p_RefRoadList         : TTranRoadList;
                p_RefCityFurnitureList: TCrowdSimCityFurnitureList;
                p_RefCityGMLCS: TCriticalSection;

                p_OnSyncEvents: TList<TMethod>;
                p_OnTerminateEvents: TList<TMethod>;

                p_Thread: TPedestrianMapGeneratorThread;

            procedure FireOnSynchronizeEvent(const aProgressRate: Single);
            procedure FireOnTerminateEvent(const aMap: TPedestrianMap);
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function  Generate(const aAPI: IF8ApplicationServices; const aFirstPointGL, aSecondPointGL: TPoint3D; const aCellSize: Double): Boolean; overload;
            function  Generate(const aAPI: IF8ApplicationServices; const aConfig: TPedestrianMatrixConfig): Boolean; overload;

            procedure AddRoad(const aRoadName: String; const aRoad: TTranRoad);
            procedure RemoveRoad(const aRoadName: String; const aRoad: TTranRoad);
            procedure AddCityFurniture(const aCityFurnitureName: String; const aCityFurniture: TCrowdSimCityFurniture);
            procedure RemoveCityFurniture(const aCityFurnitureName: String; const aCityFurniture: TCrowdSimCityFurniture);

            procedure RegisterOnSynchronizeEvent(const aEvent: OnSynchronizeEventProc);
            procedure UnRegisterOnSynchronizeEvent(const aEvent: OnSynchronizeEventProc);
            procedure RegisterOnTerminateEvent(const aEvent: OnTerminateEventProc);
            procedure UnRegisterOnTerminateEvent(const aEvent: OnTerminateEventProc);
        end;

implementation

uses
    System.SysUtils,
    LatLonHelper;

{ TPedestrianMapGenerator }

procedure TPedestrianMapGenerator.AfterConstruction;
    begin
    inherited;

    p_RefCityGMLCS         := TCriticalSection.Create;
    p_RefRoadList          := TTranRoadList.Create;
    p_RefCityFurnitureList := TCrowdSimCityFurnitureList.Create;
    p_OnSyncEvents      := TList<TMethod>.Create;
    p_OnTerminateEvents := TList<TMethod>.Create;
    end;

procedure TPedestrianMapGenerator.BeforeDestruction;
    begin
    inherited;

    if Assigned(p_Thread) then
        FreeAndNil(p_Thread);

    FreeAndNil(p_OnSyncEvents);
    FreeAndNil(p_OnTerminateEvents);
    FreeAndNil(p_RefRoadList);
    FreeAndNil(p_RefCityFurnitureList);
    FreeAndNil(p_RefCityGMLCS);
    end;

function TPedestrianMapGenerator.Generate(const aAPI: IF8ApplicationServices; const aFirstPointGL, aSecondPointGL: TPoint3D; const aCellSize: Double): Boolean;
    var
        horiConv: IF8WrHorizontalCoordinateConvertor;

    procedure ConvertGLToLatLonAndLocal(const aSrc: TPoint3D; out aLatLon, aLocal: TPoint3D);
        var
            srcPoint, dstPoint: F8PointType;
        begin
        srcPoint[_x] := aSrc.X;
        srcPoint[_y] := aSrc.Z;
        horiConv.Convert(_hctOpenGL_XZ, 0, _hctSpecifiedCS, 6668, srcPoint, dstPoint);
        aLatLon.Lat := dstPoint[_y];
        aLatLon.Lon := dstPoint[_x];
        horiConv.Convert(_hctOpenGL_XZ, 0, _hctLocal_XY, 0, srcPoint, dstPoint);
        aLocal.X := dstPoint[_x];
        aLocal.Z := dstPoint[_y];
        end;
    var
        config: TPedestrianMatrixConfig;
        left, top: Boolean;
        isOk: Boolean;

        firstLatLon, secondLatLon, newLatLon: TPoint3D;
        firstLocal, secondLocal, newLocal: TPoint3D;
        tmp: TPoint3D;
    begin
    Result := False;
    horiConv := aAPI.GetWRCoordinateConvertor.HoirizontalCSConvertor;

    ConvertGLToLatLonAndLocal(aFirstPointGL,  firstLatLon,  firstLocal);
    ConvertGLToLatLonAndLocal(aSecondPointGL, secondLatLon, secondLocal);

    // Localで計算
    left := firstLocal.X < secondLocal.X;
    top  := firstLocal.Z > secondLocal.Z;

    if left then
        begin
        if top then
            begin
            config.LeftTopLatLon := firstLatLon;
            config.CellSize    := aCellSize;
            config.RowCount    := Trunc((firstLocal.Z - secondLocal.Z) / aCellSize);
            config.ColumnCount := Trunc((secondLocal.X - firstLocal.X) / aCellSize);
            isOk := (config.AllCellCount > 0);
            end
        else
            begin
            tmp.Assign(aFirstPointGL.X, 0, aSecondPointGL.Z);
            ConvertGLToLatLonAndLocal(tmp, newLatLon, newLocal);
            config.LeftTopLatLon := newLatLon;
            config.CellSize    := aCellSize;
            config.RowCount    := Trunc((secondLocal.Z - firstLocal.Z) / aCellSize);
            config.ColumnCount := Trunc((secondLocal.X - firstLocal.X) / aCellSize);
            isOk := (config.AllCellCount > 0);
            end;
        end
    else
        begin
        if top then
            begin
            tmp.Assign(aSecondPointGL.X, 0, aFirstPointGL.Z);
            ConvertGLToLatLonAndLocal(tmp, newLatLon, newLocal);
            config.LeftTopLatLon := newLatLon;
            config.CellSize    := aCellSize;
            config.RowCount    := Trunc((firstLocal.Z - secondLocal.Z) / aCellSize);
            config.ColumnCount := Trunc((firstLocal.X - secondLocal.X) / aCellSize);
            isOk := (config.AllCellCount > 0);
            end
        else
            begin
            config.LeftTopLatLon := secondLatLon;
            config.CellSize    := aCellSize;
            config.RowCount    := Trunc((secondLocal.Z - firstLocal.Z) / aCellSize);
            config.ColumnCount := Trunc((firstLocal.X - secondLocal.X) / aCellSize);
            isOk := (config.AllCellCount > 0);
            end;
        end;

    if isOk then
        Result := Generate(aAPI, config);
    end;

function TPedestrianMapGenerator.Generate(const aAPI: IF8ApplicationServices; const aConfig: TPedestrianMatrixConfig): Boolean;
    var
        horiConv: IF8WrHorizontalCoordinateConvertor;
    begin
    Result := False;

    if not Assigned(aAPI.project) then
        Exit;

    if Assigned(p_Thread)then
        FreeAndNil(p_Thread);

    horiConv := aAPI.GetWRCoordinateConvertor.HoirizontalCSConvertor;
    p_Thread := TPedestrianMapGeneratorThread.Create(horiConv, aConfig, p_RefRoadList, p_RefCityFurnitureList);
    p_Thread.SynchronizeEvent := FireOnSynchronizeEvent;
    p_Thread.TerminateEvent := FireOnTerminateEvent;
    p_Thread.Start;

    Result := True;
    end;

procedure TPedestrianMapGenerator.AddRoad(const aRoadName: String; const aRoad: TTranRoad);
    begin
    p_RefCityGMLCS.Enter;
    try
        p_RefRoadList.AddRoad(aRoadName, aRoad.Clone);
    finally
        p_RefCityGMLCS.Leave;
        end;
    end;

procedure TPedestrianMapGenerator.RemoveRoad(const aRoadName: String; const aRoad: TTranRoad);
    begin
    p_RefCityGMLCS.Enter;
    try
        p_RefRoadList.RemoveRoad(aRoadName);
    finally
        p_RefCityGMLCS.Leave;
        end;
    end;

procedure TPedestrianMapGenerator.AddCityFurniture(const aCityFurnitureName: String; const aCityFurniture: TCrowdSimCityFurniture);
    begin
    p_RefCityGMLCS.Enter;
    try
        p_RefCityFurnitureList.AddCityFurniture(aCityFurnitureName, aCityFurniture.Clone);
    finally
        p_RefCityGMLCS.Leave;
        end;
    end;

procedure TPedestrianMapGenerator.RemoveCityFurniture(const aCityFurnitureName: String; const aCityFurniture: TCrowdSimCityFurniture);
    begin
    p_RefCityGMLCS.Enter;
    try
        p_RefCityFurnitureList.RemoveCityFurniture(aCityFurnitureName);
    finally
        p_RefCityGMLCS.Leave;
        end;
    end;

procedure TPedestrianMapGenerator.RegisterOnSynchronizeEvent(const aEvent: OnSynchronizeEventProc);
    var
        m: TMethod;
    begin
    OnSynchronizeEventProc(m) := aEvent;
    if not p_OnSyncEvents.Contains(m) then
        p_OnSyncEvents.Add(m);
    end;

procedure TPedestrianMapGenerator.UnRegisterOnSynchronizeEvent(const aEvent: OnSynchronizeEventProc);
    var
        m: TMethod;
    begin
    OnSynchronizeEventProc(m) := aEvent;
    p_OnSyncEvents.Remove(m);
    end;

procedure TPedestrianMapGenerator.RegisterOnTerminateEvent(const aEvent: OnTerminateEventProc);
    var
        m: TMethod;
    begin
    OnTerminateEventProc(m) := aEvent;
    if not p_OnTerminateEvents.Contains(m) then
        p_OnTerminateEvents.Add(m);
    end;

procedure TPedestrianMapGenerator.UnRegisterOnTerminateEvent(const aEvent: OnTerminateEventProc);
    var
        m: TMethod;
    begin
    OnTerminateEventProc(m) := aEvent;
    p_OnTerminateEvents.Remove(m);
    end;

procedure TPedestrianMapGenerator.FireOnSynchronizeEvent(const aProgressRate: Single);
    var
        m: TMethod;
    begin
    for m in p_OnSyncEvents do
        OnSynchronizeEventProc(m)(aProgressRate);
    end;

procedure TPedestrianMapGenerator.FireOnTerminateEvent(const aMap: TPedestrianMap);
    var
        m: TMethod;
    begin
    if not Assigned(aMap) then
        Exit;

    for m in p_OnTerminateEvents do
        OnTerminateEventProc(m)(aMap);
    end;

{ TPedestrianMapGenerator.TPedestrianMapGeneratorThread }
constructor TPedestrianMapGenerator.TPedestrianMapGeneratorThread.Create(const aHoriConv: IF8WrHorizontalCoordinateConvertor;
  const aConfig: TPedestrianMatrixConfig; const aRoadList: TTranRoadList;  const aCityFurnitureList: TCrowdSimCityFurnitureList);
    begin
    // Start Suspended
    inherited Create(True);

    p_HoriConv := aHoriConv;
    p_Config := aConfig;
    p_RefRoadList := aRoadList;
    p_RefCityFurnitureList := aCityFurnitureList;
    p_Map := nil;
    end;

procedure TPedestrianMapGenerator.TPedestrianMapGeneratorThread.AfterConstruction;
    begin
    inherited;

    FreeOnTerminate := False;

    // Register Event
    OnTerminate := DoTerminateEvent;
    end;

procedure TPedestrianMapGenerator.TPedestrianMapGeneratorThread.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_Map);
    p_RefRoadList := nil;
    p_RefCityFurnitureList := nil;
    p_SynchronizeEvent := nil;
    p_TerminateEvent   := nil;
    end;

procedure TPedestrianMapGenerator.TPedestrianMapGeneratorThread.Execute;
    begin
    DoExecute;
    end;

procedure TPedestrianMapGenerator.TPedestrianMapGeneratorThread.DoSynchronizeEvent(const aProgressRate: Double);
    begin
    if Assigned(p_SynchronizeEvent) then
        p_SynchronizeEvent(aProgressRate);
    end;

procedure TPedestrianMapGenerator.TPedestrianMapGeneratorThread.DoExecute;
    var
        allStepCount: Integer;
        stepCount: Integer;
        i: Integer;
        cityGMLItemNameList: TStringList;
        cityGMLItemName: String;
    begin

    // Create + overlayRoad
    allStepCount := 1 + p_RefRoadList.RoadCount + p_RefCityFurnitureList.CityFurnitureCount;
    stepCount := 0;
    Synchronize(procedure begin DoSynchronizeEvent(stepCount / allStepCount) end);

    if Assigned(p_Map) then
        FreeAndNil(p_Map);

    p_Map := TPedestrianMap.Create(p_horiConv, p_Config);
    Inc(stepCount);
    Synchronize(procedure begin DoSynchronizeEvent(stepCount / allStepCount) end);

    cityGMLItemNameList := TStringList.Create;
    try
        p_RefRoadList.ExtractAllRoadID(cityGMLItemNameList);
        for i := 0 to cityGMLItemNameList.Count- 1 do
            begin
            cityGMLItemName := cityGMLItemNameList[i];
            p_Map.OverlayRoad(p_HoriConv, p_RefRoadList.Road[cityGMLItemName]);
            Inc(stepCount);
            Synchronize(procedure begin DoSynchronizeEvent(stepCount / allStepCount) end);
            end;
        cityGMLItemNameList.Clear;
        p_RefCityFurnitureList.ExtractAllCityFurnitureID(cityGMLItemNameList);
        for i := 0 to cityGMLItemNameList.Count - 1 do
            begin
            cityGMLItemName := cityGMLItemNameList[i];
            p_Map.OverlayCityFurniture(p_horiConv, p_RefCityFurnitureList.CityFurniture[cityGMLItemName]);
            Inc(stepCount);
            Synchronize(procedure begin DoSynchronizeEvent(stepCount / allStepCount) end);
            end;

    finally
        FreeAndNil(cityGMLItemNameList);
        end;
    end;

procedure TPedestrianMapGenerator.TPedestrianMapGeneratorThread.DoTerminateEvent(aSender: TObject);
    begin
    if Assigned(p_TerminateEvent) then
        p_TerminateEvent(p_Map);

    FreeAndNil(p_Map);
    end;
end.
