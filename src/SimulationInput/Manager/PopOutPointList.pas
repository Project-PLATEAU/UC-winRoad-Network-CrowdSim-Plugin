unit PopOutPointList;

interface

uses
    System.Generics.Collections,
    F8OpenGL,
    GL,
    F8GLUtils,
    PluginCore,
    SimulationInputUtils,
    PopOutSchedule;

type
    /// <summary>
    ///    人流の発生設定データを管理するクラス
    /// </summary>
    TPedestrianData = class
        private
            {PedDetail}
            p_IsActive               : boolean;
            p_LabelName              : string;
            p_PopNum                 : integer;
            p_PopPer                 : integer;
            p_AttrPerList            : TList<integer>;
            p_DestPerList            : TList<double>;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure AddDestPer;
            procedure InsertDestPer(const Idx: integer);
            procedure DeleteDestPer(const Idx: integer);
            procedure SetNewDestPer(const Idx: integer; aValue: double);
            procedure SetNewDestPers(const aValue: TList<double>);

            {PedDetail}
            property  IsActive               : boolean read p_IsActive write p_IsActive;
            property  LabelName              : string  read p_LabelName write p_LabelName;
            property  PopNum                 : integer read p_PopNum write p_PopNum;
            property  PopPer                 : integer read p_PopPer write p_PopPer;
            property  AttrPerList            : TList<integer> read p_AttrPerList write p_AttrPerList;
            property  DestPerList            : TList<double> read p_DestPerList write p_DestPerList;
        end;

    /// <summary>
    ///    人流発生・退出地点ごとのデータを管理するクラス
    /// </summary>
    TPopOutPoint = class
        private
            p_Name        : string;
            p_PointType   : PopOutPointType;
            p_PointAttr   : PointAttrType;
            p_NodeIdx     : TNodeNumArray;
            p_PedDataList : TList<TPedestrianData>;
            p_DestList    : TList<string>;

            p_IsAssignedDispersionPoint : boolean;
            p_IsFocused              : boolean;
            p_DispersionPointList : TList<GLPointType>;

            {PopPoint}
            p_PopSchedule      : TPedestrianPopOutSchedule;
            p_PopRule          : PopRuleType;
            p_PopInterval      : integer;
            p_PopNumOnInterval : integer;
            p_PopPerHour       : integer;
            p_PopFrequency     : integer;
            p_PopDetailArray   : TPopDetailArray;

            {OutPoint}
            p_OutRule          : OutRuleType;
            p_OutInterval      : integer;
            p_OutNumOnInterval : integer;
            p_OutSchedule      : TPedestrianPopOutSchedule;

            p_BusModel     : IF8ThreeDeeStudio;
            p_BusRoadIndex : integer;
        public
            constructor Create(const aName: string; const aType: PopOutPointType = PopOutPointType._None);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure AddNewPedData;
            procedure AddPedData(const aData: TPedestrianData);
            procedure DeletePedData(const Idx: integer);
            procedure AddDestination(const aName: string);
            procedure InsertDestination(const Idx: integer; aName: string);
            procedure DeleteDestination(const Idx: integer);
            procedure ClearDestination;
            procedure SetNewDetailPer(const aRow, aValue: integer);

            property  Name            : string read p_Name write p_Name;
            property  PointType       : PopOutPointType read p_PointType write p_PointType;
            property  PointAttr       : PointAttrType read p_PointAttr write p_PointAttr;
            property  NodeIdx         : TNodeNumArray read p_NodeIdx write p_NodeIdx;
            property  PedestrianData  : TList<TPedestrianData> read p_PedDataList;
            property  DestinationList : TList<string>  read p_DestList write p_DestList;

            property  IsAssignedDispersionPoint      : boolean     read p_IsAssignedDispersionPoint write p_IsAssignedDispersionPoint;
            property  IsFocused              : boolean     read p_IsFocused         write p_IsFocused;
            property  DispersionPointList : TList<GLPointType> read p_DispersionPointList write p_DispersionPointList;

            {PopPoint}
            property  PopSchedule      : TPedestrianPopOutSchedule read p_PopSchedule write p_PopSchedule;
            property  PopRule          : PopRuleType read p_PopRule write p_PopRule;
            property  PopInterval      : integer read p_PopInterval write p_PopInterval;
            property  PopNumOnInterval : integer read p_PopNumOnInterval write p_PopNumOnInterval;
            property  PopPerHour       : integer read p_PopPerHour write p_PopPerHour;
            property  PopFrequency     : integer read p_PopFrequency write p_PopFrequency;
            property  PopDetailArray   : TPopDetailArray read p_PopDetailArray write p_PopDetailArray;

            {OutPoint}
            property  OutRule          : OutRuleType read p_OutRule write p_OutRule;
            property  OutInterval      : integer read p_OutInterval write p_OutInterval;
            property  OutNumOnInterval : integer read p_OutNumOnInterval write p_OutNumOnInterval;
            property  OutSchedule      : TPedestrianPopOutSchedule read p_OutSchedule write p_OutSchedule;

            property  BusModel     : IF8ThreeDeeStudio read p_BusModel write p_BusModel;
            property  BusRoadIndex : integer           read p_BusRoadIndex write p_BusRoadIndex;
        end;

    /// <summary>
    ///    全ての人流発生・退出地点のデータを管理するクラス
    /// </summary>
    TPopOutPointList = class
        private
            p_List : TList<TPopOutPoint>;
            p_IsVisibleDispersionPoint : boolean;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure DeleteData(const Idx: integer);
            procedure AddNewPedestrianData(const LblCount: integer; LblName: string = '');
            procedure RenderDP(const opengl: TF8OpenGL);
            procedure ReceiveDispersionPointData;

            property  Data : TList<TPopOutPoint> read p_List;
            property  IsVisibleDispersionPoint : boolean read p_IsVisibleDispersionPoint write p_IsVisibleDispersionPoint;
        end;

implementation

uses
    System.SysUtils,
    System.Math;

const
    DP_POLYGON_COLOR         : GLPointType = (0.96, 0.74, 0.56, 0.7);
    DP_FOCUSED_POLYGON_COLOR : GLPointType = (0.96, 0.30, 0.12, 0.7);
    DP_LINE_COLOR            : GLPointType = (0.21, 0.18, 0.92, 1.0);
    HEIGHT_OFFSET = 0.2;

{ TPedestrianData }
procedure TPedestrianData.AfterConstruction;
    begin
    inherited;

    p_IsActive    := false;
    p_PopNum      := 10;
    p_AttrPerList := TList<integer>.Create;
    p_DestPerList := TList<double>.Create;
    end;

procedure TPedestrianData.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_AttrPerList);
    FreeAndNil(p_DestPerList);
    end;

procedure TPedestrianData.AddDestPer;
    begin
    p_DestPerList.Add(100);
    end;

procedure TPedestrianData.InsertDestPer(const Idx: integer);
    begin
    p_DestPerList.Insert(Idx, 100);
    end;

procedure TPedestrianData.DeleteDestPer(const Idx: integer);
    begin
    p_DestPerList.Delete(Idx);
    end;

procedure TPedestrianData.SetNewDestPer(const Idx: integer; aValue: double);
    begin
    p_DestPerList[Idx] := aValue;
    end;

procedure TPedestrianData.SetNewDestPers(const aValue: TList<double>);
    var
        i, FixedN, FixedNSum : integer;
        PerSum : double;
        FixedPopNums : TList<integer>;
        tmpRoundMode: TRoundingMode;
    begin
    FixedPopNums := TList<integer>.Create;
    PerSum := 0;
    for i := 0 to aValue.Count - 1 do
        PerSum := PerSum + aValue[i];

    if PerSum = 0 then
        PerSum := 1;

    FixedNSum := 0;
    tmpRoundMode := GetRoundMode;
    try
        SetRoundMode(rmTruncate);
        for i := 0 to aValue.Count - 1 do
            begin
            FixedN := Round(SimpleRoundTo((PopNum * (aValue[i] / PerSum)), 0));
            FixedPopNums.Add(FixedN);
            FixedNSum := FixedNSum + FixedN;
            end;

        i := 0;
        while FixedNSum <> PopNum do
            begin
            if FixedNSum > PopNum then
                begin
                if FixedPopNums[i] > 1 then
                    begin
                    FixedPopNums[i] := FixedPopNums[i] - 1;
                    FixedNSum := FixedNSum - 1;
                    end
                else
                    begin
                    FixedPopNums[i] := FixedPopNums[i] + 1;
                    FixedNSum := FixedNSum + 1;
                    end;
                end
            else
                begin
                FixedPopNums[i] := FixedPopNums[i] + 1;
                FixedNSum := FixedNSum + 1;
                end;

            if i = FixedPopNums.Count - 1 then
                i := 0
            else
                i := i + 1;
            end;

        for i := 0 to aValue.Count - 1 do
            begin
            aValue[i] := (FixedPopNums[i] / PopNum) * 100;
            p_DestPerList[i] := aValue[i];
            end;
    finally
        SetRoundMode(tmpRoundMode);
        end;
    FreeAndNil(FixedPopNums);
    end;

{ TPopOutPoint }
constructor TPopOutPoint.Create(const aName: string; const aType: PopOutPointType = PopOutPointType._None);
    begin
    p_Name      := aName;
    p_PointType := aType;
    p_OutRule   := OutRuleType._NoRule;
    end;

procedure TPopOutPoint.AfterConstruction;

    procedure InitializePopData;
        var
            i : integer;
        begin
        p_PopSchedule      := TPedestrianPopOutSchedule.Create;
        p_PopRule          := PopRuleType._PopbyInterval;
        p_PopInterval      := 300;
        p_PopNumOnInterval := 10;
        p_PopPerHour       := 600;
        p_PopFrequency     := 60;
        for i := 0 to 23 do
            p_PopDetailArray[i]  := 100;
        end;

    procedure InitializeOutData;
        begin
        p_OutRule          := OutRuleType._NoRule;
        p_OutInterval      := 300;
        p_OutNumOnInterval := 10;
        p_OutSchedule      := TPedestrianPopOutSchedule.Create;
        end;

    begin
    inherited;
    p_PedDataList := TList<TPedestrianData>.Create;
    p_DestList    := TList<string>.Create;
    p_IsAssignedDispersionPoint      := false;
    p_IsFocused              := false;
    p_DispersionPointList := TList<GLPointType>.Create;
    InitializePopData;
    InitializeOutData;
    p_BusModel := nil;
    p_BusRoadIndex := -1;
    end;

procedure TPopOutPoint.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_PedDataList);
    FreeAndNil(p_DestList);
    FreeAndNil(p_DispersionPointList);
    FreeAndNil(p_PopSchedule);
    FreeAndNil(p_OutSchedule);
    end;

procedure TPopOutPoint.AddNewPedData;
    begin
    p_PedDataList.Add(TPedestrianData.Create());
    end;

procedure TPopOutPoint.AddPedData(const aData: TPedestrianData);
    var
        newPedData : TPedestrianData;
        i          : integer;
    begin
    newPedData := TPedestrianData.Create();
    newPedData.IsActive               := aData.IsActive;
    newPedData.LabelName              := aData.LabelName;
    newPedData.PopNum                 := aData.PopNum;
    newPedData.PopPer                 := aData.PopPer;
    for i := 0 to aData.AttrPerList.Count - 1 do
        newPedData.AttrPerList.Add(aData.AttrPerList[i]);

    for i := 0 to aData.DestPerList.Count - 1 do
        newPedData.DestPerList.Add(aData.DestPerList[i]);

    p_PedDataList.Add(newPedData);
    end;

procedure TPopOutPoint.DeletePedData(const Idx: integer);
    begin
    p_PedDataList.Delete(Idx);
    end;

procedure TPopOutPoint.AddDestination(const aName: string);
    begin
    p_DestList.Add(aName);
    end;

procedure TPopOutPoint.InsertDestination(const Idx: integer; aName: string);
    begin
    p_DestList.Insert(Idx, aName);
    end;

procedure TPopOutPoint.DeleteDestination(const Idx: integer);
    begin
    p_DestList.Delete(Idx);
    end;

procedure TPopOutPoint.ClearDestination;
    begin
    p_DestList.Clear;
    end;

procedure TPopOutPoint.SetNewDetailPer(const aRow, aValue: integer);
    begin
    p_PopDetailArray[aRow - 1] := aValue;
    end;

{ TPopOutPointList }
procedure TPopOutPointList.AfterConstruction;
    begin
    inherited;
    p_List := TList<TPopOutPoint>.Create;
    p_IsVisibleDispersionPoint := false;
    end;

procedure TPopOutPointList.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_List);
    end;

procedure TPopOutPointList.AddNewPedestrianData(const LblCount: integer; LblName: string = '');
    var
        i, NewIdx : integer;
    begin
    for i := 0 to p_List.Count - 1 do
        begin
        if p_List[i].PedestrianData.Count = LblCount then
            Continue;

        p_List[i].AddNewPedData;
        NewIdx := p_List[i].PedestrianData.Count - 1;
        p_List[i].PedestrianData[NewIdx].LabelName := LblName;
        end;
    end;

procedure TPopOutPointList.DeleteData(const Idx: integer);
    begin
    p_List.Delete(Idx);
    end;

procedure TPopOutPointList.RenderDP(const opengl: TF8OpenGL);

    procedure Render(const point: TPopOutPoint; DisPoint: GLPointType);
        const
            WIDTH  = 0.5;
        var
            height, R : double;
            color : GLPointType;
        begin
        height := DisPoint[_y] + HEIGHT_OFFSET;
        R      := WIDTH + 0.01;
        if point.IsFocused then
            color := DP_FOCUSED_POLYGON_COLOR
        else
            color := DP_POLYGON_COLOR;

        glMatrixMode(GL_MODELVIEW);
        glPushAttrib(GL_ALL_ATTRIB_BITS);
        glDisable(GL_LIGHTING);
        try
            glDepthMask(GL_TRUE);
            glLineWidth(3.0);
            glColor4f(color[_x], color[_y], color[_z], color[_w]);
            glBegin(GL_POLYGON);
                glVertex3f(DisPoint[_x] + WIDTH, height, DisPoint[_z] - WIDTH);
                glVertex3f(DisPoint[_x] - WIDTH, height, DisPoint[_z] - WIDTH);
                glVertex3f(DisPoint[_x] - WIDTH, height, DisPoint[_z] + WIDTH);
                glVertex3f(DisPoint[_x] + WIDTH, height, DisPoint[_z] + WIDTH);
            glEnd;

            glDepthMask(GL_FALSE);
            glLineWidth(5.0);
            glColor4f(DP_LINE_COLOR[_x], DP_LINE_COLOR[_y], DP_LINE_COLOR[_z], DP_LINE_COLOR[_w]);
            glPushMatrix;
            glBegin(GL_LINE_LOOP);
                glVertex3f(DisPoint[_x] - R, height, DisPoint[_z] - R);
                glVertex3f(DisPoint[_x] + R, height, DisPoint[_z] - R);
                glVertex3f(DisPoint[_x] + R, height, DisPoint[_z] + R);
                glVertex3f(DisPoint[_x] - R, height, DisPoint[_z] + R);
            glEnd;
            glPopMatrix;
        finally
            glLineWidth(1.0);
            glColor4f(1.0, 1.0, 1.0, 1.0);
            glPopAttrib;
            end;
        end;

    var
        i, j : integer;
    begin
    for i := 0 to p_List.Count - 1 do
        begin
        if p_List[i].IsAssignedDispersionPoint then
            begin
            for j := 0 to p_List[i].DispersionPointList.Count - 1 do
                Render(p_List[i], p_List[i].DispersionPointList[j]);
            end;
        end;
    end;

procedure TPopOutPointList.ReceiveDispersionPointData;
    var
        nwk     : IF8Networks;
        newName : string;
        newData : TPopOutPoint;
        tmpNodeIdx : TNodeNumArray;
        i, j    : integer;
    begin
    if theApplicationServices.project.NumberOfFlightWayNwks < 1 then
        Exit;

    nwk := theApplicationServices.project.FlightWayNwk[1];
    for i := 0 to nwk.NodeCount - 1 do
        begin
        if nwk.Node[i].NumberOfDispersionPoints < 1 then
            Continue;

        newName := nwk.Node[i].Name;
        newData := TPopOutPoint.Create(newName);
        tmpNodeIdx[0] := 1;
        tmpNodeIdx[1] := i;
        newData.NodeIdx := tmpNodeIdx;
        newData.IsFocused := false;
        newData.IsAssignedDispersionPoint := true;
        for j := 0 to nwk.Node[i].NumberOfDispersionPoints - 1 do
            newData.DispersionPointList.Add(nwk.Node[i].DispersionPoint[j]);

        p_List.Add(newData);
        IsVisibleDispersionPoint := true;
        end;
    end;
end.
