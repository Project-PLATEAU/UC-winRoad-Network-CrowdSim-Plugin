unit WalkingRouteAutomaticGenerator;

interface

uses
    System.Generics.Collections,
    F8Utils,
    PluginCore,
    WalkingRouteCandidatePath;

type
    TWalkingRouteAutomaticGenerator = class
        private
            procedure DefineDefaultPedestrianProfile;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure GenerateNetworkPath(const aCandidatePathList: TCandidatePathListArray);
        end;

implementation

uses
    System.SysUtils,
    F8GLUtils,
    ParametricLine2DHelper;

{ TWalkingRouteAutomaticGenerator }
procedure TWalkingRouteAutomaticGenerator.AfterConstruction;
    begin
    inherited;
    {no action}
    end;

procedure TWalkingRouteAutomaticGenerator.BeforeDestruction;
    begin
    inherited;
    {no action}
    end;

procedure TWalkingRouteAutomaticGenerator.GenerateNetworkPath(const aCandidatePathList: TCandidatePathListArray);
    function  ToGLPointType(const AveragePoint: TPoint3D): GLPointType;
        begin
        Result[_x] := AveragePoint.X;
        Result[_y] := AveragePoint.Y;
        Result[_z] := AveragePoint.Z;
        end;
    var
        targetNetwork   : IF8Networks;
        registeredLine: TList<TParametricLine2D>;
        newLine: TParametricLine2D;
        registered: Boolean;
        i, j, k: Integer;
        networkIndex, generatorIndex: Integer;
    begin
    if not Assigned(aCandidatePathList) then
        Exit;

    if aCandidatePathList.Count < 1 then
        Exit;

    if not Assigned(theApplicationServices.project) then
        Exit;

    if theApplicationServices.project.NumberOfFlightWayNwks < 1 then
        networkIndex :=theApplicationServices.project.AddEmptyNetwork
    else
        networkIndex := 1;

    targetNetwork  := theApplicationServices.project.FlightWayNwk[networkIndex];

    if targetNetwork.NumberOfGenerator < 1 then
        generatorIndex := targetNetwork.AddPedestriansGeneratorIF(_pgNormal)
    else
        generatorIndex := 0;

    // Cleanup
    for i := targetNetwork.LinkCount - 1 downto 0 do
        targetNetwork.RemoveLink(i);

    targetNetwork.Clean;

    registeredLine :=TList<TParametricLine2D>.Create;
    try
        for i := 0 to aCandidatePathList.Count - 1 do
            begin
            for j := 0 to aCandidatePathList[i].UnionPathCount - 1 do
                begin
                newLine := aCandidatePathList[i].UnionPath[j];
                registered := False;
                for k := 0 to registeredLine.Count - 1 do
                    begin
                    if registeredLine[k].IsMatch(newLine) then
                        begin
                        registered := True;
                        Break;
                        end;
                    end;
                if registered then
                    Continue;

                targetNetwork.AddLinearPath(ToGLPointType(newLine.a), ToGLPointType(newLine.b));
                registeredLine.Add(newLine);
                end;
            end;
    finally
        targetNetwork.Clean; // パスで接続されていないノードを取り除く
        FreeAndNil(registeredLine);
        end;

    if targetNetwork.Generator[generatorIndex].TypeOfGenerator = _pgNormal then
        targetNetwork.ChangeGeneratorType(generatorIndex, _pgMatrixOD);
    targetNetwork.ChangeActivePedestriansGeneratorIndex(generatorIndex);
    DefineDefaultPedestrianProfile;
    end;

procedure TWalkingRouteAutomaticGenerator.DefineDefaultPedestrianProfile;
    var
        pedProf : IF8PedestrianProfile;
        pedCate : IF8PedestrianCategory;
        newAdjustRate : AdjustRateType;

    procedure SetAdjustRate(const linkType: NetworkLinkType; linkDirectionType : NetworkLinkDirectionType; actionType: CrowdCharacterActionType; percentage: integer);
        begin
        newAdjustRate.rate := 1.0;
        newAdjustRate.percentage := percentage;
        pedCate.adjustRate[linkType, linkDirectionType, actionType] := newAdjustRate;
        end;

    begin
    //デフォルト属性を設定
    if theApplicationServices.project.NumberOfPedestrianCategory < 1 then
        theApplicationServices.project.AddPedestrianCategory;

    pedCate := theApplicationServices.project.PedestrianCategoryIF[0];
    pedCate.name := 'デフォルト設定';
    SetAdjustRate(_nlStairs, _nldUp, _ccaWalking, 100);
    SetAdjustRate(_nlStairs, _nldDown, _ccaWalking, 100);
    SetAdjustRate(_nlEscalator, _nldUp, _ccaStanding, 100);
    SetAdjustRate(_nlEscalator, _nldUp, _ccaWalking, 100);
    SetAdjustRate(_nlEscalator, _nldDown, _ccaStanding, 100);
    SetAdjustRate(_nlEscalator, _nldDown, _ccaWalking, 100);
    SetAdjustRate(_nlElevator, _nldUp, _ccaStanding, 100);
    SetAdjustRate(_nlElevator, _nldDown, _ccaStanding, 100);
    SetAdjustRate(_nlCrosswalkBlue, _nldNone, _ccaWalking, 100);
    SetAdjustRate(_nlCrosswalkRed, _nldNone, _ccaWalking, 100);
    pedCate.widthAction[_ccaStanding]         := 0.5;
    pedCate.widthAction[_ccaWalking]          := 0.5;
    pedCate.widthAction[_ccaWalkingQueue]     := 0.5;
    pedCate.thicknessAction[_ccaStanding]     := 0.5;
    pedCate.thicknessAction[_ccaWalking]      := 0.5;
    pedCate.thicknessAction[_ccaWalkingQueue] := 0.5;
    pedCate.thicknessAction[_ccaFlush]        := 0.5;
    pedCate.horizontalSpeed[_nlFloor, _nldNone, _ccaWalking]         := 4.0;
    pedCate.horizontalSpeed[_nlStairs, _nldUp, _ccaWalking]          := 4.0;
    pedCate.horizontalSpeed[_nlStairs, _nldDown, _ccaWalking]        := 4.0;
    pedCate.horizontalSpeed[_nlEscalator, _nldUp, _ccaWalking]       := 4.0;
    pedCate.horizontalSpeed[_nlEscalator, _nldDown, _ccaWalking]     := 4.0;
    pedCate.horizontalSpeed[_nlCrosswalkBlue, _nldNone, _ccaWalking] := 4.0;
    pedCate.horizontalSpeed[_nlCrosswalkRed, _nldNone, _ccaWalking]  := 4.0;

    pedProf := theApplicationServices.project.PedestrianProfileIF[1];
    pedProf.option[1].TakeBusTime := 2.0;
    pedProf.option[1].PassThroughTicketGateSec := 0.2;
    pedProf.option[1].pedestrianCategoryIndex := 0;
    pedProf.option[1].Weight := 100;
    pedProf.AttributeLabel := 'デフォルトラベル';
    end;
end.

