unit WaitingQueueAreaList;

interface

uses
    System.Generics.Collections,
    XML.XMLIntf,
    F8OpenGL,
    GL,
    F8GLUtils,
    F8Utils,
    PluginCore,
    SimulationInputUtils,
    WQAreaRenderer;

type
    /// <summary>
    ///    待機列形成範囲ごとのデータを管理するクラス
    /// </summary>
    TWaitingQueueArea = class
        private
            p_Name : string;
            p_OriginPoint : GLPointType;
            p_EndPoint    : GLPointType;
            p_PedestrianMargin : double;
            p_IsUniqueExitRule : boolean;
            p_ExitInterval : integer;
            p_ExitPedNum   : integer;
            p_LinkedNodeIdx : TNodeNumArray;
            p_WaitingQueueNodeIdx : TNodeNumArray;
            p_LinkedWQList : TList<string>;
            p_CurrentWQIndex : integer;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure Initialize;

            property  Name : string read p_Name write p_Name;
            property  OriginPoint : GLPointType read p_OriginPoint write p_OriginPoint;
            property  EndPoint : GLPointType read p_EndPoint write p_EndPoint;
            property  PedestrianMargin : double read p_PedestrianMargin write p_PedestrianMargin;
            property  IsUniqueExitRule : boolean read p_IsUniqueExitRule write p_IsUniqueExitRule;
            property  ExitInterval : integer read p_ExitInterval write p_ExitInterval;
            property  ExitPedNum : integer read p_ExitPedNum write p_ExitPedNum;
            property  LinkedNodeIdx : TNodeNumArray read p_LinkedNodeIdx write p_LinkedNodeIdx;
            property  WaitingQueueNodeIdx : TNodeNumArray read p_WaitingQueueNodeIdx write p_WaitingQueueNodeIdx;
            property  LinkedWQList : TList<string> read p_LinkedWQList write p_LinkedWQList;
            property  CurrentWQIndex : integer read p_CurrentWQIndex write p_CurrentWQIndex;
        end;

    /// <summary>
    ///    全ての待機列形成範囲を管理するクラス
    /// </summary>
    TWaitingQueueAreaList = class
        private
            p_List : TList<TWaitingQueueArea>;
            p_Renderer : TWQAreaRenderer;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure AddNewData(const newName: string; newArea: TRenderDataArray);
            procedure DeleteData(const Idx: integer);
            procedure SaveData;
            procedure LoadData;

            property  Data     : TList<TWaitingQueueArea> read p_List;
            property  Renderer : TWQAreaRenderer read p_Renderer;
        end;

implementation

uses
    System.SysUtils,
    System.Math,
    F8CrowdSimPluginDataConstant;

{ TWaitingQueueArea }
procedure TWaitingQueueArea.AfterConstruction;
    begin
    inherited;
    Initialize;
    p_LinkedWQList := TList<string>.Create;
    end;

procedure TWaitingQueueArea.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_LinkedWQList);
    end;

procedure TWaitingQueueArea.Initialize;
    begin
    p_Name := '';
    p_OriginPoint := GLZERO;
    p_EndPoint    := GLZERO;
    p_PedestrianMargin := 1.0;
    p_IsUniqueExitRule := false;
    p_ExitInterval := 300;
    p_ExitPedNum   := 10;
    p_LinkedNodeIdx[0] := -1;
    p_LinkedNodeIdx[1] := -1;
    p_WaitingQueueNodeIdx[0] := -1;
    p_WaitingQueueNodeIdx[1] := -1;
    p_CurrentWQIndex := -1;
    end;

{ TWaitingQueueAreaList }
procedure TWaitingQueueAreaList.AfterConstruction;
    begin
    p_List := TList<TWaitingQueueArea>.Create;
    p_Renderer := TWQAreaRenderer.Create;
    end;

procedure TWaitingQueueAreaList.BeforeDestruction;
    begin
    FreeAndNil(p_Renderer);
    FreeAndNil(p_List);
    end;

procedure TWaitingQueueAreaList.AddNewData(const newName: string; newArea: TRenderDataArray);
    var
        newWQArea : TWaitingQueueArea;
    begin
    newWQArea := TWaitingQueueArea.Create;
    newWQArea.Name := newName;
    newWQArea.OriginPoint := newArea[0];
    newWQArea.EndPoint    := newArea[1];
    p_List.Add(newWQArea);
    end;

procedure TWaitingQueueAreaList.DeleteData(const Idx: integer);
    begin
    p_List.Delete(Idx);
    end;

procedure TWaitingQueueAreaList.SaveData;
    var
        i, j       : integer;
        tmpProject : IF8ProjectForRoad;
    begin
    tmpProject := theApplicationServices.project;
    if Assigned(tmpProject) then
        begin
        tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, 'WQListCount'] := p_List.Count.ToString;
        for i := 0 to p_List.Count - 1 do
            begin
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.Name']             := p_List[i].Name;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.OriginPoint_X']    := p_List[i].OriginPoint[_x].ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.OriginPoint_Y']    := p_List[i].OriginPoint[_y].ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.OriginPoint_Z']    := p_List[i].OriginPoint[_z].ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.EndPoint_X']       := p_List[i].EndPoint[_x].ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.EndPoint_Y']       := p_List[i].EndPoint[_y].ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.EndPoint_Z']       := p_List[i].EndPoint[_z].ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.PedestrianMargin'] := p_List[i].PedestrianMargin.ToString;
            if p_List[i].IsUniqueExitRule then
                tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.IsUniqueExitRule'] := 'TRUE'
            else
                tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.IsUniqueExitRule'] := 'FALSE';

            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.ExitInterval']  := p_List[i].ExitInterval.ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.ExitPedNum']    := p_List[i].ExitPedNum.ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.NwkIdx']        := p_List[i].LinkedNodeIdx[0].ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.NdIdx']         := p_List[i].LinkedNodeIdx[1].ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.WQNwkIdx']      := p_List[i].WaitingQueueNodeIdx[0].ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.WQNdIdx']       := p_List[i].WaitingQueueNodeIdx[1].ToString;
            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.LinkedWQCount'] := p_List[i].LinkedWQList.Count.ToString;
            for j := 0 to p_List[i].LinkedWQList.Count - 1 do
                tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + Format('LinkedWQ[%d]', [j])] := p_List[i].LinkedWQList[j];

            tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.CurrentWQIndex'] := p_List[i].CurrentWQIndex.ToString;
            end;
        end;

    tmpProject := nil;
    end;

procedure TWaitingQueueAreaList.LoadData;

    procedure SetLoadData(const aProject: IF8ProjectForRoad; DataCount: integer);
        var
            i, j, tmpC : integer;
            tmpWQArea  : TWaitingQueueArea;
            tmpGLPoint : GLPointType;
            BoolStr    : string;
            tmpRenderData : TRenderDataArray;
            tmpNodeNum    : TNodeNumArray;
        begin
        for i := 0 to DataCount - 1 do
            begin
            tmpWQArea := TWaitingQueueArea.Create;

            tmpWQArea.Name             := aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.Name'];
            tmpGLPoint[_x]             := StrToFloat(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.OriginPoint_X']);
            tmpGLPoint[_y]             := StrToFloat(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.OriginPoint_Y']);
            tmpGLPoint[_z]             := StrToFloat(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.OriginPoint_Z']);
            tmpWQArea.OriginPoint      := tmpGLPoint;
            tmpGLPoint[_x]             := StrToFloat(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.EndPoint_X']);
            tmpGLPoint[_y]             := StrToFloat(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.EndPoint_Y']);
            tmpGLPoint[_z]             := StrToFloat(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.EndPoint_Z']);
            tmpWQArea.EndPoint         := tmpGLPoint;
            tmpWQArea.PedestrianMargin := StrToFloat(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.PedestrianMargin']);
            BoolStr                    := aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.IsUniqueExitRule'];
            if BoolStr = 'TRUE' then
                tmpWQArea.IsUniqueExitRule := true
            else
                tmpWQArea.IsUniqueExitRule := false;

            tmpWQArea.ExitInterval  := StrToInt(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.ExitInterval']);
            tmpWQArea.ExitPedNum    := StrToInt(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.ExitPedNum']);
            tmpNodeNum[0]           := StrToIntDef(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.NwkIdx'], -1);
            tmpNodeNum[1]           := StrToIntDef(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.NdIdx'], -1);
            tmpWQArea.LinkedNodeIdx := tmpNodeNum;
            tmpNodeNum[0]           := StrToIntDef(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.WQNwkIdx'], -1);
            tmpNodeNum[1]           := StrToIntDef(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.WQNdIdx'], -1);
            tmpWQArea.WaitingQueueNodeIdx := tmpNodeNum;
            tmpC := StrToIntDef(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.LinkedWQCount'], 0);
            if tmpC > 0 then
                begin
                for j := 0 to tmpC - 1 do
                    tmpWQArea.LinkedWQList.Add(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + Format('LinkedWQ[%d]', [j])]);
                end;

            tmpWQArea.CurrentWQIndex := StrToIntDef(aProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, Format('WQList[%d]', [i]) + '.CurrentWQIndex'], -1);
            p_List.Add(tmpWQArea);
            tmpRenderData[0] := tmpWQArea.OriginPoint;
            tmpRenderData[1] := tmpWQArea.EndPoint;
            p_Renderer.UpdateDrawDataList(tmpWQArea.Name, tmpRenderData);
            end;
        end;

    var
        tmpCount   : integer;
        tmpProject : IF8ProjectForRoad;
    begin
    tmpProject := theApplicationServices.project;
    if Assigned(tmpProject) then
        begin
        tmpCount := StrToIntDef(tmpProject.pluginData[F8_CROWD_SIM_PLUGIN_ID, 'WQListCount'], 0);
        if tmpCount < 1 then
            begin
            tmpProject := nil;
            Exit;
            end;

        SetLoadData(tmpProject, tmpCount);
        end;

    tmpProject := nil;
    end;
end.
