unit PopOutSchedule;

interface

uses
    System.Generics.Collections,
    PluginCore,
    SimulationInputUtils;

type
    /// <summary>
    ///    ダイヤの行ごとのデータを管理するクラス
    /// </summary>
    TScheduleData = class
        private
            p_Time         : TTime;
            p_SeatOrPedNum : integer;
            p_Capacity     : integer;
            p_GetOnNum     : integer;
            p_GetOffNum    : integer;
            p_BusID        : integer;
            p_LblPopNums   : TList<integer>;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            property  Time         : TTime   read p_Time         write p_Time;
            property  SeatOrPedNum : integer read p_SeatOrPedNum write p_SeatOrPedNum;
            property  Capacity     : integer read p_Capacity     write p_Capacity;
            property  GetOnNum     : integer read p_GetOnNum     write p_GetOnNum;
            property  GetOffNum    : integer read p_GetOffNum    write p_GetOffNum;
            property  BusID        : integer read p_BusID        write p_BusID;
            property  LblPopNums   : TList<integer> read p_LblPopNums write p_LblPopNums;
        end;

    /// <summary>
    ///    ダイヤを管理するクラス
    /// </summary>
    TPedestrianPopOutSchedule = class
        private
            p_List           : TList<TScheduleData>;
            p_UpdateRuleList : TList<TWaitingQueueUpdateRuleType>;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure ClearData;

            property  Data        : TList<TScheduleData>               read p_List;
            property  DataforLink : TList<TWaitingQueueUpdateRuleType> read p_UpdateRuleList;
        end;

implementation

uses
    System.SysUtils,
    System.Math,
    F8CrowdSimPluginDataConstant;

{ TScheduleData }
procedure TScheduleData.AfterConstruction;
    begin
    inherited;
    p_LblPopNums := TList<integer>.Create;
    end;

procedure TScheduleData.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_LblPopNums);
    end;

{ TPedestrianPopOutSchedule }
procedure TPedestrianPopOutSchedule.AfterConstruction;
    begin
    inherited;
    p_List           := TList<TScheduleData>.Create;
    p_UpdateRuleList := TList<TWaitingQueueUpdateRuleType>.Create;
    end;

procedure TPedestrianPopOutSchedule.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_List);
    FreeAndNil(p_UpdateRuleList);
    end;

procedure TPedestrianPopOutSchedule.ClearData;
    begin
    p_List.Clear;
    p_UpdateRuleList.Clear;
    end;
end.
