unit PedestrianCategoryData;

interface

uses
    Winapi.Windows,
    System.SysUtils,
    System.DateUtils,
    System.Types,
    System.Classes,
    System.Generics.Collections,
    System.Math,
    System.JSON,
    System.JSON.Readers,
    System.JSON.Builders,
    System.JSON.Serializers,
    PluginCore,
    F8GLUtils,
    SimulationInputUtils;

type
    TPedestrianCategoryData = class
        private
            p_Name                : string;
            p_StairUpPer          : integer;
            p_StairDownPer        : integer;
            p_EscaUpPer           : integer;
            p_EscaUpWalkPer       : integer;
            p_EscaDownPer         : integer;
            p_EscaDownWalkPer     : integer;
            p_CrosswalkBluePer    : integer;
            p_CrosswalkRedPer     : integer;
            p_ElevUpPer           : integer;
            p_ElevDownPer         : integer;
            p_StopCollision       : double;
            p_WalkCollision       : double;
            p_WQWalkCollision     : double;
            p_DoorOrGateCollision : double;
            p_NormalSpeed         : double;
            p_StairUpSpeed        : double;
            p_StairDownSpeed      : double;
            p_EscaWalkSpeed       : double;
            p_CrosswalkBlueSpeed  : double;
            p_CrosswalkRedSpeed   : double;
            p_TakeBusTime         : double;
            p_PassGateTime        : double;

        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            property  Name                : string  read p_Name                write p_Name;
            property  StairUpPer          : integer read p_StairUpPer          write p_StairUpPer;
            property  StairDownPer        : integer read p_StairDownPer        write p_StairDownPer;
            property  EscaUpPer           : integer read p_EscaUpPer           write p_EscaUpPer;
            property  EscaUpWalkPer       : integer read p_EscaUpWalkPer       write p_EscaUpWalkPer;
            property  EscaDownPer         : integer read p_EscaDownPer         write p_EscaDownPer;
            property  EscaDownWalkPer     : integer read p_EscaDownWalkPer     write p_EscaDownWalkPer;
            property  ElevUpPer           : integer read p_ElevUpPer           write p_ElevUpPer;
            property  ElevDownPer         : integer read p_ElevDownPer         write p_ElevDownPer;
            property  CrosswalkBluePer    : integer read p_CrosswalkBluePer    write p_CrosswalkBluePer;
            property  CrosswalkRedPer     : integer read p_CrosswalkRedPer     write p_CrosswalkRedPer;
            property  StopCollision       : double  read p_StopCollision       write p_StopCollision;
            property  WalkCollision       : double  read p_WalkCollision       write p_WalkCollision;
            property  WQWalkCollision     : double  read p_WQWalkCollision     write p_WQWalkCollision;
            property  DoorOrGateCollision : double  read p_DoorOrGateCollision write p_DoorOrGateCollision;
            property  NormalSpeed         : double  read p_NormalSpeed         write p_NormalSpeed;
            property  StairUpSpeed        : double  read p_StairUpSpeed        write p_StairUpSpeed;
            property  StairDownSpeed      : double  read p_StairDownSpeed      write p_StairDownSpeed;
            property  EscaWalkSpeed       : double  read p_EscaWalkSpeed       write p_EscaWalkSpeed;
            property  CrosswalkBlueSpeed  : double  read p_CrosswalkBlueSpeed  write p_CrosswalkBlueSpeed;
            property  CrosswalkRedSpeed   : double  read p_CrosswalkRedSpeed   write p_CrosswalkRedSpeed;
            property  TakeBusTime         : double  read p_TakeBusTime         write p_TakeBusTime;
            property  PassGateTime        : double  read p_PassGateTime        write p_PassGateTime;
        end;

    TPedestrianCategoryList = class
        private
            p_List : TObjectList<TPedestrianCategoryData>;

        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            property  Data : TObjectList<TPedestrianCategoryData> read p_List;
        end;

implementation

{ TPedestrianCategoryData }

procedure TPedestrianCategoryData.AfterConstruction;
    begin
    inherited;

    p_Name                := '';
    p_StairUpPer          := 100;
    p_StairDownPer        := 100;
    p_EscaUpPer           := 100;
    p_EscaUpWalkPer       := 100;
    p_EscaDownPer         := 100;
    p_EscaDownWalkPer     := 100;
    p_ElevUpPer           := 100;
    p_ElevDownPer         := 100;
    p_CrosswalkBluePer    := 100;
    p_CrosswalkRedPer     := 100;
    p_StopCollision       := 0.5;
    p_WalkCollision       := 0.5;
    p_WQWalkCollision     := 0.5;
    p_DoorOrGateCollision := 0.5;
    p_NormalSpeed         := 4.0;
    p_StairUpSpeed        := 4.0;
    p_StairDownSpeed      := 4.0;
    p_EscaWalkSpeed       := 4.0;
    p_CrosswalkBlueSpeed  := 4.0;
    p_CrosswalkRedSpeed   := 4.0;
    p_TakeBusTime         := 2.0;
    p_PassGateTime        := 0.2;
    end;

procedure TPedestrianCategoryData.BeforeDestruction;
    begin
    inherited;
    end;

{ TPedestrianCategoryList }

procedure TPedestrianCategoryList.AfterConstruction;
    begin
    inherited;
    p_List := TObjectList<TPedestrianCategoryData>.Create;
    end;

procedure TPedestrianCategoryList.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_List);
    end;
end.
