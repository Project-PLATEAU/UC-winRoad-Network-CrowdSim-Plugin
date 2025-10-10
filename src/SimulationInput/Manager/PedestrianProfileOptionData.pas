unit PedestrianProfileOptionData;

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
    TPedestrianProfileOptionData = class
        private
            p_Name          : string;
            p_Weight        : integer;
            //p_Model         : IF8QuakeIII;
            p_ModelIndex    : integer;
            p_MinAge        : integer;
            p_MaxAge        : integer;
            p_Gender        : integer;
            p_CategoryIndex : integer;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            property  Name          : string      read p_Name          write p_Name;
            property  Weight        : integer     read p_Weight        write p_Weight;
            //property  Model         : IF8QuakeIII read p_Model         write p_Model;
            property  ModelIndex         : integer read p_ModelIndex         write p_ModelIndex;
            property  MinAge        : integer     read p_MinAge        write p_MinAge;
            property  MaxAge        : integer     read p_MaxAge        write p_MaxAge;
            property  Gender        : integer     read p_Gender        write p_Gender;
            property  CategoryIndex : integer     read p_CategoryIndex write p_CategoryIndex;
        end;

    TPedestrianProfileOptionList = class
        private
            p_List : TObjectList<TPedestrianProfileOptionData>;

        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            property  Data : TObjectList<TPedestrianProfileOptionData> read p_List;
        end;

implementation

{ TPedestrianProfileOptionData }

procedure TPedestrianProfileOptionData.AfterConstruction;
    begin
    inherited;

    p_Name          := '';
    p_Weight        := 100;
    //p_Model         := nil;
    p_ModelIndex    := -1;
    p_MinAge        := 10;
    p_MaxAge        := 60;
    p_Gender        := 0;
    p_CategoryIndex := -1;
    end;

procedure TPedestrianProfileOptionData.BeforeDestruction;
    begin
    inherited;
    end;

{ TPedestrianProfileOptionList }

procedure TPedestrianProfileOptionList.AfterConstruction;
    begin
    inherited;
    p_List := TObjectList<TPedestrianProfileOptionData>.Create;
    end;

procedure TPedestrianProfileOptionList.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_List);
    end;
end.
