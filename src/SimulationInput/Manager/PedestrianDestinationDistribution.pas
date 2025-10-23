unit PedestrianDestinationDistribution;

interface

uses
    System.Generics.Collections;

type
    TPedestrianDistinationDistributionItem = record
        Name: String;
        Rate: Double;
        end;

    TPedestrianDistinationDistribution = class
        private
            p_List: TList<TPedestrianDistinationDistributionItem>;

            function  GetCount: Integer;
            function  GetItem(const aIdx: Integer): TPedestrianDistinationDistributionItem;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure AddDistination(const aItem: TPedestrianDistinationDistributionItem);
            procedure ChangeDistinationRate(const aIdx: Integer; const aNewItem: TPedestrianDistinationDistributionItem);

            property  ItemCount                : Integer                                read GetCount;
            property  Item[const aIdx: Integer]: TPedestrianDistinationDistributionItem read GetItem; default;
        end;

implementation

uses
    System.SysUtils;

{ TPedestrianDistinationDistribution }
procedure TPedestrianDistinationDistribution.AfterConstruction;
    begin
    inherited;
    p_List := TList<TPedestrianDistinationDistributionItem>.Create;
    end;

procedure TPedestrianDistinationDistribution.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_List);
    end;

procedure TPedestrianDistinationDistribution.AddDistination(const aItem: TPedestrianDistinationDistributionItem);
    begin
    p_List.Add(aItem);
    end;

procedure TPedestrianDistinationDistribution.ChangeDistinationRate(const aIdx: Integer; const aNewItem: TPedestrianDistinationDistributionItem);
    begin
    Assert((0 <= aIdx) and (aIdx < p_List.Count));
    p_List[aIdx] := aNewItem;
    end;

function TPedestrianDistinationDistribution.GetCount: Integer;
    begin
    Result := p_List.Count;
    end;

function TPedestrianDistinationDistribution.GetItem(const aIdx: Integer): TPedestrianDistinationDistributionItem;
    begin
    Assert((0 <= aIdx) and (aIdx < p_List.Count));
    Result := p_List[aIdx];
    end;
end.
