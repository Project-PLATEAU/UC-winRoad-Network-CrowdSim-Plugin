unit MFJsonLoaderOptions;

interface

uses
    System.Generics.Collections,
    System.SysUtils,
    PluginCore,
    F8GLUtils;

type
    AgeRangeType    = (under20, under60, over60); // <20, <60, 60<=

    /// <summary>
    ///    Agentごとのキャラクターモデルを割り当てるクラス
    ///    読み込んだMF-Jsonに性・年齢のデータが入っていた場合はそれらの値に応じてモデルを割り当てる
    /// </summary>
    MFJsonLoaderOptionsClass = class
        private
            p_addData      : Boolean;
            p_Unknown      : IF8QuakeIII;
            p_male         : array[AgeRangeType] of IF8QuakeIII;
            p_maleinRain   : array[AgeRangeType] of IF8QuakeIII;
            p_female       : array[AgeRangeType] of IF8QuakeIII;
            p_femaleinRain : array[AgeRangeType] of IF8QuakeIII;
            function    GetFemale(const ar: AgeRangeType): IF8QuakeIII;
            function    GetMale(const ar: AgeRangeType): IF8QuakeIII;
            procedure   SetFemale(const ar: AgeRangeType; const Value: IF8QuakeIII);
            procedure   SetMale(const ar: AgeRangeType; const Value: IF8QuakeIII);

            procedure   DestroyModel(Instance: IF8DBObject);
            procedure   SetunknownModel(const Value: IF8QuakeIII);
            function    GetFemaleinRain(const ar: AgeRangeType): IF8QuakeIII;
            function    GetMaleinRain(const ar: AgeRangeType): IF8QuakeIII;
            procedure   SetFemaleinRain(const ar: AgeRangeType; const Value: IF8QuakeIII);
            procedure   SetMaleinRain(const ar: AgeRangeType; const Value: IF8QuakeIII);
        public
            procedure   AfterConstruction; override;
            procedure   BeforeDestruction; override;

            procedure   Initilize(const aproject: IF8ProjectForRoad);

            property    addData : Boolean read p_addData write p_addData;
            property    unknown : IF8QuakeIII       read p_Unknown         write SetunknownModel;
            property    male[const ar: AgeRangeType]  : IF8QuakeIII       read GetMale         write SetMale;
            property    maleinRain[const ar: AgeRangeType]  : IF8QuakeIII read GetMaleinRain   write SetMaleinRain;
            property    female[const ar: AgeRangeType]: IF8QuakeIII       read GetFemale       write SetFemale;
            property    femaleinRain[const ar: AgeRangeType]: IF8QuakeIII read GetFemaleinRain write SetFemaleinRain;
        end;

implementation

{ MFJsonLoaderOptionsClass }

procedure MFJsonLoaderOptionsClass.AfterConstruction;
    var
        i   : AgeRangeType;
    begin
    inherited;
    p_addData := False;
    for i := Low(AgeRangeType) to High(AgeRangeType) do
        begin
        p_male[i] := nil;
        p_female[i] := nil;
        p_maleinRain[i] := nil;
        p_femaleinRain[i] := nil;
        end;

    end;

procedure MFJsonLoaderOptionsClass.BeforeDestruction;
    var
        i   : AgeRangeType;
    begin
    inherited;
    if Assigned(p_Unknown) then
        begin
        p_Unknown.UnregisterOnBeforeDestructionProc(DestroyModel);
        p_Unknown := nil;
        end;
    for i := Low(AgeRangeType) to High(AgeRangeType) do
        begin
        if Assigned(male[i]) then
            begin
            male[i].UnregisterOnBeforeDestructionProc(DestroyModel);
            male[i] := nil;
            end;
        if Assigned(female[i]) then
            begin
            female[i].UnregisterOnBeforeDestructionProc(DestroyModel);
            female[i] := nil;
            end;
        if Assigned(maleinRain[i]) then
            begin
            maleinRain[i].UnregisterOnBeforeDestructionProc(DestroyModel);
            maleinRain[i] := nil;
            end;
        if Assigned(femaleinRain[i]) then
            begin
            femaleinRain[i].UnregisterOnBeforeDestructionProc(DestroyModel);
            femaleinRain[i] := nil;
            end;
        end;
    end;

procedure MFJsonLoaderOptionsClass.DestroyModel(Instance: IF8DBObject);
    var
        i   : AgeRangeType;
        m   : IF8QuakeIII;
    begin
    if Supports(Instance, IF8QuakeIII, m) then
        begin
        if p_Unknown = m then
            p_Unknown := nil;
        for i := Low(AgeRangeType) to High(AgeRangeType) do
            begin
            if male[i] = m then
                male[i] := nil;
            if female[i] = m then
                female[i] := nil;
            if maleinRain[i] = m then
                maleinRain[i] := nil;
            if femaleinRain[i] = m then
                femaleinRain[i] := nil;
            end;
        end;
    end;

function MFJsonLoaderOptionsClass.GetFemale(const ar: AgeRangeType): IF8QuakeIII;
    begin
    Result := p_female[ar];
    end;

function MFJsonLoaderOptionsClass.GetFemaleinRain(const ar: AgeRangeType): IF8QuakeIII;
    begin
    Result := p_femaleinRain[ar];
    end;

function MFJsonLoaderOptionsClass.GetMale(const ar: AgeRangeType): IF8QuakeIII;
    begin
    Result := p_male[ar];
    end;

function MFJsonLoaderOptionsClass.GetMaleinRain(const ar: AgeRangeType): IF8QuakeIII;
    begin
    Result := p_maleinRain[ar];
    end;

procedure MFJsonLoaderOptionsClass.Initilize(const aproject: IF8ProjectForRoad);
    var
        i   : AgeRangeType;
    begin
    if aproject.numberOfCharacters > 0 then
        begin
        for i := Low(AgeRangeType) to High(AgeRangeType) do
            begin
            if not Assigned(male[i]) then
                male[i] := aproject.character[1];
            if not Assigned(female[i]) then
                female[i] := aproject.character[1];
            if not Assigned(maleinRain[i]) then
                maleinRain[i] := aproject.character[1];
            if not Assigned(femaleinRain[i]) then
                femaleinRain[i] := aproject.character[1];
            end;
        end;
    end;

procedure MFJsonLoaderOptionsClass.SetFemale(const ar: AgeRangeType; const Value: IF8QuakeIII);
    begin
    if Assigned(p_female[ar]) then
        begin
        p_female[ar].UnregisterOnBeforeDestructionProc(DestroyModel);
        p_female[ar] := nil;
        end;
    if Assigned(Value) then
        begin
        p_female[ar] := Value;
        p_female[ar].RegisterOnBeforeDestructionProc(DestroyModel);
        end;
    end;

procedure MFJsonLoaderOptionsClass.SetFemaleinRain(const ar: AgeRangeType; const Value: IF8QuakeIII);
    begin
    if Assigned(p_femaleinRain[ar]) then
        begin
        p_femaleinRain[ar].UnregisterOnBeforeDestructionProc(DestroyModel);
        p_femaleinRain[ar] := nil;
        end;
    if Assigned(Value) then
        begin
        p_femaleinRain[ar] := Value;
        p_femaleinRain[ar].RegisterOnBeforeDestructionProc(DestroyModel);
        end;
    end;

procedure MFJsonLoaderOptionsClass.SetMale(const ar: AgeRangeType; const Value: IF8QuakeIII);
    begin
    if Assigned(p_male[ar]) then
        begin
        p_male[ar].UnregisterOnBeforeDestructionProc(DestroyModel);
        p_male[ar] := nil;
        end;
    if Assigned(Value) then
        begin
        p_male[ar] := Value;
        p_male[ar].RegisterOnBeforeDestructionProc(DestroyModel);
        end;
    end;

procedure MFJsonLoaderOptionsClass.SetMaleinRain(const ar: AgeRangeType;  const Value: IF8QuakeIII);
    begin
    if Assigned(p_maleinRain[ar]) then
        begin
        p_maleinRain[ar].UnregisterOnBeforeDestructionProc(DestroyModel);
        p_maleinRain[ar] := nil;
        end;
    if Assigned(Value) then
        begin
        p_maleinRain[ar] := Value;
        p_maleinRain[ar].RegisterOnBeforeDestructionProc(DestroyModel);
        end;
    end;

procedure MFJsonLoaderOptionsClass.SetunknownModel(const Value: IF8QuakeIII);
    begin
    if Assigned(p_Unknown) then
        begin
        p_Unknown.UnregisterOnBeforeDestructionProc(DestroyModel);
        p_Unknown := nil;
        end;
    if Assigned(Value) then
        begin
        p_Unknown := Value;
        p_Unknown.RegisterOnBeforeDestructionProc(DestroyModel);
        end;
    end;

end.
