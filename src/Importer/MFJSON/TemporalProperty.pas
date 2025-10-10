unit TemporalProperty;

interface

uses
    System.SysUtils,
    System.Rtti,
    System.JSON,
    System.JSON.Readers,
    System.JSON.Builders,
    System.Generics.Collections;

type
    TemporalPropertyClass = class abstract
        private
            p_name  : String;
            function GetName: String;
            procedure SetName(const Value: String);
        protected
            procedure SetValue(const ait: TJSONIterator); virtual; abstract;
            function  GetValue: TValue; virtual; abstract;
        public
            class function   ReadJson(const ait: TJSONIterator): TemporalPropertyClass;
            function clone : TemporalPropertyClass; virtual; abstract;
            property    name    : String read GetName write SetName;
            property    value   : TValue read GetValue;

        end;

    TemporalStringPropertyClass = class sealed(TemporalPropertyClass)
        private
            p_values: String;
        protected
            procedure SetValue(const ait: TJSONIterator); override;
            function  GetValue: TValue; override;
        public
            function clone : TemporalPropertyClass; override;
        end;

    TemporalSinglePropertyClass = class sealed(TemporalPropertyClass)
        private
            p_values: Single;
        protected
            procedure SetValue(const ait: TJSONIterator); override;
            function  GetValue: TValue; override;
        public
            function clone : TemporalPropertyClass; override;
        end;

    TemporalIntegerPropertyClass = class sealed(TemporalPropertyClass)
        private
            p_values: Integer;
        protected
            procedure SetValue(const ait: TJSONIterator); override;
            function  GetValue: TValue; override;
        public
            function clone : TemporalPropertyClass; override;
        end;

    TemporalArraySinglePropertyClass = class(TemporalPropertyClass)
        private
            p_values: TValue;
        protected
            procedure SetValue(const ait: TJSONIterator); override;
            function  GetValue: TValue; override;
        public
            function clone : TemporalPropertyClass; override;
        end;

    TemporalArraySingleWithDTPropertyClass = class sealed(TemporalArraySinglePropertyClass)
        private
            p_dateTimes : TValue;
            function    GetDateTimes: TArray<String>;
        protected
        public
            function clone : TemporalPropertyClass; override;
            property    dateTimes   : TArray<String> read GetDateTimes;
        end;


implementation

uses
    System.JSON.Types;

{ TemporalPropertyClass<t> }

function TemporalPropertyClass.GetName: String;
    begin
    Result := p_name;
    end;

class function TemporalPropertyClass.ReadJson(const ait: TJSONIterator): TemporalPropertyClass;
    var
        name    : String;
        prop    : TemporalPropertyClass;
    begin
    Result := nil;
    while ait.Next do
        begin
        if ait.Key = 'name' then
            name := ait.AsString
        else if ait.Key = 'values' then
            begin
            case ait.&Type of
                TJsonToken.Integer:
                    begin
                    end;
                TJsonToken.Float:
                    begin
                    prop := TemporalSinglePropertyClass.Create;
                    prop.p_name := name;
                    prop.SetValue(ait);
                    Result := prop;
                    end;

                TJsonToken.String:
                    begin
                    prop := TemporalStringPropertyClass.Create;
                    prop.p_name := name;
                    prop.SetValue(ait);
                    Result := prop;
                    end;

                TJsonToken.StartArray:
                    begin
                    if Assigned(Result) then
                        prop := Result
                    else
                        prop := TemporalArraySingleWithDTPropertyClass.Create;
                    prop.p_name := name;
                    prop.SetValue(ait);
                    Result := prop;
                    end;
                end;
            end
        else if ait.Key = 'datetimes' then
            begin
            if Assigned(Result) then
                prop := Result
            else
                prop := TemporalArraySingleWithDTPropertyClass.Create;
            (prop as TemporalArraySingleWithDTPropertyClass).p_dateTimes := ait.AsValue;
            Result := prop
            end;
        end;
    end;

procedure TemporalPropertyClass.SetName(const Value: String);
    begin
    p_name := Value;
    end;

{ TemporalArraySingleWithDTPropertyClass }

function TemporalArraySingleWithDTPropertyClass.clone: TemporalPropertyClass;
    begin
    Result := TemporalArraySingleWithDTPropertyClass.Create;
    TemporalArraySingleWithDTPropertyClass(Result).p_dateTimes := p_dateTimes
    end;

function TemporalArraySingleWithDTPropertyClass.GetDateTimes: TArray<String>;
    begin
    Result := nil;
    end;

{ TemporalStringPropertyClass }

function TemporalStringPropertyClass.clone: TemporalPropertyClass;
    begin
    Result := TemporalStringPropertyClass.Create;
    TemporalStringPropertyClass(result).p_values := p_values;
    end;

function TemporalStringPropertyClass.GetValue: TValue;
    begin
    Result := p_values;
    end;

procedure TemporalStringPropertyClass.SetValue(const ait: TJSONIterator);
    begin
    p_values := ait.AsString;
    end;

{ TemporalSinglePropertyClass }

function TemporalSinglePropertyClass.clone: TemporalPropertyClass;
    begin
    Result := TemporalSinglePropertyClass.Create;
    TemporalSinglePropertyClass(Result).p_values := p_values;
    end;

function TemporalSinglePropertyClass.GetValue: TValue;
    begin
    Result := p_values;
    end;

procedure TemporalSinglePropertyClass.SetValue(const ait: TJSONIterator);
    begin
    p_values := ait.AsDouble;
    end;

{ TemporalArraySinglePropertyClass }

function TemporalArraySinglePropertyClass.clone: TemporalPropertyClass;
    begin
    Result := TemporalArraySinglePropertyClass.Create;
    TemporalArraySinglePropertyClass(Result).p_values := p_values;
    end;

function TemporalArraySinglePropertyClass.GetValue: TValue;
    begin
    Result := p_values;
    end;

procedure TemporalArraySinglePropertyClass.SetValue(const ait: TJSONIterator);
    begin
    p_values := ait.AsValue;
    end;

{ TemporalIntegerPropertyClass }

function TemporalIntegerPropertyClass.clone: TemporalPropertyClass;
    begin
    Result := TemporalIntegerPropertyClass.Create;
    TemporalIntegerPropertyClass(Result).p_values := p_values;
    end;

function TemporalIntegerPropertyClass.GetValue: TValue;
    begin
    Result := p_values;
    end;

procedure TemporalIntegerPropertyClass.SetValue(const ait: TJSONIterator);
    begin
    p_values := ait.AsInteger;
    end;

end.
