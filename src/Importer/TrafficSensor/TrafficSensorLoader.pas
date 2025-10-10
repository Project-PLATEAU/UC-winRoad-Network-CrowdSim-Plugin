unit TrafficSensorLoader;

interface

uses
    PluginCore,
    TrafficSensor,
    TrafficSensorDat;

procedure ImportTrafficSensorJson(project : IF8ProjectForRoad; filename : String; data: TrafficSensorListClass);
procedure ImportTrafficSensorDetailJson(project : IF8ProjectForRoad; filename : String; data: TrafficSensorListClass);
procedure ImportTrafficSensorDat(project : IF8ProjectForRoad; filename : String; data: TrafficSensorDatClass);
procedure ImportTrafficSensorGenderOldDat(project : IF8ProjectForRoad; filename : String; data: TrafficSensorDatClass);

implementation

uses
    System.SysUtils,
    System.Classes,
    System.JSON,
    System.JSON.Readers,
    System.JSON.Builders;

//==============================================================================
procedure ImportTrafficSensorJson(project : IF8ProjectForRoad; filename : String; data: TrafficSensorListClass);
    var
        aFileStream : TFileStream;
        JSONReader  : TJsonTextReader;
        JSONIterator: TJSONIterator;
    begin
    if not FileExists(filename) then
        raise Exception.Create('ファイルが存在しません');
    aFileStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
    try
        JSONReader := TJsonTextReader.Create(TStreamReader.Create(aFileStream, TEncoding.UTF8));
        try
            JSONIterator := TJSONIterator.Create(JSONReader);
            try
                data.ReadTSJson(JSONIterator, project);

            finally
                FreeAndNIl(JSONIterator);
                end;

        finally
            FreeAndNil(JSONReader);
            end;
    finally
        FreeAndNil(aFileStream);
        end;
    end;

//==============================================================================
procedure ImportTrafficSensorDetailJson(project : IF8ProjectForRoad; filename : String; data: TrafficSensorListClass);
    var
        aFileStream : TFileStream;
        JSONReader  : TJsonTextReader;
        JSONIterator: TJSONIterator;
    begin
    if not FileExists(filename) then
        raise Exception.Create('ファイルが存在しません');
    aFileStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
    try
        JSONReader := TJsonTextReader.Create(TStreamReader.Create(aFileStream, TEncoding.UTF8));
        try
            JSONIterator := TJSONIterator.Create(JSONReader);
            try
                data.ReadTSDetailJson(JSONIterator, project);

            finally
                FreeAndNIl(JSONIterator);
                end;

        finally
            FreeAndNil(JSONReader);
            end;
    finally
        FreeAndNil(aFileStream);
        end;
    end;

//==============================================================================
procedure ImportTrafficSensorDat(project : IF8ProjectForRoad; filename : String; data: TrafficSensorDatClass);
    var
        FileReader : TStreamReader;
    begin
    if not FileExists(filename) then
        raise Exception.Create('ファイルが存在しません');
    FileReader := TStreamReader.Create(filename, TEncoding.UTF8, true);

    try
        data.ReadInOutDat(FileReader, project);

    finally
        FreeAndNil(FileReader);
        end;
    end;

//==============================================================================
procedure ImportTrafficSensorGenderOldDat(project : IF8ProjectForRoad; filename : String; data: TrafficSensorDatClass);
    var
        FileReader : TStreamReader;
    begin
    if not FileExists(filename) then
        raise Exception.Create('ファイルが存在しません');
    FileReader := TStreamReader.Create(filename, TEncoding.UTF8, true);

    try
        data.ReadGenderOldDat(FileReader, project);

    finally
        FreeAndNil(FileReader);
        end;
    end;
end.
