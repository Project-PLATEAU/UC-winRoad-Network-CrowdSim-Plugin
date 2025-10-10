unit MFJsonLoader;

interface

uses
    PluginCore,
    F8GLUtils,
    MFJsonLoaderOptions;

procedure ImportMFJson(project : IF8ProjectForRoad; filename : String; options: MFJsonLoaderOptionsClass);

implementation

uses
    System.SysUtils,
    System.Classes,
    System.JSON,
    System.JSON.Readers,
    System.JSON.Builders,
    MovingFeature;

procedure ImportMFJson(project : IF8ProjectForRoad; filename : String; options: MFJsonLoaderOptionsClass);
    var
        aFileStream : TFileStream;
        JSONReader  : TJsonTextReader;
        JSONIterator: TJSONIterator;
    begin
    if not FileExists(filename) then
        raise Exception.Create('ファイルが存在しません。');
    aFileStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
    try
        JSONReader := TJsonTextReader.Create(TStreamReader.Create(aFileStream, TEncoding.UTF8));
        try
            JSONIterator := TJSONIterator.Create(JSONReader);
            try

                MovingFeatureListClass.ReadJson(JSONIterator, project, options);

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

end.
