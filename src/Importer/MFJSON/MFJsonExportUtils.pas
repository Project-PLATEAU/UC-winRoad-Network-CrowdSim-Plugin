unit MFJsonExportUtils;

interface

uses
    System.SysUtils,
    System.DateUtils,
    System.Types,
    System.Classes,
    System.JSON,
    System.JSON.Types,
    System.JSON.Readers,
    System.JSON.Builders,
    System.JSON.Serializers,
    System.Generics.Collections,
    System.Math;

type
    TExportProperties = record
        name    : string;
        weather : string;
        gender  : string;
        age     : string;
        end;

    TExportPropertiesNoAgeAndGender = record
        name    : string;
        weather : string;
        end;

    TExporttemporalGeometry = record
        interpolations : string;
        [JsonName('type')]
        _type          : string;
        coordinates    : TArray<array[0..2] of double>;
        datetimes      : TArray<string>;
        end;

    TMFJsonExportData = record
        properties : TExportProperties;
        [JsonName('type')]
        _type : string;
        temporalGeometry : TExporttemporalGeometry;
        end;

    TMFJsonExportDataNoAgeAndGender = record
        properties : TExportPropertiesNoAgeAndGender;
        [JsonName('type')]
        _type : string;
        temporalGeometry : TExporttemporalGeometry;
        end;

implementation

end.
