unit CrowdSimLogUtils;

interface

uses
    PluginCore,
    F8Utils;

const
    CSLOG_PATH = '\Log';

    _saName      = 0;
    _simPassTime = 1;
    _simCurrTime = 2;
    _saCenLat    = 3;
    _saCenLon    = 4;
    _saRange     = 5;
    _saOriLat    = 6;
    _saOriLon    = 7;
    _saEndLat    = 8;
    _saEndLon    = 9;
    _saPassedNum = 10;
    _saDensity   = 11;


type
    TSensorAreaData = record
        SensorAreaName : string;
        AreaOrigin     : TPoint3D; //OpenGL
        AreaEnd        : TPoint3D; //OpenGL
        LatLonOrigin   : TPoint3D; //緯度経度
        LatLonEnd      : TPoint3D; //緯度経度
        PassedNum      : integer;  //通過人数
        end;

    TCharacterAndCell = record
        Character  : IF8CharacterInstance;
        CellIdx    : integer;
        IntoSAName : string;
        end;

implementation

end.
