unit LatLonHelper;

interface

uses
    F8Utils;

type
    TPoint3DLatLonHelper = record helper for TPoint3D
        private
            procedure SetLat(const aValue: Double);
            procedure SetLon(const aValue: Double);
            procedure SetHeight(const aValue: Double);

            function  GetLat: Double;
            function  GetLon: Double;
            function  GetHeight: Double;

            function  GetLatLon: TDoublePoint;
        public
            property  Lat   : Double       read GetLat    write SetLat;
            property  Lon   : Double       read GetLon    write SetLon;
            property  LatLon: TDoublePoint read GetLatLon;
            property  Height: Double       read GetHeight write SetHeight;
        end;

implementation

{ TPoint3DLatLonHelper }

procedure TPoint3DLatLonHelper.SetLat(const aValue: Double);
    begin
    X := aValue;
    end;

procedure TPoint3DLatLonHelper.SetLon(const aValue: Double);
    begin
    Y := aValue;
    end;

procedure TPoint3DLatLonHelper.SetHeight(const aValue: Double);
    begin
    Z := aValue;
    end;

function TPoint3DLatLonHelper.GetLat: Double;
    begin
    Result := X;
    end;

function TPoint3DLatLonHelper.GetLon: Double;
    begin
    Result := Y;
    end;

function TPoint3DLatLonHelper.GetHeight: Double;
    begin
    Result := Z;
    end;

function TPoint3DLatLonHelper.GetLatLon: TDoublePoint;
    begin
    Result := XY;
    end;
end.
