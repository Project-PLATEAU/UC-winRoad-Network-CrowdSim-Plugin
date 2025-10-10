unit MapOrigin;

interface

uses
    PluginCore,
    F8Utils,
    PedestrianUtil,
    LatLonHelper;

type
    /// <summary>
    ///    生成時に設定した任意の地点を原点として、その地点から任意の距離移動した地点の座標を求めるクラス
    ///    座標はOpenGL座標、ローカル座標、緯度経度座標系の3つの座標系の位置を一括で計算する
    /// </summary>
    TMatrixOrigin = class
        private
            p_Origin: TPoint3D; // 緯度経度
            p_OriginLocal: F8PointType;
            p_HoriConv: IF8WrHorizontalCoordinateConvertor;
            p_Project: IF8ProjectForRoad;

            function  GetOriginLatLon: TPoint3D;
            function  GetOriginLocal: TPoint3D;
        public
            constructor Create(const aHoriConv: IF8WrHorizontalCoordinateConvertor; const aPosition: TPoint3D); overload;
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function  MoveTo(const aSouthOffset, aEastOffset: Double): TPointPositionData;

            property  OriginLatLon: TPoint3D read GetOriginLatLon;
            property  OriginLocal : TPoint3D read GetOriginLocal;
        end;

implementation

uses
    System.Math;

{ TMatrixOrigin }
constructor TMatrixOrigin.Create(const aHoriConv: IF8WrHorizontalCoordinateConvertor; const aPosition: TPoint3D);
    var
        srcPoint: F8PointType;
    begin
    p_Origin := aPosition;
    p_HoriConv := aHoriConv;

    srcPoint[_y] := p_Origin.Lat;
    srcPoint[_x] := p_Origin.Lon;

    p_HoriConv.Convert(_hctSpecifiedCS, 6668, _hctLocal_XY, 0, srcPoint, p_OriginLocal);
    end;

procedure TMatrixOrigin.AfterConstruction;
    begin
    inherited;

    p_Project := theApplicationServices.project;
    Assert(Assigned(p_Project));
    end;

procedure TMatrixOrigin.BeforeDestruction;
    begin
    inherited;

    p_HoriConv := nil;
    p_Project  := nil;
    end;

function TMatrixOrigin.MoveTo(const aSouthOffset, aEastOffset: Double): TPointPositionData;
    var
        srcPoint, dstPoint, dstPointGL: F8PointType;
        hr: HeightResultArrayType;
        newHeight, maxH: Double;
        i: Integer;
    begin
    srcPoint[_x] := p_OriginLocal[_x] + aEastOffset;
    srcPoint[_y] := p_OriginLocal[_y] - aSouthOffset;
    p_HoriConv.Convert(_hctLocal_XY, 0, _hctSpecifiedCS, 6668, srcPoint, dstPoint);
    p_HoriConv.Convert(_hctLocal_XY, 0, _hctOpenGL_XZ, 0, srcPoint, dstPointGL);
    newHeight := p_Origin.Height;
    maxH := 0.0;
    hr := p_Project.GetHeightsAt(dstPointGL[_x], dstPointGL[_y], [_hTerrain, _hRoad, _hInterSection]);
    if Length(hr) > 0 then
        begin
        for i := 0 to Length(hr) - 1 do
            maxH := Max(maxH, hr[i].hHeight);
        newHeight := maxH;
        end;

    Result.LatLon.Lat    := dstPoint[_y];
    Result.LatLon.Lon    := dstPoint[_x];
    Result.LatLon.Height := newHeight;
    Result.Local.X := srcPoint[_x];
    Result.Local.Y := newHeight;
    Result.Local.Z := srcPoint[_y];
    Result.OpenGL.X := dstPointGL[_x];
    Result.OpenGL.Y := newHeight;
    Result.OpenGL.Z := dstPointGL[_y];
    end;

function TMatrixOrigin.GetOriginLatLon: TPoint3D;
    begin
    Result := p_Origin;
    end;

function TMatrixOrigin.GetOriginLocal: TPoint3D;
    begin
    Result.X := p_OriginLocal[_x];
    Result.Y := p_Origin.Y;
    Result.Z := p_ORiginLocal[_y];
    end;
end.
