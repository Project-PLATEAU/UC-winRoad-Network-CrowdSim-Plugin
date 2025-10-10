unit SurfaceTriangle;

interface

uses
    F8Utils,
    F8OpenGL,
    PedestrianUtil;

type
    ///    三角形
    ///    SurfaceItemと仕様を合わせるため、次の仕様とする
    ///    4点の順列から構成され、始点と終点が一致する。
    ///    始点と終点を除き、一致しない。
    ///    ただし生成時は実用性を鑑み3点を与えて生成する。始点はpointAとする
    ///    pointA, B, Cは(緯度, 経度, 高さ)、pointGLA, GLB, GLCはOpenGLとする
    TSurfaceTriangle = class
        private
            p_Points: TPoint3DListType;
            p_PointGLs: TPoint3DListType;
            constructor Create; overload;

            function  GetPointGL(const aIdx: Integer): TPoint3D;
            function  GetEdgeGL(const aIdx: Integer): TParametricLine2D;
        public
            constructor Create(const pointA, pointB, pointC, pointGLA, pointGLB, pointGLC: TPoint3D); overload;

            function  Clone: TSurfaceTriangle;
            function  IsOverlappedTriangle(const aRect: VertexPositions): Boolean;
            procedure DoRender(const aOpenGL: TF8OpenGL; const aColor: TPoint3D);

            property  PointGL[const aIdx: Integer] : TPoint3D        read GetPointGL;
            property  EdgeGL[const aIdx: Integer]: TParametricLine2D read GetEdgeGL;
        end;

implementation

uses
    Winapi.Windows,
    GL,
    F8GLUtils,
    LatLonHelper;

{ TSurfaceTriangle }
constructor TSurfaceTriangle.Create;
    begin
    SetLength(p_Points, 4);
    SetLength(p_PointGLs, 4);
    end;

constructor TSurfaceTriangle.Create(const pointA, pointB, pointC, pointGLA, pointGLB, pointGLC: TPoint3D);
    begin
    SetLength(p_Points, 4);
    p_Points[0] := pointA;
    p_Points[1] := pointB;
    p_Points[2] := pointC;
    p_Points[3] := pointA;

    SetLength(p_PointGLs, 4);
    p_PointGLs[0] := pointGLA;
    p_PointGLs[1] := pointGLB;
    p_PointGLs[2] := pointGLC;
    p_PointGLs[3] := pointGLA;
    end;

function TSurfaceTriangle.Clone: TSurfaceTriangle;
    begin
    Result := TSurfaceTriangle.Create;
    CopyMemory(Result.p_Points,   p_Points,   SizeOf(TPoint3D) * 4);
    CopyMemory(Result.p_PointGLs, p_PointGLs, SizeOf(TPoint3D) * 4);
    end;

function TSurfaceTriangle.IsOverlappedTriangle(const aRect: VertexPositions): Boolean;
    var
        ab, bc, ca: TDoublePoint;
        ap, bp, cp: TDoublePoint;
        v: VertexPositionType;
        c1, c2, c3: Double;
    begin
//    Result := IsOverlay(aRect, p_Points);
    Result := False;
    ab := p_Points[1].LatLon - p_Points[0].LatLon;
    bc := p_Points[2].LatLon - p_Points[1].LatLon;
    ca := p_Points[0].LatLon - p_Points[2].LatLon;

    for v in ALL_VERTEX_POSITION do
        begin
        ap := aRect[v].LatLon - p_Points[0].LatLon;
        bp := aRect[v].LatLon - p_Points[1].LatLon;
        cp := aRect[v].LatLon - p_Points[2].LatLon;

        c1 := ab.Cross(ap);
        c2 := bc.Cross(bp);
        c3 := ca.Cross(cp);
        if ((c1 >= 0) and (c2 >= 0) and (c3 >= 0)) or ((c1 <= 0) and (c2 <= 0) and (c3 <= 0)) then
            begin
            Result := True;
            Break;
            end;
        end;
    end;

procedure TSurfaceTriangle.DoRender(const aOpenGL: TF8OpenGL; const aColor: TPoint3D);
    var
        i: Integer;
    begin
    glMatrixMode(GL_MODELVIEW);
        glPushAttrib(GL_ALL_ATTRIB_BITS);
        try
            glDisable(GL_LIGHTING);
            glDepthMask(GL_FALSE);
            glLineWidth(3.0);
            glColor4f(aColor.X, aColor.Y, aColor.Z, 1.0);

            glBegin(GL_LINE_LOOP);
            for i := 0 to Length(p_PointGLs) - 1 do
                glVertex3f(p_PointGLs[i].X, p_PointGLs[i].Y, p_PointGLs[i].Z);
            glEnd;
        finally
            glColor4f(1.0, 1.0, 1.0, 1.0);
            glLineWidth(1.0);
            glPopAttrib;
        end;
    end;

function TSurfaceTriangle.GetPointGL(const aIdx: Integer): TPoint3D;
    begin
    Assert((aIdx >= 0) and (aIdx <= 2));
    Result := p_PointGLs[aIdx];
    end;

function TSurfaceTriangle.GetEdgeGL(const aIdx: Integer): TParametricLine2D;
    begin
    Assert((aIdx >= 0) and (aIdx <= 2));
    Result.a := p_PointGLs[aIdx];
    Result.b := p_PointGLs[aIdx + 1];
    end;
end.
