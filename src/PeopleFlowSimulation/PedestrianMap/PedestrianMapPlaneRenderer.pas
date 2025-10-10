unit PedestrianMapPlaneRenderer;

interface

uses
    F8OpenGL,
    F8Utils,
    PedestrianUtil;

type
    /// <summary>
    ///    シミュレーションエリア生成時に生成予定範囲を可視化するクラス
    /// </summary>
    TPedestrianMapPlaneRenderer = class
        public
            procedure Render(const aVertexs: VertexPositions; const aOpenGL: TF8OpenGL);
        end;

implementation

uses
    GL;

{ TPedestrianMapPlaneRenderer }
procedure TPedestrianMapPlaneRenderer.Render(const aVertexs: VertexPositions; const aOpenGL: TF8OpenGL);
    procedure glVertexTPoint3D(const aPoint: TPoint3D);
        begin
        glVertex3f(aPoint.X, aPoint.Y + 3, aPoint.Z);
        end;
    begin
    // 引数はOpenGL座標値
    glMatrixMode(GL_MODELVIEW);
        glPushAttrib(GL_ALL_ATTRIB_BITS);
        try
            glDisable(GL_LIGHTING);
            glDepthMask(GL_TRUE);
            glLineWidth(3.0);
            glColor4f(0.69, 0.87, 0.95, 0.7);

            glBegin(GL_POLYGON);
            glVertexTPoint3D(aVertexs[VertexPositionType._LeftTop]);
            glVertexTPoint3D(aVertexs[VertexPositionType._RightTop]);
            glVertexTPoint3D(aVertexs[VertexPositionType._RightBottom]);
            glVertexTPoint3D(aVertexs[VertexPositionType._LeftBottom]);
            glEnd;
        finally
            glColor4f(1.0, 1.0, 1.0, 1.0);
            glLineWidth(1.0);
            glPopAttrib;
        end;
    end;
end.
