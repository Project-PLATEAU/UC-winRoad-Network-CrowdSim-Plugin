unit PedestrianMapEditingGuide;

interface

uses
    PluginCore,
    F8OpenGL,
    F8Utils,
    PedestrianMap,
    PedestrianUtil;

type
    /// <summary>
    ///    マウスカーソルの位置にあるTPedestrianCellに枠線を表示する
    ///    シミュレーションエリア編集時に編集予定地点を可視化するクラス
    ///    単一のセルだけでなく、カーソルの位置からaRangeで指定した範囲のTPedestrianCellを囲うような枠線を表示することも可能
    /// </summary>
    TPedestrianMapEditingGuide = class
        private
            p_MouseAreaGuide: VertexPositions;
        public
            procedure UpdateMouseAreaGuide(const aMap: TPedestrianMap; const aLocalXZ: TPoint3D; const aRange: Cardinal);
            procedure DoRender(const aOpenGL: TF8OpenGL);
        end;

implementation

uses
    System.SysUtils,
    System.Math,
    System.Generics.Collections,
    GL,
    PedestrianCell;

{ TPedestrianMapEditingGuide }
procedure TPedestrianMapEditingGuide.UpdateMouseAreaGuide(const aMap: TPedestrianMap; const aLocalXZ: TPoint3D;
  const aRange: Cardinal);
    var
        cells: TObjectList<TPedestrianCell>;
        lt, rb: TPoint3D;
        ht: Double;
    begin
    if aMap.RequireCellsInfo(aLocalXZ, aRange, cells) then
        begin
        lt := cells.First.AreaGL[VertexPositionType._RightBottom];
        rb := cells.Last.AreaGL[VertexPositionType._LeftTop];
        ht := (lt.Y + rb.Y) / 2 + aMap.HeightOffset;

        p_MouseAreaGuide[VertexPositionType._LeftTop]     := Point3D(lt.X, ht, lt.Z);
        p_MouseAreaGuide[VertexPositionType._RightTop]    := Point3D(rb.X, ht, lt.Z);
        p_MouseAreaGuide[VertexPositionType._RightBottom] := Point3D(rb.X, ht, rb.Z);
        p_MouseAreaGuide[VertexPositionType._LeftBottom]  := Point3D(lt.X, ht, rb.Z);

        FreeAndNil(cells);
        end;
    end;

procedure TPedestrianMapEditingGuide.DoRender(const aOpenGL: TF8OpenGL);
    procedure glVertexTPoint3D(const aPoint: TPoint3D);
        begin
        glVertex3f(aPoint.X, aPoint.Y + 0.5, aPoint.Z);
        end;
    begin
    // 引数はOpenGL座標値
    glMatrixMode(GL_MODELVIEW);
        glPushAttrib(GL_ALL_ATTRIB_BITS);
        try
            glDisable(GL_LIGHTING);
            glDepthMask(GL_TRUE);
            glLineWidth(3.0);
            glColor4f(0.8, 0.8, 0.4, 0.7);

            glBegin(GL_LINE_LOOP);
            glVertexTPoint3D(p_MouseAreaGuide[VertexPositionType._LeftTop]);
            glVertexTPoint3D(p_MouseAreaGuide[VertexPositionType._RightTop]);
            glVertexTPoint3D(p_MouseAreaGuide[VertexPositionType._RightBottom]);
            glVertexTPoint3D(p_MouseAreaGuide[VertexPositionType._LeftBottom]);
            glEnd;
        finally
            glColor4f(1.0, 1.0, 1.0, 1.0);
            glLineWidth(1.0);
            glPopAttrib;
        end;
    end;
end.
