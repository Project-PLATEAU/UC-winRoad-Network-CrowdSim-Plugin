unit CrosswalkEditingAreaRenderer;

interface

uses
    Vcl.Graphics,
    Vcl.Dialogs,
    System.SysUtils,
    System.Math,
    GL,
    F8GLUtils,
    F8Utils,
    F8OpenGLExts,
    F8OpenGL,
    F8Shaders,
    F8GLSL,
    PluginCore,
    PedestrianUtil;

const
    NAN_POINT       : GLPointType = (NaN, NaN, NaN, NaN);
    AREA_LINE_COLOR : GLPointType = (1.0, 0.0, 0.0, 1.0);
    HEIGHT_OFFSET = 0.6;

type
    /// <summary>
    ///    â°ífï‡ìπïœçXîÕàÕÇé¶Ç∑ògê¸Çï`âÊÇ∑ÇÈÉNÉâÉX
    /// </summary>
    TCrosswalkEditingAreaRenderer = class
        private
            p_SelectedPoint : GLPointType;
            p_NowPoint      : GLPointType;
            p_IsActive      : boolean;

            procedure SetIsActive(const aIsActive: boolean);
            function  GetIsActive: boolean;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure Initialize;
            procedure DoRender(const aOpenGL: TF8OpenGL);

            property SelectedPoint : GLPointType read p_SelectedPoint write p_SelectedPoint;
            property IsActive : boolean read GetIsActive write SetIsActive;
        end;

implementation

procedure TCrosswalkEditingAreaRenderer.AfterConstruction;
    begin
    inherited;

    Initialize;
    end;

procedure TCrosswalkEditingAreaRenderer.BeforeDestruction;
    begin
    inherited;

    Initialize;
    end;

procedure TCrosswalkEditingAreaRenderer.Initialize;
    begin
    p_SelectedPoint := NAN_POINT;
    p_NowPoint      := NAN_POINT;
    end;

procedure TCrosswalkEditingAreaRenderer.DoRender(const aOpenGL: TF8OpenGL);
    var
        terrainHeight  : single;
    begin
    if (not IsActive) or (not aOpenGL.CtrlKeyDown) then
        Exit;

    if (IsNaN(p_SelectedPoint[_x])) then
        Exit;

    aOpenGL.Camera.MoveMode := mNone;
    p_NowPoint := aOpenGL.Find3DCoordinatesUnderMouse;
    terrainHeight := Max(p_SelectedPoint[_y], p_NowPoint[_y]) + HEIGHT_OFFSET;
    glMatrixMode(GL_MODELVIEW);
    glPushAttrib(GL_ALL_ATTRIB_BITS);
    glDisable(GL_LIGHTING);
    glDepthMask(GL_FALSE);
    try
        glLineWidth(5.0);
        glColor4f(AREA_LINE_COLOR[_x], AREA_LINE_COLOR[_y], AREA_LINE_COLOR[_z], AREA_LINE_COLOR[_w]);
        glPushMatrix;
        glBegin(GL_LINE_LOOP);
            glVertex3f(p_SelectedPoint[_x], terrainHeight, p_SelectedPoint[_z]);
            glVertex3f(p_NowPoint[_x], terrainHeight, p_SelectedPoint[_z]);
            glVertex3f(p_NowPoint[_x], terrainHeight, p_NowPoint[_z]);
            glVertex3f(p_SelectedPoint[_x], terrainHeight, p_NowPoint[_z]);
        glEnd;
        glPopMatrix;
    finally
        glLineWidth(1.0);
        glColor4f(1.0, 1.0, 1.0, 1.0);
        glPopAttrib;
        end;
    end;

procedure TCrosswalkEditingAreaRenderer.SetIsActive(const aIsActive: boolean);
    begin
    p_IsActive := aIsActive;
    if not IsActive then
        Initialize;
    end;

function TCrosswalkEditingAreaRenderer.GetIsActive: boolean;
    begin
    Result := p_IsActive;
    end;
end.
