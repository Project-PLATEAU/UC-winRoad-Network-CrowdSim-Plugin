unit WQAreaRenderer;

interface

uses
    System.Generics.Collections,
    F8OpenGL,
    GL,
    F8GLUtils,
    F8Utils,
    PluginCore,
    SimulationInputUtils;

type
    /// <summary>
    ///    待機列形成範囲を描画するクラス
    /// </summary>
    TWQAreaRenderer = class
        private
            p_DrawDataList : TList<TRenderDataArray>;
            p_SelectedPoint : GLPointType;
            p_NowPoint      : GLPointType;
            p_IsActiveEditingRender : boolean;
            p_FocusedIndex : integer;

            procedure DoRender(const aOpenGL: TF8OpenGL; const aAreaPoints: TRenderDataArray; const aHeightOffset: Double);
            procedure FocusedRender(const aOpenGL: TF8OpenGL; const aHeightOffset: Double);
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure Render(const aOpenGL: TF8OpenGL);

            procedure Initialize;
            procedure MoveCameraToWQArea(const Idx: integer);
            procedure SetEditingPoints(const FirstPoint, SecondPoint: GLPointType);
            procedure SetFocusedPoints(const FocusedIdx: integer);
            procedure DeleteData(const Idx: integer);
            procedure UpdateDrawDataList(const newName: String; const newData: TRenderDataArray);
            procedure SetIsActiveEditingRender(const aIsActive: boolean);

            property  DrawDataList : TList<TRenderDataArray> read p_DrawDataList;
        end;

implementation

uses
    System.SysUtils,
    System.Math;

const
    AREA_FOCUS_COLOR   : GLPointType = (0.93, 0.51, 0.93, 1.0);
    AREA_NOFOCUS_COLOR : GLPointType = (0.5, 0.0, 0.5, 1.0);
    HEIGHT_OFFSET = 0.6;

{ TWQAreaRenderer }
procedure TWQAreaRenderer.AfterConstruction;
    begin
    inherited;

    p_DrawDataList := TList<TRenderDataArray>.Create;
    Initialize;
    end;

procedure TWQAreaRenderer.BeforeDestruction;
    begin
    inherited;

    p_DrawDataList.Clear;
    FreeAndNil(p_DrawDataList);
    end;

procedure TWQAreaRenderer.Initialize;
    begin
    p_SelectedPoint := NAN_POINT;
    p_NowPoint      := NAN_POINT;
    p_FocusedIndex  := -1;
    end;

procedure TWQAreaRenderer.SetEditingPoints(const FirstPoint, SecondPoint: GLPointType);
    begin
    p_SelectedPoint := FirstPoint;
    p_NowPoint      := SecondPoint;
    p_FocusedIndex  := -1;
    end;

procedure TWQAreaRenderer.SetFocusedPoints(const FocusedIdx: integer);
    begin
    if FocusedIdx >= 0 then
        begin
        p_SelectedPoint := p_DrawDataList[FocusedIdx][0];
        p_NowPoint      := p_DrawDataList[FocusedIdx][1];
        end;

    p_FocusedIndex  := FocusedIdx;
    end;

procedure TWQAreaRenderer.DeleteData(const Idx: integer);
    begin
    p_DrawDataList.Delete(Idx);
    end;

procedure TWQAreaRenderer.UpdateDrawDataList(const newName: String; const newData: TRenderDataArray);
    begin
    if IsNaN(newData[0][_x]) then
        Exit;

    p_DrawDataList.Add(newData);
    end;

procedure TWQAreaRenderer.Render(const aOpenGL: TF8OpenGL);
    var
        i: integer;
    begin
    FocusedRender(aOpenGL, HEIGHT_OFFSET);
    for i := 0 to p_DrawDataList.Count - 1 do
        begin
        if i <> p_FocusedIndex then
            DoRender(aOpenGL, p_DrawDataList[i], HEIGHT_OFFSET);
        end;
    end;

procedure TWQAreaRenderer.FocusedRender(const aOpenGL: TF8OpenGL; const aHeightOffset: Double);

    //--------------------------------------------------------------------------
    procedure GetSelectedHeight(const CurPos: GLPointType; SelectedPoint: TPoint3D; var terrainHeight: single);
        var
            HeightsData    : HeightResultArrayType;
            SelectedHeight : single;
            i              : integer;
        begin
        SelectedHeight := 0.0;
        HeightsData := theApplicationServices.project.GetHeightsAt(SelectedPoint.X, SelectedPoint.Z, [_hTerrain, _hRoad, _hIntersection]);
        if Length(HeightsData) > 0 then
            begin
            for i := 0 to Length(HeightsData) - 1 do
                SelectedHeight := Max(CurPos[_y], HeightsData[i].hHeight);
            end
        else
            Assert(False);

        terrainHeight := SelectedHeight + 0.6;
        end;

    //--------------------------------------------------------------------------
    procedure RenderArea(const CurPos: GLPointType; SelectedPoint: TPoint3D);
        var
            terrainHeight  : single;
        begin
        GetSelectedHeight(CurPos, SelectedPoint, terrainHeight);
        glMatrixMode(GL_MODELVIEW);
        glPushAttrib(GL_ALL_ATTRIB_BITS);
        glDisable(GL_LIGHTING);
        glDepthMask(GL_FALSE);
        try
            glLineWidth(5.0);
            glColor4f(AREA_FOCUS_COLOR[_x], AREA_FOCUS_COLOR[_y], AREA_FOCUS_COLOR[_z], AREA_FOCUS_COLOR[_w]);
            glPushMatrix;
            glBegin(GL_LINE_LOOP);
                glVertex3f(SelectedPoint.X, terrainHeight, SelectedPoint.Z);
                glVertex3f(CurPos[_x], terrainHeight, SelectedPoint.Z);
                glVertex3f(CurPos[_x], terrainHeight, CurPos[_z]);
                glVertex3f(SelectedPoint.X, terrainHeight, CurPos[_z]);
            glEnd;
            glPopMatrix;
        finally
            glLineWidth(1.0);
            glColor4f(1.0, 1.0, 1.0, 1.0);
            glPopAttrib;
            end;
        end;

    //--------------------------------------------------------------------------
    var
        terrainHeight  : single;
    begin
    if (IsNaN(p_SelectedPoint[_x])) then
        Exit;

    if p_IsActiveEditingRender then
        begin
        aOpenGL.Camera.MoveMode := mNone;
        p_NowPoint := aOpenGL.Find3DCoordinatesUnderMouse;
        end;

    terrainHeight := Max(p_SelectedPoint[_y], p_NowPoint[_y]) + HEIGHT_OFFSET;
    glMatrixMode(GL_MODELVIEW);
    glPushAttrib(GL_ALL_ATTRIB_BITS);
    glDisable(GL_LIGHTING);
    glDepthMask(GL_FALSE);
    try
        glLineWidth(5.0);
        glColor4f(AREA_FOCUS_COLOR[_x], AREA_FOCUS_COLOR[_y], AREA_FOCUS_COLOR[_z], AREA_FOCUS_COLOR[_w]);
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

procedure TWQAreaRenderer.DoRender(const aOpenGL: TF8OpenGL; const aAreaPoints: TRenderDataArray; const aHeightOffset: Double);

    //--------------------------------------------------------------------------
    procedure GetHeight(var terrainHeight: single);
        var
            originHeights, endHeights: HeightResultArrayType;
            originHeight, endHeight: Single;
            i: Integer;
        begin
        terrainHeight := aHeightOffset;
        originHeight := 0.0;
        endHeight    := 0.0;
        originHeights := theApplicationServices.project.GetHeightsAt(aAreaPoints[0][_x], aAreaPoints[0][_z], [_hTerrain, _hRoad, _hIntersection]);
        if Length(originHeights) > 0 then
            begin
            for i := 0 to Length(originHeights) - 1 do
                originHeight := Max(originHeight, originHeights[i].hHeight);
            end
        else
            Assert(False);

        endHeights := theApplicationServices.project.GetHeightsAt(aAreaPoints[1][_x], aAreaPoints[1][_z], [_hTerrain, _hRoad, _hIntersection]);
        if Length(endHeights) > 0 then
            begin
            for i := 0 to Length(endHeights) - 1 do
                endHeight := Max(endHeight, endHeights[i].hHeight);
            end
        else
            Assert(False);

        terrainHeight := Max(originHeight, endHeight) + 0.6;
        end;

    //--------------------------------------------------------------------------
    var
        terrainHeight : single;
    begin
    GetHeight(terrainHeight);
    glMatrixMode(GL_MODELVIEW);
    glPushAttrib(GL_ALL_ATTRIB_BITS);
    glDisable(GL_LIGHTING);
    glDepthMask(GL_FALSE);
    try
        glLineWidth(5.0);
        glColor4f(AREA_NOFOCUS_COLOR[_x], AREA_NOFOCUS_COLOR[_y], AREA_NOFOCUS_COLOR[_z], AREA_NOFOCUS_COLOR[_w]);
        glPushMatrix;
        glBegin(GL_LINE_LOOP);
            glVertex3f(aAreaPoints[0][_x], terrainHeight, aAreaPoints[0][_z]);
            glVertex3f(aAreaPoints[1][_x], terrainHeight, aAreaPoints[0][_z]);
            glVertex3f(aAreaPoints[1][_x], terrainHeight, aAreaPoints[1][_z]);
            glVertex3f(aAreaPoints[0][_x], terrainHeight, aAreaPoints[1][_z]);
        glEnd;
        glPopMatrix;
    finally
        glLineWidth(1.0);
        glColor4f(1.0, 1.0, 1.0, 1.0);
        glPopAttrib;
        end;
    end;

procedure TWQAreaRenderer.MoveCameraToWQArea(const Idx: integer);
    const
        EYE_HEIGHT = 50;
    var
        eye, view : GLPointType;
    begin
    eye[_x]  := Min(p_DrawDataList[Idx][0][_x], p_DrawDataList[Idx][1][_x]) + (Abs(p_DrawDataList[Idx][0][_x] - p_DrawDataList[Idx][1][_x]) / 2);
    eye[_y]  := p_DrawDataList[Idx][0][_y] + EYE_HEIGHT;
    eye[_z]  := Min(p_DrawDataList[Idx][0][_z], p_DrawDataList[Idx][1][_z]) + (Abs(p_DrawDataList[Idx][0][_z] - p_DrawDataList[Idx][1][_z]) / 2);
    view[_x] := Min(p_DrawDataList[Idx][0][_x], p_DrawDataList[Idx][1][_x]) + (Abs(p_DrawDataList[Idx][0][_x] - p_DrawDataList[Idx][1][_x]) / 2);
    view[_y] := p_DrawDataList[Idx][0][_y];
    view[_z] := Min(p_DrawDataList[Idx][0][_z], p_DrawDataList[Idx][1][_z]) + (Abs(p_DrawDataList[Idx][0][_z] - p_DrawDataList[Idx][1][_z]) / 2);
    theApplicationServices.MoveCameraTo(eye, view, 0);
    end;

procedure TWQAreaRenderer.SetIsActiveEditingRender(const aIsActive: boolean);
    begin
    p_IsActiveEditingRender := aIsActive;
    end;
end.
