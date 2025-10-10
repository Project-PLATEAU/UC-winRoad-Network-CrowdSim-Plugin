unit PedestrianMapEditor;

interface

uses
    PluginCore,
    F8OpenGL,
    F8Utils,
    F8GLUtils,
    PedestrianMap,
    PedestrianMapEditingGuide,
    PedestrianUtil;

type
    /// <summary>
    ///    TPedestrianMapを編集するクラス
    ///    2種類の操作が可能
    ///    1. 歩行エリアの編集
    ///        TPedestrianCellのStatus情報を操作する
    ///    2. 横断歩道の編集
    ///        TPedestrianCellGroup_CrossWalkで横断歩道の作成・編集・削除を行う
    /// </summary>
    TPedestrianMapEditor = class
        private
            p_API    : IF8ApplicationServices;
            p_EditMap: TPedestrianMap;
            p_Coord  : IF8WrHorizontalCoordinateConvertor;

            p_Enabled: Boolean;
            p_WriteStatus: PedestrianAreaStatus;
            p_EditingGuide: TPedestrianMapEditingGuide;
            p_StatusEditSize: Cardinal;
            p_CrosswalkEditSize: Cardinal;

            procedure SetEnabled(const aValue: Boolean);
            procedure SetWriteStatus(const aValue: PedestrianAreaStatus);
            procedure SetEditMap(const aValue: TPedestrianMap);
            procedure SetStatusEditSize(const aValue: Cardinal);
            procedure SetCrosswalkEditSize(const aValue: Cardinal);
            function  GetIsEditingCrossWalk: Boolean;
            function  GetEditingCrosswalkIdx: Integer;

            procedure OnFormMainTerrainClick(const ClickPoint : GLPointType; const ClickTri : F8TriangleType; var ClickCommand : ModelSelectCommandType);
            function  ConvertGLToLocal(const aPositionGL: TPoint3D): TPoint3D;
        public
            constructor Create(const aAPI: IF8ApplicationServices);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure AddCrossWalk;
            procedure DeleteCrossWalk(const aIdx: Integer);
            procedure BeginEditingCrossWalk(const aIdx: Integer);
            function  EndEditingCrossWalk: Boolean;
            procedure ChangeCrossWalkFirstStatus(const aSignal: PedestrianAreaCrossingSignal);
            procedure ChangeCrosswalkToAddCellMode;
            procedure ChangeCrosswalkToRemoveCellMode;
            procedure UpdateUnderMouseGuide(const aGLPosition: TPoint3D);
            procedure DoEditingGuideRender(const aOpenGL: TF8OpenGL);

            property  Enabled           : Boolean              read p_Enabled     write SetEnabled;
            property  WriteStatus       : PedestrianAreaStatus read p_WriteStatus write SetWriteStatus;
            property  EditMap           : TPedestrianMap       read p_EditMap     write SetEditMap;
            property  IsEditingCrossWalk: Boolean              read GetIsEditingCrossWalk;
            property  EditingCrosswalIdx: Integer              read GetEditingCrosswalkIdx;
            property  StatusEditSize    : Cardinal             read p_StatusEditSize    write SetStatusEditSize;
            property  CrosswalkEditSize : Cardinal             read p_CrosswalkEditSize write SetCrosswalkEditSize;
        end;

implementation

uses
    System.SysUtils,
    System.Generics.Collections,
    PedestrianCell,
    CellID;

{ TPedestrianMapEditor }
constructor TPedestrianMapEditor.Create(const aAPI: IF8ApplicationServices);
    begin
    p_API := aAPI;
    Assert(Assigned(p_API));
    end;

procedure TPedestrianMapEditor.AfterConstruction;
    var
        method : TMethod;
    begin
    inherited;

    p_Coord   := p_API.GetWRCoordinateConvertor.HoirizontalCSConvertor;
    p_EditingGuide := TPedestrianMapEditingGuide.Create;
    p_Enabled := False;
    p_EditMap := nil;
    p_StatusEditSize := 0;
    p_CrosswalkEditSize := 0;

    FormMainTerrainClickProc(method) := OnFormMainTerrainClick;
    p_API.RegisterEventHandler(_plgFormMainTerrainClick, method);
    end;

procedure TPedestrianMapEditor.BeforeDestruction;
    var
        method : TMethod;
    begin
    inherited;

    FormMainTerrainClickProc(method) := OnFormMainTerrainClick;
    p_API.UnRegisterEventHandler(_plgFormMainTerrainClick, method);

    p_Coord   := nil;
    p_Enabled := False;
    p_EditMap := nil;
    FreeAndNil(p_EditingGuide);
    end;

procedure TPedestrianMapEditor.AddCrossWalk;
    begin
    if Enabled then
        p_EditMap.CrossWalks.AddNewCrossWalk;
    end;

procedure TPedestrianMapEditor.DeleteCrossWalk(const aIdx: Integer);
    begin
    if Enabled then
        p_EditMap.CrossWalks.DeleteCrossWalk(aIdx);
    end;

procedure TPedestrianMapEditor.BeginEditingCrossWalk(const aIdx: Integer);
    begin
    if Assigned(p_EditMap) and (not p_EditMap.CrossWalks.IsEditing) then
        p_EditMap.CrossWalks.BeginEditCrossWalk(aIdx);
    end;

function  TPedestrianMapEditor.EndEditingCrossWalk: Boolean;
    begin
    if Assigned(p_EditMap) and p_EditMap.CrossWalks.IsEditing then
        Result := p_EditMap.CrossWalks.EndEditCrossWalk
    else
        Result := True;
    end;

procedure TPedestrianMapEditor.ChangeCrossWalkFirstStatus(const aSignal: PedestrianAreaCrossingSignal);
    begin
    if p_EditMap.CrossWalks.IsEditing then
        p_EditMap.CrossWalks.EditCrossWalk.FirstSignal := aSignal;
    end;

procedure TPedestrianMapEditor.ChangeCrosswalkToAddCellMode;
    begin
    if p_EditMap.CrossWalks.IsEditing then
        p_EditMap.CrossWalks.ChangeToAddCellMode;
    end;

procedure TPedestrianMapEditor.ChangeCrosswalkToRemoveCellMode;
    begin
    if p_EditMap.CrossWalks.IsEditing then
        p_EditMap.CrossWalks.ChangeToRemoveCellMode;
    end;

procedure TPedestrianMapEditor.UpdateUnderMouseGuide(const aGLPosition: TPoint3D);
    var
        localPosition: TPoint3D;
    begin
    if not Enabled then
        Exit;

    localPosition := ConvertGLToLocal(aGLPosition);

    if p_EditMap.CrossWalks.IsEditing then
        p_EditingGuide.UpdateMouseAreaGuide(p_EditMap, localPosition, CrosswalkEditSize)
    else
        p_EditingGuide.UpdateMouseAreaGuide(p_EditMap, localPosition, StatusEditSize);
    end;

procedure TPedestrianMapEditor.DoEditingGuideRender(const aOpenGL: TF8OpenGL);
    begin
    if not Enabled then
        Exit;

    p_EditingGuide.DoRender(aOpenGL);
    end;

procedure TPedestrianMapEditor.SetEnabled(const aValue: Boolean);
    begin
    EndEditingCrossWalk;
    p_Enabled := Assigned(EditMap) and aValue;
    end;

procedure TPedestrianMapEditor.SetWriteStatus(const aValue: PedestrianAreaStatus);
    begin
    p_WriteStatus := aValue;
    end;

procedure TPedestrianMapEditor.SetEditMap(const aValue: TPedestrianMap);
    begin
    if Assigned(aValue) then
        p_EditMap := aValue
    else
        p_EditMap := nil;
    end;

procedure TPedestrianMapEditor.SetStatusEditSize(const aValue: Cardinal);
    begin
    p_StatusEditSize := aValue;
    end;

procedure TPedestrianMapEditor.SetCrosswalkEditSize(const aValue: Cardinal);
    begin
    p_CrosswalkEditSize := aValue;
    end;

function TPedestrianMapEditor.GetIsEditingCrossWalk: Boolean;
    begin
    Result := p_EditMap.CrossWalks.IsEditing;
    end;

function TPedestrianMapEditor.GetEditingCrosswalkIdx: Integer;
    begin
    Result := p_EditMap.Crosswalks.EditingIdx;
    end;

procedure TPedestrianMapEditor.OnFormMainTerrainClick(const ClickPoint : GLPointType; const ClickTri : F8TriangleType; var ClickCommand : ModelSelectCommandType);
    var
        cells: TObjectList<TPedestrianCell>;
        i: Integer;
    begin
    ClickCommand := _msc_none;
    if not Enabled then
        Exit;

    if p_EditMap.CrossWalks.IsEditing then
        begin
        if p_EditMap.RequireCellsInfo(ConvertGLToLocal(Point3D(ClickPoint[_x], ClickPoint[_y], ClickPoint[_z])), CrosswalkEditSize, cells) then
            begin
            p_EditMap.CrossWalks.AddOrRemoveCells(cells);
            FreeAndNil(cells);
            end;
        end
    else
        begin
        if p_EditMap.RequireCellsInfo(ConvertGLToLocal(Point3D(ClickPoint[_x], ClickPoint[_y], ClickPoint[_z])), StatusEditSize, cells) then
            begin
            for i := 0 to cells.Count - 1 do
                begin
                if cells[i].status <> WriteStatus then
                    p_EditMap.ChangeCellState(cells[i].CellID.id, WriteStatus);
                end;
            FreeAndNil(cells);
            end;
        end;
    end;

function TPedestrianMapEditor.ConvertGLToLocal(const aPositionGL: TPoint3D): TPoint3D;
    var
        srcPoint, dstPoint: F8PointType;
    begin
    srcPoint[_x] := aPositionGL.X;
    srcPoint[_y] := aPositionGL.Z;

    p_Coord.Convert(_hctOpenGL_XZ, 0, _hctLocal_XY, 0, srcPoint, dstPoint);

    Result.X := dstPoint[_x];
    Result.Y := aPositionGL.Y;
    Result.Z := dstPoint[_y];
    end;
end.
