unit MapRenderer;

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
    HEIGHT_FROM_ZERO = 50.0;

type
    /// <summary>
    ///    シミュレーションエリアを示すメッシュを描画するクラス
    /// </summary>
    TMapRenderer = class
        private
            p_Data              : VertexDataArray;
            p_vertices          : array of array [0..2] of GLfloat;
            p_offsetedVertices  : array of array [0..2] of GLfloat;
            p_walkable          : array of GLfloat;
            p_Shader            : ShaderProgramClass;
            p_vertex_vbo        : GLUint;
            p_walkable_vbo      : GLUint;
            p_VertexArrayObject : GLUint;
            p_HeightOffset      : Double;
            p_IsFirstRendering  : Boolean;
            p_holdShaderProjectFilePath: String;

            procedure SetheightOffset(const aValue: Double);
            function  GetHeightOffset: Double;

            procedure SetUp;
            procedure BindVertexBuffer;
            procedure SetUpShader(const aOpenGL: TF8OpenGL);
            procedure ResetShader;
            procedure UpDateWalkableBuffer;
            function GetWalkable(const idx: Integer): GLfloat;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure SetWalkable(const idx: integer; boolval: GLUint);
            procedure Initialize(const aData: VertexDataArray; CellSize: Double);
            procedure DoRender(const aOpenGL: TF8OpenGL; CellSize: Double);

            property Walkable[const idx: Integer] : GLfloat read GetWalkable;
            property HeightOffset                 : Double  read GetHeightOffset write SetHeightOffset;
        end;

implementation

uses
    Winapi.Windows,
    VCL.Forms;

{ TMapRenderer }
//==============================================================================
procedure TMapRenderer.AfterConstruction;
    begin
    inherited;

    p_IsFirstRendering := True;
    p_HeightOffset := 0.0;
    end;

procedure TMapRenderer.BeforeDestruction;
    begin
    inherited;
    glDeleteVertexArrays(1, @p_VertexArrayObject);
    glDeleteBuffers(1, @p_vertex_vbo);
    end;

//==============================================================================
procedure TMapRenderer.Initialize(const aData: VertexDataArray; CellSize: Double);
    var
        i: Integer;
    begin
    SetLength(p_Data, Length(aData));
    CopyMemory(p_Data, aData, SizeOf(TVertexData) * Length(aData));
    SetLength(p_vertices, Length(p_Data));
    SetLength(p_walkable, Length(p_Data));
    for i := 0 to Length(p_Data) - 1 do
        begin
        p_vertices[i][0] := p_Data[i].X;
        p_vertices[i][1] := p_Data[i].Y;
        p_vertices[i][2] := p_Data[i].Z;
        p_walkable[i]    := p_Data[i].Status;
        end;
    p_IsFirstRendering := True;
    end;

//==============================================================================
procedure TMapRenderer.SetWalkable(const idx: integer; boolval: GLUint);
    begin
    Assert(InRange(idx, 0, Length(p_walkable) - 1));
    if (p_walkable[idx] <> boolval) then
        p_walkable[idx] := boolval;
    end;

//==============================================================================
function TMapRenderer.GetWalkable(const idx: Integer): GLfloat;
    begin
    Assert(InRange(idx, 0, Length(p_walkable) - 1));
    Result := p_walkable[idx];
    end;

//==============================================================================
procedure TMapRenderer.DoRender(const aOpenGL: TF8OpenGL; CellSize: Double);
    var
        glprojMat, glviewMat : FloatMatrixType;
        vao    : GLuint;
    begin
    if p_IsFirstRendering then
        begin
        Setup;
        p_IsFirstRendering := False;
        end;

    SetUpShader(aOpenGL);
    glGetFloatV(GL_PROJECTION_MATRIX, @glprojMat);
    glGetFloatV(GL_MODELVIEW_MATRIX, @glviewMat);

    if Assigned(p_Shader) then
        begin
        p_Shader.SetUniformMatrixVariable4fv('ProjMat', 1, GL_FALSE, @glprojMat[0]);
        p_Shader.SetUniformMatrixVariable4fv('ModelMat', 1, GL_FALSE, @glviewMat[0]);
        p_Shader.SetUniformVariable1f('CellSize', CellSize);
        end;

    vao := p_VertexArrayObject;
    glBindVertexArray(vao);
    UpDateWalkableBuffer;
    glPushAttrib(GL_ALL_ATTRIB_BITS);
    try
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glDisable(GL_CULL_FACE);
        glDrawArrays(GL_POINTS, 0, Length(p_vertices));
        glBindVertexArray(0);
        glEnable(GL_CULL_FACE);
        glDisable(GL_BLEND);
    finally
        glPopAttrib;
        TheShaderManager.PopActiveShader;
        glDeleteBuffers(1, @p_walkable_vbo);
        ResetShader;
        end;
    end;

//==============================================================================
procedure TMapRenderer.SetheightOffset(const aValue: Double);
    begin
    if p_HeightOffset <> aValue then
        begin
        p_HeightOffset := aValue;
        p_IsFirstRendering := True;
        end;
    end;

//==============================================================================
function TMapRenderer.GetHeightOffset: Double;
    begin
    Result := p_HeightOffset;
    end;

//==============================================================================
procedure TMapRenderer.SetUp;
    begin
    theApplicationServices.mainForm.openGL.MakeCurrent(True);
    glGenVertexArrays(1, @p_VertexArrayObject);
    glBindVertexArray(p_VertexArrayObject);
    BindVertexBuffer;
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
    end;

//==============================================================================
procedure TMapRenderer.BindVertexBuffer;
    var
        i: Integer;
    begin
    SetLength(p_offsetedVertices, Length(p_vertices));
    for i := 0 to Length(p_offsetedVertices) - 1 do
        begin
        p_offsetedVertices[i][0] := p_vertices[i][0];
        p_offsetedVertices[i][1] := p_vertices[i][1] + HeightOffset;
        p_offsetedVertices[i][2] := p_vertices[i][2];
        end;

    glGenBuffers(1, @p_vertex_vbo);
    glBindBuffer(GL_ARRAY_BUFFER, p_vertex_vbo);
    glBufferData(GL_ARRAY_BUFFER, Length(p_offsetedVertices) * 3 * sizeof(GLFloat), @p_offsetedVertices[0], GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nil);
    end;

//==============================================================================
procedure TMapRenderer.UpDateWalkableBuffer;
    begin
    glGenBuffers(1, @p_walkable_vbo);
    glBindBuffer(GL_ARRAY_BUFFER, p_walkable_vbo);
    glBufferData(GL_ARRAY_BUFFER, Length(p_walkable) * sizeof(GLfloat), @p_walkable[0], GL_DYNAMIC_DRAW);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 1, GL_FLOAT, GL_FALSE, 0, nil);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    end;

//==============================================================================
/// <summary>メッシュ描画用のシェーダをコンパイルする関数</summary>
procedure TMapRenderer.SetUpShader(const aOpenGL: TF8OpenGL);
    const
        SHADER_PATH = '\Shaders\Plugins\CrowdSimMesh';
        SHADER_NAME = 'CrowdSimMesh';
    begin
    p_holdShaderProjectFilePath := TheShaderManager.shaderProjectFilesPath;
    TheShaderManager.ShaderProjectFilesPath := ExtractFileDir(Application.ExeName) + SHADER_PATH;
    TheApplicationServices.MainForm.ReloadAllShadersIfNeeded(aOpenGL);

    if p_Shader = nil then
        p_Shader := TheShaderManager.GetShaderProgram(SHADER_NAME);
    if p_shader = nil then
        begin
        p_Shader := TheShaderManager.CreateShaderProgram(SHADER_NAME);
        p_Shader.ignoreUndefinedVariables := true;
        TheShaderManager.CompileAllPrograms;
        end;
    TheShaderManager.PushActiveShader;
    p_Shader.UseShader;
    end;

procedure TMapRenderer.ResetShader;
    begin
    TheShaderManager.shaderProjectFilesPath := p_holdShaderProjectFilePath;
    end;
end.
