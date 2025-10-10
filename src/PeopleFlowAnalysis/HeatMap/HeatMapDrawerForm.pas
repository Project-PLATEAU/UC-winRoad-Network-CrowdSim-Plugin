unit HeatMapDrawerForm;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.Generics.Collections,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    Vcl.GraphUtil,
    Vcl.BaseImageCollection,
    Vcl.ImageCollection,
    Vcl.VirtualImage;

const
    FORM_WIDTH_OUTLINE = 16;
    FORM_HEIGHT_OUTLINE = 41;
    LEGEND_GRID = 150;

type
    /// <summary>
    ///    ヒートマップを画像として保存するためのクラス
    /// </summary>
    TClipPanel = class(TCustomControl)
        property Canvas;
    end;

    /// <summary>
    ///    ヒートマップを描画するフォームとその機能を定義するクラス
    /// </summary>
    TFormDrawHeatMap = class(TForm)
        PanelLegend: TPanel;
        ImageLegendBtoG: TImage;
        ImageLegendYtoR: TImage;
        LabelLegend: TLabel;
        LabelMaxNum: TLabel;
        LabelMiddleNum: TLabel;
        LabelMinNum: TLabel;
        PanelHeatMap: TPanel;
        ImageHeatMap: TImage;
        PanelFotter: TPanel;
        ButtonExportImage: TButton;
        ImageLegendGtoY: TImage;
        SaveDialogHeatMapImage: TSaveDialog;
        PanelMain: TPanel;
        GridPanel1: TGridPanel;
        chbNSEW: TCheckBox;
    imgNSEW: TVirtualImage;
    ImageCollection1: TImageCollection;
        procedure FormResize(Sender: TObject);
        procedure ButtonExportImageClick(Sender: TObject);
        procedure FormShow(Sender: TObject);
    procedure chbNSEWClick(Sender: TObject);
        private
            p_DrawData      : TBitmap;
            p_AspectRaitoHM : double;
            p_preWidth      : integer;
            p_preHeight     : integer;
            p_LegendHalfHeightRatio : double;
            p_ResizingWidth  : boolean;
            p_ResizingHeight : boolean;

            procedure SetLegendGrad;
            procedure ChangeLegendLabels(const max, middle, min: integer);
            procedure Draw;
            procedure SetDrawsSize;
            procedure ResizeImages;
        public
            procedure AfterConstruction; override;

            procedure DrawHeatMap(const HMData: TBitmap; MaxNum, MiddleNum, MinNum: integer);
        end;

implementation

{$R *.dfm}

{ TFormDrawHeatMap }
//==============================================================================
procedure TFormDrawHeatMap.AfterConstruction;
    begin
    inherited;
    SetLegendGrad;
    p_preWidth  := Width;
    p_preHeight := Height;
    p_LegendHalfHeightRatio := ImageLegendYtoR.Height / (PanelLegend.Height - ImageLegendYtoR.Top);
    end;

//==============================================================================
procedure TFormDrawHeatMap.SetLegendGrad;
    begin
    GradientFillCanvas(ImageLegendBtoG.Canvas, clLime, clBlue, ImageLegendBtoG.Canvas.ClipRect, gdVertical);
    ImageLegendBtoG.Invalidate;
    GradientFillCanvas(ImageLegendGtoY.Canvas, clYellow, clLime, ImageLegendGtoY.Canvas.ClipRect, gdVertical);
    ImageLegendGtoY.Invalidate;
    GradientFillCanvas(ImageLegendYtoR.Canvas, clRed, clYellow, ImageLegendYtoR.Canvas.ClipRect, gdVertical);
    ImageLegendYtoR.Invalidate;
    end;

//==============================================================================
procedure TFormDrawHeatMap.ButtonExportImageClick(Sender: TObject);
    var
        HMClip   : TBitmap;
        ClipRect : TRect;
    begin
    HMClip := TBitmap.Create;
    HMClip.PixelFormat := pf24bit;
    HMClip.Width  := PanelMain.ClientWidth;
    HMClip.Height := PanelMain.ClientHeight;
    ClipRect := Rect(0, 0, HMClip.Width, HMClip.Height);
    HMClip.Canvas.CopyRect(ClipRect, TClipPanel(PanelMain).Canvas, ClipRect);
    if SaveDialogHeatMapImage.Execute then
        HMClip.SaveToFile(SaveDialogHeatMapImage.FileName);

    FreeAndNil(HMClip);
    end;

//==============================================================================
procedure TFormDrawHeatMap.ChangeLegendLabels(const max, middle, min: integer);
    begin
    LabelMaxNum.Caption    := max.ToString;
    LabelMiddleNum.Caption := middle.ToString;
    LabelMinNum.Caption    := min.ToString+'(人)';
    end;

//==============================================================================
procedure TFormDrawHeatMap.chbNSEWClick(Sender: TObject);
    begin
    imgNSEW.Visible := chbNSEW.Checked;
    end;

//==============================================================================
procedure TFormDrawHeatMap.DrawHeatMap(const HMData: TBitmap; MaxNum, MiddleNum, MinNum: integer);
    begin
    p_DrawData := TBitmap.Create;
    p_DrawData := HMData;
    p_AspectRaitoHM := p_DrawData.Width / p_DrawData.Height;
    ChangeLegendLabels(MaxNum, MiddleNum, MinNum);
    Draw;
    end;

//==============================================================================
procedure TFormDrawHeatMap.Draw;
    var
        DrawRect : TRect;
    begin
    SetDrawsSize;
    DrawRect := Rect(0, 0, ImageHeatMap.Width, ImageHeatMap.Height);
    ImageHeatMap.Canvas.StretchDraw(DrawRect, p_DrawData);
    end;

//==============================================================================
procedure TFormDrawHeatMap.SetDrawsSize;
    begin
    OnResize := nil;
    if (Width <> p_preWidth) then
        begin
        if p_ResizingHeight then
            begin
            p_ResizingHeight := false;
            Width  := p_preWidth;
            Height := p_preHeight;
            end
        else
            begin
            Height := Round(ImageHeatMap.Width / p_AspectRaitoHM) + FORM_HEIGHT_OUTLINE;
            p_preWidth  := Width;
            p_preHeight := Height;
            p_ResizingWidth := true;
            end;
        end
    else if (Height <> p_preHeight) then
        begin
        if p_ResizingWidth then
            begin
            p_ResizingWidth := false;
            Width  := p_preWidth;
            Height := p_preHeight;
            end
        else
            begin
            Width := Round(ImageHeatMap.Height * p_AspectRaitoHM) + LEGEND_GRID + FORM_WIDTH_OUTLINE;
            p_preWidth  := Width;
            p_preHeight := Height;
            p_ResizingHeight := true;
            end;
        end
    else
        begin
        Width  := p_preWidth;
        Height := p_preHeight;
        end;

    ResizeImages;
    end;

//==============================================================================
procedure TFormDrawHeatMap.FormResize(Sender: TObject);
    begin
    if Assigned(p_DrawData) then
        Draw;

    if not Assigned(OnResize) then
        OnResize := FormResize;
    end;

//==============================================================================
procedure TFormDrawHeatMap.FormShow(Sender: TObject);
    var
        DrawRect : TRect;
    begin
    OnResize := nil;
    if Width <> Round(ImageHeatMap.Height * p_AspectRaitoHM) + LEGEND_GRID + FORM_WIDTH_OUTLINE then
        Width := Round(ImageHeatMap.Height * p_AspectRaitoHM) + LEGEND_GRID + FORM_WIDTH_OUTLINE
    else if Height <> Round(ImageHeatMap.Width / p_AspectRaitoHM) + FORM_HEIGHT_OUTLINE then
        Height := Round(ImageHeatMap.Width / p_AspectRaitoHM) + FORM_HEIGHT_OUTLINE;

    ResizeImages;
    OnResize := FormResize;
    DrawRect := Rect(0, 0, ImageHeatMap.Width, ImageHeatMap.Height);
    ImageHeatMap.Canvas.StretchDraw(DrawRect, p_DrawData);
    end;

//==============================================================================
procedure TFormDrawHeatMap.ResizeImages;
    var
        tmpbmp : TBitmap;
    begin
    ImageLegendYtoR.Height := Round((PanelLegend.Height - ImageLegendYtoR.Top) * p_LegendHalfHeightRatio);
    tmpbmp := TBitmap.Create;
    tmpbmp.Width  := ImageLegendYtoR.Width;
    tmpbmp.Height := ImageLegendYtoR.Height;
    ImageLegendYtoR.Picture.Bitmap := tmpbmp;
    FreeAndNil(tmpbmp);

    ImageLegendGtoY.Height := Round(ImageLegendYtoR.Height / 2);
    ImageLegendGtoY.Top    := (ImageLegendYtoR.Top + ImageLegendYtoR.Height) - 1;
    LabelMiddleNum.Top     := ImageLegendGtoY.Top - 10;
    tmpbmp := TBitmap.Create;
    tmpbmp.Width  := ImageLegendGtoY.Width;
    tmpbmp.Height := ImageLegendGtoY.Height;
    ImageLegendGtoY.Picture.Bitmap := tmpbmp;
    FreeAndNil(tmpbmp);

    ImageLegendBtoG.Height := Round(ImageLegendYtoR.Height / 2);
    ImageLegendBtoG.Top    := (ImageLegendGtoY.Top + ImageLegendGtoY.Height) - 1;
    LabelMinNum.Top        := (ImageLegendBtoG.Top + ImageLegendBtoG.Height) - 13;
    tmpbmp := TBitmap.Create;
    tmpbmp.Width  := ImageLegendBtoG.Width;
    tmpbmp.Height := ImageLegendBtoG.Height;
    ImageLegendBtoG.Picture.Bitmap := tmpbmp;
    FreeAndNil(tmpbmp);
    SetLegendGrad;

    tmpbmp := TBitmap.Create;
    tmpbmp.Width  := ImageHeatMap.Width;
    tmpbmp.Height := ImageHeatMap.Height;
    ImageHeatMap.Picture.Bitmap := tmpbmp;
    FreeAndNil(tmpbmp);
    end;
end.
