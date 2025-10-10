unit F_CrossSectionGraph;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.DateUtils,
    System.Variants,
    System.Classes,
    System.Generics.Collections,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    Vcl.ComCtrls,
    F8RealSpinEdit,
    VCLTee.TeeGDIPlus,
    VCLTee.TeEngine,
    VCLTee.Series,
    VCLTee.TeeProcs,
    VCLTee.Chart,
    PedestrianMapUser;

type
    /// <summary>
    ///    �f�ʌ�ʗ���͌��ʂ̃O���t��`�悷��t���[���Ƃ��̋@�\���`����N���X
    /// </summary>
    TFrameCrossSectionGraph = class(TFrame)
        PanelCrossSectionGraph: TPanel;
        ChartCrossSection: TChart;
        PanelExportButton: TPanel;
        ButtonExportGraphImage: TButton;
        SaveDialogGraphImage: TSaveDialog;
        procedure ButtonExportGraphImageClick(Sender: TObject);
        private
            p_PedestrianMapUser : TPedestrianMapUser;
        public
            procedure OnSimStop;

            property  PedestrianMapUser: TPedestrianMapUser read p_PedestrianMapUser write p_PedestrianMapUser;
        end;

implementation

{$R *.dfm}

type
    /// <summary>
    ///    �f�ʌ�ʗ���͌��ʂ̃O���t���摜�Ƃ��ĕۑ����邽�߂̃N���X
    /// </summary>
    TClipPanel = class(TCustomControl)
        property Canvas;
    end;

{ TFrameCrossSectionGraph }
//==============================================================================
procedure TFrameCrossSectionGraph.ButtonExportGraphImageClick(Sender: TObject);
    var
        GraphClip : TBitmap;
        ClipRect  : TRect;
    begin
    GraphClip := TBitmap.Create;
    GraphClip.PixelFormat := pf24bit;
    GraphClip.Width       := ChartCrossSection.ClientWidth;
    GraphClip.Height      := ChartCrossSection.ClientHeight;
    ClipRect := Rect(0, 0, GraphClip.Width, GraphClip.Height);
    GraphClip.Canvas.CopyRect(ClipRect, TClipPanel(ChartCrossSection).Canvas, ClipRect);
    if SaveDialogGraphImage.Execute then
        GraphClip.SaveToFile(SaveDialogGraphImage.FileName);

    FreeAndNil(GraphClip);
    end;

//==============================================================================
procedure TFrameCrossSectionGraph.OnSimStop;
    begin
    if Assigned(p_PedestrianMapUSer) then
        begin
        if Assigned(p_PedestrianMapUser.CrossSectionFlowLog) then
            begin
            p_PedestrianMapUser.CrossSectionFlowLog.SetGraphData(ChartCrossSection);
            end;
        end;
    end;
end.
