unit F_AgentAttributes;

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    F8RealSpinEdit,
    VclTee.TeeGDIPlus,
    VCLTee.TeEngine,
    VCLTee.Series,
    VCLTee.TeeProcs,
    VCLTee.Chart, System.ImageList, Vcl.ImgList, Vcl.Buttons;

type
    TFrameAgentAttributes = class(TFrame)
        PanelFAAMain: TPanel;
        GroupBoxAttributesSetting: TGroupBox;
        PanelDestination: TPanel;
        seDestPer: TF8RealSpinEdit;
        cbbDest: TComboBox;
        Label8: TLabel;
        Label9: TLabel;
        ChartGoalDistribution: TChart;
        SeriesGoalDistribution: TPieSeries;
        ImageList1: TImageList;
        private

        public

        end;

implementation

{$R *.dfm}

end.
