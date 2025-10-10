unit F_ODTripGraph;

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
    VclTee.TeeGDIPlus,
    VCLTee.TeEngine,
    VCLTee.TeeProcs,
    VCLTee.Chart,
    VCLTee.Series,
    Vcl.ExtCtrls,
    Vcl.StdCtrls,
    TimeLineSeriesSupport,
    ODTripResultExport;

type
    TFrameODTripGraph = class(TFrame)
        PanelODTripGraph: TPanel;
        ChartODTripGraph: TChart;
        PanelExportButton: TPanel;
        ButtonExportGraphImage: TButton;
        SaveDialogGraphImage: TSaveDialog;
        procedure ButtonExportGraphImageClick(Sender: TObject);
        private
            p_TripTimeAverageGraph: TTimeLineGraphList;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure UpdateData(const aData: TODTripResultExport);
        end;

implementation

uses
    System.DateUtils,
    System.Types,
    F8Utils;

type
    /// <summary>
    ///    断面交通流解析結果のグラフを画像として保存するためのクラス
    /// </summary>
    TClipPanel = class(TCustomControl)
        property Canvas;
    end;

const
    AXIS_LABEL_MINUTES = '分';
    AXIS_LABEL_TIME    = '時刻';

{$R *.dfm}

{ TFrameODTripGraph }
procedure TFrameODTripGraph.AfterConstruction;
    begin
    inherited;

    p_TripTimeAverageGraph := TTimeLineGraphList.Create(AXIS_LABEL_MINUTES, AXIS_LABEL_TIME);
    end;

procedure TFrameODTripGraph.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_TripTimeAverageGraph);
    end;

procedure TFrameODTripGraph.UpdateData(const aData: TODTripResultExport);
    var
        newSeriesName, tmpSeriesName: String;
        newSeries: TLineGraphSeriesData;
        logStartTime: TDateTime;

        i, j: Integer;
        logdata: TODTripLogData;
        idx: Integer;
        logRes: TODAggregateResult;
        logLabel: String;
    begin
    p_TripTimeAverageGraph.ClearAll;
    p_TripTimeAverageGraph.AddBottomAxisData(aData.simulationStartTime, aData.SimulationEndTime, 1);

    // Create Graph
    // OD
    for i := 0 to aData.ODLogCount - 1 do
        begin
        logdata := aData.ODLog[i];
        // OD, label
        for j := 0 to logdata.PedestrianLabelCount - 1 do
            begin
            logLabel := logData.PedestrianLabel[j];
            newSeriesName := 'line_' + logData.ID.OriginNodeName + '_' + logdata.ID.DestinationNodeName + '_' + logLabel;
            if p_TripTimeAverageGraph.ContainsSeries(newSeriesName) then
                begin
                idx := 0;
                tmpSeriesName := newSeriesName;
                while True do
                    begin
                    newSeriesName := Format('%s_%d', [tmpSeriesName, idx]);
                    if not p_TripTimeAverageGraph.ContainsSeries(newSeriesName) then
                        Break;
                    Inc(idx);
                    end;
                end;

            newSeries := TLineGraphSeriesData.Create(newSeriesName);
            logStartTime := aData.SimulationStartTime;
            idx := 0;
            // データの追加
            // 出現人数0は除外
            while True do
                begin
                logRes := logData.CalcSpecificPeriod(logLabel, logStartTime);
                if logRes.NumberOfPerson <> 0 then
                    newSeries.AddPoint(F8Point(idx, logRes.TripTimeAverage / 60));
                logStartTime := IncMinute(logStartTime, 1);
                Inc(idx);
                if CompareTime(logStartTime, aData.SimulationEndTime) = GreaterThanValue then
                    Break;
                end;

            p_TripTimeAverageGraph.AddNewGraphData(newSeries);
            end;
        end;

    // Apply
    p_TripTimeAverageGraph.ApplyGraph(ChartODTripGraph);
    end;

procedure TFrameODTripGraph.ButtonExportGraphImageClick(Sender: TObject);
    var
        GraphClip : TBitmap;
        ClipRect  : TRect;
    begin
    GraphClip := TBitmap.Create;
    GraphClip.PixelFormat := pf24bit;
    GraphClip.Width       := ChartODTripGraph.ClientWidth;
    GraphClip.Height      := ChartODTripGraph.ClientHeight;
    ClipRect := Rect(0, 0, GraphClip.Width, GraphClip.Height);
    GraphClip.Canvas.CopyRect(ClipRect, TClipPanel(ChartODTripGraph).Canvas, ClipRect);
    if SaveDialogGraphImage.Execute then
        GraphClip.SaveToFile(SaveDialogGraphImage.FileName);

    FreeAndNil(GraphClip);
    end;
end.

