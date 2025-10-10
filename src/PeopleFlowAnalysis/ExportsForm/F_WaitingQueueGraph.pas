unit F_WaitingQueueGraph;

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
    VclTee.TeeGDIPlus,
    VCLTee.TeEngine,
    VCLTee.TeeProcs,
    VCLTee.Chart,
    VCLTee.Series,
    F8Utils,
    WaitingQueueResultExport,
    TimeLineSeriesSupport;

type

    TFrameWaitingQueueGraph = class(TFrame)
        PanelSelectDisplayItem: TPanel;
        RadioButtonAverageWaitTime: TRadioButton;
        RadioButtonWaitNum: TRadioButton;
        RadioButtonReleaseNum: TRadioButton;
        PanelExportButton: TPanel;
        ButtonExportGraphImage: TButton;
        PanelWaitingQueueGraph: TPanel;
        RadioButtonEnterNum: TRadioButton;
        RadioButtonTotalReleaseNum: TRadioButton;
        ChartWaitingQueueGraph: TChart;
        SaveDialogGraphImage: TSaveDialog;
        procedure RadioButtonAverageWaitTimeClick(Sender: TObject);
        procedure RadioButtonWaitNumClick(Sender: TObject);
        procedure RadioButtonEnterNumClick(Sender: TObject);
        procedure RadioButtonReleaseNumClick(Sender: TObject);
        procedure RadioButtonTotalReleaseNumClick(Sender: TObject);
        procedure ButtonExportGraphImageClick(Sender: TObject);
        private
            p_Data: TWaitingQueueResultExport;

            p_AverageWaitTimeGraph: TTimeLineGraphList;
            p_WaitNumGraph        : TTimeLineGraphList;
            p_EnterNumGraph       : TTimeLineGraphList;
            p_ReleaseNumGraph     : TTimeLineGraphList;
            p_TotalReleaSeNumGraph: TTimeLineGraphList;

            procedure ChangeUIEnabled(const aEnabled: Boolean);
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure UpdateData(const aData: TWaitingQueueResultExport);
        end;

implementation

uses
    System.Types,
    System.Math,
    System.DateUtils,
    System.Generics.Defaults;

type
    /// <summary>
    ///    断面交通流解析結果のグラフを画像として保存するためのクラス
    /// </summary>
    TClipPanel = class(TCustomControl)
        property Canvas;
    end;

const
    AXIS_LABEL_MINUTES          = '分';
    AXIS_LABEL_NUMBER_OF_PERSON = '人';
    AXIS_LABEL_TIME             = '時刻';

{$R *.dfm}

{ TFrameWaitingQueueGraph }
procedure TFrameWaitingQueueGraph.AfterConstruction;
    begin
    inherited;

    p_AverageWaitTimeGraph := TTimeLineGraphList.Create(AXIS_LABEL_MINUTES, AXIS_LABEL_TIME);
    p_WaitNumGraph         := TTimeLineGraphList.Create(AXIS_LABEL_NUMBER_OF_PERSON, AXIS_LABEL_TIME);
    p_EnterNumGraph        := TTimeLineGraphList.Create(AXIS_LABEL_NUMBER_OF_PERSON, AXIS_LABEL_TIME);
    p_ReleaseNumGraph      := TTimeLineGraphList.Create(AXIS_LABEL_NUMBER_OF_PERSON, AXIS_LABEL_TIME);
    p_TotalReleaSeNumGraph := TTimeLineGraphList.Create(AXIS_LABEL_NUMBER_OF_PERSON, AXIS_LABEL_TIME);
    end;

procedure TFrameWaitingQueueGraph.BeforeDestruction;
    begin
    inherited;

    if Assigned(p_Data) then
        FreeAndNil(p_Data);

    FreeAndNil(p_AverageWaitTimeGraph);
    FreeAndNil(p_WaitNumGraph);
    FreeAndNil(p_EnterNumGraph);
    FreeAndNil(p_ReleaseNumGraph);
    FreeAndNil(p_TotalReleaSeNumGraph);
    end;

procedure TFrameWaitingQueueGraph.UpdateData(const aData: TWaitingQueueResultExport);
    var
        i, j: Integer;
        waitingQueue: TWaitingQueueLogData;
        newAverageWaitTimeSeries, newWaitNumSeries,
        newEnterNumSeries, newReleaseNumSeries, newTotalReleaseNumSeries: TLineGraphSeriesData;
    begin
    if Assigned(p_Data) then
        begin
        FreeAndNil(p_Data);

        p_AverageWaitTimeGraph.ClearAll;
        p_WaitNumGraph.ClearAll;
        p_EnterNumGraph.ClearAll;
        p_ReleaseNumGraph.ClearAll;
        p_TotalReleaseNumGraph.ClearAll;
        end;
    // Reset UI
    RadioButtonAverageWaitTime.Checked := True;

    p_Data := aData.Clone;

    // Updata Graphs
    ChangeUIEnabled(Assigned(p_Data));
    if Assigned(p_Data) then
        begin

        p_AverageWaitTimeGraph.AddBottomAxisData(p_Data.SimulationStartTime, p_Data.SimulationEndTime, p_Data.OutputIntervalMinutes);
        p_WaitNumGraph.AddBottomAxisData(p_Data.SimulationStartTime, p_Data.SimulationEndTime, p_Data.OutputIntervalMinutes);
        p_EnterNumGraph.AddBottomAxisData(p_Data.SimulationStartTime, p_Data.SimulationEndTime, p_Data.OutputIntervalMinutes);
        p_ReleaseNumGraph.AddBottomAxisData(p_Data.SimulationStartTime, p_Data.SimulationEndTime, p_Data.OutputIntervalMinutes);
        p_TotalReleaSeNumGraph.AddBottomAxisData(p_Data.SimulationStartTime, p_Data.SimulationEndTime, p_Data.OutputIntervalMinutes);

        for i := 0 to p_Data.WaitingQueueLogCount - 1 do
            begin
            waitingQueue := p_Data.WaitingQueueLog[i];

            if waitingQueue.LogCount < 1 then
                Continue;

            newAverageWaitTimeSeries := TLineGraphSeriesData.Create(waitingQueue.ID.WaitingQueueName);
            newWaitNumSeries         := TLineGraphSeriesData.Create(waitingQueue.ID.WaitingQueueName);
            newEnterNumSeries        := TLineGraphSeriesData.Create(waitingQueue.ID.WaitingQueueName);
            newReleaseNumSeries      := TLineGraphSeriesData.Create(waitingQueue.ID.WaitingQueueName);
            newTotalReleaseNumSeries := TLineGraphSeriesData.Create(waitingQueue.ID.WaitingQueueName);

            for j := 0 to waitingQueue.LogCount - 1 do
                begin
                newAverageWaitTimeSeries.AddPoint(F8Point(j, Trunc(waitingQueue.Log[j].AverageWait / 60)));
                newWaitNumSeries.AddPoint(F8Point(j, waitingQueue.Log[j].WaitNum));
                newEnterNumSeries.AddPoint(F8Point(j, waitingQueue.Log[j].EnterNum));
                newReleaseNumSeries.AddPoint(F8Point(j, waitingQueue.Log[j].ReleaseNum));
                newTotalReleaseNumSeries.AddPoint(F8Point(j, waitingQueue.Log[j].TotalReleaseNum));
                end;

            p_AverageWaitTimeGraph.AddNewGraphData(newAverageWaitTimeSeries);
            p_WaitNumGraph.AddNewGraphData(newWaitNumSeries);
            p_EnterNumGraph.AddNewGraphData(newEnterNumSeries);
            p_ReleaseNumGraph.AddNewGraphData(newReleaseNumSeries);
            p_TotalReleaseNumGraph.AddNewGraphData(newTotalReleaseNumSeries);
            end;

        p_AverageWaitTimeGraph.ApplyGraph(ChartWaitingQueueGraph);
        end;
    end;

procedure TFrameWaitingQueueGraph.RadioButtonAverageWaitTimeClick(Sender: TObject);
    begin
    if Assigned(p_Data) then
        p_AverageWaitTimeGraph.ApplyGraph(ChartWaitingQueueGraph, True);
    end;

procedure TFrameWaitingQueueGraph.RadioButtonWaitNumClick(Sender: TObject);
    begin
    if Assigned(p_Data) then
        p_WaitNumGraph.ApplyGraph(ChartWaitingQueueGraph, True);
    end;

procedure TFrameWaitingQueueGraph.RadioButtonEnterNumClick(Sender: TObject);
    begin
    if Assigned(p_Data) then
        p_EnterNumGraph.ApplyGraph(ChartWaitingQueueGraph, True);
    end;

procedure TFrameWaitingQueueGraph.RadioButtonReleaseNumClick(Sender: TObject);
    begin
    if Assigned(p_Data) then
        p_ReleaseNumGraph.ApplyGraph(ChartWaitingQueueGraph, True);
    end;

procedure TFrameWaitingQueueGraph.RadioButtonTotalReleaseNumClick(Sender: TObject);
    begin
    if Assigned(p_Data) then
        p_TotalReleaseNumGraph.ApplyGraph(ChartWaitingQueueGraph, True);
    end;

procedure TFrameWaitingQueueGraph.ButtonExportGraphImageClick(Sender: TObject);
    var
        GraphClip : TBitmap;
        ClipRect  : TRect;
    begin
    GraphClip := TBitmap.Create;
    GraphClip.PixelFormat := pf24bit;
    GraphClip.Width       := ChartWaitingQueueGraph.ClientWidth;
    GraphClip.Height      := ChartWaitingQueueGraph.ClientHeight;
    ClipRect := Rect(0, 0, GraphClip.Width, GraphClip.Height);
    GraphClip.Canvas.CopyRect(ClipRect, TClipPanel(ChartWaitingQueueGraph).Canvas, ClipRect);
    if SaveDialogGraphImage.Execute then
        GraphClip.SaveToFile(SaveDialogGraphImage.FileName);

    FreeAndNil(GraphClip);
    end;

procedure TFrameWaitingQueueGraph.ChangeUIEnabled(const aEnabled: Boolean);
    begin
    RadioButtonAverageWaitTime.Enabled := aEnabled;
    RadioButtonWaitNum.Enabled         := aEnabled;
    RadioButtonEnterNum.Enabled        := aEnabled;
    RadioButtonReleaseNum.Enabled      := aEnabled;
    RadioButtonTotalReleaseNum.Enabled := aEnabled;
    ButtonExportGraphImage.Enabled     := aEnabled;

    ChartWaitingQueueGraph.Enabled     := aEnabled;
    end;
end.
