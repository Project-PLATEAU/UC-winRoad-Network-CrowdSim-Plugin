unit F_BusTransportationGraph;

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
    VclTee.TeeGDIPlus,
    Vcl.StdCtrls,
    VCLTee.TeEngine,
    VCLTee.TeeProcs,
    VCLTee.Chart,
    VCLTee.Series,
    Vcl.ExtCtrls,
    F8Utils,
    BusTransportationResultExport;

type
    TBarGraphSeriesData = class
        private
            p_SeriesName: String;
            p_GraphPoint: F8PointType;

            function  GetGraphPoint: F8PointType;
        public
            constructor Create(const aSeriesName: String; const aYValue: Double);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            property  SeriesName : String      read p_SeriesName;
            property  GraphPoint : F8PointType read GetGraphPoint;
        end;

    TBarGraphSeriesDataList = class
        private
            p_Data: TObjectList<TBarGraphSeriesData>;// BudIdとSimTimeごとに系列化

            p_LeftAxisName  : String;
            p_BottomAxisName: String;

            procedure SetLeftAxisName(const aValue: String);
            procedure SetBottomAxisName(const aValue: String);
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure ClearAllPoints;
            procedure AddBar(const aSeriesName: String; const aBarValue: Double);
            procedure ApplyData(const aChart: TChart; const aFixedMinMax: Boolean = False; const aFixedMin: Integer = 0; aFixedMax: Integer = 1);

            property  LeftAxisname  : String  read p_LeftAxisName   write SetLeftAxisName;
            property  BottomAxisName: String  read p_BottomAxisName write SetBottomAxisName;
        end;

    TBusStopGraphList = class
        private
            p_BusStopInfo: BusStopInfo;

            p_RideOnGraph      : TBarGraphSeriesDataList;
            p_GetOffGraph      : TBarGraphSeriesDataList;
            p_OccupancyPerGraph: TBarGraphSeriesDataList;
        public
            constructor Create(const aBusStopInfo: BusStopInfo);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure ClearAllGraph;
            procedure UpdateGraph(const aBusData: TBusOperationLog);

            procedure ApplyRideOnGraph(const aChart: TChart);
            procedure ApplyGetOffGraph(const aChart: TChart);
            procedure ApplyOccupancyPerGraph(const aChart: TChart);
        end;

    TFrameBusTransportationGraph = class(TFrame)
        PanelBusTransportationGraph: TPanel;
        ChartBusTransportation: TChart;
        PanelExportButton: TPanel;
        ButtonExportGraphImage: TButton;
        SaveDialogGraphImage: TSaveDialog;
        PanelSelectDisplayItem: TPanel;
        RadioButtonDisplayTotalRideOn: TRadioButton;
        RadioButtonDisplayTotalGetOff: TRadioButton;
        RadioButtonDisplayOccupancyPer: TRadioButton;
        ComboBoxBusStopName: TComboBox;
        procedure RadioButtonDisplayTotalRideOnClick(Sender: TObject);
        procedure RadioButtonDisplayTotalGetOffClick(Sender: TObject);
        procedure RadioButtonDisplayOccupancyPerClick(Sender: TObject);
        procedure ComboBoxBusStopNameChange(Sender: TObject);
        procedure ButtonExportGraphImageClick(Sender: TObject);
        private
            p_GraphList: TObjectList<TBusStopGraphList>;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure UpdateData(const aData: TBusOperationLogsExport);
        end;

implementation

{$R *.dfm}

uses
    System.Math,
    System.DateUtils;

type
    /// <summary>
    ///    断面交通流解析結果のグラフを画像として保存するためのクラス
    /// </summary>
    TClipPanel = class(TCustomControl)
        property Canvas;
    end;

const
    LEFT_AXIS_NAME_RIDE_ON       = '乗車人数(人)';
    LEFT_AXIS_NAME_GET_OFF       = '降車人数(人)';
    LEFT_AXIS_NAME_OCCUPANCY_PER = '乗車率(%)';
    BOTTOM_AXIS_NAME             = '到着時刻';

{ TBarGraphSeriesData }
constructor TBarGraphSeriesData.Create(const aSeriesName: String; const aYValue: Double);
    begin
    p_SeriesName := aSeriesName;
    p_GraphPoint := F8Point(0, aYValue);
    end;

procedure TBarGraphSeriesData.AfterConstruction;
    begin
    inherited;
    {no action}
    end;

procedure TBarGraphSeriesData.BeforeDestruction;
    begin
    inherited;
    {no action}
    end;

function TBarGraphSeriesData.GetGraphPoint: F8PointType;
    begin
    Result := p_GraphPoint;
    end;

{ TBarGraphSeriesDataList }
procedure TBarGraphSeriesDataList.AfterConstruction;
    begin
    inherited;
    p_Data := TObjectList<TBarGraphSeriesData>.Create;
    end;

procedure TBarGraphSeriesDataList.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_Data);
    end;

procedure TBarGraphSeriesDataList.ClearAllPoints;
    begin
    p_Data.Clear;
    end;

procedure TBarGraphSeriesDataList.AddBar(const aSeriesName: String; const aBarValue: Double);
    var
        i: Integer;
        find: Boolean;
    begin
    find := False;
    for i := 0 to p_Data.Count - 1 do
        begin
        if p_Data[i].SeriesName = aSeriesName then
            begin
            find := True;
            Break;
            end;
        end;
    if not find then
        p_Data.Add(TBarGraphSeriesData.Create(aSeriesName, aBarValue));
    end;

procedure TBarGraphSeriesDataList.ApplyData(const aChart: TChart; const aFixedMinMax: Boolean = False; const aFixedMin: Integer = 0; aFixedMax: Integer = 1);
    var
        i: Integer;
        barSeries: TBarSeries;
        series: TChartSeries;
        maxBottomVal: Integer;
        maxLeftVal: Integer;
        tmp: Integer;
    begin
    if not Assigned(aChart) then
        Exit;

    while aChart.SeriesCount > 0 do
        begin
        series := aChart.Series[0];
        aChart.RemoveSeries(0);
        series.Free;
        end;

    aChart.LeftAxis.Title.Caption   := LeftAxisname;
    aChart.BottomAxis.Title.Caption := BottomAxisName;

    aChart.BottomAxis.Automatic := False;
    aChart.Axes.Bottom.Items.Clear;
    aChart.LeftAxis.Automatic := False;
    aChart.Axes.Left.Items.Clear;

    maxBottomVal := p_Data.Count + 1;

    aChart.BottomAxis.Minimum := 0;
    aChart.BottomAxis.Maximum := maxBottomVal;

    maxLeftVal := 0;
    for i := 0 to p_Data.Count - 1 do
        begin
        aChart.Axes.Bottom.Items.Add((i + 1), p_Data[i].SeriesName);
        barSeries := TBarSeries(aChart.AddSeries(TBarSeries));
        barSeries.Name := p_Data[i].SeriesName;
        barSeries.Marks.Style := smsValue;
        barSeries.BarWidthPercent := 40;
        barSeries.MultiBar := mbNone;
        barSeries.Pen.Width := 1;
        barSeries.AddXY((i + 1), p_Data[i].GraphPoint[_y]);
        maxLeftVal := Max(maxLeftVal, Ceil(p_Data[i].GraphPoint[_y]));
        end;

    aChart.LeftAxis.Minimum := 0;
    aChart.LeftAxis.Maximum := Power(10, Trunc(Log10(maxLeftVal)) + 1);
    tmp := Trunc(aChart.LeftAxis.Maximum / 10);
    for i := 0 to 9 do
        aChart.Axes.Left.Items.Add(i * tmp, (i * tmp).ToString);
    end;

procedure TBarGraphSeriesDataList.SetLeftAxisName(const aValue: String);
    begin
    p_LeftAxisName := aValue;
    end;
procedure TBarGraphSeriesDataList.SetBottomAxisName(const aValue: String);
    begin
    p_BottomAxisName := aValue;
    end;

{ TBusStopGraphList }
constructor TBusStopGraphList.Create(const aBusStopInfo: BusStopInfo);
    begin
    p_BusStopInfo := aBusStopInfo;
    end;

procedure TBusStopGraphList.AfterConstruction;
    begin
    inherited;
    p_RideOnGraph       := TBarGraphSeriesDataList.Create;
    p_GetOffGraph       := TBarGraphSeriesDataList.Create;
    p_OccupancyPerGraph := TBarGraphSeriesDataList.Create;

    p_RideOnGraph.LeftAxisname         := LEFT_AXIS_NAME_RIDE_ON;
    p_RideOnGraph.BottomAxisName       := BOTTOM_AXIS_NAME;
    p_GetOffGraph.LeftAxisname         := LEFT_AXIS_NAME_GET_OFF;
    p_GetOffGraph.BottomAxisName       := BOTTOM_AXIS_NAME;
    p_OccupancyPerGraph.LeftAxisname   := LEFT_AXIS_NAME_OCCUPANCY_PER;
    p_OccupancyPerGraph.BottomAxisName := BOTTOM_AXIS_NAME;
    end;

procedure TBusStopGraphList.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_RideOnGraph);
    FreeAndNil(p_GetOffGraph);
    FreeAndNil(p_OccupancyPerGraph);
    end;

procedure TBusStopGraphList.ClearAllGraph;
    begin
    p_RideOnGraph.ClearAllPoints;
    p_GetOffGraph.ClearAllPoints;
    p_OccupancyPerGraph.ClearAllPoints;
    end;

procedure TBusStopGraphList.UpdateGraph(const aBusData: TBusOperationLog);
    var
        i: Integer;
        barName: String;
    begin
    for i := 0 to aBusData.InOutLogCount - 1 do
        begin
        if IsEqualGUID(p_BusStopInfo.GUID, aBusData.InOutLog[i].BusStopGUID) then
            begin
            DateTimeToString(barName, 'hh_nn', aBusData.InOutLog[i].BusLog.ArrivalTime);
            barName := 'Time_' + barName;
            p_RideOnGraph.AddBar(barName, aBusData.InOutLog[i].BusLog.RideOnNum);
            p_GetOffGraph.AddBar(barName, aBusData.InOutLog[i].BusLog.GetOffNum);
            p_OccupancyPerGraph.AddBar(barName, aBusData.InOutLog[i].BusLog.OccupancyPer * 100);
            end;
        end;
    end;

procedure TBusStopGraphList.ApplyRideOnGraph(const aChart: TChart);
    begin
    p_RideOnGraph.ApplyData(aChart);
    end;

procedure TBusStopGraphList.ApplyGetOffGraph(const aChart: TChart);
    begin
    p_GetOffGraph.ApplyData(aChart);
    end;

procedure TBusStopGraphList.ApplyOccupancyPerGraph(const aChart: TChart);
    begin
    p_OccupancyPerGraph.ApplyData(aChart);
    end;

{ TFrameBusTransportationGraph }
procedure TFrameBusTransportationGraph.AfterConstruction;
    begin
    inherited;
    p_GraphList := TObjectList<TBusStopGraphList>.Create;
    end;

procedure TFrameBusTransportationGraph.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_GraphList);
    end;

procedure TFrameBusTransportationGraph.UpdateData(const aData: TBusOperationLogsExport);
    procedure UpdateUIEnabled(const aControlEnabled: Boolean);
        begin
        RadioButtonDisplayTotalRideOn.Enabled  := aControlEnabled;
        RadioButtonDisplayTotalGetOff.Enabled  := aControlEnabled;
        RadioButtonDisplayOccupancyPer.Enabled := aControlEnabled;
        ComboBoxBusStopName.Enabled            := aControlEnabled;
        ButtonExportGraphImage.Enabled         := aControlEnabled;
        end;
    var
        i, j: Integer;
    begin
    p_GraphList.Clear;
    ComboBoxBusStopName.Clear;

    if not Assigned(aData) then
        begin
        UpdateUIEnabled(False);
        Exit;
        end;

    UpdateUIEnabled(aData.BusStopCount > 0);

    for i := 0 to aData.BusStopCount - 1 do
        begin
        p_GraphList.Add(TBusStopGraphList.Create(aData.BusStop[i]));
        ComboBoxBusStopName.Items.AddObject(aData.BusStop[i].name, p_GraphList.Last);
        end;

    for i := 0 to aData.BusLogCount - 1 do
        begin
        for j := 0 to p_GraphList.Count - 1 do
            p_GraphList[j].UpdateGraph(aData.BusLog[i]);
        end;

    if aData.BusStopCount > 0 then
        begin
        ComboBoxBusStopName.ItemIndex := 0;

        if RadioButtonDisplayTotalRideOn.Checked then
            p_GraphList[ComboBoxBusStopName.ItemIndex].ApplyRideOnGraph(ChartBusTransportation)
        else if RadioButtonDisplayTotalGetOff.Checked then
            p_GraphList[ComboBoxBusStopName.ItemIndex].ApplyGetOffGraph(ChartBusTransportation)
        else
            p_GraphList[ComboBoxBusStopName.ItemIndex].ApplyOccupancyPerGraph(ChartBusTransportation);
        end;
    end;

procedure TFrameBusTransportationGraph.RadioButtonDisplayTotalRideOnClick(Sender: TObject);
    var
        obj: TObject;
    begin
    if ComboBoxBusStopName.ItemIndex < 0 then
        Exit;

    obj := ComboBoxBusStopName.Items.Objects[ComboBoxBusStopName.ItemIndex];
    if not Assigned(obj) then
        Exit;

    if obj is TBusStopGraphList then
        (obj as TBusStopGraphList).ApplyRideOnGraph(ChartBusTransportation);
    end;

procedure TFrameBusTransportationGraph.RadioButtonDisplayTotalGetOffClick(Sender: TObject);
    var
        obj: TObject;
    begin
    if ComboBoxBusStopName.ItemIndex < 0 then
        Exit;

    obj := ComboBoxBusStopName.Items.Objects[ComboBoxBusStopName.ItemIndex];
    if not Assigned(obj) then
        Exit;

    if obj is TBusStopGraphList then
        (obj as TBusStopGraphList).ApplyGetOffGraph(ChartBusTransportation);
    end;

procedure TFrameBusTransportationGraph.RadioButtonDisplayOccupancyPerClick(Sender: TObject);
    var
        obj: TObject;
    begin
    if ComboBoxBusStopName.ItemIndex < 0 then
        Exit;

    obj := ComboBoxBusStopName.Items.Objects[ComboBoxBusStopName.ItemIndex];
    if not Assigned(obj) then
        Exit;

    if obj is TBusStopGraphList then
        (obj as TBusStopGraphList).ApplyOccupancyPerGraph(ChartBusTransportation);
    end;

procedure TFrameBusTransportationGraph.ComboBoxBusStopNameChange(Sender: TObject);
    var
        obj: TObject;
    begin
    if ComboBoxBusStopName.ItemIndex < 0 then
        Exit;

    obj := ComboBoxBusStopName.Items.Objects[ComboBoxBusStopName.ItemIndex];
    if not Assigned(obj) then
        Exit;

    if obj is TBusStopGraphList then
        begin
        if RadioButtonDisplayTotalRideOn.Checked then
            (obj as TBusStopGraphList).ApplyRideOnGraph(ChartBusTransportation)
        else if RadioButtonDisplayTotalGetOff.Checked then
            (obj as TBusStopGraphList).ApplyGetOffGraph(ChartBusTransportation)
        else
            (obj as TBusStopGraphList).ApplyOccupancyPerGraph(ChartBusTransportation);
        end;
    end;

procedure TFrameBusTransportationGraph.ButtonExportGraphImageClick(Sender: TObject);
    var
        GraphClip : TBitmap;
        ClipRect  : TRect;
    begin
    GraphClip := TBitmap.Create;
    GraphClip.PixelFormat := pf24bit;
    GraphClip.Width       := ChartBusTransportation.ClientWidth;
    GraphClip.Height      := ChartBusTransportation.ClientHeight;
    ClipRect := Rect(0, 0, GraphClip.Width, GraphClip.Height);
    GraphClip.Canvas.CopyRect(ClipRect, TClipPanel(ChartBusTransportation).Canvas, ClipRect);
    if SaveDialogGraphImage.Execute then
        GraphClip.SaveToFile(SaveDialogGraphImage.FileName);

    FreeAndNil(GraphClip);
    end;
end.

