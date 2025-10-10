unit TimeLineSeriesSupport;

interface

uses
    System.Generics.Collections,
    System.Classes,
    VCLTee.TeEngine,
    VCLTee.Chart,
    VCLTee.Series,
    F8Utils;

type
    TLineGraphSeriesData = class
        private
            p_SeriesName: String;
            p_GraphPointList: TList<F8PointType>;

            function  GetGraphPointCount: Integer;
            function  GetGraphPoint(const aIdx: Integer): F8PointType;
        public
            constructor Create(const aSeriesName: String);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure AddPoint(const aPoint: F8PointType);

            property  SeriesName                     : String      read p_SeriesName;
            property  GraphPointCount                : Integer     read GetGraphPointCount;
            property  GraphPoint[const aIdx: Integer]: F8PointType read GetGraphPoint;
        end;

    ///    グラフデータ
    ///    横軸は時刻で固定とする
    TTimeLineGraphList = class
        private
            p_SeriesList: TObjectList<TLineGraphSeriesData>;
            p_BottomLabelList: TStringList;

            p_LeftAxisCaption  : String;
            p_BottomAxisCaption: String;

            function  ContainsGraph(const aGraphData: TLineGraphSeriesData): Boolean;
        public
            constructor Create(const aLeftAxisCaption: String; const aBottomAxisCaption: String);
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure ClearAll;
            procedure AddNewGraphData(const aGraphData: TLineGraphSeriesData);
            procedure AddBottomAxisData(const aStartTime, aEndTime: TDateTime; const aIntervalMinute: Integer);
            function  ContainsSeries(const aSeriesName: String): Boolean;

            procedure ApplyGraph(const aChart: TChart; const aInheritedVisibility: Boolean = False);

            property  LeftAxisCaption                : String      read p_LeftAxisCaption;
            property  BottomAxisCaption              : String      read p_BottomAxisCaption;
        end;

implementation

uses
    System.SysUtils,
    System.Math,
    System.Generics.Defaults,
    System.Types,
    System.DateUtils;

{ TLineGraphSeriesData }
constructor TLineGraphSeriesData.Create(const aSeriesName: String);
    begin
    p_SeriesName := aSeriesName;
    end;

procedure TLineGraphSeriesData.AfterConstruction;
    begin
    inherited;

    p_GraphPointList := TList<F8PointType>.Create;
    end;

procedure TLineGraphSeriesData.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_GraphPointList);
    end;

procedure TLineGraphSeriesData.AddPoint(const aPoint: F8PointType);
    begin
    if p_GraphPointList.Contains(aPoint) then
        Exit;

    p_GraphPointList.Add(aPoint);
    p_GraphPointList.Sort(TComparer<F8PointType>.Construct(
        function(const L, R: F8PointType): Integer
            begin
            Result := Sign(L[_x] - R[_x]);
            end));
    end;

function TLineGraphSeriesData.GetGraphPointCount: Integer;
    begin
    Result := p_GraphPointList.Count;
    end;

function TLineGraphSeriesData.GetGraphPoint(const aIdx: Integer): F8PointType;
    begin
    Assert(InRange(aIdx, 0, p_GraphPointList.Count - 1));
    Result := p_GraphPointList[aIdx];
    end;

{ TTimeLineGraphList }
constructor TTimeLineGraphList.Create(const aLeftAxisCaption: String; const aBottomAxisCaption: String);
    begin
    p_LeftAxisCaption   := aLeftAxisCaption;
    p_BottomAxisCaption := aBottomAxisCaption;
    end;

procedure TTimeLineGraphList.AfterConstruction;
    begin
    inherited;

    p_SeriesList := TObjectList<TLineGraphSeriesData>.Create;
    p_BottomLabelList := TStringList.Create;
    end;

procedure TTimeLineGraphList.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_SeriesList);
    FreeAndNil(p_BottomLabelList);
    end;

procedure TTimeLineGraphList.ClearAll;
    begin
    p_SeriesList.Clear;
    end;

procedure TTimeLineGraphList.AddNewGraphData(const aGraphData: TLineGraphSeriesData);
    begin
    if Assigned(aGraphData) and not ContainsGraph(aGraphData) then
        p_SeriesList.Add(aGraphData);
    end;

procedure TTimeLineGraphList.AddBottomAxisData(const aStartTime, aEndTime: TDateTime; const aIntervalMinute: Integer);
    var
        newTime: TDateTime;
        newLabelString: String;
        counter: Integer;
    begin
    // Use only time(hh:nn:ss). Ignore date
    // Interval = 1以外の場合ちゃんと動かないかも
    Assert(CompareTime(aStartTime, aEndTime) = LessThanValue);

    p_BottomLabelList.Clear;
    counter := 0;
    while True do
        begin
        newTime := IncMinute(aStartTime, aIntervalMinute * counter);
        if CompareTime(newTime, aEndTime) in [GreaterThanValue] then
            Break;

        DateTimeToString(newLabelString, 'hh:nn', newTime);
        p_BottomLabelList.Add(newLabelString);
        Inc(counter);
        end;
    end;

function TTimeLineGraphList.ContainsSeries(const aSeriesName: String): Boolean;
    var
        i: Integer;
    begin
    Result := False;
    for i := 0 to p_SeriesList.Count - 1 do
        begin
        if aSeriesName = p_SeriesList[i].SeriesName then
            begin
            Result := True;
            Break;
            end;
        end;
    end;

procedure TTimeLineGraphList.ApplyGraph(const aChart: TChart; const aInheritedVisibility: Boolean);
    const
        LINE_CHART_NAME = 'Chart%d';
    var
        series: TChartSeries;
        lineChart: TLineSeries;

        i: Integer;
        idx: Integer;
        s: TLineGraphSeriesData;
        // オブジェクト名が同じであれば表示設定を引き継ぐ
        // 良い設計ではないが、今回はグラフの数は変化しないので使える
        tmpVisibilityTable: TDictionary<String, Boolean>;
    begin
    if not Assigned(aChart) then
        Exit;

    tmpVisibilityTable := TDictionary<String, Boolean>.Create;
    try
        while aChart.SeriesCount > 0 do
            begin
            series := aChart.Series[0];
            tmpVisibilityTable.Add(series.Name, series.Visible);
            aChart.RemoveSeries(0);
            series.Free;
            end;

        if p_BottomLabelList.Count = 0 then
            Exit;

        // 縦軸(LeftAxis)は自動、横軸(BottomAxis)は手動で設定する
        aChart.BottomAxis.Automatic := False;
        aChart.Axes.Bottom.Items.Clear;
        aChart.LeftAxis.Automatic := True;

        // 軸名
        aChart.BottomAxis.Title.Caption := BottomAxisCaption;
        aChart.LeftAxis.Title.Caption   := LeftAxisCaption;

        // 横軸の軸設定
        aChart.BottomAxis.Minimum := 0;
        aChart.BottomAxis.Maximum := Max(p_BottomLabelList.Count - 1, aChart.BottomAxis.Minimum + 1);
        for i := 0 to p_BottomLabelList.Count - 1 do
            aChart.Axes.Bottom.Items.Add(i, p_BottomLabelList[i]);

        idx := 0;
        for s in p_SeriesList do
            begin
            lineChart := TLineSeries(aChart.AddSeries(TLineSeries));
            lineChart.Name := Format(LINE_CHART_NAME, [idx]);
            lineChart.Pen.Width := 2;
            // Pointにひし形を表示させる
            lineChart.Pointer.InflateMargins := True;
            lineChart.Pointer.Style := psDiamond;
            lineChart.Pointer.Visible := True;

            // 系列名として最後の点のTextを表示させる
            // TChart側のLegendで設定が必要
            for i := 0 to s.GraphPointCount - 1 do
                begin
                if i = s.GraphPointCount - 1 then
                    lineChart.AddXY(s.GraphPoint[i][_x], s.GraphPoint[i][_y], s.SeriesName)
                else
                    lineChart.AddXY(s.GraphPoint[i][_x], s.GraphPoint[i][_y]);
                end;

            if aInheritedVisibility then
                begin
                if tmpVisibilityTable.ContainsKey(lineChart.Name) then
                    lineChart.Visible := tmpVisibilityTable[lineChart.Name];
                end;

            Inc(idx);
            end;
    finally
        FreeAndNil(tmpVisibilityTable);
        end;
    end;

function TTimeLineGraphList.ContainsGraph(const aGraphData: TLineGraphSeriesData): Boolean;
    var
        item: TLineGraphSeriesData;
    begin
    Result := False;
    if not Assigned(aGraphData) then
        Exit;

    for item in p_SeriesList do
        begin
        if aGraphData.SeriesName = item.SeriesName then
            begin
            Result := True;
            Break;
            end;
        end;
    end;
end.
