unit F_HeatMapSettings;

interface

uses
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
    PedestrianMapUser,
    HeatMapGenerator,
    HeatMapDrawerForm,
    F8RealSpinEdit;

type
    /// <summary>
    ///    ヒートマップ生成に関する設定を行うフレームとその機能を定義するクラス
    /// </summary>
    TFrameHeatMapSettings = class(TFrame)
        PanelSimSpan: TPanel;
        LabelSimSpan: TLabel;
        LabelSpanBetween: TLabel;
        PanelHeatMap: TPanel;
        ButtonGenerateHeatMap: TButton;
        SEStartTime: TF8RealSpinEdit;
        SEEndTime: TF8RealSpinEdit;
        procedure ButtonGenerateHeatMapClick(Sender: TObject);
        private
            p_DrawHeatMapForm   : TFormDrawHeatMap;
            p_HeatMapGenerator  : THeatMap;
            p_PedestrianMapUser : TPedestrianMapUser;
            p_CellsInOutList    : TList<TArray<integer>>;
            p_MinimumResolution : integer;
            p_HeatMapData       : TBitmap;
            procedure SetCellsInOutList(const IOList: TList<TArray<integer>>);
            procedure SetGenerateTimeIncrement(const MinReso: integer);
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            property PedestrianMapUser : TPedestrianMapUser     read p_PedestrianMapUser write p_PedestrianMapUser;
            property CellsInOutList    : TList<TArray<integer>> read p_CellsInOutList    write SetCellsInOutList;
            property MinimumResolution : integer                read p_MinimumResolution write p_MinimumResolution;
        end;

implementation

{$R *.dfm}

{ TFrameHeatMapSettings }
//==============================================================================
procedure TFrameHeatMapSettings.AfterConstruction;
    begin
    inherited;
    p_DrawHeatMapForm  := TFormDrawHeatMap.Create(nil);
    p_HeatMapGenerator := THeatMap.Create;
    end;

//==============================================================================
procedure TFrameHeatMapSettings.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_HeatMapGenerator);
    FreeAndNil(p_DrawHeatMapForm);
    end;

//==============================================================================
procedure TFrameHeatMapSettings.SetCellsInOutList(const IOList: TList<TArray<integer>>);
    var
        minreso : integer;
    begin
    p_CellsInOutList := IOList;
    if p_PedestrianMapUser.CrossSectionFlowLog.AddDataInterval < 0 then
        minreso := 1
    else
        minreso := p_PedestrianMapUser.CrossSectionFlowLog.AddDataInterval;

    p_MinimumResolution := minreso * 60;
    SetGenerateTimeIncrement(minreso);
    end;

//==============================================================================
procedure TFrameHeatMapSettings.SetGenerateTimeIncrement(const MinReso: integer);
    begin
    SEStartTime.MaxValue  := MinReso * p_CellsInOutList.Count;
    SEStartTime.Increment := MinReso;
    SEEndTime.MinValue    := MinReso;
    SEEndTime.MaxValue    := MinReso * p_CellsInOutList.Count;
    SEEndTime.Value       := MinReso * p_CellsInOutList.Count;
    SEEndTime.Increment   := MinReso;
    end;

//==============================================================================
procedure TFrameHeatMapSettings.ButtonGenerateHeatMapClick(Sender: TObject);
    var
        MaxNum, MiddleNum, MinNum : integer;
        StartTime, EndTime        : TTime;
    begin
    if (SEStartTime.Value = SEEndTime.Value) or (SEStartTime.Value > SEEndTime.Value) then
        begin
        ShowMessage('指定されたシミュレーション区間が不適切です');
        Exit;
        end;

    ButtonGenerateHeatMap.Enabled := false;
    StartTime := (SEStartTime.Value * 60) / SecsPerDay;
    EndTime   := (SEEndTime.Value * 60) / SecsPerDay;
    p_HeatMapData := TBitmap.Create;
    p_HeatMapData := p_HeatMapGenerator.GenerateHeatMap(p_PedestrianMapUser,
                                                        p_CellsInOutList,
                                                        StartTime, EndTime,
                                                        p_MinimumResolution,
                                                        MaxNum, MiddleNum, MinNum);

    p_DrawHeatMapForm.DrawHeatMap(p_HeatMapData, MaxNum, MiddleNum, MinNum);
    p_DrawHeatMapForm.Show;
    ButtonGenerateHeatMap.Enabled := true;
    end;
end.
