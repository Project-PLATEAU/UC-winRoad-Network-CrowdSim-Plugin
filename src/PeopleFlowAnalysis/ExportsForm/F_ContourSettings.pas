unit F_ContourSettings;

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
    Vcl.ExtCtrls,
    Vcl.StdCtrls,
    PedestrianMapUser,
    F8RealSpinEdit,
    ContourGenerator,
    ContourDrawerForm;

type
    /// <summary>
    ///    コンター生成に関する設定を行うフレームとその機能を定義するクラス
    /// </summary>
    TFrameContourSettings = class(TFrame)
        PanelSimSpan: TPanel;
        PanelContour: TPanel;
        LabelSimSpan: TLabel;
        SEStartTime: TF8RealSpinEdit;
        SEEndTime: TF8RealSpinEdit;
        LabelSpanBetween: TLabel;
        ButtonGenerateContour: TButton;
        procedure ButtonGenerateContourClick(Sender: TObject);
        private
            p_DrawContourForm   : TFormDrawContour;
            p_ContourGenerator  : TContour;
            p_PedestrianMapUser : TPedestrianMapUser;
            p_CellsInOutList    : TList<TArray<integer>>;
            p_MinimumResolution : integer;
            p_ContourData : TBitmap;
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

{ TFrameContourSettings }
//==============================================================================
procedure TFrameContourSettings.AfterConstruction;
    begin
    inherited;
    p_DrawContourForm  := TFormDrawContour.Create(nil);
    p_ContourGenerator := TContour.Create;
    end;

//==============================================================================
procedure TFrameContourSettings.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_ContourGenerator);
    FreeAndNil(p_DrawContourForm);
    end;

//==============================================================================
procedure TFrameContourSettings.SetCellsInOutList(const IOList: TList<TArray<integer>>);
    var
        minreso : integer;
    begin
    p_CellsInOutList := IOList;
    if p_PedestrianMapUser.CrossSectionFlowLog.AddDataInterval < 0 then
        minreso := 5
    else
        minreso := p_PedestrianMapUser.CrossSectionFlowLog.AddDataInterval;
    p_MinimumResolution := minreso * 60;
    SetGenerateTimeIncrement(minreso);
    end;

//==============================================================================
procedure TFrameContourSettings.SetGenerateTimeIncrement(const MinReso: integer);
    begin
    SEStartTime.MaxValue  := MinReso * p_CellsInOutList.Count;
    SEStartTime.Increment := MinReso;
    SEEndTime.MinValue    := MinReso;
    SEEndTime.MaxValue    := MinReso * p_CellsInOutList.Count;
    SEEndTime.Value       := MinReso * p_CellsInOutList.Count;
    SEEndTime.Increment   := MinReso;
    end;

//==============================================================================
procedure TFrameContourSettings.ButtonGenerateContourClick(Sender: TObject);
    var
        MaxNum, MiddleNum, MinNum : integer;
        StartTime, EndTime        : TTime;
    begin
    if (SEStartTime.Value = SEEndTime.Value) or (SEStartTime.Value > SEEndTime.Value) then
        begin
        ShowMessage('指定されたシミュレーション区間が不適切です');
        Exit;
        end;

    ButtonGenerateContour.Enabled := false;
    StartTime := (SEStartTime.Value * 60) / SecsPerDay;
    EndTime   := (SEEndTime.Value * 60) / SecsPerDay;
    p_ContourData := TBitmap.Create;
    p_ContourData := p_ContourGenerator.GenerateContour(p_PedestrianMapUser,
                                                        p_CellsInOutList,
                                                        StartTime, EndTime,
                                                        p_MinimumResolution,
                                                        MaxNum, MiddleNum, MinNum);

    p_DrawContourForm.DrawContour(p_ContourData, MaxNum, MiddleNum, MinNum);
    p_DrawContourForm.Show;
    ButtonGenerateContour.Enabled := true;
    end;
end.
