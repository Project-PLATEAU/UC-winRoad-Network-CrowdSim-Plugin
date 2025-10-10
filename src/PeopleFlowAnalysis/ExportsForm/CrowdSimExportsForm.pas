unit CrowdSimExportsForm;

interface

uses
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.ComCtrls,
    PedestrianMapUser,
    PedestrianMovingData,
    F_CrossSectionGraph,
    F_BusTransportationGraph,
    F_WaitingQueueGraph,
    F_HeatMapSettings,
    F_ODTripGraph,
    BusTransportationResultExport,
    WaitingQueueResultExport,
    ODTripResultExport;

type
    /// <summary>
    ///    断面交通流解析結果のグラフ、ヒートマップ、コンターの描画フレームをまとめるフォームを定義するクラス
    /// </summary>
    TFormCrowdSimExports = class(TForm)
        PageControlExports: TPageControl;
        TabSheetCrossSectionGraph: TTabSheet;
        TabSheetHeatMap: TTabSheet;
        TabSheetBusTransportation: TTabSheet;
        TabSheetWaitingQueue: TTabSheet;
        TabSheetODTrip: TTabSheet;
        procedure FormShow(Sender: TObject);
        private
            CSGraphFrame : TFrameCrossSectionGraph;
            HeatMapFrame : TFrameHeatMapSettings;
            BusTransportationFrame: TFrameBusTransportationGraph;
            WaitingQueueFrame: TFrameWaitingQueueGraph;
            ODTripFrame: TFrameODTripGraph;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure SetUpExportsData(const PedMapUser: TPedestrianMapUser);
            procedure UpdateBusTransportationLog(const aData: TBusOperationLogsExport);
            procedure UpdateWaitingQueueLog(const aData: TWaitingQueueResultExport);
            procedure UpdateODTripLog(const aData: TODTripResultExport);
        end;

implementation

{$R *.dfm}

{ TFormCrowdSimExports }
//==============================================================================
procedure TFormCrowdSimExports.AfterConstruction;
    begin
    inherited;

    CSGraphFrame        := TFrameCrossSectionGraph.Create(Self);
    CSGraphFrame.Parent := TabSheetCrossSectionGraph;
    CSGraphFrame.Name   := 'FrameCrossSectionGraph';
    CSGraphFrame.Align  := alClient;

    HeatMapFrame        := TFrameHeatMapSettings.Create(Self);
    HeatMapFrame.Parent := TabSheetHeatMap;
    HeatMapFrame.Name   := 'FrameHeatMap';
    HeatMapFrame.Align  := alClient;

    BusTransportationFrame        := TFrameBusTransportationGraph.Create(Self);
    BusTransportationFrame.Parent := TabSheetBusTransportation;
    BusTransportationFrame.Name   := 'FrameBusTransportationGraph';
    BusTransportationFrame.Align  := alClient;

    WaitingQueueFrame        := TFrameWaitingQueueGraph.Create(Self);
    WaitingQueueFrame.Parent := TabSheetWaitingQueue;
    WaitingQueueFrame.Name   := 'FrameWaitingQueueGraph';
    WaitingQueueFrame.Align  := alClient;

    ODTripFrame        := TFrameODTripGraph.Create(Self);
    ODTripFrame.Parent := TabSheetODTrip;
    ODTripFrame.Name   := 'FrameODTripGraph';
    ODTripFrame.Align  := alClient;
    end;

//==============================================================================
procedure TFormCrowdSimExports.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(CSGraphFrame);
    FreeAndNil(HeatMapFrame);
    FreeAndNil(BusTransportationFrame);
    FreeAndNil(WaitingQueueFrame);
    FreeAndNil(ODTripFrame);
    end;

//==============================================================================
procedure TFormCrowdSimExports.SetUpExportsData(const PedMapUser: TPedestrianMapUser);
    begin
    CSGraphFrame.PedestrianMapUser := PedMapUser;
    CSGraphFrame.OnSimStop;

    HeatMapFrame.PedestrianMapUser := PedMapUser;
    HeatMapFrame.CellsInOutList    := PedMapUser.CrossSectionFlowLog.CellsInOutList;
    end;

//==============================================================================
procedure TFormCrowdSimExports.UpdateBusTransportationLog(const aData: TBusOperationLogsExport);
    begin
    BusTransportationFrame.UpdateData(aData);
    end;

//==============================================================================
procedure TFormCrowdSimExports.UpdateWaitingQueueLog(const aData: TWaitingQueueResultExport);
    begin
    WaitingQueueFrame.UpdateData(aData);
    end;

//==============================================================================
procedure TFormCrowdSimExports.UpdateODTripLog(const aData: TODTripResultExport);
    begin
    ODTripFrame.UpdateData(aData);
    end;

//==============================================================================
procedure TFormCrowdSimExports.FormShow(Sender: TObject);
    begin
    PageControlExports.ActivePageIndex := 0;
    end;
end.
