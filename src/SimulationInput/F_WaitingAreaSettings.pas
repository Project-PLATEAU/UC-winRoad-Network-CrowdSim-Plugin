unit F_WaitingAreaSettings;

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
    F8FloatSpinEdit,
    F_ExitInterval,
    Vcl.Buttons,
    System.ImageList,
    Vcl.ImgList,
    Vcl.ControlList;

type
    TFrameWaitingAreaSettings = class(TFrame)
        PanelMain: TPanel;
        chbVisibleWaitingArea: TCheckBox;
        grpbWaitingAreaSettings: TGroupBox;
        lblAreaName: TLabel;
        edtWaitingAreaName: TEdit;
        btnSetWaitingArea: TButton;
        PanelWaitingQueueSettings: TPanel;
        grpbWaitingQueueSettings: TGroupBox;
        cbbSelectWaitingArea: TComboBox;
        lblSetWaitingArea: TLabel;
        lblPedestrianMargin: TLabel;
        btnCreateWaitingQueue: TButton;
        btnDeleteWaitingArea: TButton;
        grpbExitInterval: TGroupBox;
        PanelSettings: TPanel;
        PanelIntervalSettings: TPanel;
        cbbExitIntervalRule: TComboBox;
        lblExitIntervalRule: TLabel;
        PanelFrameExitInterval: TPanel;
        FrameExitInterval1: TFrameExitInterval;
        PanelPedestrianMarginEditor: TPanel;
        btnSelectedView: TButton;
        Label1: TLabel;
        edtBusTerminal: TEdit;
        ImageList1: TImageList;
        btnBusTerminalLinkFree: TSpeedButton;
        btnMovetoBusTerminal: TButton;
        btnSelectBusTerminal: TButton;
        Label2: TLabel;
        Panel1: TPanel;
        lbWQ: TListBox;
        Label3: TLabel;
        lbLinkedWQ: TListBox;
        Label4: TLabel;
        edtWQNode: TEdit;
        btnWQNodeChange: TButton;
        btnMovetoWQNode: TButton;
        btnLinkWQandArea: TSpeedButton;
        ImageList2: TImageList;
        btnLinkFree: TSpeedButton;
        private
            p_sePedestrianMargin  : TF8FloatSpinEdit;
            p_WaitingAreaVisible  : boolean;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            property  WaitingAreaVisible : boolean read p_WaitingAreaVisible write p_WaitingAreaVisible;
            property  sePedestrianMargin : TF8FloatSpinEdit read p_sePedestrianMargin;
        end;

implementation

{$R *.dfm}

procedure TFrameWaitingAreaSettings.AfterConstruction;

    procedure CreateMarginEditor;
        begin
        p_sePedestrianMargin := TF8FloatSpinEdit.Create(Self);
        p_sePedestrianMargin.Name      := 'sePedestrianMargin';
        p_sePedestrianMargin.Parent    := PanelPedestrianMarginEditor;
        p_sePedestrianMargin.Align     := alLeft;
        p_sePedestrianMargin.Width     := 80;
        p_sePedestrianMargin.MaxValue  := 10000.0;
        p_sePedestrianMargin.MinValue  := 0.00;
        p_sePedestrianMargin.Digits    := 2;
        p_sePedestrianMargin.Increment := 0.1;
        p_sePedestrianMargin.Tail      := ' m';
        p_sePedestrianMargin.Value     := 1;
        end;

    begin
    inherited;

    p_WaitingAreaVisible := false;
    chbVisibleWaitingArea.Checked := WaitingAreaVisible;
    CreateMarginEditor;
    end;

procedure TFrameWaitingAreaSettings.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_sePedestrianMargin);
    end;
end.
