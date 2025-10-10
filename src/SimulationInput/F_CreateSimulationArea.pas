unit F_CreateSimulationArea;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Generics.Collections,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
    Vcl.StdCtrls, F8FloatSpinEdit, Vcl.ComCtrls;

type
    TFrameCreateSimulationArea = class(TFrame)
        PanelMain: TPanel;
        chbVisibleSimArea: TCheckBox;
        lblDetailSimArea: TLabel;
        btnSetSimArea: TButton;
        btnCreateSimArea: TButton;
        PanelCreateSimArea: TPanel;
        PanelDetailEditor: TPanel;
        pbCreateSimArea: TProgressBar;
        PanelProgress: TPanel;
        GroupBoxViewOption: TGroupBox;
        btnBirdEye: TButton;
        chbStreetMap: TCheckBox;
        ButtonMeshOrigin: TButton;
        TimerUIEnable: TTimer;
        ButtonCreateNewNetwork: TButton;
        LabelMapHeightOffset: TLabel;
        PanelMapHeightOffsetEdit: TPanel;
        PanelMapHeightOffset: TPanel;
        GroupBoxMeshColorLegend: TGroupBox;
        PanelLegendRow1: TPanel;
        LabelWalkable: TLabel;
        LabelNoEntry: TLabel;
        PanelNoEntryColorLegend: TPanel;
        PanelWalkableColorLegend: TPanel;
        GroupBoxEditWalkable: TGroupBox;
        PanelWalkableRow1: TPanel;
        RadioButtonWalkable: TRadioButton;
        RadioButtonUnwalkable: TRadioButton;
        Panel3: TPanel;
        Panel4: TPanel;
        PanelStatusEditSize: TPanel;
        LabelStatusEditSizeCaption: TLabel;
        LabelStatusEditSize: TLabel;
        TrackBarStatusEditSize: TTrackBar;
        private
            p_seNetworkDetail: TF8FloatSpinEdit;
            p_SpinEditMapHeightOffset: TF8FloatSpinEdit;

            p_HeightOffsetOnChangeEvent: TList<TMethod>;

            function  GetNetworkDetail: Double;
            function  GetMeshVisible: Boolean;
            procedure SetMeshVisible(const aValue: Boolean);
            function  GetStatusEditSizeCaption: String;
            procedure SetStatusEditSizeCaption(const aValue: String);

            procedure FireHeightOffsetOnChangeEnvet(Sender: TObject);
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            procedure UpdateHeightOffset(const aValue: Double);

            procedure RegisterOnHeightOffsetChangeEvent(const aEvent: TNotifyEvent);
            procedure UnRegisterOnHeightOffsetChangeEvent(const aEvent: TNotifyEvent);

            procedure MeshEditorEnabled;
            procedure MeshEditorDisabled;

            property  NetworkDetail        : Double  read GetNetworkDetail;
            property  MeshVisible          : Boolean read GetMeshVisible           write SetMeshVisible;
            property  StatusEditSizeCaption: String  read GetStatusEditSizeCaption write SetStatusEditSizeCaption;
        end;

implementation

{$R *.dfm}

{ TFrameCreateSimulationArea }
procedure TFrameCreateSimulationArea.AfterConstruction;
    procedure CreateDetailEditor;
        begin
        p_seNetworkDetail := TF8FloatSpinEdit.Create(Self);
        p_seNetworkDetail.Name      := 'seNetworkDetail';
        p_seNetworkDetail.Parent    := PanelDetailEditor;
        p_seNetworkDetail.Align     := alLeft;
        p_seNetworkDetail.Width     := 80;
        p_seNetworkDetail.MaxValue  := 10000.0;
        p_seNetworkDetail.MinValue  := 0.25;
        p_seNetworkDetail.Digits    := 2;
        p_seNetworkDetail.Increment := 0.1;
        p_seNetworkDetail.Tail      := ' m';
        p_seNetworkDetail.Value     := 1;

        p_SpinEditMapHeightOffset := TF8FloatSpinEdit.Create(Self);
        p_SpinEditMapHeightOffset.Name := 'SpinEditMapHeightOffset';
        p_SpinEditMapHeightOffset.Parent := PanelMapHeightOffsetEdit;
        p_SpinEditMapHeightOffset.Align     := alLeft;
        p_SpinEditMapHeightOffset.Width     := 80;
        p_SpinEditMapHeightOffset.MaxValue  := 100.0;
        p_SpinEditMapHeightOffset.MinValue  := -100.0;
        p_SpinEditMapHeightOffset.Digits    := 2;
        p_SpinEditMapHeightOffset.Increment := 0.1;
        p_SpinEditMapHeightOffset.Tail      := ' m';
        p_SpinEditMapHeightOffset.Value     := 0.0000;
        p_SpinEditMapHeightOffset.OnChange := FireHeightOffsetOnChangeEnvet;
        end;
    begin
    inherited;

    p_HeightOffsetOnChangeEvent := TList<TMethod>.Create;
    CreateDetailEditor;
    end;

procedure TFrameCreateSimulationArea.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(p_HeightOffsetOnChangeEvent);
    end;

procedure TFrameCreateSimulationArea.UpdateHeightOffset(const aValue: Double);
    begin
    p_SpinEditMapHeightOffset.Value := aValue;
    end;

procedure TFrameCreateSimulationArea.RegisterOnHeightOffsetChangeEvent(const aEvent: TNotifyEvent);
    var
        m: TMethod;
    begin
    TNotifyEvent(m) := aEvent;
    if not p_HeightOffsetOnChangeEvent.Contains(m) then
        p_HeightOffsetOnChangeEvent.Add(m);
    end;

procedure TFrameCreateSimulationArea.UnRegisterOnHeightOffsetChangeEvent(const aEvent: TNotifyEvent);
    var
        m: TMethod;
    begin
    TNotifyEvent(m) := aEvent;
    p_HeightOffsetOnChangeEvent.Remove(m);
    end;

procedure TFrameCreateSimulationArea.MeshEditorEnabled;
    begin
    RadioButtonWalkable.Enabled       := True;
    RadioButtonUnwalkable.Enabled     := True;
    TrackBarStatusEditSize.Enabled    := True;
    p_SpinEditMapHeightOffset.Enabled := True;
    ButtonCreateNewNetwork.Enabled    := True;
    end;

procedure TFrameCreateSimulationArea.MeshEditorDisabled;
    begin
    RadioButtonWalkable.Enabled       := False;
    RadioButtonUnwalkable.Enabled     := False;
    TrackBarStatusEditSize.Enabled    := False;
    p_SpinEditMapHeightOffset.Enabled := False;
    ButtonCreateNewNetwork.Enabled    := False;
    end;

function TFrameCreateSimulationArea.GetNetworkDetail: Double;
    begin
    Result := p_seNetworkDetail.Value;
    end;

function TFrameCreateSimulationArea.GetMeshVisible: Boolean;
    begin
    Result := not chbVisibleSimArea.Checked;
    end;

procedure TFrameCreateSimulationArea.SetMeshVisible(const aValue: Boolean);
    begin
    chbVisibleSimArea.Checked := not aValue;
    end;

function TFrameCreateSimulationArea.GetStatusEditSizeCaption: String;
    begin
    Result := LabelStatusEditSizeCaption.Caption;
    end;

procedure TFrameCreateSimulationArea.SetStatusEditSizeCaption(const aValue: String);
    begin
    LabelStatusEditSize.Caption := aValue;
    end;

procedure TFrameCreateSimulationArea.FireHeightOffsetOnChangeEnvet(Sender: TObject);
    var
        m: TMethod;
    begin
    for m in p_HeightOffsetOnChangeEvent do
        TNotifyEvent(m)(Sender);
    end;
end.
