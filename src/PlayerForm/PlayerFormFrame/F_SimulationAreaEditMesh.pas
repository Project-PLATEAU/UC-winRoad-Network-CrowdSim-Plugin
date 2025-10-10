unit F_SimulationAreaEditMesh;

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
    Vcl.ComCtrls,
    F8FloatSpinEdit,
    PedestrianMapUser,
    PedestrianCell;

type
    /// <summary>
    ///    シミュレーションエリア編集に関する設定/操作を行うフレームとその機能を定義するクラス
    /// </summary>
    TFrameSimulationAreaEditMesh = class(TFrame)
        PanelCrossWalkGreenColorLegend: TPanel;
        PanelCrossWalkRedColorLegend: TPanel;
        PanelWalkableColorLegend: TPanel;
        PanelNoEntryColorLegend: TPanel;
        PanelSignalIntervalTime: TPanel;
        LabelSignalIntervalTime: TLabel;
        GroupBoxMeshColorLegend: TGroupBox;
        PanelLegendRow1: TPanel;
        LabelWalkable: TLabel;
        LabelNoEntry: TLabel;
        LabelSignalGreen: TLabel;
        LabelSignalRed: TLabel;
        PageControlEditMesh: TPageControl;
        TabSheetEditWalkable: TTabSheet;
        TabSheetEditCrossWalk: TTabSheet;
        RadioButtonWalkable: TRadioButton;
        RadioButtonUnwalkable: TRadioButton;
        PanelCrosswalkList: TPanel;
        PanelCrosswalkControl: TPanel;
        ButtonAddCrossWalk: TButton;
        ButtonDeleteCrosswalk: TButton;
        ListBoxCrosswalk: TListBox;
        GroupBoxCrosswalkConfig: TGroupBox;
        PanelSignal: TPanel;
        RadioButtonSignalGreen: TRadioButton;
        RadioButtonSignalRed: TRadioButton;
        Panel1: TPanel;
        Panel2: TPanel;
        PanelWalkableRow1: TPanel;
        Panel3: TPanel;
        Panel4: TPanel;
        PanelEditingMode: TPanel;
        RadioButtonAddMode: TRadioButton;
        RadioButtonRemoveMode: TRadioButton;
        TrackBarCrosswalkEditSize: TTrackBar;
        PanelCrosswalkEditSize: TPanel;
        LabelCrosswalkEditSizeCaption: TLabel;
        LabelCrosswalkEditSize: TLabel;
        PanelStatusEditSize: TPanel;
        LabelStatusEditSizeCaption: TLabel;
        LabelStatusEditSize: TLabel;
        TrackBarStatusEditSize: TTrackBar;
        procedure ButtonAddCrossWalkClick(Sender: TObject);
        procedure ButtonDeleteCrosswalkClick(Sender: TObject);
        procedure ListBoxCrosswalkClick(Sender: TObject);
        procedure RadioButtonSignalGreenClick(Sender: TObject);
        procedure RadioButtonSignalRedClick(Sender: TObject);
        procedure RadioButtonWalkableClick(Sender: TObject);
        procedure RadioButtonUnwalkableClick(Sender: TObject);
        procedure PageControlEditMeshChange(Sender: TObject);
        procedure RadioButtonAddModeClick(Sender: TObject);
        procedure RadioButtonRemoveModeClick(Sender: TObject);
        procedure TrackBarCrosswalkEditSizeChange(Sender: TObject);
        procedure TrackBarStatusEditSizeChange(Sender: TObject);
        private
            p_MapUser: TPedestrianMapUser;
        // UI
            p_SignalIntervalTime: TF8FloatSpinEdit;

            procedure ChangeUIEnabled(const aEnabled: Boolean);
            procedure RepopulateCrosswalkListBox(const aSelectIndex: Integer);
            procedure BeforeChangeMap;
            procedure AfterChangeMap;
            procedure ChangeSelectedCrosswalk(const aSelectIndex: Integer);

            procedure OnChangeSignalIntervalSecond(Sender: TObject);

            function  GetMapUser: TPedestrianMapUser;
            procedure SetMapUser(const aValue: TPedestrianMapUser);
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            property  MapUser: TPedestrianMapUSer read GetMapUser write SetMapUser;
        end;

implementation

uses
    PedestrianUtil;

const
    PAGE_EDIT_WALKABLE  = 0;
    PAGE_EDIT_CROSSWALK = 1;

{$R *.dfm}

{ TFrameSimulationAreaEditMesh }
procedure TFrameSimulationAreaEditMesh.AfterConstruction;
    procedure CreateComponent;
        begin
        p_SignalIntervalTime := TF8FloatSpinEdit.Create(Self);
        p_SignalIntervalTime.Name := 'SpinEditSignalIntervalSecond';
        p_SignalIntervalTime.Parent := PanelSignalIntervalTime;
        p_SignalIntervalTime.Align := alLeft;
        p_SignalIntervalTime.Width := 100;
        p_SignalIntervalTime.MaxValue := CROSSWALK_INTERVAL_MAX;
        p_SignalIntervalTime.MinValue := CROSSWALK_INTERVAL_MIN;
        p_SignalIntervalTime.Digits := 0;
        p_SignalIntervalTime.Increment := 1;
        p_SignalIntervalTime.Tail := ' 秒';
        p_SignalIntervalTime.Value := CROSSWALK_INTERVAL_MIN;
        p_SignalIntervalTime.OnChange := OnChangeSignalIntervalSecond;
        end;
    begin
    inherited;

    CreateComponent;
    PageControlEditMesh.ActivePageIndex := PAGE_EDIT_WALKABLE;
    end;

procedure TFrameSimulationAreaEditMesh.BeforeDestruction;
    begin
    inherited;

    MapUser := nil;
    end;

procedure TFrameSimulationAreaEditMesh.ButtonAddCrossWalkClick(Sender: TObject);
    begin
    MapUser.CellEditor.Enabled := True;
    MapUser.CellEditor.AddCrossWalk;
    RepopulateCrosswalkListBox(MapUser.Map.Crosswalks.ListCount - 1);
    ChangeSelectedCrosswalk(ListBoxCrosswalk.ItemIndex);
    end;

procedure TFrameSimulationAreaEditMesh.ButtonDeleteCrosswalkClick(Sender: TObject);
    begin
    MapUser.CellEditor.Enabled := True;
    // Single select only
    MapUser.CellEditor.DeleteCrossWalk(ListBoxCrosswalk.ItemIndex);
    RepopulateCrossWalkListBox(-1);
    ChangeSelectedCrosswalk(-1);
    end;

procedure TFrameSimulationAreaEditMesh.ListBoxCrosswalkClick(Sender: TObject);
    begin
    ChangeSelectedCrosswalk(ListBoxCrosswalk.ItemIndex);
    end;

procedure TFrameSimulationAreaEditMesh.RadioButtonSignalGreenClick(Sender: TObject);
    begin
    if Assigned(MapUser) then
        MapUser.CellEditor.ChangeCrossWalkFirstStatus(PedestrianAreaCrossingSignal._pacsCrossingGreen);
    end;

procedure TFrameSimulationAreaEditMesh.RadioButtonSignalRedClick(Sender: TObject);
    begin
    if Assigned(MapUser) then
        MapUser.CellEditor.ChangeCrossWalkFirstStatus(PedestrianAreaCrossingSignal._pacsCrossingRed);
    end;

procedure TFrameSimulationAreaEditMesh.RadioButtonWalkableClick(Sender: TObject);
    begin
    if Assigned(MapUser) then
        MapUser.CellEditor.WriteStatus := PedestrianAreaStatus._pasWalkable;
    end;

procedure TFrameSimulationAreaEditMesh.RadioButtonUnwalkableClick(Sender: TObject);
    begin
    if Assigned(MapUser) then
        MapUser.CellEditor.WriteStatus := PedestrianAreaStatus._pasNoEntry;
    end;

procedure TFrameSimulationAreaEditMesh.PageControlEditMeshChange(Sender: TObject);
    begin
    case PageControlEditMesh.ActivePageIndex of
        PAGE_EDIT_WALKABLE :
            begin
            ChangeSelectedCrosswalk(-1);
            MapUser.CellEditor.Enabled := True;
            end;
        PAGE_EDIT_CROSSWALK: ChangeSelectedCrosswalk(ListBoxCrosswalk.ItemIndex);
        end;
    end;

procedure TFrameSimulationAreaEditMesh.RadioButtonAddModeClick(Sender: TObject);
    begin
    if Assigned(MapUser) then
        MapUser.CellEditor.ChangeCrosswalkToAddCellMode;
    end;

procedure TFrameSimulationAreaEditMesh.RadioButtonRemoveModeClick(Sender: TObject);
    begin
    if Assigned(MapUser) then
        MapUser.CellEditor.ChangeCrosswalkToRemoveCellMode;
    end;

procedure TFrameSimulationAreaEditMesh.TrackBarCrosswalkEditSizeChange(Sender: TObject);
    var
        volume: Integer;
    begin
    if Assigned(p_MapUser) then
        p_MapUser.CellEditor.CrosswalkEditSize := TrackBarCrosswalkEditSize.Position;

    volume := TrackBarCrosswalkEditSize.Position * 2 + 1;

    if volume < 10 then
        LabelCrosswalkEditSize.Caption := Format('  %d ×   %d', [volume, volume])
    else if volume < 100 then
        LabelCrosswalkEditSize.Caption := Format(' %d ×  %d', [volume, volume])
    else
        LabelCrosswalkEditSize.Caption := Format('%d × %d', [volume, volume]);
    end;

procedure TFrameSimulationAreaEditMesh.TrackBarStatusEditSizeChange(Sender: TObject);
    var
        volume: Integer;
    begin
    if Assigned(p_MapUser) then
        p_MapUser.CellEditor.StatusEditSize := TrackBarStatusEditSize.Position;

    volume := TrackBarStatusEditSize.Position * 2 + 1;
    if volume < 10 then
        LabelStatusEditSize.Caption := Format('  %d ×   %d', [volume, volume])
    else if volume < 100 then
        LabelStatusEditSize.Caption := Format(' %d ×  %d', [volume, volume])
    else
        LabelStatusEditSize.Caption := Format('%d × %d', [volume, volume]);
    end;

procedure TFrameSimulationAreaEditMesh.ChangeUIEnabled(const aEnabled: Boolean);
    begin
    RadioButtonWalkable.Enabled       := aEnabled;
    RadioButtonUnwalkable.Enabled     := aEnabled;
    p_SignalIntervalTime.Enabled      := aEnabled;
    ButtonAddCrossWalk.Enabled        := aEnabled;
    ButtonDeleteCrosswalk.Enabled     := aEnabled;
    ListBoxCrosswalk.Enabled          := aEnabled;
    RadioButtonSignalGreen.Enabled    := aEnabled;
    RadioButtonSignalRed.Enabled      := aEnabled;
    TrackBarCrosswalkEditSize.Enabled := aEnabled;
    TrackBarStatusEditSize.Enabled    := aEnabled;
    end;

procedure TFrameSimulationAreaEditMesh.RepopulateCrosswalkListBox(const aSelectIndex: Integer);
    var
        i: Integer;
    begin
    ListBoxCrosswalk.Clear;
    if Assigned(MapUser) and Assigned(MapUser.Map) then
        begin
        for i := 0 to MapUser.Map.CrossWalks.ListCount - 1 do
            begin
            ListBoxCrosswalk.Items.Add(MapUser.Map.CrossWalks.CrossWalk[i].CrosswalkName);
            if i = aSelectIndex then
                ListBoxCrosswalk.ItemIndex := aSelectIndex;
            end;
        end;
    end;

procedure TFrameSimulationAreaEditMesh.BeforeChangeMap;
    begin
    ChangeUIEnabled(False);
    ListBoxCrosswalk.Clear;
    end;

procedure TFrameSimulationAreaEditMesh.AfterChangeMap;
    var
        existed: Boolean;
    begin
    existed := Assigned(MapUser) and MapUser.Enabled;
    ChangeUIEnabled(existed);
    RepopulateCrosswalkListBox(-1);
    if existed then
        begin
        if RadioButtonWalkable.Checked then
            MapUser.CellEditor.WriteStatus := PedestrianAreaStatus._pasWalkable
        else if RadioButtonUnwalkable.Checked then
            MapUser.CellEditor.WriteStatus := PedestrianAreaStatus._pasNoEntry;

        MapUser.CellEditor.StatusEditSize    := TrackBarStatusEditSize.Position;
        MapUser.CellEditor.CrosswalkEditSize := TrackBarCrossWalkEditSize.Position;

        p_SignalIntervalTime.OnChange := nil;
        p_SignalIntervalTime.Value := MapUser.Map.CrosswalkIntervalSecond;
        p_SignalIntervalTime.OnChange := OnChangeSignalIntervalSecond;
        end;

    end;

procedure TFrameSimulationAreaEditMesh.ChangeSelectedCrosswalk(const aSelectIndex: Integer);
    procedure DetachRadioButtonSignalEvent;
        begin
        RadioButtonSignalGreen.OnClick := nil;
        RadioButtonSignalRed.OnClick := nil;
        RadioButtonAddMode.OnClick := nil;
        RadioButtonRemoveMode.OnClick := nil;
        end;

    procedure AttachRadioButtonSignalEvent;
        begin
        RadioButtonSignalGreen.OnClick := RadioButtonSignalGreenClick;
        RadioButtonSignalRed.OnClick := RadioButtonSignalRedClick;
        RadioButtonAddMode.OnClick := RadioButtonAddModeClick;
        RadioButtonRemoveMode.OnClick := RadioButtonRemoveModeClick;
        end;
    begin
    if MapUser.CellEditor.IsEditingCrossWalk then
        MapUser.CellEditor.EndEditingCrosswalk;

    DetachRadioButtonSignalEvent;

    if aSelectIndex >= 0 then
        begin
        case MapUser.Map.Crosswalks.Crosswalk[aSelectIndex].FirstSignal of
            PedestrianAreaCrossingSignal._pacsCrossingGreen: RadioButtonSignalGreen.Checked := True;
            PedestrianAreaCrossingSignal._pacsCrossingRed  : RadioButtonSignalRed.Checked   := True;
            end;

        MapUser.CellEditor.Enabled := True;
        MapUser.CellEditor.BeginEditingCrosswalk(aSelectIndex);

        RadioButtonAddMode.Checked := True;
        MapUser.CellEditor.ChangeCrosswalkToAddCellMode;
        end
    else
        MapUser.CellEditor.Enabled := False;

    AttachRadioButtonSignalEvent;
    end;

procedure TFrameSimulationAreaEditMesh.OnChangeSignalIntervalSecond(Sender: TObject);
    begin
    if Assigned(MapUser) and MapUser.Enabled then
        MapUser.Map.CrossWalkIntervalSecond := p_SignalIntervalTime.IntegerValue;
    end;

function TFrameSimulationAreaEditMesh.GetMapUser: TPedestrianMapUser;
    begin
    Result := p_MapUser;
    end;

procedure TFrameSimulationAreaEditMesh.SetMapUser(const aValue: TPedestrianMapUser);
    begin
    if Assigned(p_MapUser) then
        p_MapUser.UnRegisterOnChangeUserPedestrianMapEvent(AfterChangeMap);

    BeforeChangeMap;
    p_MapUser := aValue;
    AfterChangeMap;

    if Assigned(p_MapUser) then
        p_MapUser.RegisterOnChangeUserPedestrianMapEvent(AfterChangeMap);
    end;
end.
