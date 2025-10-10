unit F_PedestrianCategoryEditor;

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
    Vcl.ExtCtrls,
    Vcl.StdCtrls,
    F8RealSpinEdit,
    F8FloatSpinEdit,
    System.ImageList,
    Vcl.ImgList,
    Vcl.Buttons,
    Vcl.ComCtrls,
    PluginCore,
    PedestrianCategoryData,
    F_NameChanger;

type
    TFramePedestrianCategoryEditor = class(TFrame)
        PanelMain: TPanel;
        PanelFotter: TPanel;
        pgcCategory: TPageControl;
        TabAdjustRate: TTabSheet;
        TabCollision: TTabSheet;
        TabSpeed: TTabSheet;
        TabPassTime: TTabSheet;
        PanelCategoryList: TPanel;
        LabelCategory: TLabel;
        btnOK: TButton;
        btnCancel: TButton;
        LabelTakeBusTime: TLabel;
        LabelPassGateTime: TLabel;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        pnlNormalSpeed: TPanel;
        pnlStairUpSpeed: TPanel;
        pnlStairDownSpeed: TPanel;
        pnlEscaWalkSpeed: TPanel;
        pnlDoorOrGateCollision: TPanel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        pnlWQWalkCollision: TPanel;
        pnlStopCollision: TPanel;
        pnlWalkCollision: TPanel;
        Label9: TLabel;
        Label10: TLabel;
        Label11: TLabel;
        Label12: TLabel;
        Label13: TLabel;
        Label14: TLabel;
        Label15: TLabel;
        Label16: TLabel;
        seEscaUpWalk: TF8RealSpinEdit;
        seEscaDownWalk: TF8RealSpinEdit;
        seEscaDown: TF8RealSpinEdit;
        seEscaUp: TF8RealSpinEdit;
        seStairDown: TF8RealSpinEdit;
        seStairUp: TF8RealSpinEdit;
        seElevUp: TF8RealSpinEdit;
        seElevDown: TF8RealSpinEdit;
        pnlTakeBusTime: TPanel;
        pnlPassGateTime: TPanel;
        edtCateName: TEdit;
        Label17: TLabel;
        Label18: TLabel;
        seCrosswalkRed: TF8RealSpinEdit;
        seCrosswalkBlue: TF8RealSpinEdit;
        Label19: TLabel;
        Label20: TLabel;
        pnlCWBlueSpeed: TPanel;
        pnlCWRedSpeed: TPanel;
        procedure seStairUpChange(Sender: TObject);
        procedure seStairDownChange(Sender: TObject);
        procedure seEscaUpChange(Sender: TObject);
        procedure seEscaUpWalkChange(Sender: TObject);
        procedure seEscaDownChange(Sender: TObject);
        procedure seEscaDownWalkChange(Sender: TObject);
        procedure seElevUpChange(Sender: TObject);
        procedure seElevDownChange(Sender: TObject);
        procedure seCrosswalkBlueChange(Sender: TObject);
        procedure seCrosswalkRedChange(Sender: TObject);
        procedure seStopCollisionChange(Sender: TObject);
        procedure seWalkCollisionChange(Sender: TObject);
        procedure seWQWalkCollisionChange(Sender: TObject);
        procedure seDoorOrGateCollisionChange(Sender: TObject);
        procedure seNormalSpeedChange(Sender: TObject);
        procedure seStairUpSpeedChange(Sender: TObject);
        procedure seStairDownSpeedChange(Sender: TObject);
        procedure seEscaWalkSpeedChange(Sender: TObject);
        procedure seCrosswalkBlueSpeedChange(Sender: TObject);
        procedure seCrosswalkRedSpeedChange(Sender: TObject);
        procedure seTakeBusTimeChange(Sender: TObject);
        procedure sePassGateTimeChange(Sender: TObject);
        procedure edtCateNameChange(Sender: TObject);
        private
            p_seStopCollision       : TF8FloatSpinEdit;
            p_seWalkCollision       : TF8FloatSpinEdit;
            p_seWQWalkCollision     : TF8FloatSpinEdit;
            p_seDoorOrGateCollision : TF8FloatSpinEdit;
            p_seNormalSpeed         : TF8FloatSpinEdit;
            p_seStairUpSpeed        : TF8FloatSpinEdit;
            p_seStairDownSpeed      : TF8FloatSpinEdit;
            p_seEscaWalkSpeed       : TF8FloatSpinEdit;
            p_seCrosswalkBlueSpeed  : TF8FloatSpinEdit;
            p_seCrosswalkRedSpeed   : TF8FloatSpinEdit;
            p_seTakeBusTime         : TF8FloatSpinEdit;
            p_sePassGateTime        : TF8FloatSpinEdit;
            p_EditingCategory       : TPedestrianCategoryData;

            procedure SetParamToUI;
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure SetEditData(const aCateList: TPedestrianCategoryList; idx: integer);

            property  seStopCollision       : TF8FloatSpinEdit read p_seStopCollision       write p_seStopCollision;
            property  seWalkCollision       : TF8FloatSpinEdit read p_seWalkCollision       write p_seWalkCollision;
            property  seWQWalkCollision     : TF8FloatSpinEdit read p_seWQWalkCollision     write p_seWQWalkCollision;
            property  seDoorOrGateCollision : TF8FloatSpinEdit read p_seDoorOrGateCollision write p_seDoorOrGateCollision;
            property  seNormalSpeed         : TF8FloatSpinEdit read p_seNormalSpeed         write p_seNormalSpeed;
            property  seStairUpSpeed        : TF8FloatSpinEdit read p_seStairUpSpeed        write p_seStairUpSpeed;
            property  seStairDownSpeed      : TF8FloatSpinEdit read p_seStairDownSpeed      write p_seStairDownSpeed;
            property  seEscaWalkSpeed       : TF8FloatSpinEdit read p_seEscaWalkSpeed       write p_seEscaWalkSpeed;
            property  seCrosswalkBlueSpeed  : TF8FloatSpinEdit read p_seCrosswalkBlueSpeed  write p_seCrosswalkBlueSpeed;
            property  seCrosswalkRedSpeed   : TF8FloatSpinEdit read p_seCrosswalkRedSpeed   write p_seCrosswalkRedSpeed;
            property  seTakeBusTime         : TF8FloatSpinEdit read p_seTakeBusTime         write p_seTakeBusTime;
            property  sePassGateTime        : TF8FloatSpinEdit read p_sePassGateTime        write p_sePassGateTime;
            property  EditingCategory       : TPedestrianCategoryData read p_EditingCategory;
        end;

implementation

{$R *.dfm}

procedure TFramePedestrianCategoryEditor.AfterConstruction;

    procedure SetSEParams(const name, tail: string; digits: integer; max, min, inc, val: double; parent: TPanel; var se: TF8FloatSpinEdit);
        begin
        se.Name      := name;
        se.Parent    := parent;
        se.Align     := alClient;
        se.MaxValue  := max;
        se.MinValue  := min;
        se.Digits    := digits;
        se.Increment := inc;
        se.Tail      := tail;
        se.Value     := val;
        end;

    begin
    inherited;

    p_seStopCollision                := TF8FloatSpinEdit.Create(nil);
    SetSEParams('seStopCollision',       ' m',    2, 100.0, 0.01, 0.1, 0.5, pnlStopCollision,       p_seStopCollision);
    p_seStopCollision.OnChange       := seStopCollisionChange;

    p_seWalkCollision                := TF8FloatSpinEdit.Create(nil);
    SetSEParams('seWalkCollision',       ' m',    2, 100.0, 0.01, 0.1, 0.5, pnlWalkCollision,       p_seWalkCollision);
    p_seWalkCollision.OnChange       := seWalkCollisionChange;

    p_seWQWalkCollision              := TF8FloatSpinEdit.Create(nil);
    SetSEParams('seWQWalkCollision',     ' m',    2, 100.0, 0.01, 0.1, 0.5, pnlWQWalkCollision,     p_seWQWalkCollision);
    p_seWQWalkCollision.OnChange     := seWQWalkCollisionChange;

    p_seDoorOrGateCollision          := TF8FloatSpinEdit.Create(nil);
    SetSEParams('seDoorOrGateCollision', ' m',    2, 100.0, 0.01, 0.1, 0.5, pnlDoorOrGateCollision, p_seDoorOrGateCollision);
    p_seDoorOrGateCollision.OnChange := seDoorOrGateCollisionChange;

    p_seNormalSpeed                  := TF8FloatSpinEdit.Create(nil);
    SetSEParams('seNormalSpeed',         ' km/h', 2, 100.0, 0.01, 0.1, 4.0, pnlNormalSpeed,         p_seNormalSpeed);
    p_seNormalSpeed.OnChange         := seNormalSpeedChange;

    p_seStairUpSpeed                 := TF8FloatSpinEdit.Create(nil);
    SetSEParams('seStairUpSpeed',        ' km/h', 2, 100.0, 0.01, 0.1, 4.0, pnlStairUpSpeed,        p_seStairUpSpeed);
    p_seStairUpSpeed.OnChange        := seStairUpSpeedChange;

    p_seStairDownSpeed               := TF8FloatSpinEdit.Create(nil);
    SetSEParams('seStairDownSpeed',      ' km/h', 2, 100.0, 0.01, 0.1, 4.0, pnlStairDownSpeed,      p_seStairDownSpeed);
    p_seStairDownSpeed.OnChange      := seStairDownSpeedChange;

    p_seEscaWalkSpeed                := TF8FloatSpinEdit.Create(nil);
    SetSEParams('seEscaWalkSpeed',       ' km/h', 2, 100.0, 0.01, 0.1, 4.0, pnlEscaWalkSpeed,       p_seEscaWalkSpeed);
    p_seEscaWalkSpeed.OnChange       := seEscaWalkSpeedChange;

    p_seCrosswalkBlueSpeed           := TF8FloatSpinEdit.Create(nil);
    SetSEParams('seCrosswalkBlueSpeed',  ' km/h', 2, 100.0, 0.01, 0.1, 4.0, pnlCWBlueSpeed,         p_seCrosswalkBlueSpeed);
    p_seCrosswalkBlueSpeed.OnChange  := seCrosswalkBlueSpeedChange;

    p_seCrosswalkRedSpeed            := TF8FloatSpinEdit.Create(nil);
    SetSEParams('seCrosswalkRedSpeed',   ' km/h', 2, 100.0, 0.01, 0.1, 4.0, pnlCWRedSpeed,          p_seCrosswalkRedSpeed);
    p_seCrosswalkRedSpeed.OnChange   := seCrosswalkRedSpeedChange;

    p_seTakeBusTime                  := TF8FloatSpinEdit.Create(nil);
    SetSEParams('seTakeBusTime',         ' •b',   1, 100.0, 0.1, 0.1, 2.0,  pnlTakeBusTime,         p_seTakeBusTime);
    p_seTakeBusTime.OnChange         := seTakeBusTimeChange;

    p_sePassGateTime                 := TF8FloatSpinEdit.Create(nil);
    SetSEParams('sePassGateTime',        ' •b',   1, 100.0, 0.1, 0.1, 0.2,  pnlPassGateTime,        p_sePassGateTime);
    p_sePassGateTime.OnChange        := sePassGateTimeChange;

    p_EditingCategory := TPedestrianCategoryData.Create;
    end;

procedure TFramePedestrianCategoryEditor.BeforeDestruction;
    begin
    inherited;

    FreeAndNil(p_EditingCategory);
    end;

procedure TFramePedestrianCategoryEditor.edtCateNameChange(Sender: TObject);
    begin
    p_EditingCategory.Name := edtCateName.Text;
    end;

procedure TFramePedestrianCategoryEditor.SetParamToUI;
    begin
    edtCateName.Text            := p_EditingCategory.Name;
    seStairUp.Value             := p_EditingCategory.StairUpPer;
    seStairDown.Value           := p_EditingCategory.StairDownPer;
    seEscaUp.Value              := p_EditingCategory.EscaUpPer;
    seEscaUpWalk.Value          := p_EditingCategory.EscaUpWalkPer;
    seEscaDown.Value            := p_EditingCategory.EscaDownPer;
    seEscaDownWalk.Value        := p_EditingCategory.EscaDownWalkPer;
    seElevUp.Value              := p_EditingCategory.ElevUpPer;
    seElevDown.Value            := p_EditingCategory.ElevDownPer;
    seCrosswalkBlue.Value       := p_EditingCategory.CrosswalkBluePer;
    seCrosswalkRed.Value        := p_EditingCategory.CrosswalkRedPer;
    seStopCollision.Value       := p_EditingCategory.StopCollision;
    seWalkCollision.Value       := p_EditingCategory.WalkCollision;
    seWQWalkCollision.Value     := p_EditingCategory.WQWalkCollision;
    seDoorOrGateCollision.Value := p_EditingCategory.DoorOrGateCollision;
    seNormalSpeed.Value         := p_EditingCategory.NormalSpeed;
    seStairUpSpeed.Value        := p_EditingCategory.StairUpSpeed;
    seStairDownSpeed.Value      := p_EditingCategory.StairDownSpeed;
    seEscaWalkSpeed.Value       := p_EditingCategory.EscaWalkSpeed;
    seCrosswalkBlueSpeed.Value  := p_EditingCategory.CrosswalkBlueSpeed;
    seCrosswalkRedSpeed.Value   := p_EditingCategory.CrosswalkRedSpeed;
    seTakeBusTime.Value         := p_EditingCategory.TakeBusTime;
    sePassGateTime.Value        := p_EditingCategory.PassGateTime;
    end;

procedure TFramePedestrianCategoryEditor.seStairUpChange(Sender: TObject);
    begin
    p_EditingCategory.StairUpPer := seStairUp.iValue;
    end;

procedure TFramePedestrianCategoryEditor.seStairDownChange(Sender: TObject);
    begin
    p_EditingCategory.StairDownPer := seStairDown.iValue;
    end;

procedure TFramePedestrianCategoryEditor.seEscaUpChange(Sender: TObject);
    begin
    p_EditingCategory.EscaUpPer := seEscaUp.iValue;
    end;

procedure TFramePedestrianCategoryEditor.seEscaUpWalkChange(Sender: TObject);
    begin
    p_EditingCategory.EscaUpWalkPer := seEscaUpWalk.iValue;
    end;

procedure TFramePedestrianCategoryEditor.seEscaDownChange(Sender: TObject);
    begin
    p_EditingCategory.EscaDownPer := seEscaDown.iValue;
    end;

procedure TFramePedestrianCategoryEditor.seEscaDownWalkChange(Sender: TObject);
    begin
    p_EditingCategory.EscaDownWalkPer := seEscaDownWalk.iValue;
    end;

procedure TFramePedestrianCategoryEditor.seElevUpChange(Sender: TObject);
    begin
    p_EditingCategory.ElevUpPer := seElevUp.iValue;
    end;

procedure TFramePedestrianCategoryEditor.seElevDownChange(Sender: TObject);
    begin
    p_EditingCategory.ElevDownPer := seElevDown.iValue;
    end;

procedure TFramePedestrianCategoryEditor.seCrosswalkBlueChange(Sender: TObject);
    begin
    p_EditingCategory.CrosswalkBluePer := seCrosswalkBlue.iValue;
    end;

procedure TFramePedestrianCategoryEditor.seCrosswalkRedChange(Sender: TObject);
    begin
    p_EditingCategory.CrosswalkRedPer := seCrosswalkRed.iValue;
    end;

procedure TFramePedestrianCategoryEditor.seStopCollisionChange(Sender: TObject);
    begin
    p_EditingCategory.StopCollision := seStopCollision.Value;
    end;

procedure TFramePedestrianCategoryEditor.seWalkCollisionChange(Sender: TObject);
    begin
    p_EditingCategory.WalkCollision := seWalkCollision.Value;
    end;

procedure TFramePedestrianCategoryEditor.seWQWalkCollisionChange(Sender: TObject);
    begin
    p_EditingCategory.WQWalkCollision := seWQWalkCollision.Value;
    end;

procedure TFramePedestrianCategoryEditor.seDoorOrGateCollisionChange(Sender: TObject);
    begin
    p_EditingCategory.DoorOrGateCollision := seDoorOrGateCollision.Value;
    end;

procedure TFramePedestrianCategoryEditor.seNormalSpeedChange(Sender: TObject);
    begin
    p_EditingCategory.NormalSpeed := seNormalSpeed.Value;
    end;

procedure TFramePedestrianCategoryEditor.seStairUpSpeedChange(Sender: TObject);
    begin
    p_EditingCategory.StairUpSpeed := seStairUpSpeed.Value;
    end;

procedure TFramePedestrianCategoryEditor.seStairDownSpeedChange(Sender: TObject);
    begin
    p_EditingCategory.StairDownSpeed := seStairDownSpeed.Value;
    end;

procedure TFramePedestrianCategoryEditor.seEscaWalkSpeedChange(Sender: TObject);
    begin
    p_EditingCategory.EscaWalkSpeed := seEscaWalkSpeed.Value;
    end;

procedure TFramePedestrianCategoryEditor.seCrosswalkBlueSpeedChange(Sender: TObject);
    begin
    p_EditingCategory.CrosswalkBlueSpeed := seCrosswalkBlueSpeed.Value;
    end;

procedure TFramePedestrianCategoryEditor.seCrosswalkRedSpeedChange(Sender: TObject);
    begin
    p_EditingCategory.CrosswalkRedSpeed := seCrosswalkRedSpeed.Value;
    end;

procedure TFramePedestrianCategoryEditor.seTakeBusTimeChange(Sender: TObject);
    begin
    p_EditingCategory.TakeBusTime := seTakeBusTime.Value;
    end;

procedure TFramePedestrianCategoryEditor.sePassGateTimeChange(Sender: TObject);
    begin
    p_EditingCategory.PassGateTime := sePassGateTime.Value;
    end;

procedure TFramePedestrianCategoryEditor.SetEditData(const aCateList: TPedestrianCategoryList; idx: integer);
    begin
    if idx < 0 then
        Exit;

    p_EditingCategory.Name                := aCateList.Data[idx].Name;
    p_EditingCategory.StairUpPer          := aCateList.Data[idx].StairUpPer;
    p_EditingCategory.StairDownPer        := aCateList.Data[idx].StairDownPer;
    p_EditingCategory.EscaUpPer           := aCateList.Data[idx].EscaUpPer;
    p_EditingCategory.EscaUpWalkPer       := aCateList.Data[idx].EscaUpWalkPer;
    p_EditingCategory.EscaDownPer         := aCateList.Data[idx].EscaDownPer;
    p_EditingCategory.EscaDownWalkPer     := aCateList.Data[idx].EscaDownWalkPer;
    p_EditingCategory.ElevUpPer           := aCateList.Data[idx].ElevUpPer;
    p_EditingCategory.ElevDownPer         := aCateList.Data[idx].ElevDownPer;
    p_EditingCategory.CrosswalkBluePer    := aCateList.Data[idx].CrosswalkBluePer;
    p_EditingCategory.CrosswalkRedPer     := aCateList.Data[idx].CrosswalkRedPer;
    p_EditingCategory.StopCollision       := aCateList.Data[idx].StopCollision;
    p_EditingCategory.WalkCollision       := aCateList.Data[idx].WalkCollision;
    p_EditingCategory.WQWalkCollision     := aCateList.Data[idx].WQWalkCollision;
    p_EditingCategory.DoorOrGateCollision := aCateList.Data[idx].DoorOrGateCollision;
    p_EditingCategory.NormalSpeed         := aCateList.Data[idx].NormalSpeed;
    p_EditingCategory.StairUpSpeed        := aCateList.Data[idx].StairUpSpeed;
    p_EditingCategory.StairDownSpeed      := aCateList.Data[idx].StairDownSpeed;
    p_EditingCategory.EscaWalkSpeed       := aCateList.Data[idx].EscaWalkSpeed;
    p_EditingCategory.CrosswalkBlueSpeed  := aCateList.Data[idx].CrosswalkBlueSpeed;
    p_EditingCategory.CrosswalkRedSpeed   := aCateList.Data[idx].CrosswalkRedSpeed;
    p_EditingCategory.TakeBusTime         := aCateList.Data[idx].TakeBusTime;
    p_EditingCategory.PassGateTime        := aCateList.Data[idx].PassGateTime;

    SetParamToUI;
    end;
end.
