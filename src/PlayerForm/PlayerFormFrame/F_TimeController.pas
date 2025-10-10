unit F_TimeController;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  F8RealSpinEdit, Vcl.Buttons, Vcl.ToolWin, Vcl.ComCtrls, System.Actions,
  Vcl.ActnList,
  F8CrowdSimController, Vcl.ExtCtrls;

type
    /// <summary>
    ///    シミュレーションの再生/早送り/停止等を操作するフレームとその機能を定義するクラス
    /// </summary>
    TFrameControlTime = class(TFrame)
        ToolBarTrafficController: TToolBar;
        EditPlaySpeed: TF8RealSpinEdit;
        SpeedButtonPlay: TSpeedButton;
        SpeedButtonFastPlay: TSpeedButton;
        SpeedButtonPause: TSpeedButton;
        SpeedButtonStop: TSpeedButton;
        SpeedButtonBackPlay: TSpeedButton;
        ActionList: TActionList;
        ActionPlay: TAction;
        ActionPause: TAction;
        ActionStop: TAction;
        ActionBack: TAction;
        ActionSelectDir: TAction;
        ActionCheckAll: TAction;
        ActionUncheckAll: TAction;
        ActionCheckEverything: TAction;
        ActionUncheckEverything: TAction;
        ActionFast: TAction;
        SimEndTimePicker: TDateTimePicker;
        SimDatePicker: TDateTimePicker;
        PanelSimPlay: TPanel;
        GroupBoxReplay: TGroupBox;
        btnSimStop: TButton;
        PanelSimTime: TPanel;
        PanelTrafficController: TPanel;
        btnSimStart: TButton;
        SimTimePicker: TDateTimePicker;
        Label1: TLabel;
        GroupBox1: TGroupBox;
        rdSunny: TRadioButton;
        rdRain: TRadioButton;
            procedure ActionPauseExecute(Sender: TObject);
            procedure ActionPauseUpdate(Sender: TObject);
            procedure ActionPlayExecute(Sender: TObject);
            procedure ActionPlayUpdate(Sender: TObject);
            procedure ActionStopExecute(Sender: TObject);
            procedure ActionStopUpdate(Sender: TObject);
            procedure ActionFastUpdate(Sender: TObject);
            procedure ActionFastExcute(Sender: TObject);
            procedure ActionBackUpdate(Sender: TObject);
            procedure ActionBackExcute(Sender: TObject);
            procedure ActionReplayExecute(Sender: TObject);

            function  GetController: F8CrowdSimControllerClass;
            procedure SetController(const Value: F8CrowdSimControllerClass);
            procedure SimDatePickerChange(Sender: TObject);
            procedure SimTimePickerChange(Sender: TObject);

        private
            p_controller : F8CrowdSimControllerClass;
            FOnNotifyPlay : TNotifyEvent;
            FOnNotifyStop : TNotifyEvent;
            FOnNotifyReset : TNotifyEvent;
            p_Enable : Boolean;
            p_BeginningTime : TDateTime;

            procedure ActionsUpdate;
            procedure UpdateTimePickers;
            function  GetAllEnable: Boolean;
            procedure SetAllEnable(const Value: Boolean);

        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;

            function  UpdateTime : TDateTime;

            property  Controller : F8CrowdSimControllerClass read GetController write SetController;
            property  OnNotifyPlay : TNotifyEvent  read FOnNotifyPlay  write FOnNotifyPlay;
            property  OnNotifyStop : TNotifyEvent  read FOnNotifyStop  write FOnNotifyStop;
            property  OnNotifyReset : TNotifyEvent read FOnNotifyReset write FOnNotifyReset;

            property  AllEnable : Boolean read GetAllEnable write SetAllEnable;
    end;

implementation

uses
    System.DateUtils,
    MovingFeature,
    PluginCore;

//==============================================================================
procedure TFrameControlTime.AfterConstruction;
    var
        time : TDateTime;
    begin
    inherited;
    p_controller := nil;
     if MovingFeatureListClass.numberOfMovingFeatures > 0 then
        begin
        time := MovingFeatureListClass.beginningTime;
        SimTimePicker.Time := time;
        SimDatePicker.Date := time;
        end;
    end;

//==============================================================================
procedure TFrameControlTime.BeforeDestruction;
    begin
    if Assigned(p_controller) then
        p_controller := nil;
    inherited;
    end;

//==============================================================================
procedure TFrameControlTime.ActionReplayExecute(Sender: TObject);
    var
        time : TDatetime;
    begin
    if (p_controller.currentState = csStop) then
        begin
        time := SimTimePicker.Time;
        ReplaceDate(time,SimDatePicker.Date);
        p_Controller.AssignCurrentTime(time);
        p_BeginningTime := time;

        p_Controller.Replay;
        end;

    p_controller.playingSpeed := 1.0;
    ActionsUpdate;
    end;

//==============================================================================
function TFrameControlTime.UpdateTime: TDateTime;
    begin
    if Assigned(p_Controller) then
        begin
        Result := p_controller.currentTime;
        end
    else
        begin
        Result := Now;
        end;
    SimDatePicker.Date := Result;
    SimTimePicker.Time := Result;
    end;

//==============================================================================
procedure TFrameControlTime.UpdateTimePickers;
    var
        enable : Boolean;
    begin
    enable := (p_controller.CheckTheState in [csStop]);
    if enable then
        begin
        SimTimePicker.OnChange := SimTimePickerChange;
        SimDatePicker.OnChange := SimDatePickerChange;
        end
    else
        begin
        SimTimePicker.OnChange := nil;
        SimDatePicker.OnChange := nil;
        end;
    SimTimePicker.Enabled := enable;
    SimDatePicker.Enabled := enable;
    SimEndTimePicker.Enabled := enable;
    end;

//==============================================================================
procedure TFrameControlTime.ActionBackExcute(Sender: TObject);
    begin
    if p_controller.CheckTheState in [csPlay, csPause]   then
        begin
        p_controller.playingSpeed := -1*EditPlaySpeed.Value;
        end
    else
        begin
        p_controller.playingSpeed := 1.0;
        end;
    ActionsUpdate;
    end;

//==============================================================================
procedure TFrameControlTime.ActionBackUpdate(Sender: TObject);
    var
        res : Boolean;
    begin
    res := False;
    if not Assigned(p_controller) then
        begin
        res := False;
        end
    else
        begin
        if  (p_controller.CheckTheState = csBack) then
            begin
            res := True;
            end
        else if(p_controller.CheckTheState = csPause) and (p_controller.playingSpeed < 0.0) then
            begin
            res := True;
            end;
        end;

    SpeedButtonBackPlay.Down := res;
    EditPlaySpeed.Enabled := not res;
    end;

//==============================================================================
procedure TFrameControlTime.ActionFastExcute(Sender: TObject);
    begin
    if p_controller.CheckTheState in [csPlay, csPause]   then
        begin
        p_controller.playingSpeed := EditPlaySpeed.Value;
        end
    else
        begin
        p_controller.playingSpeed := 1.0;
        end;
    ActionsUpdate;
    end;

//==============================================================================
procedure TFrameControlTime.ActionFastUpdate(Sender: TObject);
    var
        res : Boolean;
    begin
    res := False;
    if not Assigned(p_controller) then
        begin
        res := False;
        end
    else
        begin
        if  (p_controller.CheckTheState = csFast) then
            begin
            res := True;
            end
        else if(p_controller.CheckTheState = csPause)
            and (p_controller.playingSpeed > 0.0)
            and (not (p_controller.playingSpeed <> 1.0))then
            begin
            res := True;
            end;
        end;

    SpeedButtonFastPlay.Down := res;
    EditPlaySpeed.Enabled := not res;
    end;

//==============================================================================
procedure TFrameControlTime.ActionPauseExecute(Sender: TObject);
    begin
    p_controller.Pause;
    ActionsUpdate;
    end;

//==============================================================================
procedure TFrameControlTime.ActionPauseUpdate(Sender: TObject);
    begin
    SpeedButtonPause.Down := Assigned(p_controller) and (p_controller.CheckTheState = csPause);
    end;

//==============================================================================
procedure TFrameControlTime.ActionPlayExecute(Sender: TObject);
    var
        time : TDatetime;
    begin
    if (p_controller.currentState = csStop) then
        begin
        time := SimTimePicker.Time;
        ReplaceDate(time,SimDatePicker.Date);
        if not theApplicationServices.project.UseDateTime then
            theApplicationServices.project.UseDateTime := true;

        theApplicationServices.project.DateTime := time;
        if Assigned(FOnNotifyReset) then
            FOnNotifyReset(Sender);
        p_controller.Reset;

        p_Controller.AssignCurrentTime(time);
        p_BeginningTime := time;

        p_controller.Play;
        if Assigned(FOnNotifyPlay) then
            FOnNotifyPlay(Sender);

        end;
    p_controller.playingSpeed := 1.0;
    ActionsUpdate;
    SpeedButtonPlay.Down := False;
    end;

//==============================================================================
procedure TFrameControlTime.ActionPlayUpdate(Sender: TObject);
    begin
    if not Assigned(p_controller) then
        begin
        SpeedButtonPlay.Down := False;
        end
    else
        begin
        if (p_controller.CheckTheState = csPlay) then
            begin
            SpeedButtonPlay.Down := True;
            end
        else if (p_controller.CheckTheState= csPause) and (p_controller.playingSpeed = 1.0) then
            begin
            SpeedButtonPlay.Down := True;
            end
        else
            begin
            SpeedButtonPlay.Down := False;
            end;
        end;
    end;

//==============================================================================
procedure TFrameControlTime.ActionStopExecute(Sender: TObject);
    begin
    if Assigned(FOnNotifyStop) then
        FOnNotifyStop(sender);

    p_controller.Stop;
    p_controller.AssignCurrenttime(p_BeginningTime);
    SimTimePicker.Time := p_BeginningTime;
    SimDatePicker.Date := p_BeginningTime;

    ActionsUpdate;
    end;

//==============================================================================
procedure TFrameControlTime.ActionStopUpdate(Sender: TObject);
    begin
    SpeedButtonStop.Down := false;
    end;

//==============================================================================
procedure TFrameControlTime.ActionsUpdate;
    begin
    ActionPlayUpdate(ActionPlay);
    ActionPauseUpdate(ActionPause);
    ActionStopUpdate(ActionStop);
    ActionFastUpdate(ActionFast);
    ActionBackUpdate(ActionBack);
    UpdateTimePickers;
    end;

//==============================================================================
procedure TFrameControlTime.SetController(const Value: F8CrowdSimControllerClass);
    var
        startDate, endDate: TDateTime;
    begin
    p_controller := Value;
    if Assigned(p_controller) then
        begin
        ReplaceDate(startDate, SimDatePicker.Date);
        ReplaceTime(startDate, SimTimePicker.TIme);
        ReplaceDate(endDate, SimDatePicker.Date);
        ReplaceTime(endDate, SimEndTimePicker.TIme);
        p_controller.AssignSimlationTime(startDate, endDate);
        end;
    end;

//==============================================================================
procedure TFrameControlTime.SimDatePickerChange(Sender: TObject);
    var
        time : TDatetime;
    begin
    time := p_Controller.Currenttime;
    ReplaceDate(time,SimDatePicker.Date);
    p_Controller.AssignCurrentTime(time);
    end;

//==============================================================================
procedure TFrameControlTime.SimTimePickerChange(Sender: TObject);
    var
        time : TDatetime;
    begin
    time := p_Controller.Currenttime;
    ReplaceTime(time,SimTimePicker.Time);
    p_Controller.AssignCurrentTime(time);
    end;

//==============================================================================
function TFrameControlTime.GetAllEnable: Boolean;
    begin
    Result := p_Enable;
    end;

//==============================================================================
procedure TFrameControlTime.SetAllEnable(const Value: Boolean);
    begin
    p_Enable := Value;
    SimDatePicker.Enabled := Value;
    SimTimePicker.Enabled := Value;
    EditPlaySpeed.Enabled := Value;
    SpeedButtonPlay.Enabled := Value;
    SpeedButtonFastPlay.Enabled := Value;
    SpeedButtonPause.Enabled := Value;
    SpeedButtonStop.Enabled := Value;
    SpeedButtonBackPlay.Enabled := Value;
    end;

//==============================================================================
function TFrameControlTime.GetController: F8CrowdSimControllerClass;
    begin
    Result := p_controller;
    end;

{$R *.dfm}

end.
