unit F_PedestrianPopPointSettings;

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
    PluginCore,
    F8GLUtils,
    F_PedestrianPopbyInterval,
    F_PedestrianOutbyInterval,
    F_PopPerHour,
    F_ScheduleButton,
    System.ImageList,
    Vcl.ImgList,
    Vcl.Buttons, F8RealSpinEdit,
    PopOutPointList,
    PedestrianProfileOptionData,
    PedestrianCategoryData,
    SimulationInputUtils;

const
    _PByInterval = 0;
    _PBySchedule = 1;
    _PPerHour    = 2;

type
    TFramePedestrianPopPointSettings = class(TFrame)
        PanelTop: TPanel;
        PanelMain: TPanel;
        btnLookAt: TButton;
        grpbPopRule: TGroupBox;
        lblSelectPopRule: TLabel;
        cbbSelectPopRule: TComboBox;
        PanelRuleFrame: TPanel;
        btnSelectPopPoint: TButton;
        PanelPedLbls: TPanel;
        cbbPedLabel: TComboBox;
        Label1: TLabel;
        grpbPedLabel: TGroupBox;
        PanelDetailSettings: TPanel;
        btnPedDetailSettings: TButton;
        btnAddLabel: TSpeedButton;
        ImageList1: TImageList;
        btnDelLabel: TSpeedButton;
        lbPopLbls: TListBox;
        Label2: TLabel;
        Label3: TLabel;
        seLabelPopPer: TF8RealSpinEdit;
        Label4: TLabel;
        seLabelPopNum: TF8RealSpinEdit;
        PanelBusModel: TPanel;
        lblBusModel: TLabel;
        cbbBusModel: TComboBox;
        grpbTermOrGate: TGroupBox;
        rbBusTerminal: TRadioButton;
        rbTicketGate: TRadioButton;
        rbNormal: TRadioButton;
        Label5: TLabel;
        cbbBusRoad: TComboBox;
        procedure cbbSelectPopRuleChange(Sender: TObject);
        procedure lbPopLblsClick(Sender: TObject);
        procedure ComboBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
        procedure rbPointAttrsClick(Sender: TObject);
        procedure cbbBusModelChange(Sender: TObject);
        procedure cbbBusRoadChange(Sender: TObject);
        private
            PedPopIntervalFrame : TFramePedestrianPopbyInterval;
            ScheduleButtonFrame : TFrameScheduleButton;
            PedPopPerHourFrame  : TFramePedestrianPopPerHour;
            p_PopOutPointList     : TPopOutPointList;
            p_AllPedestrianLabelList : TList<TPedestrianData>;
            p_PedestrianProfileOptionList : TPedestrianProfileOptionList;
            p_PedestrianCategoryList      : TPedestrianCategoryList;
            p_lbIdx : integer;
            p_ItemName : string;
            p_car : IF8carInstance;

            function  GetPOPointList: TPopOutPointList;
            procedure SetPOPointList(const aValue: TPopOutPointList);
            function  GetPedLblList: TList<TPedestrianData>;
            procedure SetPedLblList(const aValue: TList<TPedestrianData>);
            function  GetPedestrianProfileOptionList: TPedestrianProfileOptionList;
            procedure SetPedestrianProfileOptionList(const aValue: TPedestrianProfileOptionList);
            function  GetPedestrianCategoryList: TPedestrianCategoryList;
            procedure SetPedestrianCategoryList(const aValue: TPedestrianCategoryList);
            procedure SetPedPopIntervalFrame;
            procedure SetScheduleButtonFrame;
            procedure SetPedPopPerHourFrame;
            procedure DrawBitmapInRect(thumbNail: TBitmap; const name: String; canvas: TCanvas; rect: TRect; const useEllipsisIfTooLong: Boolean = false);
            procedure FindTotalIndex(out idx: integer);
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure HideDetailSettingsFrame;
            procedure InitializeComboBox;

            property  F_PedPopInterval : TFramePedestrianPopbyInterval read PedPopIntervalFrame;
            property  F_ScheduleButton : TFrameScheduleButton          read ScheduleButtonFrame;
            property  F_PedPopPerHour  : TFramePedestrianPopPerHour    read PedPopPerHourFrame;
            property  POPointList : TPopOutPointList read GetPOPointList write SetPOPointList;
            property  AllPedestrianLabelList : TList<TPedestrianData> read GetPedLblList write SetPedLblList;
            property  PedestrianProfileOptionList : TPedestrianProfileOptionList read GetPedestrianProfileOptionList write SetPedestrianProfileOptionList;
            property  PedestrianCategoryList      : TPedestrianCategoryList      read GetPedestrianCategoryList      write SetPedestrianCategoryList;
            property  lbIdx : integer read p_lbIdx write p_lbIdx;
            property  ItemName : string read p_ItemName write p_ItemName;
        end;

implementation

{$R *.dfm}

procedure TFramePedestrianPopPointSettings.AfterConstruction;
    var
        i : integer;
    begin
    inherited;
    p_car := nil;
    PedPopIntervalFrame := TFramePedestrianPopbyInterval.Create(self);
    ScheduleButtonFrame := TFrameScheduleButton.Create(self);
    PedPopPerHourFrame  := TFramePedestrianPopPerHour.Create(self);
    for i := 1 to theApplicationServices.project.numberOfRoads do
        cbbBusRoad.Items.Add(theApplicationServices.project.road[i].name);
    end;

procedure TFramePedestrianPopPointSettings.BeforeDestruction;
    begin
    inherited;
    FreeAndNil(PedPopIntervalFrame);
    FreeAndNil(ScheduleButtonFrame);
    FreeAndNil(PedPopPerHourFrame);
    end;

procedure TFramePedestrianPopPointSettings.SetPedPopIntervalFrame;
    begin
    if Assigned(PedPopPerHourFrame) then
        PedPopPerHourFrame.Visible := false;

    if Assigned(ScheduleButtonFrame) then
        ScheduleButtonFrame.Visible := false;

    PedPopIntervalFrame.Parent  := PanelRuleFrame;
    PedPopIntervalFrame.Name    := 'FramePopInterval';
    PedPopIntervalFrame.Align   := alClient;
    PedPopIntervalFrame.Visible := true;
    seLabelPopNum.Enabled       := false;
    end;

procedure TFramePedestrianPopPointSettings.SetScheduleButtonFrame;
    begin
    if Assigned(PedPopIntervalFrame) then
        PedPopIntervalFrame.Visible := false;

    if Assigned(PedPopPerHourFrame) then
        PedPopPerHourFrame.Visible := false;

    ScheduleButtonFrame.Parent  := PanelRuleFrame;
    ScheduleButtonFrame.Name    := 'FrameScheBtn';
    ScheduleButtonFrame.Align   := alClient;
    ScheduleButtonFrame.Visible := true;
    seLabelPopNum.Enabled       := false;
    end;

procedure TFramePedestrianPopPointSettings.SetPedPopPerHourFrame;
    begin
    if Assigned(PedPopIntervalFrame) then
        PedPopIntervalFrame.Visible := false;

    if Assigned(ScheduleButtonFrame) then
        ScheduleButtonFrame.Visible := false;

    PedPopPerHourFrame.Parent  := PanelRuleFrame;
    PedPopPerHourFrame.Name    := 'FramePopHour';
    PedPopPerHourFrame.Align   := alClient;
    PedPopPerHourFrame.Visible := true;
    seLabelPopNum.Enabled      := false;
    end;

procedure TFramePedestrianPopPointSettings.HideDetailSettingsFrame;
    begin
    if Assigned(PedPopIntervalFrame) then
        PedPopIntervalFrame.Visible := false;

    if Assigned(ScheduleButtonFrame) then
        ScheduleButtonFrame.Visible := false;

    if Assigned(PedPopPerHourFrame) then
        PedPopPerHourFrame.Visible := false;
    end;

procedure TFramePedestrianPopPointSettings.lbPopLblsClick(Sender: TObject);
    begin
    if lbPopLbls.ItemIndex >= 0 then
        begin
        btnPedDetailSettings.Enabled := true;
        if cbbSelectPopRule.ItemIndex <> 1 then
            seLabelPopNum.Enabled    := true
        else
            seLabelPopNum.Enabled    := false;
        end
    else
        begin
        btnPedDetailSettings.Enabled := false;
        seLabelPopNum.Enabled        := false;
        end;
    end;

procedure TFramePedestrianPopPointSettings.FindTotalIndex(out idx: integer);
    var
        i : integer;
    begin
    idx := -1;
    if lbIdx >= 0 then
        begin
        idx := lbIdx;
        for i := 0 to POPointList.Data.Count - 1 do
            begin
            if ItemName = POPointList.Data[i].Name then
                begin
                idx := i;
                Break;
                end;
            end;
        end;
    end;

procedure TFramePedestrianPopPointSettings.rbPointAttrsClick(Sender: TObject);
    var
        SelIdx, i : integer;
    begin
    FindTotalIndex(SelIdx);
    if SelIdx < 0 then
        Exit;

    if Sender = rbNormal then
        begin
        POPointList.Data[SelIdx].PointAttr := _Normal;
        cbbBusModel.Enabled := false;
        cbbBusRoad.Enabled  := false;
        end
    else if Sender = rbBusTerminal then
        begin
        POPointList.Data[SelIdx].PointAttr := _BusTerminal;
        cbbBusModel.Enabled := true;
        cbbBusRoad.Enabled  := true;
        end
    else if Sender = rbTicketGate then
        begin
        POPointList.Data[SelIdx].PointAttr := _TicketGate;
        cbbBusModel.Enabled := false;
        cbbBusRoad.Enabled  := false;
        end;

    for i := 0 to POPointList.Data.Count - 1 do
        begin
        if (POPointList.Data[i].PointType = _PopPoint) then
            Continue;

        if (POPointList.Data[SelIdx].NodeIdx[1] = POPointList.Data[i].NodeIdx[1]) then
            begin
            POPointList.Data[i].PointAttr := POPointList.Data[SelIdx].PointAttr;
            Break;
            end;
        end;
    end;

procedure TFramePedestrianPopPointSettings.cbbBusModelChange(Sender: TObject);
    var
        cb : TComboBox;
        ch : IF8ThreeDeeStudio;
        SelIdx, i : integer;
    begin
    FindTotalIndex(SelIdx);
    if SelIdx < 0 then
        Exit;

    cb := Sender as TComboBox;
    if Supports(cb.items.Objects[cb.ItemIndex], IF8ThreeDeeStudio, ch) then
        POPointList.Data[SelIdx].BusModel := ch;

    for i := 0 to POPointList.Data.Count - 1 do
        begin
        if (POPointList.Data[i].PointType = _PopPoint) then
            Continue;

        if (POPointList.Data[SelIdx].NodeIdx[1] = POPointList.Data[i].NodeIdx[1]) then
            begin
            POPointList.Data[i].BusModel := ch;
            Break;
            end;
        end;
    end;

procedure TFramePedestrianPopPointSettings.cbbBusRoadChange(Sender: TObject);
    var
        SelIdx, i : integer;
    begin
    FindTotalIndex(SelIdx);
    if SelIdx < 0 then
        Exit;

    POPointList.Data[SelIdx].BusRoadIndex := cbbBusRoad.ItemIndex + 1;
    for i := 0 to POPointList.Data.Count - 1 do
        begin
        if (POPointList.Data[i].PointType = _PopPoint) then
            Continue;

        if (POPointList.Data[SelIdx].NodeIdx[1] = POPointList.Data[i].NodeIdx[1]) then
            begin
            POPointList.Data[i].BusRoadIndex := cbbBusRoad.ItemIndex + 1;
            Break;
            end;
        end;
    end;

procedure TFramePedestrianPopPointSettings.cbbSelectPopRuleChange(Sender: TObject);
    begin
    if cbbSelectPopRule.ItemIndex = _PByInterval then
        SetPedPopIntervalFrame
    else if cbbSelectPopRule.ItemIndex = _PBySchedule then
        SetScheduleButtonFrame
    else if cbbSelectPopRule.ItemIndex = _PPerHour then
        SetPedPopPerHourFrame
    else
        HideDetailSettingsFrame;

    if cbbSelectPopRule.ItemIndex = _PBySchedule then
        begin
        PanelPedLbls.Visible := false;
        PanelDetailSettings.Visible := false;
        end
    else
        begin
        PanelDetailSettings.Visible := true;
        PanelPedLbls.Visible := true;
        end;
    end;

procedure TFramePedestrianPopPointSettings.ComboBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    var
        cb  : TComboBox;
        r : TRect;
        bitmap : TBitmap;
        cc  : IF8ModelResource;
    begin
    cb := Control as TComboBox;
    bitmap := TBitmap.Create;
    try
        bitmap.Width := 60;
        bitmap.Height := 60;
        r.Top := 1;
        r.Left := 1;
        r.Bottom := 58;
        r.Right := 58;
        if Supports(cb.Items.Objects[Index], IF8ModelResource, cc) then
            bitmap.Canvas.StretchDraw(r, cc.GetThumbnail);
        DrawBitmapInRect(bitmap,
                         cb.Items.Strings[Index],
                         cb.Canvas,
                         Rect);
    finally
        bitmap.Free;
        end;
    end;

procedure TFramePedestrianPopPointSettings.DrawBitmapInRect(thumbNail: TBitmap; const name: String; canvas: TCanvas; rect: TRect; const useEllipsisIfTooLong: Boolean);
    const
        OFFSET = 2;
    var
        displayedName: String;
        lastNameCharacterIndex: Integer;
        maxTextWidth: Integer;
        textExtent: TSize;
        textWidth: Integer;
    begin
    canvas.FillRect(rect);
    if Assigned(thumbnail) then
        BitBlt(canvas.Handle, rect.Left + 1, rect.Top + 1, rect.Right - rect.Left - 1,
                            rect.Bottom - rect.Top - 1, thumbnail.Canvas.Handle, 1, 1, SRCCOPY);

    displayedName := name;
    textExtent := Canvas.TextExtent(displayedName);

    if useEllipsisIfTooLong then     // Equivalent to Windows' DT_END_ELLIPSIS
        begin
        maxTextWidth := rect.Right - (rect.Left + OFFSET) - 1;
        textWidth := textExtent.cx;
        if textWidth > maxTextWidth then
            begin
            lastNameCharacterIndex := Length(displayedName);
            displayedName := displayedName + '...';
            repeat
                Delete(displayedName, lastNameCharacterIndex, 1);
                Dec(lastNameCharacterIndex);
                textWidth := Canvas.TextWidth(displayedName);
            until (textWidth <= maxTextWidth) or (lastNameCharacterIndex = 1);
            end;
        end;
    Canvas.TextOut(rect.Left + OFFSET, rect.Bottom - textExtent.cy - 2, displayedName);
    end;

procedure TFramePedestrianPopPointSettings.InitializeComboBox;
    var
        i   : Integer;
    begin
    cbbBusModel.Clear;
    for i := 1 to theApplicationServices.project.numberOfthreeDModels do
        begin
        if theApplicationServices.project.threeDModel[i].modelGroup = _VehicleModel then
            cbbBusModel.AddItem(theApplicationServices.project.threeDModel[i].name, TObject(theApplicationServices.project.threeDModel[i]));
        end;
    end;

function TFramePedestrianPopPointSettings.GetPOPointList: TPopOutPointList;
    begin
    Result := p_PopOutPointList;
    end;

procedure TFramePedestrianPopPointSettings.SetPOPointList(const aValue: TPopOutPointList);
    begin
    p_PopOutPointList := aValue;
    end;

function TFramePedestrianPopPointSettings.GetPedLblList: TList<TPedestrianData>;
    begin
    Result := p_AllPedestrianLabelList;
    end;

procedure TFramePedestrianPopPointSettings.SetPedLblList(const aValue: TList<TPedestrianData>);
    begin
    p_AllPedestrianLabelList := aValue;
    end;

function TFramePedestrianPopPointSettings.GetPedestrianProfileOptionList: TPedestrianProfileOptionList;
    begin
    result := p_PedestrianProfileOptionList;
    end;

procedure TFramePedestrianPopPointSettings.SetPedestrianProfileOptionList(const aValue: TPedestrianProfileOptionList);
    begin
    p_PedestrianProfileOptionList := aValue;
    end;

function TFramePedestrianPopPointSettings.GetPedestrianCategoryList: TPedestrianCategoryList;
    begin
    result := p_PedestrianCategoryList;
    end;

procedure TFramePedestrianPopPointSettings.SetPedestrianCategoryList(const aValue: TPedestrianCategoryList);
    begin
    p_PedestrianCategoryList := aValue;
    end;
end.
