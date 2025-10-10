unit F_PedestrianPopOutSettings;

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
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    F_PedestrianPopPointSettings,
    F_PedestrianOutPointSettings,
    Vcl.Buttons,
    System.ImageList,
    Vcl.ImgList,
    PopOutPointList,
    PedestrianProfileOptionData,
    PedestrianCategoryData;

const
    _PopPoint = 0;
    _OutPoint = 1;

type
    TFramePedestrianPopOutSettings = class(TFrame)
        PanelMain: TPanel;
        ListBoxInOutPoints: TListBox;
        PanelPopOutPoints: TPanel;
        btnChangePointName: TButton;
        btnAdd: TSpeedButton;
        ImageList1: TImageList;
        btnDelete: TSpeedButton;
        PanelPedestrianSettings: TPanel;
        btnPedSetting: TButton;
        grpbPointSettingType: TGroupBox;
        rbNormalPoint: TRadioButton;
        rbDispersionPoint: TRadioButton;
        chbVisibleDispersionPoint: TCheckBox;
        lbOutOnly: TListBox;
        Label1: TLabel;
        Label2: TLabel;
        btnAddOutP: TSpeedButton;
        btnDelOutP: TSpeedButton;
        btnLabelSetting: TButton;
        private
            PopPointSettingsFrame: TFramePedestrianPopPointSettings;
            OutPointSettingsFrame: TFramePedestrianOutPointSettings;
            p_PopOutPointList     : TPopOutPointList;
            p_AllPedestrianLabelList : TList<TPedestrianData>;
            p_PedestrianProfileOptionList : TPedestrianProfileOptionList;
            p_PedestrianCategoryList      : TPedestrianCategoryList;

            function  GetPOPointList: TPopOutPointList;
            procedure SetPOPointList(const aValue: TPopOutPointList);
            function  GetPedLblList: TList<TPedestrianData>;
            procedure SetPedLblList(const aValue: TList<TPedestrianData>);
            function  GetPedestrianProfileOptionList: TPedestrianProfileOptionList;
            procedure SetPedestrianProfileOptionList(const aValue: TPedestrianProfileOptionList);
            function  GetPedestrianCategoryList: TPedestrianCategoryList;
            procedure SetPedestrianCategoryList(const aValue: TPedestrianCategoryList);
        public
            procedure AfterConstruction; override;
            procedure BeforeDestruction; override;
            procedure SetPopPointSettingsFrame;
            procedure SetOutPointSettingsFrame;

            property  PopSettingsFrame : TFramePedestrianPopPointSettings read PopPointSettingsFrame;
            property  OutSettingsFrame : TFramePedestrianOutPointSettings read OutPointSettingsFrame;
            property  POPointList : TPopOutPointList read GetPOPointList write SetPOPointList;
            property  AllPedestrianLabelList : TList<TPedestrianData> read GetPedLblList write SetPedLblList;
            property  PedestrianProfileOptionList : TPedestrianProfileOptionList read GetPedestrianProfileOptionList write SetPedestrianProfileOptionList;
            property  PedestrianCategoryList      : TPedestrianCategoryList      read GetPedestrianCategoryList      write SetPedestrianCategoryList;
        end;

implementation

{$R *.dfm}

procedure TFramePedestrianPopOutSettings.AfterConstruction;
    begin
    PopPointSettingsFrame := TFramePedestrianPopPointSettings.Create(self);
    OutPointSettingsFrame := TFramePedestrianOutPointSettings.Create(self);
    end;

procedure TFramePedestrianPopOutSettings.BeforeDestruction;
    begin
    FreeAndNil(OutPointSettingsFrame);
    FreeAndNil(PopPointSettingsFrame);
    end;

procedure TFramePedestrianPopOutSettings.SetPopPointSettingsFrame;
    begin
    if Assigned(OutPointSettingsFrame) then
        OutPointSettingsFrame.Visible := false;

    PopPointSettingsFrame.Parent  := PanelMain;
    PopPointSettingsFrame.Name    := 'FramePopPointSettings';
    PopPointSettingsFrame.Align   := alClient;
    PopPointSettingsFrame.Visible := true;
    end;

procedure TFramePedestrianPopOutSettings.SetOutPointSettingsFrame;
    begin
    if Assigned(PopPointSettingsFrame) then
        PopPointSettingsFrame.Visible := false;

    OutPointSettingsFrame.Parent  := PanelMain;
    OutPointSettingsFrame.Name    := 'FrameOutPointSettings';
    OutPointSettingsFrame.Align   := alClient;
    OutPointSettingsFrame.Visible := true;
    end;

function TFramePedestrianPopOutSettings.GetPOPointList: TPopOutPointList;
    begin
    Result := p_PopOutPointList;
    end;

procedure TFramePedestrianPopOutSettings.SetPOPointList(const aValue: TPopOutPointList);
    begin
    p_PopOutPointList := aValue;
    end;

function TFramePedestrianPopOutSettings.GetPedLblList: TList<TPedestrianData>;
    begin
    Result := p_AllPedestrianLabelList;
    end;

procedure TFramePedestrianPopOutSettings.SetPedLblList(const aValue: TList<TPedestrianData>);
    begin
    p_AllPedestrianLabelList := aValue;
    end;

function TFramePedestrianPopOutSettings.GetPedestrianProfileOptionList: TPedestrianProfileOptionList;
    begin
    result := p_PedestrianProfileOptionList;
    end;

procedure TFramePedestrianPopOutSettings.SetPedestrianProfileOptionList(const aValue: TPedestrianProfileOptionList);
    begin
    p_PedestrianProfileOptionList := aValue;
    end;

function TFramePedestrianPopOutSettings.GetPedestrianCategoryList: TPedestrianCategoryList;
    begin
    result := p_PedestrianCategoryList;
    end;

procedure TFramePedestrianPopOutSettings.SetPedestrianCategoryList(const aValue: TPedestrianCategoryList);
    begin
    p_PedestrianCategoryList := aValue;
    end;
end.
