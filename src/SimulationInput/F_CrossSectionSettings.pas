unit F_CrossSectionSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TFrameCrossSectionSettings = class(TFrame)
    PanelMain: TPanel;
    chbVisibleCrossSectionArea: TCheckBox;
    grpbWaitingAreaSettings: TGroupBox;
    lblAreaName: TLabel;
    edtWaitingAreaName: TEdit;
    btnSetWaitingArea: TButton;
    btnAddWaitingArea: TButton;
    lblWaitingAreaInfo: TLabel;
    MemoWaitingAreaInfos: TMemo;
    PanelCrossSectionSettings: TPanel;
    PanelVisibleCrossSectionArea: TPanel;
    PanelSensorAreaInfo: TPanel;
    GroupBoxSelectSensorArea: TGroupBox;
    LabelAreaDrawSelect: TLabel;
    ComboBoxDrawAreaName: TComboBox;
    ButtonDeleteSensorArea: TButton;
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
  end;

implementation

{$R *.dfm}

end.
