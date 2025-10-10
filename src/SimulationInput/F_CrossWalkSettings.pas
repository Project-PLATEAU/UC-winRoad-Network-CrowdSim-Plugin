unit F_CrossWalkSettings;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
    Vcl.ComCtrls, F8RealSpinEdit;

type
    TFrameCrossWalkSettings = class(TFrame)
        PanelMain: TPanel;
        PageControlEditMesh: TPageControl;
        TabSheetEditCrosswalk: TTabSheet;
        rbAddCrosswalk: TRadioButton;
        rbToCrosswalk: TRadioButton;
        Panel1: TPanel;
        Label3: TLabel;
        rseSignalInterval: TF8RealSpinEdit;
        Panel2: TPanel;
        lbCrosswalks: TListBox;
        GroupBox2: TGroupBox;
        Panel13: TPanel;
        rbBlueSignal: TRadioButton;
        rbRedSignal: TRadioButton;
        Panel14: TPanel;
        Panel15: TPanel;
        rbFloor: TRadioButton;
        Panel18: TPanel;
        PanelNetwork: TPanel;
        LabelNetwork: TLabel;
        cbbNetwork: TComboBox;
        Panel16: TPanel;
        Panel17: TPanel;
        btnSetCrosswalk: TButton;
        Label5: TLabel;
        btnlbCrosswalksUpdate: TButton;
        Label4: TLabel;
        PanelCrosswalkWidth: TPanel;
        btnChangeCWName: TButton;
        Label1: TLabel;
        seRedSignalInterval: TF8RealSpinEdit;
        btnDeleteCW: TButton;
        private
            { Private êÈåæ }
        public
            { Public êÈåæ }
        end;

implementation

{$R *.dfm}

end.
