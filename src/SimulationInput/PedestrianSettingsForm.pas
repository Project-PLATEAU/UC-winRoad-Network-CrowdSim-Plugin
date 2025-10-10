unit PedestrianSettingsForm;

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
    F_PedestrianSettings;

type
    TFormPedestrianSettings = class(TForm)
        F_PedSetting: TFramePedestrianSettings;
        procedure FormShow(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure F_PedSettingbtnOKClick(Sender: TObject);
    procedure F_PedSettingbtnCancelClick(Sender: TObject);
        private

        public

        end;

implementation

{$R *.dfm}

procedure TFormPedestrianSettings.FormShow(Sender: TObject);
    begin
    F_PedSetting.HideCategoryEditor;
    F_PedSetting.ResetNodes;
    F_PedSetting.Enabled := true;
    end;

procedure TFormPedestrianSettings.F_PedSettingbtnOKClick(Sender: TObject);
    begin
    ModalResult := mrOK;
    end;

procedure TFormPedestrianSettings.F_PedSettingbtnCancelClick(Sender: TObject);
    begin
    Close;
    end;

procedure TFormPedestrianSettings.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
    F_PedSetting.Enabled := false;
    end;
end.
