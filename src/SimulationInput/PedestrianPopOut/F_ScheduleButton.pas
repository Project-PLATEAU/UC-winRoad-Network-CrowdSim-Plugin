unit F_ScheduleButton;

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
    Vcl.ExtCtrls;

type
    TFrameScheduleButton = class(TFrame)
        PanelMain: TPanel;
        btnSetSchedule: TButton;
        private

        public

        end;

implementation

{$R *.dfm}

end.
