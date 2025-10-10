unit F_PedestrianDetailSettings;

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
    F_AgentAttributes,
    Vcl.StdCtrls,
    Vcl.ExtCtrls;

type
    TFramePedestrianDetailSettings = class(TFrame)
        PanelBottom: TPanel;
        grpbPedestrianSettings: TGroupBox;
        PanelPopPedestrianSettings: TPanel;
        F_PedAttr: TFrameAgentAttributes;
        private

        public

        end;

implementation

{$R *.dfm}

end.
