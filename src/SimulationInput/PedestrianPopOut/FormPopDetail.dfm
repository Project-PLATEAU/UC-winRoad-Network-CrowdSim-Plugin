object FormPopDetailSettings: TFormPopDetailSettings
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  Caption = #35443#32048#35373#23450
  ClientHeight = 395
  ClientWidth = 237
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  inline F_PopDetail: TFramePopDetailSetting
    Left = 0
    Top = 0
    Width = 237
    Height = 395
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 395
    inherited PanelMain: TPanel
      Height = 395
      ExplicitHeight = 395
      inherited sgPopDetail: TStringGrid
        Height = 334
        OnSelectCell = sgPopDetailSelectCell
        ExplicitHeight = 334
      end
      inherited PanelFotter: TPanel
        Top = 354
        ExplicitTop = 354
      end
    end
  end
end
