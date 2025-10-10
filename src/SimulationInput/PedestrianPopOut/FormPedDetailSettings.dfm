object FormPedestrianDetailSettings: TFormPedestrianDetailSettings
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  Caption = #20154#27969#12398#35443#32048#35373#23450
  ClientHeight = 476
  ClientWidth = 399
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
  inline F_PedDetail: TFramePedestrianDetailSettings
    Left = 0
    Top = 0
    Width = 399
    Height = 435
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 399
    ExplicitHeight = 435
    inherited PanelBottom: TPanel
      Width = 399
      Height = 435
      ExplicitWidth = 399
      ExplicitHeight = 435
      inherited grpbPedestrianSettings: TGroupBox
        Width = 389
        Height = 432
        ExplicitWidth = 389
        ExplicitHeight = 432
        inherited PanelPopPedestrianSettings: TPanel
          Width = 385
          Height = 415
          ExplicitWidth = 385
          ExplicitHeight = 415
          inherited F_PedAttr: TFrameAgentAttributes
            Width = 385
            Height = 415
            ExplicitWidth = 385
            ExplicitHeight = 415
            inherited PanelFAAMain: TPanel
              Width = 385
              Height = 415
              ExplicitWidth = 385
              ExplicitHeight = 415
              inherited GroupBoxAttributesSetting: TGroupBox
                Width = 379
                Height = 409
                ExplicitWidth = 379
                ExplicitHeight = 409
                inherited PanelDestination: TPanel
                  Height = 392
                  ExplicitHeight = 392
                  inherited cbbDest: TComboBox
                    OnChange = cbbDestinationChange
                  end
                  inherited ChartGoalDistribution: TChart
                    Height = 355
                    ExplicitHeight = 355
                    PrintMargins = (
                      15
                      -16
                      15
                      -16)
                    ColorPalette = (
                      7566335
                      33280
                      1621242
                      16564391
                      16777215
                      8487297
                      16711935
                      8553216
                      8519680
                      130
                      65280
                      33411
                      8585346
                      12632256
                      16776960
                      0
                      12770497
                      15846312
                      15858687
                      10854817)
                  end
                  inherited seDestPer: TF8RealSpinEdit
                    OnChange = seDestPerChange
                  end
                end
              end
            end
          end
        end
      end
    end
  end
  object PanelFotter: TPanel
    Left = 0
    Top = 435
    Width = 399
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TButton
      AlignWithMargins = True
      Left = 233
      Top = 6
      Width = 75
      Height = 25
      Margins.Top = 6
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alRight
      Caption = #30906#23450
      TabOrder = 0
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 321
      Top = 6
      Width = 75
      Height = 25
      Margins.Top = 6
      Margins.Bottom = 10
      Align = alRight
      Caption = #21462#28040
      TabOrder = 1
    end
  end
end
