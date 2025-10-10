object FramePedestrianDetailSettings: TFramePedestrianDetailSettings
  Left = 0
  Top = 0
  Width = 395
  Height = 403
  TabOrder = 0
  object PanelBottom: TPanel
    Left = 0
    Top = 0
    Width = 395
    Height = 403
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object grpbPedestrianSettings: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 0
      Width = 385
      Height = 400
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 5
      Align = alClient
      Caption = #20154#27969#35373#23450
      TabOrder = 0
      object PanelPopPedestrianSettings: TPanel
        AlignWithMargins = True
        Left = 2
        Top = 15
        Width = 381
        Height = 383
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        inline F_PedAttr: TFrameAgentAttributes
          Left = 0
          Top = 0
          Width = 381
          Height = 383
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 381
          ExplicitHeight = 383
          inherited PanelFAAMain: TPanel
            Width = 381
            Height = 383
            ExplicitWidth = 381
            ExplicitHeight = 383
            inherited GroupBoxAttributesSetting: TGroupBox
              Width = 375
              Height = 377
              ExplicitWidth = 375
              ExplicitHeight = 377
              inherited PanelDestination: TPanel
                Height = 360
                ExplicitHeight = 360
                inherited ChartGoalDistribution: TChart
                  Height = 323
                  ExplicitHeight = 323
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
              end
            end
          end
          inherited ImageList1: TImageList
            Left = 56
            Top = 232
          end
        end
      end
    end
  end
end
