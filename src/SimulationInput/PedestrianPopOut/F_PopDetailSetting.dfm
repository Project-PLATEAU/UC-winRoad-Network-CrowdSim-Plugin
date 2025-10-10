object FramePopDetailSetting: TFramePopDetailSetting
  Left = 0
  Top = 0
  Width = 237
  Height = 290
  TabOrder = 0
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 237
    Height = 290
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object sgPopDetail: TStringGrid
      AlignWithMargins = True
      Left = 10
      Top = 10
      Width = 217
      Height = 229
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alClient
      ColCount = 2
      DefaultColWidth = 97
      RowCount = 25
      ScrollBars = ssVertical
      TabOrder = 0
      RowHeights = (
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24)
    end
    object PanelFotter: TPanel
      Left = 0
      Top = 249
      Width = 237
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnOK: TButton
        Left = 10
        Top = 3
        Width = 75
        Height = 25
        Caption = #30906#23450
        TabOrder = 0
      end
      object btnCancel: TButton
        Left = 152
        Top = 3
        Width = 75
        Height = 25
        Caption = #21462#28040
        TabOrder = 1
      end
    end
  end
end
