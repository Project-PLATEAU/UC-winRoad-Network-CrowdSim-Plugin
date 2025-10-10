object FrameCrossSectionSettings: TFrameCrossSectionSettings
  Left = 0
  Top = 0
  Width = 562
  Height = 316
  TabOrder = 0
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 562
    Height = 316
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 144
    ExplicitTop = 80
    ExplicitWidth = 185
    ExplicitHeight = 41
    object PanelCrossSectionSettings: TPanel
      Left = 0
      Top = 49
      Width = 265
      Height = 267
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitTop = 72
      ExplicitHeight = 453
      object grpbWaitingAreaSettings: TGroupBox
        AlignWithMargins = True
        Left = 10
        Top = 0
        Width = 245
        Height = 155
        Margins.Left = 10
        Margins.Top = 0
        Margins.Right = 10
        Margins.Bottom = 10
        Align = alTop
        Caption = #26029#38754#20132#36890#27969#35336#28204#31684#22258#25351#23450
        TabOrder = 0
        ExplicitLeft = 11
        ExplicitWidth = 249
        object lblAreaName: TLabel
          Left = 12
          Top = 64
          Width = 120
          Height = 13
          Caption = #26029#38754#20132#36890#27969#35336#28204#31684#22258#21517
        end
        object edtWaitingAreaName: TEdit
          Left = 12
          Top = 83
          Width = 229
          Height = 21
          TabOrder = 0
        end
        object btnSetWaitingArea: TButton
          Left = 12
          Top = 25
          Width = 85
          Height = 25
          Caption = #31684#22258#12434#36984#25246
          TabOrder = 1
        end
        object btnAddWaitingArea: TButton
          AlignWithMargins = True
          Left = 12
          Top = 113
          Width = 224
          Height = 25
          Margins.Left = 10
          Margins.Right = 7
          Margins.Bottom = 15
          Align = alBottom
          Caption = #26029#38754#20132#36890#37327#35336#28204#31684#22258#12434#36861#21152
          TabOrder = 2
          ExplicitWidth = 228
        end
      end
      object GroupBoxSelectSensorArea: TGroupBox
        AlignWithMargins = True
        Left = 10
        Top = 168
        Width = 245
        Height = 89
        Margins.Left = 10
        Margins.Right = 10
        Margins.Bottom = 10
        Align = alClient
        Caption = #26029#38754#20132#36890#27969#35336#28204#31684#22258#30906#35469
        TabOrder = 1
        ExplicitHeight = 289
        object LabelAreaDrawSelect: TLabel
          Left = 12
          Top = 24
          Width = 54
          Height = 13
          Caption = #35336#28204#31684#22258#65306
        end
        object ComboBoxDrawAreaName: TComboBox
          Left = 70
          Top = 21
          Width = 170
          Height = 21
          Style = csDropDownList
          TabOrder = 0
        end
        object ButtonDeleteSensorArea: TButton
          Left = 12
          Top = 48
          Width = 224
          Height = 25
          Caption = #36984#25246#12375#12383#31684#22258#12434#21066#38500#12377#12427
          TabOrder = 1
        end
      end
    end
    object PanelVisibleCrossSectionArea: TPanel
      Left = 0
      Top = 0
      Width = 562
      Height = 49
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object chbVisibleCrossSectionArea: TCheckBox
        Left = 16
        Top = 16
        Width = 241
        Height = 17
        Caption = #35373#23450#12375#12383#26029#38754#20132#36890#27969#35336#28204#31684#22258#12434#21487#35222#21270#12377#12427
        TabOrder = 0
      end
    end
    object PanelSensorAreaInfo: TPanel
      Left = 265
      Top = 49
      Width = 297
      Height = 267
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitLeft = 261
      ExplicitHeight = 476
      object lblWaitingAreaInfo: TLabel
        Left = 11
        Top = 0
        Width = 132
        Height = 13
        Caption = #26029#38754#20132#36890#27969#35336#28204#31684#22258#24773#22577
      end
      object MemoWaitingAreaInfos: TMemo
        AlignWithMargins = True
        Left = 10
        Top = 20
        Width = 267
        Height = 232
        Margins.Left = 10
        Margins.Top = 20
        Margins.Right = 20
        Margins.Bottom = 15
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        ExplicitTop = 25
        ExplicitHeight = 436
      end
    end
  end
end
