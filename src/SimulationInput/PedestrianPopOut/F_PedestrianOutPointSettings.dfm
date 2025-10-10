object FramePedestrianOutPointSettings: TFramePedestrianOutPointSettings
  Left = 0
  Top = 0
  Width = 400
  Height = 330
  TabOrder = 0
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 400
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btnLookAt: TButton
      AlignWithMargins = True
      Left = 208
      Top = 8
      Width = 182
      Height = 25
      Margins.Left = 10
      Margins.Top = 8
      Margins.Right = 10
      Margins.Bottom = 8
      Align = alRight
      Caption = #35373#23450#12375#12383#22320#28857#12395#35222#28857#12434#31227#21205
      TabOrder = 0
    end
    object btnSelectOutPoint: TButton
      AlignWithMargins = True
      Left = 10
      Top = 8
      Width = 186
      Height = 25
      Margins.Left = 10
      Margins.Top = 8
      Margins.Right = 10
      Margins.Bottom = 8
      Align = alLeft
      Caption = #30446#30340#22320#12434#22793#26356
      TabOrder = 1
    end
  end
  object PanelMain: TPanel
    Left = 0
    Top = 41
    Width = 400
    Height = 286
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object grpbOutRule: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 135
      Width = 390
      Height = 141
      Margins.Left = 5
      Margins.Right = 5
      Margins.Bottom = 10
      Align = alClient
      Caption = #20154#27969#36864#20986#35215#21063
      TabOrder = 0
      ExplicitLeft = 10
      ExplicitWidth = 380
      object lblSelectOutRule: TLabel
        Left = 16
        Top = 24
        Width = 112
        Height = 13
        Caption = #20154#27969#36864#20986#35215#21063#12398#22522#28310#65306
      end
      object cbbSelectOutRule: TComboBox
        Left = 134
        Top = 21
        Width = 211
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = #35215#21063#12394#12375
        OnChange = cbbSelectOutRuleChange
        Items.Strings = (
          #35215#21063#12394#12375
          #26178#38291#21608#26399
          #12480#12452#12516)
      end
      object PanelRuleFrame: TPanel
        AlignWithMargins = True
        Left = 2
        Top = 50
        Width = 386
        Height = 89
        Margins.Left = 0
        Margins.Top = 35
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitWidth = 376
      end
    end
    object grpbTermOrGate: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 3
      Width = 390
      Height = 126
      Margins.Left = 5
      Margins.Right = 5
      Align = alTop
      Caption = #22320#28857#31278#21029
      TabOrder = 1
      ExplicitLeft = 10
      ExplicitWidth = 380
      object rbBusTerminal: TRadioButton
        Left = 150
        Top = 15
        Width = 79
        Height = 17
        Caption = #12496#12473#20572
        TabOrder = 0
        OnClick = rbPointAttrsClick
      end
      object rbTicketGate: TRadioButton
        Left = 285
        Top = 15
        Width = 73
        Height = 17
        Caption = #25913#26413
        TabOrder = 1
        OnClick = rbPointAttrsClick
      end
      object rbNormal: TRadioButton
        Left = 12
        Top = 15
        Width = 93
        Height = 17
        Caption = #25351#23450#12394#12375
        Checked = True
        TabOrder = 2
        TabStop = True
        OnClick = rbPointAttrsClick
      end
      object PanelBusModel: TPanel
        AlignWithMargins = True
        Left = 2
        Top = 40
        Width = 386
        Height = 80
        Margins.Left = 0
        Margins.Top = 25
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 3
        ExplicitWidth = 376
        DesignSize = (
          386
          80)
        object lblBusModel: TLabel
          Left = 10
          Top = 32
          Width = 64
          Height = 13
          Caption = #12496#12473#12398#12514#12487#12523#65306
        end
        object Label1: TLabel
          Left = 188
          Top = 32
          Width = 30
          Height = 13
          Caption = #36947#36335#65306
        end
        object cbbBusModel: TComboBox
          Left = 79
          Top = 3
          Width = 79
          Height = 72
          Margins.Left = 6
          Margins.Top = 0
          Margins.Right = 6
          Margins.Bottom = 6
          Style = csOwnerDrawFixed
          Anchors = []
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemHeight = 66
          MaxLength = 6
          ParentFont = False
          TabOrder = 0
          OnChange = cbbBusModelChange
          OnDrawItem = ComboBoxDrawItem
          ExplicitLeft = 76
        end
        object cbbBusRoad: TComboBox
          Left = 224
          Top = 29
          Width = 145
          Height = 21
          Style = csDropDownList
          Enabled = False
          TabOrder = 1
          OnChange = cbbBusRoadChange
        end
      end
    end
  end
end
