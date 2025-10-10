object FrameExitInterval: TFrameExitInterval
  Left = 0
  Top = 0
  Width = 425
  Height = 102
  TabOrder = 0
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 425
    Height = 102
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object grpbExitInterval: TGroupBox
      AlignWithMargins = True
      Left = 10
      Top = 3
      Width = 405
      Height = 89
      Margins.Left = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alClient
      Caption = #36864#20986#26178#38291#12398#35373#23450
      TabOrder = 0
      object lblExitInterval: TLabel
        Left = 16
        Top = 24
        Width = 149
        Height = 13
        Caption = #24453#27231#21015#12363#12425#25244#12369#12427#26178#38291#12398#38291#38548#65306
      end
      object lblExitNum: TLabel
        Left = 16
        Top = 53
        Width = 148
        Height = 13
        Caption = #24453#27231#21015#12363#12425#19968#24230#12395#25244#12369#12427#20154#25968#65306
      end
      object seExitInterval: TF8RealSpinEdit
        Left = 171
        Top = 21
        Width = 78
        Height = 21
        TabOrder = 0
        Value = 300.000000000000000000
        MaxNumericCharacters = 9
        MaxValue = 99999.000000000000000000
        Increment = 1.000000000000000000
        Tail = #31186
      end
      object seExitNum: TF8RealSpinEdit
        Left = 171
        Top = 49
        Width = 78
        Height = 21
        TabOrder = 1
        Value = 10.000000000000000000
        MaxNumericCharacters = 9
        MaxValue = 99999.000000000000000000
        Increment = 1.000000000000000000
        Tail = #20154
      end
    end
  end
end
