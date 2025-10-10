object FramePedestrianPopbyInterval: TFramePedestrianPopbyInterval
  Left = 0
  Top = 0
  Width = 386
  Height = 96
  TabOrder = 0
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 386
    Height = 96
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object grpbPopInterval: TGroupBox
      AlignWithMargins = True
      Left = 10
      Top = 3
      Width = 366
      Height = 83
      Margins.Left = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alClient
      Caption = #30330#29983#35215#21063#35443#32048#35373#23450
      TabOrder = 0
      object lblPopInterval: TLabel
        Left = 16
        Top = 24
        Width = 141
        Height = 13
        Caption = #20154#27969#12364#30330#29983#12377#12427#26178#38291#12398#38291#38548#65306
      end
      object lblPopNum: TLabel
        Left = 16
        Top = 52
        Width = 106
        Height = 13
        Caption = #19968#24230#12395#30330#29983#12377#12427#20154#25968#65306
      end
      object sePopInterval: TF8RealSpinEdit
        Left = 163
        Top = 21
        Width = 70
        Height = 21
        TabOrder = 0
        Value = 300.000000000000000000
        MaxNumericCharacters = 9
        MaxValue = 99999.000000000000000000
        Increment = 1.000000000000000000
        Tail = #31186
      end
      object sePopNum: TF8RealSpinEdit
        Left = 163
        Top = 49
        Width = 70
        Height = 21
        Enabled = False
        TabOrder = 1
        Value = 10.000000000000000000
        MaxNumericCharacters = 9
        MaxValue = 10000000.000000000000000000
        Increment = 1.000000000000000000
        Tail = #20154
      end
      object btnDetailSettings: TButton
        Left = 256
        Top = 21
        Width = 89
        Height = 49
        Caption = #35443#32048#35373#23450
        TabOrder = 2
      end
    end
  end
end
