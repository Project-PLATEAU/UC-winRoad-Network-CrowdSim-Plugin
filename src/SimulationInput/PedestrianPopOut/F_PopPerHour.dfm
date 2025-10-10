object FramePedestrianPopPerHour: TFramePedestrianPopPerHour
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
    object grpbPopPerHour: TGroupBox
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
      object Label1: TLabel
        Left = 16
        Top = 38
        Width = 141
        Height = 13
        Caption = '1'#26178#38291#24403#12383#12426#12395#30330#29983#12377#12427#20154#25968#65306
      end
      object sePopPerHour: TF8RealSpinEdit
        Left = 163
        Top = 35
        Width = 86
        Height = 21
        Enabled = False
        TabOrder = 0
        Value = 600.000000000000000000
        MaxNumericCharacters = 9
        MaxValue = 10000000.000000000000000000
        Increment = 1.000000000000000000
        Tail = #20154'/h'
      end
      object btnDetailSettings: TButton
        Left = 256
        Top = 21
        Width = 89
        Height = 49
        Caption = #35443#32048#35373#23450
        TabOrder = 1
      end
    end
  end
end
