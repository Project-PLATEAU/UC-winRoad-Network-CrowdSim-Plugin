object FramePedestrianOutbyInterval: TFramePedestrianOutbyInterval
  Left = 0
  Top = 0
  Width = 491
  Height = 102
  TabOrder = 0
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 491
    Height = 102
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object grpbOutInterval: TGroupBox
      AlignWithMargins = True
      Left = 10
      Top = 3
      Width = 471
      Height = 89
      Margins.Left = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alClient
      Caption = #36864#20986#35215#21063#35443#32048#35373#23450
      TabOrder = 0
      object lblOutInterval: TLabel
        Left = 16
        Top = 24
        Width = 141
        Height = 13
        Caption = #20154#27969#12364#36864#20986#12377#12427#26178#38291#12398#38291#38548#65306
      end
      object lblOutNum: TLabel
        Left = 16
        Top = 52
        Width = 139
        Height = 13
        Caption = #19968#24230#12395#36864#20986#12391#12365#12427#26368#22823#20154#25968#65306
      end
      object seOutInterval: TF8RealSpinEdit
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
      object seOutNum: TF8RealSpinEdit
        Left = 163
        Top = 49
        Width = 70
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
