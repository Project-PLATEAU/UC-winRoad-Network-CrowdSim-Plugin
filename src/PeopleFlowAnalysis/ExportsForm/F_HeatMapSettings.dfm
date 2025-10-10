object FrameHeatMapSettings: TFrameHeatMapSettings
  Left = 0
  Top = 0
  Width = 555
  Height = 339
  TabOrder = 0
  object PanelSimSpan: TPanel
    Left = 0
    Top = 0
    Width = 555
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LabelSimSpan: TLabel
      Left = 8
      Top = 12
      Width = 196
      Height = 13
      Caption = #12498#12540#12488#12510#12483#12503#12434#29983#25104#12377#12427#12471#12511#12517#12524#12540#12471#12519#12531#21306#38291
    end
    object LabelSpanBetween: TLabel
      Left = 324
      Top = 12
      Width = 8
      Height = 13
      Caption = '~'
    end
    object SEStartTime: TF8RealSpinEdit
      Left = 210
      Top = 9
      Width = 108
      Height = 21
      TabOrder = 0
      MaxNumericCharacters = 9
      MaxValue = 99999.000000000000000000
      Increment = 1.000000000000000000
      Tail = #20998#26178#28857
    end
    object SEEndTime: TF8RealSpinEdit
      Left = 338
      Top = 9
      Width = 108
      Height = 21
      TabOrder = 1
      Value = 5.000000000000000000
      MaxNumericCharacters = 9
      MaxValue = 99999.000000000000000000
      MinValue = 1.000000000000000000
      Increment = 1.000000000000000000
      Tail = #20998#26178#28857
    end
  end
  object PanelHeatMap: TPanel
    Left = 0
    Top = 33
    Width = 555
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object ButtonGenerateHeatMap: TButton
      AlignWithMargins = True
      Left = 30
      Top = 7
      Width = 495
      Height = 27
      Margins.Left = 30
      Margins.Top = 7
      Margins.Right = 30
      Margins.Bottom = 7
      Align = alClient
      Caption = #12498#12540#12488#12510#12483#12503#12434#29983#25104#12377#12427
      TabOrder = 0
      OnClick = ButtonGenerateHeatMapClick
    end
  end
end
