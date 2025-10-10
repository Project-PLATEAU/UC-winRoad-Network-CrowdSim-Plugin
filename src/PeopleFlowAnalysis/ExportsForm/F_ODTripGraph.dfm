object FrameODTripGraph: TFrameODTripGraph
  Left = 0
  Top = 0
  Width = 472
  Height = 394
  TabOrder = 0
  object PanelODTripGraph: TPanel
    Left = 0
    Top = 0
    Width = 472
    Height = 362
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ChartODTripGraph: TChart
      Left = 0
      Top = 0
      Width = 472
      Height = 362
      AllowPanning = pmNone
      Legend.CheckBoxes = True
      Legend.LegendStyle = lsLastValues
      Legend.TextStyle = ltsPlain
      Title.Font.Color = clBlack
      Title.Text.Strings = (
        'OD'#36890#36942#32080#26524)
      BottomAxis.Automatic = False
      BottomAxis.AutomaticMaximum = False
      BottomAxis.AutomaticMinimum = False
      LeftAxis.Automatic = False
      LeftAxis.AutomaticMaximum = False
      LeftAxis.AutomaticMinimum = False
      Panning.MouseWheel = pmwNone
      RightAxis.Automatic = False
      RightAxis.AutomaticMaximum = False
      RightAxis.AutomaticMinimum = False
      RightAxis.Visible = False
      TopAxis.Automatic = False
      TopAxis.AutomaticMaximum = False
      TopAxis.AutomaticMinimum = False
      TopAxis.Visible = False
      View3D = False
      Zoom.Allow = False
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      DefaultCanvas = 'TGDIPlusCanvas'
      ColorPaletteIndex = 13
    end
  end
  object PanelExportButton: TPanel
    Left = 0
    Top = 362
    Width = 472
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object ButtonExportGraphImage: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 466
      Height = 26
      Align = alClient
      Caption = #35299#26512#32080#26524#12434#30011#20687#12392#12375#12390#20445#23384#12377#12427
      TabOrder = 0
      OnClick = ButtonExportGraphImageClick
    end
  end
  object SaveDialogGraphImage: TSaveDialog
    DefaultExt = 'png'
    Filter = 'png'#12501#12449#12452#12523'|*.png|jpeg'#12501#12449#12452#12523'|*.jpeg|bmp'#12501#12449#12452#12523'|*.bmp'
    Left = 399
    Top = 11
  end
end
