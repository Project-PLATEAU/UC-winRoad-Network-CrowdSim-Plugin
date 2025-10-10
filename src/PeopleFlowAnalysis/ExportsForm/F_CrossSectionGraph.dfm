object FrameCrossSectionGraph: TFrameCrossSectionGraph
  Left = 0
  Top = 0
  Width = 472
  Height = 394
  TabOrder = 0
  object PanelCrossSectionGraph: TPanel
    Left = 0
    Top = 0
    Width = 472
    Height = 362
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ChartCrossSection: TChart
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 464
      Height = 356
      Margins.Right = 5
      Legend.Alignment = laBottom
      Legend.LegendStyle = lsSeries
      Legend.Shadow.Visible = False
      Legend.Symbol.Shadow.Color = clBlack
      Legend.Symbol.Shadow.Transparency = 69
      Legend.TextSymbolGap = 0
      Legend.TopPos = 0
      MarginBottom = 3
      MarginRight = 5
      MarginTop = 12
      Title.Alignment = taLeftJustify
      Title.CustomPosition = True
      Title.Font.Color = clBlack
      Title.Left = 10
      Title.Text.Strings = (
        #26029#38754#20132#36890#27969#35299#26512#32080#26524)
      Title.Top = 15
      BottomAxis.Title.Caption = #32076#36942#26178#38291'('#20998')'
      LeftAxis.Increment = 1.000000000000000000
      LeftAxis.Title.Caption = #36890#36942#20154#25968'('#20154')'
      View3D = False
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
