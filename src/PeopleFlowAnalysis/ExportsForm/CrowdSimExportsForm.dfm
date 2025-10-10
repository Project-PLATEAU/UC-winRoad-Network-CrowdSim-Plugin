object FormCrowdSimExports: TFormCrowdSimExports
  Left = 0
  Top = 0
  Caption = #20986#21147#12487#12540#12479#36984#25246
  ClientHeight = 458
  ClientWidth = 608
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControlExports: TPageControl
    Left = 0
    Top = 0
    Width = 608
    Height = 458
    ActivePage = TabSheetODTrip
    Align = alClient
    TabOrder = 0
    object TabSheetCrossSectionGraph: TTabSheet
      Caption = #26029#38754#20132#36890#27969#35299#26512#32080#26524
    end
    object TabSheetHeatMap: TTabSheet
      Caption = #12498#12540#12488#12510#12483#12503#29983#25104
      ImageIndex = 1
    end
    object TabSheetBusTransportation: TTabSheet
      Caption = #12496#12473#20055#38477#32080#26524
      ImageIndex = 2
    end
    object TabSheetWaitingQueue: TTabSheet
      Caption = #24453#27231#21015#24418#25104#32080#26524
      ImageIndex = 3
    end
    object TabSheetODTrip: TTabSheet
      Caption = 'OD'#36890#36942#12525#12464
      ImageIndex = 4
    end
  end
end
