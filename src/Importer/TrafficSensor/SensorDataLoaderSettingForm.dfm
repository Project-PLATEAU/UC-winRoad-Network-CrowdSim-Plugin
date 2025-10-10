object FormSensorDataLoaderSetting: TFormSensorDataLoaderSetting
  Left = 0
  Top = 0
  Caption = #26029#38754#20132#36890#27969#12487#12540#12479' '#12452#12531#12509#12540#12488#35373#23450
  ClientHeight = 509
  ClientWidth = 366
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PanelFutter: TPanel
    Left = 0
    Top = 473
    Width = 366
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object BevelFutter: TBevel
      Left = 0
      Top = 0
      Width = 366
      Height = 3
      Align = alTop
      ExplicitLeft = 296
      ExplicitTop = -8
      ExplicitWidth = 50
    end
    object ButtonCancel: TButton
      AlignWithMargins = True
      Left = 288
      Top = 6
      Width = 75
      Height = 27
      Align = alRight
      Caption = #12461#12515#12531#12475#12523
      ModalResult = 2
      TabOrder = 0
    end
    object ButtonOK: TButton
      AlignWithMargins = True
      Left = 207
      Top = 6
      Width = 75
      Height = 27
      Align = alRight
      Caption = 'OK'
      TabOrder = 1
      OnClick = ButtonOKClick
    end
  end
  object PageControlImport: TPageControl
    Left = 0
    Top = 0
    Width = 366
    Height = 473
    ActivePage = TabSheetJson
    Align = alClient
    TabOrder = 1
    object TabSheetJson: TTabSheet
      Caption = #12475#12531#12469#12487#12540#12479#65288'JSON'#65289
      object PanelImportJson: TPanel
        Left = 0
        Top = 0
        Width = 358
        Height = 233
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object LabelImportedJson: TLabel
          Left = 8
          Top = 7
          Width = 99
          Height = 13
          Caption = #12475#12531#12469#12487#12540#12479#65306#26410#36984#25246
        end
        object LabelImporteddetailJson: TLabel
          Left = 8
          Top = 74
          Width = 123
          Height = 13
          Caption = #12475#12531#12469#35443#32048#12487#12540#12479#65306#26410#36984#25246
        end
        object LabelID: TLabel
          Left = 8
          Top = 186
          Width = 61
          Height = 13
          Caption = #12456#12531#12486#12451#12486#12451'ID'
        end
        object LabelImportDataIndex: TLabel
          Left = 8
          Top = 213
          Width = 200
          Height = 13
          Caption = #12452#12531#12509#12540#12488#12375#12383#12487#12540#12479#65288'index'#65306#12288#12288#12288#12288#12288#12288#12288#12288#65289
        end
        object ButtonImportJson: TButton
          Left = 8
          Top = 26
          Width = 177
          Height = 25
          Caption = 'JSON'#12501#12449#12452#12523#12434#36984#25246#12375#12390#12452#12531#12509#12540#12488
          TabOrder = 0
          OnClick = ButtonImportJsonClick
        end
        object ButtonImportdetailJson: TButton
          Left = 8
          Top = 93
          Width = 177
          Height = 25
          Caption = 'JSON'#12501#12449#12452#12523#12434#36984#25246#12375#12390#12452#12531#12509#12540#12488
          TabOrder = 1
          OnClick = ButtonImportdetailJsonClick
        end
        object CheckBoxAddedData: TCheckBox
          Left = 8
          Top = 139
          Width = 225
          Height = 17
          Caption = #12452#12531#12509#12540#12488#12375#12383#21508#12487#12540#12479#12434#26082#23384#12487#12540#12479#12395#36861#21152
          TabOrder = 2
          OnClick = CheckBoxAddedDataClick
        end
        object SpinEditIndex: TSpinEdit
          Left = 140
          Top = 210
          Width = 57
          Height = 22
          MaxValue = 99999
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = IDorIndexChange
        end
        object ComboBoxID: TComboBox
          Left = 75
          Top = 183
          Width = 278
          Height = 21
          DropDownCount = 20
          TabOrder = 4
          OnChange = IDorIndexChange
        end
      end
      object PanelJsonDataMemo: TPanel
        Left = 0
        Top = 233
        Width = 358
        Height = 212
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object MemoImportDataJson: TMemo
          Left = 0
          Top = 0
          Width = 358
          Height = 212
          Align = alClient
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
    object TabSheetDat: TTabSheet
      Caption = #20154#27969#12487#12540#12479#65288'DAT'#65289
      ImageIndex = 1
      object PanelImportDat: TPanel
        Left = 0
        Top = 0
        Width = 358
        Height = 193
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object LabelImportedDat: TLabel
          Left = 8
          Top = 7
          Width = 130
          Height = 13
          Caption = #20837#36864#22580#20154#27969#12487#12540#12479#65306#26410#36984#25246
        end
        object LabelImportedGenderOldDat: TLabel
          Left = 8
          Top = 74
          Width = 172
          Height = 13
          Caption = #24615#21029#12539#24180#40802#23652#21029#20154#27969#12487#12540#12479#65306#26410#36984#25246
        end
        object LabelDataRetrievedTime: TLabel
          Left = 8
          Top = 143
          Width = 162
          Height = 13
          Caption = #20154#27969#12487#12540#12479#35336#28204#26178#21051#12288#12288#12288#12288#12288#12288#12288#65306
        end
        object LabelImportDatData: TLabel
          Left = 8
          Top = 174
          Width = 116
          Height = 13
          Caption = #12452#12531#12509#12540#12488#12375#12383#20154#27969#12487#12540#12479
        end
        object ButtonImportDat: TButton
          Left = 8
          Top = 26
          Width = 177
          Height = 25
          Caption = 'DAT'#12501#12449#12452#12523#12434#36984#25246#12375#12390#12452#12531#12509#12540#12488
          TabOrder = 0
          OnClick = ButtonImportDatClick
        end
        object ButtonImportGenderOldDat: TButton
          Left = 8
          Top = 93
          Width = 177
          Height = 25
          Caption = 'DAT'#12501#12449#12452#12523#12434#36984#25246#12375#12390#12452#12531#12509#12540#12488
          TabOrder = 1
          OnClick = ButtonImportGenderOldDatClick
        end
        object SpinEditHour: TSpinEdit
          Left = 114
          Top = 140
          Width = 45
          Height = 22
          MaxValue = 23
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = SpinEditTimeChange
        end
        object SpinEditMinute: TSpinEdit
          Left = 174
          Top = 140
          Width = 45
          Height = 22
          Increment = 15
          MaxValue = 45
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = SpinEditTimeChange
        end
      end
      object PanelDatDataMemo: TPanel
        Left = 0
        Top = 193
        Width = 358
        Height = 252
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object MemoImportDataDat: TMemo
          Left = 0
          Top = 0
          Width = 358
          Height = 252
          Align = alClient
          BevelInner = bvNone
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
  end
  object OpenDialogDat: TOpenDialog
    Left = 312
    Top = 104
  end
  object OpenDialogGenderOldDat: TOpenDialog
    Left = 312
    Top = 144
  end
  object OpenDialogJson: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 312
    Top = 24
  end
  object OpenDialogDetailJson: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 312
    Top = 64
  end
end
