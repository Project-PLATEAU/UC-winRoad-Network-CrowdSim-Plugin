object FramePedestrianCategoryEditor: TFramePedestrianCategoryEditor
  Left = 0
  Top = 0
  Width = 270
  Height = 435
  TabOrder = 0
  object PanelMain: TPanel
    Left = 0
    Top = 41
    Width = 270
    Height = 353
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pgcCategory: TPageControl
      Left = 0
      Top = 0
      Width = 270
      Height = 353
      ActivePage = TabAdjustRate
      Align = alClient
      TabOrder = 0
      object TabAdjustRate: TTabSheet
        Caption = #32076#36335#36984#25246#20670#21521
        object Label9: TLabel
          Left = 11
          Top = 11
          Width = 50
          Height = 13
          Caption = #19978#12426#38542#27573#65306
        end
        object Label10: TLabel
          Left = 11
          Top = 43
          Width = 50
          Height = 13
          Caption = #19979#12426#38542#27573#65306
        end
        object Label11: TLabel
          Left = 11
          Top = 75
          Width = 90
          Height = 13
          Caption = #19978#12426#12456#12473#12459#12524#12540#12479#12540#65306
        end
        object Label12: TLabel
          Left = 11
          Top = 107
          Width = 126
          Height = 13
          Caption = #19978#12426#12456#12473#12459#12524#12540#12479#12540#65288#27497#34892#65289#65306
        end
        object Label13: TLabel
          Left = 11
          Top = 139
          Width = 90
          Height = 13
          Caption = #19979#12426#12456#12473#12459#12524#12540#12479#12540#65306
        end
        object Label14: TLabel
          Left = 11
          Top = 171
          Width = 126
          Height = 13
          Caption = #19979#12426#12456#12473#12459#12524#12540#12479#12540#65288#27497#34892#65289#65306
        end
        object Label15: TLabel
          Left = 11
          Top = 203
          Width = 82
          Height = 13
          Caption = #19978#12426#12456#12524#12505#12540#12479#12540#65306
        end
        object Label16: TLabel
          Left = 11
          Top = 235
          Width = 82
          Height = 13
          Caption = #19979#12426#12456#12524#12505#12540#12479#12540#65306
        end
        object Label17: TLabel
          Left = 11
          Top = 267
          Width = 102
          Height = 13
          Caption = #27178#26029#27497#36947#65288#38738#20449#21495#65289#65306
        end
        object Label18: TLabel
          Left = 11
          Top = 299
          Width = 102
          Height = 13
          Caption = #27178#26029#27497#36947#65288#36196#20449#21495#65289#65306
        end
        object seEscaUpWalk: TF8RealSpinEdit
          Left = 143
          Top = 104
          Width = 85
          Height = 21
          TabOrder = 0
          OnChange = seEscaUpWalkChange
          Value = 100.000000000000000000
          MaxNumericCharacters = 9
          MaxValue = 100.000000000000000000
          Increment = 1.000000000000000000
          Tail = ' %'
        end
        object seEscaDownWalk: TF8RealSpinEdit
          Left = 143
          Top = 168
          Width = 85
          Height = 21
          TabOrder = 1
          OnChange = seEscaDownWalkChange
          Value = 100.000000000000000000
          MaxNumericCharacters = 9
          MaxValue = 100.000000000000000000
          Increment = 1.000000000000000000
          Tail = ' %'
        end
        object seEscaDown: TF8RealSpinEdit
          Left = 143
          Top = 136
          Width = 85
          Height = 21
          TabOrder = 2
          OnChange = seEscaDownChange
          Value = 100.000000000000000000
          MaxNumericCharacters = 9
          MaxValue = 100.000000000000000000
          Increment = 1.000000000000000000
          Tail = ' %'
        end
        object seEscaUp: TF8RealSpinEdit
          Left = 143
          Top = 72
          Width = 85
          Height = 21
          TabOrder = 3
          OnChange = seEscaUpChange
          Value = 100.000000000000000000
          MaxNumericCharacters = 9
          MaxValue = 100.000000000000000000
          Increment = 1.000000000000000000
          Tail = ' %'
        end
        object seStairDown: TF8RealSpinEdit
          Left = 143
          Top = 40
          Width = 85
          Height = 21
          TabOrder = 4
          OnChange = seStairDownChange
          Value = 100.000000000000000000
          MaxNumericCharacters = 9
          MaxValue = 100.000000000000000000
          Increment = 1.000000000000000000
          Tail = ' %'
        end
        object seStairUp: TF8RealSpinEdit
          Left = 143
          Top = 8
          Width = 85
          Height = 21
          TabOrder = 5
          OnChange = seStairUpChange
          Value = 100.000000000000000000
          MaxNumericCharacters = 9
          MaxValue = 100.000000000000000000
          Increment = 1.000000000000000000
          Tail = ' %'
        end
        object seElevUp: TF8RealSpinEdit
          Left = 143
          Top = 200
          Width = 85
          Height = 21
          TabOrder = 6
          OnChange = seElevUpChange
          Value = 100.000000000000000000
          MaxNumericCharacters = 9
          MaxValue = 100.000000000000000000
          Increment = 1.000000000000000000
          Tail = ' %'
        end
        object seElevDown: TF8RealSpinEdit
          Left = 143
          Top = 232
          Width = 85
          Height = 21
          TabOrder = 7
          OnChange = seElevDownChange
          Value = 100.000000000000000000
          MaxNumericCharacters = 9
          MaxValue = 100.000000000000000000
          Increment = 1.000000000000000000
          Tail = ' %'
        end
        object seCrosswalkRed: TF8RealSpinEdit
          Left = 143
          Top = 296
          Width = 85
          Height = 21
          TabOrder = 8
          OnChange = seCrosswalkRedChange
          Value = 100.000000000000000000
          MaxNumericCharacters = 9
          MaxValue = 100.000000000000000000
          Increment = 1.000000000000000000
          Tail = ' %'
        end
        object seCrosswalkBlue: TF8RealSpinEdit
          Left = 143
          Top = 264
          Width = 85
          Height = 21
          TabOrder = 9
          OnChange = seCrosswalkBlueChange
          Value = 100.000000000000000000
          MaxNumericCharacters = 9
          MaxValue = 100.000000000000000000
          Increment = 1.000000000000000000
          Tail = ' %'
        end
      end
      object TabCollision: TTabSheet
        Caption = #34909#31361#21322#24452
        ImageIndex = 1
        object Label5: TLabel
          Left = 11
          Top = 11
          Width = 42
          Height = 13
          Caption = #20572#27490#26178#65306
        end
        object Label6: TLabel
          Left = 11
          Top = 43
          Width = 42
          Height = 13
          Caption = #27497#34892#26178#65306
        end
        object Label7: TLabel
          Left = 11
          Top = 75
          Width = 90
          Height = 13
          Caption = #24453#27231#21015#20869#27497#34892#26178#65306
        end
        object Label8: TLabel
          Left = 11
          Top = 107
          Width = 85
          Height = 13
          Caption = #12496#12473#25161#31561#36890#36942#26178#65306
        end
        object pnlDoorOrGateCollision: TPanel
          Left = 107
          Top = 101
          Width = 97
          Height = 25
          BevelOuter = bvNone
          TabOrder = 0
        end
        object pnlWQWalkCollision: TPanel
          Left = 107
          Top = 69
          Width = 97
          Height = 25
          BevelOuter = bvNone
          TabOrder = 1
        end
        object pnlStopCollision: TPanel
          Left = 107
          Top = 5
          Width = 97
          Height = 25
          BevelOuter = bvNone
          TabOrder = 2
        end
        object pnlWalkCollision: TPanel
          Left = 107
          Top = 37
          Width = 97
          Height = 25
          BevelOuter = bvNone
          TabOrder = 3
        end
      end
      object TabSpeed: TTabSheet
        Caption = #27497#34892#36895#24230
        ImageIndex = 2
        object Label1: TLabel
          Left = 11
          Top = 11
          Width = 30
          Height = 13
          Caption = #36890#24120#65306
        end
        object Label2: TLabel
          Left = 11
          Top = 43
          Width = 50
          Height = 13
          Caption = #19978#12426#38542#27573#65306
        end
        object Label3: TLabel
          Left = 11
          Top = 75
          Width = 50
          Height = 13
          Caption = #19979#12426#38542#27573#65306
        end
        object Label4: TLabel
          Left = 11
          Top = 107
          Width = 106
          Height = 13
          Caption = #12456#12473#12459#12524#12540#12479#12540#27497#34892#26178#65306
        end
        object Label19: TLabel
          Left = 11
          Top = 139
          Width = 102
          Height = 13
          Caption = #27178#26029#27497#36947#65288#38738#20449#21495#65289#65306
        end
        object Label20: TLabel
          Left = 11
          Top = 171
          Width = 102
          Height = 13
          Caption = #27178#26029#27497#36947#65288#36196#20449#21495#65289#65306
        end
        object pnlNormalSpeed: TPanel
          Left = 128
          Top = 5
          Width = 97
          Height = 25
          BevelOuter = bvNone
          TabOrder = 0
        end
        object pnlStairUpSpeed: TPanel
          Left = 128
          Top = 37
          Width = 97
          Height = 25
          BevelOuter = bvNone
          TabOrder = 1
        end
        object pnlStairDownSpeed: TPanel
          Left = 128
          Top = 69
          Width = 97
          Height = 25
          BevelOuter = bvNone
          TabOrder = 2
        end
        object pnlEscaWalkSpeed: TPanel
          Left = 128
          Top = 101
          Width = 97
          Height = 25
          BevelOuter = bvNone
          TabOrder = 3
        end
        object pnlCWBlueSpeed: TPanel
          Left = 128
          Top = 133
          Width = 97
          Height = 25
          BevelOuter = bvNone
          TabOrder = 4
        end
        object pnlCWRedSpeed: TPanel
          Left = 128
          Top = 165
          Width = 97
          Height = 25
          BevelOuter = bvNone
          TabOrder = 5
        end
      end
      object TabPassTime: TTabSheet
        Caption = #36890#36942#26178#38291
        ImageIndex = 3
        object LabelTakeBusTime: TLabel
          Left = 11
          Top = 11
          Width = 97
          Height = 13
          Caption = #12496#12473#20055#38477#25152#35201#26178#38291#65306
        end
        object LabelPassGateTime: TLabel
          Left = 11
          Top = 48
          Width = 102
          Height = 13
          Caption = #25913#26413#36890#36942#25152#35201#26178#38291#65306
        end
        object pnlTakeBusTime: TPanel
          Left = 119
          Top = 5
          Width = 97
          Height = 25
          BevelOuter = bvNone
          TabOrder = 0
        end
        object pnlPassGateTime: TPanel
          Left = 119
          Top = 42
          Width = 97
          Height = 25
          BevelOuter = bvNone
          TabOrder = 1
        end
      end
    end
  end
  object PanelFotter: TPanel
    Left = 0
    Top = 394
    Width = 270
    Height = 41
    Align = alBottom
    TabOrder = 1
    object btnOK: TButton
      AlignWithMargins = True
      Left = 103
      Top = 8
      Width = 75
      Height = 25
      Margins.Top = 7
      Margins.Bottom = 7
      Align = alRight
      Caption = #30906#23450
      TabOrder = 0
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 184
      Top = 8
      Width = 75
      Height = 25
      Margins.Top = 7
      Margins.Right = 10
      Margins.Bottom = 7
      Align = alRight
      Caption = #21462#28040
      TabOrder = 1
    end
  end
  object PanelCategoryList: TPanel
    Left = 0
    Top = 0
    Width = 270
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object LabelCategory: TLabel
      Left = 10
      Top = 16
      Width = 66
      Height = 13
      Caption = #21205#20316#35373#23450#21517#65306
    end
    object edtCateName: TEdit
      Left = 82
      Top = 13
      Width = 177
      Height = 21
      TabOrder = 0
      OnChange = edtCateNameChange
    end
  end
end
