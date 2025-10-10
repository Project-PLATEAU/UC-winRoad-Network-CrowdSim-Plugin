object FrameCrossWalkSettings: TFrameCrossWalkSettings
  Left = 0
  Top = 0
  Width = 400
  Height = 601
  TabOrder = 0
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 400
    Height = 520
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object PageControlEditMesh: TPageControl
      Left = 0
      Top = 0
      Width = 400
      Height = 457
      ActivePage = TabSheetEditCrosswalk
      Align = alTop
      TabOrder = 0
      object TabSheetEditCrosswalk: TTabSheet
        Caption = #27178#26029#27497#36947#12398#32232#38598
        ImageIndex = 2
        object Panel1: TPanel
          AlignWithMargins = True
          Left = 0
          Top = 115
          Width = 392
          Height = 47
          Margins.Left = 0
          Margins.Top = 6
          Margins.Right = 0
          Margins.Bottom = 6
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object Label3: TLabel
            AlignWithMargins = True
            Left = 190
            Top = 3
            Width = 99
            Height = 21
            Margins.Left = 6
            Margins.Top = 2
            Margins.Right = 0
            Margins.Bottom = 0
            AutoSize = False
            Caption = #36196#20449#21495#20999#26367#38291#38548': '
          end
          object Label4: TLabel
            AlignWithMargins = True
            Left = 0
            Top = 30
            Width = 80
            Height = 21
            Margins.Left = 6
            Margins.Top = 2
            Margins.Right = 0
            Margins.Bottom = 0
            AutoSize = False
            Caption = #24133': '
          end
          object Label1: TLabel
            AlignWithMargins = True
            Left = 0
            Top = 3
            Width = 103
            Height = 21
            Margins.Left = 6
            Margins.Top = 2
            Margins.Right = 0
            Margins.Bottom = 0
            AutoSize = False
            Caption = #38738#20449#21495#20999#26367#38291#38548': '
          end
          object rseSignalInterval: TF8RealSpinEdit
            Left = 97
            Top = 0
            Width = 75
            Height = 21
            TabOrder = 0
            Value = 30.000000000000000000
            MaxNumericCharacters = 9
            MaxValue = 99999.000000000000000000
            Increment = 1.000000000000000000
            Tail = #31186
          end
          object PanelCrosswalkWidth: TPanel
            Left = 25
            Top = 26
            Width = 85
            Height = 21
            BevelOuter = bvNone
            TabOrder = 1
          end
          object seRedSignalInterval: TF8RealSpinEdit
            Left = 289
            Top = 0
            Width = 75
            Height = 21
            TabOrder = 2
            Value = 30.000000000000000000
            MaxNumericCharacters = 9
            MaxValue = 99999.000000000000000000
            Increment = 1.000000000000000000
            Tail = #31186
          end
        end
        object Panel2: TPanel
          Left = 0
          Top = 193
          Width = 392
          Height = 168
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object Label5: TLabel
            Left = 3
            Top = 16
            Width = 86
            Height = 13
            Caption = #27178#26029#27497#36947#12398#19968#35239':'
          end
          object lbCrosswalks: TListBox
            AlignWithMargins = True
            Left = 3
            Top = 37
            Width = 386
            Height = 128
            Margins.Top = 37
            Align = alClient
            ItemHeight = 13
            TabOrder = 0
          end
          object btnlbCrosswalksUpdate: TButton
            Left = 314
            Top = 6
            Width = 75
            Height = 25
            Caption = #26356#26032
            TabOrder = 1
          end
        end
        object Panel18: TPanel
          Left = 0
          Top = 0
          Width = 392
          Height = 20
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 2
          object rbAddCrosswalk: TRadioButton
            Left = 3
            Top = 3
            Width = 169
            Height = 17
            Caption = #27178#26029#27497#36947#12434#26032#12375#12367#36861#21152#12377#12427
            Checked = True
            TabOrder = 0
            TabStop = True
          end
          object rbToCrosswalk: TRadioButton
            Left = 178
            Top = 3
            Width = 214
            Height = 17
            Caption = #29983#25104#28168#12415#12398#32076#36335#12398#35373#23450#12434#22793#26356#12377#12427
            TabOrder = 1
          end
        end
        object PanelNetwork: TPanel
          Left = 0
          Top = 20
          Width = 392
          Height = 35
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 3
          object LabelNetwork: TLabel
            Left = 0
            Top = 15
            Width = 159
            Height = 13
            Caption = #27178#26029#27497#36947#12434#36861#21152#12377#12427#12493#12483#12488#12527#12540#12463': '
          end
          object cbbNetwork: TComboBox
            Left = 165
            Top = 12
            Width = 222
            Height = 21
            Style = csDropDownList
            TabOrder = 0
          end
        end
        object Panel16: TPanel
          Left = 0
          Top = 55
          Width = 392
          Height = 54
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 4
          object GroupBox2: TGroupBox
            AlignWithMargins = True
            Left = 3
            Top = 6
            Width = 384
            Height = 42
            Margins.Top = 6
            Margins.Bottom = 6
            Align = alLeft
            Caption = #12471#12511#12517#12524#12540#12471#12519#12531#38283#22987#26178#12398#20449#21495#35373#23450
            TabOrder = 0
            object Panel13: TPanel
              AlignWithMargins = True
              Left = 2
              Top = 15
              Width = 380
              Height = 17
              Margins.Left = 0
              Margins.Top = 0
              Margins.Right = 0
              Margins.Bottom = 8
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object rbBlueSignal: TRadioButton
                AlignWithMargins = True
                Left = 29
                Top = 0
                Width = 76
                Height = 17
                Margins.Left = 6
                Margins.Top = 0
                Margins.Right = 0
                Margins.Bottom = 0
                Align = alLeft
                Caption = #38738#20449#21495
                Checked = True
                TabOrder = 0
                TabStop = True
              end
              object Panel14: TPanel
                AlignWithMargins = True
                Left = 6
                Top = 0
                Width = 17
                Height = 17
                Margins.Left = 6
                Margins.Top = 0
                Margins.Right = 0
                Margins.Bottom = 0
                Align = alLeft
                Color = 3394867
                ParentBackground = False
                TabOrder = 2
              end
              object Panel15: TPanel
                AlignWithMargins = True
                Left = 111
                Top = 0
                Width = 17
                Height = 17
                Margins.Left = 6
                Margins.Top = 0
                Margins.Right = 0
                Margins.Bottom = 0
                Align = alLeft
                Color = 33777
                ParentBackground = False
                TabOrder = 3
              end
              object rbFloor: TRadioButton
                AlignWithMargins = True
                Left = 222
                Top = 0
                Width = 147
                Height = 17
                Margins.Left = 6
                Margins.Top = 0
                Margins.Right = 0
                Margins.Bottom = 0
                Align = alLeft
                Caption = #36890#24120#12398#27497#34892#32076#36335
                TabOrder = 1
              end
              object rbRedSignal: TRadioButton
                AlignWithMargins = True
                Left = 134
                Top = 0
                Width = 82
                Height = 17
                Margins.Left = 6
                Margins.Top = 0
                Margins.Right = 0
                Margins.Bottom = 0
                Align = alLeft
                Caption = #36196#20449#21495
                TabOrder = 4
              end
            end
          end
        end
        object Panel17: TPanel
          Left = 0
          Top = 168
          Width = 392
          Height = 25
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 5
          object btnSetCrosswalk: TButton
            AlignWithMargins = True
            Left = 3
            Top = 0
            Width = 386
            Height = 25
            Margins.Top = 0
            Margins.Bottom = 0
            Align = alClient
            Caption = #19978#35352#12398#35373#23450#12391#36861#21152'('#22793#26356')'#12377#12427
            TabOrder = 0
          end
        end
        object btnChangeCWName: TButton
          Left = 3
          Top = 364
          Width = 386
          Height = 25
          Caption = #36984#25246#20013#12398#27178#26029#27497#36947#21517#12434#22793#26356#12377#12427
          TabOrder = 6
        end
        object btnDeleteCW: TButton
          Left = 3
          Top = 395
          Width = 386
          Height = 25
          Caption = #36984#25246#20013#12398#27178#26029#27497#36947#12434#21066#38500#12377#12427
          TabOrder = 7
        end
      end
    end
  end
end
