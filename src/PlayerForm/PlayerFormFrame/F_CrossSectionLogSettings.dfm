object FrameCrossSectionLogSettings: TFrameCrossSectionLogSettings
  Left = 0
  Top = 0
  Width = 586
  Height = 559
  TabOrder = 0
  object PageControlCSLogSettings: TPageControl
    Left = 0
    Top = 0
    Width = 586
    Height = 559
    ActivePage = TabSheetCSLogSettings
    Align = alClient
    TabOrder = 0
    OnChange = PageControlCSLogSettingsChange
    ExplicitWidth = 615
    object TabSheetCSLogSettings: TTabSheet
      Caption = #12525#12464#20986#21147#35373#23450
      TabVisible = False
      object PanelDataInterval: TPanel
        Left = 0
        Top = 0
        Width = 578
        Height = 65
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 655
        object grpbLogSettings: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 572
          Height = 59
          Align = alClient
          Caption = #12525#12464#20986#21147#35373#23450
          TabOrder = 0
          ExplicitLeft = 152
          ExplicitTop = 16
          ExplicitWidth = 185
          ExplicitHeight = 105
          object chbCrossSectionLog: TCheckBox
            Left = 12
            Top = 26
            Width = 125
            Height = 17
            Caption = #26029#38754#20132#36890#27969
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = chbLogsClick
          end
          object chbBusTransportLog: TCheckBox
            Left = 133
            Top = 26
            Width = 125
            Height = 17
            Caption = #12496#12473#20055#38477#32080#26524
            Checked = True
            State = cbChecked
            TabOrder = 1
            OnClick = chbLogsClick
          end
          object chbWaitingQueueLog: TCheckBox
            Left = 264
            Top = 26
            Width = 125
            Height = 17
            Caption = #24453#27231#21015#24418#25104#32080#26524
            Checked = True
            State = cbChecked
            TabOrder = 2
            OnClick = chbLogsClick
          end
          object chbPedestrianODLog: TCheckBox
            Left = 412
            Top = 26
            Width = 150
            Height = 17
            Caption = #27497#34892#32773#31227#21205#26178#38291#65288#32076#36335#21029#65289
            Checked = True
            State = cbChecked
            TabOrder = 3
            OnClick = chbLogsClick
          end
        end
      end
      object PanelCSLog: TPanel
        Left = 0
        Top = 65
        Width = 578
        Height = 484
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitTop = 72
        ExplicitWidth = 607
        ExplicitHeight = 477
        object GroupBoxSensorArea: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 572
          Height = 478
          Align = alClient
          Caption = #26029#38754#20132#36890#27969#35373#23450
          TabOrder = 0
          ExplicitWidth = 601
          ExplicitHeight = 471
          object PanelSensorAreaInfo: TPanel
            Left = 329
            Top = 15
            Width = 241
            Height = 461
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            ExplicitWidth = 270
            ExplicitHeight = 454
            object MemoSensorAreaInfos: TMemo
              AlignWithMargins = True
              Left = 20
              Top = 15
              Width = 201
              Height = 426
              Margins.Left = 20
              Margins.Top = 15
              Margins.Right = 20
              Margins.Bottom = 20
              Align = alClient
              ReadOnly = True
              ScrollBars = ssVertical
              TabOrder = 0
              ExplicitWidth = 230
              ExplicitHeight = 419
            end
          end
          object PanelSensorAreaSettings: TPanel
            Left = 2
            Top = 15
            Width = 327
            Height = 461
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 1
            ExplicitLeft = -1
            ExplicitTop = 9
            ExplicitHeight = 454
            object GroupBoxSensorAreaSettings: TGroupBox
              AlignWithMargins = True
              Left = 10
              Top = 15
              Width = 307
              Height = 187
              Margins.Left = 10
              Margins.Top = 10
              Margins.Right = 10
              Margins.Bottom = 10
              Caption = #35336#28204#31684#22258#25351#23450
              TabOrder = 0
              object LabelAreaName: TLabel
                Left = 12
                Top = 104
                Width = 60
                Height = 13
                Caption = #35336#28204#31684#22258#21517
              end
              object EditSensorAreaName: TEdit
                Left = 10
                Top = 123
                Width = 284
                Height = 21
                TabOrder = 0
              end
              object ButtonSetLeftTop: TButton
                Left = 20
                Top = 27
                Width = 126
                Height = 25
                Caption = #35336#28204#31684#22258#24038#19978#12434#36984#25246
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clTeal
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
                TabOrder = 1
                OnClick = ButtonSetLeftTopClick
              end
              object ButtonSetRightBottom: TButton
                Left = 20
                Top = 65
                Width = 126
                Height = 25
                Caption = #35336#28204#31684#22258#21491#19979#12434#36984#25246
                TabOrder = 2
                OnClick = ButtonSetRightBottomClick
              end
              object ButtonAddSensorAreas: TButton
                Left = 10
                Top = 150
                Width = 284
                Height = 25
                Caption = #35336#28204#31684#22258#12434#36861#21152
                TabOrder = 3
                OnClick = ButtonAddSensorAreasClick
              end
            end
            object GroupBoxSelectSensorArea: TGroupBox
              Left = 10
              Top = 215
              Width = 307
              Height = 107
              Caption = #35336#28204#31684#22258#30906#35469
              TabOrder = 1
              object LabelAreaDrawSelect: TLabel
                Left = 12
                Top = 24
                Width = 48
                Height = 13
                Caption = #35336#28204#31684#22258
              end
              object ComboBoxDrawAreaName: TComboBox
                Left = 10
                Top = 43
                Width = 284
                Height = 21
                Style = csDropDownList
                TabOrder = 0
                OnChange = ComboBoxDrawAreaNameChange
              end
              object ButtonDeleteSensorArea: TButton
                Left = 10
                Top = 70
                Width = 284
                Height = 25
                Caption = #36984#25246#20013#12398#35336#28204#31684#22258#12434#21066#38500
                TabOrder = 1
                OnClick = ButtonDeleteSensorAreaClick
              end
            end
          end
        end
      end
    end
  end
end
