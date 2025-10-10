object FrameWaitingQueueSidePanel: TFrameWaitingQueueSidePanel
  Left = 0
  Top = 0
  Width = 400
  Height = 680
  TabOrder = 0
  inline F_WaitAreaSet: TFrameWaitingAreaSettings
    Left = 0
    Top = 0
    Width = 400
    Height = 641
    Align = alTop
    TabOrder = 0
    ExplicitHeight = 641
    inherited PanelMain: TPanel
      Height = 150
      ExplicitHeight = 142
      inherited chbVisibleWaitingArea: TCheckBox
        OnClick = chbVisibleWaitingAreaClick
      end
      inherited grpbWaitingAreaSettings: TGroupBox
        Height = 90
        ExplicitHeight = 82
        inherited btnSetWaitingArea: TButton
          OnClick = btnSetWaitingAreaClick
        end
      end
    end
    inherited PanelWaitingQueueSettings: TPanel
      Top = 150
      ExplicitTop = 150
      inherited grpbWaitingQueueSettings: TGroupBox
        ExplicitHeight = 478
        inherited PanelSettings: TPanel
          inherited btnBusTerminalLinkFree: TSpeedButton
            Left = 327
            OnClick = btnBusTerminalLinkFreeClick
            ExplicitLeft = 327
          end
          inherited cbbSelectWaitingArea: TComboBox
            OnChange = cbbSelectWaitingAreaChange
          end
          inherited btnDeleteWaitingArea: TButton
            OnClick = btnDeleteWaitingAreaClick
          end
          inherited btnSelectedView: TButton
            OnClick = btnSelectedViewClick
          end
          inherited edtBusTerminal: TEdit
            Width = 137
            ExplicitWidth = 137
          end
          inherited btnMovetoBusTerminal: TButton
            Left = 283
            OnClick = btnMovetoBusTerminalClick
            ExplicitLeft = 283
          end
          inherited btnSelectBusTerminal: TButton
            Left = 239
            OnClick = btnSelectBusTerminalClick
            ExplicitLeft = 239
          end
          inherited edtWQNode: TEdit
            Width = 137
            ExplicitWidth = 137
          end
          inherited btnWQNodeChange: TButton
            Left = 239
            OnClick = btnWQNodeChangeClick
            ExplicitLeft = 239
          end
          inherited btnMovetoWQNode: TButton
            Left = 283
            OnClick = btnMovetoWQNodeClick
            ExplicitLeft = 283
          end
        end
        inherited PanelIntervalSettings: TPanel
          inherited grpbExitInterval: TGroupBox
            inherited cbbExitIntervalRule: TComboBox
              OnChange = cbbExitIntervalRuleChange
            end
            inherited PanelFrameExitInterval: TPanel
              inherited FrameExitInterval1: TFrameExitInterval
                inherited PanelMain: TPanel
                  inherited grpbExitInterval: TGroupBox
                    inherited seExitInterval: TF8RealSpinEdit
                      OnChange = seExitIntervalChange
                    end
                    inherited seExitNum: TF8RealSpinEdit
                      OnChange = seExitNumChange
                    end
                  end
                end
              end
            end
          end
        end
        inherited Panel1: TPanel
          inherited btnLinkWQandArea: TSpeedButton
            OnClick = btnLinkWQandAreaClick
          end
          inherited btnLinkFree: TSpeedButton
            OnClick = btnLinkFreeClick
          end
          inherited btnCreateWaitingQueue: TButton
            Left = 9
            OnClick = btnCreateWaitingQueueClick
            ExplicitLeft = 9
          end
        end
      end
    end
    inherited ImageList1: TImageList
      Left = 264
    end
    inherited ImageList2: TImageList
      Left = 324
      Top = 2
    end
  end
end
