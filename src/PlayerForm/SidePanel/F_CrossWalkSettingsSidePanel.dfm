object FrameCrossWalkSettingsSidePanel: TFrameCrossWalkSettingsSidePanel
  Left = 0
  Top = 0
  Width = 400
  Height = 678
  TabOrder = 0
  inline FrameCrossWalkSettings1: TFrameCrossWalkSettings
    Left = 0
    Top = 0
    Width = 400
    Height = 513
    Align = alTop
    TabOrder = 0
    ExplicitHeight = 513
    inherited PanelMain: TPanel
      inherited PageControlEditMesh: TPageControl
        inherited TabSheetEditCrosswalk: TTabSheet
          OnShow = TabSheetEditCrosswalkShow
          inherited Panel1: TPanel
            inherited rseSignalInterval: TF8RealSpinEdit
              OnChange = seValuesOnChange
            end
          end
          inherited Panel2: TPanel
            inherited lbCrosswalks: TListBox
              OnClick = lbCrosswalksClick
            end
            inherited btnlbCrosswalksUpdate: TButton
              OnClick = btnlbCrosswalksUpdateClick
            end
          end
          inherited Panel18: TPanel
            inherited rbAddCrosswalk: TRadioButton
              OnClick = rbCrosswalkEditTypeClick
            end
            inherited rbToCrosswalk: TRadioButton
              OnClick = rbCrosswalkEditTypeClick
            end
          end
          inherited Panel16: TPanel
            inherited GroupBox2: TGroupBox
              inherited Panel13: TPanel
                inherited rbBlueSignal: TRadioButton
                  OnClick = rbSignalClick
                end
                inherited Panel14: TPanel
                  Color = 13092403
                end
                inherited rbFloor: TRadioButton
                  OnClick = rbSignalClick
                end
                inherited rbRedSignal: TRadioButton
                  OnClick = rbSignalClick
                end
              end
            end
          end
          inherited Panel17: TPanel
            inherited btnSetCrosswalk: TButton
              OnClick = btnSetCrosswalkClick
            end
          end
          inherited btnChangeCWName: TButton
            OnClick = btnChangeCWNameClick
          end
          inherited btnDeleteCW: TButton
            OnClick = btnDeleteCWClick
          end
        end
      end
    end
  end
end
