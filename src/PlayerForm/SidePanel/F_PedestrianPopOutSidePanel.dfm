object FramePedestrianPopOutSidePanel: TFramePedestrianPopOutSidePanel
  Left = 0
  Top = 0
  Width = 400
  Height = 870
  TabOrder = 0
  inline F_PopOutSet: TFramePedestrianPopOutSettings
    Left = 0
    Top = 0
    Width = 400
    Height = 870
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 870
    inherited PanelMain: TPanel
      Height = 569
      ExplicitTop = 301
      ExplicitHeight = 569
    end
    inherited PanelPopOutPoints: TPanel
      inherited btnAdd: TSpeedButton
        OnClick = btnAddClick
      end
      inherited btnDelete: TSpeedButton
        OnClick = btnDeleteClick
      end
      inherited btnAddOutP: TSpeedButton
        OnClick = btnAddOutPClick
      end
      inherited btnDelOutP: TSpeedButton
        OnClick = btnDeleteClick
      end
      inherited ListBoxInOutPoints: TListBox
        OnClick = ListBoxInOutPointsClick
      end
      inherited btnChangePointName: TButton
        OnClick = btnChangePointNameClick
      end
      inherited grpbPointSettingType: TGroupBox
        inherited chbVisibleDispersionPoint: TCheckBox
          OnClick = chbVisibleDispersionPointClick
        end
      end
      inherited lbOutOnly: TListBox
        OnClick = lbOutOnlyClick
      end
    end
    inherited PanelPedestrianSettings: TPanel
      inherited btnPedSetting: TButton
        OnClick = btnPedSettingClick
      end
      inherited btnLabelSetting: TButton
        OnClick = btnLabelSettingClick
      end
    end
    inherited ImageList1: TImageList
      Left = 8
      Top = 384
    end
  end
end
