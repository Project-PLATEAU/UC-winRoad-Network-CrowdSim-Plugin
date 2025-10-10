object FormPedestrianCategoryEditor: TFormPedestrianCategoryEditor
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  Caption = #27497#34892#32773#21205#20316#35373#23450
  ClientHeight = 444
  ClientWidth = 292
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
  inline F_PedCateEditor: TFramePedestrianCategoryEditor
    Left = 0
    Top = 0
    Width = 292
    Height = 444
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 511
    inherited PanelMain: TPanel
      Width = 292
      Height = 362
      ExplicitWidth = 511
      inherited pgcCategory: TPageControl
        Width = 292
        Height = 362
        ExplicitWidth = 511
        inherited TabAdjustRate: TTabSheet
          ExplicitWidth = 284
          ExplicitHeight = 334
        end
        inherited TabCollision: TTabSheet
          ExplicitWidth = 284
          ExplicitHeight = 334
        end
        inherited TabSpeed: TTabSheet
          ExplicitWidth = 284
          ExplicitHeight = 334
        end
        inherited TabPassTime: TTabSheet
          ExplicitWidth = 284
          ExplicitHeight = 334
        end
      end
    end
    inherited PanelFotter: TPanel
      Top = 403
      Width = 292
      ExplicitWidth = 511
      inherited btnOK: TButton
        Left = 125
        ExplicitLeft = 344
      end
      inherited btnCancel: TButton
        Left = 206
        ExplicitLeft = 425
      end
    end
    inherited PanelCategoryList: TPanel
      Width = 292
      ExplicitWidth = 511
    end
  end
end
