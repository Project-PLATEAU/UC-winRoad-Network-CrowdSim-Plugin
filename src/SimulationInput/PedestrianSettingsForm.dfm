object FormPedestrianSettings: TFormPedestrianSettings
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  Caption = #27497#34892#32773#35373#23450
  ClientHeight = 486
  ClientWidth = 666
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inline F_PedSetting: TFramePedestrianSettings
    Left = 0
    Top = 0
    Width = 666
    Height = 486
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 647
    ExplicitHeight = 486
    inherited PanelMain: TPanel
      Width = 442
      Height = 445
      ExplicitWidth = 423
      ExplicitHeight = 445
      inherited vbtPedestrianSettings: TVirtualButtonTree
        Width = 422
        Height = 392
        ExplicitWidth = 403
        ExplicitHeight = 392
      end
    end
    inherited PanelFotter: TPanel
      Top = 445
      Width = 666
      ExplicitTop = 445
      ExplicitWidth = 647
      inherited btnOK: TButton
        Left = 500
        OnClick = F_PedSettingbtnOKClick
        ExplicitLeft = 481
      end
      inherited btnCancel: TButton
        Left = 581
        OnClick = F_PedSettingbtnCancelClick
        ExplicitLeft = 562
      end
    end
    inherited Panel1: TPanel
      Left = 442
      Height = 445
      ExplicitLeft = 423
      ExplicitHeight = 445
      inherited lbCategory: TListBox
        Height = 365
        ExplicitHeight = 365
      end
      inherited btnEditCategory: TButton
        Top = 417
        ExplicitTop = 417
      end
    end
  end
end
