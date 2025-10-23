object FormPedestrianDetailSettings: TFormPedestrianDetailSettings
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = #20154#27969#12398#35443#32048#35373#23450
  ClientHeight = 486
  ClientWidth = 409
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
  inline F_PedDetail: TFramePedestrianDetailSettings
    Left = 0
    Top = 0
    Width = 409
    Height = 445
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 409
    ExplicitHeight = 445
  end
  object PanelFotter: TPanel
    Left = 0
    Top = 445
    Width = 409
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TButton
      AlignWithMargins = True
      Left = 243
      Top = 6
      Width = 75
      Height = 25
      Margins.Top = 6
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alRight
      Caption = #30906#23450
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 331
      Top = 6
      Width = 75
      Height = 25
      Margins.Top = 6
      Margins.Bottom = 10
      Align = alRight
      Caption = #21462#28040
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
end
