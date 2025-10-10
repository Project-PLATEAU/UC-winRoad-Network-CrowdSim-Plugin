object FormNameChange: TFormNameChange
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  Caption = #20154#27969#12521#12505#12523#21517#12398#22793#26356
  ClientHeight = 96
  ClientWidth = 300
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 300
    Height = 96
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lblNewName: TLabel
      Left = 16
      Top = 16
      Width = 92
      Height = 13
      Caption = #22793#26356#24460#12398#12521#12505#12523#21517#65306
    end
    object edtNewName: TEdit
      Left = 114
      Top = 13
      Width = 167
      Height = 21
      TabOrder = 0
    end
    object btnOK: TButton
      Left = 56
      Top = 56
      Width = 75
      Height = 25
      Caption = #30906#23450
      TabOrder = 1
    end
    object btnCancel: TButton
      Left = 168
      Top = 56
      Width = 75
      Height = 25
      Caption = #21462#28040
      TabOrder = 2
    end
  end
end
