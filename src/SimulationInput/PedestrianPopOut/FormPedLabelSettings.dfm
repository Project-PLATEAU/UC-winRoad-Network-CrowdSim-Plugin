object FormPedestrianLabelSettings: TFormPedestrianLabelSettings
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  Caption = #20154#27969#12521#12505#12523#35373#23450
  ClientHeight = 436
  ClientWidth = 568
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
  inline F_AttrDist: TFrameAttributesDistribution
    Left = 0
    Top = 0
    Width = 568
    Height = 395
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 568
    ExplicitHeight = 395
    inherited PanelFAAMain: TPanel
      Width = 383
      Height = 395
      ExplicitWidth = 383
      ExplicitHeight = 395
      inherited GroupBoxAttributesSetting: TGroupBox
        Width = 377
        Height = 389
        ExplicitWidth = 377
        ExplicitHeight = 389
        inherited ChartAttrDistribution: TChart
          Width = 373
          Height = 342
          ExplicitWidth = 373
          ExplicitHeight = 342
          PrintMargins = (
            15
            -16
            15
            -16)
          ColorPalette = (
            7566335
            33280
            1621242
            16564391
            16777215
            8487297
            16711935
            8553216
            8519680
            130
            65280
            33411
            8585346
            12632256
            16776960
            0
            12770497
            15846312
            15858687
            10854817)
        end
      end
    end
    inherited Panel1: TPanel
      Height = 395
      ExplicitHeight = 395
      inherited lbPedLabel: TListBox
        Height = 322
        ExplicitHeight = 322
      end
      inherited btnChangeLblName: TButton
        Top = 367
        ExplicitTop = 367
      end
    end
    inherited ImageList1: TImageList
      Left = 40
      Top = 232
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 395
    Width = 568
    Height = 41
    Align = alBottom
    TabOrder = 1
    object btnOK: TButton
      AlignWithMargins = True
      Left = 401
      Top = 8
      Width = 75
      Height = 25
      Margins.Top = 7
      Margins.Bottom = 7
      Align = alRight
      Caption = #30906#23450
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 482
      Top = 8
      Width = 75
      Height = 25
      Margins.Top = 7
      Margins.Right = 10
      Margins.Bottom = 7
      Align = alRight
      Caption = #21462#28040
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
end
