object FormSchedule: TFormSchedule
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  Caption = #12480#12452#12516#35373#23450
  ClientHeight = 352
  ClientWidth = 588
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
  inline F_ScheSet: TFrameScheduleSettings
    Left = 0
    Top = 0
    Width = 588
    Height = 352
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 804
    ExplicitHeight = 352
    inherited PanelSceduleIO: TPanel
      Width = 588
      ExplicitWidth = 804
    end
    inherited PanelScheduleData: TPanel
      Width = 259
      Height = 230
      ExplicitWidth = 475
      ExplicitHeight = 230
      inherited sgSchedule: TStringGrid
        Width = 239
        Height = 217
        ExplicitWidth = 455
        ExplicitHeight = 217
      end
    end
    inherited PanelFotter: TPanel
      Top = 311
      Width = 588
      ExplicitTop = 311
      ExplicitWidth = 804
      inherited btnOK: TButton
        Left = 415
        OnClick = btnOKClick
        ExplicitLeft = 631
      end
      inherited btnCancel: TButton
        Left = 503
        OnClick = btnCancelClick
        ExplicitLeft = 719
      end
    end
    inherited PanelPopLblNums: TPanel
      Left = 259
      Height = 230
      ExplicitLeft = 475
      ExplicitHeight = 230
    end
  end
end
