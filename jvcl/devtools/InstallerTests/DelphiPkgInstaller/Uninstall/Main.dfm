object FormMain: TFormMain
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Binary Component Uninstall'
  ClientHeight = 81
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LblStatus: TLabel
    Left = 8
    Top = 8
    Width = 71
    Height = 13
    Caption = 'Uninstall of %s.'
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 24
    Width = 361
    Height = 17
    TabOrder = 0
    Visible = False
  end
  object BtnUninstall: TButton
    Left = 208
    Top = 48
    Width = 75
    Height = 25
    Caption = '&Uninstall'
    TabOrder = 1
    OnClick = BtnUninstallClick
  end
  object BtnCancel: TButton
    Left = 296
    Top = 48
    Width = 75
    Height = 25
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = BtnCancelClick
  end
end
