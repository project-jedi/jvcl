object frTransparentForm: TfrTransparentForm
  Left = 490
  Top = 248
  Width = 340
  Height = 252
  Caption = 'Transparent Form'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 88
    Top = 40
    Width = 115
    Height = 13
    Caption = 'TRANSPARENT FORM'
  end
  object Button1: TButton
    Left = 88
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 0
    OnClick = Button1Click
  end
  object JvTransparentForm1: TJvTransparentForm
    Active = True
    AutoSize = False
    Left = 184
    Top = 72
  end
end
