object Form1: TForm1
  Left = 261
  Top = 237
  Width = 405
  Height = 243
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvZoom1: TJvZoom
    Left = 10
    Top = 10
    Width = 233
    Height = 189
    Active = False
  end
  object CheckBox1: TCheckBox
    Left = 286
    Top = 12
    Width = 97
    Height = 17
    Caption = 'Active'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object Button1: TButton
    Left = 306
    Top = 42
    Width = 75
    Height = 25
    Caption = '+'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 306
    Top = 76
    Width = 75
    Height = 25
    Caption = '-'
    TabOrder = 3
    OnClick = Button2Click
  end
end
