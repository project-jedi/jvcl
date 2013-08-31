object Form1: TForm1
  Left = 196
  Top = 113
  Width = 386
  Height = 142
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 185
    Height = 25
    Caption = 'Clear Tracer Log'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 216
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 1
    OnClick = Button2Click
  end
  object cbTimer1: TCheckBox
    Left = 16
    Top = 64
    Width = 65
    Height = 17
    Caption = 'cbTimer1'
    TabOrder = 2
    OnClick = cbTimer1Click
  end
end
