object Script: TScript
  Left = 273
  Top = 177
  Width = 293
  Height = 140
  Caption = 'Script'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object MyLabel1: TMyLabel
    Left = 88
    Top = 56
    Width = 105
    Height = 17
    AutoSize = False
    Caption = 'MyLabel1'
    Color = clBackground
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Button1: TButton
    Left = 48
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Click me'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 168
    Top = 16
    Width = 75
    Height = 25
    Caption = 'And me'
    TabOrder = 1
    OnClick = Button2Click
  end
end
