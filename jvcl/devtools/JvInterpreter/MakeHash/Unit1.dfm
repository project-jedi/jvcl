object Form1: TForm1
  Left = 192
  Top = 114
  Width = 640
  Height = 491
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
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 62
    Height = 13
    Caption = 'Hash Indices'
  end
  object Label2: TLabel
    Left = 320
    Top = 8
    Width = 60
    Height = 13
    Caption = 'Hash Values'
  end
  object Label3: TLabel
    Left = 8
    Top = 232
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Memo1: TMemo
    Left = 8
    Top = 32
    Width = 305
    Height = 145
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 320
    Top = 32
    Width = 305
    Height = 417
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Button1: TButton
    Left = 8
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Run/Stop'
    TabOrder = 2
    OnClick = Button1Click
  end
end
