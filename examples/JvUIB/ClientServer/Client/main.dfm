object MainForm: TMainForm
  Left = 285
  Top = 157
  Width = 422
  Height = 246
  HorzScrollBar.Range = 342
  VertScrollBar.Range = 191
  ActiveControl = Button1
  AutoScroll = False
  Caption = 'Client Application'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 77
    Top = 181
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 6
    Top = 6
    Width = 75
    Height = 27
    Caption = 'Open Query'
    TabOrder = 0
    OnClick = Button1Click
  end
  object StringGrid: TStringGrid
    Left = 6
    Top = 35
    Width = 336
    Height = 128
    TabOrder = 1
  end
  object Button2: TButton
    Left = 6
    Top = 173
    Width = 59
    Height = 28
    Caption = 'Get Count'
    TabOrder = 2
    OnClick = Button2Click
  end
end
