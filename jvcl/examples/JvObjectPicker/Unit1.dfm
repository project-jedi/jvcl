object Form1: TForm1
  Left = 257
  Top = 157
  Width = 664
  Height = 543
  Caption = 'Delphi Informant - Object Picker Article Code'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 46
    Width = 656
    Height = 463
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Button2: TButton
    Left = 16
    Top = 12
    Width = 169
    Height = 25
    Caption = 'Run Article Code'
    TabOrder = 1
    OnClick = Button2Click
  end
end
