object JvWindowsTitleMainForm: TJvWindowsTitleMainForm
  Left = 336
  Top = 211
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Lister Demo'
  ClientHeight = 283
  ClientWidth = 368
  Color = clBtnFace
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
  object Button1: TButton
    Left = 124
    Top = 16
    Width = 75
    Height = 25
    Caption = '&Refresh'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 12
    Top = 50
    Width = 341
    Height = 227
    ItemHeight = 13
    TabOrder = 1
  end
end
