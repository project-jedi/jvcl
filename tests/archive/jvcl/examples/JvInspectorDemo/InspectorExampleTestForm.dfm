object frmTest: TfrmTest
  Left = 556
  Top = 215
  AutoScroll = False
  Caption = 'Test Form'
  ClientHeight = 278
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PanelForLabel: TPanel
    Left = 0
    Top = 0
    Width = 402
    Height = 141
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblTest: TLabel
      Left = 4
      Top = 4
      Width = 21
      Height = 13
      Caption = 'Test'
    end
  end
  object ListBox1: TListBox
    Left = 55
    Top = 175
    Width = 121
    Height = 97
    ItemHeight = 13
    Items.Strings = (
      'Item1'
      'Item2'
      'Item3')
    TabOrder = 1
  end
end
