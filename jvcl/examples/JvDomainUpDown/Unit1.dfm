object Form1: TForm1
  Left = 252
  Top = 133
  Width = 870
  Height = 640
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 678
    Top = 444
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object JvPanel1: TJvPanel
    Left = 38
    Top = 40
    Width = 221
    Height = 111
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'JvPanel1'
    TabOrder = 0
    Transparent = True
    MultiLine = False
  end
  object JvTreeView1: TJvTreeView
    Left = 518
    Top = 52
    Width = 119
    Height = 213
    Indent = 19
    MultiSelect = True
    MultiSelectStyle = [msControlSelect, msShiftSelect]
    TabOrder = 1
    Items.Data = {
      060000001A0000000000000000000000FFFFFFFFFFFFFFFF0000000006000000
      01311A0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000161
      1A0000000000000000000000FFFFFFFFFFFFFFFF000000000000000001621A00
      00000000000000000000FFFFFFFFFFFFFFFF000000000000000001631A000000
      0000000000000000FFFFFFFFFFFFFFFF000000000000000001641A0000000000
      000000000000FFFFFFFFFFFFFFFF000000000000000001651A00000000000000
      00000000FFFFFFFFFFFFFFFF000000000000000001661A000000000000000000
      0000FFFFFFFFFFFFFFFF000000000000000001321A0000000000000000000000
      FFFFFFFFFFFFFFFF000000000000000001331A0000000000000000000000FFFF
      FFFFFFFFFFFF000000000000000001341A0000000000000000000000FFFFFFFF
      FFFFFFFF000000000000000001351A0000000000000000000000FFFFFFFFFFFF
      FFFF00000000000000000136}
  end
  object JvPageListTreeView1: TJvPageListTreeView
    Left = 96
    Top = 224
    Width = 273
    Height = 313
    AutoExpand = False
    ShowButtons = True
    ShowLines = True
    ReadOnly = False
    PageDefault = 0
    Indent = 19
    TabOrder = 2
    Items.Links = {00000000}
  end
  object Edit1: TEdit
    Left = 518
    Top = 443
    Width = 129
    Height = 21
    TabOrder = 3
    Text = 'Peter'
    OnChange = Edit1Change
  end
  object JvDomainUpDown1: TJvDomainUpDown
    Left = 647
    Top = 443
    Width = 15
    Height = 21
    Associate = Edit1
    Items.Strings = (
      'Peter'
      'Bella'
      'Simon'
      'Elias'
      'Maja'
      'Higgins'
      'Felix')
    Position = 0
    Text = 'Peter'
    TabOrder = 4
    Wrap = True
  end
end
