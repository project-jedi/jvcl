object Form1: TForm1
  Left = 252
  Top = 133
  Width = 236
  Height = 153
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
    Left = 12
    Top = 18
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Edit1: TEdit
    Left = 8
    Top = 35
    Width = 129
    Height = 21
    TabOrder = 1
    Text = 'Peter'
    OnChange = Edit1Change
  end
  object JvDomainUpDown1: TJvDomainUpDown
    Left = 137
    Top = 35
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
    TabOrder = 0
    Wrap = True
  end
end
