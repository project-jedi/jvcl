object IniSections: TIniSections
  Left = 200
  Top = 137
  Width = 359
  Height = 295
  Caption = 'Go to section'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 114
    Height = 13
    Caption = 'Choose section form list:'
  end
  object ListBox1: TListBox
    Left = 16
    Top = 40
    Width = 321
    Height = 177
    ItemHeight = 13
    TabOrder = 0
  end
  object Button1: TButton
    Left = 84
    Top = 232
    Width = 91
    Height = 25
    Caption = 'Go to section'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 188
    Top = 232
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
