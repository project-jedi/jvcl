object JvSIMDModifyFrm: TJvSIMDModifyFrm
  Left = 654
  Top = 246
  BorderStyle = bsDialog
  Caption = 'JvSIMDModifyFrm'
  ClientHeight = 385
  ClientWidth = 481
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDisplay: TLabel
    Left = 8
    Top = 16
    Width = 40
    Height = 13
    Caption = 'Display :'
    Layout = tlCenter
  end
  object LabelFormat: TLabel
    Left = 240
    Top = 16
    Width = 38
    Height = 13
    Caption = 'Format :'
    Layout = tlCenter
  end
  object LabelBlank: TLabel
    Left = 8
    Top = 48
    Width = 123
    Height = 13
    Caption = 'Keep blank for no change'
  end
  object ComboBoxDisplay: TComboBox
    Left = 56
    Top = 16
    Width = 137
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = ComboBoxDisplayChange
    Items.Strings = (
      'Bytes'
      'Words'
      'DWords'
      'QWords'
      'Singles'
      'Doubles')
  end
  object ComboBoxFormat: TComboBox
    Left = 288
    Top = 16
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = ComboBoxFormatChange
    Items.Strings = (
      'Binary'
      'Signed Decimal'
      'Unsigned Decimal'
      'Hexadecimal')
  end
  object PanelModify: TPanel
    Left = 8
    Top = 72
    Width = 465
    Height = 265
    BevelInner = bvLowered
    TabOrder = 2
  end
  object ButtonOK: TButton
    Left = 384
    Top = 352
    Width = 91
    Height = 25
    Caption = '&OK'
    TabOrder = 3
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 280
    Top = 352
    Width = 91
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
