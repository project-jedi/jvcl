object DeepEmbeddedForm: TDeepEmbeddedForm
  Left = 725
  Top = 269
  Width = 288
  Height = 198
  Caption = 'DeepEmbeddedForm'
  Color = 15980476
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 280
    Height = 16
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'Deeply Embedded Shared Form'
    Color = clMaroon
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object RadioGroup1: TRadioGroup
    Left = 0
    Top = 16
    Width = 280
    Height = 155
    Align = alClient
    Caption = 'RadioGroup1'
    TabOrder = 0
  end
  object RadioButton1: TRadioButton
    Left = 10
    Top = 82
    Width = 113
    Height = 17
    Caption = 'RadioButton1'
    TabOrder = 1
  end
  object CheckBox2: TCheckBox
    Left = 10
    Top = 56
    Width = 97
    Height = 17
    Caption = 'CheckBox2'
    TabOrder = 2
  end
  object CheckBox1: TCheckBox
    Left = 10
    Top = 34
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 3
  end
  object JvEmbeddedFormLink1: TJvEmbeddedFormLink
    Left = 192
    Top = 72
  end
end
