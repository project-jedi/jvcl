object Form2: TForm2
  Left = 307
  Top = 248
  Width = 442
  Height = 381
  Caption = 'Form2'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    Left = 8
    Top = 152
    Width = 417
    Height = 193
    Caption = ' Database '
    TabOrder = 0
    object Label3: TLabel
      Left = 129
      Top = 20
      Width = 251
      Height = 13
      Caption = 'Click button "Show Form3 after dataset was opened"'
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 18
      Width = 97
      Height = 17
      Caption = 'Open dataset'
      TabOrder = 0
      OnClick = CheckBox2Click
    end
    object Button2: TButton
      Left = 14
      Top = 161
      Width = 75
      Height = 23
      Caption = 'AddRecord'
      TabOrder = 1
      OnClick = Button2Click
    end
    object DBGrid1: TDBGrid
      Left = 8
      Top = 43
      Width = 401
      Height = 110
      DataSource = DataModule1.DataSource1
      TabOrder = 2
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
    object pnlStatus: TPanel
      Left = 104
      Top = 162
      Width = 305
      Height = 22
      BevelOuter = bvLowered
      Caption = 'pnlStatus'
      TabOrder = 3
    end
  end
  object Button3: TButton
    Left = 8
    Top = 9
    Width = 113
    Height = 25
    Caption = 'Show Form3'
    TabOrder = 1
    OnClick = Button3Click
  end
  object RegAuto1: TJvFormStorage
    StoredValues = <>
    Left = 232
    Top = 104
  end
  object Table1: TTable
    Left = 192
    Top = 40
  end
end
