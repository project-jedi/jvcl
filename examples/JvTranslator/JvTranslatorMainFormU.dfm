object JvTranslatorMainForm: TJvTranslatorMainForm
  Left = 368
  Top = 147
  Width = 449
  Height = 403
  Caption = 'JvTranslator'
  Color = clBtnFace
  Constraints.MinHeight = 230
  Constraints.MinWidth = 345
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 142
    Top = 20
    Width = 51
    Height = 13
    Caption = 'Some Text'
  end
  object TreeView1: TTreeView
    Left = 8
    Top = 16
    Width = 123
    Height = 341
    Anchors = [akLeft, akTop, akBottom]
    Indent = 19
    TabOrder = 0
    Items.Data = {
      030000001E0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000
      054974656D311E0000000000000000000000FFFFFFFFFFFFFFFF000000000000
      0000054974656D321E0000000000000000000000FFFFFFFFFFFFFFFF00000000
      00000000054974656D33}
  end
  object CheckBox1: TCheckBox
    Left = 140
    Top = 40
    Width = 97
    Height = 17
    Caption = 'Again some text'
    TabOrder = 1
  end
  object Button1: TButton
    Left = 355
    Top = 301
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'French'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 355
    Top = 331
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'English'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 140
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Push me'
    TabOrder = 4
    OnClick = Button3Click
  end
  object JvTranslator1: TJvTranslator
    Left = 232
    Top = 8
  end
  object Variables: TJvTranslatorStrings
    Left = 296
    Top = 8
  end
  object JvTranslator2: TJvTranslator
    Left = 264
    Top = 8
  end
end
