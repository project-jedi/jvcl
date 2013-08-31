object JvTranslatorMainForm: TJvTranslatorMainForm
  Left = 368
  Top = 147
  Width = 458
  Height = 407
  ActiveControl = Button1
  Caption = 'JvTranslator'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 142
    Top = 20
    Width = 147
    Height = 13
    AutoSize = False
    Caption = 'Some Text'
  end
  object TreeView1: TTreeView
    Left = 8
    Top = 16
    Width = 123
    Height = 137
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
    Width = 125
    Height = 17
    Caption = 'Again some text'
    TabOrder = 1
  end
  object Button1: TButton
    Left = 352
    Top = 28
    Width = 75
    Height = 25
    Caption = 'French'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 352
    Top = 58
    Width = 75
    Height = 25
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
  object Button4: TButton
    Left = 10
    Top = 161
    Width = 133
    Height = 25
    Caption = 'Create XML from Form'
    TabOrder = 5
    OnClick = Button4Click
  end
  object RichEdit1: TRichEdit
    Left = 10
    Top = 192
    Width = 431
    Height = 177
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 6
    WordWrap = False
  end
  object ListView1: TListView
    Left = 156
    Top = 110
    Width = 285
    Height = 73
    Columns = <
      item
        Caption = 'Filename'
        Width = 100
      end
      item
        Caption = 'Size'
        Width = -2
        WidthType = (
          -2)
      end>
    Items.Data = {
      7B0000000300000000000000FFFFFFFFFFFFFFFF01000000000000000C617574
      6F657865632E62617403356B4200000000FFFFFFFFFFFFFFFF01000000000000
      000A636F6E6669672E73797303316B4200000000FFFFFFFFFFFFFFFF01000000
      000000000B636F6D6D616E642E636F6D0432336B42FFFFFFFFFFFF}
    TabOrder = 7
    ViewStyle = vsReport
  end
  object JvTranslator1: TJvTranslator
    Left = 302
    Top = 12
  end
  object Variables: TJvTranslatorStrings
    Left = 336
    Top = 12
  end
end
