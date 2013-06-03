object Form1: TForm1
  Left = 304
  Top = 186
  BorderStyle = bsDialog
  Caption = 'JvValidateEdit Demo'
  ClientHeight = 143
  ClientWidth = 396
  Color = clBtnFace
  Constraints.MinHeight = 170
  Constraints.MinWidth = 325
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 48
    Width = 79
    Height = 13
    Caption = 'Character Type:'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 30
    Height = 13
    Caption = 'Input:'
  end
  object Label3: TLabel
    Left = 8
    Top = 96
    Width = 245
    Height = 13
    Caption = 'Custom Characters (used with Valid/Invalid Chars):'
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 64
    Width = 380
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnChange = ComboBox1Change
    OnKeyPress = ComboBox1KeyPress
  end
  object Button2: TButton
    Left = 367
    Top = 112
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = Button2Click
  end
  object RichEdit1: TRichEdit
    Left = 8
    Top = 112
    Width = 353
    Height = 21
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
  end
end
