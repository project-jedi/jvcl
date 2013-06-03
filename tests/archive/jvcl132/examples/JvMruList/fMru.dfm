object Form1: TForm1
  Left = 256
  Top = 235
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 246
  ClientWidth = 434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 6
    Top = 4
    Width = 419
    Height = 75
    Caption = 'GroupBox1'
    TabOrder = 0
    object Label1: TLabel
      Left = 18
      Top = 22
      Width = 59
      Height = 13
      Caption = 'Registry Key'
    end
    object Label2: TLabel
      Left = 8
      Top = 48
      Width = 76
      Height = 13
      Caption = 'Number of items'
    end
    object Edit1: TEdit
      Left = 94
      Top = 18
      Width = 223
      Height = 21
      TabOrder = 0
      Text = 'Software\MruList'
    end
    object Button1: TButton
      Left = 332
      Top = 28
      Width = 75
      Height = 25
      Caption = '&Open'
      TabOrder = 1
      OnClick = Button1Click
    end
    object SpinEdit1: TSpinEdit
      Left = 94
      Top = 44
      Width = 223
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 10
    end
  end
  object ListBox1: TListBox
    Left = 8
    Top = 84
    Width = 323
    Height = 153
    ItemHeight = 13
    TabOrder = 1
  end
  object Button2: TButton
    Left = 338
    Top = 86
    Width = 87
    Height = 25
    Caption = '&Refresh'
    Enabled = False
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 338
    Top = 128
    Width = 87
    Height = 25
    Caption = '&Get First one'
    Enabled = False
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 338
    Top = 170
    Width = 87
    Height = 25
    Caption = '&Delete First one'
    Enabled = False
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 338
    Top = 212
    Width = 87
    Height = 25
    Caption = '&Add'
    Enabled = False
    TabOrder = 5
    OnClick = Button5Click
  end
  object JvMruList1: TJvMruList
    SubKey = 'Software\MruTest'
    OnEnumText = JvMruList1EnumText
    Left = 116
    Top = 130
  end
end
