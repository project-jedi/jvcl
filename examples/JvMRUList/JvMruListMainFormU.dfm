object JvMruListMainForm: TJvMruListMainForm
  Left = 256
  Top = 235
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Demo for JvMruList'
  ClientHeight = 246
  ClientWidth = 434
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 6
    Top = 4
    Width = 419
    Height = 75
    Caption = ' Settings'
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
    object btnOpen: TButton
      Left = 332
      Top = 28
      Width = 75
      Height = 25
      Caption = '&Open'
      TabOrder = 1
      OnClick = btnOpenClick
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
  object btnRefresh: TButton
    Left = 338
    Top = 86
    Width = 87
    Height = 25
    Caption = '&Refresh'
    Enabled = False
    TabOrder = 2
    OnClick = btnRefreshClick
  end
  object btnFirst: TButton
    Left = 338
    Top = 128
    Width = 87
    Height = 25
    Caption = '&Get First one'
    Enabled = False
    TabOrder = 3
    OnClick = btnFirstClick
  end
  object btnDeleteFirst: TButton
    Left = 338
    Top = 170
    Width = 87
    Height = 25
    Caption = '&Delete First one'
    Enabled = False
    TabOrder = 4
    OnClick = btnDeleteFirstClick
  end
  object btnAdd: TButton
    Left = 338
    Top = 212
    Width = 87
    Height = 25
    Caption = '&Add'
    Enabled = False
    TabOrder = 5
    OnClick = btnAddClick
  end
  object JvMruList1: TJvMRUList
    SubKeyUnicode = 'Software\MruTest'
    OnEnumText = JvMruList1EnumText
    Active = False
    Left = 116
    Top = 130
  end
end
