object JvMruListForm: TJvMruListForm
  Left = 291
  Top = 150
  Width = 696
  Height = 480
  Caption = 'JvMruListForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvCaptionPanel1: TJvCaptionPanel
    Left = 26
    Top = 21
    Width = 583
    Height = 396
    Buttons = [capClose, capHelp]
    Caption = 'Demo for JvMruList'
    CaptionFont.Charset = DEFAULT_CHARSET
    CaptionFont.Color = clWhite
    CaptionFont.Height = -13
    CaptionFont.Name = 'Arial'
    CaptionFont.Style = [fsBold]
    OutlookLook = False
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 54
      Top = 52
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
      Left = 56
      Top = 132
      Width = 323
      Height = 153
      ItemHeight = 13
      TabOrder = 1
    end
    object btnRefresh: TButton
      Left = 386
      Top = 134
      Width = 87
      Height = 25
      Caption = '&Refresh'
      Enabled = False
      TabOrder = 2
      OnClick = btnRefreshClick
    end
    object btnFirst: TButton
      Left = 386
      Top = 176
      Width = 87
      Height = 25
      Caption = '&Get First one'
      Enabled = False
      TabOrder = 3
      OnClick = btnFirstClick
    end
    object btnDeleteFirst: TButton
      Left = 386
      Top = 218
      Width = 87
      Height = 25
      Caption = '&Delete First one'
      Enabled = False
      TabOrder = 4
      OnClick = btnDeleteFirstClick
    end
    object btnAdd: TButton
      Left = 386
      Top = 260
      Width = 87
      Height = 25
      Caption = '&Add'
      Enabled = False
      TabOrder = 5
      OnClick = btnAddClick
    end
  end
  object JvMruList1: TJvMruList
    SubKeyUnicode = 'Software\MruTest'
    OnEnumText = JvMruList1EnumText
    Active = False
    Left = 180
    Top = 226
  end
end
