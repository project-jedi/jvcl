object Form1: TForm1
  Left = 216
  Top = 73
  Width = 833
  Height = 688
  Caption = 'JvUninstall Controls Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 385
    Top = 0
    Width = 5
    Height = 661
    Cursor = crSizeWE
  end
  object Panel2: TPanel
    Left = 390
    Top = 0
    Width = 435
    Height = 661
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object JvUninstallComboBox1: TJvUninstallComboBox
      Left = 0
      Top = 0
      Width = 434
      Height = 21
      DropDownCount = 25
      ItemHeight = 13
      Style = csDropDownList
      TabOrder = 0
      OnClick = JvUninstallComboBox1Click
      Anchors = [akLeft, akTop, akRight]
    end
    object chkComboShowAll: TCheckBox
      Left = 15
      Top = 32
      Width = 69
      Height = 17
      Caption = 'Show All'
      TabOrder = 1
      OnClick = chkComboShowAllClick
    end
    object chkComboSorted: TCheckBox
      Left = 95
      Top = 32
      Width = 64
      Height = 17
      Caption = 'Sorted'
      TabOrder = 2
      OnClick = chkComboSortedClick
    end
    object memComboInfo: TMemo
      Left = 0
      Top = 56
      Width = 434
      Height = 603
      Anchors = [akLeft, akTop, akRight, akBottom]
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 3
      WordWrap = False
    end
    object chkComboEmptyValues: TCheckBox
      Left = 168
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Show Empty'
      TabOrder = 4
      OnClick = chkComboEmptyValuesClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 385
    Height = 661
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object JvUninstallListBox1: TJvUninstallListBox
      Left = 2
      Top = 0
      Width = 383
      Height = 203
      ItemHeight = 13
      TabOrder = 0
      OnClick = JvUninstallListBox1Click
      Anchors = [akLeft, akTop, akBottom]
    end
    object chkListShowAll: TCheckBox
      Left = 8
      Top = 210
      Width = 63
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Show All'
      TabOrder = 1
      OnClick = chkListShowAllClick
    end
    object chkListSorted: TCheckBox
      Left = 88
      Top = 210
      Width = 55
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Sorted'
      TabOrder = 2
      OnClick = chkListSortedClick
    end
    object chkShowEmpty: TCheckBox
      Left = 168
      Top = 210
      Width = 97
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Show Empty'
      TabOrder = 3
      OnClick = chkShowEmptyClick
    end
    object memListInfo: TMemo
      Left = 0
      Top = 233
      Width = 383
      Height = 425
      Anchors = [akLeft, akRight, akBottom]
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 4
      WantReturns = False
      WordWrap = False
    end
  end
end
