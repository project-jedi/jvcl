object Form1: TForm1
  Left = 193
  Top = 107
  AutoScroll = False
  Caption = 'JvUninstall Controls Demo'
  ClientHeight = 343
  ClientWidth = 432
  Color = clBtnFace
  Constraints.MinHeight = 370
  Constraints.MinWidth = 440
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnResize = FormResize
  DesignSize = (
    432
    343)
  PixelsPerInch = 96
  TextHeight = 13
  object JvUninstallComboBox1: TJvUninstallComboBox
    Left = 224
    Top = 16
    Width = 201
    Height = 21
    ShowAll = False
    DropDownCount = 25
    ItemHeight = 13
    Style = csDropDownList
    TabOrder = 0
    OnClick = JvUninstallComboBox1Click
  end
  object JvUninstallListBox1: TJvUninstallListBox
    Left = 8
    Top = 16
    Width = 201
    Height = 112
    ShowAll = False
    ItemHeight = 13
    TabOrder = 1
    OnClick = JvUninstallListBox1Click
    Anchors = [akLeft, akTop, akBottom]
  end
  object chkListShowAll: TCheckBox
    Left = 16
    Top = 136
    Width = 97
    Height = 17
    Caption = 'Show All'
    TabOrder = 2
    OnClick = chkListShowAllClick
  end
  object chkComboShowAll: TCheckBox
    Left = 224
    Top = 136
    Width = 97
    Height = 17
    Caption = 'Show All'
    TabOrder = 3
    OnClick = chkComboShowAllClick
  end
  object memListInfo: TMemo
    Left = 8
    Top = 159
    Width = 201
    Height = 177
    Anchors = [akLeft, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
    WantReturns = False
    WordWrap = False
  end
  object memComboInfo: TMemo
    Left = 224
    Top = 160
    Width = 201
    Height = 176
    Anchors = [akLeft, akTop, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 5
    WordWrap = False
  end
  object chkListSorted: TCheckBox
    Left = 112
    Top = 136
    Width = 55
    Height = 17
    Caption = 'Sorted'
    TabOrder = 6
    OnClick = chkListSortedClick
  end
  object chkComboSorted: TCheckBox
    Left = 320
    Top = 136
    Width = 97
    Height = 17
    Caption = 'Sorted'
    TabOrder = 7
    OnClick = chkComboSortedClick
  end
end
