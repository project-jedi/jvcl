object FindAndReplaceForm: TFindAndReplaceForm
  Left = 513
  Top = 163
  Width = 415
  Height = 367
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderStyle = bsSizeToolWin
  Caption = 'Find'
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #203#206#204#229
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 15
  object LookInLabel: TLabel
    Left = 150
    Top = 99
    Width = 70
    Height = 15
    AutoSize = False
    Caption = '&Look In:'
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 475
    Height = 25
    ButtonHeight = 23
    ButtonWidth = 24
    Caption = 'ToolBar1'
    EdgeBorders = [ebLeft, ebTop, ebRight, ebBottom]
    EdgeOuter = esNone
    Flat = True
    Images = MainForm.ImageList1
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Caption = 'ToolButton1'
      ImageIndex = 0
    end
    object ToolButton4: TToolButton
      Left = 24
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object ToolButton2: TToolButton
      Left = 32
      Top = 0
      Caption = 'ToolButton2'
      ImageIndex = 1
    end
    object ToolButton3: TToolButton
      Left = 56
      Top = 0
      Caption = 'ToolButton3'
      ImageIndex = 2
    end
    object ToolButton5: TToolButton
      Left = 80
      Top = 0
      Caption = 'ToolButton5'
      ImageIndex = 3
    end
    object ToolButton6: TToolButton
      Left = 104
      Top = 0
      Caption = 'ToolButton6'
      ImageIndex = 4
    end
    object ToolButton7: TToolButton
      Left = 128
      Top = 0
      Width = 8
      Caption = 'ToolButton7'
      ImageIndex = 5
      Style = tbsSeparator
    end
    object ToolButton8: TToolButton
      Left = 136
      Top = 0
      Caption = 'ToolButton8'
      ImageIndex = 5
    end
    object ToolButton9: TToolButton
      Left = 160
      Top = 0
      Caption = 'ToolButton9'
      ImageIndex = 6
    end
  end
  object Find: TButton
    Left = 6
    Top = 35
    Width = 135
    Height = 25
    Caption = '&Find'
    TabOrder = 1
  end
  object ReplaceButton: TButton
    Left = 6
    Top = 65
    Width = 135
    Height = 25
    Caption = '&Replace'
    TabOrder = 2
  end
  object ReplaceAllButton: TButton
    Left = 6
    Top = 95
    Width = 135
    Height = 25
    Caption = 'Replace &All'
    TabOrder = 3
  end
  object FindComboBox: TComboBox
    Left = 150
    Top = 35
    Width = 181
    Height = 23
    ItemHeight = 15
    TabOrder = 4
  end
  object ReplaceComboBox: TComboBox
    Left = 150
    Top = 65
    Width = 181
    Height = 23
    ItemHeight = 15
    TabOrder = 5
  end
  object LookInComboBox: TComboBox
    Left = 220
    Top = 95
    Width = 111
    Height = 23
    ItemHeight = 15
    TabOrder = 6
  end
  object SubfoldersCheckBox: TCheckBox
    Left = 220
    Top = 125
    Width = 121
    Height = 21
    Caption = 'Subfolders'
    TabOrder = 7
  end
  object BrowseButton: TButton
    Left = 340
    Top = 125
    Width = 135
    Height = 25
    Caption = 'Browse...'
    TabOrder = 8
  end
  object ListView: TListView
    Left = 6
    Top = 156
    Width = 465
    Height = 222
    Columns = <
      item
        Caption = 'Name'
        Width = 75
      end
      item
        Caption = 'Folder'
        Width = 119
      end
      item
        Caption = 'Line'
        Width = 65
      end
      item
        Caption = 'Text'
        Width = 150
      end>
    TabOrder = 9
    ViewStyle = vsReport
  end
  object lbDockClient1: TJvDockClient
    LRDockWidth = 100
    TBDockHeight = 100
    DirectDrag = False
    ShowHint = True
    EnableCloseButton = True
    Left = 304
    Top = 64
  end
end
