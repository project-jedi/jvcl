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
  Font.Name = 'ËÎÌו'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 12
  object LookInLabel: TLabel
    Left = 120
    Top = 79
    Width = 56
    Height = 12
    AutoSize = False
    Caption = '&Look In:'
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 407
    Height = 25
    ButtonHeight = 23
    ButtonWidth = 24
    Caption = 'ToolBar1'
    EdgeBorders = [ebLeft, ebTop, ebRight, ebBottom]
    EdgeOuter = esNone
    Flat = True
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
    Left = 5
    Top = 28
    Width = 108
    Height = 20
    Caption = '&Find'
    TabOrder = 1
  end
  object ReplaceButton: TButton
    Left = 5
    Top = 52
    Width = 108
    Height = 20
    Caption = '&Replace'
    TabOrder = 2
  end
  object ReplaceAllButton: TButton
    Left = 5
    Top = 76
    Width = 108
    Height = 20
    Caption = 'Replace &All'
    TabOrder = 3
  end
  object FindComboBox: TComboBox
    Left = 120
    Top = 28
    Width = 145
    Height = 20
    ItemHeight = 12
    TabOrder = 4
  end
  object ReplaceComboBox: TComboBox
    Left = 120
    Top = 52
    Width = 145
    Height = 20
    ItemHeight = 12
    TabOrder = 5
  end
  object LookInComboBox: TComboBox
    Left = 176
    Top = 76
    Width = 89
    Height = 20
    ItemHeight = 12
    TabOrder = 6
  end
  object SubfoldersCheckBox: TCheckBox
    Left = 176
    Top = 100
    Width = 97
    Height = 17
    Caption = 'Subfolders'
    TabOrder = 7
  end
  object BrowseButton: TButton
    Left = 272
    Top = 100
    Width = 108
    Height = 20
    Caption = 'Browse...'
    TabOrder = 8
  end
  object ListView: TListView
    Left = 5
    Top = 125
    Width = 372
    Height = 177
    Columns = <
      item
        Caption = 'Name'
        Width = 60
      end
      item
        Caption = 'Folder'
        Width = 95
      end
      item
        Caption = 'Line'
        Width = 52
      end
      item
        Caption = 'Text'
        Width = 120
      end>
    TabOrder = 9
    ViewStyle = vsReport
  end
  object lbDockClient1: TJvDockClient
    LRDockWidth = 100
    TBDockHeight = 100
    DirectDrag = False
    ShowHint = True
    DockStyle = MainForm.JvDockVIDStyle1
    Left = 304
    Top = 64
  end
end
