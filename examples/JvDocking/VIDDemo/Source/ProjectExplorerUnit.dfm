object ProjectExplorerForm: TProjectExplorerForm
  Left = 256
  Top = 183
  Width = 220
  Height = 270
  BorderStyle = bsSizeToolWin
  Caption = 'Project Explorer'
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
  PixelsPerInch = 96
  TextHeight = 12
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 212
    Height = 24
    Caption = 'ToolBar1'
    EdgeBorders = [ebLeft, ebTop, ebRight, ebBottom]
    EdgeOuter = esNone
    Flat = True
    Images = MainForm.ImageList1
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Action = MainForm.Action_Open
    end
    object ToolButton3: TToolButton
      Left = 23
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object ToolButton2: TToolButton
      Left = 31
      Top = 0
      Action = MainForm.Action_Properties_Window
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 24
    Width = 212
    Height = 218
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 1
    object TreeView1: TTreeView
      Left = 2
      Top = 2
      Width = 208
      Height = 214
      Align = alClient
      BorderStyle = bsNone
      Indent = 19
      TabOrder = 0
    end
  end
  object lbDockClient1: TJvDockClient
    LRDockWidth = 100
    TBDockHeight = 100
    DirectDrag = False
    ShowHint = True
    DockStyle = MainForm.JvDockVIDStyle1
    Left = 64
    Top = 88
  end
end
