object ProjectExplorerForm: TProjectExplorerForm
  Left = 461
  Top = 195
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
  Font.Name = #203#206#204#229
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 15
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
      ImageIndex = 19
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
      ImageIndex = 20
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 24
    Width = 212
    Height = 219
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 1
    object TreeView1: TTreeView
      Left = 2
      Top = 2
      Width = 208
      Height = 215
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
    EnableCloseButton = True
    DockStyle = MainForm.JvDockVIDStyle1
    Left = 64
    Top = 88
  end
end
