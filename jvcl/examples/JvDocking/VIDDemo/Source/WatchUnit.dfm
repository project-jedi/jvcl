object WatchForm: TWatchForm
  Left = 256
  Top = 183
  Width = 220
  Height = 270
  BorderStyle = bsSizeToolWin
  Caption = 'Watch'
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
  object Panel: TPanel
    Left = 0
    Top = 16
    Width = 212
    Height = 226
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'Panel'
    TabOrder = 0
    OnResize = PanelResize
    object ListBox1: TListBox
      Left = 2
      Top = 2
      Width = 208
      Height = 222
      Align = alClient
      BorderStyle = bsNone
      ItemHeight = 12
      TabOrder = 0
    end
  end
  object Header: THeader
    Left = 0
    Top = 0
    Width = 212
    Height = 16
    Align = alTop
    BorderStyle = bsNone
    Sections.Sections = (
      #0'79'#0'Name'
      #0'92'#0'Value'
      #0'32'#0'Type')
    TabOrder = 1
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
