object CallStackForm: TCallStackForm
  Left = 437
  Top = 259
  Width = 220
  Height = 270
  BorderStyle = bsSizeToolWin
  Caption = 'Call Stack'
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
  object Panel: TPanel
    Left = 0
    Top = 50
    Width = 212
    Height = 193
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'Panel'
    TabOrder = 0
    object ListBox1: TListBox
      Left = 2
      Top = 2
      Width = 208
      Height = 189
      Align = alClient
      BorderStyle = bsNone
      ItemHeight = 15
      TabOrder = 0
    end
  end
  object Header: THeader
    Left = 0
    Top = 30
    Width = 212
    Height = 20
    Align = alTop
    BorderStyle = bsNone
    Sections.Sections = (
      #0'8'#0
      #0'20'#0'ID'
      #0'56'#0'Language')
    TabOrder = 1
  end
  object ComboBoxPanel: TPanel
    Left = 0
    Top = 0
    Width = 212
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    OnResize = ComboBoxPanelResize
    object ComboBox: TComboBox
      Left = 0
      Top = 1
      Width = 221
      Height = 23
      ItemHeight = 15
      TabOrder = 0
    end
  end
  object lbDockClient1: TJvDockClient
    LRDockWidth = 100
    TBDockHeight = 100
    DirectDrag = False
    ShowHint = True
    EnableCloseButton = True
    Left = 64
    Top = 88
  end
end
