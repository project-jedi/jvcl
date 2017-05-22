object AutosForm: TAutosForm
  Left = 563
  Top = 86
  Width = 380
  Height = 290
  BorderStyle = bsSizeToolWin
  Caption = 'Autos'
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
    Top = 20
    Width = 372
    Height = 243
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'Panel'
    TabOrder = 0
    OnResize = PanelResize
    object ListBox1: TListBox
      Left = 2
      Top = 2
      Width = 368
      Height = 239
      Align = alClient
      BorderStyle = bsNone
      ItemHeight = 15
      TabOrder = 0
    end
  end
  object Header: THeader
    Left = 0
    Top = 0
    Width = 372
    Height = 20
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
    EnableCloseButton = True
    Left = 64
    Top = 88
  end
end
