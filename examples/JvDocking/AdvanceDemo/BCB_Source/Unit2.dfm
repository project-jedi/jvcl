object Form2: TForm2
  Left = 234
  Top = 212
  Width = 194
  Height = 198
  BorderStyle = bsSizeToolWin
  Caption = 'VC++ Style'
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 186
    Height = 171
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 0
    object Memo1: TMemo
      Left = 2
      Top = 2
      Width = 182
      Height = 167
      Align = alClient
      BorderStyle = bsNone
      Lines.Strings = (
        'Memo1')
      TabOrder = 0
    end
  end
  object lbDockClient1: TJvDockClient
    OnFormShow = lbDockClient1FormShow
    OnFormHide = lbDockClient1FormHide
    LRDockWidth = 100
    TBDockHeight = 100
    NCPopupMenu = MainForm.PopupMenu2
    DirectDrag = True
    ShowHint = True
    EnableCloseButton = True
    EachOtherDock = False
    DockStyle = MainForm.JvDockVCStyle1
    Left = 48
    Top = 40
  end
end
