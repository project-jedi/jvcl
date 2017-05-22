object Form3: TForm3
  Left = 324
  Top = 254
  Width = 164
  Height = 180
  BorderStyle = bsSizeToolWin
  Caption = 'VID Style'
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
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 156
    Height = 153
    Align = alClient
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object lbDockClient1: TJvDockClient
    OnFormShow = lbDockClient1FormShow
    OnFormHide = lbDockClient1FormHide
    LRDockWidth = 100
    TBDockHeight = 100
    NCPopupMenu = MainForm.PopupMenu2
    DirectDrag = False
    ShowHint = True
    EnableCloseButton = True
    DockStyle = MainForm.JvDockVIDStyle1
    Left = 64
    Top = 56
  end
end
