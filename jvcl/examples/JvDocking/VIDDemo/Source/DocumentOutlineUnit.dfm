object DocumentOutlineForm: TDocumentOutlineForm
  Left = 235
  Top = 161
  Width = 223
  Height = 148
  BorderStyle = bsSizeToolWin
  BorderWidth = 3
  Caption = 'Document Outline'
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
  PixelsPerInch = 96
  TextHeight = 12
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 209
    Height = 114
    Align = alClient
    BorderStyle = bsNone
    Lines.Strings = (
      'There are no items to show for the '
      'selected document.')
    ParentColor = True
    TabOrder = 0
  end
  object lbDockClient1: TJvDockClient
    LRDockWidth = 100
    TBDockHeight = 100
    DirectDrag = False
    ShowHint = True
    DockStyle = MainForm.JvDockVIDStyle1
    Left = 56
    Top = 48
  end
end
