object DocumentOutlineForm: TDocumentOutlineForm
  Left = 618
  Top = 241
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
  Font.Name = #203#206#204#229
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 209
    Height = 115
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
    EnableCloseButton = True
    Left = 56
    Top = 48
  end
end
