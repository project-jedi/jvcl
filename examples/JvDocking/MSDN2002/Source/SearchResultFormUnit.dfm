object SearchResultForm: TSearchResultForm
  Left = 572
  Top = 165
  Width = 236
  Height = 270
  BorderStyle = bsSizeToolWin
  Caption = 'Search Result'
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
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 228
    Height = 243
    Align = alClient
    Columns = <
      item
        Caption = 'Title'
        Width = 250
      end
      item
        Caption = 'Location'
        Width = 250
      end
      item
        Caption = 'Rank'
        Width = 250
      end>
    TabOrder = 0
    ViewStyle = vsReport
  end
  object lbDockClient1: TJvDockClient
    LRDockWidth = 150
    TBDockHeight = 150
    NCPopupMenu = MSDN2002.PopupMenu1
    DirectDrag = False
    ShowHint = True
    EnableCloseButton = True
    DockStyle = MSDN2002.JvDockVSNetStyle1
    Left = 64
    Top = 88
  end
end
