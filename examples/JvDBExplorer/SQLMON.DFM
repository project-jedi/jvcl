object TraceSQL: TTraceSQL
  Left = 288
  Top = 162
  Width = 412
  Height = 262
  Caption = 'SQL Monitor'
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000000020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000009F00000
    0000000000000000000000009F900000000000000000000000000009F9000000
    00000000000000000000009F900000000000000000000000000009F900000000
    000000000880000008809F900000008888888888800FFFFFF009F90888000F77
    777777770FFCCCCCCFF0907778800F7777777770FCCCCCCCCCCF077777800F77
    77FFFF0FCCCCCCCCCCCCF07777800F777000080FCCCCCCCCCCCCF0FF77800F77
    00C4C0FCCBBCCCBBBCBBCF00F7800F770C4EE0FCBCCBCBCBBCBCCF00F7800F77
    04C4C0FCCCCBCBCCBCBCCF00F7800F770C4EE0FCCBBCCBCCBCBCCF00F7800F77
    04C4C0FCBCCCCBCCBCBCCF00F7800F770C4EE0FCBCCBCBCCBCBCCF00F7800F77
    04C4C40FCBBCCCBBCCBCF0C0F7800F770C4EEE0FCCCCCCCCCCCCF040F7800F77
    04C4C4C0FCCCCCCCCCCF04C0F7800F770C4EEEEE0FFCCCCCCFF04C40F7800F77
    04C4C4C4C00FFFFFF004C4C0F7800F770C4EEEEEEC4000000CCC4C40F7800F77
    04C4C4C4C4C4C4C4C4C4C4C0F7800F770C4EEEEEEEEE4C4C4C4C4C40F7800F77
    00C4C4C4C4C4C4C4C4C4C40077800F7770000000000000000000000777800F77
    77777777777777777777777777800FF7777777777777777777777777778000FF
    FFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000000000000000000000FFFF
    FFF9FFFFFFF0FFFFFFE0FFFFFFC1FFFFFF83FFFFFF0780000001000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000080000001}
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnHide = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TraceBox: TJvDrawGrid
    Left = 0
    Top = 0
    Width = 404
    Height = 173
    Align = alClient
    ColCount = 3
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 2
    Options = [goFixedHorzLine, goDrawFocusSelected, goRowSelect, goThumbTracking]
    ScrollBars = ssVertical
    TabOrder = 0
    OnClick = TraceBoxClick
    OnDrawCell = TraceBoxDrawCell
    ColWidths = (
      40
      75
      285)
  end
  object Splitter: TJvxSplitter
    Left = 0
    Top = 173
    Width = 404
    Height = 3
    ControlFirst = TraceBox
    ControlSecond = ViewPanel
    Align = alBottom
    LimitOffset = 10
  end
  object ViewPanel: TMemo
    Left = 0
    Top = 176
    Width = 404
    Height = 59
    Align = alBottom
    PopupMenu = PopupMenu
    ReadOnly = True
    TabOrder = 2
  end
  object FormStorage: TJvFormStorage
    StoredProps.Strings = (
      'ViewPanel.Height'
      'SaveLogDialog.InitialDir'
      'SaveLogDialog.FileName')
    Left = 8
    Top = 160
  end
  object SaveLogDialog: TSaveDialog
    DefaultExt = 'txt'
    FileEditStyle = fsEdit
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoChangeDir]
    Title = 'Save Trace Log As'
    Left = 36
    Top = 160
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 64
    Top = 160
    object miPopupCopy: TMenuItem
      Caption = '&Copy'
      ShortCut = 0
      OnClick = miPopupCopyClick
    end
    object miPopupClear: TMenuItem
      Caption = 'C&lear Log'
      ShortCut = 0
      OnClick = miPopupClearClick
    end
    object miPopupSelectAll: TMenuItem
      Caption = 'Select &All'
      ShortCut = 0
      OnClick = miPopupSelectAllClick
    end
    object N1: TMenuItem
      Caption = '-'
      ShortCut = 0
    end
    object miSaveLog: TMenuItem
      Caption = '&Save Log...'
      ShortCut = 0
      OnClick = miSaveLogClick
    end
  end
end
