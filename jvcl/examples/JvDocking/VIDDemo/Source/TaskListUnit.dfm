object TaskListForm: TTaskListForm
  Left = 381
  Top = 211
  Width = 486
  Height = 270
  BorderStyle = bsSizeToolWin
  Caption = 'Task List'
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 478
    Height = 243
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 0
    object StringGrid1: TStringGrid
      Left = 2
      Top = 2
      Width = 474
      Height = 239
      Align = alClient
      BorderStyle = bsNone
      ColCount = 6
      DefaultRowHeight = 16
      FixedCols = 0
      RowCount = 100
      ScrollBars = ssHorizontal
      TabOrder = 0
      ColWidths = (
        16
        18
        17
        199
        131
        134)
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
