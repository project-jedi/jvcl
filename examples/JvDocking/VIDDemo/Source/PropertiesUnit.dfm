object PropertiesForm: TPropertiesForm
  Left = 265
  Top = 98
  Width = 257
  Height = 294
  BorderStyle = bsSizeToolWin
  Caption = 'Properties'
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'ËÎÌו'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 12
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 249
    Height = 48
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = PanelResize
    object ToolBar1: TToolBar
      Left = 0
      Top = 24
      Width = 249
      Height = 24
      Align = alBottom
      Caption = 'ToolBar1'
      EdgeBorders = [ebLeft, ebTop, ebRight, ebBottom]
      EdgeOuter = esNone
      Flat = True
      Images = MainForm.ImageList1
      TabOrder = 0
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        Action = MainForm.Action_Open
      end
      object ToolButton3: TToolButton
        Left = 23
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object ToolButton2: TToolButton
        Left = 31
        Top = 0
        Action = MainForm.Action_Properties_Window
      end
    end
    object ComboBox: TComboBox
      Left = 1
      Top = 2
      Width = 217
      Height = 20
      ItemHeight = 12
      TabOrder = 1
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 48
    Width = 249
    Height = 218
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 1
    object StringGrid: TStringGrid
      Left = 2
      Top = 2
      Width = 245
      Height = 214
      Align = alClient
      BorderStyle = bsNone
      ColCount = 2
      DefaultRowHeight = 14
      FixedCols = 0
      RowCount = 100
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowMoving, goColMoving, goEditing]
      TabOrder = 0
      ColWidths = (
        64
        140)
    end
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
