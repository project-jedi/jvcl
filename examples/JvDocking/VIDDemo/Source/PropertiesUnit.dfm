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
  Font.Name = #203#206#204#229
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 249
    Height = 56
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = PanelResize
    object ToolBar1: TToolBar
      Left = 0
      Top = 32
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
        ImageIndex = 23
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
        ImageIndex = 20
      end
    end
    object ComboBox: TComboBox
      Left = 1
      Top = 2
      Width = 253
      Height = 22
      ItemHeight = 14
      TabOrder = 1
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 56
    Width = 249
    Height = 211
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 1
    object StringGrid: TStringGrid
      Left = 2
      Top = 2
      Width = 245
      Height = 207
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
    EnableCloseButton = True
    DockStyle = MainForm.JvDockVIDStyle1
    Left = 64
    Top = 88
  end
end
