object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 589
  ClientWidth = 848
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TreeView1: TTreeView
    Left = 25
    Top = 75
    Width = 131
    Height = 136
    Indent = 19
    TabOrder = 0
    Items.NodeData = {
      01030000001B0000000000000000000000FFFFFFFFFFFFFFFF00000000020000
      000161001B0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000
      0162001B0000000000000000000000FFFFFFFFFFFFFFFF000000000000000001
      76001D0000000000000000000000FFFFFFFFFFFFFFFF00000000010000000276
      007600250000000000000000000000FFFFFFFFFFFFFFFF000000000000000006
      6100730064006100730064001F0000000000000000000000FFFFFFFFFFFFFFFF
      000000000100000003760032003300250000000000000000000000FFFFFFFFFF
      FFFFFF000000000000000006610073006400610073006400}
  end
  object StringGrid1: TStringGrid
    Left = 170
    Top = 75
    Width = 320
    Height = 120
    TabOrder = 1
  end
  object DBGrid1: TDBGrid
    Left = 170
    Top = 215
    Width = 320
    Height = 120
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object cxGrid1: TcxGrid
    Left = 496
    Top = 75
    Width = 250
    Height = 196
    TabOrder = 3
    object cxGrid1DBTableView1: TcxGridDBTableView
      NavigatorButtons.ConfirmDelete = False
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
    end
    object cxGrid1BandedTableView1: TcxGridBandedTableView
      NavigatorButtons.ConfirmDelete = False
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      Bands = <
        item
        end>
      object cxGrid1BandedTableView1Column1: TcxGridBandedColumn
        Position.BandIndex = 0
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object cxGrid1BandedTableView1Column2: TcxGridBandedColumn
        Position.BandIndex = 0
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
    end
    object cxGrid1Level1: TcxGridLevel
      GridView = cxGrid1BandedTableView1
    end
  end
  object cxDBPivotGrid1: TcxDBPivotGrid
    Left = 15
    Top = 350
    Width = 475
    Height = 205
    Groups = <>
    TabOrder = 4
    TabStop = True
  end
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 848
    Height = 48
    ActionManager = ActionManager1
    Caption = 'ActionToolBar1'
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = clWhite
    Spacing = 0
  end
  object cxVerticalGrid1: TcxVerticalGrid
    Left = 505
    Top = 277
    Width = 150
    Height = 200
    TabOrder = 6
  end
  object cxDBVerticalGrid1: TcxDBVerticalGrid
    Left = 661
    Top = 277
    Width = 150
    Height = 200
    TabOrder = 7
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = JvControlCollapseAction2
            Caption = '&JvControlCollapseAction2'
          end
          item
            Action = JvControlExpandAction1
            Caption = 'J&vControlExpandAction1'
          end
          item
            Action = JvControlExportAction1
            Caption = 'Jv&ControlExportAction1'
          end
          item
            Action = JvControlOptimizeColumnsAction1
            Caption = 'JvC&ontrolOptimizeColumnsAction1'
          end
          item
            Action = JvControlCustomizeColumnsAction1
            Caption = 'JvCo&ntrolCustomizeColumnsAction1'
          end
          item
            Action = JvControlPrintAction1
            Caption = 'JvCon&trolPrintAction1'
          end
          item
            Action = JvControlCustomizeAction1
            Caption = 'JvCont&rolCustomizeAction1'
          end>
        ActionBar = ActionToolBar1
      end>
    Left = 580
    Top = 425
    StyleName = 'XP Style'
    object JvControlCollapseAction2: TJvControlCollapseAction
      Category = 'JVCL'
      Caption = 'JvControlCollapseAction2'
    end
    object JvControlExpandAction1: TJvControlExpandAction
      Category = 'JVCL'
      Caption = 'JvControlExpandAction1'
    end
    object JvControlExportAction1: TJvControlExportAction
      Category = 'JVCL'
      Caption = 'JvControlExportAction1'
    end
    object JvControlOptimizeColumnsAction1: TJvControlOptimizeColumnsAction
      Category = 'JVCL'
      Caption = 'JvControlOptimizeColumnsAction1'
    end
    object JvControlCustomizeColumnsAction1: TJvControlCustomizeColumnsAction
      Category = 'JVCL'
      Caption = 'JvControlCustomizeColumnsAction1'
    end
    object JvControlPrintAction1: TJvControlPrintAction
      Category = 'JVCL'
      Caption = 'JvControlPrintAction1'
    end
    object JvControlCustomizeAction1: TJvControlCustomizeAction
      Category = 'JVCL'
      Caption = 'JvControlCustomizeAction1'
    end
  end
end
