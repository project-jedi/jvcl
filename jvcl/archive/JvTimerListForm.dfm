object JvTimerItemsEditor: TJvTimerItemsEditor
  Left = 352
  Top = 190
  Width = 253
  Height = 155
  BorderIcons = [biSystemMenu]
  Color = clBtnFace
  Constraints.MinHeight = 155
  Constraints.MinWidth = 253
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000000020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000008888888880000000000000000
    000008877777777788000000000000000008877FFFF9FFFFFF88000000000000
    00877FFFFFFFFFFFFFF8800000000000087FF9FFFFFFFFFFF9FF880000000000
    87F9FFFFFFFFFFFFFFFFF8800000000087FF9FFFFFFFFFFFFFFFFF8000000008
    7FFFF9FFFFFFFFFFFFFFFF88000000087F9FFF9FFFFFFFFFFFFF9FF800000087
    FFFFFFF9FFFFFFFFFFFFFFF880000087FFFFFFFF9FFFFFFFFFFFFFF880000087
    FFFFFFFFF9FFFFFFFFFFFFFF80000087FFFFFFFFFF000FFFFFFFFFFF80000087
    9FFFFFFFFF000FFFFFFFFF9F80000087FFFFFFFFFF000FFFFFFFFFFF80000087
    FFFFFFFFF0F0FFFFFFFFFFF880000087FFFFFFFF0FF0FFFFFFFFFFF880000008
    7FFFFFF0FFF0FFFFFFFFFFF8000000087FFFFF0FFFF0FFFFFFFF9F8800000000
    879FF0FFFFF0FFFFFFFFFF800000000087FF0FFFFFFFFFFFFFFFF88000000000
    087FFFFFFFFFFFFFFFFF8800000000008087FF9FFFFFFFFF9FF8808000000078
    080887FFFFFFFFFFF8880708800000F78000088FFFF9FFFF880000778000000F
    8000000777777777000000F70000000000000000000000000000000000000000
    00000000007880000000000000000000000000000F0708000000000000000000
    000000000F07080000000000000000000000000000000000000000000000FFE0
    0FFFFF8003FFFE0000FFFC00007FF800003FF000001FE000000FE000000FC000
    0007C00000078000000380000003800000038000000380000003800000038000
    000380000003C0000007C0000007E000000FE000000FF000001FC00000078000
    000382000083C3800387E7E00FCFFFF83FFFFFF01FFFFFF01FFFFFF83FFF0000}
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object BtnPanel: TPanel
    Left = 159
    Top = 0
    Width = 86
    Height = 128
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object NewBtn: TButton
      Left = 4
      Top = 5
      Width = 77
      Height = 25
      Caption = '&New'
      TabOrder = 0
      OnClick = NewClick
    end
    object DeleteBtn: TButton
      Left = 4
      Top = 35
      Width = 77
      Height = 25
      Caption = '&Delete'
      TabOrder = 1
      OnClick = DeleteClick
    end
    object ClearBtn: TButton
      Left = 4
      Top = 65
      Width = 77
      Height = 25
      Caption = 'C&lear'
      TabOrder = 2
      OnClick = ClearBtnClick
    end
    object Panel1: TPanel
      Left = 0
      Top = 94
      Width = 86
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      object CloseBtn: TButton
        Left = 4
        Top = 4
        Width = 77
        Height = 25
        Caption = '&Close'
        TabOrder = 0
        OnClick = CloseBtnClick
      end
    end
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 159
    Height = 128
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 1
    object DrawGrid: TDrawGrid
      Left = 2
      Top = 2
      Width = 155
      Height = 124
      Align = alClient
      ColCount = 1
      DefaultRowHeight = 16
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goRowSelect]
      PopupMenu = PopupMenu
      ScrollBars = ssVertical
      TabOrder = 0
      OnDrawCell = DrawGridDrawCell
      OnKeyDown = DrawGridKeyDown
      OnSelectCell = DrawGridSelectCell
      ColWidths = (
        190)
    end
  end
  object FormStorage: TJvFormStorage
    RegistryRoot = hkClassesRoot
    StoredValues = <>
    Left = 88
    Top = 32
  end
  object PopupMenu: TPopupMenu
    Left = 24
    Top = 32
    object CutMenu: TMenuItem
      Caption = '&Cut'
      ShortCut = 8238
      OnClick = CutClick
    end
    object CopyMenu: TMenuItem
      Caption = 'Co&py'
      ShortCut = 16429
      OnClick = CopyClick
    end
    object PasteMenu: TMenuItem
      Caption = '&Paste'
      ShortCut = 8237
      OnClick = PasteClick
    end
    object DeleteMenu: TMenuItem
      Caption = '&Delete'
      ShortCut = 16430
      OnClick = DeleteClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object NewMenu: TMenuItem
      Caption = '&New Item'
      ShortCut = 45
      OnClick = NewClick
    end
  end
end
