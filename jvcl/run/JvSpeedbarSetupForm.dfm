object JvSpeedbarSetupWindow: TJvSpeedbarSetupWindow
  Left = 231
  Top = 121
  ActiveControl = SectionList
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 262
  ClientWidth = 421
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000000020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000088000000000000000000000000000000088000000
    0000000000000000000000FF080000000000000000000000000000FF08000000
    000000000000000008000FF080000000000000000000000000880FF000000000
    00000000000000000F00FF080000000000000000000000000FF0FF0888000000
    00008888888888880FFFFF000000000000000000000000000FFFFFFF00000000
    00088888888888880FFFFFF000000000000F7777777777770FFFFF0000000000
    000F7777777777770FFFF08888000000000F7777000000070FFF000008800000
    000F77770FFFFF070FF0F7F7F0800088880F77770FFFFF070F00FFFF70800000
    000F77770FFFFF07008088F7F0800888880F77770FFFFF077780F8FF70800F77
    770F77770FF000077780F8F7F0800F77770F77770FF0F0777780F8FF70800F77
    770F77770FF007777780F8F7F0800F77770F7777000077777780887F70800F77
    770F777777777777778087F7F0800F77770FFFFFFFFFFFFFFF807F7F70800F77
    77800000000000000007F7F7F0800F77778FF888087F7F7F7F7F7F7F70800F77
    778FF8F808F7F7F7F7F7F7F7F0800F77778FF887088888888888888880000F77
    77888877700000000000000000000F7777777777777780800000000000000FFF
    FFFFFFFFFFFF800000000000000000000000000000000000000000000000FFFF
    FFE7FFFFFFC3FFFFFF83FFFFFF83FFFFF307FFFFF00FFFFFF00FFFFFF003FF00
    0003FE000007FC00000FFC00001FFC000003FC000001FC000001C00000018000
    0001000000010000000100000001000000010000000100000001000000010000
    00010000000100000001000000030000000700001FFF00003FFF80007FFFBA00}
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 4
    Width = 328
    Height = 221
    Shape = bsFrame
  end
  object ButtonsLabel: TLabel
    Left = 146
    Top = 11
    Width = 96
    Height = 13
    Caption = '&Available buttons:    '
    FocusControl = ButtonsList
  end
  object CategoriesLabel: TLabel
    Left = 12
    Top = 11
    Width = 65
    Height = 13
    Caption = '&Categories:    '
  end
  object HintLabel: TLabel
    Left = 4
    Top = 231
    Width = 413
    Height = 29
    AutoSize = False
    Caption = 
      'To add command buttons, drag and drop buttons onto the JvSpeedba' +
      'r. To remove command buttons, drag them off of the JvSpeedbar.'
    WordWrap = True
  end
  object ButtonsList: TDrawGrid
    Left = 146
    Top = 30
    Width = 179
    Height = 188
    ColCount = 1
    DefaultColWidth = 169
    DefaultRowHeight = 26
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goDrawFocusSelected, goRowSelect]
    ScrollBars = ssVertical
    TabOrder = 1
    OnDrawCell = ButtonsListDrawCell
    OnMouseDown = ButtonsListMouseDown
    OnMouseMove = ButtonsListMouseMove
    OnMouseUp = ButtonsListMouseUp
    OnSelectCell = ButtonsListSelectCell
  end
  object SectionList: TDrawGrid
    Left = 12
    Top = 30
    Width = 129
    Height = 188
    ColCount = 1
    DefaultColWidth = 127
    DefaultRowHeight = 15
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goDrawFocusSelected, goRowSelect]
    ScrollBars = ssVertical
    TabOrder = 0
    OnDrawCell = SectionListDrawCell
    OnSelectCell = SectionListSelectCell
  end
  object CloseBtn: TButton
    Left = 339
    Top = 12
    Width = 77
    Height = 25
    Cancel = True
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = CloseBtnClick
  end
  object HelpBtn: TButton
    Left = 339
    Top = 44
    Width = 77
    Height = 25
    TabOrder = 3
    OnClick = HelpBtnClick
  end
end
