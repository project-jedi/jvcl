object JvSpeedbarEditor: TJvSpeedbarEditor
  Left = 222
  Top = 126
  ActiveControl = SectionList
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'JvSpeedbar Designer'
  ClientHeight = 277
  ClientWidth = 437
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
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
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SectionNameLabel: TLabel
    Left = 180
    Top = 10
    Width = 86
    Height = 13
    Caption = '&Section name: '
    FocusControl = SectionName
  end
  object LabelHint: TLabel
    Left = 4
    Top = 248
    Width = 429
    Height = 29
    AutoSize = False
    Caption = 
      'To add command buttons, drag and drop button onto the JvSpeedbar' +
      '. To remove command buttons, drag them off of the JvSpeedbar.'
    WordWrap = True
  end
  object SectionsBox: TGroupBox
    Left = 4
    Top = 3
    Width = 165
    Height = 240
    Caption = ' Sections '
    TabOrder = 0
    object NewSection: TButton
      Left = 9
      Top = 208
      Width = 69
      Height = 25
      Caption = '&New'
      TabOrder = 0
      OnClick = NewSectionClick
    end
    object DelSection: TButton
      Left = 85
      Top = 208
      Width = 69
      Height = 25
      Caption = '&Delete'
      Enabled = False
      TabOrder = 1
      OnClick = DelSectionClick
    end
    object SectionList: TDrawGrid
      Left = 8
      Top = 16
      Width = 149
      Height = 185
      ColCount = 1
      DefaultColWidth = 147
      DefaultRowHeight = 15
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Options = [goDrawFocusSelected, goRowSelect]
      ScrollBars = ssVertical
      TabOrder = 2
      OnDragDrop = SectionListDragDrop
      OnDragOver = SectionListDragOver
      OnDrawCell = SectionListDrawCell
      OnKeyDown = SectionListKeyDown
      OnMouseDown = SectionListMouseDown
      OnSelectCell = SectionListSelectCell
    end
  end
  object ButtonsBox: TGroupBox
    Left = 172
    Top = 30
    Width = 262
    Height = 213
    Caption = ' Available buttons '
    TabOrder = 2
    object UpBtn: TSpeedButton
      Left = 232
      Top = 68
      Width = 24
      Height = 25
      Hint = 'Move up|'
      Enabled = False
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD00000DD
        DDDDDDDDD04440DDDDDDD0000044400000DDDD04444444440DDDDDD044444440
        DDDDDDDD0444440DDDDDDDDDD04440DDDDDDDDDDDD040DDDDDDDDDDDDDD0DDDD
        DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD}
      ParentShowHint = False
      ShowHint = True
      OnClick = UpBtnClick
    end
    object DownBtn: TSpeedButton
      Left = 232
      Top = 100
      Width = 24
      Height = 25
      Hint = 'Move Down|'
      Enabled = False
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD0DDDDDDDDDDDDDD040DDD
        DDDDDDDDD04440DDDDDDDDDD0444440DDDDDDDD044444440DDDDDD0444444444
        0DDDD0000044400000DDDDDDD04440DDDDDDDDDDD00000DDDDDDDDDDDDDDDDDD
        DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD}
      ParentShowHint = False
      ShowHint = True
      OnClick = DownBtnClick
    end
    object AddButton: TButton
      Left = 8
      Top = 181
      Width = 69
      Height = 25
      Caption = '&Add'
      Enabled = False
      TabOrder = 1
      OnClick = AddButtonClick
    end
    object RemoveButton: TButton
      Left = 85
      Top = 181
      Width = 69
      Height = 25
      Caption = '&Remove'
      Enabled = False
      TabOrder = 2
      OnClick = RemoveButtonClick
    end
    object CloseBtn: TButton
      Left = 185
      Top = 181
      Width = 69
      Height = 25
      Caption = '&Close'
      TabOrder = 3
      OnClick = CloseBtnClick
    end
    object ButtonsList: TDrawGrid
      Left = 8
      Top = 16
      Width = 219
      Height = 158
      ColCount = 1
      Ctl3D = True
      DefaultColWidth = 215
      DefaultRowHeight = 26
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Options = [goDrawFocusSelected, goRowSelect]
      ParentCtl3D = False
      PopupMenu = PopupMenu
      ScrollBars = ssVertical
      TabOrder = 0
      OnDblClick = ButtonsListDblClick
      OnDrawCell = ButtonsListDrawCell
      OnKeyDown = ButtonsListKeyDown
      OnMouseDown = ButtonsListMouseDown
      OnMouseMove = ButtonsListMouseMove
      OnMouseUp = ButtonsListMouseUp
      OnSelectCell = ButtonsListSelectCell
    end
  end
  object SectionName: TEdit
    Left = 268
    Top = 7
    Width = 165
    Height = 21
    MaxLength = 255
    TabOrder = 1
    OnExit = SectionNameExit
    OnKeyDown = SectionNameKeyDown
  end
  object PopupMenu: TPopupMenu
    Left = 185
    Top = 78
    object CutMenu: TMenuItem
      Caption = 'Cut'
      ShortCut = 8238
      OnClick = CutMenuClick
    end
    object CopyMenu: TMenuItem
      Caption = 'Copy'
      ShortCut = 16429
      OnClick = CopyMenuClick
    end
    object PasteMenu: TMenuItem
      Caption = 'Paste'
      ShortCut = 8237
      OnClick = PasteMenuClick
    end
  end
end
