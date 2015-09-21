object JvID3FramesEditor: TJvID3FramesEditor
  Left = 214
  Top = 191
  BorderStyle = bsSizeToolWin
  Caption = 'Frame Editor'
  ClientHeight = 239
  ClientWidth = 173
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100001001000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000BBBB0000000000BB000BB000000000BB0000B000000000B
    BB000BB00000000BBB000BB00000000000000BB00000000000000BB000000000
    00000BB00000000000000BB00000000000000BB00000000000000BB000000000
    00000BB0000000000000BBBB00000000000BBBBBB0000000000000000000FFFF
    0000F87F0000E73F0000E7BF0000E39F0000E39F0000FF9F0000FF9F0000FF9F
    0000FF9F0000FF9F0000FF9F0000FF9F0000FF0F0000FE070000FFFF0000}
  OldCreateOrder = False
  PopupMenu = LocalMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object FrameListBox: TListBox
    Left = 0
    Top = 0
    Width = 173
    Height = 239
    Align = alClient
    DragMode = dmAutomatic
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
    OnClick = ListBoxClick
    OnDragDrop = ListBoxDragDrop
    OnDragOver = ListBoxDragOver
    OnKeyDown = ListBoxKeyDown
  end
  object LocalMenu: TPopupMenu
    HelpContext = 30130
    Left = 42
    Top = 32
    object NewItem: TMenuItem
      Caption = '&New frame...'
      HelpContext = 30132
      ShortCut = 16462
      OnClick = NewFrameClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object DeleteItem: TMenuItem
      Caption = '&Delete'
      HelpContext = 30136
      ShortCut = 46
      OnClick = DeleteClick
    end
    object SelectAllItem: TMenuItem
      Caption = 'Se&lect all'
      HelpContext = 30137
      ShortCut = 16460
      OnClick = SelectAllClick
    end
  end
end
