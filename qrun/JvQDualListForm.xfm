object JvDualListForm: TJvDualListForm
  Left = 198
  Top = 100
  ActiveControl = SrcList
  BorderIcons = []
  BorderStyle = bsSingle
  ClientHeight = 269
  ClientWidth = 392
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
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = ListClick
  OnCreate = FormCreate
  OnShow = ListClick
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 7
    Width = 384
    Height = 224
    ParentShowHint = False
    ShowHint = True
  end
  object SrcLabel: TLabel
    Left = 12
    Top = 12
    Width = 3
    Height = 13
  end
  object DstLabel: TLabel
    Left = 216
    Top = 12
    Width = 3
    Height = 13
  end
  object SrcList: TJvListBox
    Left = 12
    Top = 30
    Width = 164
    Height = 194
    DragMode = dmAutomatic
    ItemHeight = 13
    MultiSelect = True
    ParentShowHint = False
    ShowHint = True
    Sorted = True
    TabOrder = 0
    OnClick = ListClick
    OnDblClick = IncBtnClick
    OnDragDrop = SrcListDragDrop
    OnDragOver = SrcListDragOver
    OnKeyDown = SrcListKeyDown
    Background.FillMode = bfmTile
    Background.Visible = False
    ScrollBars = ssNone
  end
  object DstList: TJvListBox
    Left = 216
    Top = 30
    Width = 164
    Height = 194
    DragMode = dmAutomatic
    ItemHeight = 13
    MultiSelect = True
    ParentShowHint = False
    ShowHint = True
    Sorted = True
    TabOrder = 5
    OnClick = ListClick
    OnDblClick = ExclBtnClick
    OnDragDrop = DstListDragDrop
    OnDragOver = DstListDragOver
    OnKeyDown = DstListKeyDown
    Background.FillMode = bfmTile
    Background.Visible = False
    ScrollBars = ssNone
  end
  object IncBtn: TButton
    Left = 183
    Top = 32
    Width = 26
    Height = 26
    Caption = '>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = IncBtnClick
  end
  object IncAllBtn: TButton
    Left = 183
    Top = 64
    Width = 26
    Height = 26
    Caption = '>>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = IncAllBtnClick
  end
  object ExclBtn: TButton
    Left = 183
    Top = 97
    Width = 26
    Height = 26
    Caption = '<'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = ExclBtnClick
  end
  object ExclAllBtn: TButton
    Left = 183
    Top = 129
    Width = 26
    Height = 26
    Caption = '<<'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    OnClick = ExclAllBtnClick
  end
  object OkBtn: TButton
    Left = 130
    Top = 239
    Width = 77
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object CancelBtn: TButton
    Left = 213
    Top = 239
    Width = 77
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object HelpBtn: TButton
    Left = 310
    Top = 239
    Width = 77
    Height = 25
    Caption = 'Help'
    TabOrder = 8
    OnClick = HelpBtnClick
  end
end
