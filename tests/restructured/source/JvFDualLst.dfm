object JvDualListForm: TJvDualListForm
  Left = 198
  Top = 100
  ActiveControl = SrcList
  BorderIcons = []
  BorderStyle = bsDialog
  ClientHeight = 269
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
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
    Width = 5
    Height = 13
  end
  object DstLabel: TLabel
    Left = 216
    Top = 12
    Width = 5
    Height = 13
  end
  object SrcList: TJvTextListBox
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
  end
  object DstList: TJvTextListBox
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
    Left = 138
    Top = 239
    Width = 77
    Height = 25
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object CancelBtn: TButton
    Left = 221
    Top = 239
    Width = 77
    Height = 25
    Cancel = True
    ModalResult = 2
    TabOrder = 7
  end
  object HelpBtn: TButton
    Left = 310
    Top = 239
    Width = 77
    Height = 25
    TabOrder = 8
    OnClick = HelpBtnClick
  end
end
