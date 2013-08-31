object JvCheckItemsEditor: TJvCheckItemsEditor
  Left = 285
  Top = 129
  Width = 350
  Height = 290
  BorderIcons = [biSystemMenu]
  Color = clBtnFace
  Constraints.MinHeight = 290
  Constraints.MinWidth = 350
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
    00000000000000000000000000000000000000000000C00C0000000000000000
    0000000000C00FF0C00000000000000000000000C00FF00F0C00000000000000
    000000C00FF00FF0F0C00000000000000000C00FF00FFFFB0F0C000000000000
    00C00FF00FFFFFFFF0F0C00000000000000FF00FFFFBF77FFF0F0C0000000000
    00000FFFFFF77FFFFFF0F0C000000000000FFFFBF77FFFFB77FF0F0C00000000
    0000FFF77FFFFF77FFFFF0F0C000000000000FFFFFFB77FFFFFBFF0F0C000000
    000000FFFF77FFFFFF88FFF0F0C000C0C000000BF7FFFFFB80FFFFFB0F0C0C0C
    00000000000FFF800FFFF88FF0F00CC007700FFFFFF000010FF88FFFFF000CC0
    7FFFFFFFFFF77700F88FFF8088F00CC0FFFFFFFFF00000088FFFFFF0F0000C10
    FFFFFFFF7000B00FFFFFFFF000000CC0FFFFFFFF700B070F8088F00000800000
    0FFFFFFF70B07F0FF0F0000800000000000FFFFF0B07F00BF000000000000000
    0000FFF0B0FF000000008000000000000000000B0FF000000800000000000000
    000000B000000000000000000000000000000B00000000000000000000000000
    0000B000000000000000000000000000000B0000000000000000000000000000
    00B000000000000000000000000000000B000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFF0FFFFFFC07FFFFF003FFFFC001FFFF0000FFFC00007FFC00003FFC00
    001FFC00000FFE000007FF000003C380000181C0000001800000000000000000
    000000000001000200050000000D80000063F800017FFE00637FFF00F8FFFF81
    FFFFFF1FFFFFFE3FFFFFFC7FFFFFF8FFFFFFF1FFFFFFF3FFFFFFFFFFFFFF0000}
  OldCreateOrder = True
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object OkBtn: TButton
    Left = 259
    Top = 205
    Width = 75
    Height = 24
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 259
    Top = 232
    Width = 75
    Height = 24
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object DownBtn: TButton
    Left = 259
    Top = 168
    Width = 75
    Height = 24
    Anchors = [akTop, akRight]
    Caption = 'Move do&wn'
    TabOrder = 2
    OnClick = UpDownBtnClick
  end
  object UpBtn: TButton
    Left = 259
    Top = 141
    Width = 75
    Height = 24
    Anchors = [akTop, akRight]
    Caption = 'Move &up'
    TabOrder = 3
    OnClick = UpDownBtnClick
  end
  object ClearBtn: TButton
    Left = 259
    Top = 114
    Width = 75
    Height = 24
    Anchors = [akTop, akRight]
    Caption = '&Clear all'
    TabOrder = 4
    OnClick = ClearBtnClick
  end
  object AddListBtn: TButton
    Left = 259
    Top = 87
    Width = 75
    Height = 24
    Anchors = [akTop, akRight]
    Caption = '&Add Strings'
    TabOrder = 5
    OnClick = AddListBtnClick
  end
  object DeleteBtn: TButton
    Left = 259
    Top = 60
    Width = 75
    Height = 24
    Anchors = [akTop, akRight]
    Caption = '&Delete'
    TabOrder = 6
    OnClick = DeleteBtnClick
  end
  object NewBtn: TButton
    Left = 259
    Top = 33
    Width = 75
    Height = 24
    Anchors = [akTop, akRight]
    Caption = '&New'
    TabOrder = 7
    OnClick = NewBtnClick
  end
  object EditBtn: TButton
    Left = 259
    Top = 6
    Width = 75
    Height = 24
    Anchors = [akTop, akRight]
    Caption = '&Edit'
    Default = True
    TabOrder = 8
    OnClick = EditBtnClick
  end
  object CheckList: TJvxCheckListBox
    Left = 6
    Top = 6
    Width = 246
    Height = 248
    AllowGrayed = True
    Ctl3D = True
    DragMode = dmAutomatic
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    ParentCtl3D = False
    PopupMenu = Popup
    TabOrder = 9
    OnClick = CheckListClick
    OnDragDrop = CheckListDragDrop
    OnDragOver = CheckListDragOver
    OnKeyDown = CheckListKeyDown
    InternalVersion = 202
  end
  object Popup: TPopupMenu
    OnPopup = PopupPopup
    Left = 24
    Top = 16
    object cbCheckedItem: TMenuItem
      Caption = 'cbC&hecked'
      OnClick = cbCheckedItemClick
    end
    object cbGrayedItem: TMenuItem
      Caption = 'cb&Grayed'
      OnClick = cbGrayedItemClick
    end
    object cbUncheckedItem: TMenuItem
      Caption = 'cb&Unchecked'
      OnClick = cbUncheckedItemClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object EnabledItem: TMenuItem
      Caption = '&Enabled'
      OnClick = EnabledItemClick
    end
  end
  object FormPlacement: TJvFormStorage
    StoredValues = <>
    Left = 88
    Top = 16
  end
end
