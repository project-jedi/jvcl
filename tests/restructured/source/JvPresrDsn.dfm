object JvFormPropsDlg: TJvFormPropsDlg
  Left = 200
  Top = 111
  BorderStyle = bsDialog
  Caption = 'Form Storage Designer'
  ClientHeight = 319
  ClientWidth = 352
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000FF
    FFF000000FFFFF000000FFFFFF00000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000008888800000000000000000
    0000000008800088000000000000000000000000880FFF088000000000000000
    0770000080FFFFF080000000000000000770000080FFFFF08000000000000000
    07700000880FFF08800000000000000000000000088000880000000000000000
    0000000000888880000000000000000000000000000000000000000000000000
    0000000007000007000000000000000777777777777777777777777000000000
    0000000000000000000000000000000000000000070000070000000000000000
    0000000000700070000000000000000000000000000777000000000000000000
    000000000000000000000000000000000000000000000000000000000000C1F8
    3F03F3FC7F9FF1FC7F1FF8FC7E3FFC7C7C7FFE3C78FFFF1C71FFFF8C63FFFFC4
    47FFFFE00FFFFFF01FFFFFF83FFFC000000F8000000780000007800000078000
    0007800000078000000780000007800000078000000780000007800000078000
    000780000007C000000F8120021F8130061FFFF80FFFFFFC1FFFFFFFFFFF}
  OldCreateOrder = True
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 49
    Width = 344
    Height = 236
    Shape = bsFrame
  end
  object Label30: TLabel
    Left = 12
    Top = 53
    Width = 82
    Height = 13
    Caption = '&Components   '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    IsControl = True
  end
  object Label31: TLabel
    Left = 224
    Top = 53
    Width = 70
    Height = 13
    Caption = '&Properties   '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    IsControl = True
  end
  object Label2: TLabel
    Left = 12
    Top = 169
    Width = 111
    Height = 13
    Caption = '&Stored Properties   '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object UpBtn: TSpeedButton
    Left = 272
    Top = 186
    Width = 25
    Height = 25
    Glyph.Data = {
      DE000000424DDE0000000000000076000000280000000D0000000D0000000100
      0400000000006800000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3000333333333333300033330000033330003333066603333000333306660333
      3000333306660333300030000666000030003306666666033000333066666033
      3000333306660333300033333060333330003333330333333000333333333333
      3000}
    OnClick = UpBtnClick
  end
  object DownBtn: TSpeedButton
    Left = 305
    Top = 186
    Width = 25
    Height = 25
    Glyph.Data = {
      DE000000424DDE0000000000000076000000280000000D0000000D0000000100
      0400000000006800000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3000333333333333300033333303333330003333306033333000333306660333
      3000333066666033300033066666660330003000066600003000333306660333
      3000333306660333300033330666033330003333000003333000333333333333
      3000}
    OnClick = DownBtnClick
  end
  object FormBox: TGroupBox
    Left = 4
    Top = 2
    Width = 344
    Height = 44
    Caption = ' Form Properties '
    TabOrder = 0
    object ActiveCtrlBox: TCheckBox
      Left = 12
      Top = 16
      Width = 94
      Height = 17
      Caption = ' Acti&ve Control'
      TabOrder = 0
    end
    object PositionBox: TCheckBox
      Left = 124
      Top = 16
      Width = 94
      Height = 17
      Caption = ' &Form Position'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object StateBox: TCheckBox
      Left = 236
      Top = 16
      Width = 94
      Height = 17
      Caption = ' &Window State'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object AddButton: TButton
    Left = 137
    Top = 102
    Width = 77
    Height = 25
    Caption = '&Add'
    TabOrder = 3
    OnClick = AddButtonClick
  end
  object DeleteButton: TButton
    Left = 263
    Top = 218
    Width = 77
    Height = 25
    Caption = '&Delete'
    TabOrder = 5
    OnClick = DeleteButtonClick
  end
  object ClearButton: TButton
    Left = 263
    Top = 248
    Width = 77
    Height = 25
    Caption = 'Cl&ear'
    TabOrder = 1
    OnClick = ClearButtonClick
  end
  object OkBtn: TButton
    Left = 184
    Top = 290
    Width = 77
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 268
    Top = 290
    Width = 77
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object ComponentsList: TJvTextListBox
    Left = 16
    Top = 72
    Width = 105
    Height = 89
    ItemHeight = 13
    TabOrder = 6
  end
  object PropertiesList: TJvTextListBox
    Left = 224
    Top = 72
    Width = 113
    Height = 97
    ItemHeight = 13
    TabOrder = 7
  end
  object StoredList: TJvTextListBox
    Left = 16
    Top = 184
    Width = 241
    Height = 97
    ItemHeight = 13
    TabOrder = 8
  end
end
