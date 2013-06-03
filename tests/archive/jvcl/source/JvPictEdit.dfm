object PictureEditDialog: TPictureEditDialog
  Left = 202
  Top = 102
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Picture Editor'
  ClientHeight = 335
  ClientWidth = 353
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object UsePreviewBox: TCheckBox
    Left = 7
    Top = 270
    Width = 339
    Height = 14
    Caption = ' &Use preview dialog box '
    TabOrder = 1
  end
  object OKButton: TButton
    Left = 105
    Top = 306
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object CancelButton: TButton
    Left = 188
    Top = 306
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object HelpBtn: TButton
    Left = 271
    Top = 306
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 5
    OnClick = HelpBtnClick
  end
  object GroupBox: TGroupBox
    Left = 7
    Top = 3
    Width = 339
    Height = 262
    TabOrder = 0
    object Bevel: TBevel
      Left = 9
      Top = 15
      Width = 237
      Height = 237
    end
    object PathsBtn: TJvSpeedButton
      Left = 307
      Top = 186
      Width = 22
      Height = 25
      DropDownMenu = PathsMenu
      Enabled = False
    end
    object ImagePanel: TPanel
      Left = 10
      Top = 16
      Width = 235
      Height = 235
      BevelOuter = bvNone
      BorderWidth = 5
      BorderStyle = bsSingle
      Color = clWindow
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 0
      object ImagePaintBox: TPaintBox
        Left = 5
        Top = 5
        Width = 221
        Height = 221
        Align = alClient
        OnPaint = ImagePaintBoxPaint
      end
    end
    object Load: TButton
      Left = 254
      Top = 16
      Width = 75
      Height = 25
      Caption = '&Load...'
      TabOrder = 1
      OnClick = LoadClick
    end
    object Save: TButton
      Left = 254
      Top = 49
      Width = 75
      Height = 25
      Caption = '&Save...'
      TabOrder = 2
      OnClick = SaveClick
    end
    object Clear: TButton
      Left = 254
      Top = 146
      Width = 75
      Height = 25
      Caption = 'Cl&ear'
      TabOrder = 5
      OnClick = ClearClick
    end
    object Copy: TButton
      Left = 254
      Top = 81
      Width = 75
      Height = 25
      Caption = '&Copy'
      TabOrder = 3
      OnClick = CopyClick
    end
    object Paste: TButton
      Left = 254
      Top = 114
      Width = 75
      Height = 25
      Caption = '&Paste'
      TabOrder = 4
      OnClick = PasteClick
    end
    object Paths: TButton
      Left = 254
      Top = 186
      Width = 53
      Height = 25
      Caption = 'P&aths'
      TabOrder = 6
      OnClick = PathsClick
    end
  end
  object DecreaseBox: TCheckBox
    Left = 7
    Top = 287
    Width = 339
    Height = 14
    Caption = ' &Decrease to 16 colors when paste '
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object FormStorage: TJvFormStorage
    IniFileName = 'DELPHI.INI'
    IniSection = 'RX.ImageEditor'
    Options = [fpPosition]
    OnSavePlacement = FormStorageSavePlacement
    OnRestorePlacement = FormStorageRestorePlacement
    StoredProps.Strings = (
      'UsePreviewBox.Checked'
      'DecreaseBox.Checked')
    StoredValues = <>
    Left = 32
    Top = 30
  end
  object PathsMenu: TPopupMenu
    Alignment = paRight
    OnPopup = PathsMenuPopup
    Left = 60
    Top = 30
  end
  object PathsMRU: TJvMRUManager
    Duplicates = dupIgnore
    Capacity = 30
    IniStorage = FormStorage
    ShowAccelChar = False
    OnChange = PathsMRUChange
    OnClick = PathsMRUClick
    Left = 88
    Top = 30
  end
end
