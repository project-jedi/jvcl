object BlobViewDlg: TBlobViewDlg
  Left = 215
  Top = 129
  ActiveControl = CancelBtn
  BorderStyle = bsDialog
  ClientHeight = 276
  ClientWidth = 476
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultPosOnly
  ShowHint = True
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel: TBevel
    Left = 6
    Top = 8
    Width = 465
    Height = 229
    Shape = bsFrame
  end
  object SizeLabel: TLabel
    Left = 16
    Top = 16
    Width = 3
    Height = 13
  end
  object CancelBtn: TBitBtn
    Left = 392
    Top = 244
    Width = 77
    Height = 25
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 4
    Style = bsNew
    Spacing = -1
  end
  object DBNavigator: TDBNavigator
    Left = 373
    Top = 14
    Width = 88
    Height = 21
    DataSource = DataSource
    VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast]
    TabOrder = 0
  end
  object HexPanel: TPanel
    Left = 12
    Top = 39
    Width = 454
    Height = 193
    BevelOuter = bvNone
    TabOrder = 1
  end
  object ClearBtn: TBitBtn
    Left = 307
    Top = 244
    Width = 77
    Height = 25
    Caption = '&Clear'
    TabOrder = 3
    OnClick = ClearBtnClick
    Glyph.Data = {
      46010000424D460100000000000076000000280000001C0000000D0000000100
      040000000000D000000000000000000000000000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333000033333333333333333FF33333FF330000333003333300
      3333833F333833F3000033300033300033338333F38333F30000333300030003
      3333383338333F330000333330000033333333833333F3330000333333000333
      33333338333F33330000333330000033333333833333F3330000333300030003
      3333383338333F33000033300033300033338333F38333F30000333003333300
      3333833F333833F3000033333333333333333883333388330000333333333333
      33333333333333330000}
    Margin = 6
    NumGlyphs = 2
    Style = bsNew
    Spacing = -1
  end
  object AddressBox: TCheckBox
    Left = 8
    Top = 248
    Width = 129
    Height = 17
    Caption = 'Show Address'
    TabOrder = 2
    OnClick = AddressBoxClick
  end
  object DataSource: TDataSource
    OnDataChange = DataSourceDataChange
    Left = 240
    Top = 4
  end
  object FormStorage: TJvFormStorage
    StoredProps.Strings = (
      'AddressBox.Checked')
    Left = 268
    Top = 4
  end
end
