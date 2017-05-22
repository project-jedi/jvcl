object PictEditDlg: TPictEditDlg
  Left = 497
  Top = 165
  BorderStyle = bsDialog
  ClientHeight = 279
  ClientWidth = 476
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefaultPosOnly
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel: TBevel
    Left = 6
    Top = 8
    Width = 465
    Height = 229
    Shape = bsFrame
  end
  object OkBtn: TBitBtn
    Left = 308
    Top = 244
    Width = 77
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = OkBtnClick
    Glyph.Data = {
      46010000424D460100000000000076000000280000001C0000000D0000000100
      040000000000D000000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33333333333333330000333333333333333333F3333333330000333303333333
      33333F3F3333333300003330003333333333F333F33333330000330000033333
      333F33333F33333300003000300033333383333333F333330000300333000333
      338333F8333F33330000333333300033333888338333F3330000333333330003
      3333333338333F33000033333333300033333333338333F30000333333333300
      3333333333383383000033333333333333333333333388330000333333333333
      33333333333333330000}
    Margin = 4
    NumGlyphs = 2
    Spacing = -1
  end
  object CancelBtn: TBitBtn
    Left = 392
    Top = 244
    Width = 77
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    Glyph.Data = {
      46010000424D460100000000000076000000280000001C0000000D0000000100
      040000000000D000000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333000033333333333333333FF33333FF330000333003333300
      3333833F333833F3000033300033300033338333F38333F30000333300030003
      3333383338333F330000333330000033333333833333F3330000333333000333
      33333338333F33330000333330000033333333833333F3330000333300030003
      3333383338333F33000033300033300033338333F38333F30000333003333300
      3333833F333833F3000033333333333333333883333388330000333333333333
      33333333333333330000}
    Margin = 4
    NumGlyphs = 2
    Spacing = -1
  end
  object LoadBtn: TBitBtn
    Left = 8
    Top = 244
    Width = 77
    Height = 25
    Caption = '&Load...'
    TabOrder = 0
    OnClick = FileOpen
  end
  object SaveBtn: TBitBtn
    Left = 92
    Top = 244
    Width = 77
    Height = 25
    Caption = '&Save as...'
    TabOrder = 1
    OnClick = SaveBtnClick
  end
  object DBNavigator: TDBNavigator
    Left = 373
    Top = 14
    Width = 84
    Height = 21
    DataSource = DataSource
    VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast]
    TabOrder = 4
  end
  object StretchBox: TCheckBox
    Left = 16
    Top = 16
    Width = 181
    Height = 17
    Caption = ' S&tretch bitmap'
    TabOrder = 5
    OnClick = StretchBoxClick
  end
  object DBImage: TDBImage
    Left = 12
    Top = 40
    Width = 453
    Height = 189
    DataSource = DataSource
    PopupMenu = PopupMenu
    TabOrder = 6
  end
  object DataSource: TDataSource
    Left = 240
    Top = 4
  end
  object PopupMenu: TPopupMenu
    Left = 212
    Top = 4
    object Cut1: TMenuItem
      Tag = 1
      Caption = 'Cut'
      OnClick = MenuClick
    end
    object Copy1: TMenuItem
      Tag = 2
      Caption = 'Copy'
      OnClick = MenuClick
    end
    object Paste1: TMenuItem
      Tag = 3
      Caption = 'Paste'
      OnClick = MenuClick
    end
    object N1: TMenuItem
      Caption = '-'
      OnClick = MenuClick
    end
    object Clear1: TMenuItem
      Tag = 4
      Caption = 'Clear'
      OnClick = MenuClick
    end
  end
end
