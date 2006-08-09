object JvStrEditDlg: TJvStrEditDlg
  Left = 381
  Top = 76
  ActiveControl = Memo
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'String list editor'
  ClientHeight = 274
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
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
  PixelsPerInch = 96
  TextHeight = 13
  object BevelBorder: TBevel
    Left = 8
    Top = 8
    Width = 413
    Height = 229
    Shape = bsFrame
  end
  object LineCount: TLabel
    Left = 12
    Top = 12
    Width = 169
    Height = 17
    AutoSize = False
    Caption = '0 lines'
  end
  object Memo: TMemo
    Left = 16
    Top = 28
    Width = 397
    Height = 201
    ScrollBars = ssBoth
    TabOrder = 0
    OnChange = UpdateStatus
    OnKeyDown = MemoKeyDown
  end
  object OKBtn: TButton
    Left = 187
    Top = 245
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object CancelBtn: TButton
    Left = 267
    Top = 245
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object HelpBtn: TButton
    Left = 347
    Top = 245
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 5
    OnClick = HelpBtnClick
  end
  object LoadBtn: TButton
    Left = 8
    Top = 245
    Width = 75
    Height = 25
    Caption = '&Load...'
    TabOrder = 1
    OnClick = FileOpen
  end
  object SaveBtn: TButton
    Left = 92
    Top = 245
    Width = 75
    Height = 25
    Caption = '&Save...'
    TabOrder = 2
    OnClick = FileSave
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'TXT'
    Filter = 
      'Text files (*.TXT)|*.TXT|Config files (*.SYS;*.INI)|*.SYS;*.INI|' +
      'Batch files (*.BAT)|*.BAT|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist]
    Title = 'Load string list'
    Left = 292
  end
  object SaveDialog: TSaveDialog
    Filter = 
      'Text files (*.TXT)|*.TXT|Config files (*.SYS;*.INI)|*.SYS;*.INI|' +
      'Batch files (*.BAT)|*.BAT|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofShowHelp, ofPathMustExist]
    Title = 'Save string list'
    Left = 360
  end
end
