object StrEditDlg: TStrEditDlg
  Left = 439
  Top = 330
  ActiveControl = Memo
  BorderStyle = bsDialog
  Caption = 'String list editor'
  ClientHeight = 278
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
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
    Left = 364
  end
  object SaveDialog: TSaveDialog
    Filter = 
      'Text files (*.TXT)|*.TXT|Config files (*.SYS;*.INI)|*.SYS;*.INI|' +
      'Batch files (*.BAT)|*.BAT|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofShowHelp, ofPathMustExist]
    Title = 'Save string list'
    Left = 392
  end
end
