object RenameDialog: TRenameDialog
  Left = 273
  Top = 175
  ActiveControl = TabnameEdit
  BorderStyle = bsDialog
  Caption = 'Rename Table'
  ClientHeight = 132
  ClientWidth = 331
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 7
    Top = 49
    Width = 92
    Height = 13
    Caption = '&New Table Name:  '
    FocusControl = TabnameEdit
  end
  object Label2: TLabel
    Left = 7
    Top = 8
    Width = 104
    Height = 13
    Caption = 'Current Table Name:  '
  end
  object OKBtn: TButton
    Left = 167
    Top = 97
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 247
    Top = 97
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object TabnameEdit: TJvFilenameEdit
    Left = 7
    Top = 64
    Width = 315
    Height = 21
    AddQuotes = False
    DefaultExt = 'db'
    Filter = 
      'Paradox or DBase files (*.db; *.dbf)|*.DB;*.DBF|ASCII files (*.t' +
      'xt)|*.TXT|All files (*.*)|*.*'
    DialogOptions = [ofHideReadOnly, ofPathMustExist]
    DialogTitle = 'Browse files'
    ButtonHint = 'Browse files|'
    ButtonFlat = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object TabnameView: TEdit
    Left = 7
    Top = 24
    Width = 315
    Height = 21
    ParentColor = True
    ReadOnly = True
    TabOrder = 3
  end
end
