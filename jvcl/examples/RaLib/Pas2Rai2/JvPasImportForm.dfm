object JvPasImport: TJvPasImport
  Left = 277
  Top = 122
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Importing pas-file to JvI2'
  ClientHeight = 400
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  ShowHint = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 50
    Height = 13
    Caption = 'Source file'
  end
  object Label2: TLabel
    Left = 16
    Top = 56
    Width = 69
    Height = 13
    Caption = 'Destination file'
  end
  object Label3: TLabel
    Left = 16
    Top = 105
    Width = 298
    Height = 13
    Caption = 'Press '#39'Read Classes'#39', select classes and press '#39'GO!'#39
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 16
    Top = 128
    Width = 36
    Height = 13
    Caption = 'Classes'
  end
  object eSource: TEdit
    Left = 16
    Top = 24
    Width = 297
    Height = 21
    TabOrder = 0
    Text = 'C:\Delphi\Source\VCL\Classes.pas'
    OnChange = eSourceChange
  end
  object bSource: TButton
    Left = 320
    Top = 24
    Width = 25
    Height = 23
    Caption = '...'
    TabOrder = 1
    OnClick = bSourceClick
  end
  object eDestination: TEdit
    Left = 16
    Top = 72
    Width = 297
    Height = 21
    TabOrder = 2
    Text = 'C:\Temp\JvI2_Classes.pas'
  end
  object bDestination: TButton
    Left = 320
    Top = 72
    Width = 25
    Height = 23
    Caption = '...'
    TabOrder = 3
    OnClick = bDestinationClick
  end
  object ProgressBar1: TProgressBar
    Left = 16
    Top = 104
    Width = 329
    Height = 16
    Min = 0
    Max = 100
    TabOrder = 5
    Visible = False
  end
  object bImport: TButton
    Left = 184
    Top = 360
    Width = 169
    Height = 23
    Caption = 'GO!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    OnClick = bImportClick
  end
  object lbClasses: TListBox
    Left = 16
    Top = 144
    Width = 153
    Height = 241
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 6
  end
  object bReadClasses: TButton
    Left = 176
    Top = 144
    Width = 100
    Height = 23
    Caption = 'Read Classes'
    TabOrder = 7
    OnClick = bImportClick
  end
  object bParams: TButton
    Left = 176
    Top = 178
    Width = 100
    Height = 23
    Caption = 'Classes list...'
    TabOrder = 8
    OnClick = bParamsClick
  end
  object bAddToReg: TButton
    Left = 176
    Top = 218
    Width = 100
    Height = 23
    Hint = 'Add selected to classes list'
    Caption = 'Add  to classes...'
    TabOrder = 9
    OnClick = bAddToRegClick
  end
  object cbClasses: TCheckBox
    Left = 184
    Top = 256
    Width = 161
    Height = 17
    Hint = 'Import methods from selected classes'
    Caption = 'Import methods'
    Checked = True
    State = cbChecked
    TabOrder = 10
  end
  object cbFunctions: TCheckBox
    Left = 184
    Top = 280
    Width = 161
    Height = 17
    Caption = 'Import functions, procedures'
    TabOrder = 11
  end
  object cbConstants: TCheckBox
    Left = 184
    Top = 304
    Width = 161
    Height = 17
    Caption = 'Import enum constants'
    TabOrder = 12
  end
  object cbDirectCall: TCheckBox
    Left = 184
    Top = 328
    Width = 161
    Height = 17
    Caption = 'Use DirectCall if possible'
    TabOrder = 13
  end
  object OpenDialog: TOpenDialog
    Filter = 'Delphi source files (*.pas)|*.pas|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist]
    Title = 'Select pas file for importing'
    Left = 256
    Top = 16
  end
  object SaveDialog: TSaveDialog
    Filter = 'Delphi source files (*.pas)|*.pas|All files (*.*)|*.*'
    Title = 'Select destination file name'
    Left = 256
    Top = 72
  end
end
