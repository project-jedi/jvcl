object FormMain: TFormMain
  Left = 192
  Top = 114
  ActiveControl = BtnExecute
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JVCL VCL to CLX Converter'
  ClientHeight = 396
  ClientWidth = 641
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LblProgress: TLabel
    Left = 15
    Top = 320
    Width = 55
    Height = 13
    Caption = 'LblProgress'
  end
  object Label1: TLabel
    Left = 16
    Top = 128
    Width = 78
    Height = 13
    Caption = '&Output directory:'
    FocusControl = EditOutDir
  end
  object Label2: TLabel
    Left = 16
    Top = 199
    Width = 71
    Height = 13
    Caption = '&JVCL directory:'
    FocusControl = EditJVCLDir
  end
  object Bevel1: TBevel
    Left = 424
    Top = 1
    Width = 6
    Height = 345
    Shape = bsLeftLine
  end
  object BtnExecute: TButton
    Left = 16
    Top = 366
    Width = 75
    Height = 25
    Caption = '&Execute'
    TabOrder = 11
    OnClick = BtnExecuteClick
  end
  object ProgressBar: TProgressBar
    Left = 15
    Top = 336
    Width = 403
    Height = 17
    TabOrder = 5
  end
  object EditOutDir: TJvDirectoryEdit
    Left = 16
    Top = 144
    Width = 401
    Height = 21
    ClipboardCommands = []
    DialogKind = dkWin32
    TabOrder = 2
  end
  object EditSingleFile: TJvFilenameEdit
    Left = 16
    Top = 40
    Width = 401
    Height = 21
    OnButtonClick = EditSingleFileButtonClick
    ClipboardCommands = []
    TabOrder = 1
  end
  object RBtnSingleFile: TRadioButton
    Left = 8
    Top = 16
    Width = 209
    Height = 17
    Caption = '&Single file:'
    TabOrder = 0
  end
  object RBtnDir: TRadioButton
    Left = 8
    Top = 73
    Width = 201
    Height = 17
    Caption = '&Directory'
    TabOrder = 3
  end
  object BtnQuit: TButton
    Left = 343
    Top = 366
    Width = 75
    Height = 25
    Caption = '&Quit'
    TabOrder = 12
    OnClick = BtnQuitClick
  end
  object EditJVCLDir: TJvDirectoryEdit
    Left = 16
    Top = 215
    Width = 401
    Height = 21
    OnButtonClick = EditJVCLDirButtonClick
    ClipboardCommands = []
    DialogKind = dkWin32
    TabOrder = 4
  end
  object CheckBoxReduceConditions: TCheckBox
    Left = 434
    Top = 8
    Width = 201
    Height = 17
    Caption = '&Reduce conditions'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object CheckBoxKeepLines: TCheckBox
    Left = 450
    Top = 32
    Width = 185
    Height = 17
    Caption = '&Keep removed lines'
    TabOrder = 7
  end
  object CheckBoxUnixLineBreaks: TCheckBox
    Left = 434
    Top = 56
    Width = 201
    Height = 17
    Caption = '&Unix line breaks'
    TabOrder = 8
  end
  object CheckBoxForceOverwrite: TCheckBox
    Left = 434
    Top = 104
    Width = 201
    Height = 17
    Caption = '&Force overwrite'
    Checked = True
    State = cbChecked
    TabOrder = 10
  end
  object ListBox1: TListBox
    Left = 15
    Top = 254
    Width = 400
    Height = 57
    ItemHeight = 13
    TabOrder = 13
  end
  object RBtnAll: TRadioButton
    Left = 8
    Top = 176
    Width = 113
    Height = 17
    Caption = '&All JVCL JVCLX files'
    Checked = True
    TabOrder = 14
    TabStop = True
  end
  object EditDirectory: TJvDirectoryEdit
    Left = 16
    Top = 96
    Width = 401
    Height = 21
    OnButtonClick = EditDirectoryButtonClick
    ClipboardCommands = []
    DialogKind = dkWin32
    TabOrder = 15
  end
  object CheckBoxRecursiveDir: TCheckBox
    Left = 320
    Top = 117
    Width = 97
    Height = 17
    Caption = 'Recursive'
    TabOrder = 16
  end
  object CheckBoxUnixPathDelim: TCheckBox
    Left = 434
    Top = 80
    Width = 201
    Height = 17
    Caption = '&Unix Path Delimiter'
    TabOrder = 9
  end
end
