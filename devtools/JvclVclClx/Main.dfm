object FormMain: TFormMain
  Left = 192
  Top = 114
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JVCL VCL to CLX Converter'
  ClientHeight = 268
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
    Left = 8
    Top = 200
    Width = 55
    Height = 13
    Caption = 'LblProgress'
  end
  object Label1: TLabel
    Left = 16
    Top = 72
    Width = 78
    Height = 13
    Caption = '&Output directory:'
    FocusControl = EditOutDir
  end
  object Label2: TLabel
    Left = 16
    Top = 152
    Width = 71
    Height = 13
    Caption = '&JVCL directory:'
    FocusControl = EditJVCLDir
  end
  object Bevel1: TBevel
    Left = 424
    Top = 1
    Width = 6
    Height = 268
    Shape = bsLeftLine
  end
  object BtnExecute: TButton
    Left = 8
    Top = 240
    Width = 75
    Height = 25
    Caption = '&Execute'
    TabOrder = 10
    OnClick = BtnExecuteClick
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 216
    Width = 409
    Height = 17
    TabOrder = 5
  end
  object EditOutDir: TJvDirectoryEdit
    Left = 16
    Top = 88
    Width = 401
    Height = 21
    ButtonFlat = False
    TabOrder = 2
  end
  object EditSingleFile: TJvFilenameEdit
    Left = 16
    Top = 40
    Width = 401
    Height = 21
    ButtonFlat = False
    TabOrder = 1
  end
  object RBtnSingleFile: TRadioButton
    Left = 8
    Top = 16
    Width = 209
    Height = 17
    Caption = '&Single file / directory:'
    TabOrder = 0
  end
  object RBtnAll: TRadioButton
    Left = 8
    Top = 128
    Width = 201
    Height = 17
    Caption = '&All JVCL JVCLX files'
    Checked = True
    TabOrder = 3
    TabStop = True
  end
  object BtnQuit: TButton
    Left = 344
    Top = 240
    Width = 75
    Height = 25
    Caption = '&Quit'
    TabOrder = 11
    OnClick = BtnQuitClick
  end
  object EditJVCLDir: TJvDirectoryEdit
    Left = 16
    Top = 168
    Width = 401
    Height = 21
    DialogKind = dkWin32
    ButtonFlat = False
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
    Top = 80
    Width = 201
    Height = 17
    Caption = '&Force overwrite'
    TabOrder = 9
  end
end
