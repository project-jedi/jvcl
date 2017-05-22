object FormMain: TFormMain
  Left = 258
  Top = 166
  BorderStyle = bsDialog
  Caption = 'Unit Versioning'
  ClientHeight = 116
  ClientWidth = 351
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
  object Label1: TLabel
    Left = 8
    Top = 40
    Width = 62
    Height = 13
    Caption = '&Logical Path:'
    FocusControl = EditLogPath
  end
  object BtnExecute: TButton
    Left = 8
    Top = 88
    Width = 89
    Height = 25
    Caption = 'Add Versioning'
    Default = True
    TabOrder = 0
    OnClick = BtnExecuteClick
  end
  object BtnQuit: TButton
    Left = 272
    Top = 88
    Width = 74
    Height = 25
    Caption = '&Quit'
    TabOrder = 1
    OnClick = BtnQuitClick
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 64
    Width = 338
    Height = 17
    Step = 1
    TabOrder = 2
  end
  object DEditDir: TJvDirectoryEdit
    Left = 8
    Top = 8
    Width = 338
    Height = 21
    DialogKind = dkWin32
    TabOrder = 3
  end
  object CheckBoxSubDirs: TCheckBox
    Left = 104
    Top = 88
    Width = 161
    Height = 17
    Caption = 'Include subdirectories'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object EditLogPath: TEdit
    Left = 80
    Top = 36
    Width = 265
    Height = 21
    TabOrder = 5
    Text = 'JVCL\run'
  end
  object XPManifest1: TXPManifest
    Left = 232
    Top = 88
  end
  object JvSearchFiles: TJvSearchFiles
    Options = [soSearchFiles, soSorted]
    FileParams.SearchTypes = [stFileMask]
    FileParams.FileMasks.Strings = (
      '*.pas')
    Left = 208
    Top = 88
  end
end
