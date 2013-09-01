object Profiler32MainForm: TProfiler32MainForm
  Left = 343
  Top = 157
  Width = 506
  Height = 307
  Caption = 'Profiler 32 test program'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 0
    Top = 41
    Width = 498
    Height = 225
    Align = alClient
    BorderStyle = bsNone
    ItemHeight = 13
    Items.Strings = (
      'ASSOC'
      'AT'
      'ATTRIB'
      'BREAK'
      'CACLS'
      'CALL'
      'CD'
      'CHCP'
      'CHDIR'
      'CHKDSK'
      'CLS'
      'CMD'
      'COLOR'
      'COMP'
      'COMPACT'
      'CONVERT'
      'COPY'
      'DATE'
      'DEL'
      'DIR'
      'DISKCOMP'
      'DISKCOPY'
      'DOSKEY'
      'ECHO'
      'ENDLOCAL'
      'ERASE'
      'EXIT'
      'FC'
      'FIND'
      'FINDSTR'
      'FOR'
      'FORMAT'
      'FTYPE'
      'GOTO'
      'GRAFTABL'
      'HELP'
      'IF'
      'KEYB'
      'LABEL'
      'MD'
      'MKDIR'
      'MODE'
      'MORE'
      'MOVE'
      'PATH'
      'PAUSE'
      'POPD'
      'PRINT'
      'PROMPT'
      'PUSHD'
      'RD'
      'RECOVER'
      'REM'
      'REN'
      'RENAME'
      'REPLACE'
      'RESTORE'
      'RMDIR'
      'SET'
      'SETLOCAL'
      'SHIFT'
      'SORT'
      'START'
      'SUBST'
      'TIME'
      'TITLE'
      'TREE'
      'TYPE'
      'VER'
      'VERIFY'
      'VOL'
      'XCOPY')
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 498
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 10
      Top = 10
      Width = 64
      Height = 13
      Caption = 'Create report:'
    end
    object UseIdBtn: TButton
      Left = 104
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Use &ID'
      TabOrder = 0
      OnClick = UseIdBtnClick
    end
    object UseNameBtn: TButton
      Left = 192
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Use &name'
      TabOrder = 1
      OnClick = UseNameBtnClick
    end
    object ResultBtn: TButton
      Left = 392
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Result'
      TabOrder = 2
      OnClick = ResultBtnClick
    end
  end
  object Progress: TProgressBar
    Left = 0
    Top = 266
    Width = 498
    Height = 16
    Align = alBottom
    TabOrder = 2
  end
  object P: TJvProfiler
    Left = 376
    Top = 56
  end
end
