object frmMain: TfrmMain
  Left = 200
  Top = 87
  Width = 586
  Height = 627
  Caption = 'ShFileOperation demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object JvLabel1: TJvLabel
    Left = 8
    Top = 16
    Width = 37
    Height = 13
    Caption = 'Source:'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Shell Dlg 2'
    HotTrackFont.Style = []
  end
  object JvLabel2: TJvLabel
    Left = 8
    Top = 176
    Width = 58
    Height = 13
    Caption = 'Destination:'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Shell Dlg 2'
    HotTrackFont.Style = []
  end
  object JvBevel1: TJvBevel
    Left = 8
    Top = 336
    Width = 569
    Height = 5
    Shape = bsTopLine
  end
  object JvLabel3: TJvLabel
    Left = 8
    Top = 344
    Width = 128
    Height = 13
    Caption = 'Renamed files / messages:'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Shell Dlg 2'
    HotTrackFont.Style = []
  end
  object btnCopy: TJvButton
    Left = 248
    Top = 304
    Width = 75
    Height = 25
    Hint = 'Copy Source to Destination'
    Caption = 'Copy'
    TabOrder = 0
    OnClick = btnCopyClick
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Shell Dlg 2'
    HotTrackFont.Style = []
  end
  object btnMove: TJvButton
    Left = 328
    Top = 304
    Width = 75
    Height = 25
    Hint = 'Move Source to Destination'
    Caption = 'Move'
    TabOrder = 1
    OnClick = btnMoveClick
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Shell Dlg 2'
    HotTrackFont.Style = []
  end
  object btnRename: TJvButton
    Left = 408
    Top = 304
    Width = 75
    Height = 25
    Hint = 'Rename Source to Destination'
    Caption = 'Rename'
    TabOrder = 2
    OnClick = btnRenameClick
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Shell Dlg 2'
    HotTrackFont.Style = []
  end
  object btnDelete: TJvButton
    Left = 488
    Top = 304
    Width = 75
    Height = 25
    Hint = 'Delete files in Source (ignore Destination)'
    Caption = 'Delete'
    TabOrder = 3
    OnClick = btnDeleteClick
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Shell Dlg 2'
    HotTrackFont.Style = []
  end
  object memSource: TJvMemo
    Left = 8
    Top = 32
    Width = 225
    Height = 137
    Hint = 
      'List that specifies one or more source file names. '#13#10'Multiple na' +
      'mes must be on separate rows.'#13#10'Accept wildcards in filenames but' +
      ' not in foldernames.'
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    Lines.Strings = (
      'C:\temp\*.*')
    ReadOnly = False
    ScrollBars = ssBoth
    TabOrder = 4
    Wordwrap = False
  end
  object memDest: TJvMemo
    Left = 8
    Top = 192
    Width = 225
    Height = 137
    Hint = 
      'List that contains the name of the destination file or directory' +
      '. '#13#10'The list can contain mutiple destination file names if "Mult' +
      'i Destination Files" is checked'#13#10'Multiple names must be on separ' +
      'ate lines.'
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    Lines.Strings = (
      'C:\temp\temp')
    ReadOnly = False
    ScrollBars = ssBoth
    TabOrder = 5
    Wordwrap = False
  end
  object JvGroupBox1: TJvGroupBox
    Left = 240
    Top = 24
    Width = 321
    Height = 257
    Caption = ' Options: '
    TabOrder = 6
    object Label1: TLabel
      Left = 16
      Top = 200
      Width = 24
      Height = 13
      Caption = 'Title:'
    end
    object chkUndo: TJvCheckBox
      Left = 16
      Top = 32
      Width = 201
      Height = 17
      Hint = 'Preserves undo information, if possible'
      Caption = 'Allow undo'
      Checked = True
      State = cbChecked
      TabOrder = 0
      AutoSize = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Shell Dlg 2'
      HotTrackFont.Style = []
    end
    object chkFiles: TJvCheckBox
      Left = 16
      Top = 48
      Width = 201
      Height = 17
      Hint = 
        'Performs the operation only on files if a wildcard filename (*.*' +
        ') is specified.'
      Caption = 'Files only'
      Checked = True
      State = cbChecked
      TabOrder = 1
      AutoSize = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Shell Dlg 2'
      HotTrackFont.Style = []
    end
    object chkMulti: TJvCheckBox
      Left = 16
      Top = 64
      Width = 201
      Height = 17
      Hint = 
        'Indicates that Destination specifies multiple destination files ' +
        #13#10'(one for each source file) rather than one directory where '#13#10'a' +
        'll source files are to be deposited.'
      Caption = 'Multi destination files'
      TabOrder = 2
      AutoSize = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Shell Dlg 2'
      HotTrackFont.Style = []
    end
    object chkNoConfirm: TJvCheckBox
      Left = 16
      Top = 80
      Width = 201
      Height = 17
      Hint = 'Responds with "yes to all" for any dialog box that is displayed.'
      Caption = 'Don'#39't confirm'
      TabOrder = 3
      AutoSize = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Shell Dlg 2'
      HotTrackFont.Style = []
    end
    object chkNoDirCreate: TJvCheckBox
      Left = 16
      Top = 96
      Width = 201
      Height = 17
      Hint = 
        'Does not confirm the creation of a new directory if the operatio' +
        'n requires one to be created.'
      Caption = 'Don'#39't confirm dir create'
      TabOrder = 4
      AutoSize = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Shell Dlg 2'
      HotTrackFont.Style = []
    end
    object chkRename: TJvCheckBox
      Left = 16
      Top = 112
      Width = 201
      Height = 17
      Hint = 
        'Gives the file being operated on a new name (such as "Copy #1 of' +
        '...") in a move, copy, or rename operation if a file of the targ' +
        'et name already exists.'
      Caption = 'Rename on collision'
      Checked = True
      State = cbChecked
      TabOrder = 5
      AutoSize = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Shell Dlg 2'
      HotTrackFont.Style = []
    end
    object chkSilent: TJvCheckBox
      Left = 16
      Top = 128
      Width = 201
      Height = 17
      Hint = 'Does not display a progress dialog box.'
      Caption = 'Silent'
      TabOrder = 6
      AutoSize = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Shell Dlg 2'
      HotTrackFont.Style = []
    end
    object chkSimple: TJvCheckBox
      Left = 16
      Top = 144
      Width = 201
      Height = 17
      Hint = 'Displays a progress dialog box, but does not show the filenames.'
      Caption = 'Simple Progress'
      TabOrder = 7
      AutoSize = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Shell Dlg 2'
      HotTrackFont.Style = []
    end
    object chkMappings: TJvCheckBox
      Left = 16
      Top = 160
      Width = 201
      Height = 17
      Hint = 'Return list of renamed files.'
      Caption = 'Want file mappings'
      Checked = True
      State = cbChecked
      TabOrder = 8
      AutoSize = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Shell Dlg 2'
      HotTrackFont.Style = []
    end
    object chkNoErrors: TJvCheckBox
      Left = 16
      Top = 176
      Width = 201
      Height = 17
      Hint = 'Don'#39't show any error dialogs (just return false).'
      Caption = 'Don'#39't show errors'
      TabOrder = 9
      AutoSize = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Shell Dlg 2'
      HotTrackFont.Style = []
    end
    object edTitle: TEdit
      Left = 16
      Top = 216
      Width = 289
      Height = 21
      Hint = 'Dialog title to show when "Simple Progress" is true.'
      TabOrder = 10
    end
  end
  object memMessages: TJvMemo
    Left = 8
    Top = 360
    Width = 561
    Height = 233
    AutoSize = False
    ClipboardCommands = [caCopy]
    MaxLines = 0
    HideCaret = False
    Lines.Strings = (
      'Before running this demo, make sure the files you'
      'are using for testing has been BACKUPPED!'
      '')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 7
    Wordwrap = False
  end
  object JvSHFileOperation1: TJvSHFileOperation
    Options = []
    OnFileMapping = JvSHFileOperation1FileMapping
    Left = 480
    Top = 56
  end
end
