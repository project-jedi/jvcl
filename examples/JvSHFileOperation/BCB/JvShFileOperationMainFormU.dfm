object JvShFileOperationMainForm: TJvShFileOperationMainForm
  Left = 256
  Top = 106
  Width = 613
  Height = 540
  Caption = 'ShFileOperation demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvLabel1: TLabel
    Left = 8
    Top = 8
    Width = 37
    Height = 13
    Caption = 'Source:'
  end
  object JvLabel2: TLabel
    Left = 8
    Top = 168
    Width = 56
    Height = 13
    Caption = 'Destination:'
  end
  object JvBevel1: TBevel
    Left = 8
    Top = 320
    Width = 584
    Height = 5
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object JvLabel3: TLabel
    Left = 8
    Top = 328
    Width = 128
    Height = 13
    Caption = 'Renamed files / messages:'
  end
  object btnCopy: TButton
    Left = 248
    Top = 288
    Width = 75
    Height = 25
    Hint = 'Copy Source to Destination'
    Caption = 'Copy'
    TabOrder = 0
    OnClick = btnCopyClick
  end
  object btnMove: TButton
    Left = 328
    Top = 288
    Width = 75
    Height = 25
    Hint = 'Move Source to Destination'
    Caption = 'Move'
    TabOrder = 1
    OnClick = btnMoveClick
  end
  object btnRename: TButton
    Left = 408
    Top = 288
    Width = 75
    Height = 25
    Hint = 
      'Rename Source to Destination'#13#10#13#10'NOTE: that this function can onl' +
      'y rename one file: '#13#10'use move to rename multiple files'
    Caption = 'Rename'
    TabOrder = 2
    OnClick = btnRenameClick
  end
  object btnDelete: TButton
    Left = 488
    Top = 288
    Width = 75
    Height = 25
    Hint = 'Delete files in Source (ignore Destination)'
    Caption = 'Delete'
    TabOrder = 3
    OnClick = btnDeleteClick
  end
  object memSource: TMemo
    Left = 8
    Top = 24
    Width = 225
    Height = 137
    Hint = 
      'List that specifies one or more source file names. '#13#10'Multiple na' +
      'mes must be on separate rows.'#13#10'Accept wildcards in filenames but' +
      ' not in foldernames.'
    HideSelection = False
    Lines.Strings = (
      'C:\temp\*.*')
    ScrollBars = ssBoth
    TabOrder = 4
    WordWrap = False
  end
  object memDest: TMemo
    Left = 8
    Top = 184
    Width = 225
    Height = 129
    Hint = 
      'List that contains the name of the destination file or directory' +
      '. '#13#10'The list can contain mutiple destination file names '#13#10'if "Mu' +
      'lti Destination Files" is checked.'#13#10#13#10'Multiple names must be on ' +
      'separate lines.'
    HideSelection = False
    Lines.Strings = (
      'C:\temp\temp')
    ScrollBars = ssBoth
    TabOrder = 5
    WordWrap = False
  end
  object JvGroupBox1: TGroupBox
    Left = 240
    Top = 16
    Width = 356
    Height = 257
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Options: '
    TabOrder = 6
    object Label1: TLabel
      Left = 16
      Top = 200
      Width = 23
      Height = 13
      Caption = 'Title:'
    end
    object chkUndo: TCheckBox
      Left = 16
      Top = 32
      Width = 137
      Height = 17
      Hint = 
        'Preserves undo information, if possible'#13#10'(requires fully qualifi' +
        'ed filenames)'
      Caption = 'Allow undo'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object chkFiles: TCheckBox
      Left = 16
      Top = 48
      Width = 137
      Height = 17
      Hint = 
        'Performs the operation only on files if a wildcard filename (*.*' +
        ') is specified.'
      Caption = 'Files only'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object chkMulti: TCheckBox
      Left = 16
      Top = 64
      Width = 137
      Height = 17
      Hint = 
        'Indicates that Destination specifies multiple destination files ' +
        #13#10'(one row for each source file) rather than one directory where' +
        ' '#13#10'all source files are to be deposited.'
      Caption = 'Multi destination files'
      TabOrder = 2
    end
    object chkNoConfirm: TCheckBox
      Left = 16
      Top = 80
      Width = 137
      Height = 17
      Hint = 'Responds with "Yes To All" for any dialog box that is displayed.'
      Caption = 'Don'#39't confirm'
      TabOrder = 3
    end
    object chkNoDirCreate: TCheckBox
      Left = 16
      Top = 96
      Width = 137
      Height = 17
      Hint = 
        'Does not confirm the creation of a new directory if the operatio' +
        'n requires one to be created.'
      Caption = 'Don'#39't confirm dir create'
      TabOrder = 4
    end
    object chkRename: TCheckBox
      Left = 16
      Top = 112
      Width = 137
      Height = 17
      Hint = 
        'Gives the file being operated on a new name (such as "Copy #1 of' +
        '...") '#13#10'in a move, copy, or rename operation if a file of the ta' +
        'rget name already exists.'
      Caption = 'Rename on collision'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object chkSilent: TCheckBox
      Left = 16
      Top = 128
      Width = 137
      Height = 17
      Hint = 'Does not display a progress dialog box.'
      Caption = 'Silent'
      TabOrder = 6
    end
    object chkSimple: TCheckBox
      Left = 16
      Top = 144
      Width = 137
      Height = 17
      Hint = 'Displays a progress dialog box, but does not show the filenames.'
      Caption = 'Simple Progress'
      TabOrder = 7
    end
    object chkMappings: TCheckBox
      Left = 16
      Top = 160
      Width = 137
      Height = 17
      Hint = 'Return list of renamed files.'
      Caption = 'Want file mappings'
      Checked = True
      State = cbChecked
      TabOrder = 8
    end
    object chkNoErrors: TCheckBox
      Left = 16
      Top = 176
      Width = 137
      Height = 17
      Hint = 'Don'#39't show any error dialogs (just return false).'
      Caption = 'Don'#39't show errors'
      TabOrder = 9
    end
    object edTitle: TEdit
      Left = 16
      Top = 216
      Width = 327
      Height = 21
      Hint = 'Dialog title to show when "Simple Progress" is true.'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 10
    end
    object chkNoSecAttrs: TCheckBox
      Left = 168
      Top = 32
      Width = 142
      Height = 17
      Hint = 'Do not copy the security attributes of the file.'#13#10'(Version 4.71)'
      Caption = 'Ignore security attributes'
      TabOrder = 11
    end
    object chkNoRecurse: TCheckBox
      Left = 168
      Top = 48
      Width = 137
      Height = 17
      Hint = 
        'Only operate in the local directory. '#13#10'Don'#39't operate recursively' +
        ' into subdirectories.'
      Caption = 'Don'#39't recurse'
      TabOrder = 12
    end
    object chkNoConElem: TCheckBox
      Left = 168
      Top = 64
      Width = 151
      Height = 17
      Hint = 
        'Do not move connected files as a group. Only move the specified ' +
        'files.'#13#10'(Version 5.0)'
      Caption = 'Ignore connected elements'
      TabOrder = 13
    end
    object chkNoParse: TCheckBox
      Left = 168
      Top = 80
      Width = 137
      Height = 17
      Hint = 
        'Treat reparse points as objects, not containers. '#13#10'(Version 5.01' +
        ')'
      Caption = 'No recurse parse'
      TabOrder = 14
    end
    object chkWantNukes: TCheckBox
      Left = 168
      Top = 96
      Width = 137
      Height = 17
      Hint = 
        'Send a warning if a file is being destroyed during a delete oper' +
        'ation '#13#10'rather than recycled. This flag partially overrides fofN' +
        'oConfirm'#13#10'(Version 5.0). '
      Caption = 'Want nuke warning'
      TabOrder = 15
    end
  end
  object memMessages: TMemo
    Left = 8
    Top = 344
    Width = 587
    Height = 161
    Anchors = [akLeft, akTop, akRight, akBottom]
    HideSelection = False
    Lines.Strings = (
      'Before running this demo, make sure the files you'
      'are using for testing have been BACKUPPED!'
      '')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 7
    WordWrap = False
  end
  object JvSHFileOperation1: TJvSHFileOperation
    Options = []
    OnFileMapping = JvSHFileOperation1FileMapping
    Left = 456
    Top = 144
  end
end
