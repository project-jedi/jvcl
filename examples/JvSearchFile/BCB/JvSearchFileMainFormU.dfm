object JvSearchFileMainForm: TJvSearchFileMainForm
  Left = 320
  Top = 178
  Width = 371
  Height = 345
  Caption = 'JvSearchFiles Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    Left = 4
    Top = 190
    Width = 353
    Height = 105
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object reFoundFiles: TRichEdit
      Left = 6
      Top = 12
      Width = 341
      Height = 87
      Anchors = [akLeft, akTop, akRight, akBottom]
      PlainText = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 6
    Width = 353
    Height = 139
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    object Label1: TLabel
      Left = 10
      Top = 16
      Width = 45
      Height = 13
      Caption = '&Directory:'
      FocusControl = JvDirectoryBox1
    end
    object Label2: TLabel
      Left = 8
      Top = 44
      Width = 47
      Height = 13
      Caption = '&File mask:'
      FocusControl = edFileMask
    end
    object JvDirectoryBox1: TJvDirectoryEdit
      Left = 62
      Top = 14
      Width = 281
      Height = 21
      DialogKind = dkWin32
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'C:\'
      OnClick = OptionsChange
    end
    object chkRecursive: TCheckBox
      Left = 72
      Top = 64
      Width = 97
      Height = 17
      Caption = '&Recursive'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = OptionsChange
    end
    object edFileMask: TEdit
      Left = 62
      Top = 40
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = '*.*'
      OnChange = OptionsChange
    end
    object cbContainText: TComboBox
      Left = 24
      Top = 108
      Width = 319
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 5
      OnChange = OptionsChange
    end
    object rbInclude: TRadioButton
      Left = 6
      Top = 87
      Width = 79
      Height = 17
      Caption = '&With text:'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = OptionsChange
    end
    object rbExclude: TRadioButton
      Left = 132
      Top = 87
      Width = 113
      Height = 17
      Caption = 'With&out text:'
      TabOrder = 4
      OnClick = OptionsChange
    end
  end
  object chkClearList: TCheckBox
    Left = 24
    Top = 152
    Width = 134
    Height = 17
    Caption = 'C&lear list before search'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object chkNoDupes: TCheckBox
    Left = 24
    Top = 171
    Width = 134
    Height = 17
    Caption = 'Skip d&uplicates'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object btnSearch: TButton
    Left = 190
    Top = 160
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Search'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = btnSearchClick
  end
  object btnCancel: TButton
    Left = 276
    Top = 160
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = '&Cancel'
    Enabled = False
    ModalResult = 2
    TabOrder = 5
    OnClick = btnCancelClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 299
    Width = 363
    Height = 19
    Panels = <
      item
        Text = 'Ready'
        Width = 50
      end>
    SimplePanel = False
  end
  object JvFormStorage1: TJvFormStorage
    AppStorage = JvAppIniFileStorage1
    AppStoragePath = '%FORM_NAME%\'
    StoredProps.Strings = (
      'edFileMask.Text'
      'chkRecursive.Checked'
      'JvDirectoryBox1.Text'
      'cbContainText.Items'
      'cbContainText.Text'
      'chkClearList.Checked'
      'chkNoDupes.Checked'
      'rbInclude.Checked'
      'rbExclude.Checked')
    StoredValues = <>
    Left = 154
    Top = 72
  end
  object JvAppIniFileStorage1: TJvAppIniFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    FileName = 'JvSearchFilesProj.ini'
    DefaultSection = 'Settings'
    SubStorages = <>
    Left = 192
    Top = 72
  end
  object JvSearchFile1: TJvSearchFiles
    DirOption = doExcludeInvalidDirs
    RootDirectory = '(Directory)'
    FileParams.SearchTypes = [stFileMask]
    OnBeginScanDir = JvSearchFile1BeginScanDir
    OnFindFile = JvSearchFile1FindFile
    OnProgress = JvSearchFile1Progress
    Left = 234
    Top = 72
  end
end
