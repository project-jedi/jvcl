object MainFrm: TMainFrm
  Left = 372
  Top = 182
  Width = 329
  Height = 351
  Caption = 'JvSearchFiles Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    321
    324)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 4
    Top = 6
    Width = 306
    Height = 139
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    DesignSize = (
      306
      139)
    object Label1: TLabel
      Left = 10
      Top = 16
      Width = 48
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
      Width = 234
      Height = 21
      ButtonFlat = False
      NumGlyphs = 1
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'C:\'
      OnChange = OptionsChange
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
      Width = 234
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = '*.*'
      OnChange = OptionsChange
    end
    object chkContains: TCheckBox
      Left = 12
      Top = 90
      Width = 133
      Height = 17
      Caption = 'Containing the &text:'
      TabOrder = 4
      OnClick = chkContainsClick
    end
    object edContainText: TJvEdit
      Left = 30
      Top = 108
      Width = 265
      Height = 21
      DisabledColor = clBtnFace
      GroupIndex = -1
      MaxPixel.Font.Charset = DEFAULT_CHARSET
      MaxPixel.Font.Color = clWindowText
      MaxPixel.Font.Height = -11
      MaxPixel.Font.Name = 'MS Sans Serif'
      MaxPixel.Font.Style = []
      Modified = False
      SelStart = 0
      SelLength = 0
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      PasswordChar = #0
      ReadOnly = False
      TabOrder = 5
      OnChange = OptionsChange
    end
    object chkVirtual: TCheckBox
      Left = 193
      Top = 64
      Width = 97
      Height = 17
      Caption = '&Virtual'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = OptionsChange
    end
  end
  object btnSearch: TButton
    Left = 10
    Top = 154
    Width = 75
    Height = 25
    Caption = '&Search'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnSearchClick
  end
  object GroupBox2: TGroupBox
    Left = 7
    Top = 182
    Width = 306
    Height = 116
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    DesignSize = (
      306
      116)
    object reFoundFiles: TRichEdit
      Left = 12
      Top = 18
      Width = 283
      Height = 88
      Anchors = [akLeft, akTop, akRight, akBottom]
      PlainText = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object btnCancel: TButton
    Left = 96
    Top = 154
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    Enabled = False
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 305
    Width = 321
    Height = 19
    Panels = <
      item
        Text = 'Ready'
        Width = 50
      end>
    SimplePanel = False
  end
  object JvSearchFile1: TJvSearchFiles
    DirOption = doExcludeInvalidDirs
    DirParams.LastChangeAfter = 29221
    DirParams.LastChangeBefore = 29221
    FileParams.SearchTypes = [stFileMask]
    FileParams.LastChangeAfter = 29221
    FileParams.LastChangeBefore = 29221
    OnBeginScanDir = JvSearchFile1BeginScanDir
    OnFindFile = JvSearchFile1FindFile
    Left = 234
    Top = 72
  end
  object JvFormStorage1: TJvFormStorage
    IniSection = 'Settings'
    StoredProps.Strings = (
      'edContainText.Text'
      'edFileMask.Text'
      'chkVirtual.Checked'
      'chkRecursive.Checked'
      'chkContains.Checked'
      'JvDirectoryBox1.Text')
    StoredValues = <>
    Left = 186
    Top = 72
  end
end
