object Form1: TForm1
  Left = 372
  Top = 182
  Width = 328
  Height = 376
  Caption = 'JvSearchFiles Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 4
    Top = 6
    Width = 305
    Height = 95
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 16
      Width = 42
      Height = 13
      Caption = 'Directory'
    end
    object Label2: TLabel
      Left = 8
      Top = 44
      Width = 45
      Height = 13
      Caption = 'File Mask'
    end
    object JvDirectoryBox1: TJvDirectoryEdit
      Left = 62
      Top = 14
      Width = 233
      Height = 21
      ButtonFlat = False
      NumGlyphs = 1
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object chkRecursive: TCheckBox
      Left = 60
      Top = 64
      Width = 97
      Height = 17
      Caption = 'Recursive'
      TabOrder = 1
    end
    object Edit1: TEdit
      Left = 62
      Top = 40
      Width = 233
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      Text = '*.*'
    end
  end
  object btnSearch: TButton
    Left = 10
    Top = 106
    Width = 75
    Height = 25
    Caption = 'Search'
    TabOrder = 1
    OnClick = btnSearchClick
  end
  object GroupBox2: TGroupBox
    Left = 4
    Top = 138
    Width = 305
    Height = 203
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    DesignSize = (
      305
      203)
    object lbFoundFiles: TJvListBox
      Left = 8
      Top = 16
      Width = 287
      Height = 174
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
      ScrollBars = ssVertical
    end
  end
  object btnCancel: TButton
    Left = 96
    Top = 106
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    Enabled = False
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object JvSearchFile1: TJvSearchFiles
    DirOption = doExcludeInvalidDirs
    DirParams.LastChangeAfter = 29221
    DirParams.LastChangeBefore = 29221
    FileParams.SearchTypes = [stFileMask]
    FileParams.LastChangeAfter = 29221
    FileParams.LastChangeBefore = 29221
    OnFindFile = JvSearchFile1FindFile
    Left = 252
    Top = 84
  end
end
