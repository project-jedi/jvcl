object frmOpenFiles: TfrmOpenFiles
  Left = 427
  Top = 397
  Width = 276
  Height = 455
  Caption = 'Iterate Subdirectory'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ShellTreeView1: TShellTreeView
    Left = 0
    Top = 0
    Width = 268
    Height = 328
    ObjectTypes = [otFolders]
    Root = 'rfMyComputer'
    UseShellImages = True
    Align = alClient
    AutoRefresh = False
    Indent = 19
    ParentColor = False
    RightClickSelect = True
    ShowRoot = False
    TabOrder = 0
    OnChange = ShellTreeView1Change
    OnGetSelectedIndex = ShellTreeView1GetSelectedIndex
  end
  object Panel1: TPanel
    Left = 0
    Top = 328
    Width = 268
    Height = 100
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      268
      100)
    object Label1: TLabel
      Left = 4
      Top = 36
      Width = 52
      Height = 13
      Anchors = [akRight, akBottom]
      Caption = 'File Types:'
    end
    object lblDirectory: TLabel
      Left = 11
      Top = 8
      Width = 3
      Height = 13
    end
    object cbxFilesTypes: TComboBox
      Left = 56
      Top = 33
      Width = 201
      Height = 21
      Anchors = [akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        '*.pas;*.dpr;*.dpk'
        '*.txt')
    end
    object btnOK: TButton
      Left = 174
      Top = 65
      Width = 77
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 58
      Top = 64
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      TabOrder = 2
      OnClick = btnCancelClick
    end
  end
end
