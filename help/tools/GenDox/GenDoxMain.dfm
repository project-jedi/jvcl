object frmMain: TfrmMain
  Left = 294
  Top = 273
  Width = 550
  Height = 497
  Caption = 'Help project file generator'
  Color = clBtnFace
  Constraints.MinWidth = 323
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlSettings: TPanel
    Left = 5
    Top = 5
    Width = 532
    Height = 106
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    object lblJVCLDir: TLabel
      Left = 10
      Top = 10
      Width = 83
      Height = 13
      Caption = 'JVCL base folder:'
    end
    object edJVCLBaseFolder: TEdit
      Left = 100
      Top = 5
      Width = 342
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = '..\JVCL3'
    end
    object btnBrowse: TButton
      Left = 451
      Top = 5
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Browse...'
      TabOrder = 1
      OnClick = btnBrowseClick
    end
    object cxIncludeLocked: TCheckBox
      Left = 100
      Top = 35
      Width = 195
      Height = 17
      Caption = 'Include &locked files'
      TabOrder = 2
    end
    object cxGenFullHelp: TCheckBox
      Left = 100
      Top = 55
      Width = 195
      Height = 17
      Caption = 'Generate full help project'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object cxGenPackagesHelp: TCheckBox
      Left = 100
      Top = 75
      Width = 195
      Height = 17
      Caption = 'Generate per package help projects'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object pnlProgress: TPanel
    Left = 5
    Top = 115
    Width = 530
    Height = 350
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object pnlAction: TPanel
      Left = 5
      Top = 5
      Width = 520
      Height = 19
      BevelOuter = bvLowered
      TabOrder = 0
      object lblActionCaption: TLabel
        Left = 3
        Top = 3
        Width = 69
        Height = 13
        Caption = 'Current action:'
      end
      object lblAction: TLabel
        Left = 125
        Top = 3
        Width = 391
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
      end
    end
    object pnlActionInfo: TPanel
      Left = 5
      Top = 30
      Width = 520
      Height = 19
      BevelOuter = bvLowered
      TabOrder = 1
      object lblActionInfoCaption: TLabel
        Left = 3
        Top = 3
        Width = 119
        Height = 13
        AutoSize = False
      end
      object lblActionInfo: TLabel
        Left = 125
        Top = 3
        Width = 391
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
      end
    end
    object btnGenerate: TButton
      Left = 370
      Top = 320
      Width = 75
      Height = 25
      Caption = 'Generate'
      TabOrder = 2
      OnClick = btnGenerateClick
    end
    object btnClose: TButton
      Left = 450
      Top = 320
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 3
      OnClick = btnCloseClick
    end
  end
  object StorageLinks: TJvFormStorage
    AppStorage = JvAppRegistryStorage
    StoredProps.Strings = (
      'cxGenFullHelp.Checked'
      'cxGenPackagesHelp.Checked'
      'cxIncludeLocked.Checked'
      'edJVCLBaseFolder.Text')
    StoredValues = <>
    Left = 60
    Top = 35
  end
  object JvAppRegistryStorage: TJvAppRegistryStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    Root = 'Software\Project JEDI\GenDox'
    SubStorages = <>
    Left = 50
    Top = 90
  end
end
