object frmEditPasCleanOptions: TfrmEditPasCleanOptions
  Left = 256
  Top = 242
  BorderStyle = bsDialog
  Caption = 'frmEditPasCleanOptions'
  ClientHeight = 268
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 353
    Height = 89
    TabOrder = 0
    object Label1: TLabel
      Left = 32
      Top = 24
      Width = 84
      Height = 13
      Caption = 'Capitalization File:'
    end
    object chbCapitalization: TCheckBox
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Capitalization'
      TabOrder = 0
      OnClick = chbCapitalizationClick
    end
    object chbSortImplementationSection: TCheckBox
      Left = 8
      Top = 64
      Width = 209
      Height = 17
      Caption = 'Sort implementation section'
      TabOrder = 2
    end
    object edtCapitalizationFile: TJvFilenameEdit
      Left = 32
      Top = 40
      Width = 313
      Height = 21
      AddQuotes = False
      ButtonFlat = False
      NumGlyphs = 1
      TabOrder = 1
      Text = 'edtCapitalizationFile'
    end
  end
  object Panel2: TPanel
    Left = 8
    Top = 104
    Width = 353
    Height = 113
    TabOrder = 1
    object Label2: TLabel
      Left = 32
      Top = 56
      Width = 80
      Height = 13
      Caption = 'Output Directory:'
    end
    object rbtOutputToSameDir: TRadioButton
      Left = 8
      Top = 8
      Width = 249
      Height = 17
      Caption = 'Output to same dir (backup old files)'
      TabOrder = 0
      OnClick = rbtOutputToSameDirClick
    end
    object rbtOutputToOtherDir: TRadioButton
      Left = 8
      Top = 32
      Width = 113
      Height = 17
      Caption = 'Output to other dir'
      TabOrder = 1
      OnClick = rbtOutputToOtherDirClick
    end
    object edtOutputDirectory: TJvDirectoryEdit
      Left = 32
      Top = 72
      Width = 313
      Height = 21
      DialogKind = dkWin32
      ButtonFlat = False
      NumGlyphs = 1
      TabOrder = 2
      Text = 'edtOutputDirectory'
    end
  end
  object Button1: TButton
    Left = 200
    Top = 232
    Width = 75
    Height = 25
    Action = actOK
    Default = True
    TabOrder = 2
  end
  object Button2: TButton
    Left = 280
    Top = 232
    Width = 75
    Height = 25
    Action = actCancel
    Cancel = True
    TabOrder = 3
  end
  object ActionList1: TActionList
    Left = 64
    Top = 224
    object actCancel: TAction
      Caption = 'Cancel'
      OnExecute = actCancelExecute
    end
    object actOK: TAction
      Caption = 'OK'
      OnExecute = actOKExecute
    end
  end
  object JvAppRegistryStore1: TJvAppRegistryStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    Root = 'Software\JVCL\GenDtx'
    SubStorages = <>
    Left = 264
    Top = 16
  end
end
