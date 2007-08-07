object frmDirectories: TfrmDirectories
  Left = 364
  Top = 240
  Width = 516
  Height = 404
  BorderIcons = [biSystemMenu]
  Caption = 'Directories'
  Color = clBtnFace
  Constraints.MinHeight = 360
  Constraints.MinWidth = 390
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblRunTimePasDirDesc: TLabel
    Left = 8
    Top = 57
    Width = 316
    Height = 13
    Caption = 
      'Directory with run-time .pas files (typically ($JVCL)\dev\JVCL3\' +
      'run):'
  end
  object lblDesignTimePasDir: TLabel
    Left = 8
    Top = 97
    Width = 348
    Height = 13
    Caption = 
      'Directory with design-time .pas files (typically ($JVCL)\dev\JVC' +
      'L3\design):'
  end
  object lblOutDirDesc: TLabel
    Left = 8
    Top = 137
    Width = 174
    Height = 13
    Caption = 'Directory for the generated *.dtx files:'
  end
  object Label1: TLabel
    Left = 8
    Top = 201
    Width = 264
    Height = 13
    Caption = 'Directory with the *.dtx files (typically ($JVCL)\dev\help):'
  end
  object Label2: TLabel
    Left = 8
    Top = 241
    Width = 366
    Height = 13
    Caption = 
      'Directory with run-time package (typically ($JVCL)\dev\JVCL3\pac' +
      'kages\d7):'
  end
  object Label3: TLabel
    Left = 8
    Top = 0
    Width = 97
    Height = 13
    Caption = 'Root JVCL directory:'
  end
  object Label4: TLabel
    Left = 8
    Top = 281
    Width = 228
    Height = 13
    Caption = 'Root Delphi source directory ($DELPHI)\source:'
  end
  object edtRunTimePasDir: TEdit
    Left = 8
    Top = 73
    Width = 466
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 'edtRunTimePasDir'
  end
  object btnRunTimePasDir: TButton
    Left = 475
    Top = 73
    Width = 25
    Height = 21
    Action = actRunTimePasDir
    Anchors = [akTop, akRight]
    TabOrder = 4
  end
  object edtDesignTimePasDir: TEdit
    Left = 8
    Top = 113
    Width = 466
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    Text = 'edtPasDir'
  end
  object btnDesignTimePasDir: TButton
    Left = 475
    Top = 113
    Width = 25
    Height = 21
    Action = actDesignTimePasDir
    Anchors = [akTop, akRight]
    TabOrder = 6
  end
  object edtGeneratedDtxDir: TEdit
    Left = 8
    Top = 153
    Width = 466
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    Text = 'edtGeneratedDtxDir'
  end
  object btnGeneratedDtxDir: TButton
    Left = 475
    Top = 153
    Width = 25
    Height = 21
    Action = actGeneratedDtxDir
    Anchors = [akTop, akRight]
    TabOrder = 8
  end
  object edtRealDtxDir: TEdit
    Left = 8
    Top = 217
    Width = 466
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 10
    Text = 'edtOutDir'
  end
  object btnRealDtxDir: TButton
    Left = 475
    Top = 217
    Width = 25
    Height = 21
    Action = actRealDtxDir
    Anchors = [akTop, akRight]
    TabOrder = 11
  end
  object edtPackageDir: TEdit
    Left = 8
    Top = 257
    Width = 466
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 12
    Text = 'edtOutDir'
  end
  object btnPackageDir: TButton
    Left = 475
    Top = 257
    Width = 25
    Height = 21
    Action = actPackageDir
    Anchors = [akTop, akRight]
    TabOrder = 13
  end
  object chbOverwriteExisting: TCheckBox
    Left = 8
    Top = 177
    Width = 113
    Height = 17
    Caption = 'Overwrite Existing'
    TabOrder = 9
  end
  object btnApply: TButton
    Left = 263
    Top = 337
    Width = 75
    Height = 25
    Action = actApply
    Anchors = [akRight, akBottom]
    TabOrder = 14
  end
  object btnOK: TButton
    Left = 343
    Top = 337
    Width = 75
    Height = 25
    Action = actOK
    Anchors = [akRight, akBottom]
    Default = True
    TabOrder = 15
  end
  object btnCancel: TButton
    Left = 425
    Top = 337
    Width = 75
    Height = 25
    Action = actCancel
    Anchors = [akRight, akBottom]
    Cancel = True
    TabOrder = 16
  end
  object edtRootDir: TEdit
    Left = 8
    Top = 16
    Width = 466
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'edtGeneratedDtxDir'
    OnChange = edtRootDirChange
  end
  object chbUseRootDir: TCheckBox
    Left = 8
    Top = 40
    Width = 273
    Height = 17
    Caption = 'Use this directory as base for the other directories'
    TabOrder = 2
    OnClick = chbUseRootDirClick
  end
  object btnRootDir: TButton
    Left = 475
    Top = 16
    Width = 25
    Height = 21
    Action = actRootDir
    Anchors = [akTop, akRight]
    TabOrder = 1
  end
  object edtRootDelphiDir: TEdit
    Left = 8
    Top = 297
    Width = 466
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 17
    Text = 'edtOutDir'
  end
  object btnRootDelphiDir: TButton
    Left = 475
    Top = 297
    Width = 25
    Height = 21
    Action = actPackageDir
    Anchors = [akTop, akRight]
    TabOrder = 18
  end
  object ActionList1: TActionList
    Left = 136
    Top = 328
    object actCancel: TAction
      Category = 'Buttons'
      Caption = 'Cancel'
      OnExecute = actCancelExecute
    end
    object actOK: TAction
      Category = 'Buttons'
      Caption = 'OK'
      OnExecute = actOKExecute
    end
    object actApply: TAction
      Category = 'Buttons'
      Caption = '&Apply'
      OnExecute = actApplyExecute
    end
    object actRealDtxDir: TAction
      Category = 'Files'
      Caption = '...'
      OnExecute = actRealDtxDirExecute
    end
    object actRunTimePasDir: TAction
      Category = 'Files'
      Caption = '...'
      OnExecute = actRunTimePasDirExecute
    end
    object actDesignTimePasDir: TAction
      Category = 'Files'
      Caption = '...'
      OnExecute = actDesignTimePasDirExecute
    end
    object actGeneratedDtxDir: TAction
      Category = 'Files'
      Caption = '...'
      OnExecute = actGeneratedDtxDirExecute
    end
    object actPackageDir: TAction
      Category = 'Files'
      Caption = '...'
      OnExecute = actPackageDirExecute
    end
    object actRootDir: TAction
      Category = 'Files'
      Caption = '...'
      OnExecute = actRootDirExecute
    end
    object actRootDelphiDir: TAction
      Category = 'Files'
      Caption = 'actRootDelphiDir'
      OnExecute = actRootDelphiDirExecute
    end
  end
  object JvBrowseForFolderDialog1: TJvBrowseForFolderDialog
    Left = 96
    Top = 320
  end
end
