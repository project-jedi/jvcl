object frmOptions: TfrmOptions
  Left = 510
  Top = 199
  ActiveControl = chkBackup
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 442
  ClientWidth = 459
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    459
    442)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 18
    Top = 127
    Width = 49
    Height = 13
    Caption = '&Filemasks:'
  end
  object btnOK: TButton
    Left = 281
    Top = 411
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
  end
  object btnCancel: TButton
    Left = 361
    Top = 411
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object chkBackup: TCheckBox
    Left = 18
    Top = 9
    Width = 425
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Backup in *.BAK'
    TabOrder = 0
  end
  object chkWholeWords: TCheckBox
    Left = 18
    Top = 28
    Width = 425
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Replace whole words only'
    TabOrder = 1
  end
  object chkReplaceFilenames: TCheckBox
    Left = 18
    Top = 47
    Width = 425
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Replace in filenames'
    TabOrder = 2
  end
  object chkSimulate: TCheckBox
    Left = 18
    Top = 66
    Width = 425
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Simulate (Create *.SIM files)'
    TabOrder = 3
  end
  object cbFileMasks: TComboBox
    Left = 18
    Top = 145
    Width = 425
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 5
    Text = '(none)'
    Items.Strings = (
      '(none)')
  end
  object btnDelete: TButton
    Left = 361
    Top = 174
    Width = 75
    Height = 25
    Action = acDeleteMask
    Anchors = [akTop, akRight]
    TabOrder = 6
  end
  object GroupBox1: TGroupBox
    Left = 18
    Top = 202
    Width = 430
    Height = 203
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' New Filemask: '
    TabOrder = 7
    DesignSize = (
      430
      203)
    object Label2: TLabel
      Left = 12
      Top = 22
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object Label3: TLabel
      Left = 12
      Top = 70
      Width = 28
      Height = 13
      Caption = 'Mask:'
    end
    object edMaskName: TEdit
      Left = 11
      Top = 38
      Width = 407
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object edMask: TEdit
      Left = 11
      Top = 86
      Width = 407
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object btnAddMask: TButton
      Left = 343
      Top = 119
      Width = 75
      Height = 25
      Action = acAddMask
      Anchors = [akTop, akRight]
      TabOrder = 2
    end
  end
  object chkIgnoreInsideStrings: TCheckBox
    Left = 18
    Top = 85
    Width = 425
    Height = 17
    Hint = 'Ignore any words inside string definitions'
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Ignore Inside Strings'
    TabOrder = 4
  end
  object chkIgnoreInsideComments: TCheckBox
    Left = 18
    Top = 105
    Width = 425
    Height = 17
    Hint = 'Ignore any words inside comments '
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Ignore Comments'
    TabOrder = 10
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 126
    Top = 297
    object acDeleteMask: TAction
      Caption = '&Delete'
      OnExecute = acDeleteMaskExecute
    end
    object acAddMask: TAction
      Caption = '&Add'
      OnExecute = acAddMaskExecute
    end
  end
end
