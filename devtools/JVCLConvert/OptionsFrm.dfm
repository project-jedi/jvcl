object frmOptions: TfrmOptions
  Left = 510
  Top = 199
  ActiveControl = chkBackup
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 399
  ClientWidth = 456
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 18
    Top = 102
    Width = 49
    Height = 13
    Caption = '&Filemasks:'
  end
  object btnOK: TButton
    Left = 279
    Top = 355
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object btnCancel: TButton
    Left = 359
    Top = 355
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object chkBackup: TCheckBox
    Left = 18
    Top = 18
    Width = 422
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Backup in *.BAK'
    TabOrder = 0
  end
  object chkWholeWords: TCheckBox
    Left = 18
    Top = 36
    Width = 422
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Replace whole words only'
    TabOrder = 1
  end
  object chkReplaceFilenames: TCheckBox
    Left = 18
    Top = 54
    Width = 422
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Replace in filenames'
    TabOrder = 2
  end
  object chkSimulate: TCheckBox
    Left = 18
    Top = 72
    Width = 422
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Simulate (report result)'
    TabOrder = 3
  end
  object cbFileMasks: TComboBox
    Left = 18
    Top = 120
    Width = 422
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 4
    Text = '(none)'
    Items.Strings = (
      '(none)')
  end
  object btnDelete: TButton
    Left = 358
    Top = 149
    Width = 75
    Height = 25
    Action = acDeleteMask
    Anchors = [akTop, akRight]
    TabOrder = 5
  end
  object GroupBox1: TGroupBox
    Left = 18
    Top = 177
    Width = 427
    Height = 160
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' New Filemask: '
    TabOrder = 6
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
      Width = 404
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object edMask: TEdit
      Left = 11
      Top = 86
      Width = 404
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object btnAddMask: TButton
      Left = 334
      Top = 119
      Width = 75
      Height = 25
      Action = acAddMask
      Anchors = [akTop, akRight]
      TabOrder = 2
    end
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 126
    Top = 282
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
