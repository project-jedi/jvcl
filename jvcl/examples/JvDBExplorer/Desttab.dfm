object DestTableDlg: TDestTableDlg
  Left = 230
  Top = 129
  BorderStyle = bsDialog
  Caption = 'Destination Table Properties'
  ClientHeight = 208
  ClientWidth = 324
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefaultPosOnly
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 6
    Top = 4
    Width = 119
    Height = 13
    Caption = '&Select destination table:  '
    FocusControl = TabnameEdit
  end
  object OkBtn: TButton
    Left = 159
    Top = 177
    Width = 77
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 243
    Top = 177
    Width = 77
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object TypeBtn: TRadioGroup
    Left = 6
    Top = 47
    Width = 313
    Height = 45
    Caption = ' &Table Type '
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      '  &Paradox  '
      '  &DBase  '
      '  &ASCII')
    TabOrder = 1
    OnClick = TypeBtnClick
  end
  object TabnameEdit: TJvFilenameEdit
    Left = 6
    Top = 20
    Width = 313
    Height = 21
    OnAfterDialog = TabnameEditAfterDialog
    AddQuotes = False
    DialogKind = dkSave
    DefaultExt = 'DB'
    Filter = 
      'Paradox files (*.db)|*.DB|DBase files (*.dbf)|*.DBF|ASCII files ' +
      '(*.txt)|*.TXT|All files (*.*)|*.*'
    DialogOptions = [ofHideReadOnly, ofPathMustExist]
    DialogTitle = 'Browse files'
    ButtonHint = 'Browse files|'
    ButtonFlat = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnChange = TabnameEditChange
  end
  object RecordCountBox: TGroupBox
    Left = 6
    Top = 97
    Width = 313
    Height = 73
    Caption = ' &Record count to export '
    TabOrder = 2
    object Label2: TLabel
      Left = 132
      Top = 45
      Width = 139
      Height = 13
      Caption = 'records from current position  '
    end
    object FirstRecsBtn: TRadioButton
      Left = 17
      Top = 44
      Width = 48
      Height = 17
      Caption = ' &First '
      TabOrder = 1
      OnClick = RecordCountBtnClick
    end
    object AllRecsBtn: TRadioButton
      Left = 17
      Top = 22
      Width = 140
      Height = 17
      Caption = ' &All records'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RecordCountBtnClick
    end
    object RecordCntEdit: TJvValidateEdit
      Left = 68
      Top = 43
      Width = 55
      Height = 20
      AutoSize = False
      CheckChars = '01234567890'
      CriticalPoints.CheckPoints = cpNone
      CriticalPoints.ColorAbove = clBlue
      CriticalPoints.ColorBelow = clRed
      EditText = '0'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnFace
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 10
      MaxValue = 2147483647.000000000000000000
      ParentColor = True
      ParentFont = False
      PasswordChar = #0
      TabOrder = 2
      Text = '0'
      Value = 0
    end
  end
  object FormStorage: TJvFormStorage
    Options = [fpPosition]
    StoredProps.Strings = (
      'TypeBtn.ItemIndex'
      'TabnameEdit.InitialDir'
      'AllRecsBtn.Checked'
      'FirstRecsBtn.Checked'
      'RecordCntEdit.Value')
    StoredValues = <>
    Left = 264
    Top = 8
  end
end
