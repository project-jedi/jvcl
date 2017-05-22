object frmJvDBGridControlsEditor: TfrmJvDBGridControlsEditor
  Left = 201
  Top = 176
  BorderStyle = bsDialog
  Caption = 'Set Grid Edit Controls'
  ClientHeight = 260
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object sbAdd: TSpeedButton
    Left = 176
    Top = 84
    Width = 25
    Height = 25
    Caption = '>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    NumGlyphs = 2
    ParentFont = False
    OnClick = sbAddClick
  end
  object sbDelete: TSpeedButton
    Left = 176
    Top = 124
    Width = 25
    Height = 25
    Caption = '<'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    NumGlyphs = 2
    ParentFont = False
    OnClick = sbDeleteClick
  end
  object GroupBoxFields: TGroupBox
    Left = 8
    Top = 5
    Width = 161
    Height = 217
    Caption = ' Current Fields '
    TabOrder = 0
    object lbFields: TListBox
      Left = 8
      Top = 24
      Width = 145
      Height = 185
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object GroupBoxSelected: TGroupBox
    Left = 208
    Top = 5
    Width = 313
    Height = 217
    Caption = ' Selected Fields '
    TabOrder = 1
    object LabelControl: TLabel
      Left = 160
      Top = 48
      Width = 57
      Height = 13
      Caption = 'Edit Control:'
    end
    object LabelFillCell: TLabel
      Left = 160
      Top = 100
      Width = 65
      Height = 13
      Caption = 'Runtime Size:'
    end
    object lbSelected: TListBox
      Left = 8
      Top = 24
      Width = 145
      Height = 185
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbSelectedClick
    end
    object cbControl: TComboBox
      Left = 160
      Top = 64
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnClick = cbControlClick
    end
    object cbFillCell: TComboBox
      Left = 160
      Top = 116
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnClick = cbFillCellClick
      Items.Strings = (
        'Cell size'
        'Design size'
        'Design/Cell size (biggest)')
    end
    object cbLeaveOnEnterKey: TCheckBox
      Left = 160
      Top = 152
      Width = 145
      Height = 17
      Caption = 'Return/Enter key = leave'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = cbLeaveOnEnterKeyClick
    end
    object cbLeaveOnUpDownKey: TCheckBox
      Left = 160
      Top = 168
      Width = 145
      Height = 17
      Caption = 'Up/Down key = leave'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = cbLeaveOnUpDownKeyClick
    end
  end
  object btnOK: TButton
    Left = 357
    Top = 230
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 448
    Top = 230
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
