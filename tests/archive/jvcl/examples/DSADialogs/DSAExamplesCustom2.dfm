object frmDSAExamplesCustomDlg2: TfrmDSAExamplesCustomDlg2
  Left = 442
  Top = 422
  BorderIcons = [biSystemMenu, biHelp]
  BorderStyle = bsDialog
  Caption = 'Custom dialog 2'
  ClientHeight = 155
  ClientWidth = 175
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object lblMessage: TLabel
    Left = 10
    Top = 10
    Width = 155
    Height = 13
    AutoSize = False
    Caption = 'Select the action to take:'
  end
  object rbNone: TRadioButton
    Left = 20
    Top = 30
    Width = 125
    Height = 17
    Caption = 'Do nothing.'
    TabOrder = 0
    OnClick = UpdateBtnState
  end
  object rbUnlock: TRadioButton
    Left = 20
    Top = 50
    Width = 125
    Height = 17
    Caption = 'Unlock file'
    TabOrder = 1
    OnClick = UpdateBtnState
  end
  object rbReportError: TRadioButton
    Left = 20
    Top = 70
    Width = 125
    Height = 17
    Caption = 'Report error and stop'
    TabOrder = 2
    OnClick = UpdateBtnState
  end
  object cbSuppress: TCheckBox
    Left = 10
    Top = 95
    Width = 155
    Height = 17
    Caption = 'Stop bugging me!'
    TabOrder = 3
  end
  object btnOK: TButton
    Left = 50
    Top = 120
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 4
  end
  object JvDSADialog: TJvDSADialog
    CheckControl = cbSuppress
    DialogID = 8
    IgnoreDSAChkMrkTxt = True
    OnApplyKeys = JvDSADialogApplyKeys
    OnUpdateKeys = JvDSADialogUpdateKeys
    Left = 125
    Top = 25
  end
end
