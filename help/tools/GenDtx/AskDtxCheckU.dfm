object frmAskDtxCheck: TfrmAskDtxCheck
  Left = 525
  Top = 223
  BorderStyle = bsDialog
  Caption = 'frmAskDtxCheck'
  ClientHeight = 153
  ClientWidth = 244
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
  object lblVisibility: TLabel
    Left = 16
    Top = 16
    Width = 71
    Height = 13
    Caption = 'Check options:'
  end
  object chbDefaultText: TCheckBox
    Left = 24
    Top = 40
    Width = 145
    Height = 17
    Caption = 'Check for default text.'
    TabOrder = 0
  end
  object chbLinks: TCheckBox
    Left = 24
    Top = 64
    Width = 137
    Height = 17
    Caption = 'Check links.'
    TabOrder = 1
  end
  object chbParameters: TCheckBox
    Left = 24
    Top = 88
    Width = 137
    Height = 17
    Caption = 'Check parameters.'
    TabOrder = 2
  end
  object Button1: TButton
    Left = 84
    Top = 122
    Width = 75
    Height = 25
    Action = actOK
    Anchors = [akRight, akBottom]
    Default = True
    TabOrder = 3
  end
  object Button2: TButton
    Left = 165
    Top = 122
    Width = 75
    Height = 25
    Action = actCancel
    Anchors = [akRight, akBottom]
    Cancel = True
    TabOrder = 4
  end
  object ActionList1: TActionList
    Left = 200
    Top = 40
    object actOK: TAction
      Caption = 'OK'
      OnExecute = actOKExecute
    end
    object actCancel: TAction
      Caption = 'Cancel'
      OnExecute = actCancelExecute
    end
  end
  object JvAppRegistryStore1: TJvAppRegistryStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    Root = 'Software\JVCL\GenDtx'
    SubStorages = <>
    Left = 196
    Top = 76
  end
end
