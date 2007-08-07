object frmClassStructure: TfrmClassStructure
  Left = 381
  Top = 272
  BorderStyle = bsDialog
  Caption = 'frmClassStructure'
  ClientHeight = 155
  ClientWidth = 224
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
  object Button1: TButton
    Left = 57
    Top = 119
    Width = 75
    Height = 25
    Action = actOK
    Anchors = [akRight, akBottom]
    TabOrder = 0
  end
  object Button2: TButton
    Left = 137
    Top = 119
    Width = 75
    Height = 25
    Action = actCancel
    Anchors = [akRight, akBottom]
    TabOrder = 1
  end
  object rgrSource: TRadioGroup
    Left = 16
    Top = 8
    Width = 201
    Height = 65
    Caption = ' Source: '
    Items.Strings = (
      'Delphi'
      'JVCL')
    TabOrder = 2
  end
  object chbAddToOldList: TCheckBox
    Left = 16
    Top = 88
    Width = 97
    Height = 17
    Caption = 'Add to old list'
    TabOrder = 3
  end
  object ActionList1: TActionList
    Left = 160
    Top = 16
    object actOK: TAction
      Caption = 'OK'
      OnExecute = actOKExecute
    end
    object actCancel: TAction
      Caption = 'Cancel'
      OnExecute = actCancelExecute
    end
  end
end
