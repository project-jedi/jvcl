object frmInput: TfrmInput
  Left = 283
  Top = 175
  Width = 405
  Height = 132
  Caption = 'frmInput'
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
  object lblNaam: TLabel
    Left = 8
    Top = 8
    Width = 28
    Height = 13
    Caption = '&Name'
  end
  object Button1: TButton
    Left = 224
    Top = 64
    Width = 75
    Height = 25
    Action = actOK
    Default = True
    TabOrder = 1
  end
  object Button2: TButton
    Left = 312
    Top = 64
    Width = 75
    Height = 25
    Action = actCancel
    Cancel = True
    TabOrder = 2
  end
  object edtName: TEdit
    Left = 8
    Top = 24
    Width = 385
    Height = 21
    CharCase = ecUpperCase
    TabOrder = 0
    OnKeyPress = edtNameKeyPress
  end
  object ActionList1: TActionList
    Left = 24
    Top = 64
    object actOK: TAction
      Caption = 'OK'
      OnExecute = actOKExecute
      OnUpdate = actOKUpdate
    end
    object actCancel: TAction
      Caption = 'Cancel'
      OnExecute = actCancelExecute
    end
  end
end
