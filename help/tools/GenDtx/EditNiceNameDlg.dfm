object frmEditNiceName: TfrmEditNiceName
  Left = 283
  Top = 175
  Width = 370
  Height = 166
  Caption = 'frmEditNiceName'
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
  object lblClass: TLabel
    Left = 8
    Top = 8
    Width = 28
    Height = 13
    Caption = '&Class:'
    FocusControl = edtClass
  end
  object lblNiceName: TLabel
    Left = 8
    Top = 48
    Width = 54
    Height = 13
    Caption = '&Nice name:'
    FocusControl = edtNiceName
  end
  object edtClass: TEdit
    Left = 8
    Top = 24
    Width = 345
    Height = 21
    CharCase = ecUpperCase
    TabOrder = 0
    OnKeyPress = edtClassKeyPress
  end
  object Button1: TButton
    Left = 192
    Top = 96
    Width = 75
    Height = 25
    Action = actOK
    Default = True
    TabOrder = 2
  end
  object Button2: TButton
    Left = 278
    Top = 96
    Width = 75
    Height = 25
    Action = actCancel
    Cancel = True
    TabOrder = 3
  end
  object edtNiceName: TEdit
    Left = 8
    Top = 64
    Width = 345
    Height = 21
    TabOrder = 1
  end
  object ActionList1: TActionList
    Left = 16
    Top = 96
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
