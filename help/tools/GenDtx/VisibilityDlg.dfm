object frmVisibility: TfrmVisibility
  Left = 362
  Top = 223
  BorderStyle = bsDialog
  Caption = 'frmVisibility'
  ClientHeight = 193
  ClientWidth = 277
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
    Width = 186
    Height = 13
    Caption = 'Generate tags for these class members:'
  end
  object chbPrivate: TCheckBox
    Left = 24
    Top = 40
    Width = 145
    Height = 17
    Caption = 'Private members'
    TabOrder = 0
  end
  object chbProtected: TCheckBox
    Left = 24
    Top = 64
    Width = 137
    Height = 17
    Caption = 'Protected members'
    TabOrder = 1
  end
  object chbPublic: TCheckBox
    Left = 24
    Top = 88
    Width = 137
    Height = 17
    Caption = 'Public members'
    TabOrder = 2
  end
  object chbPublished: TCheckBox
    Left = 24
    Top = 112
    Width = 129
    Height = 17
    Caption = 'Published members'
    TabOrder = 3
  end
  object Button1: TButton
    Left = 104
    Top = 160
    Width = 75
    Height = 25
    Action = actOK
    Default = True
    TabOrder = 4
  end
  object Button2: TButton
    Left = 192
    Top = 160
    Width = 75
    Height = 25
    Action = actCancel
    Cancel = True
    TabOrder = 5
  end
  object Button3: TButton
    Left = 16
    Top = 160
    Width = 75
    Height = 25
    Action = actDefault
    TabOrder = 6
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
    object actDefault: TAction
      Caption = 'Default'
      OnExecute = actDefaultExecute
    end
  end
end
