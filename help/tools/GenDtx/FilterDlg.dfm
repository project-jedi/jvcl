object frmFilter: TfrmFilter
  Left = 426
  Top = 245
  Width = 380
  Height = 405
  Caption = 'frmFilter'
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
  object Bevel1: TBevel
    Left = 0
    Top = 224
    Width = 369
    Height = 9
    Shape = bsBottomLine
  end
  object Bevel2: TBevel
    Left = 0
    Top = 320
    Width = 369
    Height = 9
    Shape = bsBottomLine
  end
  object chbClasses: TCheckBox
    Left = 16
    Top = 16
    Width = 97
    Height = 17
    Caption = 'Class'
    TabOrder = 0
  end
  object chbConst: TCheckBox
    Left = 160
    Top = 88
    Width = 97
    Height = 17
    Caption = 'Const'
    TabOrder = 1
  end
  object chbDispInterface: TCheckBox
    Left = 160
    Top = 16
    Width = 97
    Height = 17
    Caption = 'DispInterface'
    TabOrder = 2
  end
  object chbFunction: TCheckBox
    Left = 16
    Top = 128
    Width = 97
    Height = 17
    Caption = 'Function'
    TabOrder = 3
  end
  object chbFunctionType: TCheckBox
    Left = 16
    Top = 152
    Width = 97
    Height = 17
    Caption = 'FunctionType'
    TabOrder = 4
  end
  object chbInterface: TCheckBox
    Left = 160
    Top = 40
    Width = 97
    Height = 17
    Caption = 'Interface'
    TabOrder = 5
  end
  object chbMethodFunc: TCheckBox
    Left = 16
    Top = 64
    Width = 97
    Height = 17
    Caption = 'MethodFunc'
    TabOrder = 6
  end
  object chbMethodProc: TCheckBox
    Left = 16
    Top = 88
    Width = 97
    Height = 17
    Caption = 'MethodProc'
    TabOrder = 7
  end
  object chbProcedure: TCheckBox
    Left = 16
    Top = 176
    Width = 81
    Height = 17
    Caption = 'Procedure'
    TabOrder = 8
  end
  object chbProcedureType: TCheckBox
    Left = 16
    Top = 200
    Width = 97
    Height = 17
    Caption = 'Procedure Type'
    TabOrder = 9
  end
  object chbProperty: TCheckBox
    Left = 16
    Top = 40
    Width = 97
    Height = 17
    Caption = 'Property'
    TabOrder = 10
  end
  object chbRecord: TCheckBox
    Left = 160
    Top = 184
    Width = 97
    Height = 17
    Caption = 'Record'
    TabOrder = 11
  end
  object chbResourcestring: TCheckBox
    Left = 160
    Top = 208
    Width = 97
    Height = 17
    Caption = 'Resourcestring'
    TabOrder = 12
  end
  object chbEnum: TCheckBox
    Left = 160
    Top = 160
    Width = 97
    Height = 17
    Caption = 'Enumerate'
    TabOrder = 13
  end
  object chbType: TCheckBox
    Left = 160
    Top = 112
    Width = 97
    Height = 17
    Caption = 'Type'
    TabOrder = 14
  end
  object chbVar: TCheckBox
    Left = 160
    Top = 136
    Width = 97
    Height = 17
    Caption = 'Variable'
    TabOrder = 15
  end
  object Button1: TButton
    Left = 200
    Top = 336
    Width = 75
    Height = 25
    Action = actOK
    Default = True
    TabOrder = 16
  end
  object Button2: TButton
    Left = 288
    Top = 336
    Width = 75
    Height = 25
    Action = actCancel
    Cancel = True
    TabOrder = 17
  end
  object rgrDuplicatesOrUnique: TRadioGroup
    Left = 8
    Top = 240
    Width = 201
    Height = 81
    Caption = ' Show Duplicates? '
    Items.Strings = (
      'Hide duplicates'
      'Hide duplicates (case sensitive)'
      'Show only duplicates'
      'Show only case sensitive duplicates'
      'Show All')
    TabOrder = 18
  end
  object ActionList1: TActionList
    Left = 312
    Top = 112
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
