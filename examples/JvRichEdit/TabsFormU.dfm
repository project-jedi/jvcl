object TabsForm: TTabsForm
  Left = 470
  Top = 191
  BorderStyle = bsDialog
  Caption = 'Tabs'
  ClientHeight = 319
  ClientWidth = 288
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel3: TBevel
    Left = 6
    Top = 278
    Width = 276
    Height = 9
    Shape = bsTopLine
  end
  object StaticText4: TStaticText
    Left = 147
    Top = 8
    Width = 106
    Height = 17
    Caption = '&Measurement units:'
    FocusControl = cmbMeasurementUnits
    TabOrder = 10
  end
  object StaticText5: TStaticText
    Left = 6
    Top = 8
    Width = 97
    Height = 17
    Caption = '&Tab stop position:'
    FocusControl = edtTabStopPosition
    TabOrder = 11
  end
  object edtTabStopPosition: TEdit
    Left = 6
    Top = 24
    Width = 135
    Height = 20
    AutoSize = False
    HideSelection = False
    TabOrder = 0
    OnKeyDown = edtTabStopPositionKeyDown
  end
  object lsbTabStopPositions: TListBox
    Left = 12
    Top = 44
    Width = 129
    Height = 69
    TabStop = False
    ItemHeight = 16
    Style = lbOwnerDrawFixed
    TabOrder = 1
    OnClick = lsbTabStopPositionsClick
    OnDrawItem = lsbTabStopPositionsDrawItem
  end
  object pnlLeader: TPanel
    Left = 2
    Top = 175
    Width = 283
    Height = 65
    BevelOuter = bvNone
    TabOrder = 4
    object Bevel2: TBevel
      Left = 43
      Top = 8
      Width = 237
      Height = 9
      Shape = bsTopLine
    end
    object StaticText3: TStaticText
      Left = 4
      Top = 2
      Width = 37
      Height = 17
      Caption = 'Leader'
      TabOrder = 5
    end
    object rbtLeader1: TRadioButton
      Left = 13
      Top = 19
      Width = 84
      Height = 17
      Caption = '&1 None'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbtLeader2: TRadioButton
      Left = 103
      Top = 19
      Width = 84
      Height = 17
      Caption = '&2 .......'
      TabOrder = 1
    end
    object rbtLeader3: TRadioButton
      Left = 193
      Top = 19
      Width = 84
      Height = 17
      Caption = '&3 -------'
      TabOrder = 2
    end
    object rbtLeader4: TRadioButton
      Left = 13
      Top = 37
      Width = 84
      Height = 17
      Caption = '&4 ____'
      TabOrder = 3
    end
    object rbtLeader5: TRadioButton
      Left = 103
      Top = 37
      Width = 84
      Height = 17
      Caption = '&5 ===='
      TabOrder = 4
    end
  end
  object pnlAlignment: TPanel
    Left = 2
    Top = 114
    Width = 283
    Height = 56
    BevelOuter = bvNone
    TabOrder = 3
    object Bevel1: TBevel
      Left = 57
      Top = 7
      Width = 223
      Height = 9
      Shape = bsTopLine
    end
    object StaticText1: TStaticText
      Left = 4
      Top = 1
      Width = 51
      Height = 17
      Caption = 'Alignment'
      TabOrder = 0
    end
    object rbtAlignment1: TRadioButton
      Left = 13
      Top = 18
      Width = 84
      Height = 17
      Caption = '&Left'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = AlignmentClick
    end
    object rbtAlignment2: TRadioButton
      Left = 103
      Top = 18
      Width = 84
      Height = 17
      Caption = '&Center'
      TabOrder = 2
      OnClick = AlignmentClick
    end
    object rbtAlignment5: TRadioButton
      Left = 103
      Top = 36
      Width = 84
      Height = 17
      Caption = '&Bar'
      TabOrder = 3
      OnClick = AlignmentClick
    end
    object rbtAlignment3: TRadioButton
      Left = 193
      Top = 18
      Width = 84
      Height = 17
      Caption = '&Right'
      TabOrder = 4
      OnClick = AlignmentClick
    end
    object rbtAlignment4: TRadioButton
      Left = 13
      Top = 36
      Width = 84
      Height = 17
      Caption = '&Decimal'
      TabOrder = 5
      OnClick = AlignmentClick
    end
  end
  object Button1: TButton
    Left = 46
    Top = 246
    Width = 74
    Height = 22
    Action = actSet
    TabOrder = 5
  end
  object Button2: TButton
    Left = 127
    Top = 246
    Width = 74
    Height = 22
    Action = actClear
    TabOrder = 6
  end
  object Button3: TButton
    Left = 207
    Top = 246
    Width = 74
    Height = 22
    Action = actClearAll
    TabOrder = 7
  end
  object Button4: TButton
    Left = 127
    Top = 290
    Width = 74
    Height = 22
    Action = actOK
    Default = True
    TabOrder = 8
  end
  object Button5: TButton
    Left = 208
    Top = 290
    Width = 74
    Height = 22
    Action = actCancel
    Cancel = True
    TabOrder = 9
  end
  object cmbMeasurementUnits: TComboBox
    Left = 147
    Top = 24
    Width = 138
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 2
    OnChange = cmbMeasurementUnitsChange
  end
  object ActionList1: TActionList
    Left = 200
    Top = 80
    object actSet: TAction
      Caption = '&Set'
      OnExecute = actSetExecute
      OnUpdate = IsTabStopFilled
    end
    object actClear: TAction
      Caption = 'Cl&ear'
      OnExecute = actClearExecute
      OnUpdate = IsTabStopFilled
    end
    object actClearAll: TAction
      Caption = 'Clear &All'
      OnExecute = actClearAllExecute
    end
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
