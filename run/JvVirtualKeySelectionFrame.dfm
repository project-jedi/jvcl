object JvVirtualKeySelectionFrame: TJvVirtualKeySelectionFrame
  Left = 0
  Top = 0
  Width = 213
  Height = 50
  TabOrder = 0
  object lblVirtualKey: TLabel
    Left = 0
    Top = 4
    Width = 69
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Virtual key'
  end
  object lblModifiers: TLabel
    Left = 0
    Top = 32
    Width = 69
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Modifiers'
  end
  object cmbVirtualKey: TComboBox
    Left = 80
    Top = 0
    Width = 133
    Height = 21
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
  end
  object chkShift: TCheckBox
    Left = 88
    Top = 32
    Width = 57
    Height = 17
    Caption = 'Shift'
    TabOrder = 1
  end
  object chkCtrl: TCheckBox
    Left = 156
    Top = 32
    Width = 57
    Height = 17
    Caption = 'Ctrl'
    TabOrder = 2
  end
end
