object JvEditsFrm: TJvEditsFrm
  Left = 0
  Top = 0
  Width = 541
  Height = 429
  TabOrder = 0
  object Label4: TLabel
    Left = 72
    Top = 287
    Width = 93
    Height = 16
    Caption = 'JvCurrencyEdit:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label1: TLabel
    Left = 72
    Top = 352
    Width = 70
    Height = 16
    Caption = 'JvFloatEdit:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label2: TLabel
    Left = 72
    Top = 251
    Width = 67
    Height = 16
    Caption = 'JvCalcEdit:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label3: TLabel
    Left = 72
    Top = 106
    Width = 84
    Height = 16
    Caption = 'JvComboEdit:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label7: TLabel
    Left = 72
    Top = 215
    Width = 69
    Height = 16
    Caption = 'JvDateEdit:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label8: TLabel
    Left = 72
    Top = 178
    Width = 91
    Height = 16
    Caption = 'JvDirectoryEdit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label9: TLabel
    Left = 72
    Top = 323
    Width = 73
    Height = 16
    Caption = 'JvxSpinEdit:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label10: TLabel
    Left = 72
    Top = 142
    Width = 93
    Height = 16
    Caption = 'JvFilenameEdit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label5: TLabel
    Left = 32
    Top = 16
    Width = 293
    Height = 60
    Caption = 
      'These Components are mostly taken'#13#10'from the JvX Controls TabShee' +
      't'#13#10'(except the last 2 ones)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 72
    Top = 385
    Width = 79
    Height = 16
    Caption = 'JvIpAddress:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object JvFloatEdit21: TJvFloatEdit2
    Left = 192
    Top = 352
    Width = 169
    Height = 21
    Alignment = taRightJustify
    ReadOnly = False
    TabOrder = 0
    Value = 21
    MaxDecimals = 5
    HasMaxValue = False
    HasMinValue = False
  end
  object JvCurrencyEdit1: TJvCurrencyEdit
    Left = 192
    Top = 284
    Width = 169
    Height = 21
    Alignment = taRightJustify
    ReadOnly = False
    TabOrder = 1
    Value = 1
    HasMaxValue = False
    HasMinValue = False
  end
  object JvCalcEdit1: TJvCalcEdit
    Left = 192
    Top = 248
    Width = 169
    Height = 21
    AutoSize = False
    NumGlyphs = 2
    TabOrder = 2
  end
  object JvComboEdit1: TJvComboEdit
    Left = 192
    Top = 104
    Width = 169
    Height = 21
    NumGlyphs = 1
    TabOrder = 3
    Text = 'JvComboEdit1'
    OnButtonClick = JvComboEdit1ButtonClick
  end
  object JvFilenameEdit1: TJvFilenameEdit
    Left = 192
    Top = 140
    Width = 169
    Height = 21
    NumGlyphs = 1
    TabOrder = 4
    Text = 'JvFilenameEdit1'
  end
  object JvDirectoryEdit1: TJvDirectoryEdit
    Left = 192
    Top = 176
    Width = 169
    Height = 21
    NumGlyphs = 1
    TabOrder = 5
    Text = 'JvDirectoryEdit1'
  end
  object JvDateEdit1: TJvDateEdit
    Left = 192
    Top = 212
    Width = 169
    Height = 21
    NumGlyphs = 2
    TabOrder = 6
  end
  object JvxSpinEdit1: TJvxSpinEdit
    Left = 192
    Top = 320
    Width = 169
    Height = 21
    TabOrder = 7
  end
  object JvIpAddress1: TJvIpAddress
    Left = 192
    Top = 385
    Width = 169
    Height = 21
    AddressValues.Address = 0
    AddressValues.Value1 = 0
    AddressValues.Value2 = 0
    AddressValues.Value3 = 0
    AddressValues.Value4 = 0
    TabOrder = 8
  end
end
