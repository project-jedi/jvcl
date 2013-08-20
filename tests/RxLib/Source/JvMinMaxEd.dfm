object MinMaxInfoEditDialog: TMinMaxInfoEditDialog
  Left = 264
  Top = 158
  ActiveControl = OkBtn
  BorderStyle = bsDialog
  Caption = 'MinMaxInfo'
  ClientHeight = 163
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 3
    Top = 2
    Width = 306
    Height = 127
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 112
    Top = 15
    Width = 35
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Left: '
  end
  object Label2: TLabel
    Left = 192
    Top = 15
    Width = 37
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Top: '
  end
  object Label3: TLabel
    Left = 14
    Top = 15
    Width = 92
    Height = 13
    Caption = 'Maximize Position:  '
  end
  object Label4: TLabel
    Left = 112
    Top = 44
    Width = 35
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Width: '
  end
  object Label5: TLabel
    Left = 192
    Top = 44
    Width = 37
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Height: '
  end
  object Label6: TLabel
    Left = 14
    Top = 44
    Width = 75
    Height = 13
    Caption = 'Maximize Size:  '
  end
  object Label7: TLabel
    Left = 112
    Top = 73
    Width = 35
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Width: '
  end
  object Label8: TLabel
    Left = 192
    Top = 73
    Width = 37
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Height: '
  end
  object Label9: TLabel
    Left = 14
    Top = 73
    Width = 83
    Height = 13
    Caption = 'Max Track Size:  '
  end
  object Label10: TLabel
    Left = 112
    Top = 102
    Width = 35
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Width: '
  end
  object Label11: TLabel
    Left = 192
    Top = 102
    Width = 37
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Height: '
  end
  object Label12: TLabel
    Left = 14
    Top = 102
    Width = 80
    Height = 13
    Caption = 'Min Track Size:  '
  end
  object MaxPosBtn: TSpeedButton
    Tag = 1
    Left = 276
    Top = 9
    Width = 25
    Height = 24
    Hint = 'Set from current'#13#10'form state|'
    Caption = '¬'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Symbol'
    Font.Style = [fsBold]
    Layout = blGlyphBottom
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = SetCurrentBtnClick
  end
  object MaxSizeBtn: TSpeedButton
    Tag = 2
    Left = 276
    Top = 38
    Width = 25
    Height = 24
    Hint = 'Set from current'#13#10'form state|'
    Caption = '¬'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Symbol'
    Font.Style = [fsBold]
    Layout = blGlyphBottom
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = SetCurrentBtnClick
  end
  object MaxTrackBtn: TSpeedButton
    Tag = 3
    Left = 276
    Top = 67
    Width = 25
    Height = 24
    Hint = 'Set from current'#13#10'form state|'
    Caption = '¬'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Symbol'
    Font.Style = [fsBold]
    Layout = blGlyphBottom
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = SetCurrentBtnClick
  end
  object MinTrackBtn: TSpeedButton
    Tag = 4
    Left = 276
    Top = 96
    Width = 25
    Height = 24
    Hint = 'Set from current'#13#10'form state|'
    Caption = '¬'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Symbol'
    Font.Style = [fsBold]
    Layout = blGlyphBottom
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = SetCurrentBtnClick
  end
  object OkBtn: TButton
    Left = 163
    Top = 135
    Width = 70
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 239
    Top = 135
    Width = 70
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ClearBtn: TButton
    Left = 5
    Top = 135
    Width = 70
    Height = 23
    Caption = '&Clear'
    TabOrder = 2
    OnClick = ClearBtnClick
  end
end
