inherited frmJvSLDMappingEditor: TfrmJvSLDMappingEditor
  Caption = 'Segmented LED Display Mapping Editor...'
  ClientHeight = 175
  ClientWidth = 410
  Constraints.MaxHeight = 221
  Constraints.MinHeight = 221
  Constraints.MinWidth = 348
  Menu = fmeMapper.mnuCharMapEdit
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object lblDigitClassCaption: TLabel
    Left = 120
    Top = 15
    Width = 51
    Height = 13
    Caption = 'Digit class:'
  end
  object lblSegmentCountCaption: TLabel
    Left = 120
    Top = 40
    Width = 70
    Height = 13
    Caption = '# of segments:'
  end
  object lblCharCaption: TLabel
    Left = 120
    Top = 65
    Width = 49
    Height = 13
    Caption = 'Character:'
  end
  object lblMapperValueCaption: TLabel
    Left = 120
    Top = 90
    Width = 73
    Height = 13
    Caption = 'Mapping value:'
  end
  object lblSegmentsCaption: TLabel
    Left = 120
    Top = 115
    Width = 50
    Height = 13
    Caption = 'Segments:'
  end
  object lblDigitClass: TLabel
    Left = 205
    Top = 15
    Width = 200
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblSegmentCount: TLabel
    Left = 205
    Top = 40
    Width = 200
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblChar: TLabel
    Left = 205
    Top = 65
    Width = 200
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblMapperValue: TLabel
    Left = 205
    Top = 90
    Width = 200
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblSegments: TLabel
    Left = 205
    Top = 115
    Width = 200
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  inline fmeMapper: TfmeJvSegmentedLEDDisplayMapper
    Left = 5
    Top = 5
  end
  object btnOK: TButton
    Left = 330
    Top = 145
    Width = 75
    Height = 25
    Action = fmeMapper.aiFileClose
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
  end
end
