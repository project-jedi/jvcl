inherited frmJvSLDMappingEditor: TfrmJvSLDMappingEditor
  Caption = 'Segmented LED Display Mapping Editor...'
  ClientHeight = 175
  ClientWidth = 410
  Constraints.MaxHeight = 224
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
    Width = 128
    Height = 135
    Color = clBlack
    ParentColor = False
    PopupMenu = fmeMapper.pmDigit
    TabOrder = 0
    object sldEdit: TJvSegmentedLEDDisplay
      Left = 5
      Top = 5
      Width = 95
      Height = 125
      AutoSize = False
      DigitClassName = 'TJv7SegmentedLEDDigit'
      DigitHeight = 90
      Digits = <>
      DigitWidth = 60
      DotSize = 12
      PopupMenu = fmeMapper.pmDigit
      SegmentLitColor = clRed
      SegmentSpacing = 4
      SegmentThickness = 8
      SegmentUnlitColor = clMaroon
      Slant = 10
    end
    object pmDigit: TPopupMenu
      Left = 5
      Top = 55
      object miSetStates: TMenuItem
        Action = fmeMapper.aiEditSetAll
      end
      object miClearStates: TMenuItem
        Action = fmeMapper.aiEditClear
      end
      object miInvertStates: TMenuItem
        Action = fmeMapper.aiEditInvert
      end
    end
    object mnuCharMapEdit: TMainMenu
      Left = 5
      Top = 10
      object File1: TMenuItem
        Caption = '&File'
        object Open1: TMenuItem
          Action = fmeMapper.aiFileOpen
        end
        object Save1: TMenuItem
          Action = fmeMapper.aiFileSave
        end
        object N1: TMenuItem
          Caption = '-'
        end
        object Default1: TMenuItem
          Action = fmeMapper.aiFileLoadDefault
        end
        object N2: TMenuItem
          Caption = '-'
        end
        object Close1: TMenuItem
          Action = fmeMapper.aiFileClose
        end
      end
      object Edit1: TMenuItem
        Caption = '&Edit'
        object Copy1: TMenuItem
          Action = fmeMapper.aiEditCopy
        end
        object Paste1: TMenuItem
          Action = fmeMapper.aiEditPaste
        end
        object N3: TMenuItem
          Caption = '-'
        end
        object Selectchar1: TMenuItem
          Action = fmeMapper.aiEditSelectChar
        end
        object Apply1: TMenuItem
          Action = fmeMapper.aiEditApply
        end
        object Revert1: TMenuItem
          Action = fmeMapper.aiEditRevert
        end
        object N4: TMenuItem
          Caption = '-'
        end
        object Setallsegments1: TMenuItem
          Action = fmeMapper.aiEditSetAll
        end
        object Emptysegments1: TMenuItem
          Action = fmeMapper.aiEditClear
        end
        object Invertsegments1: TMenuItem
          Action = fmeMapper.aiEditInvert
        end
      end
    end
    object alCharMapEditor: TActionList
      Left = 5
      Top = 100
      object aiFileOpen: TAction
        Caption = '&Open...'
        ShortCut = 16463
      end
      object aiFileSave: TAction
        Caption = '&Save...'
        ShortCut = 16467
      end
      object aiFileLoadDefault: TAction
        Caption = '&Default'
      end
      object aiFileClose: TAction
        Caption = '&Close'
        ShortCut = 32883
      end
      object aiEditCopy: TAction
        Caption = '&Copy'
        ShortCut = 16451
      end
      object aiEditPaste: TAction
        Caption = '&Paste'
        ShortCut = 16470
      end
      object aiEditClear: TAction
        Caption = '&Empty segments'
      end
      object aiEditSetAll: TAction
        Caption = '&Set all segments'
      end
      object aiEditInvert: TAction
        Caption = '&Invert segments'
      end
      object aiEditSelectChar: TAction
        Caption = 'Select c&har...'
      end
      object aiEditRevert: TAction
        Caption = '&Revert'
      end
      object aiEditApply: TAction
        Caption = '&Apply'
      end
    end
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
