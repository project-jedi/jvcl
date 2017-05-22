object fmeJvSegmentedLEDDisplayMapper: TfmeJvSegmentedLEDDisplayMapper
  Left = 0
  Top = 0
  Width = 105
  Height = 135
  Color = clBlack
  ParentColor = False
  PopupMenu = pmDigit
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
    PopupMenu = pmDigit
    SegmentLitColor = clRed
    SegmentSpacing = 4
    SegmentThickness = 8
    SegmentUnlitColor = clMaroon
    Slant = 10
    OnClick = sldEditClick
    OnMouseDown = sldEditMouseDown
  end
  object pmDigit: TPopupMenu
    Left = 5
    Top = 55
    object miSetStates: TMenuItem
      Action = aiEditSetAll
    end
    object miClearStates: TMenuItem
      Action = aiEditClear
    end
    object miInvertStates: TMenuItem
      Action = aiEditInvert
    end
  end
  object mnuCharMapEdit: TMainMenu
    Left = 5
    Top = 10
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Action = aiFileOpen
      end
      object Save1: TMenuItem
        Action = aiFileSave
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Default1: TMenuItem
        Action = aiFileLoadDefault
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Action = aiFileClose
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Copy1: TMenuItem
        Action = aiEditCopy
      end
      object Paste1: TMenuItem
        Action = aiEditPaste
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Selectchar1: TMenuItem
        Action = aiEditSelectChar
      end
      object Apply1: TMenuItem
        Action = aiEditApply
      end
      object Revert1: TMenuItem
        Action = aiEditRevert
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Setallsegments1: TMenuItem
        Action = aiEditSetAll
      end
      object Emptysegments1: TMenuItem
        Action = aiEditClear
      end
      object Invertsegments1: TMenuItem
        Action = aiEditInvert
      end
    end
  end
  object alCharMapEditor: TActionList
    OnUpdate = alCharMapEditorUpdate
    Left = 5
    Top = 100
    object aiFileOpen: TAction
      Caption = '&Open...'
      ShortCut = 16463
      OnExecute = aiFileOpenExecute
    end
    object aiFileSave: TAction
      Caption = '&Save...'
      ShortCut = 16467
      OnExecute = aiFileSaveExecute
    end
    object aiFileLoadDefault: TAction
      Caption = '&Default'
      OnExecute = aiFileLoadDefaultExecute
    end
    object aiFileClose: TAction
      Caption = '&Close'
      ShortCut = 32883
      OnExecute = aiFileCloseExecute
    end
    object aiEditCopy: TAction
      Caption = '&Copy'
      ShortCut = 16451
      OnExecute = aiEditCopyExecute
    end
    object aiEditPaste: TAction
      Caption = '&Paste'
      ShortCut = 16470
      OnExecute = aiEditPasteExecute
    end
    object aiEditClear: TAction
      Caption = '&Empty segments'
      OnExecute = aiEditClearExecute
    end
    object aiEditSetAll: TAction
      Caption = '&Set all segments'
      OnExecute = aiEditSetAllExecute
    end
    object aiEditInvert: TAction
      Caption = '&Invert segments'
      OnExecute = aiEditInvertExecute
    end
    object aiEditSelectChar: TAction
      Caption = 'Select c&har...'
      OnExecute = aiEditSelectCharExecute
    end
    object aiEditRevert: TAction
      Caption = '&Revert'
      OnExecute = aiEditRevertExecute
    end
    object aiEditApply: TAction
      Caption = '&Apply'
      OnExecute = aiEditApplyExecute
    end
  end
end
