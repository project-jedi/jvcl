object frmComment: TfrmComment
  Left = 414
  Top = 204
  Width = 381
  Height = 321
  ActiveControl = reComments
  Caption = 'Comment on "%s"'
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 204
    Top = 260
    Width = 75
    Height = 25
    Action = acOK
    Anchors = [akRight, akBottom]
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 284
    Top = 260
    Width = 75
    Height = 25
    Action = acCancel
    Anchors = [akRight, akBottom]
    Cancel = True
    ModalResult = 2
    TabOrder = 2
  end
  object reComments: TJvMemo
    Left = 6
    Top = 6
    Width = 361
    Height = 246
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    PopupMenu = popEdit
    ReadOnly = False
    ScrollBars = ssBoth
    TabOrder = 0
    WantTabs = True
    WordWrap = False
  end
  object alCommentFrm: TActionList
    Left = 84
    Top = 108
    object acOK: TAction
      Caption = '&OK'
      OnExecute = acOKExecute
    end
    object acCancel: TAction
      Caption = '&Cancel'
      ShortCut = 27
      OnExecute = acCancelExecute
    end
    object acLoad: TAction
      Caption = 'Load...'
      ShortCut = 16463
      OnExecute = acLoadExecute
    end
    object acSave: TAction
      Caption = 'Save...'
      ShortCut = 16467
      OnExecute = acSaveExecute
    end
  end
  object OpenFileDlg: TJvOpenDialog
    DefaultExt = 'txt'
    Filter = 'Text files (*.txt)|*.txt|All files|*.*'
    InitialDir = '.'
    Height = 347
    Width = 563
    Left = 66
    Top = 36
  end
  object SaveFileDlg: TJvSaveDialog
    Filter = 'Text files (*.txt)|*.txt|All files|*.*'
    InitialDir = '.'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Height = 0
    Width = 0
    Left = 138
    Top = 36
  end
  object popEdit: TPopupMenu
    Left = 174
    Top = 114
    object Load1: TMenuItem
      Action = acLoad
    end
    object Save1: TMenuItem
      Action = acSave
    end
  end
end
