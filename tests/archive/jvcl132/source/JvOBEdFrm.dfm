object frmOLBarEditFrm: TfrmOLBarEditFrm
  Left = 291
  Top = 284
  BorderStyle = bsDialog
  Caption = 'OutlookBar Page and Button Editor'
  ClientHeight = 306
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    340
    306)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 5
    Top = 4
    Width = 333
    Height = 261
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = '  Pa&ges:  '
    TabOrder = 0
    DesignSize = (
      333
      261)
    object Label1: TLabel
      Left = 174
      Top = 53
      Width = 58
      Height = 13
      Anchors = [akTop, akRight]
      Caption = '&ImageIndex:'
      FocusControl = cbImages
    end
    object Label2: TLabel
      Left = 174
      Top = 16
      Width = 39
      Height = 13
      Caption = '&Caption:'
    end
    object tvOLBar: TTreeView
      Left = 14
      Top = 19
      Width = 152
      Height = 229
      Anchors = [akLeft, akTop, akRight, akBottom]
      HideSelection = False
      Indent = 19
      PopupMenu = popPages
      ReadOnly = True
      ShowButtons = False
      TabOrder = 0
      OnAddition = tvOLBarAddition
      OnChange = tvOLBarChange
      OnChanging = tvOLBarChanging
      OnCollapsing = tvOLBarCollapsing
      OnDeletion = tvOLBarDeletion
      OnKeyPress = tvOLBarKeyPress
    end
    object btnNewPage: TButton
      Left = 174
      Top = 158
      Width = 85
      Height = 25
      Action = acNewPage
      Anchors = [akRight, akBottom]
      TabOrder = 2
    end
    object btnNewButton: TButton
      Left = 174
      Top = 190
      Width = 85
      Height = 25
      Action = acNewButton
      Anchors = [akRight, akBottom]
      TabOrder = 3
    end
    object btnDelete: TButton
      Left = 174
      Top = 222
      Width = 85
      Height = 25
      Action = acDelete
      Anchors = [akRight, akBottom]
      TabOrder = 4
    end
    object cbImages: TComboBox
      Left = 174
      Top = 68
      Width = 149
      Height = 22
      Style = csOwnerDrawFixed
      Anchors = [akTop, akRight]
      DropDownCount = 10
      ItemHeight = 16
      TabOrder = 1
      OnChange = cbImagesChange
      OnDrawItem = cbImagesDrawItem
      OnKeyPress = edCaptionKeyPress
      OnMeasureItem = cbImagesMeasureItem
    end
    object edCaption: TEdit
      Left = 174
      Top = 32
      Width = 145
      Height = 21
      TabOrder = 5
      OnChange = edCaptionChange
      OnKeyPress = edCaptionKeyPress
    end
  end
  object btnOK: TButton
    Left = 95
    Top = 274
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 255
    Top = 274
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnApply: TButton
    Left = 175
    Top = 274
    Width = 75
    Height = 25
    Action = acApply
    Anchors = [akRight, akBottom]
    TabOrder = 2
  end
  object acPages: TActionList
    OnUpdate = acPagesUpdate
    Left = 29
    Top = 32
    object acNewPage: TAction
      Caption = 'New &Page'
      ShortCut = 16462
      OnExecute = acNewPageExecute
    end
    object acNewButton: TAction
      Caption = 'New &Button'
      ShortCut = 24654
      OnExecute = acNewButtonExecute
    end
    object acDelete: TAction
      Caption = '&Delete'
      ShortCut = 16430
      OnExecute = acDeleteExecute
    end
    object acMoveUp: TAction
      Caption = 'Move &Up'
      ShortCut = 16422
      OnExecute = acMoveUpExecute
    end
    object acMoveDown: TAction
      Caption = 'Move Do&wn'
      ShortCut = 16424
      OnExecute = acMoveDownExecute
    end
    object acApply: TAction
      Caption = '&Apply'
      Enabled = False
      ShortCut = 16467
      OnExecute = acApplyExecute
    end
  end
  object popPages: TPopupMenu
    Left = 30
    Top = 64
    object NewPage1: TMenuItem
      Action = acNewPage
    end
    object NewButton1: TMenuItem
      Action = acNewButton
    end
    object Delete1: TMenuItem
      Action = acDelete
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MoveUp1: TMenuItem
      Action = acMoveUp
    end
    object MoveDown1: TMenuItem
      Action = acMoveDown
    end
  end
end
