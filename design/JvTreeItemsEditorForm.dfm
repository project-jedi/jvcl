object frmTreeViewItems: TfrmTreeViewItems
  Left = 281
  Top = 187
  AutoScroll = False
  BorderWidth = 2
  Caption = 'TreeView Items Editor'
  ClientHeight = 204
  ClientWidth = 443
  Color = clBtnFace
  Constraints.MinHeight = 235
  Constraints.MinWidth = 455
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 129
    Top = 0
    Width = 3
    Height = 164
    Cursor = crHSplit
  end
  object Panel1: TPanel
    Left = 0
    Top = 164
    Width = 443
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 1
    Caption = ' '
    TabOrder = 0
    object Bevel1: TBevel
      Left = 1
      Top = 1
      Width = 441
      Height = 3
      Align = alTop
      Shape = bsTopLine
    end
    object btnCancel: TButton
      Left = 359
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object btnOK: TButton
      Left = 279
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
  object tvItems: TTreeView
    Left = 0
    Top = 0
    Width = 129
    Height = 164
    Align = alLeft
    DragMode = dmAutomatic
    HideSelection = False
    Indent = 19
    PopupMenu = PopupMenu1
    ReadOnly = True
    TabOrder = 1
    OnChange = tvItemsChange
    OnDragDrop = tvItemsDragDrop
    OnDragOver = tvItemsDragOver
    OnStartDrag = tvItemsStartDrag
  end
  object Panel2: TPanel
    Left = 132
    Top = 0
    Width = 311
    Height = 164
    Align = alClient
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 2
    object btnNew: TButton
      Left = 16
      Top = 63
      Width = 80
      Height = 25
      Action = acNewItem
      Anchors = [akLeft, akBottom]
      TabOrder = 0
    end
    object btnNewSub: TButton
      Left = 16
      Top = 95
      Width = 80
      Height = 25
      Action = acNewSubItem
      Anchors = [akLeft, akBottom]
      TabOrder = 1
    end
    object btnDelete: TButton
      Left = 16
      Top = 127
      Width = 80
      Height = 25
      Action = acDelete
      Anchors = [akLeft, akBottom]
      TabOrder = 2
    end
    object gbProperties: TGroupBox
      Left = 112
      Top = 0
      Width = 195
      Height = 160
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = ' Properties: '
      TabOrder = 3
      object Label1: TLabel
        Left = 16
        Top = 24
        Width = 24
        Height = 13
        Caption = '&Text:'
      end
      object Label2: TLabel
        Left = 16
        Top = 56
        Width = 61
        Height = 13
        Caption = '&Image Index:'
      end
      object Label3: TLabel
        Left = 16
        Top = 88
        Width = 74
        Height = 13
        Caption = '&Selected Index:'
      end
      object Label4: TLabel
        Left = 16
        Top = 120
        Width = 57
        Height = 13
        Caption = 'State Inde&x:'
      end
      object cbImage: TComboBox
        Left = 100
        Top = 53
        Width = 88
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        OnChange = edNodeTextChange
        OnDrawItem = cbImageIndexDrawItem
      end
      object cbSelected: TComboBox
        Left = 100
        Top = 85
        Width = 88
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 1
        OnChange = edNodeTextChange
        OnDrawItem = cbImageIndexDrawItem
      end
      object cbState: TComboBox
        Left = 100
        Top = 117
        Width = 88
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 2
        OnChange = edNodeTextChange
        OnDrawItem = cbStateDrawItem
      end
      object edNodeText: TEdit
        Left = 58
        Top = 19
        Width = 127
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        OnChange = edNodeTextChange
      end
    end
  end
  object acItems: TActionList
    OnUpdate = acItemsUpdate
    Left = 72
    Top = 24
    object acNewItem: TAction
      Category = 'TreeView'
      Caption = '&New Item'
      ShortCut = 45
      OnExecute = acNewItemExecute
    end
    object acNewSubItem: TAction
      Category = 'TreeView'
      Caption = 'N&ew SubItem'
      Enabled = False
      ShortCut = 16429
      OnExecute = acNewSubItemExecute
    end
    object acDelete: TAction
      Category = 'TreeView'
      Caption = '&Delete'
      Enabled = False
      ShortCut = 16430
      OnExecute = acDeleteExecute
    end
    object acNodeMoveLeft: TAction
      Category = 'TreeView'
      Caption = 'Move left'
      ShortCut = 16421
      OnExecute = acNodeMoveLeftExecute
    end
    object acNodeMoveRight: TAction
      Category = 'TreeView'
      Caption = 'Move right'
      ShortCut = 16423
      OnExecute = acNodeMoveRightExecute
    end
    object acNodeMoveUp: TAction
      Category = 'TreeView'
      Caption = 'Move up'
      ShortCut = 16422
      OnExecute = acNodeMoveUpExecute
    end
    object acNodeMoveDown: TAction
      Category = 'TreeView'
      Caption = 'Move down'
      ShortCut = 16424
      OnExecute = acNodeMoveDownExecute
    end
    object acLoadFromFile: TAction
      Category = 'TreeView'
      Caption = 'Load...'
      ShortCut = 16463
      OnExecute = acLoadFromFileExecute
    end
    object acSaveToFile: TAction
      Category = 'TreeView'
      Caption = 'Save...'
      ShortCut = 16467
      OnExecute = acSaveToFileExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 40
    Top = 24
    object Moveup1: TMenuItem
      Action = acNodeMoveUp
    end
    object Movedown1: TMenuItem
      Action = acNodeMoveDown
    end
    object Moveleft1: TMenuItem
      Action = acNodeMoveLeft
    end
    object Moveright1: TMenuItem
      Action = acNodeMoveRight
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object LoadFromFile1: TMenuItem
      Action = acLoadFromFile
    end
    object SaveToFile1: TMenuItem
      Action = acSaveToFile
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 74
    Top = 56
  end
  object OpenDialog1: TOpenDialog
    Left = 42
    Top = 56
  end
end
