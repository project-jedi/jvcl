inherited fmeJvStdToolbarDesign: TfmeJvStdToolbarDesign
  inherited tbrToolbar: TToolBar
    ButtonWidth = 90
    object tbAddItem: TToolButton
      Left = 0
      Top = 0
      Action = aiAddItem
    end
    object tbDeleteItem: TToolButton
      Left = 90
      Top = 0
      Action = aiDeleteItem
    end
    object tbDeleteSubItems: TToolButton
      Left = 180
      Top = 0
      Action = aiDeleteSubItems
    end
    object tbDivider1: TToolButton
      Left = 270
      Top = 0
      Width = 8
      Caption = 'tbDivider1'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object tbMoveUp: TToolButton
      Left = 278
      Top = 0
      Action = aiMoveUp
    end
    object tbMoveDown: TToolButton
      Left = 368
      Top = 0
      Action = aiMoveDown
    end
  end
  inherited ilToolbar: TImageList
    Left = 300
  end
  inherited pmToolbar: TPopupMenu
    Left = 165
  end
  inherited aiToolbar: TActionList
    Images = ilToolbar
    Left = 72
    object aiAddItem: TAction
      Caption = '&Add item'
      ImageIndex = 0
    end
    object aiDeleteItem: TAction
      Caption = '&Delete item'
      ImageIndex = 1
    end
    object aiDeleteSubItems: TAction
      Caption = 'Delete &Subitems'
      ImageIndex = 2
    end
    object aiMoveUp: TAction
      Caption = 'Move &Up'
      ImageIndex = 3
    end
    object aiMoveDown: TAction
      Caption = 'Move &Down'
      ImageIndex = 4
    end
  end
end
