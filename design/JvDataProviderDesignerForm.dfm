inherited frmDataProviderDesigner: TfrmDataProviderDesigner
  Left = 312
  Top = 325
  Caption = 'frmDataProviderDesigner'
  ClientWidth = 621
  OldCreateOrder = True
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inline fmeToolbar: TfmeJvProviderToolbar
    Left = 0
    Top = 0
    Width = 621
    Height = 48
    Align = alTop
    AutoScroll = False
    AutoSize = True
    TabOrder = 0
    inherited spToolbar: TSplitter
      Width = 621
    end
    inherited tbrToolbar: TToolBar
      Width = 621
    end
    inherited aiToolbar: TActionList
      inherited aiAddItem: TAction
        OnExecute = aiAddItemExecute
      end
      inherited aiDeleteItem: TAction
        OnExecute = aiDeleteItemExecute
      end
      inherited aiDeleteSubItems: TAction
        OnExecute = aiClearSubExecute
      end
      inherited aiMoveUp: TAction
        Visible = False
      end
      inherited aiMoveDown: TAction
        Visible = False
      end
    end
  end
  inline fmeTreeList: TfmeJvProviderTreeList
    Left = 0
    Top = 48
    Width = 621
    Height = 222
    Align = alClient
    AutoScroll = False
    TabOrder = 1
    inherited lvProvider: TListView
      Width = 621
      Height = 222
      PopupMenu = pmProviderEditor
    end
  end
  object pmProviderEditor: TPopupMenu
    Left = 25
    Top = 145
    object miAddItem: TMenuItem
      Caption = '&Add item'
      ImageIndex = 0
    end
    object miDivider1: TMenuItem
      Caption = '-'
    end
    object miDeleteItem: TMenuItem
      Action = fmeToolbar.aiDeleteItem
    end
    object miClearSub: TMenuItem
      Action = fmeToolbar.aiDeleteSubItems
    end
    object miDivider2: TMenuItem
      Caption = '-'
    end
    object miShowToolbar: TMenuItem
      Action = fmeToolbar.aiShowToolbar
    end
  end
  object pmAddMenu: TPopupMenu
    Left = 90
    Top = 145
  end
end
