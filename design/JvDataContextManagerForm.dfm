inherited frmJvDataContextManager: TfrmJvDataContextManager
  Caption = 'frmJvDataContextManager'
  PixelsPerInch = 96
  TextHeight = 13
  inherited fmeTreeList: TfmeJvProviderTreeList
    inherited lvProvider: TListView
      PopupMenu = pmProviderEditor
    end
  end
  inherited pmProviderEditor: TPopupMenu
    inherited miDeleteItem: TMenuItem
      Action = fmeToolbar.aiDeleteItem
    end
    inherited miClearSub: TMenuItem
      Action = fmeToolbar.aiDeleteSubItems
    end
    inherited miShowToolbar: TMenuItem
      Action = fmeToolbar.aiShowToolbar
    end
  end
end
