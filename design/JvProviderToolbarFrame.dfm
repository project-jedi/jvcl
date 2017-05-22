inherited fmeJvProviderToolbar: TfmeJvProviderToolbar
  inherited tbrToolbar: TToolBar
    Wrapable = False
    OnResize = tbrToolbarResize
    object tbDivider2: TToolButton
      Left = 458
      Top = 0
      Width = 8
      Caption = 'tbDivider2'
      ImageIndex = 5
      Style = tbsSeparator
    end
    object pnlContexts: TPanel
      Left = 466
      Top = 0
      Width = 185
      Height = 36
      BevelOuter = bvNone
      PopupMenu = pmToolbar
      TabOrder = 0
      OnResize = pnlContextsResize
      object cbContexts: TComboBox
        Left = 0
        Top = 7
        Width = 185
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akRight]
        ItemHeight = 13
        PopupMenu = pmToolbar
        TabOrder = 0
      end
    end
  end
end
