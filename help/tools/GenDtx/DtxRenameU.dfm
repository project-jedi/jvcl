object frmDtxRename: TfrmDtxRename
  Left = 254
  Top = 128
  Width = 589
  Height = 510
  Caption = 'frmDtxRename'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel3: TPanel
    Left = 0
    Top = 435
    Width = 581
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Button6: TButton
      Left = 412
      Top = 8
      Width = 75
      Height = 25
      Action = actOK
      Anchors = [akTop, akRight]
      Default = True
      TabOrder = 0
    end
    object Button7: TButton
      Left = 500
      Top = 8
      Width = 75
      Height = 25
      Action = actCancel
      Anchors = [akTop, akRight]
      Cancel = True
      TabOrder = 1
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 33
    Width = 581
    Height = 402
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 200
      Top = 0
      Height = 402
    end
    object pnlSource: TPanel
      Left = 0
      Top = 0
      Width = 200
      Height = 402
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object lsbSource: TListBox
        Left = 0
        Top = 0
        Width = 200
        Height = 308
        Style = lbVirtualOwnerDraw
        Align = alClient
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 0
        OnData = lsbSourceData
        OnDrawItem = lsbSourceDrawItem
      end
      object Panel7: TPanel
        Left = 0
        Top = 308
        Width = 200
        Height = 94
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object lblNotInPas: TLabel
          Left = 8
          Top = 8
          Width = 64
          Height = 13
          Caption = 'Not in pas file'
        end
        object lblNotInDtx: TLabel
          Left = 8
          Top = 24
          Width = 61
          Height = 13
          Caption = 'Not in dtx file'
        end
        object lblInPasInDtx: TLabel
          Left = 8
          Top = 40
          Width = 107
          Height = 13
          Caption = 'Both in pas and dtx file'
        end
        object lblOptionalInDtx: TLabel
          Left = 8
          Top = 56
          Width = 153
          Height = 13
          Caption = 'Both in pas and dtx file (optional)'
        end
        object lblOptionalNotInDtx: TLabel
          Left = 8
          Top = 72
          Width = 107
          Height = 13
          Caption = 'Not in dtx file (optional)'
        end
        object lblNotInPasCount: TLabel
          Left = 168
          Top = 8
          Width = 64
          Height = 13
          Caption = 'Not in pas file'
        end
        object lblNotInDtxCount: TLabel
          Left = 168
          Top = 24
          Width = 64
          Height = 13
          Caption = 'Not in pas file'
        end
        object lblInPasInDtxCount: TLabel
          Left = 168
          Top = 40
          Width = 64
          Height = 13
          Caption = 'Not in pas file'
        end
        object lblOptionalInDtxCount: TLabel
          Left = 168
          Top = 56
          Width = 64
          Height = 13
          Caption = 'Not in pas file'
        end
        object lblOptionalNotInDtxCount: TLabel
          Left = 168
          Top = 72
          Width = 64
          Height = 13
          Caption = 'Not in pas file'
        end
      end
    end
    object Panel8: TPanel
      Left = 203
      Top = 0
      Width = 378
      Height = 402
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object PageControl1: TPageControl
        Left = 0
        Top = 52
        Width = 378
        Height = 350
        ActivePage = tshRename
        Align = alClient
        TabOrder = 1
        object tshAdd: TTabSheet
          Caption = 'Add'
          object lsbAdd: TListBox
            Left = 33
            Top = 0
            Width = 337
            Height = 322
            Style = lbVirtualOwnerDraw
            Align = alClient
            ItemHeight = 13
            MultiSelect = True
            TabOrder = 1
            OnData = lsbAddData
            OnDrawItem = lsbAddDrawItem
          end
          object Panel1: TPanel
            Left = 0
            Top = 0
            Width = 33
            Height = 322
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 0
            object btnIncludeAll: TButton
              Left = 4
              Top = 85
              Width = 21
              Height = 28
              Action = actAddIncludeAll
              TabOrder = 0
            end
            object btnExclude: TButton
              Left = 4
              Top = 114
              Width = 21
              Height = 28
              Action = actAddInclude
              TabOrder = 1
            end
            object btnInclude: TButton
              Left = 4
              Top = 143
              Width = 21
              Height = 28
              Action = actAddExclude
              TabOrder = 2
            end
            object btnExcludeAll: TButton
              Left = 4
              Top = 172
              Width = 21
              Height = 28
              Action = actAddExcludeAll
              TabOrder = 3
            end
          end
        end
        object tshRename: TTabSheet
          Caption = 'Rename'
          ImageIndex = 1
          object lsbRename: TListBox
            Left = 0
            Top = 216
            Width = 285
            Height = 106
            Style = lbVirtual
            Anchors = [akLeft, akTop, akRight, akBottom]
            ItemHeight = 13
            MultiSelect = True
            TabOrder = 2
            OnData = lsbRenameData
          end
          object lsbSource2: TListBox
            Left = 0
            Top = 0
            Width = 370
            Height = 177
            Style = lbVirtualOwnerDraw
            Align = alTop
            ItemHeight = 13
            TabOrder = 0
            OnData = lsbSourceData
            OnDrawItem = lsbSourceDrawItem
          end
          object Button5: TButton
            Left = 120
            Top = 184
            Width = 75
            Height = 25
            Action = actRenameInclude
            TabOrder = 1
          end
          object Button8: TButton
            Left = 292
            Top = 216
            Width = 75
            Height = 25
            Action = actRenameExclude
            Anchors = [akTop, akRight]
            TabOrder = 3
          end
          object Button9: TButton
            Left = 292
            Top = 248
            Width = 75
            Height = 25
            Action = actRenameExcludeAll
            Anchors = [akTop, akRight]
            TabOrder = 4
          end
        end
        object tshSkip: TTabSheet
          Caption = 'Skip'
          ImageIndex = 2
          object lsbSkip: TListBox
            Left = 33
            Top = 0
            Width = 337
            Height = 322
            Style = lbVirtualOwnerDraw
            Align = alClient
            ItemHeight = 13
            MultiSelect = True
            TabOrder = 1
            OnData = lsbSkipData
            OnDrawItem = lsbSkipDrawItem
          end
          object Panel2: TPanel
            Left = 0
            Top = 0
            Width = 33
            Height = 322
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 0
            object Button1: TButton
              Left = 4
              Top = 85
              Width = 21
              Height = 28
              Action = actSkipIncludeAll
              TabOrder = 0
            end
            object Button2: TButton
              Left = 4
              Top = 114
              Width = 21
              Height = 28
              Action = actSkipInclude
              TabOrder = 1
            end
            object Button3: TButton
              Left = 4
              Top = 143
              Width = 21
              Height = 28
              Action = actSkipExclude
              TabOrder = 2
            end
            object Button4: TButton
              Left = 4
              Top = 172
              Width = 21
              Height = 28
              Action = actSkipExcludeAll
              TabOrder = 3
            end
          end
        end
        object tshDelete: TTabSheet
          Caption = 'Delete'
          ImageIndex = 3
          object lsbDelete: TListBox
            Left = 33
            Top = 0
            Width = 337
            Height = 322
            Style = lbVirtualOwnerDraw
            Align = alClient
            ItemHeight = 13
            MultiSelect = True
            TabOrder = 1
            OnData = lsbDeleteData
            OnDrawItem = lsbDeleteDrawItem
          end
          object Panel5: TPanel
            Left = 0
            Top = 0
            Width = 33
            Height = 322
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 0
            object Button10: TButton
              Left = 4
              Top = 85
              Width = 21
              Height = 28
              Action = actDelIncludeAll
              TabOrder = 0
            end
            object Button11: TButton
              Left = 4
              Top = 114
              Width = 21
              Height = 28
              Action = actDelInclude
              TabOrder = 1
            end
            object Button12: TButton
              Left = 4
              Top = 143
              Width = 21
              Height = 28
              Action = actDelExclude
              TabOrder = 2
            end
            object Button13: TButton
              Left = 4
              Top = 172
              Width = 21
              Height = 28
              Action = actDelExcludeAll
              TabOrder = 3
            end
          end
        end
      end
      object Panel9: TPanel
        Left = 0
        Top = 0
        Width = 378
        Height = 52
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 8
          Width = 53
          Height = 13
          Caption = 'Unit status:'
        end
        object cmbUnitStatus: TComboBox
          Left = 8
          Top = 24
          Width = 313
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          TabOrder = 0
          Text = 'cmbUnitStatus'
          Items.Strings = (
            'Completed'
            'Incomplete'
            'Locked by ...'
            'Partial documented'
            'Review')
        end
        object Button14: TButton
          Left = 328
          Top = 24
          Width = 43
          Height = 21
          Action = actResetUnitStatus
          Anchors = [akTop, akRight]
          TabOrder = 1
        end
      end
    end
  end
  object Panel6: TPanel
    Left = 0
    Top = 0
    Width = 581
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object ToolBar2: TToolBar
      Left = 0
      Top = 0
      Width = 581
      Height = 29
      ButtonHeight = 21
      ButtonWidth = 88
      Caption = 'ToolBar1'
      ShowCaptions = True
      TabOrder = 0
      object ToolButton4: TToolButton
        Left = 0
        Top = 2
        Action = actShowNotInPas
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton5: TToolButton
        Left = 88
        Top = 2
        Action = actShowNotInDtx
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton9: TToolButton
        Left = 176
        Top = 2
        Action = actShowBothInPasDtx
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton6: TToolButton
        Left = 264
        Top = 2
        Action = actShowBothInPasDtxOptional
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton8: TToolButton
        Left = 352
        Top = 2
        Action = actShowNotInDtxOptional
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton2: TToolButton
        Left = 440
        Top = 2
        Width = 25
        Caption = 'ToolButton2'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object ToolButton1: TToolButton
        Left = 465
        Top = 2
        Action = actColors
      end
    end
  end
  object ActionList1: TActionList
    Left = 325
    Top = 96
    object actOK: TAction
      Caption = 'OK'
      OnExecute = actOKExecute
    end
    object actCancel: TAction
      Caption = 'Cancel'
      OnExecute = actCancelExecute
    end
    object actRenameInclude: TAction
      Category = 'Rename'
      Caption = 'Link'
      OnExecute = actRenameIncludeExecute
      OnUpdate = BothSourceHasSelItems
    end
    object actAddInclude: TAction
      Category = 'Add'
      Caption = '>'
      OnExecute = actAddIncludeExecute
      OnUpdate = SourceHasSelItems
    end
    object actAddExclude: TAction
      Category = 'Add'
      Caption = '<'
      OnExecute = actAddExcludeExecute
      OnUpdate = AddHasSelItems
    end
    object actAddIncludeAll: TAction
      Category = 'Add'
      Caption = '>>'
      OnExecute = actAddIncludeAllExecute
      OnUpdate = SourceHasItems
    end
    object actAddExcludeAll: TAction
      Category = 'Add'
      Caption = '<<'
      OnExecute = actAddExcludeAllExecute
      OnUpdate = AddHasItems
    end
    object actSkipInclude: TAction
      Category = 'Skip'
      Caption = '>'
      OnExecute = actSkipIncludeExecute
      OnUpdate = SourceHasSelItems
    end
    object actSkipIncludeAll: TAction
      Category = 'Skip'
      Caption = '>>'
      OnExecute = actSkipIncludeAllExecute
      OnUpdate = SourceHasItems
    end
    object actSkipExclude: TAction
      Category = 'Skip'
      Caption = '<'
      OnExecute = actSkipExcludeExecute
      OnUpdate = SkipHasSelItems
    end
    object actSkipExcludeAll: TAction
      Category = 'Skip'
      Caption = '<<'
      OnExecute = actSkipExcludeAllExecute
      OnUpdate = SkipHasItems
    end
    object actShowNotInPas: TAction
      Category = 'Toolbar'
      Caption = 'Not in Pas'
      Checked = True
      OnExecute = actShowNotInPasExecute
      OnUpdate = actShowNotInPasUpdate
    end
    object actShowNotInDtx: TAction
      Category = 'Toolbar'
      Caption = 'Not in Dtx'
      Checked = True
      OnExecute = actShowNotInDtxExecute
      OnUpdate = actShowNotInDtxUpdate
    end
    object actShowBothInPasDtx: TAction
      Category = 'Toolbar'
      Caption = 'Pas && Dtx'
      Checked = True
      OnExecute = actShowBothInPasDtxExecute
      OnUpdate = actShowBothInPasDtxUpdate
    end
    object actShowBothInPasDtxOptional: TAction
      Category = 'Toolbar'
      Caption = 'Pas && Dtx (Opt.)'
      Checked = True
      OnExecute = actShowBothInPasDtxOptionalExecute
      OnUpdate = actShowBothInPasDtxOptionalUpdate
    end
    object actShowNotInDtxOptional: TAction
      Category = 'Toolbar'
      Caption = 'Not in Dtx (Opt.)'
      Checked = True
      OnExecute = actShowNotInDtxOptionalExecute
      OnUpdate = actShowNotInDtxOptionalUpdate
    end
    object actColors: TAction
      Category = 'Toolbar'
      Caption = 'Colors'
      Checked = True
      OnExecute = actColorsExecute
      OnUpdate = actColorsUpdate
    end
    object actRenameExcludeAll: TAction
      Category = 'Rename'
      Caption = 'Clear'
      OnExecute = actRenameExcludeAllExecute
      OnUpdate = RenameHasItems
    end
    object actRenameExclude: TAction
      Category = 'Rename'
      Caption = 'Delete'
      OnExecute = actRenameExcludeExecute
      OnUpdate = RenameHasSelItems
    end
    object actDelInclude: TAction
      Category = 'Delete'
      Caption = '>'
      OnExecute = actDelIncludeExecute
      OnUpdate = SourceHasSelItems
    end
    object actDelExclude: TAction
      Category = 'Delete'
      Caption = '<'
      OnExecute = actDelExcludeExecute
      OnUpdate = DeleteHasSelItems
    end
    object actDelIncludeAll: TAction
      Category = 'Delete'
      Caption = '>>'
      OnExecute = actDelIncludeAllExecute
      OnUpdate = SourceHasItems
    end
    object actDelExcludeAll: TAction
      Category = 'Delete'
      Caption = '<<'
      OnExecute = actDelExcludeAllExecute
      OnUpdate = DeleteHasItems
    end
    object actResetUnitStatus: TAction
      Caption = 'Reset'
      OnExecute = actResetUnitStatusExecute
    end
  end
  object JvAppRegistryStore1: TJvAppRegistryStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    Root = 'Software\JVCL\GenDtx'
    SubStorages = <>
    Left = 296
    Top = 144
  end
  object JvFormStorage1: TJvFormStorage
    AppStorage = JvAppRegistryStore1
    AppStoragePath = 'DtxRename\Placement\'
    StoredValues = <>
    Left = 304
    Top = 240
  end
end
