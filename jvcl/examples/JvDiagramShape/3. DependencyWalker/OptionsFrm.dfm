object frmOptions: TfrmOptions
  Left = 531
  Top = 176
  ActiveControl = edShapeWidth
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 367
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 153
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 233
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object pcOptions: TPageControl
    Left = 2
    Top = 5
    Width = 318
    Height = 315
    ActivePage = tabGeneral
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabIndex = 0
    TabOrder = 0
    object tabGeneral: TTabSheet
      Caption = 'General'
      object gbShapes: TGroupBox
        Left = 8
        Top = 0
        Width = 297
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        Caption = ' Shapes '
        TabOrder = 0
        object Label1: TLabel
          Left = 16
          Top = 24
          Width = 32
          Height = 13
          Caption = '&Width:'
          FocusControl = edShapeWidth
        end
        object Label2: TLabel
          Left = 152
          Top = 24
          Width = 35
          Height = 13
          Caption = '&Height:'
          FocusControl = edShapeHeight
        end
        object edShapeWidth: TEdit
          Left = 16
          Top = 40
          Width = 130
          Height = 21
          TabOrder = 0
          Text = '50'
        end
        object edShapeHeight: TEdit
          Left = 152
          Top = 40
          Width = 130
          Height = 21
          TabOrder = 1
          Text = '50'
        end
      end
      object gbConnectors: TGroupBox
        Left = 7
        Top = 86
        Width = 297
        Height = 191
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = ' Connectors '
        TabOrder = 1
        object Label3: TLabel
          Left = 16
          Top = 24
          Width = 56
          Height = 13
          Caption = 'Interface:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Shell Dlg 2'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label4: TLabel
          Left = 16
          Top = 112
          Width = 95
          Height = 13
          Caption = 'Implementation:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Shell Dlg 2'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label5: TLabel
          Left = 16
          Top = 48
          Width = 37
          Height = 13
          Caption = '&Normal:'
          FocusControl = cbIntfColor
        end
        object Label6: TLabel
          Left = 153
          Top = 48
          Width = 45
          Height = 13
          Caption = 'H&ighlight:'
          FocusControl = cbIntfSelColor
        end
        object Label7: TLabel
          Left = 16
          Top = 136
          Width = 37
          Height = 13
          Caption = 'Nor&mal:'
          FocusControl = cbImplColor
        end
        object Label8: TLabel
          Left = 153
          Top = 136
          Width = 45
          Height = 13
          Caption = 'Hi&ghlight:'
          FocusControl = cbImplSelColor
        end
        object cbIntfColor: TJvColorComboBox
          Left = 16
          Top = 64
          Width = 130
          Height = 20
          ColorNameMap.Strings = (
            'clBlack=Black'
            'clMaroon=Maroon'
            'clGreen=Green'
            'clOlive=Olive'
            'clNavy=Navy'
            'clPurple=Purple'
            'clTeal=Teal'
            'clGray=Gray'
            'clSilver=Silver'
            'clRed=Red'
            'clLime=Lime'
            'clYellow=Yellow'
            'clBlue=Blue'
            'clFuchsia=Fuchsia'
            'clAqua=Aqua'
            'clLtGray=Light Gray'
            'clDkGray=Dark Gray'
            'clWhite=White'
            'clMoneyGreen=Money Green'
            'clSkyBlue=Sky Blue'
            'clCream=Cream'
            'clMedGray=Medium Gray'
            'clScrollBar=ScrollBar'
            'clBackground=Background'
            'clActiveCaption=Active Caption'
            'clInactiveCaption=Inactive Caption'
            'clMenu=Menu'
            'clWindow=Window'
            'clWindowFrame=Window Frame'
            'clMenuText=Menu Text'
            'clWindowText=Window Text'
            'clCaptionText=Caption Text'
            'clActiveBorder=Active Border'
            'clInactiveBorder=Inactive Border'
            'clAppWorkSpace=Application Workspace'
            'clHighlight=Highlight'
            'clHighlightText=Highlight Text'
            'clBtnFace=Button Face'
            'clBtnShadow=Button Shadow'
            'clGrayText=Gray Text'
            'clBtnText=Button Text'
            'clInactiveCaptionText=Inactive Caption Text'
            'clBtnHighlight=Button Highlight'
            'cl3DDkShadow=3D Dark Shadow'
            'cl3DLight=3D Light'
            'clInfoText=Info Text'
            'clInfoBk=Info Background'
            'clHotLight=Hot Light'
            'clGradientActiveCaption=Gradient Active Caption'
            'clGradientInactiveCaption=Gradient Inactive Caption'
            'clMenuHighlight=Menu Highlight'
            'clMenuBar=MenuBar'
            'clNone=None'
            'clDefault=Default')
          ColorDialogText = 'Custom...'
          DroppedDownWidth = 130
          NewColorText = 'Custom'
          Options = [coText, coSysColors]
          TabOrder = 0
        end
        object cbIntfSelColor: TJvColorComboBox
          Left = 153
          Top = 64
          Width = 130
          Height = 20
          ColorNameMap.Strings = (
            'clBlack=Black'
            'clMaroon=Maroon'
            'clGreen=Green'
            'clOlive=Olive'
            'clNavy=Navy'
            'clPurple=Purple'
            'clTeal=Teal'
            'clGray=Gray'
            'clSilver=Silver'
            'clRed=Red'
            'clLime=Lime'
            'clYellow=Yellow'
            'clBlue=Blue'
            'clFuchsia=Fuchsia'
            'clAqua=Aqua'
            'clLtGray=Light Gray'
            'clDkGray=Dark Gray'
            'clWhite=White'
            'clMoneyGreen=Money Green'
            'clSkyBlue=Sky Blue'
            'clCream=Cream'
            'clMedGray=Medium Gray'
            'clScrollBar=ScrollBar'
            'clBackground=Background'
            'clActiveCaption=Active Caption'
            'clInactiveCaption=Inactive Caption'
            'clMenu=Menu'
            'clWindow=Window'
            'clWindowFrame=Window Frame'
            'clMenuText=Menu Text'
            'clWindowText=Window Text'
            'clCaptionText=Caption Text'
            'clActiveBorder=Active Border'
            'clInactiveBorder=Inactive Border'
            'clAppWorkSpace=Application Workspace'
            'clHighlight=Highlight'
            'clHighlightText=Highlight Text'
            'clBtnFace=Button Face'
            'clBtnShadow=Button Shadow'
            'clGrayText=Gray Text'
            'clBtnText=Button Text'
            'clInactiveCaptionText=Inactive Caption Text'
            'clBtnHighlight=Button Highlight'
            'cl3DDkShadow=3D Dark Shadow'
            'cl3DLight=3D Light'
            'clInfoText=Info Text'
            'clInfoBk=Info Background'
            'clHotLight=Hot Light'
            'clGradientActiveCaption=Gradient Active Caption'
            'clGradientInactiveCaption=Gradient Inactive Caption'
            'clMenuHighlight=Menu Highlight'
            'clMenuBar=MenuBar'
            'clNone=None'
            'clDefault=Default')
          ColorValue = clRed
          ColorDialogText = 'Custom...'
          DroppedDownWidth = 130
          NewColorText = 'Custom'
          Options = [coText, coSysColors]
          TabOrder = 1
        end
        object cbImplColor: TJvColorComboBox
          Left = 16
          Top = 152
          Width = 130
          Height = 20
          ColorNameMap.Strings = (
            'clBlack=Black'
            'clMaroon=Maroon'
            'clGreen=Green'
            'clOlive=Olive'
            'clNavy=Navy'
            'clPurple=Purple'
            'clTeal=Teal'
            'clGray=Gray'
            'clSilver=Silver'
            'clRed=Red'
            'clLime=Lime'
            'clYellow=Yellow'
            'clBlue=Blue'
            'clFuchsia=Fuchsia'
            'clAqua=Aqua'
            'clLtGray=Light Gray'
            'clDkGray=Dark Gray'
            'clWhite=White'
            'clMoneyGreen=Money Green'
            'clSkyBlue=Sky Blue'
            'clCream=Cream'
            'clMedGray=Medium Gray'
            'clScrollBar=ScrollBar'
            'clBackground=Background'
            'clActiveCaption=Active Caption'
            'clInactiveCaption=Inactive Caption'
            'clMenu=Menu'
            'clWindow=Window'
            'clWindowFrame=Window Frame'
            'clMenuText=Menu Text'
            'clWindowText=Window Text'
            'clCaptionText=Caption Text'
            'clActiveBorder=Active Border'
            'clInactiveBorder=Inactive Border'
            'clAppWorkSpace=Application Workspace'
            'clHighlight=Highlight'
            'clHighlightText=Highlight Text'
            'clBtnFace=Button Face'
            'clBtnShadow=Button Shadow'
            'clGrayText=Gray Text'
            'clBtnText=Button Text'
            'clInactiveCaptionText=Inactive Caption Text'
            'clBtnHighlight=Button Highlight'
            'cl3DDkShadow=3D Dark Shadow'
            'cl3DLight=3D Light'
            'clInfoText=Info Text'
            'clInfoBk=Info Background'
            'clHotLight=Hot Light'
            'clGradientActiveCaption=Gradient Active Caption'
            'clGradientInactiveCaption=Gradient Inactive Caption'
            'clMenuHighlight=Menu Highlight'
            'clMenuBar=MenuBar'
            'clNone=None'
            'clDefault=Default')
          ColorValue = clBtnShadow
          ColorDialogText = 'Custom...'
          DroppedDownWidth = 130
          NewColorText = 'Custom'
          Options = [coText, coSysColors]
          TabOrder = 2
        end
        object cbImplSelColor: TJvColorComboBox
          Left = 153
          Top = 152
          Width = 130
          Height = 20
          ColorNameMap.Strings = (
            'clBlack=Black'
            'clMaroon=Maroon'
            'clGreen=Green'
            'clOlive=Olive'
            'clNavy=Navy'
            'clPurple=Purple'
            'clTeal=Teal'
            'clGray=Gray'
            'clSilver=Silver'
            'clRed=Red'
            'clLime=Lime'
            'clYellow=Yellow'
            'clBlue=Blue'
            'clFuchsia=Fuchsia'
            'clAqua=Aqua'
            'clLtGray=Light Gray'
            'clDkGray=Dark Gray'
            'clWhite=White'
            'clMoneyGreen=Money Green'
            'clSkyBlue=Sky Blue'
            'clCream=Cream'
            'clMedGray=Medium Gray'
            'clScrollBar=ScrollBar'
            'clBackground=Background'
            'clActiveCaption=Active Caption'
            'clInactiveCaption=Inactive Caption'
            'clMenu=Menu'
            'clWindow=Window'
            'clWindowFrame=Window Frame'
            'clMenuText=Menu Text'
            'clWindowText=Window Text'
            'clCaptionText=Caption Text'
            'clActiveBorder=Active Border'
            'clInactiveBorder=Inactive Border'
            'clAppWorkSpace=Application Workspace'
            'clHighlight=Highlight'
            'clHighlightText=Highlight Text'
            'clBtnFace=Button Face'
            'clBtnShadow=Button Shadow'
            'clGrayText=Gray Text'
            'clBtnText=Button Text'
            'clInactiveCaptionText=Inactive Caption Text'
            'clBtnHighlight=Button Highlight'
            'cl3DDkShadow=3D Dark Shadow'
            'cl3DLight=3D Light'
            'clInfoText=Info Text'
            'clInfoBk=Info Background'
            'clHotLight=Hot Light'
            'clGradientActiveCaption=Gradient Active Caption'
            'clGradientInactiveCaption=Gradient Inactive Caption'
            'clMenuHighlight=Menu Highlight'
            'clMenuBar=MenuBar'
            'clNone=None'
            'clDefault=Default')
          ColorValue = clBlue
          ColorDialogText = 'Custom...'
          DroppedDownWidth = 130
          NewColorText = 'Custom'
          Options = [coText, coSysColors]
          TabOrder = 3
        end
      end
    end
    object tabPaths: TTabSheet
      Caption = 'Paths'
      ImageIndex = 1
      OnShow = tabPathsShow
      object Label9: TLabel
        Left = 8
        Top = 16
        Width = 96
        Height = 13
        Caption = '&List of library paths:'
        FocusControl = lvPaths
      end
      object lvPaths: TListView
        Left = 8
        Top = 31
        Width = 296
        Height = 178
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Width = -2
            WidthType = (
              -2)
          end>
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        PopupMenu = popPaths
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
        OnEnter = lvPathsEnter
        OnSelectItem = lvPathsSelectItem
      end
      object edLibPath: TEdit
        Left = 8
        Top = 218
        Width = 267
        Height = 21
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 1
      end
      object btnPathBrowse: TButton
        Left = 280
        Top = 218
        Width = 21
        Height = 21
        Action = acBrowse
        Anchors = [akRight, akBottom]
        TabOrder = 2
      end
      object btnReplace: TButton
        Left = 16
        Top = 250
        Width = 75
        Height = 25
        Action = acReplace
        Anchors = [akLeft, akBottom]
        TabOrder = 3
      end
      object btnAdd: TButton
        Left = 104
        Top = 250
        Width = 75
        Height = 25
        Action = acAdd
        Anchors = [akLeft, akBottom]
        TabOrder = 4
      end
      object btnDelete: TButton
        Left = 192
        Top = 250
        Width = 75
        Height = 25
        Action = acDelete
        Anchors = [akLeft, akBottom]
        TabOrder = 5
      end
    end
  end
  object alOptions: TActionList
    OnUpdate = alOptionsUpdate
    Left = 100
    Top = 112
    object acReplace: TAction
      Category = 'LibPaths'
      Caption = '&Replace'
      ShortCut = 16466
      OnExecute = acReplaceExecute
    end
    object acAdd: TAction
      Category = 'LibPaths'
      Caption = '&Add'
      ShortCut = 16429
      OnExecute = acAddExecute
    end
    object acDelete: TAction
      Category = 'LibPaths'
      Caption = '&Delete'
      ShortCut = 16430
      OnExecute = acDeleteExecute
    end
    object acBrowse: TAction
      Category = 'LibPaths'
      Caption = '...'
      ShortCut = 16397
      OnExecute = acBrowseExecute
    end
    object acDelInvalidPaths: TAction
      Category = 'LibPaths'
      Caption = 'Delete Invalid Paths'
      ShortCut = 24622
      OnExecute = acDelInvalidPathsExecute
    end
    object acGetD5Path: TAction
      Category = 'LibPaths'
      Caption = 'Delphi 5'
      OnExecute = acGetD5PathExecute
    end
    object acGetD6Path: TAction
      Category = 'LibPaths'
      Caption = 'Delphi 6'
      OnExecute = acGetD6PathExecute
    end
    object acGetD7Path: TAction
      Category = 'LibPaths'
      Caption = 'Delphi 7'
      OnExecute = acGetD7PathExecute
    end
    object acGetBCB5Path: TAction
      Category = 'LibPaths'
      Caption = 'C++ Builder 5'
      OnExecute = acGetBCB5PathExecute
    end
    object acGetBCB6Path: TAction
      Category = 'LibPaths'
      Caption = 'C++ Builder 6'
      OnExecute = acGetBCB6PathExecute
    end
    object acSystemPath: TAction
      Category = 'LibPaths'
      Caption = 'System Path'
      OnExecute = acSystemPathExecute
    end
    object acSelectAll: TAction
      Category = 'LibPaths'
      Caption = 'Select All'
      ShortCut = 16449
      OnExecute = acSelectAllExecute
    end
    object acInvertSelect: TAction
      Category = 'LibPaths'
      Caption = 'Invert Selection'
      ShortCut = 24649
      OnExecute = acInvertSelectExecute
    end
    object acUnselectAll: TAction
      Category = 'LibPaths'
      Caption = 'Unselect All'
      ShortCut = 24641
      OnExecute = acUnselectAllExecute
    end
  end
  object JvBrowseFolder1: TJvBrowseForFolderDialog
    Options = [odFileSystemDirectoryOnly, odStatusAvailable, odNewDialogStyle]
    Position = fpFormCenter
    RootDirectory = fdRootFolder
    Left = 206
    Top = 109
  end
  object popPaths: TPopupMenu
    Left = 150
    Top = 109
    object Add1: TMenuItem
      Action = acAdd
    end
    object Replace1: TMenuItem
      Action = acReplace
    end
    object Delete1: TMenuItem
      Action = acDelete
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object InsertLibraryPath1: TMenuItem
      Caption = 'Insert Path'
      object Delphi51: TMenuItem
        Action = acGetD5Path
      end
      object Delphi61: TMenuItem
        Action = acGetD6Path
      end
      object Delphi71: TMenuItem
        Action = acGetD7Path
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object CBuilder51: TMenuItem
        Action = acGetBCB5Path
      end
      object CBuilder61: TMenuItem
        Action = acGetBCB6Path
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object SystemPath1: TMenuItem
        Action = acSystemPath
      end
    end
    object Select1: TMenuItem
      Caption = 'Select'
      object SelectAll1: TMenuItem
        Action = acSelectAll
      end
      object UnselectAll1: TMenuItem
        Action = acUnselectAll
      end
      object InvertSelection1: TMenuItem
        Action = acInvertSelect
      end
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object DeleteInvalidPaths1: TMenuItem
      Action = acDelInvalidPaths
    end
  end
end
