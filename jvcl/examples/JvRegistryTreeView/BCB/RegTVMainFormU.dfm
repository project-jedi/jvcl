object RegTVMainForm: TRegTVMainForm
  Left = 283
  Top = 95
  Width = 637
  Height = 399
  Caption = 'Registry Editor Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 209
    Top = 0
    Width = 5
    Height = 334
    Cursor = crHSplit
  end
  object ListView1: TListView
    Left = 214
    Top = 0
    Width = 415
    Height = 334
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 200
      end
      item
        AutoSize = True
        Caption = 'Value'
      end
      item
        AutoSize = True
        Caption = 'Type'
      end>
    ColumnClick = False
    FullDrag = True
    SortType = stText
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = ListView1Change
  end
  object RegistryTreeView1: TJvRegistryTreeView
    Left = 0
    Top = 0
    Width = 209
    Height = 334
    Align = alLeft
    ShowHint = False
    ReadOnly = False
    Indent = 19
    RegistryKeys = [hkClassesRoot, hkCurrentUser, hkLocalMachine, hkUsers, hkCurrentConfig]
    ListView = ListView1
    RootCaption = 'My Computer'
    DefaultCaption = '(Default)'
    DefaultNoValueCaption = '(value not set)'
    OnExpanding = RegistryTreeView1Expanding
    OnExpanded = RegistryTreeView1Expanded
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 334
    Width = 629
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object mmMain: TMainMenu
    Left = 104
    Top = 88
    object Registry1: TMenuItem
      Caption = 'Registry'
      object Importregistryfile1: TMenuItem
        Caption = 'Import registryfile...'
        OnClick = Importregistryfile1Click
      end
      object Exportregistryfile1: TMenuItem
        Caption = 'Export registryfile...'
        OnClick = Exportregistryfile1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Connectnetworkdrive1: TMenuItem
        Caption = 'Connect network drive...'
        OnClick = Connectnetworkdrive1Click
      end
      object Disconnectnetworkdrive1: TMenuItem
        Caption = 'Disconnect network drive...'
        Enabled = False
        OnClick = Disconnectnetworkdrive1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Caption = 'Print...'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object New1: TMenuItem
        Caption = 'New'
        object Key1: TMenuItem
          Caption = 'Key'
          OnClick = Key1Click
        end
        object N4: TMenuItem
          Caption = '-'
        end
        object Stringvalue1: TMenuItem
          Caption = 'String value'
        end
        object Binaryvalue1: TMenuItem
          Caption = 'Binary value'
        end
        object DWORDvalue1: TMenuItem
          Caption = 'DWORD value'
        end
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Delete1: TMenuItem
        Caption = 'Delete'
        OnClick = Delete1Click
      end
      object Rename1: TMenuItem
        Caption = 'Rename'
        OnClick = Rename1Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Copykeyname1: TMenuItem
        Caption = 'Copy key name'
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Find1: TMenuItem
        Caption = 'Find...'
      end
      object FindNext1: TMenuItem
        Caption = 'Find Next'
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object Statusbar2: TMenuItem
        Caption = 'Statusbar'
        Checked = True
        OnClick = Statusbar2Click
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Refresh1: TMenuItem
        Caption = 'Refresh'
        ShortCut = 116
        OnClick = Refresh1Click
      end
    end
    object Favorites1: TMenuItem
      Caption = 'Favorites'
      object Addtofavorites1: TMenuItem
        Caption = 'Add to Favorites'
        OnClick = Addtofavorites1Click
      end
      object Deletefavorite1: TMenuItem
        Caption = 'Delete Favorite'
      end
      object N10: TMenuItem
        Caption = '-'
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object HelpIndex1: TMenuItem
        Caption = 'Help Index'
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object AboutRegistryeditordemo1: TMenuItem
        Caption = 'About Registry Editor Demo...'
        OnClick = AboutRegistryeditordemo1Click
      end
    end
  end
  object alMain: TActionList
    Left = 80
    Top = 144
    object acImport: TAction
      Category = 'Registry'
      Caption = 'Import registryfile...'
    end
    object acExport: TAction
      Category = 'Registry'
      Caption = 'Export registryfile...'
    end
    object acConnectNetwork: TAction
      Category = 'Registry'
      Caption = 'Connect network drive...'
    end
    object acDisconnectNetwork: TAction
      Category = 'Registry'
      Caption = 'Disconnect network drive...'
    end
    object acPrint: TAction
      Category = 'Registry'
      Caption = 'Print...'
    end
    object acExit: TAction
      Category = 'Registry'
      Caption = 'Exit'
    end
    object acNewKey: TAction
      Category = 'Edit'
      Caption = 'Key'
    end
    object acNewString: TAction
      Category = 'Edit'
      Caption = 'String Value'
    end
    object acNewBinary: TAction
      Category = 'Edit'
      Caption = 'Binary Value'
    end
    object acNewDWORD: TAction
      Category = 'Edit'
      Caption = 'DWORD Value'
    end
    object acDelete: TAction
      Category = 'Edit'
      Caption = 'Delete'
    end
    object acRename: TAction
      Category = 'Edit'
      Caption = 'Rename'
    end
    object acCopyName: TAction
      Category = 'Edit'
      Caption = 'Copy Key Name'
    end
    object acFind: TAction
      Category = 'Edit'
      Caption = 'Find...'
    end
    object acFindNext: TAction
      Category = 'Edit'
      Caption = 'Find Next'
    end
    object acStatusBar: TAction
      Category = 'View'
      Caption = 'StatusBar'
    end
    object acRefresh: TAction
      Category = 'View'
      Caption = 'Refresh'
    end
    object acAddFav: TAction
      Category = 'Favorites'
      Caption = 'Add to Favorites'
    end
    object acDelFav: TAction
      Category = 'Favorites'
      Caption = 'Delete Favorite'
    end
    object acHelp: TAction
      Category = 'Help'
      Caption = 'Help Index'
    end
    object acAbout: TAction
      Category = 'Help'
      Caption = 'About Registry Editor Demo...'
    end
  end
end
