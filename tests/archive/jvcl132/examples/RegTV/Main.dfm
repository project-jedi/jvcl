object Form1: TForm1
  Left = 400
  Top = 245
  Width = 698
  Height = 540
  Caption = 'Registry Editor Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 177
    Top = 0
    Width = 5
    Height = 475
    Cursor = crHSplit
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 475
    Width = 690
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object RegistryTreeView1: TJvRegistryTreeView
    Left = 0
    Top = 0
    Width = 177
    Height = 475
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
    OnChange = RegistryTreeView1Change
  end
  object ListView1: TListView
    Left = 182
    Top = 0
    Width = 508
    Height = 475
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
    TabOrder = 2
    ViewStyle = vsReport
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
        AutoCheck = True
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
        Caption = 'Add to favorites'
        OnClick = Addtofavorites1Click
      end
      object Deletefavorite1: TMenuItem
        Caption = 'Delete favorite'
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
      end
    end
  end
end
