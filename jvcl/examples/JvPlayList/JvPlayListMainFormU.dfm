object JvPlayListMainForm: TJvPlayListMainForm
  Left = 433
  Top = 276
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PlayList Demo'
  ClientHeight = 256
  ClientWidth = 421
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 38
    Top = 232
    Width = 377
    Height = 17
    AutoSize = False
  end
  object Label2: TLabel
    Left = 4
    Top = 232
    Width = 26
    Height = 13
    Caption = 'Item :'
  end
  object JvPlaylist1: TJvPlaylist
    Left = 4
    Top = 4
    Width = 411
    Height = 221
    ItemHeight = 13
    Background.FillMode = bfmTile
    Background.Visible = False
    MultiSelect = True
    ScrollBars = ssNone
    TabOrder = 0
    OnClick = JvPlaylist1Click
  end
  object OpenDialog1: TOpenDialog
    InitialDir = '.'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 184
    Top = 46
  end
  object MainMenu1: TMainMenu
    Left = 52
    Top = 50
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Action = Open
      end
      object Delete1: TMenuItem
        Action = Delete
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = Exit
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      OnClick = Options1Click
      object ShowNumbers1: TMenuItem
        Caption = 'Show Numbers'
        OnClick = ShowNumbers1Click
      end
      object ShowExtensions1: TMenuItem
        Caption = 'Show Extensions'
        OnClick = ShowExtensions1Click
      end
      object ShowDrives1: TMenuItem
        Caption = 'Show Drive Letters'
        Checked = True
        OnClick = ShowDrives1Click
      end
    end
    object Operations1: TMenuItem
      Caption = 'Fun&ctions'
      object DeleteDeadFiles1: TMenuItem
        Action = DeleteDead
      end
      object Delete2: TMenuItem
        Caption = 'Sort'
        object SortByPath1: TMenuItem
          Action = SortPah
        end
        object SortByPathInverted1: TMenuItem
          Action = SortPathI
        end
        object SortBySongName1: TMenuItem
          Action = SortSong
        end
        object SortBySongNameInverted1: TMenuItem
          Action = SortSongNameInverted
        end
        object N2: TMenuItem
          Caption = '-'
        end
        object RandomOrder1: TMenuItem
          Action = RandomOrder
        end
        object ReverseOrder1: TMenuItem
          Action = Reverse
        end
      end
    end
    object Selection1: TMenuItem
      Caption = '&Selection'
      object SelectAll1: TMenuItem
        Action = SelectAll
      end
      object UnselectAll1: TMenuItem
        Action = UnselectAll
      end
      object InverseSelection1: TMenuItem
        Action = InvSelect
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MoveSelectedUp1: TMenuItem
        Action = MoveUp
      end
      object MoveSelectedDown1: TMenuItem
        Action = MoveDown
      end
    end
  end
  object ActionList1: TActionList
    Left = 126
    Top = 44
    object Open: TAction
      Caption = '&Open...'
      Hint = 'Open'
      ShortCut = 16463
      OnExecute = OpenExecute
    end
    object Delete: TAction
      Caption = '&Delete'
      ShortCut = 46
      OnExecute = DeleteExecute
    end
    object Exit: TAction
      Caption = '&Exit'
      ShortCut = 32883
      OnExecute = ExitExecute
    end
    object DeleteDead: TAction
      Caption = 'Delete &Dead Files'
      ShortCut = 16430
      OnExecute = DeleteDeadExecute
    end
    object SortSong: TAction
      Caption = 'Sort By &SongName'
      OnExecute = SortSongExecute
    end
    object SortPah: TAction
      Caption = 'Sort By Path'
      OnExecute = SortPahExecute
    end
    object SortPathI: TAction
      Caption = 'Sort By Path Inverted'
      OnExecute = SortPathIExecute
    end
    object SortSongNameInverted: TAction
      Caption = 'Sort By SongName Inverted'
      OnExecute = SortSongNameInvertedExecute
    end
    object RandomOrder: TAction
      Caption = 'Random Order'
      OnExecute = RandomOrderExecute
    end
    object Reverse: TAction
      Caption = 'Reverse Order'
      OnExecute = ReverseExecute
    end
    object SelectAll: TAction
      Caption = 'Select All'
      ShortCut = 16449
      OnExecute = SelectAllExecute
    end
    object UnselectAll: TAction
      Caption = 'Unselect All'
      ShortCut = 16469
      OnExecute = UnselectAllExecute
    end
    object InvSelect: TAction
      Caption = 'Inverse Selection'
      ShortCut = 16457
      OnExecute = InvSelectExecute
    end
    object MoveUp: TAction
      Caption = 'Move Selected Up'
      ShortCut = 16422
      OnExecute = MoveUpExecute
    end
    object MoveDown: TAction
      Caption = 'Move Selected Down'
      ShortCut = 16424
      OnExecute = MoveDownExecute
    end
  end
end
