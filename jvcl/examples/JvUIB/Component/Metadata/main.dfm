object MainForm: TMainForm
  Left = 321
  Top = 293
  Width = 688
  Height = 442
  Caption = 'Metadata explorer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 225
    Top = 0
    Width = 3
    Height = 393
    Cursor = crHSplit
  end
  object TreeView: TTreeView
    Left = 0
    Top = 0
    Width = 225
    Height = 393
    Align = alLeft
    Indent = 23
    ReadOnly = True
    TabOrder = 0
    OnChange = TreeViewChange
  end
  object Memo: TMemo
    Left = 228
    Top = 0
    Width = 452
    Height = 393
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object DataBase: TJvUIBDataBase
    Params.Strings = (
      'sql_dialect=1'
      'lc_ctype=NONE'
      'password=masterkey'
      'user_name=SYSDBA')
    DatabaseName = 'D:\EMPLOYEE.DB'
    SQLDialect = 1
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    Left = 16
    Top = 8
  end
  object Transaction: TJvUIBTransaction
    DataBase = DataBase
    Left = 48
    Top = 8
  end
  object MainMenu: TMainMenu
    Left = 144
    Top = 8
    object mFile: TMenuItem
      Caption = 'File'
      object Open: TMenuItem
        Caption = 'Open Database'
        OnClick = OpenClick
      end
      object SaveToFile1: TMenuItem
        Caption = 'SaveTo File'
        OnClick = SaveToFile1Click
      end
      object LoadFromFile1: TMenuItem
        Caption = 'Load From File'
        OnClick = LoadFromFile1Click
      end
    end
  end
  object OpenDialog: TOpenDialog
    Left = 80
    Top = 8
  end
  object SaveDialog: TSaveDialog
    Left = 112
    Top = 8
  end
end
