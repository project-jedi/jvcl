object MainForm: TMainForm
  Left = 202
  Top = 127
  Width = 697
  Height = 252
  HorzScrollBar.Range = 54
  VertScrollBar.Range = 157
  ActiveControl = Button1
  AutoScroll = False
  Caption = 'Save Query Result to Stream'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 0
    Top = 0
    Width = 65
    Height = 25
    Caption = 'Go'
    TabOrder = 0
    OnClick = Button1Click
  end
  object StringGrid: TStringGrid
    Left = 0
    Top = 29
    Width = 689
    Height = 193
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 6
    TabOrder = 1
    ColWidths = (
      64
      64
      64
      387
      48
      41)
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object DataBase: TJvUIBDataBase
    Params.Strings = (
      'sql_dialect=3'
      'password=masterkey'
      'user_name=SYSDBA'
      'lc_ctype=NONE')
    DatabaseName = 'd:\employee.db'
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    Left = 88
  end
  object Transaction: TJvUIBTransaction
    DataBase = DataBase
    Left = 120
  end
  object Query: TJvUIBQuery
    SQL.Strings = (
      'select * from employee')
    Transaction = Transaction
    FetchBlobs = True
    Left = 152
  end
end
