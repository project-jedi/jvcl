object Form1: TForm1
  Left = 399
  Top = 392
  Width = 687
  Height = 398
  Caption = 'JvDBSearchDemo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvDBGrid1: TJvDBGrid
    Left = 0
    Top = 73
    Width = 679
    Height = 298
    Align = alClient
    DataSource = dts1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Shell Dlg 2'
    TitleFont.Style = []
    TitleButtons = True
    TitleArrow = True
    SelectColumnsDialogStrings.Caption = 'S'#233'lection des colonnes'
    SelectColumnsDialogStrings.OK = '&OK'
    SelectColumnsDialogStrings.NoSelectionWarning = 'Au moins une colonne doit '#234'tre visible !'
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 679
    Height = 73
    Align = alTop
    TabOrder = 1
    object lbl1: TLabel
      Left = 112
      Top = 8
      Width = 220
      Height = 13
      Caption = 'Search '#39'Common_name'#39' field with ComboBox :'
    end
    object lbl2: TLabel
      Left = 112
      Top = 40
      Width = 187
      Height = 13
      Caption = 'Search '#39'Common_name'#39' field with Edit :'
    end
    object JvDBSearchComboBox1: TJvDBSearchComboBox
      Left = 348
      Top = 5
      Width = 145
      Height = 21
      DataField = 'Common_Name'
      DataSource = dts1
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object JvDBSearchEdit1: TJvDBSearchEdit
      Left = 348
      Top = 37
      Width = 145
      Height = 21
      DataSource = dts1
      DataField = 'Common_Name'
      TabOrder = 1
    end
    object btn1: TButton
      Left = 12
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Connect'
      TabOrder = 2
      OnClick = btn1Click
    end
    object CheckBox1: TCheckBox
      Left = 512
      Top = 40
      Width = 97
      Height = 17
      Caption = 'Clear on Enter'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CheckBox1Click
    end
  end
  object tbl1: TTable
    DatabaseName = 'DBDEMOS'
    TableName = 'biolife.db'
    Left = 4
    Top = 40
  end
  object dts1: TDataSource
    DataSet = tbl1
    Left = 40
    Top = 40
  end
end
