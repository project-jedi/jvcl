object JvDBSearchDemoMainFrm: TJvDBSearchDemoMainFrm
  Left = 295
  Top = 224
  ActiveControl = btnConnect
  AutoScroll = False
  Caption = 'JvDBSearchDemo'
  ClientHeight = 332
  ClientWidth = 608
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvDBGrid1: TJvDBGrid
    Left = 0
    Top = 73
    Width = 608
    Height = 259
    Align = alClient
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Shell Dlg 2'
    TitleFont.Style = []
    TitleArrow = True
    SelectColumnsDialogStrings.Caption = 'S'#233'lection des colonnes'
    SelectColumnsDialogStrings.RealNamesOption = '[With the real field name]'
    SelectColumnsDialogStrings.OK = '&OK'
    SelectColumnsDialogStrings.NoSelectionWarning = 'Au moins une colonne doit '#234'tre visible !'
    EditControls = <>
    RowsHeight = 17
    TitleRowHeight = 17
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 608
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
      Width = 150
      Height = 21
      DataField = 'Common_Name'
      DataSource = DataSource1
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object JvDBSearchEdit1: TJvDBSearchEdit
      Left = 348
      Top = 37
      Width = 150
      Height = 21
      DataSource = DataSource1
      DataField = 'Common_Name'
      TabOrder = 1
    end
    object btnConnect: TButton
      Left = 12
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Connect'
      TabOrder = 2
      OnClick = btnConnectClick
    end
    object chkClearOnEnter: TCheckBox
      Left = 512
      Top = 40
      Width = 90
      Height = 17
      Caption = 'Clear on Enter'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = chkClearOnEnterClick
    end
  end
  object Table1: TTable
    DatabaseName = 'DBDEMOS'
    TableName = 'biolife.db'
    Left = 4
    Top = 40
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 40
    Top = 40
  end
end
