object Form1: TForm1
  Left = 221
  Top = 105
  Width = 805
  Height = 629
  Caption = 'Test ADO'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object JvDBGrid1: TJvDBUltimGrid
    Left = 0
    Top = 0
    Width = 797
    Height = 561
    Align = alClient
    DataSource = DataSource1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clGreen
    TitleFont.Height = -16
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    TitleButtons = True
    AlternateRowColor = clInactiveCaptionText
    TitleArrow = True
    ShowCellHint = True
    SelectColumnsDialogStrings.Caption = 'Select columns'
    SelectColumnsDialogStrings.RealNamesOption = '[With the real field name]'
    SelectColumnsDialogStrings.OK = '&OK'
    SelectColumnsDialogStrings.NoSelectionWarning = 'At least one column must be visible!'
    EditControls = <
      item
        ControlName = 'DBMemo1'
        FieldName = 'Comment'
      end
      item
        ControlName = 'DBLookupComboBox1'
        FieldName = 'Category'
      end
      item
        ControlName = 'JvDBComboBox1'
        FieldName = 'Licenses'
      end>
    AutoSizeRows = False
    RowResize = True
    RowsHeight = 34
    TitleRowHeight = 24
    WordWrap = True
    SortWith = swFields
    OnRestoreGridPosition = JvDBGrid1RestoreGridPosition
    Columns = <
      item
        Expanded = False
        FieldName = 'RefLogiciel'
        Width = 43
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Software'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Georgia'
        Font.Style = []
        Width = 93
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Category'
        PickList.Strings = (
          'BUREAU_PAO'
          'DIVERS'
          'GRAPHISME'
          'JEUX'
          'NET_WEB'
          'OS'
          'PROGRAM'
          'UTILITAIRE')
        Title.Caption = 'Category (lookup)'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FirstBool'
        PickList.Strings = (
          'Vrai'
          'Faux')
        Width = 50
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'SecondBool'
        Width = 50
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Licenses'
        PickList.Strings = (
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8'
          '9'
          '10')
        Width = 77
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Price'
        Width = 71
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Comment'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Comic Sans MS'
        Font.Style = [fsBold]
        Width = 227
        Visible = True
      end>
  end
  object DBMemo1: TDBMemo
    Left = 32
    Top = 452
    Width = 309
    Height = 97
    Color = 13434879
    DataField = 'Comment'
    DataSource = DataSource1
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Comic Sans MS'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    Visible = False
    OnEnter = DBMemo1Enter
  end
  object JvDBComboBox1: TJvDBComboBox
    Left = 364
    Top = 504
    Width = 145
    Height = 24
    Color = 13434879
    DataField = 'Licenses'
    DataSource = DataSource1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 16
    Items.Strings = (
      'Zero'
      'One'
      'Two'
      'Three'
      'Four'
      'Five'
      'Six'
      'Seven'
      'Eight'
      'Nine')
    ParentFont = False
    TabOrder = 3
    Values.Strings = (
      '0'
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9')
    Visible = False
    OnKeyPress = JvDBComboBox1KeyPress
  end
  object DBLookupComboBox1: TDBLookupComboBox
    Left = 360
    Top = 460
    Width = 281
    Height = 24
    Color = 13434879
    DataField = 'Category'
    DataSource = DataSource1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    KeyField = 'CodeLogiciel'
    ListField = 'LibelleLog'
    ListSource = DataSource2
    ParentFont = False
    TabOrder = 2
    Visible = False
    OnKeyPress = DBLookupComboBox1KeyPress
  end
  object Panel1: TPanel
    Left = 0
    Top = 561
    Width = 797
    Height = 41
    Align = alBottom
    TabOrder = 4
    object B_Connect: TButton
      Left = 8
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Connect/Disconnect'
      TabOrder = 0
      OnClick = B_ConnectClick
    end
    object B_TitleIndic: TButton
      Left = 272
      Top = 8
      Width = 129
      Height = 25
      Caption = 'Add/remove Titles/Indic'
      TabOrder = 2
      OnClick = B_TitleIndicClick
    end
    object B_WordWrap: TButton
      Left = 408
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Change Word Wrap'
      TabOrder = 3
      OnClick = B_WordWrapClick
    end
    object B_RowHeight: TButton
      Left = 528
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Change RowHeight'
      TabOrder = 4
      OnClick = B_RowHeightClick
    end
    object B_ShowEdit: TButton
      Left = 128
      Top = 8
      Width = 137
      Height = 25
      Caption = 'AlwaysShowEditor On/Off'
      TabOrder = 1
      OnClick = B_ShowEditClick
    end
    object B_Search: TButton
      Left = 648
      Top = 8
      Width = 141
      Height = 25
      Caption = 'Search W and License = 1'
      TabOrder = 5
      OnClick = B_SearchClick
    end
  end
  object ADOConnection1: TADOConnection
    Connected = True
    ConnectionString = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=BaseTest.mdb;'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 532
    Top = 368
  end
  object MainTable: TADOTable
    Active = True
    Connection = ADOConnection1
    CursorType = ctStatic
    TableName = 'Logiciels'
    Left = 500
    Top = 368
    object MainTableRefLogiciel: TAutoIncField
      FieldName = 'RefLogiciel'
    end
    object MainTableSoftware: TWideStringField
      FieldName = 'Software'
      Size = 50
    end
    object MainTableCategory: TWideStringField
      FieldName = 'Category'
      OnGetText = MainTableCategoryGetText
      Size = 10
    end
    object MainTableFirstBool: TBooleanField
      FieldName = 'FirstBool'
    end
    object MainTableSecondBool: TBooleanField
      FieldName = 'SecondBool'
    end
    object MainTableLicenses: TSmallintField
      FieldName = 'Licenses'
    end
    object MainTablePrice: TBCDField
      FieldName = 'Price'
      Precision = 19
    end
    object MainTableComment: TMemoField
      FieldName = 'Comment'
      BlobType = ftMemo
    end
  end
  object DataSource1: TDataSource
    DataSet = MainTable
    Left = 464
    Top = 368
  end
  object DataSource2: TDataSource
    DataSet = LookupTable
    Left = 464
    Top = 404
  end
  object LookupTable: TADOTable
    Active = True
    Connection = ADOConnection1
    CursorType = ctStatic
    TableName = 'CategLogiciels'
    Left = 500
    Top = 404
  end
end
