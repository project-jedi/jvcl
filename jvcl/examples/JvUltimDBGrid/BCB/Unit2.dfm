object Form2: TForm2
  Left = 230
  Top = 140
  Width = 790
  Height = 480
  Caption = 'Test BDE'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MyUltimGrid: TJvDBUltimGrid
    Left = 0
    Top = 0
    Width = 782
    Height = 408
    Align = alClient
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    TitleButtons = True
    AlternateRowColor = clInfoBk
    SelectColumn = scGrid
    TitleArrow = True
    SelectColumnsDialogStrings.Caption = 'Sélection des colonnes visibles'
    SelectColumnsDialogStrings.RealNamesOption = '[Avec le vrai nom du champ]'
    SelectColumnsDialogStrings.OK = '&OK'
    SelectColumnsDialogStrings.NoSelectionWarning = 'Il doit y avoir au moins une colonne visible !'
    EditControls = <>
    AutoSizeRows = False
    RowResize = True
    RowsHeight = 42
    TitleRowHeight = 17
    WordWrap = True
    OnIndexNotFound = MyUltimGridIndexNotFound
    Columns = <
      item
        Expanded = False
        FieldName = 'Species No'
        Width = 60
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Category'
        Width = 72
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Common_Name'
        Title.Caption = 'Common Name'
        Width = 104
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Species Name'
        Width = 146
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Length (cm)'
        Width = 62
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Length_In'
        Title.Caption = 'Length (in)'
        Width = 102
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Notes'
        Width = 154
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Graphic'
        Width = 42
        Visible = True
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 408
    Width = 782
    Height = 41
    Align = alBottom
    TabOrder = 1
    object Label1: TLabel
      Left = 12
      Top = 12
      Width = 77
      Height = 13
      Caption = 'Value to search:'
    end
    object ValueToSearch: TEdit
      Left = 92
      Top = 9
      Width = 185
      Height = 21
      TabOrder = 0
    end
    object B_Search: TBitBtn
      Left = 284
      Top = 8
      Width = 69
      Height = 25
      Caption = 'Search'
      TabOrder = 1
      OnClick = B_SearchClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333FF33333333333330003FF3FFFFF3333777003000003333
        300077F777773F333777E00BFBFB033333337773333F7F33333FE0BFBF000333
        330077F3337773F33377E0FBFBFBF033330077F3333FF7FFF377E0BFBF000000
        333377F3337777773F3FE0FBFBFBFBFB039977F33FFFFFFF7377E0BF00000000
        339977FF777777773377000BFB03333333337773FF733333333F333000333333
        3300333777333333337733333333333333003333333333333377333333333333
        333333333333333333FF33333333333330003333333333333777333333333333
        3000333333333333377733333333333333333333333333333333}
      NumGlyphs = 2
    end
    object B_SearchNext: TBitBtn
      Left = 360
      Top = 8
      Width = 69
      Height = 25
      Caption = 'Next'
      TabOrder = 2
      OnClick = B_SearchNextClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333FF33333333333330003FF3FFFFF3333777003000003333
        300077F777773F333777E00BFBFB033333337773333F7F33333FE0BFBF000333
        330077F3337773F33377E0FBFBFBF033330077F3333FF7FFF377E0BFBF000000
        333377F3337777773F3FE0FBFBFBFBFB039977F33FFFFFFF7377E0BF00000000
        339977FF777777773377000BFB03333333337773FF733333333F333000333333
        3300333777333333337733333333333333003333333333333377333333333333
        333333333333333333FF33333333333330003333333333333777333333333333
        3000333333333333377733333333333333333333333333333333}
      NumGlyphs = 2
      Spacing = 10
    end
    object DBNavigator1: TDBNavigator
      Left = 472
      Top = 12
      Width = 255
      Height = 21
      DataSource = DataSource1
      VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbRefresh]
      TabOrder = 3
    end
  end
  object Table1: TTable
    DatabaseName = 'BCDEMOS'
    Exclusive = True
    ReadOnly = True
    TableName = 'biolife.db'
    Left = 744
    Top = 368
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 712
    Top = 368
  end
end
