object Form1: TForm1
  Left = 387
  Top = 124
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'TJvDBMove Demo'
  ClientHeight = 401
  ClientWidth = 567
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 553
    Height = 353
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Source (DBDEMOS)'
      object Label1: TLabel
        Left = 8
        Top = 0
        Width = 79
        Height = 13
        Caption = 'CUSTOMER.DB'
      end
      object Label2: TLabel
        Left = 8
        Top = 148
        Width = 64
        Height = 13
        Caption = 'ORDERS.DB'
      end
      object Label3: TLabel
        Left = 8
        Top = 300
        Width = 409
        Height = 17
        AutoSize = False
        Caption = 'CUSTOMER.CustNo ->ORDERS.CustNo'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object Label4: TLabel
        Left = 424
        Top = 16
        Width = 113
        Height = 273
        AutoSize = False
        Caption = 
          'This is Source Database. It contains two tables and one referenc' +
          'e between them.'
        Color = clBtnFace
        ParentColor = False
        WordWrap = True
      end
      object DBGrid1: TDBGrid
        Left = 8
        Top = 16
        Width = 409
        Height = 121
        DataSource = dsSource1
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object DBGrid2: TDBGrid
        Left = 8
        Top = 165
        Width = 409
        Height = 120
        DataSource = dsSource2
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
    object TabSheet2: TTabSheet
      Caption = '   ->'
      object Label9: TLabel
        Left = 16
        Top = 16
        Width = 513
        Height = 33
        AutoSize = False
        Caption = 'This demo includes steps:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object BitBtn1: TBitBtn
        Left = 24
        Top = 56
        Width = 497
        Height = 25
        Caption = '1. Create destination tables'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = BitBtn1Click
        Margin = 20
      end
      object BitBtn2: TBitBtn
        Left = 24
        Top = 96
        Width = 497
        Height = 25
        Caption = '2. Batch moving'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = BitBtn2Click
        Margin = 20
      end
      object GroupBox1: TGroupBox
        Left = 16
        Top = 144
        Width = 513
        Height = 153
        Caption = ' Key properties '
        TabOrder = 2
        object Label11: TLabel
          Left = 8
          Top = 20
          Width = 98
          Height = 13
          Caption = 'RADBMove1.Tables'
        end
        object Label12: TLabel
          Left = 176
          Top = 20
          Width = 121
          Height = 13
          Caption = 'RADBMove1.References'
        end
        object Label13: TLabel
          Left = 344
          Top = 20
          Width = 112
          Height = 13
          Caption = 'RADBMove1.Mappings'
        end
        object Memo1: TMemo
          Left = 8
          Top = 40
          Width = 160
          Height = 100
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
        end
        object Memo2: TMemo
          Left = 176
          Top = 40
          Width = 160
          Height = 100
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 1
          WordWrap = False
        end
        object Memo3: TMemo
          Left = 345
          Top = 40
          Width = 160
          Height = 100
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 2
          WordWrap = False
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Destination (WINDOWS TEMP DIR)'
      object Label7: TLabel
        Left = 424
        Top = 16
        Width = 113
        Height = 273
        AutoSize = False
        Caption = 
          'This is Destination Database. You can see that all references fi' +
          'xups correctly.'#13#10'Fields CustNo in CUSTOMER.DB and OrderNo in ORD' +
          'ERS.DB are changed because they are primary keys which are gener' +
          'ated unique.'#13#10'Field CustNo (now called MyCustNo) in ORDERS.DB (n' +
          'ow called MYORDERS) also changed, so it can correctly refer to n' +
          'ew CUSTOMER.DB.'
        Color = clBtnFace
        ParentColor = False
        WordWrap = True
      end
      object Label5: TLabel
        Left = 8
        Top = 0
        Width = 79
        Height = 13
        Caption = 'CUSTOMER.DB'
      end
      object Label6: TLabel
        Left = 8
        Top = 148
        Width = 80
        Height = 13
        Caption = 'MYORDERS.DB'
      end
      object Label8: TLabel
        Left = 8
        Top = 300
        Width = 409
        Height = 17
        AutoSize = False
        Caption = 'CUSTOMER.CustNo ->MYORDERS.MyCustNo'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object DBGrid3: TDBGrid
        Left = 8
        Top = 16
        Width = 409
        Height = 121
        DataSource = dsDestination1
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object DBGrid4: TDBGrid
        Left = 8
        Top = 165
        Width = 409
        Height = 120
        DataSource = dsDestination2
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Clean up'
      object BitBtn3: TBitBtn
        Left = 24
        Top = 128
        Width = 497
        Height = 25
        Caption = '3. Remove temporary destination tables'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = BitBtn3Click
        Margin = 20
      end
      object BitBtn4: TBitBtn
        Left = 24
        Top = 168
        Width = 497
        Height = 25
        Caption = 'Exit'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = BitBtn4Click
        Margin = 20
      end
    end
  end
  object Button1: TButton
    Left = 408
    Top = 368
    Width = 75
    Height = 25
    Caption = 'Next >>'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Panel1: TPanel
    Left = 136
    Top = 80
    Width = 281
    Height = 73
    Caption = 'Panel1'
    TabOrder = 2
    Visible = False
    object Label10: TLabel
      Left = 12
      Top = 22
      Width = 244
      Height = 24
      Caption = 'Don'#39't run from Delphi IDE!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object tSource1: TTable
    DatabaseName = 'DBDemos'
    TableName = 'CUSTOMER.DB'
    Left = 72
    Top = 240
  end
  object dsSource1: TDataSource
    DataSet = tSource1
    Left = 144
    Top = 240
  end
  object tSource2: TTable
    DatabaseName = 'DBDemos'
    IndexName = 'CustNo'
    MasterFields = 'CustNo'
    MasterSource = dsSource1
    TableName = 'ORDERS.DB'
    Left = 216
    Top = 240
  end
  object dsSource2: TDataSource
    DataSet = tSource2
    Left = 288
    Top = 240
  end
  object RADBMove1: TJvDBMove
    Source = 'DBDemos'
    Destination = 'DestBase'
    Tables.Strings = (
      'CUSTOMER'
      'ORDERS')
    TempTable = '_RATMP1_.DB'
    References.Strings = (
      'ORDERS.CustNo = CUSTOMER.CustNo')
    Mappings.Strings = (
      'ORDERS=MYORDERS'
      'ORDERS.CustNo=MyCustNo')
    OnMoveRecord = RADBMove1MoveRecord
    Progress = True
    Left = 212
    Top = 192
  end
  object RASQLScript1: TJvBDESQLScript
    Script.Strings = (
      '/* Creating two destination tables */'
      ''
      'create table "CUSTOMER.DB"('
      '  CustNo     integer,       /* CustNo */'
      '  Company    char(30),      /* Company */'
      '  Addr1      char(30),      /* Addr1 */'
      '  primary key (CustNo)'
      ');'
      ''
      'create table "MYORDERS.DB"('
      '  OrderNo     integer,'
      '  MyCustNo      integer,'
      '  SaleDate    timestamp,'
      '  ShipDate    timestamp,'
      '  EmpNo       integer,'
      '  primary key (OrderNo)'
      ');'
      ''
      'create index CustNo on MYORDERS(MyCustNo);'
      ''
      '')
    Commit = ctNone
    Database = DestinationDatabase
    Left = 396
    Top = 248
  end
  object DestinationDatabase: TDatabase
    Connected = True
    DatabaseName = 'DestBase'
    DriverName = 'STANDARD'
    Params.Strings = (
      'PATH=C:\Temp')
    SessionName = 'Default'
    TransIsolation = tiDirtyRead
    Left = 308
    Top = 192
  end
  object tDestination1: TTable
    DatabaseName = 'DestBase'
    TableName = 'CUSTOMER.DB'
    Left = 72
    Top = 288
  end
  object dsDestination1: TDataSource
    DataSet = tDestination1
    Left = 144
    Top = 288
  end
  object tDestination2: TTable
    DatabaseName = 'DestBase'
    IndexName = 'CustNo'
    MasterFields = 'CustNo'
    MasterSource = dsDestination1
    TableName = 'MYORDERS.DB'
    Left = 208
    Top = 288
  end
  object dsDestination2: TDataSource
    DataSet = tDestination2
    Left = 288
    Top = 288
  end
  object RAProgressForm1: TJvProgressComponent
    Caption = 'TJvDBMove in progress'
    ProgressMax = 0
    OnShow = RAProgressForm1Show
    Left = 396
    Top = 208
  end
end
