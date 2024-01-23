object Form1: TForm1
  Left = 486
  Top = 139
  BiDiMode = bdLeftToRight
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'TJvDBTreeView component demo'
  ClientHeight = 439
  ClientWidth = 519
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  ParentBiDiMode = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DBText1: TDBText
    Left = 224
    Top = 270
    Width = 289
    Height = 19
    Color = clTeal
    DataField = 'Info'
    DataSource = DataSource1
    ParentColor = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 519
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 305
      Height = 13
      Caption = 
        'You can use Insert, Alt+Insert, Ctrl+Delete, F2 keys, Drag'#39'n'#39'Dro' +
        'p.'
    end
    object Label2: TLabel
      Left = 8
      Top = 40
      Width = 183
      Height = 13
      Caption = 'Note: MasterField must be unique field.'
      Visible = False
    end
    object Label3: TLabel
      Left = 224
      Top = 34
      Width = 257
      Height = 31
      AutoSize = False
      Caption = 
        'Delphi 4.0 note: drag'#39'n'#39'drop don'#39't work. See ralib\readme.txt fo' +
        'r suggestions for this problem.'
      WordWrap = True
    end
  end
  object RADBTreeView1: TJvDBTreeView
    Left = 8
    Top = 72
    Width = 209
    Height = 217
    DataSource = DataSource1
    MasterField = 'Uni'
    DetailField = 'Parent'
    ItemField = 'Name'
    StartMasterValue = '0'
    UseFilter = True
    PersistentNode = True
    DragMode = dmAutomatic
    HideSelection = False
    Indent = 19
    TabOrder = 1
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    Mirror = False
  end
  object Memo1: TMemo
    Left = 224
    Top = 72
    Width = 289
    Height = 193
    Lines.Strings = (
      'TJvDBTreeView key properties used by this demo:'
      '  - DataSource'
      '  - MasterField'
      '  - DetailField'
      '  - ItemField'
      '  - StartMasterValue'
      '  - DragMode'
      '  - ReadOnly'
      ''
      'Table1.OnNewRecord has following code'
      ' (it required for Insert feature):'
      '  Table1['#39'Uni'#39'] := JvBDEUtils.GetQueryResult('
      '     Table1.DatabaseName,'
      '     '#39'select max(Uni) from "Tree.dbf"'#39') + 1;'
      '                    ')
    ParentColor = True
    TabOrder = 2
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 296
    Width = 505
    Height = 137
    DataSource = DataSource1
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object Table1: TTable
    OnNewRecord = Table1NewRecord
    DatabaseName = 'dbRADBTreeViewDemo'
    TableName = 'Tree.dbf'
    Left = 168
    Top = 144
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 168
    Top = 192
  end
  object RASQLScript1: TJvBDESQLScript
    Script.Strings = (
      'create table "Tree.dbf"('
      '  Uni       integer,'
      '  Parent    integer,'
      '  Name      char(100),'
      '  Info      char(100)'
      ');'
      ''
      'insert into "Tree.dbf" values(1, 0, '#39'Russia'#39', '#39'motherland'#39');'
      'insert into "Tree.dbf" values(2, 0, '#39'Usa'#39', '#39#39');'
      'insert into "Tree.dbf" values(3, 0, '#39'UK'#39', '#39#39');'
      'insert into "Tree.dbf" values(4, 0, '#39'Ukraine'#39', '#39#39');'
      'insert into "Tree.dbf" values(5, 0, '#39'Japan'#39', '#39#39');'
      ''
      'insert into "Tree.dbf" values(6, 1, '#39'Government'#39', '#39#39');'
      'insert into "Tree.dbf" values(7, 1, '#39'President'#39', '#39#39');'
      'insert into "Tree.dbf" values(8, 1, '#39'Duma'#39', '#39#39');'
      'insert into "Tree.dbf" values(9, 1, '#39'Police'#39', '#39#39');'
      'insert into "Tree.dbf" values(10, 1, '#39'Programmers'#39', '#39#39');'
      ''
      'insert into "Tree.dbf" values(11, 10, '#39'JVCL'#39', '#39#39');'

        'insert into "Tree.dbf" values(12, 11, '#39'black'#39', '#39'blackbs@chat.ru'#39 +
        ');'
      'insert into "Tree.dbf" values(13, 11, '#39'roman'#39', '#39#39');'
      ''
      'insert into "Tree.dbf" values(14, 9, '#39'Cop #1'#39', '#39#39');'
      'insert into "Tree.dbf" values(15, 9, '#39'Cop #2'#39', '#39#39');'
      'insert into "Tree.dbf" values(16, 9, '#39'Cop #3'#39', '#39#39');'
      ''
      'insert into "Tree.dbf" values(17, 10, '#39'Inprise'#39', '#39#39');'
      'insert into "Tree.dbf" values(18, 10, '#39'New Bank Systems'#39', '#39#39');'
      'insert into "Tree.dbf" values(19, 18, '#39'black'#39', '#39#39');'
      'insert into "Tree.dbf" values(20, 18, '#39'roman'#39', '#39#39');'
      'insert into "Tree.dbf" values(21, 18, '#39'igor'#39', '#39#39');'
      'insert into "Tree.dbf" values(22, 18, '#39'era'#39', '#39#39');'
      'insert into "Tree.dbf" values(23, 18, '#39'xxx'#39', '#39#39');'
      '')
    Commit = ctAll
    Database = Database1
    Left = 456
    Top = 32
  end
  object Database1: TDatabase
    AliasName = 'DBDemos'
    DatabaseName = 'dbRADBTreeViewDemo'
    SessionName = 'Default'
    TransIsolation = tiDirtyRead
    Left = 56
    Top = 144
  end
  object RegAuto1: TJvFormStorage
    StoredValues = <>
    Left = 416
    Top = 104
  end
end
