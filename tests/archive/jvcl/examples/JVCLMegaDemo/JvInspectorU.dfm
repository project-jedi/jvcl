object JvInspectorDBForm: TJvInspectorDBForm
  Left = 101
  Top = 171
  Width = 612
  Height = 484
  Caption = 'JvInspectorDBForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    604
    459)
  PixelsPerInch = 96
  TextHeight = 13
  object JvInspector1: TJvInspector
    Left = 0
    Top = 0
    Width = 217
    Height = 450
    Anchors = [akLeft, akTop, akBottom]
    BandWidth = 150
    BevelInner = bvNone
    BevelKind = bkTile
    RelativeDivider = False
    Divider = 75
    ItemHeight = 16
    Painter = JvInspectorBorlandPainter1
    ReadOnly = False
    UseBands = False
    WantTabs = False
  end
  object DBGrid1: TDBGrid
    Left = 220
    Top = 0
    Width = 373
    Height = 412
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'EmpNo'
        Width = 49
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FirstName'
        Width = 86
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LastName'
        Width = 104
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Salary'
        Width = 90
        Visible = True
      end>
  end
  object btnOpenCloseDS: TButton
    Left = 232
    Top = 419
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Open table'
    TabOrder = 2
    OnClick = btnOpenCloseDSClick
  end
  object JvInspectorBorlandPainter1: TJvInspectorBorlandPainter
    Left = 92
    Top = 116
  end
  object dbInspector: TDatabase
    AliasName = 'DBDEMOS'
    DatabaseName = 'InspDemo'
    LoginPrompt = False
    SessionName = 'Default'
    Left = 144
    Top = 32
  end
  object Table1: TTable
    DatabaseName = 'DBDEMOS'
    TableName = 'employee.db'
    Left = 96
    Top = 32
    object Table1EmpNo: TIntegerField
      FieldName = 'EmpNo'
    end
    object Table1FirstName: TStringField
      DisplayLabel = 'First name'
      FieldName = 'FirstName'
      Size = 15
    end
    object Table1LastName: TStringField
      DisplayLabel = 'Last name'
      FieldName = 'LastName'
    end
    object Table1Salary: TFloatField
      FieldName = 'Salary'
    end
    object Table1PhoneExt: TStringField
      FieldName = 'PhoneExt'
      Size = 4
    end
    object Table1HireDate: TDateTimeField
      FieldName = 'HireDate'
    end
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 96
    Top = 168
  end
end
