object JvInspectorDBDemoMainForm: TJvInspectorDBDemoMainForm
  Left = 224
  Top = 196
  Width = 604
  Height = 481
  Caption = 'JvInspectorDBDemoMainForm'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 450
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  DesignSize = (
    596
    447)
  PixelsPerInch = 96
  TextHeight = 13
  object JvInspector1: TJvInspector
    Left = 0
    Top = 0
    Width = 233
    Height = 447
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 16
    Painter = JvInspectorBorlandPainter1
  end
  object DBGrid1: TDBGrid
    Left = 236
    Top = 0
    Width = 357
    Height = 409
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
        Width = 70
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FirstName'
        Width = 103
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LastName'
        Width = 98
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Salary'
        Visible = True
      end>
  end
  object btnOpenCloseDS: TButton
    Left = 240
    Top = 416
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Open table'
    TabOrder = 2
    OnClick = btnOpenCloseDSClick
  end
  object btnClose: TButton
    Left = 517
    Top = 416
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Exit'
    TabOrder = 3
    OnClick = btnCloseClick
  end
  object JvInspectorBorlandPainter1: TJvInspectorBorlandPainter
    DrawNameEndEllipsis = False
    Left = 88
    Top = 96
  end
  object dbInspector: TDatabase
    AliasName = 'DBDEMOS'
    DatabaseName = 'InspDemo'
    LoginPrompt = False
    SessionName = 'Default'
    Left = 88
    Top = 32
  end
  object Table1: TTable
    DatabaseName = 'DBDEMOS'
    TableName = 'employee.db'
    Left = 24
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
    Left = 152
    Top = 32
  end
end
