object JvDBFindEditDemoFrm: TJvDBFindEditDemoFrm
  Left = 243
  Top = 164
  Width = 690
  Height = 454
  Caption = 'JvDBFindEdit'
  Color = clBtnFace
  Constraints.MinHeight = 454
  Constraints.MinWidth = 690
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DBNavigator1: TDBNavigator
    Left = 400
    Top = 128
    Width = 230
    Height = 26
    DataSource = DataSource1
    TabOrder = 0
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 160
    Width = 681
    Height = 265
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 385
    Height = 145
    Caption = 'Filter'
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 32
      Width = 55
      Height = 13
      Caption = 'Text to filter'
    end
    object Label2: TLabel
      Left = 8
      Top = 64
      Width = 36
      Height = 13
      Caption = 'Filter by'
    end
    object ComboBox1: TComboBox
      Left = 72
      Top = 64
      Width = 217
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = ComboBox1Change
    end
    object CheckBox1: TCheckBox
      Left = 72
      Top = 96
      Width = 177
      Height = 17
      Caption = 'Case sensitivity'
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 72
      Top = 120
      Width = 201
      Height = 17
      Caption = 'Any position'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox2Click
    end
    object Reset: TButton
      Left = 296
      Top = 27
      Width = 73
      Height = 25
      Caption = 'Reset'
      TabOrder = 3
      OnClick = ResetClick
    end
    object yulFindEdit1: TJvDBFindEdit
      Left = 72
      Top = 28
      Width = 217
      Height = 21
      TabOrder = 4
      DataField = 'Event_Date'
      DataSource = DataSource1
      FindStyle = fsFilter
    end
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 176
    Top = 288
  end
  object Table1: TTable
    DatabaseName = 'DBDEMOS'
    TableName = 'events.db'
    Left = 144
    Top = 288
  end
end
