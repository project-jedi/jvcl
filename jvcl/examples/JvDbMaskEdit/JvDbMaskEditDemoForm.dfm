object JvDbMaskEditDemoFrm: TJvDbMaskEditDemoFrm
  Left = 185
  Top = 156
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JvDBMaskEdit Demo'
  ClientHeight = 426
  ClientWidth = 383
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 326
    Width = 305
    Height = 78
    Anchors = [akLeft, akBottom]
    Caption = 
      'Validation/Accepting new values: Try typing in Bob as the name o' +
      'f a person, then hit enter or tab, or click on a different contr' +
      'ol (to Exit the control, thus updating the data aware object). S' +
      'ee OnAcceptNewValue handler for validation sample. This is a lot' +
      ' more flexible than OnExit event where you would have to effecti' +
      'vely UNDO a change that has already been done.'
    WordWrap = True
  end
  object DBCtrlGrid1: TDBCtrlGrid
    Left = 24
    Top = 64
    Width = 337
    Height = 255
    Anchors = [akLeft, akTop, akBottom]
    DataSource = DataSource1
    PanelHeight = 85
    PanelWidth = 321
    TabOrder = 0
    object Label3: TLabel
      Left = 16
      Top = 19
      Width = 28
      Height = 13
      Caption = 'Name'
    end
    object Label4: TLabel
      Left = 16
      Top = 47
      Width = 31
      Height = 13
      Caption = 'Phone'
    end
    object EditNAME: TJvDBMaskEdit
      Left = 63
      Top = 16
      Width = 223
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DataField = 'NAME'
      DataSource = DataSource1
      TabOrder = 0
      OnExit = EditNAMEExit
      OnKeyDown = EditNAMEKeyDown
      OnAcceptNewValue = EditNAMEAcceptNewValue
    end
    object EditPHONE: TJvDBMaskEdit
      Left = 63
      Top = 48
      Width = 221
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DataField = 'PHONE'
      DataSource = DataSource1
      TabOrder = 1
    end
  end
  object DBNavigator1: TDBNavigator
    Left = 72
    Top = 24
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 1
  end
  object JvCsvDataSet1: TJvCsvDataSet
    FieldDefs = <
      item
        Name = 'NAME'
        DataType = ftString
        Size = 80
      end
      item
        Name = 'PHONE'
        DataType = ftString
        Size = 80
      end>
    Active = True
    FileName = 'Phones.csv'
    Changed = False
    CsvFieldDef = 'NAME,PHONE'
    CsvUniqueKeys = False
    ExtendedHeaderInfo = False
    CaseInsensitive = False
    AutoBackupCount = 0
    StoreDefs = True
    Left = 144
    Top = 176
    object JvCsvDataSet1NAME: TStringField
      FieldName = 'NAME'
      Size = 80
    end
    object JvCsvDataSet1PHONE: TStringField
      FieldName = 'PHONE'
      Size = 80
    end
  end
  object DataSource1: TDataSource
    DataSet = JvCsvDataSet1
    Left = 224
    Top = 176
  end
end
