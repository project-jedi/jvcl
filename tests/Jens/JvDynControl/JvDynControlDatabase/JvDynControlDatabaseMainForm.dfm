object Form1: TForm1
  Left = 419
  Top = 153
  Width = 870
  Height = 640
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    862
    613)
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 3
    Top = 30
    Width = 785
    Height = 223
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'NAME'
        Width = 100
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ADDRESS'
        Width = 151
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ADDRESS2'
        Width = 140
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TELEPHONE'
        Width = 140
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'AGE'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LASTPHONECALL'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PRIVATENUMBER'
        Visible = True
      end>
  end
  object DBNavigator1: TDBNavigator
    Left = 3
    Top = 6
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 1
  end
  object JvPanel1: TJvPanel
    Left = 5
    Top = 305
    Width = 646
    Height = 226
    MultiLine = False
    ArrangeSettings.BorderLeft = 3
    ArrangeSettings.BorderTop = 3
    ArrangeSettings.DistanceHorizontal = 3
    ArrangeSettings.AutoSize = asHeight
    ArrangeSettings.AutoArrange = True
    Anchors = [akTop, akRight]
    TabOrder = 2
  end
  object Button1: TButton
    Left = 5
    Top = 270
    Width = 161
    Height = 25
    Caption = 'Show Dialog Default Engine'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 170
    Top = 270
    Width = 161
    Height = 25
    Caption = 'Show Dialog VCL Engine'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 335
    Top = 270
    Width = 161
    Height = 25
    Caption = 'Show Dialog JVCL Engine'
    TabOrder = 5
    OnClick = Button3Click
  end
  object ScrollBox1: TScrollBox
    Left = 675
    Top = 370
    Width = 185
    Height = 41
    TabOrder = 6
  end
  object JvCsvDataSet2: TJvCsvDataSet
    FieldDefs = <
      item
        Name = 'NAME'
        DataType = ftString
        Size = 80
      end
      item
        Name = 'ADDRESS'
        DataType = ftString
        Size = 80
      end
      item
        Name = 'ADDRESS2'
        DataType = ftString
        Size = 80
      end
      item
        Name = 'TELEPHONE'
        DataType = ftString
        Size = 80
      end
      item
        Name = 'AGE'
        DataType = ftInteger
      end
      item
        Name = 'LASTPHONECALL'
        DataType = ftDateTime
      end
      item
        Name = 'PRIVATENUMBER'
        DataType = ftBoolean
      end>
    Active = True
    FileName = 'PhoneList.csv'
    Changed = False
    CsvFieldDef = 
      'NAME,ADDRESS,ADDRESS2,TELEPHONE,AGE:%,LASTPHONECALL:@,PRIVATENUM' +
      'BER:!'
    CsvUniqueKeys = False
    ExtendedHeaderInfo = False
    CaseInsensitive = False
    AutoBackupCount = 0
    StoreDefs = True
    Left = 123
    Top = 86
    object JvCsvDataSet1NAME: TStringField
      FieldName = 'NAME'
      Size = 80
    end
    object JvCsvDataSet1ADDRESS: TStringField
      FieldName = 'ADDRESS'
      Size = 80
    end
    object JvCsvDataSet1ADDRESS2: TStringField
      FieldName = 'ADDRESS2'
      Size = 80
    end
    object JvCsvDataSet1TELEPHONE: TStringField
      FieldName = 'TELEPHONE'
      Size = 40
    end
    object JvCsvDataSet1AGE: TIntegerField
      FieldName = 'AGE'
    end
    object JvCsvDataSet1LASTPHONECALL: TDateTimeField
      FieldName = 'LASTPHONECALL'
    end
    object JvCsvDataSet1PRIVATENUMBER: TBooleanField
      FieldName = 'PRIVATENUMBER'
    end
  end
  object DataSource1: TDataSource
    DataSet = JvCsvDataSet2
    Left = 51
    Top = 86
  end
end
