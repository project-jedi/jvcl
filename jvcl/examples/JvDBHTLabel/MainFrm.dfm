object Form1: TForm1
  Left = 396
  Top = 112
  AutoScroll = False
  Caption = 'JvDBHTLabel Demo'
  ClientHeight = 330
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object JvDBHTLabel1: TJvDBHTLabel
    Left = 8
    Top = 6
    Width = 115
    Height = 27
    DataSource = DataSource1
    Mask = 
      '<b><font color="$0000FF">Filename:   </font></b><u><FIELD="Filen' +
      'ame"></u><br><b><font color="$0000FF">Type:   </font></b><i><FIE' +
      'LD="Type"></i>'
  end
  object DBNavigator1: TDBNavigator
    Left = 8
    Top = 292
    Width = 80
    Height = 19
    DataSource = DataSource1
    VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast]
    Anchors = [akLeft, akBottom]
    TabOrder = 0
  end
  object memFormat: TMemo
    Left = 8
    Top = 64
    Width = 442
    Height = 217
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Button1: TButton
    Left = 361
    Top = 289
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Update'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
  object Edit1: TJvEdit
    Left = 8
    Top = 40
    Width = 441
    Height = 21
    Hint = 'Standard hint'
    PasswordChar = #0
    GroupIndex = -1
    MaxPixel.Length = 200
    MaxPixel.UseControlFont = False
    MaxPixel.Font.Charset = DEFAULT_CHARSET
    MaxPixel.Font.Color = clWindowText
    MaxPixel.Font.Height = -9
    MaxPixel.Font.Name = 'MS Shell Dlg 2'
    MaxPixel.Font.Style = []
    Modified = False
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object DataSource1: TDataSource
    DataSet = JvCsvDataSet1
    OnDataChange = DataSource1DataChange
    Left = 400
    Top = 16
  end
  object JvCsvDataSet1: TJvCsvDataSet
    FileName = 'TestData.csv'
    ReadOnly = True
    Changed = False
    CsvFieldDef = 'Filename:$255,Size:%,Attributes:$64,Type:$255'
    CsvUniqueKeys = False
    CaseInsensitive = True
    SavesChanges = False
    AutoBackupCount = 0
    StoreDefs = True
    Left = 368
    Top = 16
  end
end
