object Form1: TForm1
  Left = 192
  Top = 107
  Width = 716
  Height = 388
  Caption = 'JvDBImage Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TJvDBGrid
    Left = 8
    Top = 8
    Width = 478
    Height = 315
    Anchors = [akLeft, akTop, akBottom]
    DataSource = DataSource1
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgCancelOnExit, dgMultiSelect]
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    MultiSelect = True
    AutoSizeColumns = True
    SelectColumnsDialogStrings.Caption = 'Select columns'
    SelectColumnsDialogStrings.OK = '&OK'
    SelectColumnsDialogStrings.NoSelectionWarning = 'At least one column must be visible!'
    Columns = <
      item
        Expanded = False
        FieldName = 'Filename'
        Width = 381
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FileType'
        Width = 80
        Visible = True
      end>
  end
  object btnAdd: TButton
    Left = 616
    Top = 328
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Add...'
    TabOrder = 1
    OnClick = btnAddClick
  end
  object btnClear: TButton
    Left = 488
    Top = 328
    Width = 115
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Clear DB'
    TabOrder = 2
    OnClick = btnClearClick
  end
  object chkTransparent: TCheckBox
    Left = 16
    Top = 336
    Width = 80
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Transparent'
    TabOrder = 3
    OnClick = chkTransparentClick
  end
  object chkStretch: TCheckBox
    Left = 99
    Top = 336
    Width = 56
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Stretch'
    TabOrder = 4
    OnClick = chkStretchClick
  end
  object chkProportional: TCheckBox
    Left = 160
    Top = 336
    Width = 80
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Proportional'
    TabOrder = 5
    OnClick = chkProportionalClick
  end
  object chkAutoDisplay: TCheckBox
    Left = 247
    Top = 336
    Width = 87
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&AutoDisplay'
    TabOrder = 6
    OnClick = chkAutoDisplayClick
  end
  object chkAutoSize: TCheckBox
    Left = 345
    Top = 336
    Width = 87
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'AutoSi&ze'
    TabOrder = 7
    OnClick = chkAutoSizeClick
  end
  object ScrollBox1: TScrollBox
    Left = 496
    Top = 8
    Width = 203
    Height = 313
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    TabOrder = 8
    object JvDBImage1: TJvDBImage
      Left = 0
      Top = 0
      Width = 203
      Height = 313
      Align = alClient
      DataField = 'Image'
      DataSource = DataSource1
      TabOrder = 0
    end
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 112
    Top = 48
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    FileName = 'Data\images.xml'
    FieldDefs = <
      item
        Name = 'Filename'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 255
      end
      item
        Name = 'FileType'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 12
      end
      item
        Name = 'Image'
        Attributes = [faUnNamed]
        DataType = ftGraphic
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    BeforeOpen = ClientDataSet1BeforeOpen
    Left = 152
    Top = 48
    object ClientDataSet1Filename: TStringField
      FieldName = 'Filename'
      Size = 255
    end
    object ClientDataSet1FileType: TStringField
      FieldName = 'FileType'
      Size = 12
    end
    object ClientDataSet1Image: TGraphicField
      FieldName = 'Image'
      BlobType = ftGraphic
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 192
    Top = 48
  end
end
