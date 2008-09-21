object JvDBImageDemoMainFrm: TJvDBImageDemoMainFrm
  Left = 192
  Top = 107
  Caption = 'JvDBImage Demo'
  ClientHeight = 345
  ClientWidth = 698
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    698
    345)
  PixelsPerInch = 120
  TextHeight = 17
  object DBGrid1: TJvDBGrid
    Left = 10
    Top = 10
    Width = 626
    Height = 412
    Anchors = [akLeft, akTop, akBottom]
    DataSource = DataSource1
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgCancelOnExit, dgMultiSelect]
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -14
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    MultiSelect = True
    AutoSizeColumns = True
    SelectColumnsDialogStrings.Caption = 'Select columns'
    SelectColumnsDialogStrings.RealNamesOption = '[With the real field name]'
    SelectColumnsDialogStrings.OK = '&OK'
    SelectColumnsDialogStrings.NoSelectionWarning = 'At least one column must be visible!'
    EditControls = <>
    RowsHeight = 21
    TitleRowHeight = 21
    Columns = <
      item
        Expanded = False
        FieldName = 'Filename'
        Width = 502
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FileType'
        Width = 106
        Visible = True
      end>
  end
  object btnAdd: TButton
    Left = 806
    Top = 429
    Width = 98
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'Add...'
    TabOrder = 1
    OnClick = btnAddClick
  end
  object btnClear: TButton
    Left = 638
    Top = 429
    Width = 151
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'Clear DB'
    TabOrder = 2
    OnClick = btnClearClick
  end
  object chkTransparent: TCheckBox
    Left = 21
    Top = 439
    Width = 105
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = '&Transparent'
    TabOrder = 3
    OnClick = chkTransparentClick
  end
  object chkStretch: TCheckBox
    Left = 129
    Top = 439
    Width = 74
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = '&Stretch'
    TabOrder = 4
    OnClick = chkStretchClick
  end
  object chkProportional: TCheckBox
    Left = 209
    Top = 439
    Width = 105
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = '&Proportional'
    TabOrder = 5
    OnClick = chkProportionalClick
  end
  object chkAutoDisplay: TCheckBox
    Left = 323
    Top = 439
    Width = 114
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = '&AutoDisplay'
    TabOrder = 6
    OnClick = chkAutoDisplayClick
  end
  object chkAutoSize: TCheckBox
    Left = 451
    Top = 439
    Width = 114
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = 'AutoSi&ze'
    TabOrder = 7
    OnClick = chkAutoSizeClick
  end
  object ScrollBox1: TScrollBox
    Left = 649
    Top = 10
    Width = 265
    Height = 410
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    TabOrder = 8
    object JvDBImage1: TJvDBImage
      Left = 0
      Top = 0
      Width = 265
      Height = 410
      Align = alClient
      DataField = 'Image'
      DataSource = DataSource1
      TabOrder = 0
      ExplicitHeight = 409
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
