object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 485
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvDBGrid1: TJvDBGrid
    Left = 0
    Top = 0
    Width = 673
    Height = 485
    Align = alClient
    DataSource = dsItems
    Options = [dgEditing, dgAlwaysShowEditor, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    SelectColumnsDialogStrings.Caption = 'Select columns'
    SelectColumnsDialogStrings.OK = '&OK'
    SelectColumnsDialogStrings.NoSelectionWarning = 'At least one column must be visible!'
    EditControls = <>
    RowsHeight = 17
    TitleRowHeight = 17
    OnGetColumnLookupInfo = JvDBGrid1GetColumnLookupInfo
  end
  object cdsItems: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'ID'
        DataType = ftInteger
      end
      item
        Name = 'OMSCHR'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'LISTTYPE'
        DataType = ftInteger
      end
      item
        Name = 'LISTID'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = cdsItemsAfterOpen
    Left = 80
    Top = 88
    Data = {
      600000009619E0BD010000001800000004000000000003000000600002494404
      00010000000000064F4D53434852010049000000010005574944544802000200
      1400084C495354545950450400010000000000064C4953544944040001000000
      00000000}
    object cdsItemsID: TIntegerField
      FieldName = 'ID'
    end
    object cdsItemsOMSCHR: TStringField
      FieldName = 'OMSCHR'
    end
    object cdsItemsLISTTYPE: TIntegerField
      Alignment = taLeftJustify
      FieldName = 'LISTTYPE'
      LookupDataSet = cdsListTypes
      LookupKeyFields = 'ID'
      LookupResultField = 'OMSCHR'
      KeyFields = 'LISTTYPE'
      OnGetText = cdsItemsLISTTYPEGetText
    end
    object cdsItemsLISTID: TIntegerField
      Alignment = taLeftJustify
      FieldName = 'LISTID'
      LookupKeyFields = 'ID'
      LookupResultField = 'OMSCHR'
      KeyFields = 'LISTID'
      OnGetText = cdsItemsLISTIDGetText
    end
  end
  object dsItems: TDataSource
    DataSet = cdsItems
    OnDataChange = dsItemsDataChange
    Left = 80
    Top = 144
  end
  object cdsList1: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'ID'
        DataType = ftInteger
      end
      item
        Name = 'OMSCHR'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'EX'
        DataType = ftString
        Size = 20
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = cdsList1AfterOpen
    Left = 232
    Top = 104
    Data = {
      570000009619E0BD010000001800000003000000000003000000570002494404
      00010000000000064F4D53434852010049000000010005574944544802000200
      140002455801004900000001000557494454480200020014000000}
    object cdsList1ID: TIntegerField
      FieldName = 'ID'
    end
    object cdsList1OMSCHR: TStringField
      FieldName = 'OMSCHR'
    end
    object cdsList1EX: TStringField
      FieldName = 'EX'
    end
  end
  object cdsList2: TClientDataSet
    Active = True
    Aggregates = <>
    Params = <>
    AfterOpen = cdsList2AfterOpen
    Left = 232
    Top = 168
    Data = {
      400000009619E0BD010000001800000002000000000003000000400002494404
      00010000000000064F4D53434852010049000000010005574944544802000200
      14000000}
    object IntegerField1: TIntegerField
      FieldName = 'ID'
    end
    object StringField1: TStringField
      FieldName = 'OMSCHR'
    end
  end
  object cdsListTypes: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'ID'
        DataType = ftInteger
      end
      item
        Name = 'OMSCHR'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'EX'
        DataType = ftString
        Size = 20
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterOpen = cdsListTypesAfterOpen
    Left = 152
    Top = 88
    Data = {
      570000009619E0BD010000001800000003000000000003000000570002494404
      00010000000000064F4D53434852010049000000010005574944544802000200
      140002455801004900000001000557494454480200020014000000}
    object IntegerField2: TIntegerField
      FieldName = 'ID'
    end
    object StringField2: TStringField
      FieldName = 'OMSCHR'
    end
  end
end
