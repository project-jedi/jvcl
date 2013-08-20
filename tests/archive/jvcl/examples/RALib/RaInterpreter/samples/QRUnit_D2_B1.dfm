object QRListForm: TQRListForm
  Left = 198
  Top = 136
  Width = 578
  Height = 212
  HorzScrollBar.Range = 1200
  VertScrollBar.Range = 2000
  AutoScroll = False
  Caption = 'QuickReport List'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object Title: TQRBand
    Left = 0
    Top = 0
    Width = 15
    Height = 37
    Frame.Color = clBlack
    Frame.DrawTop = False
    Frame.DrawBottom = False
    Frame.DrawLeft = False
    Frame.DrawRight = False
    Frame.Width = 0
    AlignToBottom = False
    Color = clWhite
    ForceNewColumn = False
    ForceNewPage = False
    Size.Values = (
      97.8958333333333
      40)
    BandType = rbTitle
    object QRSysData1: TQRSysData
      Left = 2
      Top = 0
      Width = 10
      Height = 10
      Frame.Color = clBlack
      Frame.DrawTop = False
      Frame.DrawBottom = False
      Frame.DrawLeft = False
      Frame.DrawRight = False
      Size.Values = (
        26.4583333333333
        5.29166666666667
        0
        26.4583333333333)
      Alignment = taCenter
      AlignToBand = True
      AutoSize = False
      Color = clWhite
      Data = qrsReportTitle
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -27
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
      FontSize = 20
    end
  end
  object PageHeader: TQRBand
    Left = 0
    Top = 0
    Width = 15
    Height = 24
    Frame.Color = clBlack
    Frame.DrawTop = False
    Frame.DrawBottom = False
    Frame.DrawLeft = False
    Frame.DrawRight = False
    Frame.Width = 0
    AlignToBottom = False
    Color = clWhite
    ForceNewColumn = False
    ForceNewPage = False
    Size.Values = (
      63.5
      40)
    BandType = rbPageHeader
    object PageNumber: TQRSysData
      Left = 5
      Top = 4
      Width = 10
      Height = 10
      Frame.Color = clBlack
      Frame.DrawTop = False
      Frame.DrawBottom = False
      Frame.DrawLeft = False
      Frame.DrawRight = False
      Size.Values = (
        26.4583333333333
        13.2291666666667
        10.5833333333333
        26.4583333333333)
      Alignment = taRightJustify
      AlignToBand = True
      AutoSize = False
      Color = clWhite
      Data = qrsPageNumber
      Text = 'Page number '
      Transparent = False
      FontSize = 8
    end
    object QRLabel1: TQRLabel
      Left = 12
      Top = 4
      Width = 10
      Height = 10
      Frame.Color = clBlack
      Frame.DrawTop = False
      Frame.DrawBottom = False
      Frame.DrawLeft = False
      Frame.DrawRight = False
      Size.Values = (
        26.4583333333333
        31.75
        10.5833333333333
        26.4583333333333)
      Alignment = taLeftJustify
      AlignToBand = False
      AutoSize = True
      AutoStretch = False
      Caption = 'Page header information goes here'
      Color = clWhite
      Transparent = False
      WordWrap = True
      FontSize = 8
    end
  end
  object Detail: TQRBand
    Left = 0
    Top = 0
    Width = 15
    Height = 24
    Frame.Color = clBlack
    Frame.DrawTop = False
    Frame.DrawBottom = False
    Frame.DrawLeft = False
    Frame.DrawRight = False
    Frame.Width = 0
    AlignToBottom = False
    Color = clWhite
    ForceNewColumn = False
    ForceNewPage = False
    Size.Values = (
      63.5
      40)
    BandType = rbDetail
    object QRDBText1: TQRDBText
      Left = 208
      Top = 3
      Width = 10
      Height = 10
      Frame.Color = clBlack
      Frame.DrawTop = False
      Frame.DrawBottom = False
      Frame.DrawLeft = False
      Frame.DrawRight = False
      Size.Values = (
        26.4583333333333
        550.333333333333
        7.9375
        26.4583333333333)
      Alignment = taLeftJustify
      AlignToBand = False
      AutoSize = True
      AutoStretch = False
      Color = clWhite
      DataField = 'LastName'
      Transparent = False
      WordWrap = True
      FontSize = 8
    end
    object QRDBText2: TQRDBText
      Left = 8
      Top = 3
      Width = 10
      Height = 10
      Frame.Color = clBlack
      Frame.DrawTop = False
      Frame.DrawBottom = False
      Frame.DrawLeft = False
      Frame.DrawRight = False
      Size.Values = (
        26.4583333333333
        21.1666666666667
        7.9375
        26.4583333333333)
      Alignment = taLeftJustify
      AlignToBand = False
      AutoSize = True
      AutoStretch = False
      Color = clWhite
      DataField = 'FirstName'
      Transparent = False
      WordWrap = True
      FontSize = 8
    end
  end
  object PageFooter: TQRBand
    Left = 0
    Top = 0
    Width = 15
    Height = 24
    Frame.Color = clBlack
    Frame.DrawTop = False
    Frame.DrawBottom = False
    Frame.DrawLeft = False
    Frame.DrawRight = False
    Frame.Width = 0
    AlignToBottom = False
    Color = clWhite
    ForceNewColumn = False
    ForceNewPage = False
    Size.Values = (
      63.5
      40)
    BandType = rbPageFooter
    object QRDateTime: TQRSysData
      Left = 5
      Top = 4
      Width = 10
      Height = 10
      Frame.Color = clBlack
      Frame.DrawTop = False
      Frame.DrawBottom = False
      Frame.DrawLeft = False
      Frame.DrawRight = False
      Size.Values = (
        26.4583333333333
        13.2291666666667
        10.5833333333333
        26.4583333333333)
      Alignment = taRightJustify
      AlignToBand = True
      AutoSize = False
      Color = clWhite
      Data = qrsDateTime
      Text = 'Printed at '
      Transparent = False
      FontSize = 8
    end
    object QRLabel3: TQRLabel
      Left = 12
      Top = 4
      Width = 10
      Height = 10
      Frame.Color = clBlack
      Frame.DrawTop = False
      Frame.DrawBottom = False
      Frame.DrawLeft = False
      Frame.DrawRight = False
      Size.Values = (
        26.4583333333333
        31.75
        10.5833333333333
        26.4583333333333)
      Alignment = taLeftJustify
      AlignToBand = False
      AutoSize = True
      AutoStretch = False
      Caption = 'Page footer information goes here'
      Color = clWhite
      Transparent = False
      WordWrap = True
      FontSize = 8
    end
  end
  object Table1: TTable
    Active = True
    DatabaseName = 'DBDemos'
    TableName = 'EMPLOYEE.DB'
    Left = 56
    Top = 128
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 96
    Top = 128
  end
end
