object fglXMLSerializerDemo: TfglXMLSerializerDemo
  Left = 347
  Top = 219
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JvgXMLSerializer demo'
  ClientHeight = 159
  ClientWidth = 407
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 132
    Width = 43
    Height = 13
    Caption = 'Test File:'
  end
  object bLoadXML: TButton
    Left = 312
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Load XML'
    TabOrder = 0
    OnClick = bLoadXMLClick
  end
  object bSaveXML: TButton
    Left = 312
    Top = 16
    Width = 75
    Height = 25
    Caption = 'SaveXML'
    TabOrder = 1
    OnClick = bSaveXMLClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 209
    Height = 89
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'this example demonstrates saving'
      'a class in XML and load of data from '
      'XML.'
      'Before saving the class will be '
      'initialized with test data ')
    ReadOnly = True
    TabOrder = 2
  end
  object eTestFileName: TEdit
    Left = 120
    Top = 128
    Width = 265
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 3
    Text = 'eTestFileName'
  end
  object bViewXML: TButton
    Left = 312
    Top = 80
    Width = 75
    Height = 25
    Caption = 'View XML'
    TabOrder = 4
    OnClick = bViewXMLClick
  end
  object JvgXMLSerializer: TJvgXMLSerializer
    ExcludeEmptyValues = True
    ExcludeDefaultValues = True
    ReplaceReservedSymbols = True
    IgnoreUnknownTags = False
    OnGetXMLHeader = glXMLSerializerGetXMLHeader
    Left = 240
    Top = 8
  end
end
