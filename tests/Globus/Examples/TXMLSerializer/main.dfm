object fglXMLSerializerDemo: TfglXMLSerializerDemo
  Left = 347
  Top = 219
  BorderStyle = bsDialog
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
    Width = 82
    Height = 13
    Caption = 'Тестовый файл:'
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
    Width = 185
    Height = 89
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'Пример демонстрирует сохранение '
      'класса в XML и загрузку данных из '
      'XML.'
      ''
      'Перед сохранением класс '
      'инициализируется тестовыми '
      'данными.')
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
    OnGetXMLHeader = glXMLSerializerGetXMLHeader
    Left = 216
    Top = 8
  end
end
