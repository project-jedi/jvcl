object frmMain: TfrmMain
  Left = 269
  Top = 171
  Width = 376
  Height = 329
  Caption = 'JvSimpleXML Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    368
    302)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 18
    Width = 41
    Height = 13
    Caption = '&XML file:'
  end
  object edXMLFile: TEdit
    Left = 12
    Top = 36
    Width = 320
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object Button1: TButton
    Left = 337
    Top = 36
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object JvTreeView1: TJvTreeView
    Left = 12
    Top = 66
    Width = 344
    Height = 222
    Anchors = [akLeft, akTop, akRight, akBottom]
    Indent = 19
    TabOrder = 2
    LineColor = 13160660
  end
  object JvSimpleXml1: TJvSimpleXML
    IndentString = '  '
    Options = [sxoAutoIndent]
    Left = 102
    Top = 24
  end
  object JvOpenDialog1: TJvOpenDialog
    Filter = 'XML files|*.xml;*.xsl|All files|*.*'
    Height = 347
    Width = 563
    Left = 154
    Top = 24
  end
end
