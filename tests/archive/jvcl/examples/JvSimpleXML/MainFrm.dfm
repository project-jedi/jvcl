object Form1: TForm1
  Left = 213
  Top = 107
  Width = 783
  Height = 540
  Caption = 'JVSimpleXML Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    775
    513)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 18
    Width = 40
    Height = 13
    Caption = '&XML file:'
  end
  object edXMLFile: TEdit
    Left = 12
    Top = 36
    Width = 727
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object Button1: TButton
    Left = 744
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
    Width = 751
    Height = 433
    Anchors = [akLeft, akTop, akRight, akBottom]
    Indent = 19
    TabOrder = 2
  end
  object JvSimpleXml1: TJvSimpleXml
    Left = 102
    Top = 24
  end
  object JvOpenDialog1: TJvOpenDialog
    Filter = 'XML files|*.xml;*.xsl|All files|*.*'
    Height = 347
    Width = 563
    Left = 306
    Top = 144
  end
end
