object frmMain: TfrmMain
  Left = 192
  Top = 107
  Width = 464
  Height = 357
  Caption = 'JvSimpleXML validator'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object reXML: TRichEdit
    Left = 8
    Top = 80
    Width = 439
    Height = 177
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    PlainText = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    OnChange = reXMLChange
  end
  object btnLoad: TButton
    Left = 16
    Top = 270
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Load...'
    TabOrder = 1
    OnClick = btnLoadClick
  end
  object btnValidate: TButton
    Left = 358
    Top = 270
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Validate'
    Default = True
    Enabled = False
    TabOrder = 2
    OnClick = btnValidateClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 456
    Height = 65
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Color = clWindow
    TabOrder = 3
    object Label1: TLabel
      Left = 18
      Top = 22
      Width = 420
      Height = 36
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 
        'Type in an XML string in the memo or click "Load" to load XML da' +
        'ta from a file. Click "Validate" to check for correct syntax.'
      WordWrap = True
    end
    object Label2: TLabel
      Left = 14
      Top = 7
      Width = 77
      Height = 13
      Caption = 'XML Validator'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object sbResults: TStatusBar
    Left = 0
    Top = 311
    Width = 456
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object JvSimpleXml1: TJvSimpleXML
    IndentString = '  '
    Options = [sxoAutoIndent]
    Left = 92
    Top = 74
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'XML files|*.xml|All files|*.*'
    InitialDir = '.'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 160
    Top = 80
  end
end
