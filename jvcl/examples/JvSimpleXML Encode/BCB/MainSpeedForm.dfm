object frmSpeedTest: TfrmSpeedTest
  Left = 192
  Top = 107
  Width = 680
  Height = 388
  Caption = 'SimpleXMLEncode and  SimpleXMLDecode Speed Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    672
    361)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 8
    Width = 230
    Height = 13
    Caption = 'Type in filenames or drag and drop from Explorer.'
  end
  object Label2: TLabel
    Left = 384
    Top = 8
    Width = 179
    Height = 13
    Caption = 'Results (click column headers to sort):'
  end
  object memFiles: TMemo
    Left = 12
    Top = 24
    Width = 265
    Height = 318
    Anchors = [akLeft, akTop, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object btnEncode: TButton
    Left = 294
    Top = 30
    Width = 75
    Height = 25
    Caption = '&Encode'
    TabOrder = 1
    OnClick = btnEncodeClick
  end
  object btnDecode: TButton
    Left = 294
    Top = 60
    Width = 75
    Height = 25
    Caption = '&Decode'
    TabOrder = 2
    OnClick = btnDecodeClick
  end
  object sgResults: TJvStringGrid
    Left = 384
    Top = 24
    Width = 281
    Height = 318
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 6
    DefaultRowHeight = 19
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSizing, goColSizing, goRowSelect, goThumbTracking]
    TabOrder = 3
    Alignment = taLeftJustify
    FixedFont.Charset = DEFAULT_CHARSET
    FixedFont.Color = clWindowText
    FixedFont.Height = -11
    FixedFont.Name = 'MS Sans Serif'
    FixedFont.Style = []
    OnCaptionClick = sgResultsCaptionClick
  end
  object btnClear: TButton
    Left = 294
    Top = 90
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 4
    OnClick = btnClearClick
  end
end
