object JvgMultipleResourceEdit: TJvgMultipleResourceEdit
  Left = 198
  Top = 109
  Width = 565
  Height = 450
  Caption = 'glMresEdit'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object sg: TStringGrid
    Left = 0
    Top = 0
    Width = 557
    Height = 389
    Align = alTop
    DefaultColWidth = 100
    DefaultRowHeight = 16
    FixedCols = 3
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goAlwaysShowEditor]
    TabOrder = 0
    OnSetEditText = sgSetEditText
  end
end
