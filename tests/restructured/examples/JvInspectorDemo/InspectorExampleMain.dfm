object frmInspector: TfrmInspector
  Left = 197
  Top = 113
  Width = 254
  Height = 708
  ActiveControl = JvInspector1
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'JvInspector'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object JvInspector1: TJvInspector
    Left = 0
    Top = 0
    Width = 246
    Height = 681
    Align = alClient
    BandWidth = 150
    BevelInner = bvNone
    BevelKind = bkTile
    Divider = 75
    ItemHeight = 16
    Painter = JvInspectorBorlandPainter1
    Readonly = False
    UseBands = False
    WantTabs = False
    AfterItemCreate = JvInspector1AfterItemCreate
  end
  object JvInspectorBorlandPainter1: TJvInspectorBorlandPainter
    Left = 135
    Top = 50
  end
  object JvInspectorDotNETPainter1: TJvInspectorDotNETPainter
    SelectedColor = clNavy
    SelectedTextColor = clWhite
    Left = 130
    Top = 135
  end
end
