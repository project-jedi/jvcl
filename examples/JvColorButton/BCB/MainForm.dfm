object frmMain: TfrmMain
  Left = 192
  Top = 107
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Color Button demo'
  ClientHeight = 119
  ClientWidth = 198
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ColorButton1: TJvColorButton
    Left = 48
    Top = 40
    Width = 97
    OtherCaption = '&Other...'
    Options = [cdAnyColor]
    Color = clNavy
    OnChange = ColorButton1Change
  end
end
