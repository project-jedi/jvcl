object ColorDemoMainForm: TColorDemoMainForm
  Left = 349
  Top = 137
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'ColorButton'
  ClientHeight = 119
  ClientWidth = 198
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
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
    TabOrder = 0
    TabStop = False
  end
end
