object Form1: TForm1
  Left = 329
  Top = 168
  Width = 275
  Height = 142
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  Caption = 'CaptionButton Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClick = FormClick
  PixelsPerInch = 96
  TextHeight = 13
  object CaptionButton1: TJvCaptionButton
    Caption = '?'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = [fsBold]
    ButtonLeft = 76
    ButtonTop = 6
    ButtonWidth = 18
    ButtonHeight = 14
    Standard = tsbHelp
    OnClick = CaptionButton1Click
    OnMouseUp = CaptionButton1MouseUp
    OnMouseDown = CaptionButton1MouseDown
    Left = 72
    Top = 24
  end
end
