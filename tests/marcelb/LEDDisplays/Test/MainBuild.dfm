object Form4: TForm4
  Left = 152
  Top = 142
  BorderStyle = bsDialog
  Caption = 'Form4'
  ClientHeight = 331
  ClientWidth = 642
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Timer1: TTimer
    Interval = 400
    OnTimer = Timer1Timer
    Left = 45
    Top = 25
  end
end
