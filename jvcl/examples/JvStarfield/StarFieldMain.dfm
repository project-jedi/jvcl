object StarfieldMainForm: TStarfieldMainForm
  Left = 192
  Top = 119
  AutoScroll = False
  Caption = 'JvStarfield example'
  ClientHeight = 610
  ClientWidth = 862
  Color = clBtnFace
  Constraints.MinHeight = 50
  Constraints.MinWidth = 100
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvStarfield1: TJvStarfield
    Left = 0
    Top = 0
    Width = 862
    Height = 610
    Align = alClient
    Active = True
    Stars = 1000
    MaxSpeed = 100
  end
end
