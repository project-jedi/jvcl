object Form1: TForm1
  Left = 407
  Top = 116
  Width = 350
  Height = 422
  Caption = 'HID Collections'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object TreeView1: TTreeView
    Left = 0
    Top = 0
    Width = 342
    Height = 392
    Align = alClient
    Anchors = []
    Indent = 19
    TabOrder = 0
  end
  object HidCtl: TJvHidDeviceController
    OnEnumerate = HidCtlEnumerate
    OnDeviceChange = HidCtlDeviceChange
    Left = 248
    Top = 8
  end
end
