object JvgPrintSetup: TJvgPrintSetup
  Left = 520
  Top = 121
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Print Setup'
  ClientHeight = 59
  ClientWidth = 424
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object rgOrientation: TRadioGroup
    Left = 8
    Top = 0
    Width = 200
    Height = 56
    Caption = 'Orientation'
    ItemIndex = 0
    Items.Strings = (
      'Portrait'
      'Landscape')
    TabOrder = 0
  end
  object rgRadioGroup2: TRadioGroup
    Left = 216
    Top = 0
    Width = 200
    Height = 56
    Caption = #207#229#247#224#242#252
    ItemIndex = 0
    Items.Strings = (
      #206#228#237#238#241#242#238#240#238#237#237#255#255
      #196#226#243#241#242#238#240#238#237#237#255#255)
    TabOrder = 1
  end
end
