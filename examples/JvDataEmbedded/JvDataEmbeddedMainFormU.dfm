object JvDataEmbeddedMainForm: TJvDataEmbeddedMainForm
  Left = 270
  Top = 271
  Width = 418
  Height = 266
  Caption = 'Example for JvDataEmbedded'
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 152
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Load from EXE'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 6
    Top = 40
    Width = 397
    Height = 193
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object JvDataEmbedded1: TJvDataEmbedded
    Left = 62
    Top = 10
    EmbeddedData = {
      2A01000048656C6C6F210D0A0D0A54686973207465787420686173206265656E
      207772697474656E20616E642062756E646C656420696E746F20746865206578
      6563757461626C652E0D0A5769746820612073696D706C652063616C6C20746F
      20612066756E6374696F6E2C2077652063616E20726574726965766520616E64
      0D0A646973706C61792069742121212049736E27742069742073696D706C653F
      0D0A0D0A4275742074657874206973204E4F5420746865206F6E6C7920706F73
      736962696C6974792120596F752063616E20656D6265640D0A736F756E642C20
      646174612C20696D616765732E2E2E20616E792066696C652063616E20626520
      656D62656464656420696E0D0A746865206170706C69636174696F6E20696E20
      6120666577207365636F6E647321}
  end
end
