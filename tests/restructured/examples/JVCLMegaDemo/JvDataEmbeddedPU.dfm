object JvDataEmbeddedFrm: TJvDataEmbeddedFrm
  Left = 0
  Top = 0
  Width = 576
  Height = 366
  TabOrder = 0
  object JvCaptionPanel1: TJvCaptionPanel
    Left = 72
    Top = 45
    Width = 481
    Height = 313
    Buttons = [capClose, capHelp]
    Caption = 'JvDataEmbedded Demo'
    CaptionFont.Charset = DEFAULT_CHARSET
    CaptionFont.Color = clWhite
    CaptionFont.Height = -13
    CaptionFont.Name = 'Arial'
    CaptionFont.Style = [fsBold]
    TabOrder = 0
    object Button1: TButton
      Left = 176
      Top = 24
      Width = 97
      Height = 25
      Caption = 'Load From Exe'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Memo1: TMemo
      Left = 46
      Top = 72
      Width = 397
      Height = 193
      Lines.Strings = (
        'Memo1')
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 1
    end
  end
  object JvDataEmbedded1: TJvDataEmbedded
    Size = 298
    Left = 72
    Top = 32
    EmbeddedData = {
      2A01000048656C6C6F210D0A0D0A54686973207465787420686173206265656E
      207772697474656E20616E642062756E646C656420696E746F20746865206578
      6563757461626C652E0D0A5769746820612073696D706C652063616C6C20746F
      20612066756E6374696F6E2C2077652063616E20726574726965766520616E64
      0D0A646973706C61792069742121212049736E27742073696D706C65203F3F3F
      0D0A0D0A4275742074657874206973204E4F5420746865206F6E6C7920706F73
      736962696C6974792120596F752063616E20656D6265640D0A736F756E642C20
      646174612C20696D616765732E2E2E20616E792066696C652063616E20626520
      656D62656464656720696E0D0A746865206170706C69636174696F6E20696E20
      6120666577207365636F6E647321}
  end
end
