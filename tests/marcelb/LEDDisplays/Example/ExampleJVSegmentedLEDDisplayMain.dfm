object frmExampleSegmentedLEDDisplayMain: TfrmExampleSegmentedLEDDisplayMain
  Left = 394
  Top = 286
  BorderStyle = bsSingle
  Caption = 'Segmented LED display example'
  ClientHeight = 405
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object sldScroller: TJvSegmentLEDDisplay
    Left = 0
    Top = 0
    Width = 640
    Height = 60
    AutoSize = False
    Color = clBlack
    ColorOn = clLime
    ColorOff = 20224
    DigitCount = 16
    DigitHeight = 60
    Digits = <
      item
        UseDP = False
      end
      item
        UseDP = False
      end
      item
        UseDP = False
      end
      item
        UseDP = False
      end
      item
        UseDP = False
      end
      item
        UseDP = False
      end
      item
        UseDP = False
      end
      item
        UseDP = False
      end
      item
        UseDP = False
      end
      item
        UseDP = False
      end
      item
        UseDP = False
      end
      item
        UseDP = False
      end
      item
        UseDP = False
      end
      item
        UseDP = False
      end
      item
        UseDP = False
      end
      item
        UseDP = False
      end>
    DigitWidth = 40
    Kind = slk14Segments
    Margin = 4
    SegmentWidth = 2
    SlantAngle = 0
    Spacing = 2
  end
  object pcMain: TJvPageControl
    Left = 0
    Top = 60
    Width = 641
    Height = 346
    ActivePage = ts7Seg
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    TabPosition = tpBottom
    object ts7Seg: TTabSheet
      Caption = 'Seven segments'
      object sldCarOdometer1: TJvSegmentLEDDisplay
        Left = 0
        Top = 5
        Width = 144
        Height = 40
        AutoSize = False
        Color = 7376896
        ColorOn = clBlack
        ColorOff = 8030720
        DigitCount = 6
        DigitHeight = 40
        Digits = <
          item
            Segments = 'A,B,C'
            UseDP = True
          end
          item
            Segments = 'B,C,F,G'
            UseDP = True
          end
          item
            Segments = 'A,B,D,E,G'
            UseDP = True
          end
          item
            Segments = 'A,C,D,E,F,G'
            UseDP = True
          end
          item
            Segments = 'A,B,D,E,G'
            UseDP = True
          end
          item
            Segments = 'B,C'
            UseDP = True
          end>
        DigitWidth = 24
        Kind = slk7Segments
        Margin = 4
        SegmentWidth = 2
        SlantAngle = 0
        Spacing = 4
        Text = '742621'
      end
      object sldCarOdoMeter2: TJvSegmentLEDDisplay
        Left = 55
        Top = 45
        Width = 88
        Height = 32
        AutoSize = False
        Color = 7376896
        ColorOn = clBlack
        ColorOff = 8030720
        DigitCount = 4
        DigitHeight = 32
        Digits = <
          item
            Segments = 'A,B,C,D,G'
            UseDP = True
          end
          item
            Segments = 'A,C,D,E,F,G'
            UseDP = True
          end
          item
            Segments = 'A,B,C,D,E,F,G,DP'
            UseDP = True
          end
          item
            Segments = 'A,B,C'
            UseDP = True
          end>
        DigitWidth = 22
        Kind = slk7Segments
        Margin = 4
        SegmentWidth = 2
        SlantAngle = 0
        Spacing = 4
        Text = '36[&8DP]7'
      end
      object pnlCarOdometerRight: TPanel
        Left = 142
        Top = 5
        Width = 29
        Height = 72
        BevelOuter = bvNone
        Color = 7376896
        TabOrder = 0
        object lblCarOdometerTopKM: TLabel
          Left = 0
          Top = 20
          Width = 19
          Height = 16
          Caption = 'KM'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblCarOdometerBottomKM: TLabel
          Left = 0
          Top = 55
          Width = 16
          Height = 13
          Caption = 'KM'
        end
      end
      object pnlCarOdometerLeftBottom: TPanel
        Left = 0
        Top = 45
        Width = 56
        Height = 32
        BevelOuter = bvNone
        Color = 7376896
        TabOrder = 1
      end
      object pnlCarOdometerExplain: TPanel
        Left = 175
        Top = 5
        Width = 456
        Height = 72
        BevelOuter = bvNone
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clInfoText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        object lblCarOdometerExplain: TLabel
          Left = 3
          Top = 3
          Width = 450
          Height = 66
          Anchors = [akLeft, akTop, akRight, akBottom]
          AutoSize = False
          Caption = 
            'The odometer is made up of 2 7-segment displays and two panels (' +
            'one panel on the bottom left to fill up the room). In the right ' +
            'panel two labels are placed to show the text '#39'KM'#39'.'#13#10#13#10'The backgr' +
            'ound color is set to $00709000, ColorOn is set to clBlack and Co' +
            'lorOf is set to $007A8A00.'
          WordWrap = True
        end
      end
    end
  end
  object tmrLEDScroller: TTimer
    Interval = 400
    OnTimer = tmrLEDScrollerTimer
    Left = 545
    Top = 15
  end
end
