object frmExampleSegmentedLEDDisplayMain: TfrmExampleSegmentedLEDDisplayMain
  Left = 393
  Top = 291
  BorderStyle = bsSingle
  Caption = 'Segmented LED display example'
  ClientHeight = 507
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
    Height = 448
    ActivePage = ts7Seg
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    TabPosition = tpBottom
    object ts7Seg: TTabSheet
      Caption = 'Seven segments'
      object sldCarOdometer1: TJvSegmentLEDDisplay
        Left = 0
        Top = 37
        Width = 144
        Height = 40
        AutoSize = False
        Color = 7378944
        ColorOn = clBlack
        ColorOff = 7049984
        DigitCount = 6
        DigitHeight = 40
        Digits = <
          item
            UseDP = True
          end
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
        Text = ' 74221'
      end
      object sldCarOdoMeter2: TJvSegmentLEDDisplay
        Left = 56
        Top = 5
        Width = 88
        Height = 32
        AutoSize = False
        Color = 7378944
        ColorOn = clBlack
        ColorOff = 7049984
        DigitCount = 4
        DigitHeight = 32
        Digits = <
          item
            Segments = 'A,C,D,F,G'
            UseDP = True
          end
          item
            Segments = 'A,B,C,D,G'
            UseDP = True
          end
          item
            Segments = 'A,B,C,D,F,G,DP'
            UseDP = True
          end
          item
            Segments = 'B,C'
            UseDP = True
          end>
        DigitWidth = 22
        Kind = slk7Segments
        Margin = 4
        SegmentWidth = 2
        SlantAngle = 0
        Spacing = 4
        Text = '53[&9DP]1'
      end
      object sld7SegTester: TJvSegmentLEDDisplay
        Left = 0
        Top = 90
        Width = 630
        Height = 50
        AutoSize = False
        Color = clBlack
        ColorOn = clRed
        ColorOff = 100
        DigitCount = 18
        DigitHeight = 50
        Digits = <
          item
            UseDP = True
          end
          item
            UseDP = True
          end
          item
            UseDP = True
          end
          item
            UseDP = True
          end
          item
            UseDP = True
          end
          item
            UseDP = True
          end
          item
            UseDP = True
          end
          item
            UseDP = True
          end
          item
            UseDP = True
          end
          item
            UseDP = True
          end
          item
            UseDP = True
          end
          item
            Segments = 'G'
            UseDP = True
          end
          item
            Segments = 'B,C'
            UseDP = True
          end
          item
            Segments = 'A,B,D,E,G'
            UseDP = True
          end
          item
            Segments = 'A,B,C,D,G'
            UseDP = True
          end
          item
            Segments = 'B,C,F,G,DP'
            UseDP = True
          end
          item
            Segments = 'A,C,D,F,G'
            UseDP = True
          end
          item
            Segments = 'A,C,D,E,F,G'
            UseDP = False
          end>
        DigitWidth = 35
        Kind = slk7Segments
        Margin = 4
        SegmentWidth = 2
        SlantAngle = 5
        Spacing = 4
        Text = '           -123[&4DP]56'
      end
      object pnlCarOdometerRight: TPanel
        Left = 142
        Top = 5
        Width = 29
        Height = 72
        BevelOuter = bvNone
        Color = 7378944
        TabOrder = 0
        object lblCarOdometerTopKM: TLabel
          Left = 0
          Top = 53
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
          Top = 16
          Width = 16
          Height = 13
          Caption = 'KM'
        end
      end
      object pnlCarOdometerLeftBottom: TPanel
        Left = 0
        Top = 5
        Width = 56
        Height = 32
        BevelOuter = bvNone
        Color = 7378944
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
            'The odometer is made up of two 7-segment displays and two panels' +
            ': one panel on the top left to fill up the room and one on the r' +
            'ight to show the '#39'KM'#39' labels.'#13#10#13#10'The background color is set to ' +
            '$00709800, ColorOn is set to clBlack and ColorOf is set to $006B' +
            '9300.'
          WordWrap = True
        end
      end
      object insp7SegTester: TJvInspector
        Left = 0
        Top = 145
        Width = 630
        Height = 271
        Anchors = [akLeft, akTop, akRight, akBottom]
        BandWidth = 150
        BevelInner = bvNone
        BevelKind = bkTile
        RelativeDivider = True
        Divider = 50
        ItemHeight = 16
        Painter = idnpMain
        ReadOnly = False
        UseBands = False
        WantTabs = False
      end
    end
  end
  object tmrLEDScroller: TTimer
    Interval = 400
    OnTimer = tmrLEDScrollerTimer
    Left = 545
    Top = 15
  end
  object idnpMain: TJvInspectorDotNETPainter
    Left = 424
    Top = 11
  end
end
