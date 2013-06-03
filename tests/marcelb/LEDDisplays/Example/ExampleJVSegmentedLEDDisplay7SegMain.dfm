object fme7SegExamples: Tfme7SegExamples
  Left = 0
  Top = 0
  Width = 633
  Height = 430
  TabOrder = 0
  object pc7Seg: TPageControl
    Left = 0
    Top = 0
    Width = 633
    Height = 430
    ActivePage = tsGeneralInfo
    Align = alClient
    TabOrder = 0
    TabPosition = tpBottom
    object tsGeneralInfo: TTabSheet
      Caption = 'Information'
      ImageIndex = 1
      object sldCharsetMain: TJvSegmentLEDDisplay
        Left = 25
        Top = 150
        Width = 598
        Height = 32
        AutoSize = True
        Color = clBlack
        ColorOn = 41215
        ColorOff = 16730
        DigitCount = 26
        DigitHeight = 32
        Digits = <
          item
            Segments = 'G'
            UseDP = False
          end
          item
            Segments = 'A,B,C,D,E,F'
            UseDP = False
          end
          item
            Segments = 'B,C'
            UseDP = False
          end
          item
            Segments = 'A,B,D,E,G'
            UseDP = False
          end
          item
            Segments = 'A,B,C,D,G'
            UseDP = False
          end
          item
            Segments = 'B,C,F,G'
            UseDP = False
          end
          item
            Segments = 'A,C,D,F,G'
            UseDP = False
          end
          item
            Segments = 'A,C,D,E,F,G'
            UseDP = False
          end
          item
            Segments = 'A,B,C'
            UseDP = False
          end
          item
            Segments = 'A,B,C,D,E,F,G'
            UseDP = False
          end
          item
            Segments = 'A,B,C,D,F,G'
            UseDP = False
          end
          item
            Segments = 'A,B,C,E,F,G'
            UseDP = False
          end
          item
            Segments = 'C,D,E,F,G'
            UseDP = False
          end
          item
            Segments = 'A,D,E,F'
            UseDP = False
          end
          item
            Segments = 'B,C,D,E,G'
            UseDP = False
          end
          item
            Segments = 'A,D,E,F,G'
            UseDP = False
          end
          item
            Segments = 'A,E,F,G'
            UseDP = False
          end
          item
            UseDP = False
          end
          item
            Segments = 'E,G'
            UseDP = False
          end
          item
            Segments = 'C,D,E,G'
            UseDP = False
          end
          item
            UseDP = False
          end
          item
            Segments = 'B,C,E,F,G'
            UseDP = False
          end
          item
            Segments = 'D,E,F'
            UseDP = False
          end
          item
            Segments = 'A,B,E,F,G'
            UseDP = False
          end
          item
            UseDP = False
          end
          item
            Segments = 'B,C,D,E,F'
            UseDP = False
          end>
        DigitWidth = 23
        Kind = slk7Segments
        Margin = 4
        SegmentWidth = 2
        SlantAngle = 0
        Spacing = 2
        Text = '-0123456789ABCDEF RO HLP U'
      end
      object lblCharsetCaption: TLabel
        Left = 0
        Top = 115
        Width = 77
        Height = 13
        Caption = 'Characterset:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblMainCharset: TLabel
        Left = 10
        Top = 135
        Width = 307
        Height = 13
        Caption = 'Main set: -0123456789ABCDEF RO HLP U y #248 '#39' " and space'
      end
      object lblAlternateCharset: TLabel
        Left = 10
        Top = 190
        Width = 368
        Height = 13
        Caption = 
          'Alternates/special: lowercase versions of c, h and u. y #176/#24' +
          '8(degree)  '#39' "'
      end
      object sldTest: TJvSegmentLEDDisplay
        Left = 25
        Top = 270
        Width = 56
        Height = 64
        AutoSize = True
        Color = clBlack
        ColorOn = clRed
        ColorOff = 100
        DigitCount = 1
        DigitHeight = 64
        Digits = <
          item
            Segments = 'A,B,C,D,E,F,G,DP'
            UseDP = True
          end>
        DigitWidth = 56
        Kind = slk7Segments
        Margin = 4
        SegmentWidth = 4
        SlantAngle = 10
        Spacing = 4
        Text = '[&8DP]'
      end
      object lblDescriptionCaption: TLabel
        Left = 0
        Top = 0
        Width = 69
        Height = 13
        Caption = 'Description:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblDescription: TLabel
        Left = 10
        Top = 15
        Width = 611
        Height = 96
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'The 7-segmented display has seven segments and an optional dot. ' +
          'It'#39's commonly used in calculators, digital clocks, car odometers' +
          ', etc. The seven segments are named A - G, where A is top segmen' +
          't, B is the top right, C is the bottom right, D is the bottom E ' +
          'is the bottom left, F is the top left and G is the vertical cent' +
          'er segment. The dot is named DP.'#13#10#13#10'TJvSegmentedLEDDisplay allow' +
          's to both specify characters to render (see the character set be' +
          'low) as well as control each segment individually (where the dot' +
          ' is seen as a separate segment). At the bottom you'#39'll find a tes' +
          't segment where you can check each segment by checking/uncheckin' +
          'g them, as well as an option to enter a character.'
        WordWrap = True
      end
      object lblTestAreaCaption: TLabel
        Left = 0
        Top = 250
        Width = 59
        Height = 13
        Caption = 'Test area:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object sldCharsetAlt: TJvSegmentLEDDisplay
        Left = 27
        Top = 210
        Width = 598
        Height = 32
        AutoSize = True
        Color = clBlack
        ColorOn = 41215
        ColorOff = 16730
        DigitCount = 26
        DigitHeight = 32
        Digits = <
          item
            Segments = 'D,E,G'
            UseDP = False
          end
          item
            Segments = 'C,E,F,G'
            UseDP = False
          end
          item
            Segments = 'C,D,E'
            UseDP = False
          end
          item
            UseDP = False
          end
          item
            Segments = 'B,C,D,F,G'
            UseDP = False
          end
          item
            UseDP = False
          end
          item
            Segments = 'A,B,F,G'
            UseDP = False
          end
          item
            Segments = 'F'
            UseDP = False
          end
          item
            Segments = 'B,F'
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
          end
          item
            UseDP = False
          end
          item
            UseDP = False
          end>
        DigitWidth = 23
        Kind = slk7Segments
        Margin = 4
        SegmentWidth = 2
        SlantAngle = 0
        Spacing = 2
        Text = 'chu y °'#39'"                 '
      end
      object lblCharacter: TLabel
        Left = 300
        Top = 265
        Width = 46
        Height = 13
        Caption = 'Character'
      end
      object lblSegmentStringCaption: TLabel
        Left = 300
        Top = 290
        Width = 73
        Height = 13
        Caption = 'Segment string:'
      end
      object lblA: TLabel
        Left = 192
        Top = 255
        Width = 7
        Height = 13
        Caption = 'A'
      end
      object lblB: TLabel
        Left = 232
        Top = 290
        Width = 7
        Height = 13
        Caption = 'B'
      end
      object lblC: TLabel
        Left = 232
        Top = 320
        Width = 7
        Height = 13
        Caption = 'C'
      end
      object lblD: TLabel
        Left = 192
        Top = 360
        Width = 8
        Height = 13
        Caption = 'D'
      end
      object lblE: TLabel
        Left = 150
        Top = 320
        Width = 7
        Height = 13
        Caption = 'E'
      end
      object lblF: TLabel
        Left = 150
        Top = 290
        Width = 6
        Height = 13
        Caption = 'F'
      end
      object lblG: TLabel
        Left = 205
        Top = 305
        Width = 8
        Height = 13
        Caption = 'G'
      end
      object lblDP: TLabel
        Left = 255
        Top = 350
        Width = 15
        Height = 13
        Caption = 'DP'
      end
      object cxA: TCheckBox
        Left = 190
        Top = 270
        Width = 13
        Height = 13
        Caption = 'A'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = cxAClick
      end
      object cxB: TCheckBox
        Tag = 1
        Left = 215
        Top = 290
        Width = 13
        Height = 13
        Caption = 'B'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = cxAClick
      end
      object cxC: TCheckBox
        Tag = 2
        Left = 215
        Top = 320
        Width = 13
        Height = 13
        Caption = 'C'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = cxAClick
      end
      object cxD: TCheckBox
        Tag = 3
        Left = 190
        Top = 345
        Width = 13
        Height = 13
        Caption = 'D'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = cxAClick
      end
      object cxE: TCheckBox
        Tag = 4
        Left = 160
        Top = 320
        Width = 13
        Height = 13
        Caption = 'E'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = cxAClick
      end
      object cxF: TCheckBox
        Tag = 5
        Left = 160
        Top = 290
        Width = 13
        Height = 13
        Caption = 'F'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = cxAClick
      end
      object cxG: TCheckBox
        Tag = 6
        Left = 190
        Top = 305
        Width = 13
        Height = 13
        Caption = 'G'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = cxAClick
      end
      object cxDP: TCheckBox
        Tag = 7
        Left = 240
        Top = 350
        Width = 13
        Height = 13
        Caption = 'DP'
        Checked = True
        State = cbChecked
        TabOrder = 7
        OnClick = cxAClick
      end
      object edCharacter: TEdit
        Left = 385
        Top = 260
        Width = 16
        Height = 21
        MaxLength = 1
        TabOrder = 8
        Text = '8'
        OnChange = edCharacterChange
      end
      object edSegmentString: TEdit
        Left = 385
        Top = 285
        Width = 76
        Height = 21
        TabOrder = 9
        Text = 'ABCDEFGDP'
        OnChange = edSegmentStringChange
      end
    end
    object tsOdometer: TTabSheet
      Caption = 'Odometer'
      object sldCarOdometer1: TJvSegmentLEDDisplay
        Left = 100
        Top = 32
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
        Left = 156
        Top = 0
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
      object sldHours: TJvSegmentLEDDisplay
        Left = 0
        Top = 0
        Width = 48
        Height = 40
        AutoSize = True
        Color = 7378944
        ColorOn = clBlack
        ColorOff = 7049984
        DigitCount = 2
        DigitHeight = 40
        Digits = <
          item
            Segments = 'A,B,C,D,E,F'
            UseDP = False
          end
          item
            Segments = 'A,B,C,D,E,F'
            UseDP = False
          end>
        DigitWidth = 24
        Kind = slk7Segments
        Margin = 4
        SegmentWidth = 2
        SlantAngle = 0
        Spacing = 4
        Text = '00'
      end
      object sldTemp: TJvSegmentLEDDisplay
        Left = 0
        Top = 40
        Width = 60
        Height = 32
        AutoSize = False
        Color = 7378944
        ColorOn = clBlack
        ColorOff = 7049984
        DigitCount = 3
        DigitHeight = 32
        Digits = <
          item
            UseDP = False
          end
          item
            Segments = 'B,C'
            UseDP = False
          end
          item
            Segments = 'A,B,D,E,G'
            UseDP = False
          end>
        DigitWidth = 20
        Kind = slk7Segments
        Margin = 4
        SegmentWidth = 2
        SlantAngle = 0
        Spacing = 4
        Text = ' 12'
      end
      object sldMinutes: TJvSegmentLEDDisplay
        Left = 55
        Top = 0
        Width = 48
        Height = 40
        AutoSize = True
        Color = 7378944
        ColorOn = clBlack
        ColorOff = 7049984
        DigitCount = 2
        DigitHeight = 40
        Digits = <
          item
            Segments = 'A,B,C,D,E,F'
            UseDP = False
          end
          item
            Segments = 'A,B,C,D,E,F'
            UseDP = False
          end>
        DigitWidth = 24
        Kind = slk7Segments
        Margin = 4
        SegmentWidth = 2
        SlantAngle = 0
        Spacing = 4
        Text = '00'
      end
      object lblCelcius: TLabel
        Left = 70
        Top = 40
        Width = 31
        Height = 32
        AutoSize = False
        Caption = 'c'
        Color = 7378944
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -24
        Font.Name = 'Arial'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Layout = tlBottom
      end
      object btnResetTrip: TSpeedButton
        Left = 275
        Top = 0
        Width = 116
        Height = 22
        Caption = '&Reset trip count'
        Flat = True
        OnClick = btnResetTripClick
      end
      object btn24hClock: TSpeedButton
        Left = 390
        Top = 0
        Width = 116
        Height = 22
        AllowAllUp = True
        GroupIndex = 1
        Down = True
        Caption = '24h clock'
        Flat = True
      end
      object lblSpeed: TLabel
        Left = 275
        Top = 45
        Width = 34
        Height = 13
        Caption = 'Speed:'
      end
      object lblSpeedUnit1: TLabel
        Left = 370
        Top = 35
        Width = 14
        Height = 13
        Caption = 'km'
      end
      object lblSpeedUnitSlash: TLabel
        Left = 385
        Top = 42
        Width = 5
        Height = 13
        Caption = '/'
      end
      object lblSpeedUnit2: TLabel
        Left = 390
        Top = 50
        Width = 6
        Height = 13
        Caption = 'h'
      end
      object pnlCarOdometerRight: TPanel
        Left = 242
        Top = 0
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
        Left = 100
        Top = 0
        Width = 56
        Height = 32
        BevelOuter = bvNone
        Color = 7378944
        TabOrder = 1
        object lblAM: TLabel
          Left = 0
          Top = 5
          Width = 16
          Height = 14
          Caption = 'AM'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 7049984
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lblPM: TLabel
          Left = 0
          Top = 20
          Width = 14
          Height = 14
          Caption = 'PM'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 7049984
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
      end
      object pnlCarOdometerExplain: TPanel
        Left = 0
        Top = 75
        Width = 626
        Height = 323
        Anchors = [akLeft, akTop, akRight, akBottom]
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
          Width = 620
          Height = 315
          Anchors = [akLeft, akTop, akRight, akBottom]
          AutoSize = False
          Caption = 
            'The odometer consists of four 7-segment displays, a couple of pa' +
            'nels (to fill up room and to function as a background for the de' +
            'grees symbol and the colon), 5 labels  (celcius symbol, 2x KM un' +
            'it, AM indicator and PM indicator.'#13#10#13#10'The colors are set in such' +
            ' a way that it should look like a back-lit LCD display: '#13#10'* Back' +
            'ground =     $00709800'#13#10'* Unlit segments = $006B9300'#13#10'* Lit segm' +
            'ents =   $00000000 (clBlack)'
          WordWrap = True
        end
      end
      object pnlDegrees: TPanel
        Left = 60
        Top = 40
        Width = 11
        Height = 32
        BevelOuter = bvNone
        Color = 7378944
        TabOrder = 3
        object sphDegrees: TShape
          Left = 0
          Top = 5
          Width = 10
          Height = 10
          Brush.Style = bsClear
          Shape = stCircle
        end
      end
      object pnlColon: TPanel
        Left = 48
        Top = 0
        Width = 8
        Height = 41
        BevelOuter = bvNone
        Color = 7378944
        TabOrder = 4
        object sphTopColon: TShape
          Left = 2
          Top = 10
          Width = 4
          Height = 4
          Brush.Color = clBlack
          Shape = stCircle
        end
        object shpBottomColon: TShape
          Left = 2
          Top = 25
          Width = 4
          Height = 4
          Brush.Color = clBlack
          Shape = stCircle
        end
      end
      object edSpeed: TEdit
        Left = 315
        Top = 40
        Width = 51
        Height = 21
        TabOrder = 5
        Text = '100'
        OnChange = edSpeedChange
      end
    end
  end
end
