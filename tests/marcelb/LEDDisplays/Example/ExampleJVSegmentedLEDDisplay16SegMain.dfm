object fme16SegExamples: Tfme16SegExamples
  Left = 0
  Top = 0
  Width = 633
  Height = 430
  AutoScroll = False
  TabOrder = 0
  object pcSeg16: TPageControl
    Left = 0
    Top = 0
    Width = 633
    Height = 430
    ActivePage = tsInfo
    Align = alClient
    TabOrder = 0
    TabPosition = tpBottom
    object tsInfo: TTabSheet
      Caption = 'Information'
      object lblInfoCaption: TLabel
        Left = 5
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
        Left = 15
        Top = 20
        Width = 606
        Height = 161
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'The 16-segmented display has sixteen segments and an optional do' +
          't. It'#39's commonly used in radio displays for radios with RDS.'#13#10#13#10 +
          'The difference between the 14 and 16 segmented display is the sp' +
          'litting of the A and D segments, resulting in A1, A2, D1 and D2 ' +
          'segments (top left, top right, bottom left and bottom right bars' +
          ' respectively).'#13#10#13#10'With the splitting of the top and bottom bars' +
          ' some additional characters can be displayed, such as brackets, ' +
          'and a better degree symbol is also possible.'#13#10#13#10'The second page ' +
          'provides a testing area where you can control each segment indiv' +
          'iually, as well as specifying a character.'
        WordWrap = True
      end
      object lblCharsetCaption: TLabel
        Left = 5
        Top = 195
        Width = 83
        Height = 13
        Caption = 'Charactersets:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblCharsetMain: TLabel
        Left = 15
        Top = 215
        Width = 223
        Height = 13
        Caption = 'Main characterset (numbers and capital letters):'
      end
      object sldCharsetMain: TJvSegmentLEDDisplay
        Left = 30
        Top = 235
        Width = 576
        Height = 48
        AutoSize = False
        Color = clBlack
        ColorOn = 41215
        ColorOff = 16730
        DigitCount = 16
        DigitHeight = 48
        Digits = <
          item
            Segments = 'A1,A2,B,C,D1,D2,E,F'
            UseDP = False
          end
          item
            Segments = 'B,C'
            UseDP = False
          end
          item
            Segments = 'A1,A2,B,D1,D2,E,G1,G2'
            UseDP = False
          end
          item
            Segments = 'A1,A2,B,C,D1,D2,G1,G2'
            UseDP = False
          end
          item
            Segments = 'B,C,F,G1,G2'
            UseDP = False
          end
          item
            Segments = 'A1,A2,C,D1,D2,F,G1,G2'
            UseDP = False
          end
          item
            Segments = 'A1,A2,C,D1,D2,E,F,G1,G2'
            UseDP = False
          end
          item
            Segments = 'A1,A2,B,C'
            UseDP = False
          end
          item
            Segments = 'A1,A2,B,C,D1,D2,E,F,G1,G2'
            UseDP = False
          end
          item
            Segments = 'A1,A2,B,C,D1,D2,F,G1,G2'
            UseDP = False
          end
          item
            UseDP = False
          end
          item
            Segments = 'A1,A2,B,C,E,F,G1,G2'
            UseDP = False
          end
          item
            Segments = 'A1,A2,B,C,D1,D2,G2,J,M'
            UseDP = False
          end
          item
            Segments = 'A1,A2,D1,D2,E,F'
            UseDP = False
          end
          item
            Segments = 'A1,A2,B,C,D1,D2,J,M'
            UseDP = False
          end
          item
            Segments = 'A1,A2,D1,D2,E,F,G1'
            UseDP = False
          end>
        DigitWidth = 36
        Kind = slk16Segments
        Margin = 4
        SegmentWidth = 2
        SlantAngle = 0
        Spacing = 2
        Text = '0123456789 ABCDE'
      end
      object lblCharsetSpec: TLabel
        Left = 15
        Top = 295
        Width = 457
        Height = 13
        Caption =
          'Alternates (0 (#0), 4 (#4)), Special characters(-, +, *, (, ), °' +
          '(degree), '#39' (minutes), " (seconds) /, \, ,(comma), %, &, .(dot), :, [, ], { and }'
      end
      object sldCharsetSpec: TJvSegmentLEDDisplay
        Left = 30
        Top = 315
        Width = 576
        Height = 48
        AutoSize = False
        Color = clBlack
        ColorOn = 41215
        ColorOff = 16730
        DigitCount = 16
        DigitHeight = 48
        Digits = <
          item
            Segments = 'A1,A2,B,C,D1,D2,E,F,K,N'
            UseDP = False
          end
          item
            Segments = 'F,G1,G2,J,M'
            UseDP = False
          end
          item
            UseDP = False
          end
          item
            Segments = 'G1,G2'
            UseDP = False
          end
          item
            Segments = 'G1,G2,J,M'
            UseDP = False
          end
          item
            Segments = 'G1,G2,H,J,K,L,N'
            UseDP = False
          end
          item
            Segments = 'K,L'
            UseDP = False
          end
          item
            Segments = 'H,N'
            UseDP = False
          end
          item
            Segments = 'A2,B,G2,J'
            UseDP = False
          end
          item
            Segments = 'J'
            UseDP = False
          end
          item
            Segments = 'B,J'
            UseDP = False
          end
          item
            Segments = 'K,N'
            UseDP = False
          end
          item
            Segments = 'H,L'
            UseDP = False
          end
          item
            Segments = 'N'
            UseDP = False
          end
          item
            Segments = 'A1,C,D2,F,G1,G2,J,K,M,N'
            UseDP = False
          end
          item
            Segments = 'A1,A2,D1,D2,E,G1,H,K,L'
            UseDP = False
          end>
        DigitWidth = 36
        Kind = slk16Segments
        Margin = 4
        SegmentWidth = 2
        SlantAngle = 0
        Spacing = 2
        Text = #0#4' -+*()°'#39'"/\,%&'
      end
    end
    object tsTestArea: TTabSheet
      Caption = 'Test area'
      ImageIndex = 1
      object sldTest: TJvSegmentLEDDisplay
        Left = 10
        Top = 15
        Width = 96
        Height = 128
        AutoSize = False
        Color = clBlack
        ColorOn = clRed
        ColorOff = 100
        DigitCount = 1
        DigitHeight = 128
        Digits = <
          item
            Segments = 'A1,A2,B'
            UseDP = True
          end>
        DigitWidth = 96
        Kind = slk16Segments
        Margin = 4
        SegmentWidth = 8
        SlantAngle = 5
        Spacing = 4
      end
      object lblCharacter: TLabel
        Left = 10
        Top = 200
        Width = 49
        Height = 13
        Caption = 'Character:'
      end
      object lblSegString: TLabel
        Left = 10
        Top = 225
        Width = 73
        Height = 13
        Caption = 'Segment string:'
      end
      object btnSetChar: TSpeedButton
        Left = 170
        Top = 195
        Width = 46
        Height = 22
        Caption = 'Set'
        Flat = True
        OnClick = btnSetCharClick
      end
      object btnSetSegs: TSpeedButton
        Left = 330
        Top = 220
        Width = 46
        Height = 22
        Caption = 'Set'
        Flat = True
        OnClick = btnSetSegsClick
      end
      object cxA1: TCheckBox
        Left = 130
        Top = 15
        Width = 150
        Height = 13
        Caption = 'A1 (Top left bar)'
        TabOrder = 0
        OnClick = CheckMarkClicked
      end
      object cxB: TCheckBox
        Left = 130
        Top = 55
        Width = 150
        Height = 13
        Caption = 'B (top right vertical)'
        TabOrder = 2
        OnClick = CheckMarkClicked
      end
      object cxC: TCheckBox
        Left = 130
        Top = 75
        Width = 150
        Height = 13
        Caption = 'C (bottom right vertical)'
        TabOrder = 3
        OnClick = CheckMarkClicked
      end
      object cxD1: TCheckBox
        Left = 130
        Top = 95
        Width = 150
        Height = 13
        Caption = 'D1 (bottom left bar)'
        TabOrder = 4
        OnClick = CheckMarkClicked
      end
      object cxE: TCheckBox
        Left = 130
        Top = 135
        Width = 150
        Height = 13
        Caption = 'E (bottom left vertical)'
        TabOrder = 6
        OnClick = CheckMarkClicked
      end
      object cxF: TCheckBox
        Left = 130
        Top = 155
        Width = 150
        Height = 13
        Caption = 'F (top left vertical)'
        TabOrder = 7
        OnClick = CheckMarkClicked
      end
      object cxG1: TCheckBox
        Left = 280
        Top = 15
        Width = 150
        Height = 13
        Caption = 'G1 (left center bar)'
        TabOrder = 8
        OnClick = CheckMarkClicked
      end
      object cxG2: TCheckBox
        Left = 280
        Top = 35
        Width = 150
        Height = 13
        Caption = 'G2 (right center bar)'
        TabOrder = 9
        OnClick = CheckMarkClicked
      end
      object cxH: TCheckBox
        Left = 280
        Top = 55
        Width = 150
        Height = 13
        Caption = 'H (top left diagonal)'
        TabOrder = 10
        OnClick = CheckMarkClicked
      end
      object cxJ: TCheckBox
        Left = 280
        Top = 75
        Width = 150
        Height = 13
        Caption = 'J (top center vertical)'
        TabOrder = 11
        OnClick = CheckMarkClicked
      end
      object cxK: TCheckBox
        Left = 280
        Top = 95
        Width = 150
        Height = 13
        Caption = 'K (top right diagonal)'
        TabOrder = 12
        OnClick = CheckMarkClicked
      end
      object cxL: TCheckBox
        Left = 280
        Top = 115
        Width = 150
        Height = 13
        Caption = 'L (bottom right diagonal)'
        TabOrder = 13
        OnClick = CheckMarkClicked
      end
      object cxM: TCheckBox
        Left = 280
        Top = 135
        Width = 150
        Height = 13
        Caption = 'M (bottom center vertical)'
        TabOrder = 14
        OnClick = CheckMarkClicked
      end
      object cxN: TCheckBox
        Left = 280
        Top = 155
        Width = 150
        Height = 13
        Caption = 'N (bottom left diagonal)'
        TabOrder = 15
        OnClick = CheckMarkClicked
      end
      object cxDP: TCheckBox
        Left = 280
        Top = 175
        Width = 150
        Height = 13
        Caption = 'DP (dot)'
        TabOrder = 16
        OnClick = CheckMarkClicked
      end
      object edCharacter: TEdit
        Left = 90
        Top = 195
        Width = 76
        Height = 21
        MaxLength = 10
        TabOrder = 17
      end
      object edSegString: TEdit
        Left = 90
        Top = 220
        Width = 236
        Height = 21
        TabOrder = 18
      end
      object cxA2: TCheckBox
        Left = 130
        Top = 35
        Width = 150
        Height = 13
        Caption = 'A2 (Top right bar)'
        TabOrder = 1
        OnClick = CheckMarkClicked
      end
      object cxD2: TCheckBox
        Left = 130
        Top = 115
        Width = 150
        Height = 13
        Caption = 'D2 (bottom right bar)'
        TabOrder = 5
        OnClick = CheckMarkClicked
      end
    end
  end
end
