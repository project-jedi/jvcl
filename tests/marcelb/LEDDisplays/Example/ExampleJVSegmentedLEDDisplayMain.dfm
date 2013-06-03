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
    ActivePage = ts16Segs
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    TabPosition = tpBottom
    object ts7Seg: TTabSheet
      Caption = 'Seven segments'
      inline fme7SegExamples1: Tfme7SegExamples
        Height = 420
        Align = alClient
        inherited pc7Seg: TPageControl
          Height = 420
        end
      end
    end
    object ts14Seg: TTabSheet
      Caption = 'Fourteen segments'
      ImageIndex = 1
      inline fme14SegExamples1: Tfme14SegExamples
        Height = 420
        Align = alClient
        inherited pcSeg14: TPageControl
          Height = 420
          inherited tsTestArea: TTabSheet
            inherited sldTest: TJvSegmentLEDDisplay
              Digits = <
                item
                  Segments = 'A,B,C,D,E,F,G1,J,K,L,M,N'
                  UseDP = True
                end>
            end
          end
        end
      end
    end
    object ts16Segs: TTabSheet
      Caption = 'Sixteen segments'
      ImageIndex = 2
      inline fme16SegExamples1: Tfme16SegExamples
        Height = 420
        Align = alClient
        inherited pcSeg16: TPageControl
          Height = 420
          inherited tsTestArea: TTabSheet
            inherited sldTest: TJvSegmentLEDDisplay
              Digits = <
                item
                  Segments = 'A1,A2,B,C'
                  UseDP = True
                end>
            end
          end
        end
      end
    end
  end
  object tmrUpdater: TTimer
    Interval = 200
    OnTimer = tmrUpdaterTimer
    Left = 545
    Top = 15
  end
end
