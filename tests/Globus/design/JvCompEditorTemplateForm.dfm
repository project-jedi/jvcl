object JvgCompEditorTemplate: TJvgCompEditorTemplate
  Left = 346
  Top = 119
  Width = 512
  Height = 438
  Caption = 'JEDI VCL Component Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object hwJVCLCompEditor: TJvgWizardHeader
    Left = 0
    Top = 0
    Width = 504
    Height = 33
    CaptionFont.Charset = DEFAULT_CHARSET
    CaptionFont.Color = clWindowText
    CaptionFont.Height = -11
    CaptionFont.Name = 'MS Sans Serif'
    CaptionFont.Style = [fsBold]
    CommentFont.Charset = DEFAULT_CHARSET
    CommentFont.Color = clWindowText
    CommentFont.Height = -11
    CommentFont.Name = 'MS Sans Serif'
    CommentFont.Style = []
    SymbolFont.Charset = DEFAULT_CHARSET
    SymbolFont.Color = clHighlightText
    SymbolFont.Height = -35
    SymbolFont.Name = 'Wingdings'
    SymbolFont.Style = [fsBold]
    PageNo = 0
    Captions.Strings = (
      'JEDI VCL JvgLabel Editor')
    Comments.Strings = (
      ' ')
    Gradient.FromColor = 33023
    Gradient.ToColor = clWindow
    Gradient.Active = True
    Gradient.Orientation = fgdVertical
    BufferedDraw = False
  end
  object pnMain: TPanel
    Left = 0
    Top = 33
    Width = 504
    Height = 325
    Align = alClient
    TabOrder = 0
    object pgMain: TJvgPageControl
      Left = 1
      Top = 1
      Width = 502
      Height = 323
      ActivePage = tabMain
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TabOrder = 0
      TabStop = False
      TabStyle.Borders = [fsdLeft, fsdTop, fsdRight, fsdBottom]
      TabStyle.BevelInner = bvNone
      TabStyle.BevelOuter = bvNone
      TabStyle.Bold = False
      TabStyle.BackgrColor = clBtnShadow
      TabStyle.Font.Charset = DEFAULT_CHARSET
      TabStyle.Font.Color = clBtnHighlight
      TabStyle.Font.Height = -11
      TabStyle.Font.Name = 'Arial'
      TabStyle.Font.Style = []
      TabStyle.CaptionHAlign = fhaCenter
      TabStyle.Gradient.Active = False
      TabStyle.Gradient.Orientation = fgdHorizontal
      TabSelectedStyle.Borders = [fsdLeft, fsdTop, fsdRight, fsdBottom]
      TabSelectedStyle.BevelInner = bvNone
      TabSelectedStyle.BevelOuter = bvNone
      TabSelectedStyle.Bold = False
      TabSelectedStyle.BackgrColor = clBtnFace
      TabSelectedStyle.Font.Charset = DEFAULT_CHARSET
      TabSelectedStyle.Font.Color = clBtnText
      TabSelectedStyle.Font.Height = -11
      TabSelectedStyle.Font.Name = 'Arial'
      TabSelectedStyle.Font.Style = []
      TabSelectedStyle.CaptionHAlign = fhaCenter
      TabSelectedStyle.Gradient.Active = False
      TabSelectedStyle.Gradient.Orientation = fgdHorizontal
      Options = [ftoAutoFontDirection, ftoExcludeGlyphs]
      object tabMain: TTabSheet
        Caption = 'Text'
      end
    end
  end
  object pnBottom: TPanel
    Left = 0
    Top = 358
    Width = 504
    Height = 53
    Align = alBottom
    TabOrder = 1
    object glShadowOK: TJvgShadow
      Left = 436
      Top = 14
      Width = 63
      Height = 32
      Anchors = [akRight, akBottom]
      Style.Inner = bvRaised
      Style.Outer = bvLowered
      Style.Bold = False
      Style.BackgroundColor = 16774365
      Style.ShadowColor = 12615680
      StyleActive.Inner = bvLowered
      StyleActive.Outer = bvRaised
      StyleActive.Bold = False
      ShadowDepth = 5
      AutoTransparentColor = ftcLeftBottomPixel
      MaskedShadow = True
      TransparentColor = clWhite
    end
    object glShadowCancel: TJvgShadow
      Left = 361
      Top = 13
      Width = 67
      Height = 32
      Anchors = [akRight, akBottom]
      Style.Inner = bvRaised
      Style.Outer = bvLowered
      Style.Bold = False
      Style.BackgroundColor = 14674687
      Style.ShadowColor = 4227327
      StyleActive.Inner = bvLowered
      StyleActive.Outer = bvRaised
      StyleActive.Bold = False
      ShadowDepth = 5
      AutoTransparentColor = ftcLeftBottomPixel
      MaskedShadow = True
      TransparentColor = clWhite
    end
    object btnCancel1: TSpeedButton
      Left = 362
      Top = 14
      Width = 60
      Height = 25
      Cursor = crHandPoint
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      Flat = True
      OnClick = btnCancel1Click
    end
    object btnOK1: TSpeedButton
      Left = 437
      Top = 15
      Width = 56
      Height = 25
      Cursor = crHandPoint
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Flat = True
      OnClick = btnOK1Click
    end
  end
end
