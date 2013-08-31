object QuickPreviewF: TQuickPreviewF
  Left = 123
  Top = 127
  Width = 312
  Height = 347
  BorderStyle = bsSizeToolWin
  Caption = 'Painter Preview'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 304
    Height = 288
    Align = alClient
    TabOrder = 0
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 256
      Height = 256
      AutoSize = True
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 288
    Width = 304
    Height = 27
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnUse: TSpeedButton
      Left = 16
      Top = 4
      Width = 41
      Height = 22
      Caption = 'Use'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnUseClick
    end
  end
end
