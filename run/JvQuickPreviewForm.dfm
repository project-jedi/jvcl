object QuickPreviewForm: TQuickPreviewForm
  Left = 123
  Top = 127
  Width = 312
  Height = 347
  BorderStyle = bsSizeToolWin
  Caption = 'Painter Preview'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 304
    Height = 295
    Align = alClient
    TabOrder = 0
    object PreviewImage: TImage
      Left = 0
      Top = 0
      Width = 208
      Height = 208
      AutoSize = True
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 295
    Width = 304
    Height = 22
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object BtnUse: TSpeedButton
      Left = 13
      Top = 3
      Width = 33
      Height = 18
      Caption = 'Use'
      Flat = True
      OnClick = BtnUseClick
    end
  end
end
