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
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 304
    Height = 298
    Align = alClient
    TabOrder = 0
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 208
      Height = 208
      AutoSize = True
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 298
    Width = 304
    Height = 22
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnUse: TSpeedButton
      Left = 13
      Top = 3
      Width = 33
      Height = 18
      Caption = 'Use'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnUseClick
    end
  end
end
