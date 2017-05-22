object PreviewForm: TPreviewForm
  Left = 210
  Top = 112
  BorderStyle = bsToolWindow
  Caption = 'Preview'
  ClientHeight = 183
  ClientWidth = 282
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnShow = ImageChanged
  PixelsPerInch = 96
  TextHeight = 13
  object JvSpeedbar: TJvSpeedBar
    Left = 0
    Top = 152
    Width = 282
    Height = 31
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Align = alBottom
    Options = [sbGrayedBtns]
    BtnOffsetHorz = 3
    BtnOffsetVert = 3
    BtnWidth = 24
    BtnHeight = 23
    BevelOuter = bvNone
    TabOrder = 0
    InternalVer = 1
    object Label1: TLabel
      Left = 180
      Top = 9
      Width = 41
      Height = 13
      Caption = 'Frame:   '
    end
    object FrameNo: TLabel
      Left = 224
      Top = 9
      Width = 14
      Height = 13
      Caption = '%d'
    end
    object SpeedbarSection1: TJvSpeedBarSection
      Caption = 'Tools'
    end
    object PlayBtn: TJvSpeedItem
      Caption = 'Play'
      Glyph.Data = {
        AA000000424DAA000000000000007600000028000000080000000D0000000100
        0400000000003400000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777077
        7777700777777000777770000777700000777000000770000077700007777000
        7777700777777077777777777777}
      Hint = 'Play|'
      Spacing = 1
      Left = 11
      Top = 3
      Visible = True
      OnClick = PlayBtnClick
      SectionName = 'Tools'
    end
    object StopBtn: TJvSpeedItem
      Caption = 'Stop'
      Enabled = False
      Glyph.Data = {
        CE000000424DCE0000000000000076000000280000000A0000000B0000000100
        0400000000005800000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777700
        0000700000000700000070000000070000007000000007000000700000000700
        0000700000000700000070000000070000007000000007000000700000000700
        000070000000070000007777777777000000}
      Hint = 'Stop|'
      Spacing = 1
      Left = 35
      Top = 3
      Visible = True
      OnClick = StopBtnClick
      SectionName = 'Tools'
    end
    object RewindBtn: TJvSpeedItem
      Caption = 'Rewind'
      Enabled = False
      Glyph.Data = {
        CE000000424DCE000000000000007600000028000000100000000B0000000100
        0400000000005800000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7777700777770777770770077770077770077007770007770007700770000770
        0007700700000700000770077000077000077007770007770007700777700777
        700770077777077777077777777777777777}
      Hint = 'Rewind|'
      Spacing = 1
      Left = 67
      Top = 3
      Visible = True
      OnClick = RewindBtnClick
      SectionName = 'Tools'
    end
    object BackBtn: TJvSpeedItem
      AllowTimer = True
      Caption = 'Back One Frame'
      Enabled = False
      Glyph.Data = {
        CE000000424DCE0000000000000076000000280000000D0000000B0000000100
        0400000000005800000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7000777770777707700077770077700770007770007700077000770000700007
        7000700000000007700077000070000770007770007700077000777700777007
        700077777077770770007777777777777000}
      Hint = 'Back One Frame|'
      Spacing = 1
      Left = 91
      Top = 3
      Visible = True
      OnClick = BackBtnClick
      SectionName = 'Tools'
    end
    object NextBtn: TJvSpeedItem
      AllowTimer = True
      Caption = 'Forward One Frame'
      Glyph.Data = {
        CE000000424DCE0000000000000076000000280000000D0000000B0000000100
        0400000000005800000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7000707777077777700070077700777770007000770007777000700007000077
        7000700000000007700070000700007770007000770007777000700777007777
        700070777707777770007777777777777000}
      Hint = 'Forward One Frame|'
      Spacing = 1
      Left = 115
      Top = 3
      Visible = True
      OnClick = NextBtnClick
      SectionName = 'Tools'
    end
    object ForwardBtn: TJvSpeedItem
      Caption = 'Fast Forward'
      Glyph.Data = {
        CE000000424DCE000000000000007600000028000000100000000B0000000100
        0400000000005800000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7777707777707777700770077770077770077000777000777007700007700007
        7007700000700000700770000770000770077000777000777007700777700777
        700770777770777770077777777777777777}
      Hint = 'Fast Forward|'
      Spacing = 1
      Left = 139
      Top = 3
      Visible = True
      OnClick = ForwardBtnClick
      SectionName = 'Tools'
    end
  end
  object SliderPanel: TPanel
    Left = 0
    Top = 109
    Width = 282
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 1
    object Slider: TJvxSlider
      Left = 5
      Top = 5
      Width = 272
      Height = 33
      Align = alClient
      BevelStyle = bvLowered
      Increment = 1
      MaxValue = 10
      Options = []
      TabOrder = 0
      OnChange = SliderChange
    end
  end
  object ImagePanel: TPanel
    Left = 0
    Top = 0
    Width = 282
    Height = 109
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 2
    object Image: TJvGIFAnimator
      Left = 6
      Top = 6
      Width = 270
      Height = 97
      AutoSize = False
      Center = True
      Align = alClient
      OnChange = ImageChanged
      OnFrameChanged = ImageFrameChanged
      OnStart = ImageStartStop
      OnStop = ImageStartStop
    end
  end
end
