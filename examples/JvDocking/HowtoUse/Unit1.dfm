object Form1: TForm1
  Left = 424
  Top = 196
  Width = 467
  Height = 399
  Caption = 'Form1'
  Color = clBackground
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 459
    Height = 372
    Align = alClient
    Alignment = taCenter
    Caption = 
      'Drag the other form to any edge on this form to dock it (this is' +
      ' the server form).'
    Layout = tlCenter
    WordWrap = True
  end
  object lbDockServer1: TJvDockServer
    LeftSplitterStyle.Cursor = crHSplit
    LeftSplitterStyle.ParentColor = False
    TopSplitterStyle.Cursor = crVSplit
    TopSplitterStyle.ParentColor = False
    RightSplitterStyle.Cursor = crHSplit
    RightSplitterStyle.ParentColor = False
    BottomSplitterStyle.Cursor = crVSplit
    BottomSplitterStyle.ParentColor = False
    DockStyle = JvDockVIDStyle1
    Left = 24
    Top = 16
  end
  object JvDockVIDStyle1: TJvDockVIDStyle
    ConjoinServerOption.GrabbersSize = 18
    ConjoinServerOption.SplitterWidth = 4
    ConjoinServerOption.ActiveFont.Charset = ANSI_CHARSET
    ConjoinServerOption.ActiveFont.Color = clWhite
    ConjoinServerOption.ActiveFont.Height = -11
    ConjoinServerOption.ActiveFont.Name = 'Tahoma'
    ConjoinServerOption.ActiveFont.Style = [fsBold]
    ConjoinServerOption.InactiveFont.Charset = ANSI_CHARSET
    ConjoinServerOption.InactiveFont.Color = 13160660
    ConjoinServerOption.InactiveFont.Height = -11
    ConjoinServerOption.InactiveFont.Name = 'Tahoma'
    ConjoinServerOption.InactiveFont.Style = [fsBold]
    ConjoinServerOption.TextAlignment = taLeftJustify
    ConjoinServerOption.ActiveTitleStartColor = 6956042
    ConjoinServerOption.ActiveTitleEndColor = clSkyBlue
    ConjoinServerOption.InactiveTitleStartColor = clGray
    ConjoinServerOption.InactiveTitleEndColor = clSilver
    ConjoinServerOption.TextEllipsis = True
    ConjoinServerOption.SystemInfo = True
    TabServerOption.TabPosition = tpBottom
    TabServerOption.ActiveSheetColor = clBtnFace
    TabServerOption.InactiveSheetColor = clBtnShadow
    TabServerOption.ActiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.ActiveFont.Color = clWindowText
    TabServerOption.ActiveFont.Height = -11
    TabServerOption.ActiveFont.Name = 'MS Sans Serif'
    TabServerOption.ActiveFont.Style = []
    TabServerOption.InactiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.InactiveFont.Color = clWhite
    TabServerOption.InactiveFont.Height = -11
    TabServerOption.InactiveFont.Name = 'MS Sans Serif'
    TabServerOption.InactiveFont.Style = []
    TabServerOption.HotTrackColor = clBlue
    TabServerOption.ShowTabImages = False
    Left = 24
    Top = 48
  end
end

