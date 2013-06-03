object FormTip: TFormTip
  Left = 491
  Top = 471
  Anchors = [akLeft, akRight, akBottom]
  BorderStyle = bsDialog
  Caption = 'Tips of the day'
  ClientHeight = 207
  ClientWidth = 314
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 314
    Height = 173
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = 'Panel1'
    TabOrder = 0
    object pnlInner: TPanel
      Left = 5
      Top = 5
      Width = 304
      Height = 163
      Align = alClient
      BorderWidth = 4
      Caption = 'pnlInner'
      TabOrder = 0
      object pnlTip: TPanel
        Left = 5
        Top = 5
        Width = 294
        Height = 153
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsSingle
        Caption = 'pnlTip'
        Color = clInfoBk
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        object Image1: TImage
          Left = 0
          Top = 0
          Width = 40
          Height = 53
          AutoSize = True
          Picture.Data = {
            07544269746D61709A040000424D9A0400000000000076000000280000002800
            0000350000000100040000000000240400000000000000000000100000001000
            000000000000000080000080000000808000800000008000800080800000C0C0
            C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8FFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFF888FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8888
            8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8888888FFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFF888888888FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF88888088888FFFF
            FFFFFFFFFFFFFFFFFFFFFFFF888880F088888FFFFFFFFFFFFFFFFFFFFFFFFFF8
            88880FFF088888FFFFFFFFFFFFFFFFFFFFFFFF888880FEFEF088888FFFFFFFFF
            FFFFFFFFFFFFF888880FFFFFFF088888FFFFFFFFFFFFFFFFFFFF888880FEFEFE
            FEF088888FFFFFFFFFFFFFFFFFF888880FFFFFFFFFFF088888FFFFFFFFFFFFFF
            FF888880FEFEF0000FFEF088888FFFFFFFFFFFFFFF88880FFEFEF0000FFEFF08
            888FFFFFFFFFFFFFFFF880FFFFFF007700FFFFF088FFFFFFFFFFFFFFFFFF0FFE
            FEFE088880FEFEEF0FFFFFFFFFFFFFFFFFF0FFFFFFFF077770FFFFFFF0FFFFFF
            FFFFFFFFFF0FEEFEFEFE088880FEFEEFEF0FFFFFFFFFFFFFFFF8FFFFFFFF0BBB
            B0FFFFFFF8FFFFFFFFFFFFFFFFFF8FFEFEF0BBBBBB0EFEEF8FFFFFFFFFFFFFFF
            FFFFF8FFFF0BBB00BBB0FFF8FFFFFFFFFFFFFFFFFFFFFF8FF0BBBB00BBBB0F8F
            FFFFFFFFFFFFFFFFFFFFFFF8F0BBBB00BBBB08FFFFFFFFFFFFFFFFFFFFFFFFFF
            8BBBBBBBBBBBB0FFFFFFFFFFFFFFFFFFFFFF00FF0BBBBB00BBBBB00F00FFFFFF
            FFFFFFFFFFFFFFF0BBBBBB00BBBBBB0FFFFFFFFFFFFFFFFFBFBFBBF0BBBBBB00
            BBBBBB0FBBFBFBFFFFFFFFFF777800F0BBBBBB00BBBBBB0F008777FFFFFFFFFF
            BFBFBBF0BBBBBB00BBBBBB0FBBFBFBFFFFFFFFFFFFFFFFF0BBBBBB00BBBBBB0F
            FFFFFFFFFFFFFFFFFFFF00FF0BBBBB00BBBBB0FF00FFFFFFFFFFFFFFFFFFFFFF
            F0BBBBBBBBBB0FFFFFFFFFFFFFFFFFFFFFFFFFFFF0BBBBBBBBBB0FFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00BBBBBB00FFFFFFFFFFFFFFFFFFFFFFFFFFB0FFFF0000
            00FFFFF0BFFFFFFFFFFFFFFFFFFFFF8BFFFFFFFFFFFFFFFB8FFFFFFFFFFFFFFF
            FFFB77FFFF0FFB0BFF0FFFFFF7BFFFFFFFFFFFFFFFF7BBFFFFFFFF8FFFFFFFFF
            FBFFFFFFFFFFFFFFFFFFFFFFFFFFFB8BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFF7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB7BFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFB7BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFF}
          Transparent = True
        end
        object did: TLabel
          Left = 48
          Top = 20
          Width = 90
          Height = 13
          Caption = 'Did you know...'
          Color = clInfoBk
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object tip: TLabel
          Left = 6
          Top = 56
          Width = 277
          Height = 83
          AutoSize = False
          Color = clInfoBk
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = False
          WordWrap = True
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 173
    Width = 314
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object BUSpeedButton1: TJvSpeedButton
      Left = 140
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Next'
      OnClick = BUButton1Click
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
    end
    object BUSpeedButton2: TJvSpeedButton
      Left = 230
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Close'
      OnClick = BUButton1Click
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      ModalResult = 1
    end
    object showtips: TCheckBox
      Left = 4
      Top = 8
      Width = 127
      Height = 17
      Caption = '&Show Tips at Startup'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
  end
end
