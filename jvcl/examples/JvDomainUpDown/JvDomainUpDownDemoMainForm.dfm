object JvDomainUpDownDemoMainFrm: TJvDomainUpDownDemoMainFrm
  Left = 252
  Top = 133
  AutoScroll = False
  BorderWidth = 3
  Caption = 'JvDomainUpDown Demo'
  ClientHeight = 391
  ClientWidth = 532
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 300
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 372
    Width = 532
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object WebBrowser1: TWebBrowser
    Left = 0
    Top = 73
    Width = 532
    Height = 299
    Align = alClient
    TabOrder = 1
    OnStatusTextChange = WebBrowser1StatusTextChange
    OnTitleChange = WebBrowser1TitleChange
    OnNavigateComplete2 = WebBrowser1NavigateComplete2
    ControlData = {
      4C000000FC360000E71E00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 532
    Height = 73
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object Label2: TLabel
      Left = 9
      Top = 11
      Width = 51
      Height = 13
      Caption = 'Web sites:'
    end
    object Edit1: TEdit
      Left = 8
      Top = 28
      Width = 401
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'http://www.borland.com'
    end
    object JvDomainUpDown1: TJvDomainUpDown
      Left = 409
      Top = 28
      Width = 15
      Height = 21
      Associate = Edit1
      Items.Strings = (
        'http://www.borland.com'
        'http://community.borland.com'
        'http://www.delphi-jedi.org'
        'http://jvcl.sourceforge.net'
        'http://jcl.sourceforge.net')
      Text = 'http://www.borland.com'
      Anchors = [akTop, akRight]
      TabOrder = 1
      Wrap = True
    end
    object Button1: TButton
      Left = 445
      Top = 27
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Go'
      Default = True
      TabOrder = 2
      OnClick = Button1Click
    end
  end
end
