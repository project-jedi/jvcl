object Mainform: TMainform
  Left = 358
  Top = 122
  Width = 812
  Height = 612
  Caption = 'JVCL- MegaDemo'
  Color = clBtnFace
  Constraints.MinHeight = 517
  Constraints.MinWidth = 812
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  ShowHint = True
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object JvCaptionPanel1: TJvCaptionPanel
    Left = 203
    Top = 0
    Width = 742
    Height = 593
    Buttons = []
    CaptionPosition = dpTop
    CaptionFont.Charset = ANSI_CHARSET
    CaptionFont.Color = clWhite
    CaptionFont.Height = -13
    CaptionFont.Name = 'MS Sans Serif'
    CaptionFont.Style = [fsBold]
    OutlookLook = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 593
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object JvOutlookBar1: TJvOutlookBar
      Tag = 27
      Left = 0
      Top = 0
      Width = 201
      Height = 562
      Align = alClient
      Pages = <
        item
          Buttons = <
            item
              Caption = 'JvArrowButton'
              ImageIndex = 0
              Tag = 19
              AutoToggle = False
            end
            item
              Caption = 'JvBalloonHint'
              ImageIndex = 0
              Tag = 43
              AutoToggle = False
            end
            item
              Caption = 'JvBMPAnimator'
              ImageIndex = 0
              Tag = 18
              AutoToggle = False
            end
            item
              Caption = 'JvBrowseFolder'
              ImageIndex = 0
              Tag = 21
              AutoToggle = False
            end
            item
              Caption = 'JvChangeNotify (directory changes)'
              ImageIndex = 0
              Tag = 27
              AutoToggle = False
            end
            item
              Caption = 'JvClipboardViewer'
              ImageIndex = 0
              Tag = 20
              AutoToggle = False
            end
            item
              Caption = 'JvColorComboBox'
              ImageIndex = 0
              Tag = 33
              AutoToggle = False
            end
            item
              Caption = 'JvContentScroller'
              ImageIndex = 0
              Tag = 34
              AutoToggle = False
            end
            item
              Caption = 'JvCreateProcess'
              ImageIndex = 0
              Tag = 28
              AutoToggle = False
            end
            item
              Caption = 'JvDataEmbedded'
              ImageIndex = 0
              Tag = 17
              AutoToggle = False
            end
            item
              Caption = 'JvDSADialog (Don'#180't show again)'
              ImageIndex = 0
              Tag = 44
              AutoToggle = False
            end
            item
              Caption = 'JvDSA Message Editor'
              ImageIndex = 0
              Tag = 45
              AutoToggle = False
            end
            item
              Caption = 'JvFindReplace'
              ImageIndex = 0
              Tag = 68
              AutoToggle = False
            end
            item
              Caption = 'JvHTMLParser'
              ImageIndex = 0
              Tag = 46
              AutoToggle = False
            end
            item
              Caption = 'JvImageWindow'
              ImageIndex = 0
              Tag = 70
              AutoToggle = False
            end
            item
              Caption = 'JvInstallLabel'
              ImageIndex = 0
              Tag = 22
              AutoToggle = False
            end
            item
              Caption = 'JvInspector (JvInspectorDB)'
              ImageIndex = 0
              Tag = 39
              AutoToggle = False
            end
            item
              Caption = 'JvLinkLabel'
              ImageIndex = 0
              Tag = 47
              AutoToggle = False
            end
            item
              Caption = 'JvLogFile'
              ImageIndex = 0
              Tag = 24
              AutoToggle = False
            end
            item
              Caption = 'JvMail'
              ImageIndex = 0
              Tag = 35
              AutoToggle = False
            end
            item
              Caption = 'JvMonthCalendar2'
              ImageIndex = 0
              Tag = 10
              AutoToggle = False
            end
            item
              Caption = 'JvMousePositionner'
              ImageIndex = 0
              Tag = 16
              AutoToggle = False
            end
            item
              Caption = 'JvMruList'
              ImageIndex = 0
              Tag = 40
              AutoToggle = False
            end>
          ButtonSize = olbsLarge
          Caption = 'demos of single components (- JvN*)'
          Color = clBtnShadow
          DownFont.Charset = DEFAULT_CHARSET
          DownFont.Color = clWindowText
          DownFont.Height = -11
          DownFont.Name = 'MS Sans Serif'
          DownFont.Style = []
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentColor = True
          TopButtonIndex = 0
        end
        item
          Buttons = <
            item
              Caption = 'JvNTEventLog'
              ImageIndex = 0
              Tag = 29
              AutoToggle = False
            end
            item
              Caption = 'JvOutlookBar'
              ImageIndex = 0
              Tag = 25
              AutoToggle = False
            end
            item
              Caption = 'JvPlaylist'
              ImageIndex = 0
              Tag = 69
              AutoToggle = False
            end
            item
              Caption = 'JvProfiler'
              ImageIndex = 0
              Tag = 67
              AutoToggle = False
            end
            item
              Caption = 'JvRegistryTreeView'
              ImageIndex = 0
              Tag = 57
              AutoToggle = False
            end
            item
              Caption = 'JvSearchFile'
              ImageIndex = 0
              Tag = 11
              AutoToggle = False
            end
            item
              Caption = 'JvShellHookComponent'
              ImageIndex = 0
              Tag = 49
              AutoToggle = False
            end
            item
              Caption = 'JvShFileOperation'
              ImageIndex = 0
              Tag = 50
              AutoToggle = False
            end
            item
              Caption = 'JvSpecialProgress'
              ImageIndex = 0
              Tag = 32
              AutoToggle = False
            end
            item
              Caption = 'JvSystemPopup'
              ImageIndex = 0
              Tag = 52
              AutoToggle = False
            end
            item
              Caption = 'JvTipOfDay'
              ImageIndex = 0
              Tag = 61
              AutoToggle = False
            end
            item
              Caption = 'JvTimeLine'
              ImageIndex = 0
              Tag = 60
              AutoToggle = False
            end
            item
              Caption = 'JvTMTimeline (Team Manager)'
              ImageIndex = 0
              Tag = 62
              AutoToggle = False
            end
            item
              Caption = 'JvTransparentButton'
              ImageIndex = 0
              Tag = 63
              AutoToggle = False
            end
            item
              Caption = 'JvTranslator'
              ImageIndex = 0
              Tag = 54
              AutoToggle = False
            end
            item
              Caption = 'JvZLibMultiple'
              ImageIndex = 0
              Tag = 64
              AutoToggle = False
            end
            item
              Caption = 'JvZoom'
              ImageIndex = 0
              Tag = 6
              AutoToggle = False
            end
            item
              Caption = 'HotKey'
              ImageIndex = 0
              Tag = 30
              AutoToggle = False
            end>
          ButtonSize = olbsLarge
          Caption = 'demos of single components (JvN* -)'
          Color = clBtnShadow
          DownFont.Charset = DEFAULT_CHARSET
          DownFont.Color = clWindowText
          DownFont.Height = -11
          DownFont.Name = 'MS Sans Serif'
          DownFont.Style = []
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentColor = True
          TopButtonIndex = 0
        end
        item
          Buttons = <
            item
              Caption = 'edit components'
              ImageIndex = 0
              Tag = 7
              AutoToggle = False
            end
            item
              Caption = 'file related components'
              ImageIndex = 0
              Tag = 41
              AutoToggle = False
            end
            item
              Caption = 'panels'
              ImageIndex = 0
              Tag = 9
              AutoToggle = False
            end
            item
              Caption = 'date / time'
              ImageIndex = 0
              Tag = 12
              AutoToggle = False
            end
            item
              Caption = 'chooser'
              ImageIndex = 0
              Tag = 13
              AutoToggle = False
            end
            item
              Caption = 'misc'
              ImageIndex = 0
              Tag = 14
              AutoToggle = False
            end
            item
              Caption = 'buttons'
              ImageIndex = 0
              Tag = 42
              AutoToggle = False
            end>
          ButtonSize = olbsLarge
          Caption = 'demos by logical categories'
          Color = clBtnShadow
          DownFont.Charset = DEFAULT_CHARSET
          DownFont.Color = clWindowText
          DownFont.Height = -11
          DownFont.Name = 'MS Sans Serif'
          DownFont.Style = []
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentColor = True
          TopButtonIndex = 0
        end
        item
          Buttons = <
            item
              Caption = 'JvForms'
              ImageIndex = 0
              Tag = 1
              AutoToggle = False
            end
            item
              Caption = 'JvDialogs'
              ImageIndex = 0
              Tag = 2
              AutoToggle = False
            end
            item
              Caption = 'JvUtils (wait a moment on loading..)'
              ImageIndex = 0
              Tag = 3
              AutoToggle = False
            end
            item
              Caption = 'JvLabels'
              ImageIndex = 0
              Tag = 4
              AutoToggle = False
            end
            item
              Caption = 'JvControls'
              ImageIndex = 0
              Tag = 26
              AutoToggle = False
            end>
          ButtonSize = olbsLarge
          Caption = 'demos by IDE component tabs'
          Color = clBtnShadow
          DownFont.Charset = DEFAULT_CHARSET
          DownFont.Color = clWindowText
          DownFont.Height = -11
          DownFont.Name = 'MS Sans Serif'
          DownFont.Style = []
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentColor = True
          TopButtonIndex = 0
        end
        item
          Buttons = <
            item
              Caption = 'JvDBDate and JvDBTime'
              ImageIndex = 0
              Tag = 38
              AutoToggle = False
            end
            item
              Caption = 'JvScrollingWindow (JvExpressButton)'
              ImageIndex = 0
              Tag = 59
              AutoToggle = False
            end
            item
              Caption = 'JvThumbView and JvThumbImage'
              ImageIndex = 0
              Tag = 53
              AutoToggle = False
            end
            item
              Caption = 'JvTreeView as a Menu'
              ImageIndex = 0
              Tag = 36
              AutoToggle = False
            end
            item
              Caption = 'JvWndProcHook'
              ImageIndex = 0
              Tag = 56
              AutoToggle = False
            end
            item
              Caption = 'Ani Viewer'
              ImageIndex = 0
              Tag = 15
              AutoToggle = False
            end
            item
              Caption = 'list information about all windows'
              ImageIndex = 0
              Tag = 31
              AutoToggle = False
            end
            item
              Caption = 'show colored hints'
              ImageIndex = 0
              Tag = 5
              AutoToggle = False
            end
            item
              Caption = 'Screen capture with unit JvFunctions'
              ImageIndex = 0
              Tag = 48
              AutoToggle = False
            end
            item
              Caption = 'RunDll32 '
              ImageIndex = 0
              Tag = 58
              AutoToggle = False
            end
            item
              Caption = 'BIG not integrated demos (Ra, Rx ..)'
              ImageIndex = 0
              Tag = 66
              AutoToggle = False
            end>
          ButtonSize = olbsLarge
          Caption = 'other demos included in the JVCL'
          Color = clBtnShadow
          DownFont.Charset = DEFAULT_CHARSET
          DownFont.Color = clWindowText
          DownFont.Height = -11
          DownFont.Name = 'MS Sans Serif'
          DownFont.Style = []
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentColor = True
          TopButtonIndex = 0
        end
        item
          Buttons = <
            item
              Caption = 'seach form'
              ImageIndex = 0
              Tag = 8
              AutoToggle = False
            end
            item
              Caption = 'some resources and helper'
              ImageIndex = 0
              Tag = 71
              AutoToggle = False
            end
            item
              Caption = 'welcome page'
              ImageIndex = 0
              Tag = 65
              AutoToggle = False
            end>
          ButtonSize = olbsLarge
          Caption = 'extra'
          Color = clBtnShadow
          DownFont.Charset = DEFAULT_CHARSET
          DownFont.Color = clWindowText
          DownFont.Height = -11
          DownFont.Name = 'MS Sans Serif'
          DownFont.Style = []
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentColor = True
          TopButtonIndex = 0
        end>
      ActivePageIndex = 5
      OnButtonClick = JvOutlookBar1ButtonClick
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 0
      Top = 562
      Width = 201
      Height = 31
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnLoadIde: TButton
        Left = 8
        Top = 4
        Width = 185
        Height = 25
        Hint = 
          'Load the unit of the form at which'#13#10'you currently looking in the' +
          ' Delphi IDE'#13#10'- powered by JCL function :-)'#13#10'thanx to JCL Makers ' +
          'one more time!'
        Caption = 'load the unit of the demo in the IDE'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = btnLoadIdeClick
      end
    end
  end
end
