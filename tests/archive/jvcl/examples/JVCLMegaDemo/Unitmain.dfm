object Mainform: TMainform
  Left = 190
  Top = 56
  Width = 918
  Height = 606
  Caption = 'JVCL-Demoapplication'
  Color = clBtnFace
  Constraints.MinHeight = 517
  Constraints.MinWidth = 865
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object JvOutlookBar1: TJvOutlookBar
    Tag = 27
    Left = 0
    Top = 0
    Width = 193
    Height = 581
    Align = alLeft
    Pages = <
      item
        Buttons = <
          item
            Caption = 'Welcome'
            ImageIndex = 0
            Tag = 0
          end>
        Caption = 'About'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ButtonSize = olbsLarge
        ParentColor = True
        TopButtonIndex = 0
      end
      item
        Buttons = <
          item
            Caption = 'JvForms'
            ImageIndex = 0
            Tag = 1
          end
          item
            Caption = 'JvDialogs'
            ImageIndex = 0
            Tag = 2
          end
          item
            Caption = 'JvUtils (wait a moment on loading..)'
            ImageIndex = 0
            Tag = 3
          end
          item
            Caption = 'JvLabels'
            ImageIndex = 0
            Tag = 4
          end
          item
            Caption = 'JvControls'
            ImageIndex = 0
            Tag = 26
          end>
        Caption = 'demos by IDE component tabs'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ButtonSize = olbsLarge
        ParentColor = True
        TopButtonIndex = 0
      end
      item
        Buttons = <
          item
            Caption = 'edit components'
            ImageIndex = 0
            Tag = 7
          end
          item
            Caption = 'file related components'
            ImageIndex = 0
            Tag = 41
          end
          item
            Caption = 'panels'
            ImageIndex = 0
            Tag = 9
          end
          item
            Caption = 'date / time'
            ImageIndex = 0
            Tag = 12
          end
          item
            Caption = 'chooser'
            ImageIndex = 0
            Tag = 13
          end
          item
            Caption = 'misc'
            ImageIndex = 0
            Tag = 14
          end
          item
            Caption = 'buttons'
            ImageIndex = 0
            Tag = 42
          end>
        Caption = 'demos by logical categories'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ButtonSize = olbsLarge
        ParentColor = True
        TopButtonIndex = 0
      end
      item
        Buttons = <
          item
            Caption = 'JvRegistryTreeView'
            ImageIndex = 3
            Tag = 57
          end
          item
            Caption = 'JvZLibMultiple'
            ImageIndex = -1
            Tag = 64
          end
          item
            Caption = 'JvTipOfDay'
            ImageIndex = 2
            Tag = 61
          end
          item
            Caption = 'JvTimeLine'
            ImageIndex = 1
            Tag = 60
          end
          item
            Caption = 'JvTMTimeline (Team Manager)'
            ImageIndex = 0
            Tag = 62
          end
          item
            Caption = 'JvTransparentButton'
            ImageIndex = 0
            Tag = 63
          end
          item
            Caption = 'JvTranslator'
            ImageIndex = 0
            Tag = 54
          end
          item
            Caption = 'JvArrowButton'
            ImageIndex = 0
            Tag = 19
          end
          item
            Caption = 'JvBMPAnimator'
            ImageIndex = 0
            Tag = 18
          end
          item
            Caption = 'JvChangeNotify (directory changes)'
            ImageIndex = 0
            Tag = 27
          end
          item
            Caption = 'Ani Viewer'
            ImageIndex = 0
            Tag = 15
          end
          item
            Caption = 'JvDataEmbedded'
            ImageIndex = 0
            Tag = 17
          end
          item
            Caption = 'JvMousePositionner'
            ImageIndex = 0
            Tag = 16
          end
          item
            Caption = 'JvMonthCalendar2'
            ImageIndex = 0
            Tag = 10
          end
          item
            Caption = 'JvMail'
            ImageIndex = 0
            Tag = 35
          end
          item
            Caption = 'JvOutlookBar'
            ImageIndex = 0
            Tag = 25
          end
          item
            Caption = 'JvSearchFile'
            ImageIndex = 0
            Tag = 11
          end
          item
            Caption = 'JvNTEventLog'
            ImageIndex = 0
            Tag = 29
          end
          item
            Caption = 'JvMruList'
            ImageIndex = 0
            Tag = 40
          end
          item
            Caption = 'JvLogFile'
            ImageIndex = 0
            Tag = 24
          end
          item
            Caption = 'JvInstallLabel'
            ImageIndex = 0
            Tag = 22
          end
          item
            Caption = 'HotKey'
            ImageIndex = 0
            Tag = 30
          end
          item
            Caption = 'JvContentScroller'
            ImageIndex = 0
            Tag = 34
          end
          item
            Caption = 'JvBrowseFolder'
            ImageIndex = 0
            Tag = 21
          end
          item
            Caption = 'JvCreateProcess'
            ImageIndex = 0
            Tag = 28
          end
          item
            Caption = 'JvClipboardViewer'
            ImageIndex = 0
            Tag = 20
          end
          item
            Caption = 'JvZoom'
            ImageIndex = 0
            Tag = 6
          end
          item
            Caption = 'JvSpecialProgress'
            ImageIndex = 0
            Tag = 32
          end
          item
            Caption = 'JvColorComboBox'
            ImageIndex = 0
            Tag = 33
          end
          item
            Caption = 'JvInspector (JvInspectorDB)'
            ImageIndex = 0
            Tag = 39
          end
          item
            Caption = 'JvBalloonHint'
            ImageIndex = 0
            Tag = 43
          end
          item
            Caption = 'JvDSADialog (Don'#180't show again)'
            ImageIndex = 0
            Tag = 44
          end
          item
            Caption = 'DSA Message Editor'
            ImageIndex = 0
            Tag = 45
          end
          item
            Caption = 'JvHTMLParser'
            ImageIndex = 0
            Tag = 46
          end
          item
            Caption = 'JvLinkLabel'
            ImageIndex = 0
            Tag = 47
          end
          item
            Caption = 'TJvShellHookComponent'
            ImageIndex = 0
            Tag = 49
          end
          item
            Caption = 'JvShFileOperation'
            ImageIndex = 0
            Tag = 50
          end
          item
            Caption = 'JvSystemPopup demo1'
            ImageIndex = 0
            Tag = 51
          end
          item
            Caption = 'JvSystemPopup demo2'
            ImageIndex = 0
            Tag = 52
          end
          item
            Caption = 'JvProfiler'
            ImageIndex = 0
            Tag = 67
          end>
        Caption = 'demos of single components'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ButtonSize = olbsLarge
        ParentColor = True
        TopButtonIndex = 0
      end
      item
        Buttons = <
          item
            Caption = 'JvDBDate and JvDBTime'
            ImageIndex = 0
            Tag = 38
          end
          item
            Caption = 'list information about all windows'
            ImageIndex = 0
            Tag = 31
          end
          item
            Caption = 'show colored hints'
            ImageIndex = 0
            Tag = 5
          end
          item
            Caption = 'using a JvTreeView as a Menu'
            ImageIndex = 0
            Tag = 36
          end
          item
            Caption = 'Screen capture with unit JvFunctions'
            ImageIndex = 0
            Tag = 48
          end
          item
            Caption = 'JvThumbView and JvThumbImage'
            ImageIndex = 0
            Tag = 53
          end
          item
            Caption = 'JvWndProcHook'
            ImageIndex = 0
            Tag = 56
          end
          item
            Caption = 'RunDll32 '
            ImageIndex = 0
            Tag = 58
          end
          item
            Caption = 'JvScrollingWindow (JvExpressButton)'
            ImageIndex = 0
            Tag = 59
          end>
        Caption = 'other demos'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ButtonSize = olbsLarge
        ParentColor = True
        TopButtonIndex = 0
      end
      item
        Buttons = <
          item
            Caption = 'seach form'
            ImageIndex = 0
            Tag = 0
          end
          item
            Caption = 'BIG not integrated demos (Ra, Rx ..)'
            ImageIndex = 0
            Tag = 66
          end>
        Caption = 'extra'
        Color = clBtnShadow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ButtonSize = olbsLarge
        ParentColor = True
        TopButtonIndex = 0
      end>
    OnButtonClick = JvOutlookBar1ButtonClick
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TabOrder = 0
  end
  object JvCaptionPanel1: TJvCaptionPanel
    Left = 193
    Top = 0
    Width = 716
    Height = 581
    Buttons = []
    CaptionPosition = dpTop
    CaptionFont.Charset = ANSI_CHARSET
    CaptionFont.Color = clWhite
    CaptionFont.Height = -13
    CaptionFont.Name = 'Arial'
    CaptionFont.Style = [fsBold]
    OutlookLook = False
    TabOrder = 1
  end
end
