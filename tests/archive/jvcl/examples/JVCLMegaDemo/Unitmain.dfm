object Mainform: TMainform
  Left = 184
  Top = 137
  Width = 865
  Height = 517
  Caption = 'JVCL-Demoapplication'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object JvOutlookBar1: TJvOutlookBar
    Tag = 27
    Left = 0
    Top = 0
    Width = 193
    Height = 492
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
            Caption = 'JvControls II'
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
            Caption = 'Edit Components'
            ImageIndex = 0
            Tag = 7
          end
          item
            Caption = 'File Components'
            ImageIndex = 0
            Tag = 8
          end
          item
            Caption = 'Panels'
            ImageIndex = 0
            Tag = 9
          end
          item
            Caption = 'Date / Time'
            ImageIndex = 0
            Tag = 12
          end
          item
            Caption = 'Chooser'
            ImageIndex = 0
            Tag = 13
          end
          item
            Caption = 'Misc'
            ImageIndex = 0
            Tag = 14
          end
          item
            Caption = 'Windows Dialogs'
            ImageIndex = 0
            Tag = 23
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
            Caption = 'JvMonthCalendar2'
            ImageIndex = 0
            Tag = 10
          end
          item
            Caption = 'JvSearchFile'
            ImageIndex = 0
            Tag = 11
          end
          item
            Caption = 'Ani Viewer'
            ImageIndex = 0
            Tag = 15
          end
          item
            Caption = 'JvMousePositionner'
            ImageIndex = 0
            Tag = 16
          end
          item
            Caption = 'JvDataEmbedded'
            ImageIndex = 0
            Tag = 17
          end
          item
            Caption = 'JvBMPAnimator'
            ImageIndex = 0
            Tag = 18
          end
          item
            Caption = 'JvArrowButton'
            ImageIndex = 0
            Tag = 19
          end
          item
            Caption = 'JvClipboardViewer'
            ImageIndex = 0
            Tag = 20
          end
          item
            Caption = 'JvBrowseFolder'
            ImageIndex = 0
            Tag = 21
          end
          item
            Caption = 'JvInstallLabel'
            ImageIndex = 0
            Tag = 22
          end
          item
            Caption = 'JvLogFile'
            ImageIndex = 0
            Tag = 24
          end
          item
            Caption = 'JvOutlookBar'
            ImageIndex = 0
            Tag = 25
          end
          item
            Caption = 'JvChangeNotify (directory changes)'
            ImageIndex = 0
            Tag = 27
          end
          item
            Caption = 'JvCreateProcess'
            ImageIndex = 0
            Tag = 28
          end
          item
            Caption = 'JvNTEventLog'
            ImageIndex = 0
            Tag = 29
          end
          item
            Caption = 'HotKey'
            ImageIndex = 0
            Tag = 30
          end
          item
            Caption = 'JvZoom'
            ImageIndex = 0
            Tag = 6
          end
          item
            Caption = 'JvMail'
            ImageIndex = 0
            Tag = 35
          end
          item
            Caption = 'JvSpecialProgress'
            ImageIndex = 0
            Tag = 32
          end
          item
            Caption = 'JvColorCombo'
            ImageIndex = 0
            Tag = 32
          end
          item
            Caption = 'JvContentScroller'
            ImageIndex = 0
            Tag = 34
          end
          item
            Caption = 'JvInspector (JvInspectorDB)'
            ImageIndex = 0
            Tag = 39
          end
          item
            Caption = 'JvMruList'
            ImageIndex = 0
            Tag = 40
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
            Caption = 'List information about all windows'
            ImageIndex = 0
            Tag = 31
          end
          item
            Caption = 'colored Hint'
            ImageIndex = 0
            Tag = 33
          end
          item
            Caption = 'using a JvTreeView as a Menu'
            ImageIndex = 0
            Tag = 36
          end
          item
            Caption = 'JvList and JvCombo'
            ImageIndex = 0
            Tag = 37
          end
          item
            Caption = 'JvDBDate and JvDBTime'
            ImageIndex = 0
            Tag = 38
          end>
        Caption = 'other and standalone demos (RA, Rx ..)'
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
        Buttons = <>
        Caption = 'Search for a component'
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
    ActivePageIndex = 1
    OnButtonClick = JvOutlookBar1ButtonClick
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TabOrder = 0
  end
  object pnlParent: TPanel
    Left = 193
    Top = 0
    Width = 664
    Height = 492
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
  object JvAutoSizeCompo1: TJvAutoSizeCompo
    Left = 136
    Top = 112
  end
end
