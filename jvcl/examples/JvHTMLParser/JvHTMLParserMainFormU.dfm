object JvHTMLParserMainForm: TJvHTMLParserMainForm
  Left = 365
  Top = 143
  Width = 534
  Height = 441
  Caption = 'JvHtmlParser  DEMO'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 300
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object JvPageControl1: TJvPageControl
    Left = 0
    Top = 0
    Width = 526
    Height = 395
    ActivePage = TabSheet1
    Align = alClient
    HotTrack = True
    ParentShowHint = False
    ShowHint = True
    TabIndex = 0
    TabOrder = 0
    object TabSheet1: TTabSheet
      Hint = 'Table Demo'
      Caption = 'Table'
      object JvSplitter1: TJvSyncSplitter
        Left = 0
        Top = 153
        Width = 518
        Height = 3
        Cursor = crVSplit
        Align = alTop
        ResizeStyle = rsPattern
      end
      object JvTreeView1: TJvTreeView
        Left = 0
        Top = 0
        Width = 518
        Height = 153
        Align = alTop
        BorderStyle = bsNone
        Indent = 19
        TabOrder = 0
        LineColor = 13160660
      end
      object JvDisplayMemo1: TJvMemo
        Left = 0
        Top = 156
        Width = 518
        Height = 177
        AutoSize = False
        ClipboardCommands = []
        MaxLines = 0
        HideCaret = False
        Align = alClient
        BorderStyle = bsNone
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
      object JvPanel1: TJvPanel
        Left = 0
        Top = 333
        Width = 518
        Height = 34
        MultiLine = False
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        object btnProcessTable: TJvImgBtn
          Left = 442
          Top = 5
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Execute'
          TabOrder = 0
          OnClick = btnProcessTableClick
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = -11
          HotTrackFont.Name = 'MS Sans Serif'
          HotTrackFont.Style = []
        end
      end
    end
    object TabSheet2: TTabSheet
      Hint = 'HTML to text converter'
      Caption = 'HTML2Text'
      ImageIndex = 1
      object JvDisplayMemo2: TJvMemo
        Left = 0
        Top = 0
        Width = 518
        Height = 333
        AutoSize = False
        ClipboardCommands = []
        MaxLines = 0
        HideCaret = False
        Align = alClient
        BorderStyle = bsNone
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object JvPanel2: TJvPanel
        Left = 0
        Top = 333
        Width = 518
        Height = 34
        MultiLine = False
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object btnProcessHTML2Text: TJvImgBtn
          Left = 442
          Top = 5
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Execute'
          TabOrder = 0
          OnClick = btnProcessHTML2TextClick
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = -11
          HotTrackFont.Name = 'MS Sans Serif'
          HotTrackFont.Style = []
        end
      end
    end
    object TabSheet3: TTabSheet
      Hint = 'Simple URL detector'
      Caption = 'URL detect'
      ImageIndex = 2
      object JvDisplayMemo3: TJvMemo
        Left = 0
        Top = 0
        Width = 518
        Height = 333
        AutoSize = False
        ClipboardCommands = []
        MaxLines = 0
        HideCaret = False
        Align = alClient
        BorderStyle = bsNone
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object JvPanel3: TJvPanel
        Left = 0
        Top = 333
        Width = 518
        Height = 34
        MultiLine = False
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object btnProcessURL: TJvImgBtn
          Left = 442
          Top = 5
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Execute'
          TabOrder = 0
          OnClick = btnProcessURLClick
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = -11
          HotTrackFont.Name = 'MS Sans Serif'
          HotTrackFont.Style = []
        end
      end
    end
    object TabSheet4: TTabSheet
      Hint = 'Tags detector'
      Caption = 'Tags'
      ImageIndex = 3
      object JvDisplayMemo4: TJvMemo
        Left = 0
        Top = 0
        Width = 518
        Height = 333
        AutoSize = False
        ClipboardCommands = []
        MaxLines = 0
        HideCaret = False
        Align = alClient
        BorderStyle = bsNone
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object JvPanel4: TJvPanel
        Left = 0
        Top = 333
        Width = 518
        Height = 34
        MultiLine = False
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object btnProcessTags: TJvImgBtn
          Left = 442
          Top = 5
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Execute'
          TabOrder = 0
          OnClick = btnProcessTagsClick
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = -11
          HotTrackFont.Name = 'MS Sans Serif'
          HotTrackFont.Style = []
        end
      end
    end
  end
  object JvStatusBar1: TJvStatusBar
    Left = 0
    Top = 395
    Width = 526
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object btnOpen: TButton
    Left = 16
    Top = 363
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Open...'
    TabOrder = 2
    OnClick = btnOpenClick
  end
  object JvHtmlParser1: TJvHTMLParser
    FileName = 'data\sample.htm'
    Left = 40
    Top = 44
  end
  object OpenDialog1: TOpenDialog
    Filter = 'HTML files|*.htm;*.html;*.htt|All files|*.*'
    InitialDir = '.'
    Left = 176
    Top = 88
  end
end
