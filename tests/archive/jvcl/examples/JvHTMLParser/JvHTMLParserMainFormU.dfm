object JvHTMLParserMainForm: TJvHTMLParserMainForm
  Left = 365
  Top = 143
  Width = 534
  Height = 441
  Caption = 'JvHtmlParser  DEMO'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object JvPageControl1: TJvPageControl
    Left = 0
    Top = 0
    Width = 526
    Height = 397
    ActivePage = TabSheet2
    Align = alClient
    HotTrack = True
    ParentShowHint = False
    ShowHint = True
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
      end
      object JvDisplayMemo1: TJvMemo
        Left = 0
        Top = 156
        Width = 518
        Height = 179
        AutoSize = False
        MaxLines = 0
        HideCaret = False
        Align = alClient
        BorderStyle = bsNone
        ReadOnly = False
        ScrollBars = ssBoth
        TabOrder = 1
        Wordwrap = False
      end
      object JvPanel1: TJvPanel
        Left = 0
        Top = 335
        Width = 518
        Height = 34
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        MultiLine = False
        object btnProcessTable: TJvImgBtn
          Left = 442
          Top = 5
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Execute'
          TabOrder = 0
          OnClick = Button1Click
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
        Height = 335
        AutoSize = False
        MaxLines = 0
        HideCaret = False
        Align = alClient
        BorderStyle = bsNone
        ReadOnly = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object JvPanel2: TJvPanel
        Left = 0
        Top = 335
        Width = 518
        Height = 34
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        MultiLine = False
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
        Height = 335
        AutoSize = False
        MaxLines = 0
        HideCaret = False
        Align = alClient
        BorderStyle = bsNone
        ReadOnly = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object JvPanel3: TJvPanel
        Left = 0
        Top = 335
        Width = 518
        Height = 34
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        MultiLine = False
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
        Height = 335
        AutoSize = False
        MaxLines = 0
        HideCaret = False
        Align = alClient
        BorderStyle = bsNone
        ReadOnly = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object JvPanel4: TJvPanel
        Left = 0
        Top = 335
        Width = 518
        Height = 34
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        MultiLine = False
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
    Top = 397
    Width = 526
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object JvHtmlParser1: TJvHtmlParser
    FileName = 'sample.htm'
    Left = 40
    Top = 44
  end
end
