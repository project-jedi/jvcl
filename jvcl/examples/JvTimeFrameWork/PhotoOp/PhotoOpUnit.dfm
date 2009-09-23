object PhotoOpMain: TPhotoOpMain
  Left = 212
  Top = 138
  Width = 460
  Height = 546
  Caption = 'UTF Demo 1: PhotoOp'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 445
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 73
    Width = 452
    Height = 439
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Day View'
      object JvTFDays1: TJvTFDays
        Left = 0
        Top = 0
        Width = 444
        Height = 411
        ScheduleManager = utfScheduleManager1
        ColHdrHeight = 30
        Cols = <
          item
            Title = ' - '
            Width = 373
          end>
        Template.CompDate = 36406
        Template.LinearDayCount = 1
        Template.LinearEndDate = 36406
        Template.LinearStartDate = 36406
        Grouping = grNone
        TimeBlocks = <>
        TimeBlockProps.BlockHdrAttr.Font.Charset = DEFAULT_CHARSET
        TimeBlockProps.BlockHdrAttr.Font.Color = clWindowText
        TimeBlockProps.BlockHdrAttr.Font.Height = -11
        TimeBlockProps.BlockHdrAttr.Font.Name = 'MS Sans Serif'
        TimeBlockProps.BlockHdrAttr.Font.Style = []
        TimeBlockProps.BlockHdrAttr.FrameColor = clBlack
        TimeBlockProps.SelBlockHdrAttr.Font.Charset = DEFAULT_CHARSET
        TimeBlockProps.SelBlockHdrAttr.Font.Color = clBlack
        TimeBlockProps.SelBlockHdrAttr.Font.Height = -11
        TimeBlockProps.SelBlockHdrAttr.Font.Name = 'MS Sans Serif'
        TimeBlockProps.SelBlockHdrAttr.Font.Style = []
        TimeBlockProps.SelBlockHdrAttr.ParentFont = False
        TimeBlockProps.SelBlockHdrAttr.FrameColor = clBlack
        ApptAttr.Color = clWhite
        ApptAttr.Font.Charset = DEFAULT_CHARSET
        ApptAttr.Font.Color = clWindowText
        ApptAttr.Font.Height = -13
        ApptAttr.Font.Name = 'MS Sans Serif'
        ApptAttr.Font.Style = []
        ApptAttr.ParentFont = False
        SelApptAttr.Color = clWhite
        SelApptAttr.Font.Charset = DEFAULT_CHARSET
        SelApptAttr.Font.Color = clWindowText
        SelApptAttr.Font.Height = -13
        SelApptAttr.Font.Name = 'MS Sans Serif'
        SelApptAttr.Font.Style = []
        SelApptAttr.ParentFont = False
        HdrAttr.Font.Charset = DEFAULT_CHARSET
        HdrAttr.Font.Color = clWindowText
        HdrAttr.Font.Height = -13
        HdrAttr.Font.Name = 'MS Sans Serif'
        HdrAttr.Font.Style = []
        HdrAttr.ParentFont = False
        HdrAttr.FrameColor = clBlack
        SelHdrAttr.Font.Charset = DEFAULT_CHARSET
        SelHdrAttr.Font.Color = clBlack
        SelHdrAttr.Font.Height = -11
        SelHdrAttr.Font.Name = 'MS Sans Serif'
        SelHdrAttr.Font.Style = []
        SelHdrAttr.ParentFont = False
        SelHdrAttr.FrameColor = clBlack
        FancyRowHdrAttr.Hr2400 = False
        FancyRowHdrAttr.MinorFont.Charset = DEFAULT_CHARSET
        FancyRowHdrAttr.MinorFont.Color = clWindowText
        FancyRowHdrAttr.MinorFont.Height = -11
        FancyRowHdrAttr.MinorFont.Name = 'MS Sans Serif'
        FancyRowHdrAttr.MinorFont.Style = []
        FancyRowHdrAttr.MajorFont.Charset = DEFAULT_CHARSET
        FancyRowHdrAttr.MajorFont.Color = clWindowText
        FancyRowHdrAttr.MajorFont.Height = -21
        FancyRowHdrAttr.MajorFont.Name = 'MS Sans Serif'
        FancyRowHdrAttr.MajorFont.Style = []
        SelFancyRowHdrAttr.Hr2400 = False
        SelFancyRowHdrAttr.MinorFont.Charset = DEFAULT_CHARSET
        SelFancyRowHdrAttr.MinorFont.Color = clBlack
        SelFancyRowHdrAttr.MinorFont.Height = -11
        SelFancyRowHdrAttr.MinorFont.Name = 'MS Sans Serif'
        SelFancyRowHdrAttr.MinorFont.Style = []
        SelFancyRowHdrAttr.MajorFont.Charset = DEFAULT_CHARSET
        SelFancyRowHdrAttr.MajorFont.Color = clBlack
        SelFancyRowHdrAttr.MajorFont.Height = -21
        SelFancyRowHdrAttr.MajorFont.Name = 'MS Sans Serif'
        SelFancyRowHdrAttr.MajorFont.Style = []
        SelFancyRowHdrAttr.TickColor = clBlack
        PrimeTime.StartTime = 0.333333333333333
        PrimeTime.EndTime = 0.708333333333333
        PrimeTime.Color = clYellow
        GroupHdrAttr.Font.Charset = DEFAULT_CHARSET
        GroupHdrAttr.Font.Color = clWindowText
        GroupHdrAttr.Font.Height = -11
        GroupHdrAttr.Font.Name = 'MS Sans Serif'
        GroupHdrAttr.Font.Style = []
        GroupHdrAttr.FrameColor = clBlack
        SelGroupHdrAttr.Font.Charset = DEFAULT_CHARSET
        SelGroupHdrAttr.Font.Color = clBlack
        SelGroupHdrAttr.Font.Height = -11
        SelGroupHdrAttr.Font.Name = 'MS Sans Serif'
        SelGroupHdrAttr.Font.Style = []
        SelGroupHdrAttr.ParentFont = False
        SelGroupHdrAttr.FrameColor = clBlack
        LeftCol = 0
        OnDateChanging = JvTFDays1DateChanging
        OnDateChanged = JvTFDays1DateChanged
        OnGranularityChanged = JvTFDays1GranularityChanged
        DateFormat = 'ddddd'
        TimeFormat = 't'
        Align = alClient
        Color = clWhite
        TabOrder = 0
        OnDblClick = JvTFDays1DblClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Week View'
      ImageIndex = 1
      object JvTFWeeks1: TJvTFWeeks
        Left = 0
        Top = 0
        Width = 444
        Height = 411
        ScheduleManager = utfScheduleManager1
        Cells = <
          item
            Color = clBlack
            CellDate = 37270
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37271
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37272
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37273
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37274
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37275
            CellPics = <>
            CanSelect = True
          end>
        GapSize = 4
        TitleAttr.Visible = False
        TitleAttr.TxtAttr.Font.Charset = DEFAULT_CHARSET
        TitleAttr.TxtAttr.Font.Color = clWindowText
        TitleAttr.TxtAttr.Font.Height = -21
        TitleAttr.TxtAttr.Font.Name = 'MS Sans Serif'
        TitleAttr.TxtAttr.Font.Style = [fsBold]
        TitleAttr.Title = 'Week of Jan 14, 2002'
        CellAttr.Font.Charset = DEFAULT_CHARSET
        CellAttr.Font.Color = clWindowText
        CellAttr.Font.Height = -11
        CellAttr.Font.Name = 'MS Sans Serif'
        CellAttr.Font.Style = []
        CellAttr.TitleAttr.Color = clWhite
        CellAttr.TitleAttr.FrameAttr.Color = clGray
        CellAttr.TitleAttr.DayTxtAttr.Font.Charset = DEFAULT_CHARSET
        CellAttr.TitleAttr.DayTxtAttr.Font.Color = clWindowText
        CellAttr.TitleAttr.DayTxtAttr.Font.Height = -11
        CellAttr.TitleAttr.DayTxtAttr.Font.Name = 'MS Sans Serif'
        CellAttr.TitleAttr.DayTxtAttr.Font.Style = []
        CellAttr.DrawBottomLine = False
        SelCellAttr.Font.Charset = DEFAULT_CHARSET
        SelCellAttr.Font.Color = clWindowText
        SelCellAttr.Font.Height = -11
        SelCellAttr.Font.Name = 'MS Sans Serif'
        SelCellAttr.Font.Style = []
        SelCellAttr.TitleAttr.Color = clNavy
        SelCellAttr.TitleAttr.DayTxtAttr.Font.Charset = DEFAULT_CHARSET
        SelCellAttr.TitleAttr.DayTxtAttr.Font.Color = clWhite
        SelCellAttr.TitleAttr.DayTxtAttr.Font.Height = -11
        SelCellAttr.TitleAttr.DayTxtAttr.Font.Name = 'MS Sans Serif'
        SelCellAttr.TitleAttr.DayTxtAttr.Font.Style = []
        SelCellAttr.DrawBottomLine = False
        CellPics = StateImageList
        Viewer = GlanceTextViewer1
        DateFormat = 'ddddd'
        TimeFormat = 't'
        Align = alClient
        ParentColor = True
        TabOrder = 0
        DisplayDate = 37270
        DisplayOrder = doLeftRight
        DWNames.DWN_Sunday = 'S'
        DWNames.DWN_Monday = 'M'
        DWNames.DWN_Tuesday = 'T'
        DWNames.DWN_Wednesday = 'W'
        DWNames.DWN_Thursday = 'T'
        DWNames.DWN_Friday = 'F'
        DWNames.DWN_Saturday = 'S'
        DWTitleAttr.Height = 20
        DWTitleAttr.Visible = False
        DWTitleAttr.TxtAttr.Font.Charset = DEFAULT_CHARSET
        DWTitleAttr.TxtAttr.Font.Color = clWindowText
        DWTitleAttr.TxtAttr.Font.Height = -11
        DWTitleAttr.TxtAttr.Font.Name = 'MS Sans Serif'
        DWTitleAttr.TxtAttr.Font.Style = [fsBold]
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Month View'
      ImageIndex = 2
      object JvTFMonths1: TJvTFMonths
        Left = 0
        Top = 0
        Width = 444
        Height = 411
        ScheduleManager = utfScheduleManager1
        Cells = <
          item
            Color = clBlack
            CellDate = 37255
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37256
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37257
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37258
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37259
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37260
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37261
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37262
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37263
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37264
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37265
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37266
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37267
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37268
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37269
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37270
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37271
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37272
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37273
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37274
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37275
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37276
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37277
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37278
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37279
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37280
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37281
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37282
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37283
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37284
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37285
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37286
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37287
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37288
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37289
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37290
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37291
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37292
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37293
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37294
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37295
            CellPics = <>
            CanSelect = True
          end
          item
            Color = clBlack
            CellDate = 37296
            CellPics = <>
            CanSelect = True
          end>
        GapSize = 0
        TitleAttr.Visible = False
        TitleAttr.TxtAttr.Font.Charset = DEFAULT_CHARSET
        TitleAttr.TxtAttr.Font.Color = clWindowText
        TitleAttr.TxtAttr.Font.Height = -21
        TitleAttr.TxtAttr.Font.Name = 'MS Sans Serif'
        TitleAttr.TxtAttr.Font.Style = [fsBold]
        TitleAttr.Title = 'January 2002'
        CellAttr.Font.Charset = DEFAULT_CHARSET
        CellAttr.Font.Color = clWindowText
        CellAttr.Font.Height = -11
        CellAttr.Font.Name = 'MS Sans Serif'
        CellAttr.Font.Style = []
        CellAttr.TitleAttr.Color = clWhite
        CellAttr.TitleAttr.DayTxtAttr.Font.Charset = DEFAULT_CHARSET
        CellAttr.TitleAttr.DayTxtAttr.Font.Color = clWindowText
        CellAttr.TitleAttr.DayTxtAttr.Font.Height = -11
        CellAttr.TitleAttr.DayTxtAttr.Font.Name = 'MS Sans Serif'
        CellAttr.TitleAttr.DayTxtAttr.Font.Style = []
        CellAttr.DrawBottomLine = False
        SelCellAttr.Font.Charset = DEFAULT_CHARSET
        SelCellAttr.Font.Color = clWindowText
        SelCellAttr.Font.Height = -11
        SelCellAttr.Font.Name = 'MS Sans Serif'
        SelCellAttr.Font.Style = []
        SelCellAttr.TitleAttr.Color = clNavy
        SelCellAttr.TitleAttr.DayTxtAttr.Font.Charset = DEFAULT_CHARSET
        SelCellAttr.TitleAttr.DayTxtAttr.Font.Color = clWhite
        SelCellAttr.TitleAttr.DayTxtAttr.Font.Height = -11
        SelCellAttr.TitleAttr.DayTxtAttr.Font.Name = 'MS Sans Serif'
        SelCellAttr.TitleAttr.DayTxtAttr.Font.Style = []
        SelCellAttr.DrawBottomLine = False
        Viewer = GlanceTextViewer2
        DateFormat = 'ddddd'
        TimeFormat = 't'
        Align = alClient
        ParentColor = True
        TabOrder = 0
        Month = 1
        Year = 2002
        DisplayDate = 37270
        DWNames.DWN_Sunday = 'S'
        DWNames.DWN_Monday = 'M'
        DWNames.DWN_Tuesday = 'T'
        DWNames.DWN_Wednesday = 'W'
        DWNames.DWN_Thursday = 'T'
        DWNames.DWN_Friday = 'F'
        DWNames.DWN_Saturday = 'S'
        DWTitleAttr.FrameAttr.Style = fs3DRaised
        DWTitleAttr.Height = 20
        DWTitleAttr.TxtAttr.Font.Charset = DEFAULT_CHARSET
        DWTitleAttr.TxtAttr.Font.Color = clWindowText
        DWTitleAttr.TxtAttr.Font.Height = -11
        DWTitleAttr.TxtAttr.Font.Name = 'MS Sans Serif'
        DWTitleAttr.TxtAttr.Font.Style = []
        ExtraDayCellAttr.Font.Charset = DEFAULT_CHARSET
        ExtraDayCellAttr.Font.Color = clWindowText
        ExtraDayCellAttr.Font.Height = -11
        ExtraDayCellAttr.Font.Name = 'MS Sans Serif'
        ExtraDayCellAttr.Font.Style = []
        ExtraDayCellAttr.TitleAttr.Color = clWhite
        ExtraDayCellAttr.TitleAttr.DayTxtAttr.Font.Charset = DEFAULT_CHARSET
        ExtraDayCellAttr.TitleAttr.DayTxtAttr.Font.Color = clWindowText
        ExtraDayCellAttr.TitleAttr.DayTxtAttr.Font.Height = -11
        ExtraDayCellAttr.TitleAttr.DayTxtAttr.Font.Name = 'MS Sans Serif'
        ExtraDayCellAttr.TitleAttr.DayTxtAttr.Font.Style = []
        ExtraDayCellAttr.DrawBottomLine = False
        OffDayCellAttr.Font.Charset = DEFAULT_CHARSET
        OffDayCellAttr.Font.Color = clWindowText
        OffDayCellAttr.Font.Height = -11
        OffDayCellAttr.Font.Name = 'MS Sans Serif'
        OffDayCellAttr.Font.Style = []
        OffDayCellAttr.TitleAttr.Color = clWhite
        OffDayCellAttr.TitleAttr.DayTxtAttr.Font.Charset = DEFAULT_CHARSET
        OffDayCellAttr.TitleAttr.DayTxtAttr.Font.Color = clWindowText
        OffDayCellAttr.TitleAttr.DayTxtAttr.Font.Height = -11
        OffDayCellAttr.TitleAttr.DayTxtAttr.Font.Name = 'MS Sans Serif'
        OffDayCellAttr.TitleAttr.DayTxtAttr.Font.Style = []
        OffDayCellAttr.DrawBottomLine = False
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 452
    Height = 73
    Align = alTop
    TabOrder = 1
    object ResourceCombo: TComboBox
      Left = 198
      Top = 40
      Width = 131
      Height = 21
      Hint = 'Display Resource'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      OnChange = ResourceComboChange
    end
    object PrevDateButton: TBitBtn
      Left = 16
      Top = 8
      Width = 24
      Height = 24
      Hint = 'Previous Day'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      TabStop = False
      OnClick = PrevDateButtonClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00111111111111
        1111111111111111111111111111111111111111111111111111111119999991
        111111111F8888811111111119999991111111111FFFFFF11111111111111111
        1111111111111111111111111B1111B11111111118111181111111B111B11B11
        1B111181118118111811111B11B11B11B1111118118118118111111111111111
        11111111111111111111CCBBBBBBBBBBBBCC77F8888888888F77BCCBBBBBBBBB
        BCCC877F88888888F777CCCCBBBBBBBBCCBC7777FF8888FF7787CBCCCCBBBBCC
        CCCB787777FFFF777778BCCCBCCCCCCCCBCC8777877777777877CCCBCCCBCCBC
        CCBC7778777877877787CCBCCCCBCCCBCCCC7787777877787777}
      NumGlyphs = 2
    end
    object NextDateButton: TBitBtn
      Left = 154
      Top = 8
      Width = 24
      Height = 24
      Hint = 'Next Day'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      TabStop = False
      OnClick = NextDateButtonClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00111111199111
        11111111111F8111111111111119911111111111111F81111111111119999991
        111111111F8888811111111119999991111111111FFF88811111111111199111
        11111111111F8111111111111B1991B111111111181F8181111111B111B11B11
        1B111181118118111811111B11B11B11B1111118118118118111111111111111
        11111111111111111111CCBBBBBBBBBBBBCC77F8888888888F77BCCBBBBBBBBB
        BCCC877F88888888F777CCCCBBBBBBBBCCBC7777FF8888FF7787CBCCCCBBBBCC
        CCCB787777FFFF777778BCCCBCCCCCCCCBCC8777877777777877CCCBCCCBCCBC
        CCBC7778777877877787CCBCCCCBCCCBCCCC7787777877787777}
      NumGlyphs = 2
    end
    object NewApptButton: TBitBtn
      Left = 198
      Top = 8
      Width = 24
      Height = 24
      Hint = 'New Appointment'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      TabStop = False
      OnClick = NewApptButtonClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
        003338888888888888F330FFFFFFFFFFF033383333FFFFF338F330FFF80008FF
        F03B38333888883F3888BBBF07FFF70FFBB3888383333383F8F330F87FFFFF78
        F0333838F3333338F8F330F0FFFFFFF0F0333838F333FFF8F8F330F0FFF900F0
        F0333838F3388838F8F330F0FFF0FFF0F0333838F338F338F8F3BBB87FF0FF78
        FBBB88883F38F338388830FF07F0F70FF033383383F8FF8338F330FFF80008FF
        F03338333888883338F330FFFFFFFF00003338333333338888F330B99999990F
        0B3338888888888F88333BFFFBFFFB0033B338FFF8FFF8883383B0000B0000B3
        333B888888888883333833333B3333B333333333383333833333}
      NumGlyphs = 2
    end
    object EditApptButton: TBitBtn
      Left = 225
      Top = 8
      Width = 24
      Height = 24
      Hint = 'Edit Appointment'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      TabStop = False
      OnClick = EditApptButtonClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
        00333888888888888F3330FFFFFFFFFFF033383333FFFF338F3330FFF80008FF
        F0333833388883F38F33BFB007FFF70FF03333F88333383F8F33F0007FFFFF78
        F03338883333338F8F33BFBF0F00FFF0F0333FFF8388FF8F8F33F00000B000F0
        F033388888F8888F8F33BFBFBF00FFF0F0333FFFFF88F38F8F33000000F0FF78
        F03388888838F3838F33B00B07F0F70FF033F88F8338FF838F3300B0F80008FF
        F03388F8F88888FF8F330B0FFFFFFF0000338F8FFFFFFF888F33B0FFFFFFFF0F
        F033F8FFFFFFFF8FF83300999999990F033388888888888F833330FFFFFFFF00
        333338FFFFFFFF88333330000000000333333888888888833333}
      NumGlyphs = 2
    end
    object DeleteApptButton: TBitBtn
      Left = 252
      Top = 8
      Width = 24
      Height = 24
      Hint = 'Delete Appointment'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      TabStop = False
      OnClick = DeleteApptButtonClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00500000000000
        005558888888888888F550FFFFFFFFFFF05558FFFFFFFFFFF8F550FFF80008FF
        F05558FFF88888FFF8F550FF07FFF70FF05558FF87FFF78FF8F550F87FFF0078
        F05558F87FFF8878F8F550F0FFF01100F05558F8FFF88788F8F550F0F5099910
        F05558F8F58F8778F8F550F055099991005558F8558F877788F5505555509990
        305558555558F88878F555555555090B0305555555558F8F878F5555555FF0B0
        B0305555555FF8F8F888555555FFFF0B0B33555555FFFF8F8F785555FFFFFFF0
        BBB35555FFFFFFF8FFF755999999990F0BBB55888888888F8FFF50FFFFFFFF00
        50BB58FFFFFFFF8858FF500000000005550B588888888885558F}
      NumGlyphs = 2
    end
    object ViewSchedsButton: TBitBtn
      Left = 198
      Top = 40
      Width = 24
      Height = 24
      Hint = 'View Schedules (<Shift> + <Insert>)'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      TabStop = False
      OnClick = ViewSchedsButtonClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333030330333333333383833833333330308888883
        0333338388888883833330388F6666F88303383887FFFF788383388FF666666F
        F88338877F8888F778838FFFF660066FFFF887777F8778F777788FFFF660066F
        FFF887777F8778F77778388FF666666FF88338877F8888F7788330388F6666F8
        8303383887FFFF78838333030888888303333383888888838333333330303303
        3333333338383383333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333}
      NumGlyphs = 2
    end
    object HideSchedButton: TBitBtn
      Left = 225
      Top = 40
      Width = 24
      Height = 24
      Hint = 'Hide Selected Schedule (<Shift> + <Delete>)'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      TabStop = False
      OnClick = HideSchedButtonClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333033030333333333383383833333330308808083
        033333838FF8F8F383333038088888808303383FFFFFFFF8F383308887777778
        808338FFFF77777FF8F38887777777777888FFF7777777777FFF877777777777
        7778877777777777777838877777777778833887777777777883333887777778
        8333333887777778833333333888888333333333388888833333333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333}
      NumGlyphs = 2
    end
    object ShareButton: TBitBtn
      Left = 252
      Top = 40
      Width = 24
      Height = 24
      Hint = 'Share Appointment (<Ctrl> + Drag)'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      TabStop = False
      OnClick = ShareButtonClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        777777777777777777777777777707777777777777778777777777777700B000
        77777777778F788F77777777008B0BB0077777778F87F778F777750088B8B00B
        B0177FFF88787F877F8755BBB8BB0BB08B118F7778778778878F55BBBBBBBB08
        BB118F77777777F8778F55BBBBBBBB8BBB118F7777777787778F55BBB88BBBBB
        BB118FF778F77777778F550BBB888BB800118F8F778FFFFFFF8F5570BBB08880
        77118878FFF8888F778F57770007000777718777888788877778777777777777
        7777777777777777777777777777777777777777777777777777777777777777
        7777777777777777777777777777777777777777777777777777}
      NumGlyphs = 2
    end
    object TimeIncCombo: TComboBox
      Left = 328
      Top = 8
      Width = 105
      Height = 21
      Hint = 'Time Increments'
      Style = csDropDownList
      DropDownCount = 12
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = TimeIncComboChange
      Items.Strings = (
        '60 mins'
        '30 mins'
        '20 mins'
        '15 mins'
        '12 mins'
        '10 mins'
        '6 mins'
        '5 mins'
        '4 mins'
        '3 mins'
        '2 mins'
        '1 min')
    end
    object GotoDatePicker: TDateTimePicker
      Left = 40
      Top = 8
      Width = 113
      Height = 24
      Hint = 'Current Day'
      CalAlignment = dtaLeft
      Date = 36892.4147108796
      Time = 36892.4147108796
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkDate
      ParseInput = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      OnChange = GotoDatePickerChange
    end
    object ModeCombo: TComboBox
      Left = 16
      Top = 40
      Width = 163
      Height = 21
      Hint = 'Display Mode'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      OnChange = ModeComboChange
      Items.Strings = (
        'Single'
        'Group')
    end
    object DaysCombo: TComboBox
      Left = 344
      Top = 40
      Width = 89
      Height = 21
      Hint = 'Visible Days'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      OnChange = DaysComboChange
      Items.Strings = (
        '31 days'
        '14 days'
        '7 days'
        '5 days'
        '3 days'
        '2 days'
        '1 day')
    end
    object PrintButton: TBitBtn
      Left = 279
      Top = 8
      Width = 24
      Height = 24
      Hint = 'Print'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      OnClick = PrintButtonClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
        0003377777777777777308888888888888807F33333333333337088888888888
        88807FFFFFFFFFFFFFF7000000000000000077777777777777770F8F8F8F8F8F
        8F807F333333333333F708F8F8F8F8F8F9F07F333333333337370F8F8F8F8F8F
        8F807FFFFFFFFFFFFFF7000000000000000077777777777777773330FFFFFFFF
        03333337F3FFFF3F7F333330F0000F0F03333337F77773737F333330FFFFFFFF
        03333337F3FF3FFF7F333330F00F000003333337F773777773333330FFFF0FF0
        33333337F3F37F3733333330F08F0F0333333337F7337F7333333330FFFF0033
        33333337FFFF7733333333300000033333333337777773333333}
      NumGlyphs = 2
    end
  end
  object utfScheduleManager1: TJvTFScheduleManager
    OnRefreshAppt = utfScheduleManager1RefreshAppt
    OnPostAppt = utfScheduleManager1PostAppt
    OnDeleteAppt = utfScheduleManager1DeleteAppt
    StateImages = StateImageList
    StateImageMap.AlarmEnabled = 0
    StateImageMap.AlarmDisabled = 1
    StateImageMap.Shared = 3
    StateImageMap.Recurring = -1
    StateImageMap.Modified = 2
    OnLoadBatch = utfScheduleManager1LoadBatch
    SchedLoadMode = slmBatch
    Left = 72
    Top = 112
  end
  object StateImageList: TImageList
    Left = 100
    Top = 112
    Bitmap = {
      494C010104000500040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      7B00000000000000000000000000000000000000000000000000000000000000
      7B0000000000000000000000000000000000000000007B7B7B007B7B7B000000
      0000FFFFFF007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B00FFFFFF00FFFF
      FF007B7B7B007B7B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000007B000000
      000000000000000000000000FF00000000000000FF0000000000000000000000
      000000007B000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B007B7B7B00000000007B7B7B00FFFFFF007B7B7B007B7B7B007B7B
      7B00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF00BDBDBD00FFFFFF0000000000FFFFFF00BDBDBD000000FF000000
      00000000000000000000000000000000000000000000000000007B7B7B007B7B
      7B000000000000000000FFFFFF007B7B7B000000000000000000FFFFFF007B7B
      7B007B7B7B00FFFFFF0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF0000000000FFFF
      FF000000000000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00000000000000
      FF00BDBDBD0000000000FFFFFF00FFFFFF00FFFFFF0000000000BDBDBD000000
      FF00000000007B7B7B00000000000000000000000000000000007B7B7B000000
      0000000000007B7B7B0000000000FFFFFF00000000007B7B7B00000000000000
      00007B7B7B0000000000FFFFFF0000000000FFFF0000000000000000000000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      FF000000FF00000000000000000000000000000000007B7B7B00FFFFFF000000
      0000FFFFFF00000000007B7B7B0000000000FFFFFF000000000000000000FFFF
      FF00FFFFFF007B7B7B00FFFFFF0000000000FFFF00000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      0000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00000000000000
      00000000FF00000000000000000000000000000000007B7B7B00FFFFFF007B7B
      7B000000000000000000000000007B7B7B00FFFFFF00000000007B7B7B007B7B
      7B00000000007B7B7B00FFFFFF0000000000FFFF000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000FFFFFF000000
      000000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF00000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF000000
      FF000000FF00000000000000000000000000000000007B7B7B0000000000FFFF
      FF000000000000000000FFFFFF007B7B7B00FFFFFF0000000000FFFFFF000000
      0000000000007B7B7B000000000000000000FFFF00000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF00000000000000000000000000000000000000000000FF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000FF0000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00000000000000
      FF00BDBDBD0000000000FFFFFF0000000000FFFFFF0000000000BDBDBD000000
      FF00000000007B7B7B00000000000000000000000000000000007B7B7B00FFFF
      FF00FFFFFF007B7B7B00000000007B7B7B00FFFFFF007B7B7B00000000000000
      00007B7B7B00FFFFFF000000000000000000FFFF000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF00BDBDBD00FFFFFF0000000000FFFFFF00BDBDBD000000FF000000
      00000000000000000000000000000000000000000000FFFFFF007B7B7B007B7B
      7B00FFFFFF00FFFFFF00000000007B7B7B000000000000000000000000007B7B
      7B007B7B7B00FFFFFF00FFFFFF00FFFFFF00FFFF00000000000000FFFF00FFFF
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B0000FFFF007B7B
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B00FFFFFF0000000000000000000000000000FF
      FF00FFFFFF0000FFFF00000000000000000000FFFF0000000000FFFFFF00FFFF
      FF000000000000000000FFFFFF00000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000FFFF0000FFFF0000FF
      FF000000000000000000000000000000000000000000BDBDBD00BDBDBD007B7B
      7B0000000000000000000000000000000000000000000000000000000000BDBD
      BD007B7B7B007B7B7B0000000000000000007B7B7B00FFFFFF00000000000000
      00007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B000000
      000000000000000000007B7B7B00FFFFFF000000000000000000000000000000
      000000000000000000000000000000FFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      FF000000FF000000FF000000000000000000000000007B7B7B0000FFFF007B7B
      7B000000000000000000000000000000000000000000BDBDBD00BDBDBD00BDBD
      BD00BDBDBD007B7B7B007B7B7B00000000007B7B7B00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD007B7B7B0000000000000000007B7B7B0000000000FFFFFF000000
      000000000000000000007B7B7B007B7B7B007B7B7B0000000000FFFFFF000000
      000000000000000000007B7B7B00000000000000000000000000000000000000
      0000000000000000000000FFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B00BDBD
      BD00BDBDBD00000000007B7B7B00000000007B7B7B0000000000BDBDBD00BDBD
      BD007B7B7B00000000000000000000000000000000007B7B7B0000000000FFFF
      FF00FFFFFF007B7B7B00000000007B7B7B00FFFFFF007B7B7B0000000000FFFF
      FF00FFFFFF007B7B7B0000000000000000000000000000000000000000000000
      00000000000000FFFF0000000000FFFFFF00FFFFFF000000000000000000FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7B7B0000000000000000000000000000000000000000007B7B7B007B7B
      7B007B7B7B0000000000000000007B7B7B00FFFFFF00FFFFFF007B7B7B007B7B
      7B007B7B7B000000000000000000000000000000000000000000000000000000
      000000FFFF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B7B7B007B7B7B007B7B7B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000088239003FC00FFFFC007E203FC00F83F
      C007CCC32000E7CF8003DAB50000DFF7800395610000B01B80038E490000B83B
      8003AC5B00007C7D8003C23300007E8DC00782E0000041050001000000004105
      0001301CE000410500015C5DF800818B8003A223F00081FBC6C7C607E001DFF7
      FC7FFC7FC403E7CFFFFFFFFFEC07F83F00000000000000000000000000000000
      000000000000}
  end
  object NeedApptsQuery: TQuery
    DatabaseName = 'UTF'
    SQL.Strings = (
      'Select * From GroupLink, GroupAppt'
      '  Where (SchedName = :SchedName)'
      '    And (GroupLink.ApptID = GroupAppt.ApptID)'
      '    And ((StartDate >= :D1) and (EndDate <= :D2))')
    Left = 72
    Top = 152
    ParamData = <
      item
        DataType = ftString
        Name = 'SchedName'
        ParamType = ptUnknown
      end
      item
        DataType = ftDate
        Name = 'D1'
        ParamType = ptUnknown
      end
      item
        DataType = ftDate
        Name = 'D2'
        ParamType = ptUnknown
      end>
  end
  object ApptSchedulesQuery: TQuery
    DatabaseName = 'UTF'
    RequestLive = True
    SQL.Strings = (
      'Select * From GroupLink'
      '  Where ApptID = :ApptID')
    Left = 104
    Top = 152
    ParamData = <
      item
        DataType = ftString
        Name = 'ApptID'
        ParamType = ptUnknown
      end>
  end
  object GetApptQuery: TQuery
    DatabaseName = 'UTF'
    RequestLive = True
    SQL.Strings = (
      'Select * From GroupAppt'
      '  Where ApptID = :ApptID')
    Left = 136
    Top = 152
    ParamData = <
      item
        DataType = ftString
        Name = 'ApptID'
        ParamType = ptUnknown
      end>
  end
  object DeleteApptLinkQuery: TQuery
    DatabaseName = 'UTF'
    SQL.Strings = (
      'Delete From GroupLink'
      '  Where ApptID = :ApptID')
    Left = 168
    Top = 152
    ParamData = <
      item
        DataType = ftString
        Name = 'ApptID'
        ParamType = ptUnknown
      end>
  end
  object DeleteApptQuery: TQuery
    DatabaseName = 'UTF'
    SQL.Strings = (
      'Delete From GroupAppt'
      '  Where ApptID = :ApptID')
    Left = 200
    Top = 152
    ParamData = <
      item
        DataType = ftString
        Name = 'ApptID'
        ParamType = ptUnknown
      end>
  end
  object SchedulesQuery: TQuery
    DatabaseName = 'UTF'
    SQL.Strings = (
      'Select Distinct(SchedName) From GroupLink')
    Left = 232
    Top = 152
  end
  object JvTFDaysPrinter1: TJvTFDaysPrinter
    ScheduleManager = utfScheduleManager1
    PageLayout.FooterHeight = 0
    PageLayout.HeaderHeight = 0
    PageLayout.MarginLeft = 25
    PageLayout.MarginTop = 17
    PageLayout.MarginRight = 25
    PageLayout.MarginBottom = 63
    PageLayout.ColsPerPage = 0
    PageLayout.RowsPerPage = 0
    PageLayout.AlwaysShowColHdr = False
    PageLayout.AlwaysShowRowHdr = False
    OnPrintProgress = JvTFDaysPrinter1PrintProgress
    OnAssembleProgress = JvTFDaysPrinter1AssembleProgress
    Title = 'SIMPLDaysPrinter Demo'
    ApptAttr.Color = clLime
    ApptAttr.Font.Charset = DEFAULT_CHARSET
    ApptAttr.Font.Color = clWindowText
    ApptAttr.Font.Height = -13
    ApptAttr.Font.Name = 'MS Sans Serif'
    ApptAttr.Font.Style = []
    ApptAttr.ParentFont = False
    ApptBuffer = 0
    ColHdrHeight = 0
    Color = clBlack
    Cols = <>
    ColTitleStyle = ctsSingleClip
    DateFormat = 'ddddd'
    FancyRowHdrAttr.Hr2400 = False
    FancyRowHdrAttr.MinorFont.Charset = DEFAULT_CHARSET
    FancyRowHdrAttr.MinorFont.Color = clWindowText
    FancyRowHdrAttr.MinorFont.Height = -13
    FancyRowHdrAttr.MinorFont.Name = 'MS Sans Serif'
    FancyRowHdrAttr.MinorFont.Style = []
    FancyRowHdrAttr.MajorFont.Charset = DEFAULT_CHARSET
    FancyRowHdrAttr.MajorFont.Color = clWindowText
    FancyRowHdrAttr.MajorFont.Height = -27
    FancyRowHdrAttr.MajorFont.Name = 'MS Sans Serif'
    FancyRowHdrAttr.MajorFont.Style = []
    FormattedDesc = False
    Granularity = 1
    GridLineColor = clBlack
    GroupHdrAttr.Font.Charset = DEFAULT_CHARSET
    GroupHdrAttr.Font.Color = clWindowText
    GroupHdrAttr.Font.Height = -13
    GroupHdrAttr.Font.Name = 'MS Sans Serif'
    GroupHdrAttr.Font.Style = []
    GroupHdrAttr.ParentFont = False
    GroupHdrAttr.FrameColor = clBlack
    Grouping = grNone
    HdrAttr.Font.Charset = DEFAULT_CHARSET
    HdrAttr.Font.Color = clWindowText
    HdrAttr.Font.Height = -13
    HdrAttr.Font.Name = 'MS Sans Serif'
    HdrAttr.Font.Style = []
    HdrAttr.ParentFont = False
    HdrAttr.FrameColor = clBlack
    MinColWidth = 5
    MinRowHeight = 1
    PrimeTime.StartTime = 0.333333333333333
    PrimeTime.EndTime = 0.708333333333333
    PrimeTime.Color = clYellow
    RowHdrType = rhGrid
    RowHdrWidth = 0
    RowHeight = 0
    ShowPics = False
    ShowText = False
    TimeFormat = 't'
    OnApptProgress = JvTFDaysPrinter1ApptProgress
    Left = 128
    Top = 112
  end
  object GlanceTextViewer1: TJvTFGlanceTextViewer
    LineSpacing = 2
    Left = 156
    Top = 112
  end
  object GlanceTextViewer2: TJvTFGlanceTextViewer
    LineSpacing = 2
    ShowStartEnd = False
    Left = 184
    Top = 112
  end
  object dbUTF: TDatabase
    DatabaseName = 'UTF'
    DriverName = 'STANDARD'
    LoginPrompt = False
    Params.Strings = (
      'DEFAULT DRIVER=PARADOX'
      'ENABLE BCD=FALSE')
    SessionName = 'Default'
    Left = 72
    Top = 184
  end
end
