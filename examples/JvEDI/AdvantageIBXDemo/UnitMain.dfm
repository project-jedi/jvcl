object FormMain: TFormMain
  Left = 294
  Top = 253
  Width = 655
  Height = 411
  Caption = 'EDI SDK Database Buffering Demo'
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
  object pcMain: TPageControl
    Left = 8
    Top = 8
    Width = 622
    Height = 365
    ActivePage = TabSheet6
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet6: TTabSheet
      Caption = 'Demo Options'
      object rgDemoDatabaseOption: TRadioGroup
        Left = 8
        Top = 4
        Width = 601
        Height = 97
        Caption = 'Choose Demo Database'
        ItemIndex = 0
        Items.Strings = (
          'Advantage Database 7.0'
          'Interbase 6.0.2.0 (IBX)')
        TabOrder = 0
        OnClick = rgDemoDatabaseOptionClick
      end
      object Button1: TButton
        Left = 12
        Top = 112
        Width = 169
        Height = 25
        Caption = 'Profile'
        TabOrder = 1
        OnClick = Button1Click
      end
      object pb: TProgressBar
        Left = 192
        Top = 24
        Width = 403
        Height = 16
        Anchors = [akLeft, akTop, akRight]
        Step = 1
        TabOrder = 2
      end
      object ProgressBar1: TProgressBar
        Left = 192
        Top = 48
        Width = 403
        Height = 16
        Anchors = [akLeft, akTop, akRight]
        Step = 1
        TabOrder = 3
      end
      object ProgressBar2: TProgressBar
        Left = 192
        Top = 72
        Width = 403
        Height = 16
        Anchors = [akLeft, akTop, akRight]
        Step = 1
        TabOrder = 4
      end
      object Button2: TButton
        Left = 12
        Top = 144
        Width = 169
        Height = 25
        Caption = 'Update Database Buffer'
        TabOrder = 5
        OnClick = Button2Click
      end
      object Memo1: TMemo
        Left = 192
        Top = 108
        Width = 417
        Height = 221
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Terminal'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 6
        WantReturns = False
        WordWrap = False
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Profile'
      object PageControl2: TPageControl
        Left = 8
        Top = 8
        Width = 598
        Height = 325
        ActivePage = TabSheet3
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        object TabSheet3: TTabSheet
          Caption = 'Loops'
          object dbgL: TDBGrid
            Left = 0
            Top = 0
            Width = 590
            Height = 297
            Align = alClient
            DataSource = ADS70_Data.dsLProfile
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
          end
        end
        object TabSheet4: TTabSheet
          Caption = 'Segments'
          ImageIndex = 1
          object dbgS: TDBGrid
            Left = 0
            Top = 0
            Width = 590
            Height = 297
            Align = alClient
            DataSource = ADS70_Data.dsSProfile
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
          end
        end
        object TabSheet5: TTabSheet
          Caption = 'Elements'
          ImageIndex = 2
          object dbgE: TDBGrid
            Left = 0
            Top = 0
            Width = 590
            Height = 297
            Align = alClient
            DataSource = ADS70_Data.dsEProfile
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
          end
        end
      end
    end
  end
  object JvEDIDBSpecProfiler: TJvEDIDBSpecProfiler
    ElementProfiles = ADS70_Data.EProfile
    SegmentProfiles = ADS70_Data.SProfile
    LoopProfiles = ADS70_Data.LProfile
    OnAfterProfiledSegment = JvEDIDBSpecProfilerAfterProfiledSegment
    Left = 280
    Top = 152
  end
  object odOpenEDIFile: TOpenDialog
    DefaultExt = '*.sef'
    FileName = '*.sef'
    Filter = 
      'Standard Exchange Format File|*.sef|EDI Specification File|*.edi' +
      '|Text File|*.txt'
    Options = [ofHideReadOnly, ofNoChangeDir, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 484
    Top = 312
  end
  object JvEDIDBSEFProfiler: TJvEDIDBSEFProfiler
    ElementProfiles = ADS70_Data.EProfile
    SegmentProfiles = ADS70_Data.SProfile
    LoopProfiles = ADS70_Data.LProfile
    OnAfterProfiledSegment = JvEDIDBSEFProfilerAfterProfiledSegment
    Left = 308
    Top = 152
  end
end
