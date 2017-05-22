object JvThumbnailChildForm: TJvThumbnailChildForm
  Left = 270
  Top = 153
  Width = 826
  Height = 502
  Caption = 'JvThumbNail Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter2: TSplitter
    Left = 238
    Top = 0
    Width = 3
    Height = 475
    Cursor = crHSplit
  end
  object Panel7: TPanel
    Left = 241
    Top = 0
    Width = 577
    Height = 475
    Align = alClient
    TabOrder = 0
    object ThumbImage1: TJvThumbImage
      Left = 1
      Top = 144
      Width = 518
      Height = 330
      AutoSize = True
      IgnoreMouse = False
      Angle = AT0
      Zoom = 0
    end
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 575
      Height = 144
      Align = alTop
      TabOrder = 0
      object Label5: TLabel
        Left = 161
        Top = 100
        Width = 90
        Height = 13
        Hint = 'You can set the thumbnails title to what ever you want it to be '
        AutoSize = False
        Caption = 'Thumbnails Title'
      end
      object Bevel1: TBevel
        Left = 156
        Top = 3
        Width = 232
        Height = 137
      end
      object asButtonCB: TCheckBox
        Left = 161
        Top = 7
        Width = 97
        Height = 17
        Hint = 'This will give a button effect when the thumbnail is clicked '
        Caption = 'AsButton'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = asButtonCBClick
      end
      object autoloadCB: TCheckBox
        Left = 161
        Top = 31
        Width = 97
        Height = 17
        Hint = 
          'If AutoLoad = true when you pass a name to the filename it autom' +
          'atically loads it else not(for Future reference where the will b' +
          'e a thumblist file)'
        Caption = 'Autoload'
        Checked = True
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 1
        OnClick = autoloadcbClick
      end
      object minMemCB: TCheckBox
        Left = 161
        Top = 55
        Width = 97
        Height = 17
        Hint = 
          'This will forse the thumbimage to convert all the thumbs in BMP ' +
          'and reduce memory usage us much as possible'
        Caption = 'MinimizeMemory'
        Checked = True
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 2
        OnClick = minmemcbClick
      end
      object titlePlaceGr: TRadioGroup
        Left = 265
        Top = 9
        Width = 113
        Height = 65
        Hint = 'The position of the thumbnail'#39's title'
        Caption = ' Title Placement '
        ItemIndex = 0
        Items.Strings = (
          'T_Up'
          'T_Down'
          'T_None')
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = titleplacegrClick
      end
      object Edit1: TEdit
        Left = 161
        Top = 116
        Width = 224
        Height = 21
        Hint = 'You can set the thumbnails title to what ever you want it to be '
        TabOrder = 4
        Text = 'Edit1'
      end
      object GroupBox1: TGroupBox
        Left = 390
        Top = 8
        Width = 121
        Height = 75
        Caption = 'THUMBIMAGE'
        TabOrder = 5
        object Button4: TButton
          Left = 8
          Top = 16
          Width = 105
          Height = 25
          Caption = 'INVERT'
          TabOrder = 0
          OnClick = Button4Click
        end
        object Button5: TButton
          Left = 8
          Top = 48
          Width = 105
          Height = 25
          Caption = 'GRAYSCALE'
          TabOrder = 1
          OnClick = Button5Click
        end
      end
      object Button1: TButton
        Left = 264
        Top = 88
        Width = 113
        Height = 25
        Caption = 'EXIT'
        ModalResult = 1
        TabOrder = 6
      end
      object AngleGr: TRadioGroup
        Left = 390
        Top = 81
        Width = 123
        Height = 60
        Caption = ' ThumbImage Angle '
        Columns = 2
        Items.Strings = (
          '0'
          '90'
          '180'
          '270')
        TabOrder = 7
        OnClick = AngleGrClick
      end
      object ThumbNail1: TJvThumbnail
        Left = 1
        Top = 1
        Width = 154
        Height = 142
        TabOrder = 8
        OnClick = ThumbNail1Click
        TitleColor = clBtnFace
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        ClientWidth = 146
        ClientHeight = 119
        AsButton = False
        MinimizeMemory = True
        StreamFileType = grBMP
        ShowTitle = False
        TitlePlacement = tpUp
        AutoLoad = True
        ShadowColor = clSilver
        ShowShadow = False
      end
    end
  end
  object Panel6: TPanel
    Left = 0
    Top = 0
    Width = 238
    Height = 475
    Align = alLeft
    TabOrder = 1
    OnResize = Panel6Resize
    object Splitter4: TSplitter
      Left = 1
      Top = 138
      Width = 236
      Height = 3
      Cursor = crVSplit
      Align = alTop
    end
    object DirectoryListBox2: TDirectoryListBox
      Left = 1
      Top = 22
      Width = 236
      Height = 116
      Align = alTop
      FileList = FileListBox1
      ItemHeight = 16
      TabOrder = 0
    end
    object FileListBox1: TFileListBox
      Left = 1
      Top = 164
      Width = 236
      Height = 97
      Align = alClient
      ItemHeight = 13
      Mask = '*.ICO;*.BMP;*.EMF;*.WMF;*.JPG;*.JPEG;'
      TabOrder = 1
      OnChange = FileListBox1Change
    end
    object Panel8: TPanel
      Left = 1
      Top = 261
      Width = 236
      Height = 213
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      OnResize = Panel8Resize
      object Label6: TLabel
        Left = 8
        Top = 0
        Width = 226
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'RED'
        Color = clRed
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object Label7: TLabel
        Left = 8
        Top = 45
        Width = 226
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'GREEN'
        Color = clLime
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object Label8: TLabel
        Left = 7
        Top = 89
        Width = 226
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'BLUE'
        Color = clBlue
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object Label9: TLabel
        Left = 6
        Top = 137
        Width = 91
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'CONTRAST'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = True
      end
      object Label1: TLabel
        Left = 134
        Top = 137
        Width = 91
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'LIGHTNESS'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = True
      end
      object RedBar: TTrackBar
        Left = 0
        Top = 13
        Width = 241
        Height = 29
        Max = 255
        Min = -255
        Orientation = trHorizontal
        PageSize = 20
        Frequency = 51
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 0
        TickMarks = tmBottomRight
        TickStyle = tsNone
      end
      object GreenBar: TTrackBar
        Left = 0
        Top = 59
        Width = 241
        Height = 30
        Max = 255
        Min = -255
        Orientation = trHorizontal
        PageSize = 20
        Frequency = 51
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 1
        TickMarks = tmBottomRight
        TickStyle = tsNone
      end
      object BlueBar: TTrackBar
        Left = -1
        Top = 107
        Width = 241
        Height = 30
        Max = 255
        Min = -255
        Orientation = trHorizontal
        PageSize = 20
        Frequency = 51
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 2
        TickMarks = tmBottomRight
        TickStyle = tsNone
      end
      object contrastBar: TTrackBar
        Left = -1
        Top = 152
        Width = 106
        Height = 30
        Max = 100
        Min = -100
        Orientation = trHorizontal
        PageSize = 20
        Frequency = 51
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 3
        TickMarks = tmBottomRight
        TickStyle = tsNone
      end
      object Button2: TButton
        Left = 8
        Top = 184
        Width = 225
        Height = 25
        Caption = 'APPLY'
        TabOrder = 4
        OnClick = Button2Click
      end
      object LightnessBar: TTrackBar
        Left = 127
        Top = 152
        Width = 106
        Height = 30
        Max = 100
        Min = -100
        Orientation = trHorizontal
        PageSize = 20
        Frequency = 51
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 5
        TickMarks = tmBottomRight
        TickStyle = tsNone
      end
    end
    object Panel9: TPanel
      Left = 1
      Top = 1
      Width = 236
      Height = 21
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Panel9'
      TabOrder = 3
      object DriveComboBox2: TDriveComboBox
        Left = 2
        Top = 2
        Width = 235
        Height = 19
        DirList = DirectoryListBox2
        TabOrder = 0
      end
    end
    object Panel10: TPanel
      Left = 1
      Top = 141
      Width = 236
      Height = 23
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Panel10'
      TabOrder = 4
      OnResize = Panel10Resize
      object FilterComboBox1: TFilterComboBox
        Left = 2
        Top = 0
        Width = 235
        Height = 21
        FileList = FileListBox1
        Filter = 
          'All Known Formats|*.ICO;*.BMP;*.EMF;*.WMF;*.JPG;*.JPEG;|All File' +
          's (*.*)|*.*|JPEG Files|*.JPG;*.JPEG;|BMP Files|*.BMP|WMF Files|*' +
          '.WMF|EMF Files |*.EMF|ICONS |*.ICO'
        TabOrder = 0
      end
    end
  end
end
