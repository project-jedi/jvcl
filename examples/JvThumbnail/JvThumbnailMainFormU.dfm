object JvThumbnailMainForm: TJvThumbnailMainForm
  Left = 106
  Top = 59
  Width = 893
  Height = 647
  Caption = 'JvThumbView Demo'
  Color = clBtnFace
  Constraints.MinHeight = 430
  Constraints.MinWidth = 760
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesktopCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 885
    Height = 596
    ActivePage = TabSheet1
    Align = alClient
    TabIndex = 0
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'ThumbView Component'
      object Splitter1: TSplitter
        Left = 153
        Top = 46
        Width = 3
        Height = 522
        Cursor = crHSplit
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 877
        Height = 46
        Align = alTop
        TabOrder = 0
        object Label1: TLabel
          Left = 148
          Top = 3
          Width = 150
          Height = 17
          Alignment = taCenter
          AutoSize = False
          Caption = 'Thumbnail size'
        end
        object Label2: TLabel
          Left = 2
          Top = 5
          Width = 145
          Height = 15
          Alignment = taCenter
          AutoSize = False
          Caption = 'Select Drive'
        end
        object Label3: TLabel
          Left = 612
          Top = 3
          Width = 61
          Height = 16
          Hint = 'Change the thumbgup to change the space between the thumbnails '
          Alignment = taCenter
          AutoSize = False
          Caption = 'Gap:'
          ParentShowHint = False
          ShowHint = True
        end
        object Label4: TLabel
          Left = 677
          Top = 4
          Width = 61
          Height = 16
          Hint = 'Change the selected to select the Nth element in the list.'
          Alignment = taCenter
          AutoSize = False
          Caption = 'Selected:'
          ParentShowHint = False
          ShowHint = True
        end
        object TrackBar1: TTrackBar
          Left = 148
          Top = 15
          Width = 150
          Height = 28
          Hint = 
            'Change the size property to change the size of the thumbs it acc' +
            'epts values for 10..100'
          Max = 100
          Min = 10
          Orientation = trHorizontal
          ParentShowHint = False
          PageSize = 10
          Frequency = 10
          Position = 100
          SelEnd = 0
          SelStart = 0
          ShowHint = True
          TabOrder = 0
          TickMarks = tmTopLeft
          TickStyle = tsNone
          OnChange = TrackBar1Change
        end
        object DriveComboBox1: TJvDriveCombo
          Left = 2
          Top = 22
          Width = 145
          Height = 22
          DriveTypes = [dtFixed, dtRemote, dtCDROM]
          Offset = 4
          ItemHeight = 16
          TabOrder = 1
        end
        object CheckBox1: TCheckBox
          Left = 304
          Top = 2
          Width = 103
          Height = 17
          Hint = 
            'Set AutoScrolling to true to let the Thumbview component scroll ' +
            'the selected thumb into the screen automatically'
          Caption = 'AutoScrolling'
          Checked = True
          ParentShowHint = False
          ShowHint = True
          State = cbChecked
          TabOrder = 2
          OnClick = CheckBox1Click
        end
        object CheckBox2: TCheckBox
          Left = 304
          Top = 24
          Width = 103
          Height = 17
          Hint = 
            'Set AutoHandlekeyb to true to let the Thumbview Cupture the curs' +
            'or key and navigate through the view using them'
          Caption = 'AutoHandleKeyb'
          Checked = True
          ParentShowHint = False
          ShowHint = True
          State = cbChecked
          TabOrder = 3
          OnClick = CheckBox2Click
        end
        object SpinEdit1: TSpinEdit
          Left = 611
          Top = 21
          Width = 64
          Height = 22
          Hint = 'Change the thumbgup to change the space between the thumbnails '
          MaxLength = 3
          MaxValue = 300
          MinValue = 1
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          Value = 1
          OnChange = SpinEdit1Change
        end
        object SpinEdit2: TSpinEdit
          Left = 676
          Top = 20
          Width = 64
          Height = 22
          Hint = 'Change the thumbgup to change the space between the thumbnails '
          MaxValue = 0
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          Value = 0
          OnChange = SpinEdit2Change
        end
        object CheckBox3: TCheckBox
          Left = 411
          Top = 2
          Width = 58
          Height = 17
          Hint = 
            'Set Sorted to true to sort the files found in the directory by n' +
            'ame (other posibilities in the next version)'
          Caption = 'Sorted'
          Checked = True
          ParentShowHint = False
          ShowHint = True
          State = cbChecked
          TabOrder = 6
        end
        object CheckBox4: TCheckBox
          Left = 411
          Top = 24
          Width = 103
          Height = 17
          Hint = 
            'Set Sorted to true to sort the files found in the directory by n' +
            'ame (other posibilities in the next version)'
          Caption = 'MinMemory'
          Checked = True
          ParentShowHint = False
          ShowHint = True
          State = cbChecked
          TabOrder = 7
          OnClick = CheckBox4Click
        end
        object Button1: TButton
          Left = 489
          Top = 3
          Width = 117
          Height = 20
          Caption = 'Stop Loading'
          Enabled = False
          TabOrder = 8
          OnClick = Button1Click
        end
        object Button2: TButton
          Left = 489
          Top = 24
          Width = 117
          Height = 20
          Caption = 'Edit Selected Thumb'
          TabOrder = 9
          OnClick = Button2Click
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 46
        Width = 153
        Height = 522
        Align = alLeft
        BevelOuter = bvNone
        Caption = 'Panel2'
        TabOrder = 1
        object DirectoryListBox1: TJvDirectoryListBox
          Left = 0
          Top = 0
          Width = 153
          Height = 331
          Hint = 
            'Set the directory property to the directory where your images lo' +
            'cated and wach the thumbview creating this thumbs'
          Align = alClient
          Directory = 'C:\'
          DriveCombo = DriveComboBox1
          ItemHeight = 16
          ScrollBars = ssBoth
          TabOrder = 0
          OnChange = DirectoryListBox1Change
        end
        object RadioGroup1: TRadioGroup
          Left = 0
          Top = 331
          Width = 153
          Height = 86
          Hint = 
            'Change the AlignView property to either use the thumbGup (VTNorm' +
            'al) autocalculate the empty space to equall parts(VTSPACEEQUAL) ' +
            'or Cender the thumbs in the using the htumbgup between them.'
          Align = alBottom
          Caption = ' AlignView '
          ItemIndex = 0
          Items.Strings = (
            'VTNormal'
            'VTCenter'
            'VTFitToScreen')
          TabOrder = 1
          OnClick = RadioGroup1Click
        end
        object RadioGroup2: TRadioGroup
          Left = 0
          Top = 417
          Width = 153
          Height = 105
          Hint = 
            'Change the ScrollMode to change the direction the thumb are scro' +
            'lling in the screen SMVertical You will see a vertical scrollbar' +
            ' SMHorizontal For A horizontal ScrollBar or SMBoth to create a s' +
            'quare view '
          Align = alBottom
          Caption = ' ScrollMode '
          ItemIndex = 2
          Items.Strings = (
            'SMHorizontal'
            'SMVertical'
            'SMBoth')
          TabOrder = 2
          OnClick = RadioGroup2Click
        end
      end
      object Panel3: TPanel
        Left = 156
        Top = 46
        Width = 721
        Height = 522
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Panel3'
        TabOrder = 2
        object Panel4: TPanel
          Left = 0
          Top = 506
          Width = 721
          Height = 16
          Hint = 
            'Read the selectedFile property to get the path+filename of the s' +
            'elected thumb'
          Align = alBottom
          BevelOuter = bvLowered
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object THumbview1: TJvThumbView
          Left = 0
          Top = 0
          Width = 721
          Height = 506
          HorzScrollBar.Tracking = True
          VertScrollBar.Tracking = True
          Align = alClient
          TabOrder = 1
          TabStop = True
          OnDblClick = THumbview1DblClick
          OnMouseUp = THumbview1MouseUp
          AlignView = vtNormal
          AutoScrolling = True
          ThumbGap = 4
          AutoHandleKeyb = True
          MinMemory = True
          MaxWidth = 200
          MaxHeight = 200
          Size = 100
          ScrollMode = smHorizontal
          Sorted = True
          OnStartScanning = THumbview1StartScanning
          OnStopScanning = THumbview1StopScanning
          OnScanProgress = THumbview1ScanProgress
          OnChange = THumbview1Change
          OnKeyUp = THumbview1keyup
          AsButtons = False
          Filter = 
            'Portable Graphics Network (*.png)|*.png|PCX Image (*.pcx)|*.pcx|' +
            'ANI Image (*.ani)|*.ani|JPEG Image File (*.jpg)|*.jpg|Bitmaps (*' +
            '.bmp)|*.bmp|Icons (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf' +
            '|Metafiles (*.wmf)|*.wmf'
          ThumbColor = clNone
          ShowShadow = False
          ShadowColor = clBlack
        end
      end
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 596
    Width = 885
    Height = 24
    Align = alBottom
    BorderWidth = 2
    TabOrder = 1
    object Bevel1: TBevel
      Left = 3
      Top = 3
      Width = 291
      Height = 18
      Align = alLeft
    end
    object JvSpecialProgress1: TJvSpecialProgress
      Left = 5
      Top = 5
      Width = 284
      Height = 13
      EndColor = clNavy
      HintColor = clBtnFace
      Solid = True
      Step = 0
    end
  end
end
