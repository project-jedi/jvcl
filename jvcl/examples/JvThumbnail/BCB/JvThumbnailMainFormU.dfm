object JvThumbnailMainForm: TJvThumbnailMainForm
  Left = 155
  Top = 60
  Width = 892
  Height = 648
  Caption = 'JvThumbView Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 884
    Height = 597
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'ThumbView Component'
      object Splitter1: TSplitter
        Left = 153
        Top = 46
        Width = 3
        Height = 523
        Cursor = crHSplit
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 876
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
        object SpinEdit1: TJvSpinEdit
          Left = 611
          Top = 21
          Width = 64
          Height = 22
          Hint = 'Change the thumbgup to change the space between the thumbnails '
          MaxValue = 300
          MinValue = 1
          Value = 1
          MaxLength = 3
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnChange = SpinEdit1Change
        end
        object SpinEdit2: TJvSpinEdit
          Left = 676
          Top = 20
          Width = 64
          Height = 22
          Hint = 'Change the thumbgup to change the space between the thumbnails '
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
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
          Width = 76
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
        Height = 523
        Align = alLeft
        BevelOuter = bvNone
        Caption = 'Panel2'
        TabOrder = 1
        object DirectoryListBox1: TJvDirectoryListBox
          Left = 0
          Top = 0
          Width = 153
          Height = 332
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
          Top = 332
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
          Top = 418
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
        Width = 720
        Height = 523
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Panel3'
        TabOrder = 2
        object Panel4: TPanel
          Left = 0
          Top = 507
          Width = 720
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
        object thumbView1: TJvThumbView
          Left = 0
          Top = 0
          Width = 720
          Height = 507
          HorzScrollBar.Tracking = True
          VertScrollBar.Tracking = True
          Align = alClient
          TabOrder = 1
          TabStop = True
          OnDblClick = thumbView1DblClick
          OnMouseUp = thumbView1MouseUp
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
          OnStartScanning = thumbView1StartScanning
          OnStopScanning = thumbView1StopScanning
          OnScanProgress = thumbView1ScanProgress
          OnChange = thumbView1Change
          OnKeyUp = thumbView1KeyUp
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
    Top = 597
    Width = 884
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
