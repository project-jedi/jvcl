object FileListBoxMainForm: TFileListBoxMainForm
  Left = 280
  Top = 132
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'File and directory related examples'
  ClientHeight = 454
  ClientWidth = 596
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesktopCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvLabel6: TJvLabel
    Left = 24
    Top = 16
    Width = 452
    Height = 32
    Caption = 
      'Here you can see the combination of a JvDriveCombo (at the top)'#13 +
      #10'a JvDirectoryListBox (at the left) and a JvFileListBox (at the ' +
      'right)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = 0
  end
  object JvLabel8: TJvLabel
    Left = 32
    Top = 328
    Width = 153
    Height = 13
    Caption = 'Here you can see a JvDriveList:'
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = 0
  end
  object Label1: TLabel
    Left = 240
    Top = 352
    Width = 15
    Height = 13
    Caption = 'C:\'
  end
  object Label2: TLabel
    Left = 216
    Top = 328
    Width = 201
    Height = 13
    Caption = 'This label shows always the selected path:'
  end
  object Label3: TLabel
    Left = 216
    Top = 400
    Width = 162
    Height = 13
    Caption = 'This label shows always the mask:'
  end
  object JvDriveList1: TJvDriveList
    Left = 40
    Top = 344
    Width = 145
    Height = 97
    ScrollBars = ssBoth
    DriveTypes = [dtFixed, dtRemote, dtCDROM]
    ImageSize = isSmall
    ItemHeight = 37
    TabOrder = 0
  end
  object JvCaptionPanel1: TJvCaptionPanel
    Left = 24
    Top = 56
    Width = 553
    Height = 257
    Buttons = [capClose]
    Caption = 'Mini Explorer'
    CaptionFont.Charset = DEFAULT_CHARSET
    CaptionFont.Color = clWhite
    CaptionFont.Height = -13
    CaptionFont.Name = 'MS Shell Dlg 2'
    CaptionFont.Style = [fsBold]
    FlatButtons = True
    OutlookLook = False
    TabOrder = 1
    OnButtonClick = JvCaptionPanel1ButtonClick
    object JvFileListBox1: TJvFileListBox
      Left = 217
      Top = 23
      Width = 329
      Height = 227
      Align = alRight
      FileEdit = Edit1
      ItemHeight = 16
      ShowGlyphs = True
      TabOrder = 0
      ForceFileExtensions = False
    end
    object JvDriveCombo1: TJvDriveCombo
      Left = 22
      Top = 1
      Width = 524
      Height = 22
      Align = alTop
      DriveTypes = [dtFixed, dtRemote, dtCDROM]
      Offset = 4
      ItemHeight = 16
      TabOrder = 1
    end
    object JvDirectoryListBox1: TJvDirectoryListBox
      Left = 22
      Top = 23
      Width = 195
      Height = 227
      Align = alClient
      Directory = 'C:\'
      DirLabel = Label1
      FileList = JvFileListBox1
      DriveCombo = JvDriveCombo1
      ItemHeight = 17
      ScrollBars = ssBoth
      TabOrder = 2
      OnDriveChangeError = JvDirectoryListBox1DriveChangeError
    end
  end
  object Edit1: TEdit
    Left = 252
    Top = 415
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '*.*'
  end
end
