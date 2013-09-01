object FileListBoxMainForm: TFileListBoxMainForm
  Left = 282
  Top = 64
  Width = 604
  Height = 484
  Caption = 'various file components'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object JvLabel6: TJvLabel
    Left = 24
    Top = 16
    Width = 450
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
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object JvLabel8: TJvLabel
    Left = 32
    Top = 328
    Width = 151
    Height = 13
    Caption = 'Here you can see a JvDriveList:'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object Label1: TLabel
    Left = 240
    Top = 352
    Width = 196
    Height = 13
    Caption = 'c:\Programme\Borlan...\JVCLMegaDemo'
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
    Width = 121
    Height = 97
    Style = lbOwnerDrawFixed
    ItemHeight = 37
    Items.Strings = (
      'DRIVE_C (C:)'
      'DRIVE_D (D:)'
      'DELPHI6DG (E:)')
    ScrollWidth = 96
    TabOrder = 0
    DriveTypes = [dtFixed, dtRemote, dtCDROM]
    ImageSize = isSmall
  end
  object JvCaptionPanel1: TJvCaptionPanel
    Left = 24
    Top = 56
    Width = 553
    Height = 257
    Buttons = []
    CaptionFont.Charset = DEFAULT_CHARSET
    CaptionFont.Color = clWhite
    CaptionFont.Height = -13
    CaptionFont.Name = 'MS Shell Dlg 2'
    CaptionFont.Style = [fsBold]
    OutlookLook = False
    TabOrder = 1
    object JvFileListBox1: TJvFileListBox
      Left = 217
      Top = 21
      Width = 329
      Height = 229
      Align = alRight
      FileEdit = Edit1
      ItemHeight = 16
      ShowGlyphs = True
      TabOrder = 0
      ForceFileExtensions = False
    end
    object JvDriveCombo1: TJvDriveCombo
      Left = 20
      Top = 1
      Width = 526
      Height = 22
      Align = alTop
      DriveTypes = [dtFixed, dtRemote, dtCDROM]
      Offset = 4
      ImageSize = isSmall
      ItemHeight = 16
      TabOrder = 1
    end
    object JvDirectoryListBox1: TJvDirectoryListBox
      Left = 20
      Top = 21
      Width = 197
      Height = 229
      Style = lbOwnerDrawFixed
      Align = alClient
      ItemHeight = 17
      Items.Strings = (
        'c:\'
        'c:\Programme'
        'c:\Programme\Borland'
        'c:\Programme\Borland\Delphi7'
        'c:\Programme\Borland\Delphi7\JEDI'
        'c:\Programme\Borland\Delphi7\JEDI\jvcl'
        'c:\Programme\Borland\Delphi7\JEDI\jvcl\examples'
        'c:\Programme\Borland\Delphi7\JEDI\jvcl\examples\JVCLMegaDemo'
        'c:\Programme\Borland\Delphi7\JEDI\jvcl\examples\JVCLMegaDemo\shk')
      ScrollWidth = 387
      TabOrder = 2
      Directory = 'c:\Programme\Borland\Delphi7\JEDI\jvcl\examples\JVCLMegaDemo'
      DirLabel = Label1
      FileList = JvFileListBox1
      DriveCombo = JvDriveCombo1
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
