object frmMain: TfrmMain
  Left = 223
  Top = 103
  Width = 708
  Height = 544
  Caption = 'JvAVICapture demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblExplanations: TLabel
    Left = 24
    Top = 16
    Width = 496
    Height = 65
    Caption = 
      'This is the TJvAviCapture demo.'#13#10'First, press connect, which wil' +
      'l connect to the first driver available.'#13#10'Then use the preview b' +
      'uttons to see live video in the window.'#13#10'You can then capture th' +
      'ree seconds of video, using the values set during design time.'#13#10 +
      'The four remaining buttons are there to allow you to call the dr' +
      'iver'#39's dialog boxes to set up capture details.'
  end
  object btnConnect: TButton
    Left = 16
    Top = 104
    Width = 109
    Height = 25
    Caption = 'Connect'
    TabOrder = 0
    OnClick = btnConnectClick
  end
  object btnCompression: TButton
    Left = 16
    Top = 328
    Width = 109
    Height = 25
    Caption = 'Select compression'
    TabOrder = 1
    OnClick = btnCompressionClick
  end
  object btnStartPreview: TButton
    Left = 16
    Top = 136
    Width = 109
    Height = 25
    Caption = 'Start preview'
    TabOrder = 2
    OnClick = btnStartPreviewClick
  end
  object btnStopPreview: TButton
    Left = 16
    Top = 168
    Width = 109
    Height = 25
    Caption = 'Stop preview'
    TabOrder = 3
    OnClick = btnStopPreviewClick
  end
  object btnCapture: TButton
    Left = 16
    Top = 200
    Width = 109
    Height = 25
    Caption = 'Capture 3s'
    TabOrder = 4
    OnClick = btnCaptureClick
  end
  object btnSource: TButton
    Left = 16
    Top = 232
    Width = 109
    Height = 25
    Caption = 'Select source'
    TabOrder = 5
    OnClick = btnSourceClick
  end
  object btnFormat: TButton
    Left = 16
    Top = 264
    Width = 109
    Height = 25
    Caption = 'Select format'
    TabOrder = 6
    OnClick = btnFormatClick
  end
  object btnDisplay: TButton
    Left = 16
    Top = 296
    Width = 109
    Height = 25
    Caption = 'Select display'
    TabOrder = 7
    OnClick = btnDisplayClick
  end
  object AviCap: TJvAVICapture
    Left = 148
    Top = 96
    Width = 20
    Height = 20
    DriverIndex = -1
    FileName = 'C:\CAPTURE.AVI'
    FileSizeAlloc = 0
    MCIDevice = 'Not connected'
    NoFile = False
    Overlaying = False
    PreviewFrameDelay = 40
    PreviewFPS = 25
    Previewing = False
    ScrollPos.Left = 0
    ScrollPos.Top = 0
    UsedEvents = [ueCapControl, ueError, ueFrame, ueStatus, ueVideoStream, ueWaveStream, ueYield]
    VideoLeft = 0
    VideoTop = 0
    AutoSize = True
  end
end
