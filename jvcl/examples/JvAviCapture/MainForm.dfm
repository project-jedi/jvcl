object frmMain: TfrmMain
  Left = 215
  Top = 106
  Width = 673
  Height = 474
  Caption = 'JvAviCapture demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnConnect: TButton
    Left = 16
    Top = 104
    Width = 109
    Height = 25
    Caption = 'Connect'
    TabOrder = 0
    OnClick = btnConnectClick
  end
  object btnStartPreview: TButton
    Left = 16
    Top = 136
    Width = 109
    Height = 25
    Caption = 'Start preview'
    TabOrder = 1
    OnClick = btnStartPreviewClick
  end
  object AviCap: TJvAVICapture
    Left = 148
    Top = 96
    Width = 20
    Height = 20
    DriverIndex = -1
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
  object btnCompression: TButton
    Left = 16
    Top = 328
    Width = 109
    Height = 25
    Caption = 'Select compression'
    TabOrder = 7
    OnClick = btnCompressionClick
  end
  object btnDisplay: TButton
    Left = 16
    Top = 296
    Width = 109
    Height = 25
    Caption = 'Select display'
    TabOrder = 8
    OnClick = btnDisplayClick
  end
  object memExplanations: TMemo
    Left = 16
    Top = 8
    Width = 629
    Height = 85
    Cursor = crArrow
    TabStop = False
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'This is the TJvAviCapture demo. '
      
        'First, press connect, which will connect to the first driver ava' +
        'ilable. '
      'Then use the preview buttons to see live video in the window. '
      
        'You can then capture three seconds of video, using the values se' +
        't during design time. '
      
        'The four remaining buttons are there to allow you to call the dr' +
        'iver'#39's dialog boxes to set up capture detalis')
    ReadOnly = True
    TabOrder = 9
  end
end
