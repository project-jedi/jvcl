object frmMain: TfrmMain
  Left = 192
  Top = 107
  Width = 543
  Height = 438
  Caption = 'TJvUrlListGrabber demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblExpl: TLabel
    Left = 4
    Top = 184
    Width = 397
    Height = 33
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Please use the memo below to indicate one URL per line. Those ur' +
      'l will then be grabbed by the component when you press Go in the' +
      ' Design Time box'
    WordWrap = True
  end
  object memExplanation: TMemo
    Left = 7
    Top = 4
    Width = 520
    Height = 161
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'This is the demo for TJvUrlListGrabber.'
      
        'This component takes a list of strings through its Urls property' +
        ', each representing a URL to grab. At present it only supports H' +
        'TTP, FTP and Local Files URLs.'
      
        'This component is intended to replace TJvHttpGrabber, TJvFtpGrab' +
        'ber and TJvMultiHttpGrabber. ')
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object grbDesign: TGroupBox
    Left = 415
    Top = 236
    Width = 113
    Height = 129
    Anchors = [akTop, akRight, akBottom]
    Caption = ' Commands '
    TabOrder = 1
    object btnGoDesign: TButton
      Left = 20
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Go'
      TabOrder = 0
      OnClick = btnGoDesignClick
    end
    object btnClear: TButton
      Left = 20
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Clear list'
      TabOrder = 1
      OnClick = btnClearClick
    end
    object btnStop: TButton
      Left = 20
      Top = 88
      Width = 75
      Height = 25
      Caption = 'Stop'
      TabOrder = 2
      OnClick = btnStopClick
    end
  end
  object memUrls: TMemo
    Left = 8
    Top = 224
    Width = 389
    Height = 151
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'http://jvcl.sf.net'
      'ftp://homepages.borland.com/home/jedi/jvcl/Licensing.html'
      'file://C:/log2.txt'
      'C:\log2.txt')
    TabOrder = 2
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 392
    Width = 535
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object julGrabber: TJvUrlListGrabber
    OnDoneFile = julGrabberDoneFile
    OnError = julGrabberError
    OnProgress = julGrabberProgress
    OnConnectedToServer = julGrabberConnectedToServer
    OnSendingRequest = julGrabberSendingRequest
    OnRequestSent = julGrabberRequestSent
    OnConnectionClosed = julGrabberConnectionClosed
    OnStatusChange = julGrabberStatusChange
    Left = 384
    Top = 192
    DefaultGrabbersPropertiesList = <
      item
        UrlType = 'FTP'
        Value.FileName = 'output.txt'
        Value.OutputMode = omFile
        Value.Agent = 'JEDI-VCL'
        Value.Port = 21
      end
      item
        UrlType = 'HTTP'
        Value.FileName = 'output.txt'
        Value.OutputMode = omFile
        Value.Agent = 'JEDI-VCL'
        Value.Port = 80
      end
      item
        UrlType = 'LocalFile'
        Value.FileName = 'output.txt'
        Value.OutputMode = omFile
      end>
  end
end
