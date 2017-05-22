object Form1: TForm1
  Left = 238
  Top = 113
  AutoScroll = False
  BorderWidth = 2
  Caption = 'JvMultiHTTPGrabber demo'
  ClientHeight = 336
  ClientWidth = 401
  Color = clBtnFace
  Constraints.MinHeight = 374
  Constraints.MinWidth = 413
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = popMain
  Position = poDesktopCenter
  Scaled = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 227
    Width = 401
    Height = 5
    Cursor = crSizeNS
    Align = alBottom
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 401
    Height = 61
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 12
      Top = 12
      Width = 115
      Height = 13
      Caption = 'URL of file to download:'
    end
    object btnDownload: TButton
      Left = 229
      Top = 26
      Width = 75
      Height = 25
      Action = acDownload
      Anchors = [akTop, akRight]
      TabOrder = 0
    end
    object cbURL: TComboBox
      Left = 12
      Top = 30
      Width = 210
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      Text = 'http://www.borland.com'
      Items.Strings = (
        'http://jvcl.sf.net'
        'http://jcl.sf.net'
        'http://delphi-jedi.org'
        'http://www.borland.com'
        'http://bdn.borland.com'
        'http://msdn.microsoft.com'
        'http://www.microsoft.com'
        'www.google.com'
        'http://www.torry.net/index.htm'
        'www.delphipages.com')
    end
    object btnDownloadAll: TButton
      Left = 313
      Top = 26
      Width = 75
      Height = 25
      Action = acDownloadAll
      Anchors = [akTop, akRight]
      TabOrder = 2
    end
  end
  object gbContent: TGroupBox
    Left = 0
    Top = 61
    Width = 401
    Height = 166
    Align = alClient
    Caption = ' Content: '
    TabOrder = 1
    object reContent: TRichEdit
      Left = 5
      Top = 17
      Width = 390
      Height = 142
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        
          'This demo shows how you can use the JvMultiHTTPGrabber component' +
          ' to '
        
          'download several files "simultaneously". Click "Download" to dow' +
          'nload the '
        
          'file specified in the combobox'#39's edit field, click "Download All' +
          '" to download '
        'from all URL'#39's in the combobox'#39's list.'
        ''
        
          'Errors and informational messages are reported in the bottom ric' +
          'h edit and '
        
          'the content of the downloaded file(s) are shown in this rich edi' +
          't (unless the '
        
          '"Save To File" option is checked in which case downloads are sav' +
          'ed to file).'
        ''
        
          'When you click "Download" or "Download All", the current URL is ' +
          'added to '
        
          'the history list. To remove an item from the list, select it fro' +
          'm the list, right-'
        
          'click and select "Remove from history" from the menu. The URL'#39's ' +
          'in the '
        
          'history list are remembered until the next time you start the pr' +
          'ogram.')
      ScrollBars = ssBoth
      TabOrder = 0
      WantReturns = False
      WordWrap = False
    end
  end
  object gnLog: TGroupBox
    Left = 0
    Top = 232
    Width = 401
    Height = 85
    Align = alBottom
    Caption = ' Log: '
    TabOrder = 2
    object reLog: TRichEdit
      Left = 5
      Top = 15
      Width = 390
      Height = 64
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssBoth
      TabOrder = 0
      WantReturns = False
      WordWrap = False
    end
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 317
    Width = 401
    Height = 19
    Panels = <
      item
        Bevel = pbNone
        Width = 200
      end
      item
        Width = 50
      end>
    SimplePanel = False
    OnResize = sbMainResize
  end
  object pbProgress: TProgressBar
    Left = 34
    Top = 322
    Width = 108
    Height = 13
    Min = 0
    Max = 100
    TabOrder = 4
  end
  object JvMultiHttpGrabber1: TJvMultiHTTPGrabber
    Agent = 'TJvMultiHttpGrabber Component'
    OnClosingConnection = JvMultiHttpGrabber1ClosingConnection
    OnClosedConnection = JvMultiHttpGrabber1ClosedConnection
    OnConnectingToServer = JvMultiHttpGrabber1ConnectingToServer
    OnConnectedToServer = JvMultiHttpGrabber1ConnectedToServer
    OnDoneFile = JvMultiHttpGrabber1DoneFile
    OnDoneStream = JvMultiHttpGrabber1DoneStream
    OnError = JvMultiHttpGrabber1Error
    OnProgress = JvMultiHttpGrabber1Progress
    OnReceivingResponse = JvMultiHttpGrabber1ReceivingResponse
    OnReceivedResponse = JvMultiHttpGrabber1ReceivedResponse
    OnRedirect = JvMultiHttpGrabber1Redirect
    OnRequestComplete = JvMultiHttpGrabber1RequestComplete
    OnRequestSent = JvMultiHttpGrabber1RequestSent
    OnResolvingName = JvMultiHttpGrabber1ResolvingName
    OnResolvedName = JvMultiHttpGrabber1ResolvedName
    OnSendingRequest = JvMultiHttpGrabber1SendingRequest
    Left = 78
    Top = 138
  end
  object acMainActions: TActionList
    OnUpdate = acMainActionsUpdate
    Left = 204
    Top = 138
    object acDownload: TAction
      Caption = '&Download'
      Hint = 'Download from the selected URL.'
      OnExecute = acDownloadExecute
    end
    object acDownloadAll: TAction
      Caption = 'Download &All'
      Hint = 'Downlaod from all URLs in the list.'
      OnExecute = acDownloadAllExecute
    end
    object acURLAdd: TAction
      Caption = 'Add to history'
      Hint = 'Add URL to history'
      OnExecute = acURLAddExecute
    end
    object acURLDelete: TAction
      Caption = 'Delete from history'
      Hint = 'Delete URL from history'
      OnExecute = acURLDeleteExecute
    end
    object acClearLog: TAction
      Caption = 'Clear log'
      Hint = 'Clear the log'
      OnExecute = acClearLogExecute
    end
    object acClearContent: TAction
      Caption = 'Clear content'
      Hint = 'Clear the content'
      OnExecute = acClearContentExecute
    end
    object acSaveToFile: TAction
      Caption = 'Save To File'
      OnExecute = acSaveToFileExecute
    end
  end
  object popMain: TPopupMenu
    Left = 318
    Top = 138
    object Addtohistory1: TMenuItem
      Action = acURLAdd
    end
    object Deletefromhistory1: TMenuItem
      Action = acURLDelete
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Download1: TMenuItem
      Action = acDownload
    end
    object Downloadall1: TMenuItem
      Action = acDownloadAll
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Clearcontent1: TMenuItem
      Action = acClearContent
    end
    object Clearlog1: TMenuItem
      Action = acClearLog
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object acSaveToFile1: TMenuItem
        Action = acSaveToFile
      end
    end
  end
end
