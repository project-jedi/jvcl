object frmMain: TfrmMain
  Left = 405
  Top = 189
  Width = 593
  Height = 463
  BorderWidth = 2
  Caption = 'Dependency Walker'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object vertSplitter: TSplitter
    Left = 391
    Top = 26
    Width = 5
    Height = 250
    Cursor = crSizeWE
    Align = alRight
    AutoSnap = False
    MinSize = 110
  end
  object horzSplitter: TSplitter
    Left = 0
    Top = 276
    Width = 581
    Height = 5
    Cursor = crSizeNS
    Align = alBottom
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 394
    Width = 581
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object pnlDiagram: TPanel
    Left = 0
    Top = 26
    Width = 391
    Height = 250
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlDiagramTitle: TPanel
      Left = 0
      Top = 0
      Width = 391
      Height = 21
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      Caption = '  Diagram:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
    object sb: TScrollBox
      Left = 0
      Top = 21
      Width = 391
      Height = 229
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BorderStyle = bsNone
      Color = clWindow
      ParentColor = False
      PopupMenu = popDiagram
      TabOrder = 1
      TabStop = True
      OnExit = sbExit
      OnMouseDown = sbMouseDown
      OnMouseMove = sbMouseMove
      OnMouseUp = sbMouseUp
      OnMouseWheel = sbMouseWheel
    end
  end
  object pnlSkipList: TPanel
    Left = 396
    Top = 26
    Width = 185
    Height = 250
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object lbSkipList: TListBox
      Left = 0
      Top = 21
      Width = 185
      Height = 229
      Align = alClient
      BorderStyle = bsNone
      ItemHeight = 13
      Items.Strings = (
        'AccCtrl'
        'AclAPI'
        'ActivDbgLib'
        'ActiveScriptCat'
        'ActiveX'
        'ActnColorMaps'
        'ActnCtrls'
        'ActnList'
        'ActnMan'
        'ActnMenus'
        'ActnPopup'
        'ActnRes'
        'AdaptReq'
        'AddActn'
        'AdoConEd'
        'ADOConst'
        'ADODB'
        'ADOInt'
        'ADOReg'
        'ApacheApp'
        'ApacheHTTP'
        'ApacheTwoApp'
        'ApacheTwoHTTP'
        'AppEvnts'
        'AscrLib'
        'AspTlb'
        'AutoAdap'
        'AutoAdapAS'
        'AutoDisp'
        'AxCtrls'
        'BandActn'
        'BDE'
        'bdeconst'
        'bdemts'
        'BDEReg'
        'BindHelp'
        'BrkrConst'
        'Buttons'
        'Calendar'
        'CDSEdit'
        'CGIApp'
        'CGIHTTP'
        'CGIStubApp'
        'CheckLst'
        'Classes'
        'Clipbrd'
        'ClxDesignWindows'
        'ClxEditors'
        'ClxIconEdit'
        'ClxImgEdit'
        'ClxItemEdit'
        'ClxNodeEdit'
        'ClxPicEdit'
        'ClxSprigs'
        'ClxStrEdit'
        'CmAdmCtl'
        'ColnEdit'
        'ColorGrd'
        'COMAdmin'
        'ComApp'
        'ComConst'
        'ComCtrls'
        'CommCtrl'
        'CommDlg'
        'ComObj'
        'CompProd'
        'ComServ'
        'ComStrs'
        'ComSvcs'
        'Consts'
        'Contnrs'
        'Controls'
        'ConvUtils'
        'CopyPrsr'
        'Corba'
        'CorbaVCL'
        'CosEvent'
        'CosNaming'
        'Cpl'
        'CtlConsts'
        'CtlPanel'
        'CustomizeDlg'
        'DataBkr'
        'DateUtils'
        'DB'
        'DBActns'
        'DBAdapt'
        'DBAdaptImg'
        'DBBdeWeb'
        'dbcgrids'
        'DBClient'
        'DBClientActnRes'
        'DBClientActns'
        'DBCommon'
        'DBConnAdmin'
        'DBConsts'
        'DBCtrls'
        'Dbedit'
        'DbExcept'
        'DBGrids'
        'DbgSiteAS'
        'DBiErrs'
        'dbinpreq'
        'DBIProcs'
        'DbiTypes'
        'DBLocal'
        'DBLocalI'
        'DBLogDlg'
        'dblookup'
        'DBOleCtl'
        'DBOleEdt'
        'DBPWDlg'
        'DBReg'
        'DBT'
        'DBTables'
        'DBWeb'
        'DBXpress'
        'DBXpressWeb'
        'DdeMan'
        'DDEml'
        'DelphiMM'
        'DesignConst'
        'DesignEditors'
        'DesignerDbgSite'
        'DesignIntf'
        'DesignMenus'
        'DesignWindows'
        'Dialogs'
        'DirOutln'
        'Dlgs'
        'DrTable'
        'DSAdd'
        'DSAttrA'
        'DSAttrS'
        'DSDefine'
        'DSDesign'
        'DsgnCntxt'
        'DsgnIntf'
        'DsgnWnds'
        'DSIntf'
        'DsnConst'
        'DsnDbCst'
        'DSProd'
        'DTDSchema'
        'EditIntf'
        'EncdDecd'
        'ExptIntf'
        'ExtActns'
        'ExtCtrls'
        'ExtDlgs'
        'FileCtrl'
        'FileIntf'
        'FiltEdit'
        'FlatSB'
        'FldLinks'
        'FMTBcd'
        'Forms'
        'Gauges'
        'Graphics'
        'GraphUtil'
        'Grids'
        'HelpIntfs'
        'HTTPApp'
        'HTTPD'
        'HTTPD2'
        'HTTPIntr'
        'HTTPParse'
        'HTTPProd'
        'HTTPSOAPToPasBind'
        'HTTPUtil'
        'IB'
        'IBBlob'
        'IBConst'
        'IBCtrls'
        'IBCustomDataSet'
        'IBDatabase'
        'IBDatabaseInfo'
        'IBDCLConst'
        'IBErrorCodes'
        'IBEvents'
        'IBEvnts'
        'IBExternals'
        'IBExtract'
        'IBGeneratorEditor'
        'IBHeader'
        'IBIntf'
        'IBProc32'
        'IBQuery'
        'IBRestoreEditor'
        'IBSecurityEditor'
        'IBServiceEditor'
        'IBSQL'
        'IBSQLMonitor'
        'IBStoredProc'
        'IBTable'
        'IBUpdateSQL'
        'IBUtils'
        'IBXConst'
        'IdAbout'
        'IdAntiFreeze'
        'IdAntiFreezeBase'
        'IdASN1Util'
        'IdAssignedNumbers'
        'IdAuthentication'
        'IdAuthenticationDigest'
        'IdAuthenticationManager'
        'IdAuthenticationNTLM'
        'IdAuthenticationSSPI'
        'IdBaseComponent'
        'IdBlockCipherIntercept'
        'IdChargenServer'
        'IdChargenUDPServer'
        'IdCoder'
        'IdCoder3to4'
        'IdCoderHeader'
        'IdCoderMIME'
        'IdCoderQuotedPrintable'
        'IdCoderUUE'
        'IdCoderXXE'
        'IdComponent'
        'IdCompressionIntercept'
        'IdContainers'
        'IdCookie'
        'IdCookieManager'
        'IdCustomHTTPServer'
        'IdDateTimeStamp'
        'IdDayTime'
        'IdDayTimeServer'
        'IdDayTimeUDP'
        'IdDayTimeUDPServer'
        'IdDICTServer'
        'IdDiscardServer'
        'IdDiscardUDPServer'
        'IdDNSResolver'
        'IdDsnBaseCmpEdt'
        'IdDsnPropEdBinding'
        'IdDsnRegister'
        'IdDummyUnit'
        'IdEcho'
        'IdEchoServer'
        'IdEchoUDP'
        'IdEchoUDPServer'
        'IdEMailAddress'
        'IdException'
        'IdFinger'
        'IdFingerServer'
        'IdFTP'
        'IdFTPCommon'
        'IdFTPList'
        'IdFTPServer'
        'IdGlobal'
        'IdGopher'
        'IdGopherConsts'
        'IdGopherServer'
        'IdHash'
        'IdHashCRC'
        'IdHashElf'
        'IdHashMessageDigest'
        'IdHeaderList'
        'IdHL7'
        'IdHostnameServer'
        'IdHTTP'
        'IdHTTPHeaderInfo'
        'IdHTTPServer'
        'IdHTTPWebBrokerBridge'
        'IdIcmpClient'
        'IdIdent'
        'IdIdentServer'
        'IdIMAP4'
        'IdIMAP4Server'
        'IdIntercept'
        'IdIOHandler'
        'IdIOHandlerSocket'
        'IdIOHandlerStream'
        'IdIOHandlerThrottle'
        'IdIPMCastBase'
        'IdIPMCastClient'
        'IdIPMCastServer'
        'IdIPWatch'
        'IdIRC'
        'IdIrcServer'
        'IdLogBase'
        'IdLogDebug'
        'IdLogEvent'
        'IdLogFile'
        'IdLogStream'
        'IdLPR'
        'IdMailBox'
        'IdMappedFTP'
        'IdMappedPortTCP'
        'IdMappedPortUDP'
        'IdMessage'
        'IdMessageClient'
        'IdMessageCoder'
        'IdMessageCoderMIME'
        'IdMessageCoderUUE'
        'IdMessageCoderXXE'
        'IdMessageCollection'
        'IdMIMETypes'
        'IdMultipartFormData'
        'IdNetworkCalculator'
        'IdNNTP'
        'IdNNTPServer'
        'IdNTLM'
        'IdPOP3'
        'IdPOP3Server'
        'IdQotd'
        'IdQotdServer'
        'IdQOTDUDP'
        'IdQOTDUDPServer'
        'IdRawBase'
        'IdRawClient'
        'IdRawFunctions'
        'IdRawHeaders'
        'IdRegister'
        'IdRemoteCMDClient'
        'IdRemoteCMDServer'
        'IdResourceStrings'
        'IdRexec'
        'IdRexecServer'
        'IdRFCReply'
        'IdRSH'
        'IdRSHServer'
        'IdServerIOHandler'
        'IdServerIOHandlerSocket'
        'IdSimpleServer'
        'IdSMTP'
        'IdSMTPServer'
        'IdSNMP'
        'IdSNPP'
        'IdSNTP'
        'IdSocketHandle'
        'IdSocks'
        'IdSSLOpenSSL'
        'IdSSLOpenSSLHeaders'
        'IdSSPI'
        'IdStack'
        'IdStackConsts'
        'IdStackLinux'
        'IdStackWindows'
        'IdStream'
        'IdStrings'
        'IdSync'
        'IdSysLog'
        'IdSysLogMessage'
        'IdSysLogServer'
        'IdTCPClient'
        'IdTCPConnection'
        'IdTCPServer'
        'IdTCPStream'
        'IdTelnet'
        'IdTelnetServer'
        'IdThread'
        'IdThreadComponent'
        'IdThreadMgr'
        'IdThreadMgrDefault'
        'IdThreadMgrPool'
        'IdThreadSafe'
        'IdTime'
        'IdTimeServer'
        'IdTimeUDP'
        'IdTimeUDPServer'
        'IdTrivialFTP'
        'IdTrivialFTPBase'
        'IdTrivialFTPServer'
        'IdTunnelCommon'
        'IdTunnelMaster'
        'IdTunnelSlave'
        'IdUDPBase'
        'IdUDPClient'
        'IdUDPServer'
        'IdURI'
        'IdUserAccounts'
        'IdVCard'
        'IdWhois'
        'IdWhoIsServer'
        'IdWinSock2'
        'ImageHlp'
        'ImagPrvw'
        'ImgEdit'
        'ImgList'
        'Imm'
        'IndySockTransport'
        'IniFiles'
        'inquire_v1'
        'IntfInfo'
        'InvConst'
        'Invoker'
        'InvokeRegistry'
        'InvRules'
        'Isapi'
        'Isapi2'
        'ISAPIApp'
        'IsapiHTTP'
        'IsapiStubApp'
        'ISAPIThreadPool'
        'IStreams'
        'IWCompButton'
        'IWCompCheckbox'
        'IWCompEdit'
        'IWCompFlash'
        'IWCompLabel'
        'IWCompListbox'
        'IWCompMemo'
        'IWCompMenu'
        'IWCompRectangle'
        'IWCompText'
        'IWCSStdCtrls'
        'IWDBExtCtrls'
        'IWDBGrids'
        'IWDBStdCtrls'
        'IWDsnPaint'
        'IWDsnPaintHandlers'
        'IWDynGrid'
        'IWExtCtrls'
        'IWGrids'
        'IWHTMLControls'
        'IWMain'
        'IWRegion'
        'IWTemplateProcessorHTML'
        'IWTreeview'
        'IWTypes'
        'Ixedit'
        'JclBase'
        'JclDateTime'
        'JclGraphUtils'
        'JclLogic'
        'JclMAPI'
        'JclRegistry'
        'JclRTTI'
        'JclSchedule'
        'JclStrings'
        'JclSysInfo'
        'JclSysUtils'
        'JclUnicode'
        'JclWin32'
        'jpeg'
        'LibConst'
        'LibHelp'
        'LibIntf'
        'ListActns'
        'LZExpand'
        'Mapi'
        'Mask'
        'Masks'
        'MaskUtils'
        'Math'
        'MConnect'
        'Menus'
        'Messages'
        'Midas'
        'MidasCon'
        'MidComp'
        'MidConst'
        'MidItems'
        'MidProd'
        'MidReg'
        'MMSystem'
        'MPlayer'
        'MSHTML'
        'msxml'
        'msxmldom'
        'Mtsobj'
        'MtsRdm'
        'Mtx'
        'MultiMon'
        'Mxarrays'
        'MXBUTTON'
        'MXCOMMON'
        'Mxconsts'
        'Mxconsts'
        'MXDB'
        'MXDCONST'
        'MXDCUBE'
        'MXDIMEDT'
        'MXDSQL'
        'MXDSSQRY'
        'MXGRAPH'
        'MXGRID'
        'MXPBAR'
        'MXPIVSRC'
        'MXQEDCOM'
        'MXQPARSE'
        'MXREG'
        'Mxstore'
        'MXTABLES'
        'Nb30'
        'Ns30Fix'
        'Ns35Fix'
        'Ns36Fix'
        'Nsapi'
        'NSToIS'
        'ObjAuto'
        'ObjBrkr'
        'ObjComAuto'
        'OCXReg'
        'Ole2'
        'OleAuto'
        'OleConst'
        'OleCtl'
        'OleCtnrs'
        'OleCtrls'
        'OleDB'
        'OleDlg'
        'OleServer'
        'OPConvert'
        'OpenGL'
        'OPToSoapDomConv'
        'OPToSoapDomCustom'
        'OrbPas45'
        'Outline'
        'oxmldom'
        'PagItems'
        'PenWin'
        'PicEdit'
        'Printers'
        'PropertyCategories'
        'Provider'
        'PsAPI'
        'QActnList'
        'QButtons'
        'QCheckLst'
        'QClipbrd'
        'QComCtrls'
        'QConsts'
        'QControls'
        'QDBActns'
        'QDBConsts'
        'QDBCtrls'
        'QDBGrids'
        'QDBLogDlg'
        'QDBPWDlg'
        'QDialogs'
        'QExtCtrls'
        'QFileCtrls'
        'QFileDialog'
        'QForms'
        'QGraphics'
        'QGrids'
        'QImgList'
        'QMask'
        'QMenus'
        'QPrinters'
        'QSearch'
        'QStdActns'
        'QStdCtrls'
        'QStyle'
        'Qt'
        'QTypes'
        'recerror'
        'Registry'
        'RegStr'
        'ReqFiles'
        'ReqImpl'
        'ReqMulti'
        'RichEdit'
        'Rio'
        'RTLConsts'
        'ScktCnst'
        'ScktComp'
        'ScktMain'
        'SConnect'
        'ScrptMgr'
        'SessColn'
        'ShadowWnd'
        'ShareMem'
        'SHDocVw'
        'ShellAPI'
        'SHFolder'
        'ShlObj'
        'SimpleDS'
        'SiteComp'
        'SiteConst'
        'SiteProd'
        'SiteWiz'
        'SMINTF'
        'SOAPAttach'
        'SOAPAttachIntf'
        'SoapConn'
        'SoapConst'
        'SoapDm'
        'SOAPDomConv'
        'SoapEnv'
        'SoapHTTPClient'
        'soaphttpdisp'
        'SOAPHTTPPasInv'
        'SoapHTTPTrans'
        'soaplinked'
        'SOAPMidas'
        'soappasinv'
        'SockApp'
        'SockAppNotify'
        'SockAppReg'
        'Sockets'
        'SockHTTP'
        'SockRequestInterpreter'
        'SockTransport'
        'Spin'
        'SqlConst'
        'SqlEdit'
        'SqlExpr'
        'SqlTimSt'
        'StdActnMenus'
        'StdActns'
        'StdConvs'
        'StdCtrls'
        'StdStyleActnCtrls'
        'StdVCL'
        'StFilSys'
        'StrEdit'
        'StringsEdit'
        'StrUtils'
        'StubAction'
        'SvcMgr'
        'SvrConst'
        'SvrHTTPIndy'
        'SvrInfoConsole'
        'SvrInfoConst'
        'SvrInfoModule'
        'SvrLog'
        'SvrLogColSettingsFrame'
        'SvrLogDetailDlg'
        'SvrLogDetailFrame'
        'SvrLogFrame'
        'SvrMainForm'
        'SvrPropDlg'
        'SvrSockRequest'
        'SvrStatsFrame'
        'SyncObjs'
        'SysConst'
        'SysInit'
        'System'
        'SysUtils'
        'TabNotBk'
        'Tabs'
        'TConnect'
        'Themes'
        'TlHelp32'
        'ToolIntf'
        'ToolsAPI'
        'ToolWin'
        'TreeIntf'
        'Types'
        'TypeTrans'
        'TypInfo'
        'UDDIHelper'
        'UrlMon'
        'UxTheme'
        'ValEdit'
        'ValueEdit'
        'VarCmplx'
        'VarConv'
        'Variants'
        'VarUtils'
        'WbmConst'
        'WbmDCnst'
        'VCLCom'
        'VCLEditors'
        'VCLSprigs'
        'VcsIntf'
        'VDBConsts'
        'WebAdapt'
        'WebAppDbgAbout'
        'WebAppDbgHelp'
        'WebAuto'
        'WebBroker'
        'WebBrokerSOAP'
        'WebCat'
        'WebCntxt'
        'WebComp'
        'WebConst'
        'WebContnrs'
        'WebDisp'
        'WebFact'
        'WebForm'
        'WebLib'
        'WebModu'
        'WebNode'
        'WebReq'
        'WebScript'
        'WebScript_TLB'
        'WebScriptAS'
        'WebServExp'
        'WebSess'
        'WebSnapObjs'
        'WebUsers'
        'Windows'
        'winerror'
        'WinHelpViewer'
        'WinInet'
        'WinProcs'
        'WinSock'
        'WinSpool'
        'WinSvc'
        'WinTypes'
        'VirtIntf'
        'WSDLBind'
        'WSDLIntf'
        'WSDLItems'
        'WSDLNode'
        'WSDLPub'
        'WSDLSoap'
        'WSILIntf'
        'xdom'
        'XDRSchema'
        'xercesxmldom'
        'XMLBindGen'
        'XMLBrokr'
        'XMLConst'
        'XMLDataToSchema'
        'XMLDoc'
        'xmldom'
        'XMLIntf'
        'XMLSchema'
        'XMLSchema99'
        'XMLSchemaTags'
        'xmlutil'
        'Xmlxform'
        'XPActnCtrls'
        'XPMan'
        'XPStyleActnCtrls'
        'XSBuiltIns'
        'XSLProd'
        'ZLib'
        'ZLibConst')
      MultiSelect = True
      PopupMenu = popSkipList
      Sorted = True
      TabOrder = 0
    end
    object pnlSkipListTitle: TPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 21
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      Caption = '  Skiplist:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object SpeedButton1: TSpeedButton
        Left = 168
        Top = 2
        Width = 14
        Height = 16
        Action = acViewSkipList
        Anchors = [akTop, akRight]
        Caption = 'r'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Marlett'
        Font.Style = []
        ParentFont = False
      end
    end
  end
  object cbToolbar: TCoolBar
    Left = 0
    Top = 0
    Width = 581
    Height = 26
    AutoSize = True
    Bands = <
      item
        Control = tbStandard
        ImageIndex = -1
        MinHeight = 22
        Width = 577
      end>
    object tbStandard: TToolBar
      Left = 9
      Top = 0
      Width = 564
      Height = 22
      AutoSize = True
      Caption = 'Standard'
      EdgeBorders = []
      Flat = True
      Images = Actions
      TabOrder = 0
      Wrapable = False
      object tbNew: TToolButton
        Left = 0
        Top = 0
        Action = acNew
      end
      object tbSelectFiles: TToolButton
        Left = 23
        Top = 0
        Action = acOpen
      end
      object ToolButton1: TToolButton
        Left = 46
        Top = 0
        Action = acSaveBMP
      end
      object ToolButton2: TToolButton
        Left = 69
        Top = 0
        Width = 8
        Caption = 'ToolButton2'
        ImageIndex = 18
        Style = tbsSeparator
      end
      object tbReport: TToolButton
        Left = 77
        Top = 0
        Action = acReport
      end
      object ToolButton3: TToolButton
        Left = 100
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object tbFind: TToolButton
        Left = 108
        Top = 0
        Action = acFind
      end
      object ToolButton13: TToolButton
        Left = 131
        Top = 0
        Width = 8
        Caption = 'ToolButton13'
        ImageIndex = 18
        Style = tbsSeparator
      end
      object tbUnitStats: TToolButton
        Left = 139
        Top = 0
        Action = acUnitStats
      end
      object tbDelShape: TToolButton
        Left = 162
        Top = 0
        Action = acDelShape
      end
      object ToolButton7: TToolButton
        Left = 185
        Top = 0
        Width = 8
        Caption = 'ToolButton7'
        ImageIndex = 5
        Style = tbsSeparator
      end
      object tbAddSkip: TToolButton
        Left = 193
        Top = 0
        Action = acAdd
      end
      object tbDelSkip: TToolButton
        Left = 216
        Top = 0
        Action = acDelete
      end
      object ToolButton11: TToolButton
        Left = 239
        Top = 0
        Width = 8
        Caption = 'ToolButton11'
        ImageIndex = 6
        Style = tbsSeparator
      end
      object tbAbout: TToolButton
        Left = 247
        Top = 0
        Action = acAbout
      end
    end
  end
  object pnlStats: TPanel
    Left = 0
    Top = 281
    Width = 581
    Height = 113
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 581
      Height = 21
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      Caption = '  Details:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object SpeedButton2: TSpeedButton
        Left = 562
        Top = 2
        Width = 16
        Height = 16
        Action = acViewDetails
        Anchors = [akTop, akRight]
        Caption = 'r'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Marlett'
        Font.Style = []
        ParentFont = False
      end
    end
    object reStatistics: TRichEdit
      Left = 0
      Top = 21
      Width = 581
      Height = 92
      Align = alClient
      BorderStyle = bsNone
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 1
      WantReturns = False
    end
  end
  object mmMain: TMainMenu
    Images = Actions
    Left = 24
    Top = 56
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Action = acNew
      end
      object SelectFiles1: TMenuItem
        Action = acOpen
      end
      object SaveImage1: TMenuItem
        Action = acSaveBMP
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Action = acReport
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = acExit
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Find1: TMenuItem
        Action = acFind
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object CopyDiagramtoClipboard2: TMenuItem
        Action = acCopy
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Skiplist1: TMenuItem
        Caption = 'Skiplist'
        object Add2: TMenuItem
          Action = acAdd
        end
        object Delete2: TMenuItem
          Action = acDelete
        end
      end
      object mnuSort: TMenuItem
        Caption = 'Sort'
        object none1: TMenuItem
          Action = acNoSort
          Checked = True
          GroupIndex = 1
          RadioItem = True
          ShortCut = 16432
        end
        object byName1: TMenuItem
          Action = acSortName
          GroupIndex = 1
          RadioItem = True
        end
        object byLinksTo1: TMenuItem
          Action = acSortLinksTo
          GroupIndex = 1
          RadioItem = True
        end
        object LinksFrom1: TMenuItem
          Action = acSortLinksFrom
          GroupIndex = 1
          RadioItem = True
        end
        object byINterfaceImplementation1: TMenuItem
          Action = acSortIntfImpl
          GroupIndex = 1
        end
        object N3: TMenuItem
          Caption = '-'
          GroupIndex = 1
        end
        object InvertSort1: TMenuItem
          Action = acInvertSort
          GroupIndex = 1
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Options1: TMenuItem
        Action = acOptions
      end
    end
    object Shapes1: TMenuItem
      Caption = 'Shapes'
      object ParseUnit2: TMenuItem
        Action = acParseUnit
        Default = True
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object Addtoskiplist2: TMenuItem
        Action = acAddToSkipList
      end
      object Delete4: TMenuItem
        Action = acDelShape
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object ViewSource1: TMenuItem
        Action = acUnitView
      end
      object Statistics2: TMenuItem
        Action = acUnitStats
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object Toolbar1: TMenuItem
        Action = acViewToolBar
      end
      object Skiplist2: TMenuItem
        Action = acViewSkipList
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object StatusBar2: TMenuItem
        Action = acViewStatusBar
      end
      object Statistics3: TMenuItem
        Action = acViewDetails
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object About1: TMenuItem
        Action = acAbout
      end
    end
  end
  object dlgSelectFiles: TOpenDialog
    DefaultExt = 'pas'
    Filter = 'Pascal files (*.pas)|*.pas|All files|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Select file(s)'
    Left = 24
    Top = 104
  end
  object il32: TImageList
    Height = 32
    Width = 32
    Left = 120
    Top = 56
    Bitmap = {
      494C010102000400040020002000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000800000002000000001001000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C0288049804980498049AD350000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C0288049804980498049AD350000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000210400008049E97EE97E2D7EE05DAD350000
      0000000000000000000000000000000000001042FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7F0000000000000000210400008049E97EE97E2D7EE05DAD350000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000E71C4029AD35E97E0D7F0D7F8C31AD35AD350000
      0000000000000000000000000000000000001042FF7FFF7FFF7F0000FF7FFF7F
      FF7FFF7FFF7F000000000000E71C4029AD35E97E0D7F0D7F8C31AD35AD350000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000E71CC0284A291042517F517FAD35000000000000
      0000000000000000000000000000000000001042FF7FFF7F0000FF7FFF7FFF7F
      FF7FFF7FFF7F000000000000E71CC0284A291042517F517FAD35000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000E71CC618E71C10420D7F517FAD35000000000000
      0000000000000000000000000000000000001042FF7FFF7F00000000FF7FFF7F
      FF7FFF7FFF7F000000000000E71CC618E71C10420D7F517FAD35000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008C318C3100000000
      000000000000000000000000E71CE71CE71CAD350D7F517FAD35000000000000
      0000000000000000000000000000000000001042FF7FFF7F000000000000FF7F
      0000FF7FFF7F000000000000E71CE71CE71CAD350D7F517FAD35000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000E71C2104E71CAD35
      8C3194520000000000000000E71CE71CE71CAD350D7F517FAD35000000000000
      0000000000000000000000000000000000001042FF7FFF7FFF7F000000000000
      0000FF7FFF7F000000000000E71CE71CE71CAD350D7F517FAD35000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000524AE71CE71CAD35
      AD3500002104104200000000E71CC618E71C10420D7F517FAD35000000000000
      0000000000000000000000000000000000001042FF7FFF7FFF7FFF7F00000000
      0000FF7FFF7F000000000000E71CC618E71C10420D7F517FAD35000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      104200000000841000000000E71CE71CE71CAD350D7F517FAD35000000000000
      0000000000000000000000000000000000001042FF7FFF7FFF7F000000000000
      0000FF7FFF7F000000000000E71CE71CE71CAD350D7F517FAD35000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000008C31E71C00000000E71CC618E71CAD350D7F517FAD35000000000000
      0000000000000000000000000000000000001042FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7F000000000000E71CC618E71CAD350D7F517FAD35000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000524A524A00000000
      000000000000000000000000E71CE71CE71CAD350D7F517F1042000000000000
      0000000000000000000000000000000000001042104210421042104210421042
      104210421042000000000000E71CE71CE71CAD350D7F517F1042000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000E71C21048C311042
      000000000000000000000000E71CC618E71C10420D7F517F10420000AD359452
      000000000000000000000000000000000000000000000000E71C21048C311042
      000000000000000000000000E71CC618E71C10420D7F517F10420000AD359452
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000AD35E71CE71CAD35
      AD352104E71C000000000000E71CE71CE71CAD350D7F517F1042000021040000
      8C3100000000000000000000000000000000000000000000AD35E71CE71CAD35
      AD352104E71C000000000000E71CE71CE71CAD350D7F517F1042000021040000
      8C31000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000008C318C31
      AD3500000000C61800000000E71CC618E71CAD350D7F517F10420000E71C2104
      E71C0000C618C61800000000000000000000000000000000000000008C318C31
      AD3500000000C61800000000E71CC618E71CAD350D7F517F10420000E71C2104
      E71C0000C618C618000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001042C618C61800000000E71CE71CE71CAD350D7F517FAD3500000000E71C
      E71C0000210400001042E71C0000000000000000000000000000000000000000
      00001042C618C61800000000E71CE71CE71CAD350D7F517FAD3500000000E71C
      E71C0000210400001042E71C0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008C318C3100000000
      000000000000000000000000E71CC618E71C10420D7F517FAD35000000000000
      0000000000008C311042E71C0000000000000000000000008C318C3100000000
      000000000000000000000000E71CC618E71C10420D7F517FAD35000000000000
      0000000000008C311042E71C0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000E71C841010429452
      000000000000000000000000E71CE71CE71CAD350D7F517FAD3500009452E71C
      000000000000000000000000000000000000000000000000E71C841010429452
      000000000000000000000000E71CE71CE71CAD350D7F517FAD3500009452E71C
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008C318410E71C8C31
      8C319452734E000000000000E71CE71CE71CAD350D7F517FAD35000021040000
      AD35000000000000000000000000000000000000000000008C318410E71C8C31
      8C319452734E000000000000E71CE71CE71CAD350D7F517FAD35000021040000
      AD35000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000AD35524A
      4A29AD352104000000000000E71CC028E71C10420D7F517FAD350000C6180000
      E71C0000E71C8C310000000000000000000000000000000000000000AD35524A
      4A29AD352104000000000000E71CC028E71C10420D7F517FAD350000C6180000
      E71C0000E71C8C31000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000734E210400000000524A4A29C028E71CAD350D7F517FAD35000000001042
      10420000210400001042E71C0000000000000000000000000000000000000000
      0000734E210400000000524A4A29C028E71CAD350D7F517FAD35000000001042
      10420000210400001042E71C0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000328075CC97DAD3510420D7F0D7F80384A2994520000
      000000001042C6181042E71C0000000000000000000000000000000000000000
      000000000000000000000328075CC97DAD3510420D7F0D7F80384A2994520000
      000000001042C6181042E71C0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000E71CE71C524A0000
      0000000000000000AD352104210460288049C97DC97DC97DC97DC048E71C0000
      00000000000000000000E71C000000000000000000000000E71CE71C524A0000
      0000000000000000AD352104210460288049C97DC97DC97DC97DC048E71C0000
      00000000000000000000E71C0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000E71C2104E71CAD35
      10424A290000000000000000734EE71C00008038804920398C31AD3500000000
      734E00000000000000000000000000000000000000000000E71C2104E71CAD35
      10424A290000000000000000734EE71C00008038804920398C31AD3500000000
      734E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000010424A291042
      AD3521040000E71C00000000000000000000524A945200000000000000000000
      E71C0000AD35524A00000000000000000000000000000000000010424A291042
      AD3521040000E71C00000000000000000000524A945200000000000000000000
      E71C0000AD35524A000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      524A21042104C6188C310000E71CAD3500000000000000000000000000000000
      4A29000021042104524AAD350000000000000000000000000000000000000000
      524A21042104C6188C310000E71CAD3500000000000000000000000000000000
      4A29000021042104524AAD350000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000734E10428C312104210400000000E71C524A00000000000000000000
      000000004A2921041042E71C0000000000000000000000000000000000000000
      00000000734E10428C312104210400000000E71C524A00000000000000000000
      000000004A2921041042E71C0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001042E71C21042104210400002104AD35000000000000
      00000000000000000000AD350000000000000000000000000000000000000000
      000000000000000000001042E71C21042104210400002104AD35000000000000
      00000000000000000000AD350000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000001042E71C0000000000009452E71CE71C
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000001042E71C0000000000009452E71CE71C
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AD352104734EC6180000
      E71C0000524A0000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AD352104734EC6180000
      E71C0000524A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000524AC618
      4A290000210421049452524A0000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000524AC618
      4A290000210421049452524A0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000C61800001042E71C0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000C61800001042E71C0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000734E524AE71C0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000734E524AE71C0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000080000000200000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF00FFF003FF001003FF0000000000000000
      FFF003FF001003FF0000000000000000FFF003FF001003FF0000000000000000
      FFF80FFF00180FFF0000000000000000FFF80FFF00180FFF0000000000000000
      E7F80FFF00180FFF0000000000000000E0780FFF00180FFF0000000000000000
      E0180FFF00180FFF0000000000000000FE180FFF00180FFF0000000000000000
      FF980FFF00180FFF0000000000000000E7F80FFF00180FFF0000000000000000
      E1F809FFE1F809FF0000000000000000E03808FFE03808FF0000000000000000
      F818089FF818089F0000000000000000FF180C87FF180C870000000000000000
      E7F80FC7E7F80FC70000000000000000E1F809FFE1F809FF0000000000000000
      E03808FFE03808FF0000000000000000F838089FF838089F0000000000000000
      FF300C87FF300C870000000000000000FFE00387FFE003870000000000000000
      E3E003F7E3E003F70000000000000000E07806FFE07806FF0000000000000000
      F01F3E9FF01F3E9F0000000000000000FE01FE87FE01FE870000000000000000
      FF803F87FF803F870000000000000000FFF00FF7FFF00FF70000000000000000
      FFFE01FFFFFE01FF0000000000000000FFFFC0BFFFFFC0BF0000000000000000
      FFFFF887FFFFF8870000000000000000FFFFFF87FFFFFF870000000000000000
      FFFFFFC7FFFFFFC7000000000000000000000000000000000000000000000000
      000000000000}
  end
  object popSkipList: TPopupMenu
    Images = Actions
    Left = 168
    Top = 56
    object Add1: TMenuItem
      Action = acAdd
    end
    object Delete1: TMenuItem
      Action = acDelete
    end
  end
  object alMain: TActionList
    Images = Actions
    OnUpdate = alMainUpdate
    Left = 72
    Top = 56
    object acNoSort: TAction
      Category = 'Edit'
      Caption = '(no sorting)'
      OnExecute = acNoSortExecute
    end
    object acNew: TAction
      Category = 'File'
      Caption = 'New...'
      Hint = 'Clear diagram'
      ImageIndex = 0
      ShortCut = 16462
      OnExecute = acNewExecute
    end
    object acOpen: TAction
      Category = 'File'
      Caption = 'Open...'
      Hint = 'Select the unit(s) to display'
      ImageIndex = 1
      ShortCut = 16463
      OnExecute = acOpenExecute
    end
    object acSortName: TAction
      Category = 'Edit'
      Caption = 'by Name'
      Hint = 'Sort by name'
      ImageIndex = 23
      ShortCut = 16433
      OnExecute = acArrangeAction
    end
    object acSortIntfImpl: TAction
      Tag = 3
      Category = 'Edit'
      Caption = 'by Image'
      ShortCut = 16436
      OnExecute = acArrangeAction
    end
    object acSortLinksTo: TAction
      Tag = 1
      Category = 'Edit'
      Caption = 'by Links To'
      Hint = 'Sort by links going to unit'
      ImageIndex = 25
      ShortCut = 16434
      OnExecute = acArrangeAction
    end
    object acSortLinksFrom: TAction
      Tag = 2
      Category = 'Edit'
      Caption = 'by Links From'
      Hint = 'Sort by links going from unit'
      ImageIndex = 24
      ShortCut = 16435
      OnExecute = acArrangeAction
    end
    object acInvertSort: TAction
      Category = 'Edit'
      Caption = 'Inverted Sort'
      Hint = 'Invert sort order'
      ImageIndex = 26
      ShortCut = 16457
      OnExecute = acInvertSortExecute
    end
    object acAdd: TAction
      Category = 'Skiplist'
      Caption = 'Add...'
      Hint = 'Add a unit to the skiplist'
      ImageIndex = 3
      ShortCut = 16429
      OnExecute = acAddExecute
    end
    object acDelete: TAction
      Category = 'Skiplist'
      Caption = 'Delete...'
      Hint = 'Delete the selected units from the skiplist'
      ImageIndex = 4
      ShortCut = 16430
      OnExecute = acDeleteExecute
    end
    object acAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      Hint = 'Show About box'
      ImageIndex = 17
      OnExecute = acAboutExecute
    end
    object acUnitStats: TAction
      Category = 'Shapes'
      Caption = 'Details...'
      Hint = 'Display details for selected item'
      ImageIndex = 19
      ShortCut = 16397
      OnExecute = acUnitStatsExecute
    end
    object acDelShape: TAction
      Category = 'Shapes'
      Caption = 'Delete...'
      Hint = 'Delete item from diagram'
      ImageIndex = 9
      ShortCut = 16452
      OnExecute = acDelShapeExecute
    end
    object acReport: TAction
      Category = 'File'
      Caption = 'Create Report...'
      Hint = 'Create a report in text, html or xml format'
      ImageIndex = 10
      ShortCut = 16464
      OnExecute = acReportExecute
    end
    object acExit: TAction
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Close the program'
      ShortCut = 32883
      OnExecute = acExitExecute
    end
    object acFind: TAction
      Category = 'Edit'
      Caption = 'Find...'
      Hint = 'Search for an item in the diagram'
      ImageIndex = 21
      ShortCut = 16454
      OnExecute = acFindExecute
    end
    object acAddToSkipList: TAction
      Category = 'Shapes'
      Caption = 'Add to skiplist'
      Hint = 'Add to skiplist'
      ImageIndex = 3
      ShortCut = 16449
      OnExecute = acAddToSkipListExecute
    end
    object acViewStatusBar: TAction
      Category = 'View'
      Caption = 'Statusbar'
      Checked = True
      ShortCut = 16469
      OnExecute = acViewStatusBarExecute
    end
    object acViewSkipList: TAction
      Category = 'View'
      Caption = 'Skiplist'
      Checked = True
      ShortCut = 16460
      OnExecute = acViewSkipListExecute
    end
    object acViewToolBar: TAction
      Category = 'View'
      Caption = 'Toolbar'
      Checked = True
      ShortCut = 16450
      OnExecute = acViewToolBarExecute
    end
    object acRefresh: TAction
      Category = 'View'
      Caption = 'Refresh'
      ImageIndex = 16
      ShortCut = 116
      OnExecute = acRefreshExecute
    end
    object acSaveBMP: TAction
      Category = 'File'
      Caption = 'Save...'
      Hint = 'Save diagram image'
      ImageIndex = 2
      ShortCut = 16467
      OnExecute = acSaveBMPExecute
    end
    object acCopy: TAction
      Category = 'Edit'
      Caption = 'Copy'
      Hint = 'Copy diagram to clipboard'
      ImageIndex = 7
      ShortCut = 16451
      OnExecute = acCopyExecute
    end
    object acSaveDiagram: TAction
      Caption = 'Save diagram...'
      ImageIndex = 0
      OnExecute = acSaveDiagramExecute
    end
    object acOpenDiagram: TAction
      Caption = 'Open Diagram...'
      ImageIndex = 1
      OnExecute = acOpenDiagramExecute
    end
    object acParseUnit: TAction
      Category = 'Shapes'
      Caption = 'Parse Unit'
      ImageIndex = 1
      ShortCut = 13
      OnExecute = acParseUnitExecute
    end
    object acOptions: TAction
      Category = 'Edit'
      Caption = 'Options...'
      ImageIndex = 18
      ShortCut = 32781
      OnExecute = acOptionsExecute
    end
    object acUnitView: TAction
      Category = 'Shapes'
      Caption = 'View Source'
      ImageIndex = 22
      OnExecute = acUnitViewExecute
    end
    object acViewDetails: TAction
      Category = 'View'
      Caption = 'Details'
      Checked = True
      ShortCut = 16468
      OnExecute = acViewDetailsExecute
    end
  end
  object popShape: TPopupMenu
    Images = Actions
    Left = 168
    Top = 104
    object Parseunit1: TMenuItem
      Action = acParseUnit
      Default = True
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object Addtoskiplist1: TMenuItem
      Action = acAddToSkipList
    end
    object Delete3: TMenuItem
      Action = acDelShape
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object ViewSource2: TMenuItem
      Action = acUnitView
    end
    object Statistics1: TMenuItem
      Action = acUnitStats
    end
  end
  object Actions: TImageList
    Left = 72
    Top = 104
    Bitmap = {
      494C01011C001D00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000008000000001001000000000000040
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010421042104200000000104210421042000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000104210421042
      1042104210420000E07F00001042000000000000000000000000000000000000
      0000000000000000000010421042104200000000104210421042000000000000
      0000000000000000000000000000000000000000000000000000000000001000
      1000100010000000000000000000000000000000000000000000000000000000
      000000000000FF7FFF7F00000000104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000100010001000
      100010001000100010000000000000000000000000000000FF7FFF7FFF7FE07F
      FF7FE07FFF7FE07FFF7FFF7FFF7F000010420000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001000100010000000
      000000000000100010001000000000000000000000000000FF7FE07FFF7FFF7F
      FF7F10001000FF7FFF7FE07FFF7F000010420000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001000100000000000
      000000000000000010001000000000000000000000000000FF7FFF7FFF7FE07F
      FF7FFF7FFF7FE07FFF7FFF7FFF7F000010420000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000010001000000000000000
      000000000000000000001000100000000000000000000000FF7FE07FFF7FFF7F
      FF7F10001042FF7FFF7FE07FFF7F000010420000FF7FFF7FFF7FFF7F00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF7FFF7FFF7FFF7F00000000000010001000000000000000
      000000000000000000001000100000000000000000000000FF7FFF7FFF7FE07F
      FF7F104210000000FF7FFF7FFF7F000010420000FF7FFF7FFF7FFF7F00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF7FFF7FFF7FFF7F00000000000010001000000000000000
      000000000000000000001000100000000000000000000000FF7FE07FFF7FFF7F
      FF7FE07F104210001042E07FFF7F000010420000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000010001000000000000000
      000000000000000000001000100000000000000000000000FF7FFF7FFF7F1000
      1042FF7FFF7F10001000FF7FFF7F000010420000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001000100000000000
      000000001000000010001000000000000000000000000000FF7FE07FFF7F1000
      1000E07F000010001000E07FFF7F000010420000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001000100000000000
      000000001000100010001000000000000000000000000000FF7FFF7FFF7F0000
      10001000100010000000FF7FFF7F000010420000000000000000000000000000
      0000000000000000000010421042104200000000104210421042000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000001000100010000000000000000000000000000000FF7FE07FFF7FFF7F
      FF7FE07FFF7FFF7FFF7FE07FFF7F000010420000000000000000000000000000
      0000000000000000000010421042104200000000104210421042000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000001000100010001000000000000000000000000000FF7FFF7FFF7FE07F
      FF7FFF7FFF7FE07FFF7FFF7FFF7F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010001000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000100000001000100000000000000000000000004200420042
      0042004200420042004200000000000000000000000010001000100010000000
      0000000000000000000000400000000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000000000000000000000000
      000000000000100000001000100010000000000000000000FF7F000000420042
      0042004200420042004200420000000000000000100010000000000010001000
      0000000000000000004000400040000000000000FF7F00000000FF7F00000000
      000000000000FF7F000000000000000000000000000000000000000000000000
      000000001000000010001000100000000000000000000000E07FFF7F00000042
      0042004200420042004200420042000000000000100010000000000000000000
      0000000000000040004000400040004000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000000000000000000000000
      000010000000100010001000000000000000000000000000FF7FE07FFF7F0000
      0000000000000000000000000000000000000000100010001000100010001000
      0000000000000000000000400000000000000000FF7F00000000FF7F00000000
      000000000000FF7F000000000000000000000000000000000000000000000000
      FF7F10421000100010000000000000000000000000000000E07FFF7FE07FFF7F
      E07FFF7FE07FFF7F000000000000000000000000100010000000000010001000
      0000000000000000000000400000000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000010420000000000000000
      0000FF7F1042000000000000000000000000000000000000FF7FE07FFF7FE07F
      FF7FE07FFF7FE07F000000000000000000000000000010001000100010000000
      0000000000000000000000400000000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7F0000FF7FFF7F000000000000000000000000000000000000000000000000
      00000000FF7F000000000000000000000000000010420000E07FFF7FE07F0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000400000000000000000FF7F00000000FF7FFF7FFF7F
      000000000000FF7F000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000010421F001F000000000000000002
      00021F0000001042000000000000000000000000000000000000000000000000
      0000000000000000000000400000000000000000FF7F000000000000FF7F0000
      0000000000000000000000000000100010000000000000000000000000000000
      00000000000000000000000000000000000010421F001F001F00000200020002
      000210021F000000000000000000000000000000000010001000100010001000
      0000000000000000000000400000000000000000FF7FFF7F0000000000000000
      00000000000000000000000000001000100000000000FF7FFF03000000000000
      00000000000000000000000000000000000010421F0000020002000200020002
      1F0000021F000000000000000000000000000000100010000000000010001000
      0000000000000000000000400000000000000000000000000000000000000000
      00000000000000000000000000001000100000000000FF7FFF03000000000000
      00000000000000000000000000000000000010421F000000000200021F000002
      1F00000200020000000000000000000000000000100010000000000010001000
      0000000000000000000000400000000000000000000000000000000000000000
      00000000000000000000000000001000100000000000FF7FFF7FFF03FF030000
      000000000000000000000000000000000000104210421F00FF7F100200021F00
      1F00000200021042000000000000000000000000000010001000100010001000
      0000000000000000000000400000000000000000000000000000000000000000
      000000000000000000000000000010001000000000001042FF7FFF7FFF7F0000
      00000000104200000000000000000000000000001042FF7F0000100200000002
      0002000200000000000000000000000000000000000000000000000010001000
      0000000000000000000000400000000000000000000000000000000000000000
      0000000000000000000000000000100010000000000000000000000000000000
      0000000000000000000000000000000000000000000010421F00000010420002
      0002000000000000000000000000000000000000000010001000100010000000
      0000000000000000000000400000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001042104210421042
      1042000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F00000000000000000000E07FFF7FFF7FFF7F
      E07FFF7FFF7FFF7FE07FFF7FFF7F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF7FFF7FFF7FFF7F
      FF7F0002FF7FFF7FFF7FFF7FFF7F00000000000000000000FF7FFF7FE07FFF7F
      FF7F1863E07FFF7FFF7FFF7FE07F000000000000000000000000000010021F00
      10021F0010020000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F00000000000000000000000000000000FF7FFF7FFF7FFF7F
      00020002FF7FFF7FFF7FFF7FFF7F00000000000000000000E07FFF7FFF7FFF7F
      18631F001863FF7FE07FFF7FFF7F000000000000000000000002E00300001002
      1F0010021F0010021F0000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7F00000000000000000000000000000000000000000000FF7FFF7FFF7F0002
      0002000200020002FF7FFF7FFF7F00000000000000000000FF7FFF7FE07FFF7F
      FF7FFF7FE07FFF7FFF7FFF7FE07F00000000000000000002E0030002E0030000
      FF031F00FF030000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      000010420000000010420000000000000000000000000000FF7FFF7FFF7FFF7F
      00020002FF7FFF7F0002FF7FFF7F00000000000000000000E07FFF7FFF7FFF7F
      E07F1F00FF7FFF7FE07FFF7FFF7F0000000000000000E0030002E00300020000
      1F00FF0300000002000200020000000000000000FF7FFF7FFF7FFF7FFF7F0000
      104200000000FF0310421042000000000000000000000000FF7FFF7FFF7FFF7F
      FF7F0002FF7FFF7F0002FF7FFF7F00000000000000000000FF7FFF7FE07FFF7F
      FF7F1F001F00FF7FFF7FFF7FE07F000000000000E0030002E003000000001F00
      FF030000E0030002E00300020002000000000000FF7FFF7FFF7FFF7FFF7F0000
      000000000000000010420000000000000000000000000000FF7FFF7F0002FF7F
      FF7FFF7FFF7FFF7F0002FF7FFF7F00000000000000000000E07FFF7FFF7FFF7F
      E07FFF7F1F001F00E07FFF7FFF7F0000000000000002E0030000FF031F00FF03
      1F0000000002E0030002E0030002000000000000FF7FFF7FFF7FFF7FFF7F0000
      0000FF030000000010420000000000000000000000000000FF7FFF7F0002FF7F
      FF7F0002FF7FFF7FFF7FFF7FFF7F00000000000000000000FF7FFF7FE07FFF7F
      FF7FFF7F18631F00FF7FFF7FE07F00000000000000000000FF031F00FF031F00
      FF030000E0030002E00300020002000000000000FF7FFF7FFF7FFF7FFF7F0000
      1042FF03FF03000010421042000000000000000000000000FF7FFF7F0002FF7F
      FF7F00020002FF7FFF7FFF7FFF7F00000000000000000000E07FFF7FFF7F1F00
      E07FFF7F18631F00E07FFF7FFF7F000000000000FF03FF031F00FF031F00FF03
      1F00FF030000E0030002E0030002000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      000010420000000010420000000000000000000000000000FF7FFF7FFF7F0002
      0002000200020002FF7FFF7FFF7F00000000000000000000FF7FFF7FE07F1F00
      1F0018631F001F00FF7FFF7FE07F000000000000FF031F0000001F00FF031F00
      FF031F00FF030000E00300020002000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7F00000000000000000000000000000000000000000000FF7FFF7FFF7FFF7F
      FF7F00020002FF7FFF7FFF7FFF7F00000000000000000000E07FFF7FFF7FFF7F
      1F001F001F00FF7FE07FFF7FFF7F00000000000000000000E00300001F00FF03
      1F00FF031F00FF03000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F00000000000000000000000000000000FF7FFF7FFF7FFF7F
      FF7F0002FF7FFF7F00000000000000000000000000000000FF7FFF7FE07FFF7F
      FF7FFF7FE07FFF7F0000000000000000000000000000E0030002E00300000000
      FF031F00FF031F00FF031F000000000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7F00000000000000000000000000000000000000000000FF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F0000FF7F000000000000000000000000E07FFF7FFF7FFF7F
      E07FFF7FFF7FFF7F0000FF7F000000000000000000000000E0030002E0030000
      000000001F00FF031F0000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7F00000000000000000000000000000000000000000000FF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F00000000000000000000000000000000FF7FFF7FE07FFF7F
      FF7FFF7FE07FFF7F00000000000000000000000000000000000000000002E003
      0002E00300000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000007C007C
      007C007C007C007C000000000000000000000000000018630042000000000000
      000000000000FF7F186300000042000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000007C007C007C007C
      007C007C007C007C007C007C0000000000000000000000421863000000000000
      0000000000001863FF7F00001863000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000007C007C007C007C007C
      007C007C007C007C007C007C007C000000000000000018630042000000000000
      000000000000FF7F186300000042000000000000000000000000000000000000
      0000100000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000007C007C007C007C007C
      007C007C007C007C007C007C007C000000000000000000421863000000000000
      0000000000000000000000001863000000000000000000000000000000000000
      1000100010000000000000000000000000000000000000000000000000001000
      1000100000000000000000000000000000000000007C007C007C007C007C007C
      007C007C007C007C007C007C007C007C00000000000018630042186300421863
      0042186300421863004218630042000000000000000000000000000000001000
      1000100010001000000000000000000000000000000000000000000000001000
      1000100000000000000000000000000000000000007C007C007C007CFF7FFF7F
      007C007CFF7FFF7F007C007C007C007C00000000000000421863000000000000
      0000000000000000000000421863000000000000000000000000000010001000
      1000100010001000100000000000000000000000000000001000100010001000
      1000100010001000100000000000000000000000007C007C007C007C007CFF7F
      FF7FFF7FFF7F007C007C007C007C007C00000000000018630000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7F00000042000000000000000000000000100010001000
      1000100010001000100010000000000000000000000000000000100010001000
      1000100010001000000000000000000000000000007C007C007C007C007C007C
      FF7FFF7F007C007C007C007C007C007C00000000000000420000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7F00001863000000000000000000000000000000000000
      1000100010000000000000000000000000000000000000000000000010001000
      1000100010000000000000000000000000000000007C007C007C007C007CFF7F
      FF7FFF7FFF7F007C007C007C007C007C00000000000018630000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7F00000042000000000000000000000000000000000000
      1000100010000000000000000000000000000000000000000000000000001000
      1000100000000000000000000000000000000000007C007C007C007CFF7FFF7F
      007C007CFF7FFF7F007C007C007C007C00000000000000420000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7F00001863000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      10000000000000000000000000000000000000000000007C007C007C007C007C
      007C007C007C007C007C007C007C000000000000000018630000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000007C007C007C007C007C
      007C007C007C007C007C007C007C000000000000000000420000000200020002
      0002000200020002000200001863000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000007C007C007C007C
      007C007C007C007C007C007C0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000007C007C
      007C007C007C007C000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      1000100010001000100010001000100010000000000000000000000000000000
      0000000000000000000000000000000000000000000018631863186318631863
      18631863186318630000186300000000000000000000007C0000000000000000
      000000000000000000000000007C000000000000000000000000000000000000
      1000FF7FFF7FFF7FFF7FFF7FFF7FFF7F10000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000001863000000000000007C007C007C000000000000
      0000000000000000000000000000000000000000000000421042004210420042
      1000FF7F10001000100010001000FF7F10000000000000000000000000000000
      0000000000000000000000000000000000000000186318631863186318631863
      E003E003E0031863186300000000000000000000007C007C007C007C00000000
      00000000000000000000007C0000000000000000000010420042104200421042
      1000FF7FFF7FFF7FFF7FFF7FFF7FFF7F10000000000000000000000000000000
      0000000000000000000000000000000000000000186318631863186318631863
      00020002000218631863000018630000000000000000007C007C007C00000000
      0000000000000000007C00000000000000000000000000421042004210420042
      1000FF7F100010001000FF7F1000100010000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000186318630000000000000000007C007C007C0000
      000000000000007C007C00000000000000000000000010420042104200421042
      1000FF7FFF7FFF7FFF7FFF7F1000FF7F10000000000000000000000000000000
      0000000000000000000000000000000000000000186318631863186318631863
      1863186318631863000018630000186300000000000000000000007C007C007C
      00000000007C007C000000000000000000000000000000421042004210420042
      1000FF7FFF7FFF7FFF7FFF7F1000100000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000001863000018630000000000000000000000000000007C007C
      007C007C007C0000000000000000000000000000000010420042104200421042
      1000100010001000100010001000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F00001863000018630000000000000000000000000000007C
      007C007C00000000000000000000000000000000000000421042004210420042
      1042004210420042104200421042000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF7F1F001F00
      1F001F001F00FF7F0000000000000000000000000000000000000000007C007C
      007C007C007C0000000000000000000000000000000010420042000000000000
      0000000000000000000010420042000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7F00000000000000000000000000000000007C007C007C
      00000000007C007C000000000000000000000000000000421042000000000000
      0000000000000000000000421042000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF7F1F00
      1F001F001F001F00FF7F000000000000000000000000007C007C007C007C0000
      000000000000007C007C0000000000000000000000001042004210420000E07F
      00000000E07F0000004210420042000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7F0000000000000000007C007C007C007C00000000
      0000000000000000007C007C0000000000000000000000000000000000000000
      E07FE07F00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000007C007C007C000000000000
      000000000000000000000000007C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000420042000000000000
      0000FF7F00000042000000000000000000000000000000000000000010001000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000000420042000000000000
      0000FF7F00000042000000000000000000000000000000000000100000000000
      1000000000001000100000000000000000000000000000000000000000000000
      10001000100010001000100010001000100000000000FF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000000000000000000000420042000000000000
      0000000000000042000000420000000000000000000000000000100000000000
      1000000010000000000010000000000000000000000000000000000000000000
      1000FF7FFF7FFF7FFF7FFF7FFF7FFF7F100000000000FF7FFF7FFF7FFF7FFF7F
      FF7F0000E07FE07FE07FE07FE07F000000000000000000420042004200420042
      0042004200420042000000420000000000000000000000000000100000000000
      1000000010000000000010000000000000000000000000000000000000000000
      1000FF7F00000000000000000000FF7F100000000000FF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000000000000000000000420042000000000000
      0000000000420042000000420000000000000000000000000000000010001000
      1000000010000000000010000000000000000000000000000000000000000000
      1000FF7FFF7FFF7FFF7FFF7FFF7FFF7F100000000000FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000000420000FF7FFF7FFF7F
      FF7FFF7F00000042000000420000004200000000000000000000000000000000
      10000000100010001000000000000000000000000000FF7FFF7FFF7FFF7FFF7F
      1000FF7F00000000000000000000FF7F100000000000FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000000420000FF7FFF7FFF7F
      FF7FFF7F00000042000000420000004200000000000000000000000000000000
      10000000100000000000000000000000000000000000FF7F0000000000000000
      1000FF7FFF7FFF7FFF7FFF7FFF7FFF7F100000000000FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000000420000FF7FFF7FFF7F
      FF7FFF7F00000000000000420000004200000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7F
      1000FF7F00000000FF7F100010001000100000000000FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000000420000FF7FFF7FFF7F
      FF7FFF7F0000FF7F000000420000004200000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7F0000000000000000
      1000FF7FFF7FFF7FFF7F1000FF7F1000000000000000FF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000004200000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7F
      1000FF7FFF7FFF7FFF7F100010000000000000000000FF7FFF7FFF7FFF7FFF7F
      FF7F0000FF7F000000000000000000000000000000000000000000420000FF7F
      FF7FFF7FFF7FFF7F0000FF7F0000004200000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7F00000000FF7F0000
      10001000100010001000100000000000000000000000FF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7FFF7FFF7FFF7F0000
      FF7F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000042
      0000FF7FFF7FFF7FFF7FFF7F0000FF7F00000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7FFF7FFF7FFF7F0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7F0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000420042000000000000
      00000000000000000000000000420000000000000000FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7F0000E07F00000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7F0000000000000000000000000042004200420042
      0042004200420042004200000000000000000000000000420042000000000000
      00000000000000000000000000420000000000000000FF7FFF7FFF7FFF7FFF7F
      FF7F000000000000E07F00000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7F00000000000000000000FF7F0000004200420042
      0042004200420042004200420000000000000000000000420042000000000000
      00000000000000000000000000420000000000000000FF7FFF7FFF7FFF7FFF7F
      FF7F0000E07FE07FE07FE07FE07F000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7F00000000000000000000E07FFF7F000000420042
      0042004200420042004200420042000000000000000000420042000000000000
      00000000000000000000000000420000000000000000FF7FFF7FFF7FFF7FFF7F
      FF7F000000000000E07F00000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7F00000000000000000000FF7FE07FFF7F00000042
      0042004200420042004200420042004200000000000000420042004200420042
      00420042004200420042004200420000000000000000FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7F0000E07F00000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7F00000000000000000000E07FFF7FE07FFF7F0000
      0000000000000000000000000000000000000000000000420042000000000000
      00000000000000000000004200420000000000000000FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7F0000000000000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7F00000000000000000000FF7FE07FFF7FE07FFF7F
      E07FFF7FE07FFF7F000000000000000000000000000000420000000000000000
      00000000000000000000000000420000000000000000FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7F00000000000000000000E07FFF7FE07FFF7FE07F
      FF7FE07FFF7FE07F000000000000000000000000000000420000000000000000
      00000000000000000000000000420000000000000000FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7F00000000000000000000000000000000FF7FE07FFF7F00000000
      0000000000000000000000000000000000000000000000420000000000000000
      00000000000000000000000000420000000000000000FF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7F0000FF7F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000420000000000000000
      00000000000000000000000000420000000000000000FF7FFF7FFF7FFF7FFF7F
      FF7F0000FF7F0000000000000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000420000000000000000
      00000000000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000420000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000800000000100010000000000000400000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFF7FFE007FFFFFFFFE7
      FFE007FFFFFFF003FE2005FFFC3FE001FF2006BFF00FC000FEBFFF3FE3C7C000
      FDFFFE3FE7E7C00003FFFFC0CFF3C00003FFFFC0CFF3C02003FFFFC0CFF3C000
      03FFFFC0CFF3C000FDFFFE3FE7A7C040FEA0073FE787C210FF2006BFFF8FC000
      FE2005FFFF87C001FFE007FFFFFFE003FFFFFFF3C00FFFFF000FFFE9C007C3F7
      000FFFD1C00399E3000FFFA3C0019FC1000FF047C00081F7000FCE0FC00F99F7
      000F9F1FC008C3F7000FBF9F800CFFF7008F7FEF001AFFF711447FEF0007C1F7
      0AB84FEF001F99F7057C4FEF201F99F7FAFC83DF001FC1F7FDF8839F943FF9F7
      FE04CF3FC87FC3F7FFFFF0FFE0FFFFFFC001C001FFFFFFFFC001C001F83F000C
      C001C001E00F0008C001C001C0070001C001C00180030063C001C001800300C3
      C001C001000101EBC001C0010001016BC001C00100010023C001C00100010067
      C001C0010001000FC001C0018003000FC001C0018003000FC003C003C007005F
      C007C007E00F003FC00FC00FF83F007FFFFFFFFFFFFFF81FC001FFFFFFFFE007
      8001FFFFFFFFC0038001FF7FFFFF80018001FE3FF83F80018001FC1FF83F0000
      8001F80F800300008001F007C00700008001E003E00F00008001C001F01F0000
      8001FC1FF83F00008001FC1FFC7F80018001FFFFFEFF80018001FFFFFFFFC003
      8001FFFFFFFFE007FFFFFFFFFFFFF81FFFFFFFFFFFFFFFFFFFFFFFFFC007FFFF
      FE00FFFD8003DFFBC000F7FF00018FFF8000E3FB000187F78000E3F70001C7EF
      8000F1E70000E3CF8000F8CF0000F19F8001FC1F8000F83F8001FE3FC000FC7F
      8001FC1FE001F83F8001F8DFE007F19F87E1E1E7F007C3CF8001C3F3F00387E7
      C003CFF9F8038FFBFC3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC00FFFFFFFFF
      800F800FF9FFFFFF800F8003F6CFFE0080018003F6B7FE0080018003F6B7FE00
      80018000F8B78000800F8000FE8F8000800F8000FE3F8000800F8000FF7F8000
      800F8000FE3F8001800F8000FEBF8003801FE000FC9F8007803FE000FDDF807F
      807FF800FDDF80FFFFFFF800FDDF81FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      E003FFFFC0018007E003800F80318007E003800780318001E003800380318001
      E003800180018001E003800080018007E003800080018007E003800F8FF1800F
      E003800F8FF1800FE003800F8FF1800FE007C7F88FF1801FE00FFFFC8FF1803F
      E01FFFBA8FF5807FFFFFFFC78001FFFF00000000000000000000000000000000
      000000000000}
  end
  object popDiagram: TPopupMenu
    Images = Actions
    Left = 232
    Top = 104
    object CopyDiagramtoClipboard1: TMenuItem
      Action = acCopy
    end
  end
  object dlgSaveImage: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 'Bitmap files (*.bmp)|*.bmp|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save Diagram Image'
    Left = 32
    Top = 152
  end
end
