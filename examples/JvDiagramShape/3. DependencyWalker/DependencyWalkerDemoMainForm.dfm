object DependencyWalkerDemoMainFrm: TDependencyWalkerDemoMainFrm
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
        'VCLCom'
        'VCLEditors'
        'VCLSprigs'
        'VcsIntf'
        'VDBConsts'
        'VirtIntf'
        'WbmConst'
        'WbmDCnst'
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
      0000000000003600000028000000800000002000000001002000000000000040
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000031520000639400006394000063
      9400006394006B6B6B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000031520000639400006394000063
      9400006394006B6B6B0000000000000000000000000000000000000000000000
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
      000000000000000000000808080000000000006394004ABDFF004ABDFF006B8C
      FF00007BBD006B6B6B0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      000000000000000000000808080000000000006394004ABDFF004ABDFF006B8C
      FF00007BBD006B6B6B0000000000000000000000000000000000000000000000
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
      00000000000039393900005252006B6B6B004ABDFF006BC6FF006BC6FF006363
      63006B6B6B006B6B6B0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      00000000000039393900005252006B6B6B004ABDFF006BC6FF006BC6FF006363
      63006B6B6B006B6B6B0000000000000000000000000000000000000000000000
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
      000000000000393939000031520052525200848484008CD6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      000000000000393939000031520052525200848484008CD6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
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
      000000000000393939003131310039393900848484006BC6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      000000000000393939003131310039393900848484006BC6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006363
      6300636363000000000000000000000000000000000000000000000000000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF000000
      00000000000000000000FFFFFF0000000000FFFFFF00FFFFFF00000000000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003939
      390008080800393939006B6B6B0063636300A5A5A50000000000000000000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00000000000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009494
      940039393900393939006B6B6B006B6B6B000000000008080800848484000000
      000000000000393939003131310039393900848484006BC6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00000000000000
      000000000000393939003131310039393900848484006BC6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000000000000212121000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00000000000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000063636300393939000000
      0000000000003939390031313100393939006B6B6B006BC6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000003939390031313100393939006B6B6B006BC6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009494
      9400949494000000000000000000000000000000000000000000000000000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400000000000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003939
      3900080808006363630084848400000000000000000000000000000000000000
      000000000000393939003131310039393900848484006BC6FF008CD6FF008484
      8400000000006B6B6B00A5A5A500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003939
      3900080808006363630084848400000000000000000000000000000000000000
      000000000000393939003131310039393900848484006BC6FF008CD6FF008484
      8400000000006B6B6B00A5A5A500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006B6B
      6B0039393900393939006B6B6B006B6B6B000808080039393900000000000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF008484
      8400000000000808080000000000636363000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006B6B
      6B0039393900393939006B6B6B006B6B6B000808080039393900000000000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF008484
      8400000000000808080000000000636363000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000063636300636363006B6B6B000000000000000000313131000000
      0000000000003939390031313100393939006B6B6B006BC6FF008CD6FF008484
      8400000000003939390008080800393939000000000031313100313131000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000063636300636363006B6B6B000000000000000000313131000000
      0000000000003939390031313100393939006B6B6B006BC6FF008CD6FF008484
      8400000000003939390008080800393939000000000031313100313131000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840031313100313131000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF006B6B
      6B00000000000000000039393900393939000000000008080800000000008484
      8400393939000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840031313100313131000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF006B6B
      6B00000000000000000039393900393939000000000008080800000000008484
      8400393939000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006363
      6300636363000000000000000000000000000000000000000000000000000000
      000000000000393939003131310039393900848484006BC6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000636363008484
      8400393939000000000000000000000000000000000000000000000000006363
      6300636363000000000000000000000000000000000000000000000000000000
      000000000000393939003131310039393900848484006BC6FF008CD6FF006B6B
      6B00000000000000000000000000000000000000000000000000636363008484
      8400393939000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003939
      39002121210084848400A5A5A500000000000000000000000000000000000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF006B6B
      6B0000000000A5A5A50039393900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003939
      39002121210084848400A5A5A500000000000000000000000000000000000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF006B6B
      6B0000000000A5A5A50039393900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006363
      630021212100393939006363630063636300A5A5A5009C9C9C00000000000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF006B6B
      6B000000000008080800000000006B6B6B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006363
      630021212100393939006363630063636300A5A5A5009C9C9C00000000000000
      0000000000003939390039393900393939006B6B6B006BC6FF008CD6FF006B6B
      6B000000000008080800000000006B6B6B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006B6B6B0094949400525252006B6B6B0008080800000000000000
      000000000000393939000031520039393900848484006BC6FF008CD6FF006B6B
      6B00000000003131310000000000393939000000000039393900636363000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006B6B6B0094949400525252006B6B6B0008080800000000000000
      000000000000393939000031520039393900848484006BC6FF008CD6FF006B6B
      6B00000000003131310000000000393939000000000039393900636363000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009C9C9C0008080800000000000000
      0000949494005252520000315200393939006B6B6B006BC6FF008CD6FF006B6B
      6B00000000000000000084848400848484000000000008080800000000008484
      8400393939000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009C9C9C0008080800000000000000
      0000949494005252520000315200393939006B6B6B006BC6FF008CD6FF006B6B
      6B00000000000000000084848400848484000000000008080800000000008484
      8400393939000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000180052003900BD004A73FF006B6B6B00848484006BC6FF006BC6FF000021
      730052525200A5A5A50000000000000000000000000084848400313131008484
      8400393939000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000180052003900BD004A73FF006B6B6B00848484006BC6FF006BC6FF000021
      730052525200A5A5A50000000000000000000000000084848400313131008484
      8400393939000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003939
      3900393939009494940000000000000000000000000000000000000000006B6B
      6B00080808000808080000185200006394004A73FF004A73FF004A73FF004A73
      FF00003194003939390000000000000000000000000000000000000000000000
      0000393939000000000000000000000000000000000000000000000000003939
      3900393939009494940000000000000000000000000000000000000000006B6B
      6B00080808000808080000185200006394004A73FF004A73FF004A73FF004A73
      FF00003194003939390000000000000000000000000000000000000000000000
      0000393939000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003939
      390008080800393939006B6B6B00848484005252520000000000000000000000
      0000000000009C9C9C0039393900000000000021730000639400004A73006363
      63006B6B6B0000000000000000009C9C9C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003939
      390008080800393939006B6B6B00848484005252520000000000000000000000
      0000000000009C9C9C0039393900000000000021730000639400004A73006363
      63006B6B6B0000000000000000009C9C9C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008484840052525200848484006B6B6B000808080000000000393939000000
      00000000000000000000000000000000000094949400A5A5A500000000000000
      000000000000000000000000000039393900000000006B6B6B00949494000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008484840052525200848484006B6B6B000808080000000000393939000000
      00000000000000000000000000000000000094949400A5A5A500000000000000
      000000000000000000000000000039393900000000006B6B6B00949494000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000949494000808080008080800313131006363
      630000000000393939006B6B6B00000000000000000000000000000000000000
      0000000000000000000000000000525252000000000008080800080808009494
      94006B6B6B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000949494000808080008080800313131006363
      630000000000393939006B6B6B00000000000000000000000000000000000000
      0000000000000000000000000000525252000000000008080800080808009494
      94006B6B6B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C00848484006363
      6300080808000808080000000000000000003939390094949400000000000000
      0000000000000000000000000000000000000000000052525200080808008484
      8400393939000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C9C9C00848484006363
      6300080808000808080000000000000000003939390094949400000000000000
      0000000000000000000000000000000000000000000052525200080808008484
      8400393939000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484003939390008080800080808000808080000000000080808006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      00006B6B6B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484003939390008080800080808000808080000000000080808006B6B
      6B00000000000000000000000000000000000000000000000000000000000000
      00006B6B6B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484003939390000000000000000000000
      0000A5A5A5003939390039393900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484003939390000000000000000000000
      0000A5A5A5003939390039393900000000000000000000000000000000000000
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
      00000000000000000000000000000000000000000000000000006B6B6B000808
      08009C9C9C003131310000000000393939000000000094949400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000006B6B6B000808
      08009C9C9C003131310000000000393939000000000094949400000000000000
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
      000000000000949494003131310052525200000000000808080008080800A5A5
      A500949494000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000949494003131310052525200000000000808080008080800A5A5
      A500949494000000000000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000031313100000000008484
      8400393939000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000031313100000000008484
      8400393939000000000000000000000000000000000000000000000000000000
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
      00000000000000000000000000000000000000000000000000009C9C9C009494
      9400393939000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009C9C9C009494
      9400393939000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
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
      0000000000003600000028000000400000008000000001002000000000000080
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
      0000848484008484840084848400000000000000000084848400848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008484840084848400848484008484840084848400848484000000000000FF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840084848400000000000000000084848400848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000084000000840000008400000084000000840000008400
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000000000000000000000000000000000000840000008400
      000084000000000000000000000000000000000000000000000000000000FFFF
      FF0000FFFF00FFFFFF00FFFFFF00FFFFFF008400000084000000FFFFFF00FFFF
      FF0000FFFF00FFFFFF0000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000000000008400
      000084000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      000084000000840000000000000000000000000000000000000000000000FFFF
      FF0000FFFF00FFFFFF00FFFFFF00FFFFFF008400000084848400FFFFFF00FFFF
      FF0000FFFF00FFFFFF00000000008484840000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      000084000000840000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000FFFF00FFFFFF00848484008400000000000000FFFF
      FF00FFFFFF00FFFFFF00000000008484840000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      000084000000840000000000000000000000000000000000000000000000FFFF
      FF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF0084848400840000008484
      840000FFFF00FFFFFF0000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      000084000000840000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF008400000084848400FFFFFF00FFFFFF00840000008400
      0000FFFFFF00FFFFFF0000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000084000000000000008400
      000084000000000000000000000000000000000000000000000000000000FFFF
      FF0000FFFF00FFFFFF00840000008400000000FFFF0000000000840000008400
      000000FFFF00FFFFFF0000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000084000000840000008400
      000084000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000840000008400000084000000840000000000
      0000FFFFFF00FFFFFF0000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840084848400000000000000000084848400848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFF
      FF0000FFFF00FFFFFF0000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840084848400000000000000000084848400848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      000084000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
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
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000000000008400000084000000000000000000000000000000000000000000
      0000008484000084840000848400008484000084840000848400008484000084
      8400000000000000000000000000000000000000000000000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      00000000840000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000000000
      000084000000840000008400000000000000000000000000000000000000FFFF
      FF00000000000084840000848400008484000084840000848400008484000084
      8400008484000000000000000000000000000000000084000000840000000000
      0000000000008400000084000000000000000000000000000000000000000000
      84000000840000008400000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000000000008400
      00008400000084000000000000000000000000000000000000000000000000FF
      FF00FFFFFF000000000000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000084000000840000000000
      0000000000000000000000000000000000000000000000000000000084000000
      84000000840000008400000084000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000840000008400
      000084000000000000000000000000000000000000000000000000000000FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      00000000840000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF008484840084000000840000008400
      00000000000000000000000000000000000000000000000000000000000000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000084000000840000000000
      0000000000008400000084000000000000000000000000000000000000000000
      00000000840000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000848484000000
      000000000000000000000000000000000000FFFFFF0084848400000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      00000000840000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      00000000000000000000000000000000000000000000848484000000000000FF
      FF00FFFFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000840000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FF000000FF0000000000
      000000000000000000000084000000840000FF00000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000840000000000000000000000000000000000FFFFFF00000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FF000000FF000000FF00
      00000084000000840000008400000084000084840000FF000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      00000000840000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000000000000000000000FFFFFF00FFFF
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FF000000008400000084
      0000008400000084000000840000FF00000000840000FF000000000000000000
      0000000000000000000000000000000000000000000084000000840000000000
      0000000000008400000084000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000000000000000000000FFFFFF00FFFF
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FF000000000000000084
      000000840000FF00000000840000FF0000000084000000840000000000000000
      0000000000000000000000000000000000000000000084000000840000000000
      0000000000008400000084000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000000000000000000000FFFFFF00FFFF
      FF00FFFF0000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400FF000000FFFF
      FF008484000000840000FF000000FF0000000084000000840000848484000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008400000084000000000000000000000084848400FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF000000
      0000848400000000000000840000008400000084000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400FF00
      0000000000008484840000840000008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400848484008484840084848400848484000000000000000000000000000000
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
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000000000000000000000FF
      FF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000840000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000FFFF00FFFFFF00FFFFFF00C6C6C60000FFFF00FFFFFF00FFFF
      FF00FFFFFF0000FFFF0000000000000000000000000000000000000000000000
      00000000000084840000FF00000084840000FF00000084840000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000084000000840000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000000000000000000000FF
      FF00FFFFFF00FFFFFF00FFFFFF00C6C6C600FF000000C6C6C600FFFFFF0000FF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000084
      000000FF00000000000084840000FF00000084840000FF00000084840000FF00
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000084000000840000008400000084000000840000FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFF
      FF00FFFFFF0000FFFF00000000000000000000000000000000000084000000FF
      00000084000000FF000000000000FFFF0000FF000000FFFF0000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000008484840000000000000000008484
      840000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000084000000840000FFFFFF00FFFFFF000084
      0000FFFFFF00FFFFFF00000000000000000000000000000000000000000000FF
      FF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FF000000FFFFFF00FFFFFF0000FF
      FF00FFFFFF00FFFFFF000000000000000000000000000000000000FF00000084
      000000FF00000084000000000000FF000000FFFF000000000000008400000084
      00000084000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000848484000000000000000000FFFF00008484
      840084848400000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000840000FFFFFF00FFFFFF000084
      0000FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FF000000FF000000FFFFFF00FFFF
      FF00FFFFFF0000FFFF0000000000000000000000000000FF00000084000000FF
      00000000000000000000FF000000FFFF00000000000000FF00000084000000FF
      00000084000000840000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000008484
      840000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000840000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000084
      0000FFFFFF00FFFFFF00000000000000000000000000000000000000000000FF
      FF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FF000000FF00000000FF
      FF00FFFFFF00FFFFFF000000000000000000000000000084000000FF00000000
      0000FFFF0000FF000000FFFF0000FF000000000000000084000000FF00000084
      000000FF000000840000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000000000000FFFF000000000000000000008484
      840000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000840000FFFFFF00FFFFFF0000840000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00C6C6C600FF000000FFFF
      FF00FFFFFF0000FFFF000000000000000000000000000000000000000000FFFF
      0000FF000000FFFF0000FF000000FFFF00000000000000FF00000084000000FF
      00000084000000840000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000084848400FFFF0000FFFF0000000000008484
      840084848400000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000840000FFFFFF00FFFFFF000084000000840000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000000000000000000000FF
      FF00FFFFFF00FFFFFF00FF00000000FFFF00FFFFFF00C6C6C600FF00000000FF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFF0000FFFF0000FF00
      0000FFFF0000FF000000FFFF0000FF000000FFFF00000000000000FF00000084
      000000FF000000840000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000008484840000000000000000008484
      840000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000084000000840000008400000084000000840000FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000FFFF00FF000000FF000000C6C6C600FF000000FF000000FFFF
      FF00FFFFFF0000FFFF00000000000000000000000000FFFF0000FF0000000000
      0000FF000000FFFF0000FF000000FFFF0000FF000000FFFF00000000000000FF
      00000084000000840000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000084000000840000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000000000000000000000FF
      FF00FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FF000000FFFFFF0000FF
      FF00FFFFFF00FFFFFF00000000000000000000000000000000000000000000FF
      000000000000FF000000FFFF0000FF000000FFFF0000FF000000FFFF00000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000840000FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000FF00000084
      000000FF00000000000000000000FFFF0000FF000000FFFF0000FF000000FFFF
      0000FF00000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF0000000000000000000000000000000000000000000000000000FF
      FF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF0000000000000000000000000000000000000000000000000000FF
      00000084000000FF0000000000000000000000000000FF000000FFFF0000FF00
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000084000000FF00000084000000FF000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
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
      0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000C6C6C6000084
      8400000000000000000000000000000000000000000000000000FFFFFF00C6C6
      C600000000000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF00000000000000000000000000000000000000000000848400C6C6
      C600000000000000000000000000000000000000000000000000C6C6C600FFFF
      FF0000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000C6C6C6000084
      8400000000000000000000000000000000000000000000000000FFFFFF00C6C6
      C600000000000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000000000000000000000000000000000848400C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000C6C6C6000084
      8400C6C6C60000848400C6C6C60000848400C6C6C60000848400C6C6C6000084
      8400C6C6C6000084840000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF00FFFFFF00FFFFFF000000FF000000FF00FFFFFF00FFFFFF000000
      FF000000FF000000FF000000FF0000000000000000000000000000848400C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      000000848400C6C6C60000000000000000000000000000000000000000000000
      0000000000008400000084000000840000008400000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000084000000840000008400000084000000840000008400
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000C6C6C6000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000084840000000000000000000000000000000000000000000000
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000840000008400000084000000840000008400000084000000840000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF00FFFFFF00FFFFFF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000008484000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000840000008400000084000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000C6C6C6000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF00FFFFFF00FFFFFF000000FF000000FF00FFFFFF00FFFFFF000000
      FF000000FF000000FF000000FF00000000000000000000000000008484000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000C6C6C6000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000008484000000
      0000008400000084000000840000008400000084000000840000008400000084
      000000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
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
      0000000000000000000000000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6000000
      0000C6C6C60000000000000000000000000000000000000000000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C6000000000000000000000000000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484008484
      840000848400848484000084840084000000FFFFFF0084000000840000008400
      00008400000084000000FFFFFF00840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C60000FF000000FF000000FF0000C6C6C600C6C6
      C60000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      00000000FF000000000000000000000000000000000000000000848484000084
      840084848400008484008484840084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600008400000084000000840000C6C6C600C6C6
      C60000000000C6C6C600000000000000000000000000000000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      FF00000000000000000000000000000000000000000000000000008484008484
      840000848400848484000084840084000000FFFFFF0084000000840000008400
      0000FFFFFF008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C600C6C6C600000000000000000000000000000000000000
      FF000000FF000000FF00000000000000000000000000000000000000FF000000
      FF00000000000000000000000000000000000000000000000000848484000084
      840084848400008484008484840084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0084000000FFFFFF00840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6000000
      0000C6C6C60000000000C6C6C600000000000000000000000000000000000000
      00000000FF000000FF000000FF0000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000008484008484
      840000848400848484000084840084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C60000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000848484000084
      8400848484000084840084848400840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000C6C6C60000000000C6C6C600000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000008484008484
      8400008484008484840000848400848484000084840084848400008484008484
      8400008484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FF000000FF000000FF000000FF000000FF000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000848484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000848484000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF0000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000008484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000008484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FF000000FF000000FF000000FF000000FF000000FFFF
      FF000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF00000000000000000000000000000000000000FF000000
      FF00000000000000000000000000000000000000000000000000848484000084
      8400848484000000000000FFFF00000000000000000000FFFF00000000000084
      8400848484000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000008484000084
      840000000000000000000000000000000000FFFFFF0000000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000008484000084
      840000000000000000000000000000000000FFFFFF0000000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000008484000000
      0000008484000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000008484000084
      8400008484000084840000848400008484000084840000848400008484000000
      0000008484000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00840000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000848400008484000000
      0000008484000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000008484000000
      0000008484000000000000848400000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00840000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000008484000000
      0000008484000000000000848400000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000008484000000000000848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF000000000000000000FFFF
      FF00840000008400000084000000840000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF000000
      0000008484000000000000848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0084000000FFFFFF0084000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00840000008400000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000084840000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF000000000000848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000FFFFFF0000000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000084840000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
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
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000084840000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000084840000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000008484000084840000848400008484000084840000848400008484000084
      8400008484000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000084840000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000FFFF00FFFF
      FF00000000000084840000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000084840000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF000000000000848400008484000084840000848400008484000084
      8400008484000084840000848400000000000000000000000000008484000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000008484000084840000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000084840000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000084840000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000084840000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000084840000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000000
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
