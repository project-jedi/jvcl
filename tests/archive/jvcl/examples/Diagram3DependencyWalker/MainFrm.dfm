object frmMain: TfrmMain
  Left = 301
  Top = 157
  Width = 603
  Height = 504
  BorderWidth = 2
  Caption = 'Dependency Walker'
  Color = clBtnFace
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
  object Splitter1: TSplitter
    Left = 401
    Top = 26
    Width = 5
    Height = 409
    Cursor = crHSplit
    Align = alRight
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 435
    Width = 591
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 26
    Width = 401
    Height = 409
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 401
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
  end
  object Panel2: TPanel
    Left = 406
    Top = 26
    Width = 185
    Height = 409
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object lbSkipList: TListBox
      Left = 0
      Top = 21
      Width = 185
      Height = 388
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
    object Panel4: TPanel
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
    end
  end
  object CoolBar1: TCoolBar
    Left = 0
    Top = 0
    Width = 591
    Height = 26
    AutoSize = True
    Bands = <
      item
        Control = ToolBar1
        ImageIndex = -1
        MinHeight = 22
        Width = 587
      end>
    object ToolBar1: TToolBar
      Left = 9
      Top = 0
      Width = 574
      Height = 22
      AutoSize = True
      Caption = 'ToolBar1'
      EdgeBorders = []
      Flat = True
      Images = Actions
      TabOrder = 0
      Wrapable = False
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        Action = acSelectFiles
      end
      object ToolButton2: TToolButton
        Left = 23
        Top = 0
        Action = acClear
      end
      object ToolButton6: TToolButton
        Left = 46
        Top = 0
        Action = acPrint
      end
      object ToolButton3: TToolButton
        Left = 69
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object ToolButton8: TToolButton
        Left = 77
        Top = 0
        Action = acFind
      end
      object ToolButton13: TToolButton
        Left = 100
        Top = 0
        Width = 8
        Caption = 'ToolButton13'
        ImageIndex = 18
        Style = tbsSeparator
      end
      object ToolButton9: TToolButton
        Left = 108
        Top = 0
        Action = acUnitStats
      end
      object ToolButton12: TToolButton
        Left = 131
        Top = 0
        Action = acDelDiagram
      end
      object ToolButton7: TToolButton
        Left = 154
        Top = 0
        Width = 8
        Caption = 'ToolButton7'
        ImageIndex = 5
        Style = tbsSeparator
      end
      object ToolButton4: TToolButton
        Left = 162
        Top = 0
        Action = acAdd
      end
      object ToolButton5: TToolButton
        Left = 185
        Top = 0
        Action = acDelete
      end
      object ToolButton11: TToolButton
        Left = 208
        Top = 0
        Width = 8
        Caption = 'ToolButton11'
        ImageIndex = 6
        Style = tbsSeparator
      end
      object ToolButton10: TToolButton
        Left = 216
        Top = 0
        Action = acAbout
      end
    end
  end
  object mmMain: TMainMenu
    Images = Actions
    Left = 24
    Top = 40
    object File1: TMenuItem
      Caption = 'File'
      object SelectFiles1: TMenuItem
        Action = acSelectFiles
      end
      object Print1: TMenuItem
        Action = acPrint
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
      object Arrange1: TMenuItem
        Caption = 'Arrange'
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
        object N3: TMenuItem
          Caption = '-'
          GroupIndex = 1
        end
        object InvertSort1: TMenuItem
          Action = acInvertSort
          GroupIndex = 1
        end
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
      object N2: TMenuItem
        Caption = '-'
      end
      object Find1: TMenuItem
        Action = acFind
      end
      object Clear1: TMenuItem
        Action = acClear
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
    Left = 232
    Top = 40
  end
  object il32: TImageList
    Height = 32
    Width = 32
    Left = 120
    Top = 40
    Bitmap = {
      494C010101000400040020002000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000800000002000000001001000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000040004200420042004210420000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000042E07FE07F1863004210420000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000421042E07F186318631042104210420000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000004010421042186318631042000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001042186318631042000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001042104200000000
      0000000000000000000000000000000000001042186318631042000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001042
      1042104200000000000000000000000000001042186318631042000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001042000000001042
      1042000000001042000000000000000000001042186318631042000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      1042000000000000000000000000000000001042186318631042000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000010420000000000000000000000001042186318631042000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001042104200000000
      0000000000000000000000000000000000001042186318631042000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000010421042
      0000000000000000000000000000000000001042186318631042000010421042
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001042000000001042
      1042000000000000000000000000000000001042186318631042000000000000
      1042000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000010421042
      1042000000000000000000000000000000001042186318631042000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000104200000000000000000000000000001042186318631042000000000000
      0000000000000000104200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001042104200000000
      0000000000000000000000000000000000001042186318631042000000000000
      0000000000001042104200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000010421042
      0000000000000000000000000000000000001042186318631042000010420000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001042000000001042
      1042104210420000000000000000000000001042186318631042000000000000
      1042000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000010421042
      1042104200000000000000000000004000001042186318631042000000000000
      0000000000001042000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000104200000000000010421042004000001042186318631042000000001042
      1042000000000000104200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000400040007C10421042186318630040104218630000
      0000000010420000104200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000010420000
      000000000000000010420000000000400042007C007C007C007C004000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001042
      1042104200000000000000001042000000000040004200421042104200000000
      1042000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000104210421042
      1042000000000000000000000000000000001042186300000000000000000000
      0000000010421042000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      1042000000000000104200000000104200000000000000000000000000000000
      1042000000000000104210420000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000018631042104200000000000000000000104200000000000000000000
      0000000010420000104200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010420000000000000000000000001042000000000000
      0000000000000000000010420000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000010420000000000000000104200000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000010420000104200000000
      0000000010420000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000010420000
      1042000000000000104210420000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000104200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001042104200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000080000000200000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF00FFF003FF000000000000000000000000
      FFF003FF000000000000000000000000FFF003FF000000000000000000000000
      FFF80FFF000000000000000000000000FFF80FFF000000000000000000000000
      E7F80FFF000000000000000000000000E0780FFF000000000000000000000000
      E0180FFF000000000000000000000000FE180FFF000000000000000000000000
      FF980FFF000000000000000000000000E7F80FFF000000000000000000000000
      E1F809FF000000000000000000000000E03808FF000000000000000000000000
      F818089F000000000000000000000000FF180C87000000000000000000000000
      E7F80FC7000000000000000000000000E1F809FF000000000000000000000000
      E03808FF000000000000000000000000F838089F000000000000000000000000
      FF300C87000000000000000000000000FFE00387000000000000000000000000
      E3E003F7000000000000000000000000E07806FF000000000000000000000000
      F01F3E9F000000000000000000000000FE01FE87000000000000000000000000
      FF803F87000000000000000000000000FFF00FF7000000000000000000000000
      FFFE01FF000000000000000000000000FFFFC0BF000000000000000000000000
      FFFFF887000000000000000000000000FFFFFF87000000000000000000000000
      FFFFFFC700000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object popSkipList: TPopupMenu
    Left = 168
    Top = 40
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
    Top = 40
    object acSelectFiles: TAction
      Category = 'File'
      Caption = 'Select Files...'
      ImageIndex = 1
      ShortCut = 16463
      OnExecute = acSelectFilesExecute
    end
    object acExit: TAction
      Category = 'File'
      Caption = 'Exit'
      ShortCut = 32883
      OnExecute = acExitExecute
    end
    object acSortName: TAction
      Category = 'Arrange'
      Caption = 'by Name'
      ShortCut = 16433
      OnExecute = acArrangeAction
    end
    object acSortLinksTo: TAction
      Tag = 1
      Category = 'Arrange'
      Caption = 'by Links To'
      ShortCut = 16434
      OnExecute = acArrangeAction
    end
    object acSortLinksFrom: TAction
      Tag = 2
      Category = 'Arrange'
      Caption = 'by Links From'
      ShortCut = 16435
      OnExecute = acArrangeAction
    end
    object acInvertSort: TAction
      Category = 'Arrange'
      Caption = 'Inverted Sort'
      ShortCut = 16457
      OnExecute = acInvertSortExecute
    end
    object acAdd: TAction
      Category = 'Skiplist'
      Caption = 'Add...'
      ImageIndex = 3
      ShortCut = 16429
      OnExecute = acAddExecute
    end
    object acDelete: TAction
      Category = 'Skiplist'
      Caption = 'Delete...'
      ImageIndex = 4
      ShortCut = 16430
      OnExecute = acDeleteExecute
    end
    object acClear: TAction
      Category = 'Edit'
      Caption = 'Clear'
      ImageIndex = 11
      ShortCut = 24622
      OnExecute = acClearExecute
    end
    object acAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      ImageIndex = 17
      OnExecute = acAboutExecute
    end
    object acUnitStats: TAction
      Category = 'Diagrams'
      Caption = 'Statistics...'
      ImageIndex = 20
      ShortCut = 32781
      OnExecute = acUnitStatsExecute
    end
    object acDelDiagram: TAction
      Category = 'Diagrams'
      Caption = 'Delete...'
      ImageIndex = 9
      OnExecute = acDelDiagramExecute
    end
    object acPrint: TAction
      Category = 'File'
      Caption = 'Print...'
      ImageIndex = 10
      ShortCut = 16464
      OnExecute = acPrintExecute
    end
    object acFind: TAction
      Category = 'Edit'
      Caption = 'Find...'
      ImageIndex = 21
      ShortCut = 16454
      OnExecute = acFindExecute
    end
  end
  object popDiagram: TPopupMenu
    Left = 168
    Top = 96
    object Delete3: TMenuItem
      Action = acDelDiagram
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Statistics1: TMenuItem
      Action = acUnitStats
    end
  end
  object Actions: TImageList
    Left = 240
    Top = 80
    Bitmap = {
      494C010116001800040010001000FFFFFFFFFF00FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000006000000001001000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000010001000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000100000001000100000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000000000000000000000000
      0000000000001000000010001000100000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF7F00000000FF7F00000000
      000000000000FF7F000000000000000000000000000000000000000000000000
      0000000010000000100010001000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000000000000000000000000
      0000100000001000100010000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF7F00000000FF7F00000000
      000000000000FF7F000000000000000000000000000000000000000000000000
      FF7F104210001000100000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000010420000000000000000
      0000FF7F10420000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7F0000FF7FFF7F000000000000000000000000000000000000000000000000
      00000000FF7F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF7F00000000FF7FFF7FFF7F
      000000000000FF7F000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF7F000000000000FF7F0000
      0000000000000000000000000000100010000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF7FFF7F0000000000000000
      00000000000000000000000000001000100000000000FF7FFF03000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000001000100000000000FF7FFF03000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000001000100000000000FF7FFF7FFF03FF030000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000010001000000000001042FF7FFF7FFF7F0000
      0000000010420000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000100010000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
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
      2800000040000000600000000100010000000000000300000000000000000000
      000000000000000000000000FFFFFF00FFFFFFF300000000000FFFE900000000
      000FFFD100000000000FFFA300000000000FF04700000000000FCE0F00000000
      000F9F1F00000000000FBF9F00000000008F7FEF0000000011447FEF00000000
      0AB84FEF00000000057C4FEF00000000FAFC83DF00000000FDF8839F00000000
      FE04CF3F00000000FFFFF0FF00000000C001C001FFFFFFFFC001C001F83F000C
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
      E01FFFBA8FF5807FFFFFFFC78001FFFF}
  end
end
