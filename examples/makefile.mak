!ifndef ROOT
ROOT = $(MAKEDIR)
!endif
#---------------------------------------------------------------------------------------------------
RUN = ..\..\run
COM = ..\..\common
BIN = ..\..\bin
DCU = ..\..\dcu
JCLSRC=..\..\..\JCL\source
JCL = $(JCLSRC);$(JCLSRC)\common;$(JCLSRC)\windows;$(JCLSRC)\vcl
JCLH = ..\$(JCLSRC);..\$(JCLSRC)\common;..\$(JCLSRC)\windows;..\$(JCLSRC)\vcl
JCLHH = ..\..\$(JCLSRC);..\..\$(JCLSRC)\common;..\..\$(JCLSRC)\windows;..\..\$(JCLSRC)\vcl
JCLHHH = ..\..\..\$(JCLSRC);..\..\..\$(JCLSRC)\common;..\..\..\$(JCLSRC)\windows;..\..\..\$(JCLSRC)\vcl
SRC = $(RUN);$(COM);$(JCL);
SRCH = ..\$(RUN);..\$(COM);$(JCLH);
SRCHH = ..\..\$(RUN);..\..\$(COM);$(JCLHH);
SRCHHH = ..\..\..\$(RUN);..\..\..\$(COM);$(JCLHHH);
#---------------------------------------------------------------------------------------------------
MAKE = $(ROOT)\make.exe -$(MAKEFLAGS) -f$**
DCC  = $(ROOT)\dcc32.exe -e$(BIN) -i$(SRC) -n$(DCU) -r$(SRC) -u$(SRC) -q -w -B
DCCH = $(ROOT)\dcc32.exe -e..\$(BIN) -i$(SRCH) -n..\$(DCU) -r$(SRCH) -u$(SRCH) -q -w -B
DCCHSURVEY = $(ROOT)\dcc32.exe -e..\$(BIN) -i$(SRCH);..\common -n..\$(DCU) -r$(SRCH);..\common -u$(SRCH);..\common -q -w -B
DCCHH = $(ROOT)\dcc32.exe -e..\..\$(BIN) -i$(SRCHH) -n..\..\$(DCU) -r$(SRCHH) -u$(SRCHH) -q -w -B
DCCHHH = $(ROOT)\dcc32.exe -e..\..\..\$(BIN) -i$(SRCHHH) -n..\..\..\$(DCU) -r$(SRCHHH) -u$(SRCHHH) -q -w -B
BRCC = $(ROOT)\brcc32.exe $**
#---------------------------------------------------------------------------------------------------
all: uib globus docking inspector standard threads ralib rxlib plugin xml mega surveyor diagram
#---------------------------------------------------------------------------------------------------
surveyor: jsb.exe jsr.exe sc.exe js.exe
#---------------------------------------------------------------------------------------------------
globus: Gl_demo.exe _glXMLSerializer_demo.exe
#---------------------------------------------------------------------------------------------------
xml: JvSimpleXMLDemo.exe JvSimpleXMLEncodeDemo.exe JvSimpleXMLSpeedTest.exe XMLValidator.exe
#---------------------------------------------------------------------------------------------------
rxlib: Rxdemo.exe RxGIFAnm.exe EditorDemo.exe MessengerDemo.exe DBEXPL32.exe
#---------------------------------------------------------------------------------------------------
diagram: WebMapper.exe UseCase.exe DepWalk.exe
#---------------------------------------------------------------------------------------------------
threads: Balls.exe Philosophers.exe
#---------------------------------------------------------------------------------------------------
docking: AdvancePro.exe DockOptionDemo.exe HowToUse.exe MSDN2002Pro.exe VCDemoPro.exe VIDDemoPro.exe
#---------------------------------------------------------------------------------------------------
inspector: InspectorSimpleExample.exe InspExample.exe InspectorDBExample.exe
#---------------------------------------------------------------------------------------------------
mega: JVCLDemo.exe
#---------------------------------------------------------------------------------------------------
plugin: PlugInDemo.exe SamplePluginOne.dll MDIPlugin.dll MDISample.exe ChangePropertiesPlugin.dll \
PropertiesPlugInDemo.exe ExceptionPlugin.dll ExceptionPlugInDemo.exe DataPlugin.dll DataPlugInDemo.exe \
JvPlgMainApp.exe
#---------------------------------------------------------------------------------------------------
ralib: Pas2Rai2.exe RAControls.exe DBMove.exe DBTree.exe LineNumbers.exe RAEditorTest.exe \
RAHLEdPropDlgTest.exe ColorHintsTest.exe JvInterpreterTest.exe SampleProject1.exe DynamicLoad.exe \
JvInterpreterEndUser.exe MDIapp.exe RANotepad.exe
#---------------------------------------------------------------------------------------------------
uib: \
api1.exe api2.exe api3.exe api4.exe api5.exe api6.exe api7.exe api8.exe api10.exe \
StartBackup.exe StartRestore.exe \
#Client.exe #Server.exe \
UIB.dll Backup.exe Restore.exe BlobStream.exe BlobSample.exe cursor.exe DataPump.exe MetaData.exe \
Query.exe QuickScript.exe Script.exe StoredProc.exe QueryStream.exe
#---------------------------------------------------------------------------------------------------
standard: JvAniProj.exe AppDdeCmdExample.exe JvAppHotKeyDemo.exe JvAppInstDemo.exe ArrowButtonDemo.exe \
JvAviCapDemo.exe BalloonPrj.exe JvBehaviorLabelDemo.exe BmpAnimDemo.exe JvBrowserFolder.exe \
CaptionButtonDemo.exe ChangeNotifyDemo.exe JvCharMapDemo.exe JvChartDemo.exe CheckTVDemo.exe \
JvClipboardViewerProj.exe ColorButtonDemo.exe ColorComboDemo.exe JvComboListBoxDemo.exe \
ContentScrollerDemo.exe ControlsExample.exe CreateProcessExample.exe CsvDataDemo.exe JvDataEmbeddedProj.exe \
DBDTPDemo.exe JvDBGridExportDemo.exe JvDbMaskEditDemo.exe JvDialogsDemo.exe JvDomainUpDownDemo.exe \
JvDotNetDemo.exe DSAExamples.exe MessageDlgEditor.exe JvEdits.exe JvErrorIndicatorDemo.exe FileDirDemo.exe \
FindReplaceDemo.exe prjControls.exe BasicDemo.exe CollectionDemo.exe SimpleHIDWrite.exe \
UsagesDemo.exe JvHtmlParserProj.exe JvID3v1Demo.exe JvID3v2Demo.exe Install2LabelDemo.exe \
JvItemViewerDemo.exe JvLinkLabelDemo.exe ListCombDemo.exe JvLogFileDemo.exe MailExample.exe \
JvMarkupLabelDemo.exe JvMenusExample.exe JvMouseGestureDemo.exe JvMruListDemo.exe \
EventViewer.exe JvObjPickerDemo.exe JvOLEDragCursorsDemo.exe OLBarDemo.exe pm.exe \
JvPanelDemo.exe JvParameterListDemo.exe JvPlayListProj.exe JvPrvwDemo.exe ProfilerDemo.exe \
JvProgressDialogDemo.exe RegEditDemo.exe JvRollOutDemo.exe RunDLL32Demo.exe JvScreenCaptureProj.exe \
ScrollWinDemo.exe JvSearchFileProj.exe JvShellHookDemo.exe ShFileOpDemo.exe SpecialProgressTestPrj.exe \
JvSpellCheckerDemo.exe JvStringGridDemo.exe JvSystemPopupProj.exe SystemPopupTest.exe JvThreadProj.exe \
JvThumbnailDemo.exe PhotoOp.exe TimeLineDemo.exe TipsDemo.exe SimpleTLTest1.exe JvTranslatorProj.exe \
TransparentButtonDemo.exe JvTrayIconDemo.exe JvTreeViewAsMenu.exe JvUninstallCtrlsDemo.exe \
JvUrlListGrabberDemo.exe JvFormatEditDemo.exe JvValidatorsDemo.exe JvWinDialogsDemo.exe \
JvWindowsTitleProj.exe WndProcHookDemo.exe Hospital.exe WinXPBarDemo.exe SimpleDemo.exe \
JvZLibMultipleDemo.exe JvZoomProj.exe LinkedConsumers.exe
#
# (rom) temporarily deactivated  does not compile
# JvMultiHTTPGrabberDemo.exe
# PageListDemo.exe
#---------------------------------------------------------------------------------------------------
Gl_demo.exe: "Globus\Visual Components Demo\Gl_demo.dpr"
  @cd Globus\Visual Components Demo
  $(DCCH) $&.dpr
  @cd ..\..

_glXMLSerializer_demo.exe: "Globus\XMLSerializer\_glXMLSerializer_demo.dpr"
  @cd Globus\XMLSerializer
  $(DCCH) $&.dpr
  @cd ..\..

jsb.exe: "JediSurveyor\Builder\jsb.dpr"
  @cd JediSurveyor\Builder
  $(DCCHSURVEY) $&.dpr
  @cd ..\..

jsr.exe: "JediSurveyor\Reporter\jsr.dpr"
  @cd JediSurveyor\Reporter
  $(DCCHSURVEY) $&.dpr
  @cd ..\..

sc.exe: "JediSurveyor\SurveyConvert\sc.dpr"
  @cd JediSurveyor\SurveyConvert
  $(DCCHSURVEY) $&.dpr
  @cd ..\..

js.exe: "JediSurveyor\Surveyor\js.dpr"
  @cd JediSurveyor\Surveyor
  $(DCCHSURVEY) $&.dpr
  @cd ..\..

JvAniProj.exe: "JvAni\JvAniProj.dpr"
  @cd JvAni
  $(DCC) $&.dpr
  @cd ..

AppDdeCmdExample.exe: "JvAppDDECmd\AppDdeCmdExample.dpr"
  @cd JvAppDDECmd
  $(DCC) $&.dpr
  @cd ..

JvAppHotKeyDemo.exe: "JvAppHotKey\JvAppHotKeyDemo.dpr"
  @cd JvAppHotKey
  $(DCC) $&.dpr
  @cd ..

JvAppInstDemo.exe: "JvAppInstances\JvAppInstDemo.dpr"
  @cd JvAppInstances
  $(DCC) $&.dpr
  @cd ..

ArrowButtonDemo.exe: "JvArrowButton\ArrowButtonDemo.dpr"
  @cd JvArrowButton
  $(DCC) $&.dpr
  @cd ..

JvAviCapDemo.exe: "JvAviCapture\JvAviCapDemo.dpr"
  @cd JvAviCapture
  $(DCC) $&.dpr
  @cd ..

BalloonPrj.exe: "JvBalloonHint\BalloonPrj.dpr"
  @cd JvBalloonHint
  $(DCC) $&.dpr
  @cd ..

JvBehaviorLabelDemo.exe: "JvBehaviorLabel\JvBehaviorLabelDemo.dpr"
  @cd JvBehaviorLabel
  $(DCC) $&.dpr
  @cd ..

BmpAnimDemo.exe: "JvBMPAnimator\BmpAnimDemo.dpr"
  @cd JvBMPAnimator
  $(DCC) $&.dpr
  @cd ..

JvBrowserFolder.exe: "JvBrowseFolder\JvBrowserFolder.dpr"
  @cd JvBrowseFolder
  $(DCC) $&.dpr
  @cd ..

CaptionButtonDemo.exe: "JvCaptionButton\CaptionButtonDemo.dpr"
  @cd JvCaptionButton
  $(DCC) $&.dpr
  @cd ..

ChangeNotifyDemo.exe: "JvChangeNotify\ChangeNotifyDemo.dpr"
  @cd JvChangeNotify
  $(DCC) $&.dpr
  @cd ..

JvCharMapDemo.exe: "JvCharMap\JvCharMapDemo.dpr"
  @cd JvCharMap
  $(DCC) $&.dpr
  @cd ..

JvChartDemo.exe: "JvChartDemo\JvChartDemo.dpr"
  @cd JvChartDemo
  $(DCC) $&.dpr
  @cd ..

CheckTVDemo.exe: "JvCheckTreeView\CheckTVDemo.dpr"
  @cd JvCheckTreeView
  $(DCC) $&.dpr
  @cd ..

JvClipboardViewerProj.exe: "JvClipboardViewer\JvClipboardViewerProj.dpr"
  @cd JvClipboardViewer
  $(DCC) $&.dpr
  @cd ..

JVCLDemo.exe: "JVCLMegaDemo\JVCLDemo.dpr"
  @cd JVCLMegaDemo
  $(DCC) $&.dpr
  @cd ..

ColorButtonDemo.exe: "JvColorButton\ColorButtonDemo.dpr"
  @cd JvColorButton
  $(DCC) $&.dpr
  @cd ..

ColorComboDemo.exe: "JvColorCombo\ColorComboDemo.dpr"
  @cd JvColorCombo
  $(DCC) $&.dpr
  @cd ..

JvComboListBoxDemo.exe: "JvComboListBox\JvComboListBoxDemo.dpr"
  @cd JvComboListBox
  $(DCC) $&.dpr
  @cd ..

ContentScrollerDemo.exe: "JvContentScroller\ContentScrollerDemo.dpr"
  @cd JvContentScroller
  $(DCC) $&.dpr
  @cd ..

ControlsExample.exe: "JvControls\ControlsExample.dpr"
  @cd JvControls
  $(DCC) $&.dpr
  @cd ..

CreateProcessExample.exe: "JvCreateProcess\CreateProcessExample.dpr"
  @cd JvCreateProcess
  $(DCC) $&.dpr
  @cd ..

CsvDataDemo.exe: "JvCSVDataSet\CsvDataDemo.dpr"
  @cd JvCSVDataSet
  $(DCC) $&.dpr
  @cd ..

JvDataEmbeddedProj.exe: "JvDataEmbedded\JvDataEmbeddedProj.dpr"
  @cd JvDataEmbedded
  $(DCC) $&.dpr
  @cd ..

DBDTPDemo.exe: "JvDBDateTimePicker\DBDTPDemo.dpr"
  @cd JvDBDateTimePicker
  $(DCC) $&.dpr
  @cd ..

DBEXPL32.exe: "JvDBExplorer\DBEXPL32.DPR"
  @cd JvDBExplorer
  $(DCC) $&.DPR
  @cd ..

JvDBGridExportDemo.exe: "JvDBGridExport\JvDBGridExportDemo.dpr"
  @cd JvDBGridExport
  $(DCC) $&.dpr
  @cd ..

JvDbMaskEditDemo.exe: "JvDbMaskEdit\JvDbMaskEditDemo.dpr"
  @cd JvDbMaskEdit
  $(DCC) $&.dpr
  @cd ..

WebMapper.exe: "JvDiagramShape\1. WebSiteScanner\WebMapper.dpr"
  @cd JvDiagramShape\1. WebSiteScanner
  $(DCCH) $&.dpr
  @cd ..\..

UseCase.exe: "JvDiagramShape\2. UseCaseEditor\UseCase.dpr"
  @cd JvDiagramShape\2. UseCaseEditor
  $(DCCH) $&.dpr
  @cd ..\..

DepWalk.exe: "JvDiagramShape\3. DependencyWalker\DepWalk.dpr"
  @cd JvDiagramShape\3. DependencyWalker
  $(DCCH) $&.dpr
  @cd ..\..

JvDialogsDemo.exe: "JvDialogs\JvDialogsDemo.dpr"
  @cd JvDialogs
  $(DCC) $&.dpr
  @cd ..

AdvancePro.exe: "JvDocking\AdvanceDemo\Delphi_Source\AdvancePro.dpr"
  @cd JvDocking\AdvanceDemo\Delphi_Source
  $(DCCHH) $&.dpr
  @cd ..\..\..

DockOptionDemo.exe: "JvDocking\DockOptionDemo\Source\DockOptionDemo.dpr"
  @cd JvDocking\DockOptionDemo\Source
  $(DCCHH) $&.dpr
  @cd ..\..\..

HowToUse.exe: "JvDocking\HowtoUse\HowToUse.dpr"
  @cd JvDocking\HowtoUse
  $(DCCH) $&.dpr
  @cd ..\..

MSDN2002Pro.exe: "JvDocking\MSDN2002\Source\MSDN2002Pro.dpr"
  @cd JvDocking\MSDN2002\Source
  $(DCCHH) $&.dpr
  @cd ..\..\..

VCDemoPro.exe: "JvDocking\VCDemo\Source\VCDemoPro.dpr"
  @cd JvDocking\VCDemo\Source
  $(DCCHH) $&.dpr
  @cd ..\..\..

VIDDemoPro.exe: "JvDocking\VIDDemo\Source\VIDDemoPro.dpr"
  @cd JvDocking\VIDDemo\Source
  $(DCCHH) $&.dpr
  @cd ..\..\..

JvDomainUpDownDemo.exe: "JvDomainUpDown\JvDomainUpDownDemo.dpr"
  @cd JvDomainUpDown
  $(DCC) $&.dpr
  @cd ..

JvDotNetDemo.exe: "JvDotNetCtrls\JvDotNetDemo.dpr"
  @cd JvDotNetCtrls
  $(DCC) $&.dpr
  @cd ..

DSAExamples.exe: "JvDSADialogs\DSAExamples.dpr"
  @cd JvDSADialogs
  $(DCC) $&.dpr
  @cd ..

MessageDlgEditor.exe: "JvDSADialogs\MessageDlgEditor.dpr"
  @cd JvDSADialogs
  $(DCC) $&.dpr
  @cd ..

JvEdits.exe: "JvEdits\JvEdits.dpr"
  @cd JvEdits
  $(DCC) $&.dpr
  @cd ..

JvErrorIndicatorDemo.exe: "JvErrorIndicator\JvErrorIndicatorDemo.dpr"
  @cd JvErrorIndicator
  $(DCC) $&.dpr
  @cd ..

FileDirDemo.exe: "JvFileListBox\FileDirDemo.dpr"
  @cd JvFileListBox
  $(DCC) $&.dpr
  @cd ..

FindReplaceDemo.exe: "JvFindReplace\FindReplaceDemo.dpr"
  @cd JvFindReplace
  $(DCC) $&.dpr
  @cd ..

prjControls.exe: "JvFooterAndGroupHeader\prjControls.dpr"
  @cd JvFooterAndGroupHeader
  $(DCC) $&.dpr
  @cd ..

RxGIFAnm.exe: "JvGIFAnimator\RxGIFAnm.dpr"
  @cd JvGIFAnimator
  $(DCC) $&.dpr
  @cd ..

BasicDemo.exe: "JvHIDController\BasicDemo\BasicDemo.dpr"
  @cd JvHIDController\BasicDemo
  $(DCCH) $&.dpr
  @cd ..\..

CollectionDemo.exe: "JvHIDController\CollectionDemo\CollectionDemo.dpr"
  @cd JvHIDController\CollectionDemo
  $(DCCH) $&.dpr
  @cd ..\..

SimpleHIDWrite.exe: "JvHIDController\ReadWriteDemo\SimpleHIDWrite.dpr"
  @cd JvHIDController\ReadWriteDemo
  $(DCCH) $&.dpr
  @cd ..\..

UsagesDemo.exe: "JvHIDController\UsagesDemo\UsagesDemo.dpr"
  @cd JvHIDController\UsagesDemo
  $(DCCH) $&.dpr
  @cd ..\..

JvHtmlParserProj.exe: "JvHTMLParser\JvHtmlParserProj.dpr"
  @cd JvHTMLParser
  $(DCC) $&.dpr
  @cd ..

JvID3v1Demo.exe: "JvID3v1\JvID3v1Demo.dpr"
  @cd JvID3v1
  $(DCC) $&.dpr
  @cd ..

JvID3v2Demo.exe: "JvID3v2\JvID3v2Demo.dpr"
  @cd JvID3v2
  $(DCC) $&.dpr
  @cd ..

InspectorSimpleExample.exe: "JvInspector\InspectorSimpleExample.dpr"
  @cd JvInspector
  $(DCC) $&.dpr
  @cd ..

InspExample.exe: "JvInspector\InspExample.dpr"
  @cd JvInspector
  $(DCC) $&.dpr
  @cd ..

InspectorDBExample.exe: "JvInspectorDB\InspectorDBExample.dpr"
  @cd JvInspectorDB
  $(DCC) $&.dpr
  @cd ..

Install2LabelDemo.exe: "JvInstallLabel\Install2LabelDemo.dpr"
  @cd JvInstallLabel
  $(DCC) $&.dpr
  @cd ..

JvItemViewerDemo.exe: "JvItemViewer\JvItemViewerDemo.dpr"
  @cd JvItemViewer
  $(DCC) $&.dpr
  @cd ..

JvLinkLabelDemo.exe: "JvLinkLabel\JvLinkLabelDemo.dpr"
  @cd JvLinkLabel
  $(DCC) $&.dpr
  @cd ..

ListCombDemo.exe: "JvListComb\ListCombDemo.dpr"
  @cd JvListComb
  $(DCC) $&.dpr
  @cd ..

JvLogFileDemo.exe: "JvLogFile\JvLogFileDemo.dpr"
  @cd JvLogFile
  $(DCC) $&.dpr
  @cd ..

MailExample.exe: "JvMail\MailExample.dpr"
  @cd JvMail
  $(DCC) $&.dpr
  @cd ..

Balls.exe: "JvManagedThreads\Balls\Balls.dpr"
  @cd JvManagedThreads\Balls
  $(DCCH) $&.dpr
  @cd ..\..

Philosophers.exe: "JvManagedThreads\Philosophers\Philosophers.dpr"
  @cd JvManagedThreads\Philosophers
  $(DCCH) $&.dpr
  @cd ..\..

JvMarkupLabelDemo.exe: "JvMarkupLabel\JvMarkupLabelDemo.dpr"
  @cd JvMarkupLabel
  $(DCC) $&.dpr
  @cd ..

JvMenusExample.exe: "JvMenus\JvMenusExample.dpr"
  @cd JvMenus
  $(DCC) $&.dpr
  @cd ..

JvMouseGestureDemo.exe: "JvMouseGesture\JvMouseGestureDemo.dpr"
  @cd JvMouseGesture
  $(DCC) $&.dpr
  @cd ..

JvMruListDemo.exe: "JvMRUList\JvMruListDemo.dpr"
  @cd JvMRUList
  $(DCC) $&.dpr
  @cd ..

JvMultiHTTPGrabberDemo.exe: "JvMultiHTTPGrabber\JvMultiHTTPGrabberDemo.dpr"
  @cd JvMultiHTTPGrabber
  $(DCC) $&.dpr
  @cd ..

EventViewer.exe: "JvNTEventLog\EventViewer.dpr"
  @cd JvNTEventLog
  $(DCC) $&.dpr
  @cd ..

JvObjPickerDemo.exe: "JvObjectPicker\JvObjPickerDemo.dpr"
  @cd JvObjectPicker
  $(DCC) $&.dpr
  @cd ..

JvOLEDragCursorsDemo.exe: "JvOLEDragCursors\JvOLEDragCursorsDemo.dpr"
  @cd JvOLEDragCursors
  $(DCC) $&.dpr
  @cd ..

OLBarDemo.exe: "JvOutlookBar\OLBarDemo.dpr"
  @cd JvOutlookBar
  $(DCC) $&.dpr
  @cd ..

pm.exe: "JvPackageModify\pm.dpr"
  @cd JvPackageModify
  $(DCC) $&.dpr
  @cd ..

PageListDemo.exe: "JvPageListTreeView\PageListDemo.dpr"
  @cd JvPageListTreeView
  $(DCC) $&.dpr
  @cd ..

JvPanelDemo.exe: "JvPanel\JvPanelDemo.dpr"
  @cd JvPanel
  $(DCC) $&.dpr
  @cd ..

JvParameterListDemo.exe: "JvParameterList\JvParameterListDemo.dpr"
  @cd JvParameterList
  $(DCC) $&.dpr
  @cd ..

JvPlayListProj.exe: "JvPlayList\JvPlayListProj.dpr"
  @cd JvPlayList
  $(DCC) $&.dpr
  @cd ..

PlugInDemo.exe: "JvPlugin\1SimplePlugin\PlugInDemo.dpr"
  @cd JvPlugin\1SimplePlugin
  $(DCCH) $&.dpr
  @cd ..\..

SamplePluginOne.dll: "JvPlugin\1SimplePlugin\SamplePluginOne.dpr"
  @cd JvPlugin\1SimplePlugin
  $(DCCH) $&.dpr
  @cd ..\..

MDIPlugin.dll: "JvPlugin\2MDI\MDIPlugin.dpr"
  @cd JvPlugin\2MDI
  $(DCCH) $&.dpr
  @cd ..\..

MDISample.exe: "JvPlugin\2MDI\MDISample.dpr"
  @cd JvPlugin\2MDI
  $(DCCH) $&.dpr
  @cd ..\..

ChangePropertiesPlugin.dll: "JvPlugin\3ChangingProperties\ChangePropertiesPlugin.dpr"
  @cd JvPlugin\3ChangingProperties
  $(DCCH) $&.dpr
  @cd ..\..

PropertiesPlugInDemo.exe: "JvPlugin\3ChangingProperties\PropertiesPlugInDemo.dpr"
  @cd JvPlugin\3ChangingProperties
  $(DCCH) $&.dpr
  @cd ..\..

ExceptionPlugin.dll: "JvPlugin\4ApplicationHook\ExceptionPlugin.dpr"
  @cd JvPlugin\4ApplicationHook
  $(DCCH) $&.dpr
  @cd ..\..

ExceptionPlugInDemo.exe: "JvPlugin\4ApplicationHook\ExceptionPlugInDemo.dpr"
  @cd JvPlugin\4ApplicationHook
  $(DCCH) $&.dpr
  @cd ..\..

DataPlugin.dll: "JvPlugin\5DataAware\DataPlugin.dpr"
  @cd JvPlugin\5DataAware
  $(DCCH) $&.dpr
  @cd ..\..

DataPlugInDemo.exe: "JvPlugin\5DataAware\DataPlugInDemo.dpr"
  @cd JvPlugin\5DataAware
  $(DCCH) $&.dpr
  @cd ..\..

JvPlgMainApp.exe: "JvPlugin\6PluginPackage\PlgMainApp\JvPlgMainApp.dpr"
  @cd JvPlugin\6PluginPackage\PlgMainApp
  $(DCCHH) $&.dpr
  @cd ..\..\..

JvPrvwDemo.exe: "JvPreviewDocument\JvPrvwDemo.dpr"
  @cd JvPreviewDocument
  $(DCC) $&.dpr
  @cd ..

ProfilerDemo.exe: "JvProfiler32\ProfilerDemo.dpr"
  @cd JvProfiler32
  $(DCC) $&.dpr
  @cd ..

JvProgressDialogDemo.exe: "JvProgressDialog\JvProgressDialogDemo.dpr"
  @cd JvProgressDialog
  $(DCC) $&.dpr
  @cd ..

RegEditDemo.exe: "JvRegistryTreeView\RegEditDemo.dpr"
  @cd JvRegistryTreeView
  $(DCC) $&.dpr
  @cd ..

EditorDemo.exe: "JvRichEdit\EditorDemo.dpr"
  @cd JvRichEdit
  $(DCC) $&.dpr
  @cd ..

MessengerDemo.exe: "JvRichEdit\MessengerDemo.dpr"
  @cd JvRichEdit
  $(DCC) $&.dpr
  @cd ..

JvRollOutDemo.exe: "JvRollOut\JvRollOutDemo.dpr"
  @cd JvRollOut
  $(DCC) $&.dpr
  @cd ..

RunDLL32Demo.exe: "JvRunDll32\RunDLL32Demo.dpr"
  @cd JvRunDll32
  $(DCC) $&.dpr
  @cd ..

JvScreenCaptureProj.exe: "JvScreenCapture\JvScreenCaptureProj.dpr"
  @cd JvScreenCapture
  $(DCC) $&.dpr
  @cd ..

ScrollWinDemo.exe: "JvScrollingWindow\ScrollWinDemo.dpr"
  @cd JvScrollingWindow
  $(DCC) $&.dpr
  @cd ..

JvSearchFileProj.exe: "JvSearchFile\JvSearchFileProj.dpr"
  @cd JvSearchFile
  $(DCC) $&.dpr
  @cd ..

JvShellHookDemo.exe: "JvShellHook\JvShellHookDemo.dpr"
  @cd JvShellHook
  $(DCC) $&.dpr
  @cd ..

ShFileOpDemo.exe: "JvSHFileOperation\ShFileOpDemo.dpr"
  @cd JvSHFileOperation
  $(DCC) $&.dpr
  @cd ..

JvSimpleXMLDemo.exe: "JvSimpleXML\JvSimpleXMLDemo.dpr"
  @cd JvSimpleXML
  $(DCC) $&.dpr
  @cd ..

JvSimpleXMLEncodeDemo.exe: "JvSimpleXML Encode\JvSimpleXMLEncodeDemo.dpr"
  @cd JvSimpleXML Encode
  $(DCC) $&.dpr
  @cd ..

JvSimpleXMLSpeedTest.exe: "JvSimpleXML Encode\JvSimpleXMLSpeedTest.dpr"
  @cd JvSimpleXML Encode
  $(DCC) $&.dpr
  @cd ..

XMLValidator.exe: "JvSimpleXML Validator\XMLValidator.dpr"
  @cd JvSimpleXML Validator
  $(DCC) $&.dpr
  @cd ..

SpecialProgressTestPrj.exe: "JvSpecialProgress\SpecialProgressTestPrj.dpr"
  @cd JvSpecialProgress
  $(DCC) $&.dpr
  @cd ..

JvSpellCheckerDemo.exe: "JvSpellChecker\JvSpellCheckerDemo.dpr"
  @cd JvSpellChecker
  $(DCC) $&.dpr
  @cd ..

JvStringGridDemo.exe: "JvStringGrid\JvStringGridDemo.dpr"
  @cd JvStringGrid
  $(DCC) $&.dpr
  @cd ..

JvSystemPopupProj.exe: "JvSystemPopup\JvSystemPopupProj.dpr"
  @cd JvSystemPopup
  $(DCC) $&.dpr
  @cd ..

SystemPopupTest.exe: "JvSystemPopUp2\SystemPopupTest.dpr"
  @cd JvSystemPopUp2
  $(DCC) $&.dpr
  @cd ..

JvThreadProj.exe: "JvThread\JvThreadProj.dpr"
  @cd JvThread
  $(DCC) $&.dpr
  @cd ..

JvThumbnailDemo.exe: "JvThumbnail\JvThumbnailDemo.dpr"
  @cd JvThumbnail
  $(DCC) $&.dpr
  @cd ..

PhotoOp.exe: "JvTimeFrameWork\PhotoOp\PhotoOp.dpr"
  @cd JvTimeFrameWork\PhotoOp
  $(DCCH) $&.dpr
  @cd ..\..

TimeLineDemo.exe: "JvTimeline\TimeLineDemo.dpr"
  @cd JvTimeline
  $(DCC) $&.dpr
  @cd ..

TipsDemo.exe: "JvTipOfDay\TipsDemo.dpr"
  @cd JvTipOfDay
  $(DCC) $&.dpr
  @cd ..

SimpleTLTest1.exe: "JvTMTimeLine\SimpleTLTest1.dpr"
  @cd JvTMTimeLine
  $(DCC) $&.dpr
  @cd ..

JvTranslatorProj.exe: "JvTranslator\JvTranslatorProj.dpr"
  @cd JvTranslator
  $(DCC) $&.dpr
  @cd ..

TransparentButtonDemo.exe: "JvTransparentButton\TransparentButtonDemo.dpr"
  @cd JvTransparentButton
  $(DCC) $&.dpr
  @cd ..

JvTrayIconDemo.exe: "JvTrayIcon\JvTrayIconDemo.dpr"
  @cd JvTrayIcon
  $(DCC) $&.dpr
  @cd ..

JvTreeViewAsMenu.exe: "JvTreeViewAsMenu\JvTreeViewAsMenu.dpr"
  @cd JvTreeViewAsMenu
  $(DCC) $&.dpr
  @cd ..

api1.exe: "JvUIB\API\api1.dpr"
  @cd JvUIB\API
  $(DCCH) $&.dpr
  @cd ..\..

api10.exe: "JvUIB\API\api10.dpr"
  @cd JvUIB\API
  $(DCCH) $&.dpr
  @cd ..\..

api2.exe: "JvUIB\API\api2.dpr"
  @cd JvUIB\API
  $(DCCH) $&.dpr
  @cd ..\..

api3.exe: "JvUIB\API\api3.dpr"
  @cd JvUIB\API
  $(DCCH) $&.dpr
  @cd ..\..

api4.exe: "JvUIB\API\api4.dpr"
  @cd JvUIB\API
  $(DCCH) $&.dpr
  @cd ..\..

api5.exe: "JvUIB\API\api5.dpr"
  @cd JvUIB\API
  $(DCCH) $&.dpr
  @cd ..\..

api6.exe: "JvUIB\API\api6.dpr"
  @cd JvUIB\API
  $(DCCH) $&.dpr
  @cd ..\..

api7.exe: "JvUIB\API\api7.dpr"
  @cd JvUIB\API
  $(DCCH) $&.dpr
  @cd ..\..

api8.exe: "JvUIB\API\api8.dpr"
  @cd JvUIB\API
  $(DCCH) $&.dpr
  @cd ..\..

StartBackup.exe: "JvUIB\API\StartBackup.dpr"
  @cd JvUIB\API
  $(DCCH) $&.dpr
  @cd ..\..

StartRestore.exe: "JvUIB\API\StartRestore.dpr"
  @cd JvUIB\API
  $(DCCH) $&.dpr
  @cd ..\..

UIB.dll: "JvUIB\Automation\UIB.dpr"
  @cd JvUIB\Automation
  $(DCCH) $&.dpr
  @cd ..\..

Client.exe: "JvUIB\ClientServer\Client\Client.dpr"
  @cd JvUIB\ClientServer\Client
  $(DCCHH) $&.dpr
  @cd ..\..\..

Server.exe: "JvUIB\ClientServer\Server\Server.dpr"
  @cd JvUIB\ClientServer\Server
  $(DCCHH) $&.dpr
  @cd ..\..\..

Backup.exe: "JvUIB\Component\Backup\Backup.dpr"
  @cd JvUIB\Component\Backup
  $(DCCHH) $&.dpr
  @cd ..\..\..

BlobStream.exe: "JvUIB\Component\Blob\AsStream\BlobStream.dpr"
  @cd JvUIB\Component\Blob\AsStream
  $(DCCHHH) $&.dpr
  @cd ..\..\..\..

BlobSample.exe: "JvUIB\Component\Blob\AsString\BlobSample.dpr"
  @cd JvUIB\Component\Blob\AsString
  $(DCCHHH) $&.dpr
  @cd ..\..\..\..

cursor.exe: "JvUIB\Component\Cursor\cursor.dpr"
  @cd JvUIB\Component\Cursor
  $(DCCHH) $&.dpr
  @cd ..\..\..

DataPump.exe: "JvUIB\Component\DataPump\DataPump.dpr"
  @cd JvUIB\Component\DataPump
  $(DCCHH) $&.dpr
  @cd ..\..\..

MetaData.exe: "JvUIB\Component\Metadata\MetaData.dpr"
  @cd JvUIB\Component\Metadata
  $(DCCHH) $&.dpr
  @cd ..\..\..

Query.exe: "JvUIB\Component\Query\Query.dpr"
  @cd JvUIB\Component\Query
  $(DCCHH) $&.dpr
  @cd ..\..\..

QuickScript.exe: "JvUIB\Component\QuickScript\QuickScript.dpr"
  @cd JvUIB\Component\QuickScript
  $(DCCHH) $&.dpr
  @cd ..\..\..

Restore.exe: "JvUIB\Component\Restore\Restore.dpr"
  @cd JvUIB\Component\Restore
  $(DCCHH) $&.dpr
  @cd ..\..\..

Script.exe: "JvUIB\Component\Script\Script.dpr"
  @cd JvUIB\Component\Script
  $(DCCHH) $&.dpr
  @cd ..\..\..

StoredProc.exe: "JvUIB\Component\StoredProc\StoredProc.dpr"
  @cd JvUIB\Component\StoredProc
  $(DCCHH) $&.dpr
  @cd ..\..\..

QueryStream.exe: "JvUIB\Component\Stream\QueryStream.dpr"
  @cd JvUIB\Component\Stream
  $(DCCHH) $&.dpr
  @cd ..\..\..

#Query.exe: "JvUIB\Component\ThreadedQueries\Query.dpr"
#  @cd JvUIB\Component\ThreadedQueries
#  $(DCCHH) $&.dpr
#  @cd ..\..\..

JvUninstallCtrlsDemo.exe: "JvUninstallControls\JvUninstallCtrlsDemo.dpr"
  @cd JvUninstallControls
  $(DCC) $&.dpr
  @cd ..

JvUrlListGrabberDemo.exe: "JvUrlListGrabber\JvUrlListGrabberDemo.dpr"
  @cd JvUrlListGrabber
  $(DCC) $&.dpr
  @cd ..

JvFormatEditDemo.exe: "JvValidateEdit\JvFormatEditDemo.dpr"
  @cd JvValidateEdit
  $(DCC) $&.dpr
  @cd ..

JvValidatorsDemo.exe: "JvValidators\JvValidatorsDemo.dpr"
  @cd JvValidators
  $(DCC) $&.dpr
  @cd ..

JvWinDialogsDemo.exe: "JvWinDialogs\JvWinDialogsDemo.dpr"
  @cd JvWinDialogs
  $(DCC) $&.dpr
  @cd ..

JvWindowsTitleProj.exe: "JvWindowsTitle\JvWindowsTitleProj.dpr"
  @cd JvWindowsTitle
  $(DCC) $&.dpr
  @cd ..

WndProcHookDemo.exe: "JvWndProcHook\WndProcHookDemo.dpr"
  @cd JvWndProcHook
  $(DCC) $&.dpr
  @cd ..

Hospital.exe: "JvXMLDatabase\Sources\Hospital.dpr"
  @echo ---This demo requires Indy to be installed---
  @cd JvXMLDatabase\Sources
  -$(DCCH) $&.dpr
  @cd ..\..

WinXPBarDemo.exe: "JvXPControls\JvXPBar\WinXPBarDemo.dpr"
  @cd JvXPControls\JvXPBar
  $(DCCH) $&.dpr
  @cd ..\..

SimpleDemo.exe: "JvXPControls\Simple\SimpleDemo.dpr"
  @cd JvXPControls\Simple
  $(DCCH) $&.dpr
  @cd ..\..

JvZLibMultipleDemo.exe: "JvZLibMultiple\JvZLibMultipleDemo.dpr"
  @cd JvZLibMultiple
  $(DCC) $&.dpr
  @cd ..

JvZoomProj.exe: "JvZoom\JvZoomProj.dpr"
  @cd JvZoom
  $(DCC) $&.dpr
  @cd ..

LinkedConsumers.exe: "Providers and consumers\Linked consumers\LinkedConsumers.dpr"
  @cd Providers and consumers\Linked consumers
  $(DCCH) $&.dpr
  @cd ..\..

Pas2Rai2.exe: "RaLib\Pas2Rai2\Pas2Rai2.dpr"
  @cd RaLib\Pas2Rai2
  $(DCCH) $&.dpr
  @cd ..\..

RAControls.exe: "RaLib\RaControls\RAControls.dpr"
  @cd RaLib\RaControls
  $(DCCH) $&.dpr
  @cd ..\..

DBMove.exe: "RaLib\RaDBMove\DBMove.dpr"
  @cd RaLib\RaDBMove
  $(DCCH) $&.dpr
  @cd ..\..

DBTree.exe: "RaLib\RaDBTreeView\DBTree.dpr"
  @cd RaLib\RaDBTreeView
  $(DCCH) $&.dpr
  @cd ..\..

LineNumbers.exe: "RaLib\RaEditor\LineNumbers.dpr"
  @cd RaLib\RaEditor
  $(DCCH) $&.dpr
  @cd ..\..

RAEditorTest.exe: "RaLib\RaEditor\RAEditorTest.dpr"
  @cd RaLib\RaEditor
  $(DCCH) $&.dpr
  @cd ..\..

RAHLEdPropDlgTest.exe: "RaLib\RaEditor\RAHLEdPropDlgTest.dpr"
  @cd RaLib\RaEditor
  $(DCCH) $&.dpr
  @cd ..\..

ColorHintsTest.exe: "RaLib\RaHtHints\ColorHintsTest.dpr"
  @cd RaLib\RaHtHints
  $(DCCH) $&.dpr
  @cd ..\..

JvInterpreterTest.exe: "RaLib\RaInterpreter\JvInterpreterTest.dpr"
  @cd RaLib\RaInterpreter
  $(DCCH) $&.dpr
  @cd ..\..

SampleProject1.exe: "RaLib\RaInterpreter\samples\project1\SampleProject1.dpr"
  @cd RaLib\RaInterpreter\samples\project1
  $(DCCHHH) $&.dpr
  @cd ..\..\..\..

DynamicLoad.exe: "RaLib\RaInterpreterDynamicLoad\DynamicLoad.dpr"
  @cd RaLib\RaInterpreterDynamicLoad
  $(DCCH) $&.dpr
  @cd ..\..

JvInterpreterEndUser.exe: "RaLib\RaInterpreterEndUser\JvInterpreterEndUser.dpr"
  @cd RaLib\RaInterpreterEndUser
  $(DCCH) $&.dpr
  @cd ..\..

MDIapp.exe: "RaLib\RaInterpreterMDI\MDIapp.dpr"
  @cd RaLib\RaInterpreterMDI
  $(DCCH) $&.dpr
  @cd ..\..

RANotepad.exe: "RaLib\RaInterpreterNotepad\RANotepad.dpr"
  @cd RaLib\RaInterpreterNotepad
  $(DCCH) $&.dpr
  @cd ..\..

Rxdemo.exe: "RxLib\Rxdemo.dpr"
  @cd RxLib
  $(DCC) $&.dpr
  @cd ..
#---------------------------------------------------------------------------------------------------
.PHONY: clean
clean: 
  -@del /Q /f "..\dcu\*.dcu" "..\bin\*.exe" "..\bin\*.dll" "..\bin\*.bpl" "..\bin\*.dcp"
#---------------------------------------------------------------------------------------------------
