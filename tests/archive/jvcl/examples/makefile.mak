#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# JVCL Examples                                                                                    #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)
!endif
#---------------------------------------------------------------------------------------------------
SRC = ..\..\Source
ARCH = ..\..\Archive
COM = ..\..\Common
BIN = ..\..\Bin
DCU = ..\..\Dcu
JCL = ..\..\..\JCL\source
DRC = $&.drc
SRCP = $(SRC);$(COM);$(JCL);$(ARCH);$(DCU)
SRCH = ..\$(SRC);..\$(COM);..\$(JCL);..\$(ARCH);..\$(DCU)
#---------------------------------------------------------------------------------------------------
MAKE = $(ROOT)\make.exe -$(MAKEFLAGS) -f$**
DCC  = $(ROOT)\dcc32.exe -e$(BIN) -i$(SRCP) -n$(DCU) -r$(SRCP) -u$(SRCP) -q -w -m
DCCH = $(ROOT)\dcc32.exe -e..\$(BIN) -i$(SRCH) -n..\$(DCU) -r$(SRCH) -u$(SRCH) -q -w -m
BRCC = $(ROOT)\brcc32.exe $**
#---------------------------------------------------------------------------------------------------
default: \
AppDdeCmdExample.exe \
ArrowButtonDemo.exe \
BmpAnimDemo.exe \
CaptionButtonDemo.exe \
ChangeNotifyDemo.exe \
ColorButtonDemo.exe \
ContentScrollerDemo.exe \
ControlsExample.exe \
CreateProcessExample.exe \
WebMapper.exe \
UseCase.exe \
DepWalk.exe \
DSAExamples.exe \
MessageDlgEditor.exe \
EnvironmentList.exe \
FileDirDemo.exe \
FindReplaceDemo.exe \
BasicDemo.exe \
CollectionDemo.exe \
SimpleHIDWrite.exe \
ImageWindowDemo.exe \
Install2LabelDemo.exe \
JvAniProj.exe \
JvAppHotKeyDemo.exe \
BalloonPrj.exe \
JvBrowserFolder.exe \
JvClipboardViewerProj.exe \
JVCLDemo.exe \
ColorComboDemo.exe \
JvDataEmbeddedProj.exe \
DBDTPDemo.exe \
JvDialogsDemo.exe \
JvEdits.exe \
prjControls.exe \
JvHtmlParserProj.exe \
InspectorDBExample.exe \
InspExample.exe \
JvID3v1Demo.exe \
JvLinkLabelDemo.exe \
JvMousePositionnerProj.exe \
JvMruList.exe \
EventViewer.exe \
JvPlayListProj.exe \
PlugInDemo.exe \
SamplePluginOne.dll \
MDIPlugin.dll \
MDISample.exe \
ChangePropertiesPlugin.dll \
PropertiesPlugInDemo.exe \
ExceptionPlugin.dll \
ExceptionPlugInDemo.exe \
DataPlugin.dll \
DataPlugInDemo.exe \
JvScreenCaptureProj.exe \
JvSearchFileProj.exe \
JvShellHookDemo.exe \
ShFileOpDemo.exe \
SpecialProgressTestPrj.exe \
JvSystemPopupProj.exe \
SystemPopupTest.exe \
JvThreadProj.exe \
JvThumbnailDemo.exe \
JvTranslatorProj.exe \
JvTreeViewAsMenu.exe \
JvWinDialogsDemo.exe \
JvWindowsTitleProj.exe \
JvZoomProj.exe \
WndProcHookDemo.exe \
ListCombDemo.exe \
MailExample.exe \
MonthCalendarDemo.exe \
JvObjPickerDemo.exe \
OLBarDemo.exe \
ProfilerDemo.exe \
RAControls.exe \
DBMove.exe \
LineNumbers.exe \
RAEditorTest.exe \
RAHLEdPropDlgTest.exe \
ColorHintsTest.exe \
JvInterpreterTest.exe \
JvInterpreterEndUser.exe \
MDIapp.exe \
RANotepad.exe \
RegEditDemo.exe \
RunDLL32Demo.exe \
DBEXPL32.exe \
Rxdemo.exe \
RxGIFAnm.exe \
ScrollWinDemo.exe \
TimeLineDemo.exe \
TipsDemo.exe \
SimpleTLTest1.exe \
TransparentButtonDemo.exe \
JvLogFileDemo.exe \
JvTrayIconDemo.exe \
JvZLibMultipleDemo.exe \
JvFormatEditDemo.exe \
JvProgressDialogDemo.exe \

#---------------------------------------------------------------------------------------------------

AppDdeCmdExample.exe: AppDdeCmdExample\AppDdeCmdExample.dpr
  cd AppDdeCmdExample
  $(DCC) $&.dpr
  cd ..

ArrowButtonDemo.exe: ArrowButton\ArrowButtonDemo.dpr
  cd ArrowButton
  $(DCC) $&.dpr
  cd ..

BmpAnimDemo.exe: BmpAnim\BmpAnimDemo.dpr
  cd BmpAnim
  $(DCC) $&.dpr
  cd ..

CaptionButtonDemo.exe: CaptionBtn\CaptionButtonDemo.dpr
  cd CaptionBtn
  $(DCC) $&.dpr
  cd ..

ChangeNotifyDemo.exe: ChangeNotification\ChangeNotifyDemo.dpr
  cd ChangeNotification
  $(DCC) $&.dpr
  cd ..

ColorButtonDemo.exe: ColorBtn\ColorButtonDemo.dpr
  cd ColorBtn
  $(DCC) $&.dpr
  cd ..

ContentScrollerDemo.exe: ContentScroller\ContentScrollerDemo.dpr
  cd ContentScroller
  $(DCC) $&.dpr
  cd ..

ControlsExample.exe: ControlsExample\ControlsExample.dpr
  cd ControlsExample
  $(DCC) $&.dpr
  cd ..

CreateProcessExample.exe: CreateProcessExample\CreateProcessExample.dpr
  cd CreateProcessExample
  $(DCC) $&.dpr
  cd ..

WebMapper.exe: Diagram1WebSiteScanner\WebMapper.dpr
  cd Diagram1WebSiteScanner
  $(DCC) $&.dpr
  cd ..

UseCase.exe: Diagram2UseCaseEditor\UseCase.dpr
  cd Diagram2UseCaseEditor
  $(DCC) $&.dpr
  cd ..

DepWalk.exe: Diagram3DependencyWalker\DepWalk.dpr
  cd Diagram3DependencyWalker
  $(DCC) $&.dpr
  cd ..

DSAExamples.exe: DSADialogs\DSAExamples.dpr
  cd DSADialogs
  $(DCC) $&.dpr
  cd ..

MessageDlgEditor.exe: DSADialogs\MessageDlgEditor.dpr
  cd DSADialogs
  $(DCC) $&.dpr
  cd ..

EnvironmentList.exe: EnvironmentList\EnvironmentList.dpr
  cd EnvironmentList
  $(DCC) $&.dpr
  cd ..

FileDirDemo.exe: FileListBox\FileDirDemo.dpr
  cd FileListBox
  $(DCC) $&.dpr
  cd ..

FindReplaceDemo.exe: FindReplace\FindReplaceDemo.dpr
  cd FindReplace
  $(DCC) $&.dpr
  cd ..

BasicDemo.exe: HID\BasicDemo\BasicDemo.dpr
  cd HID\BasicDemo
  $(DCCH) $&.dpr
  cd ..\..

CollectionDemo.exe: HID\CollectionDemo\CollectionDemo.dpr
  cd HID\CollectionDemo
  $(DCCH) $&.dpr
  cd ..\..

SimpleHIDWrite.exe: HID\ReadWriteDemo\SimpleHIDWrite.dpr
  cd HID\ReadWriteDemo
  $(DCCH) $&.dpr
  cd ..\..

ImageWindowDemo.exe: ImageWindow\ImageWindowDemo.dpr
  cd ImageWindow
  $(DCC) $&.dpr
  cd ..

Install2LabelDemo.exe: InstallLabel\Install2LabelDemo.dpr
  cd InstallLabel
  $(DCC) $&.dpr
  cd ..

JvAniProj.exe: JvAni\JvAniProj.dpr
  cd JvAni
  $(DCC) $&.dpr
  cd ..

JvAppHotKeyDemo.exe: JvAppHotKeyDemo\JvAppHotKeyDemo.dpr
  cd JvAppHotKeyDemo
  $(DCC) $&.dpr
  cd ..

BalloonPrj.exe: JvBalloonHint\BalloonPrj.dpr
  cd JvBalloonHint
  $(DCC) $&.dpr
  cd ..

JvBrowserFolder.exe: JvBrowseFolder\JvBrowserFolder.dpr
  cd JvBrowseFolder
  $(DCC) $&.dpr
  cd ..

JvClipboardViewerProj.exe: JvClipboardViewer\JvClipboardViewerProj.dpr
  cd JvClipboardViewer
  $(DCC) $&.dpr
  cd ..

JVCLDemo.exe: JVCLMegaDemo\JVCLDemo.dpr
  cd JVCLMegaDemo
  $(DCC) $&.dpr
  cd ..

ColorComboDemo.exe: JvColorComboDemo\ColorComboDemo.dpr
  cd JvColorComboDemo
  $(DCC) $&.dpr
  cd ..

JvDataEmbeddedProj.exe: JvDataEmbedded\JvDataEmbeddedProj.dpr
  cd JvDataEmbedded
  $(DCC) $&.dpr
  cd ..

DBDTPDemo.exe: JvDBDateTimePicker\DBDTPDemo.dpr
  cd JvDBDateTimePicker
  $(DCC) $&.dpr
  cd ..

JvDialogsDemo.exe: JvDialogs\JvDialogsdemo.dpr
  cd JvDialogs
  $(DCC) $&.dpr
  cd ..

JvEdits.exe: JvEdits\JvEdits.dpr
  cd JvEdits
  $(DCC) $&.dpr
  cd ..

prjControls.exe: JvFooterAndGroupHeader\prjControls.dpr
  cd JvFooterAndGroupHeader
  $(DCC) $&.dpr
  cd ..

JvHtmlParserProj.exe: JvHTMLParser\JvHtmlParserProj.dpr
  cd JvHTMLParser
  $(DCC) $&.dpr
  cd ..

InspectorDBExample.exe: JvInspectorDBDemo\InspectorDBExample.dpr
  cd JvInspectorDBDemo
  $(DCC) $&.dpr
  cd ..

JvID3v1Demo.exe: JvID3v1\JvID3v1Demo.dpr
  cd JvID3v1
  $(DCC) $&.dpr
  cd ..

InspExample.exe: JvInspectorDemo\InspExample.dpr
  cd JvInspectorDemo
  $(DCC) $&.dpr
  cd ..

JvLinkLabelDemo.exe: JvLinkLabel\JvLinkLabelDemo.dpr
  cd JvLinkLabel
  $(DCC) $&.dpr
  cd ..

JvMousePositionnerProj.exe: JvMousePositionner\JvMousePositionnerProj.dpr
  cd JvMousePositionner
  $(DCC) $&.dpr
  cd ..

JvMruList.exe: JvMruList\JvMruList.dpr
  cd JvMruList
  $(DCC) $&.dpr
  cd ..

EventViewer.exe: JvNTEventLog\EventViewer.dpr
  cd JvNTEventLog
  $(DCC) $&.dpr
  cd ..

JvPlayListProj.exe: JvPlayList\JvPlayListProj.dpr
  cd JvPlayList
  $(DCC) $&.dpr
  cd ..

PlugInDemo.exe: JvPlugin\1SimplePlugin\PlugInDemo.dpr
  cd JvPlugin\1SimplePlugin
  $(DCCH) $&.dpr
  cd ..\..

SamplePluginOne.dll: JvPlugin\1SimplePlugin\SamplePluginOne.dpr
  cd JvPlugin\1SimplePlugin
  $(DCCH) $&.dpr
  cd ..\..

MDIPlugin.dll: JvPlugin\2MDI\MDIPlugin.dpr
  cd JvPlugin\2MDI
  $(DCCH) $&.dpr
  cd ..\..

MDISample.exe: JvPlugin\2MDI\MDISample.dpr
  cd JvPlugin\2MDI
  $(DCCH) $&.dpr
  cd ..\..

ChangePropertiesPlugin.dll: JvPlugin\3ChangingProperties\ChangePropertiesPlugin.dpr
  cd JvPlugin\3ChangingProperties
  $(DCCH) $&.dpr
  cd ..\..

PropertiesPlugInDemo.exe: JvPlugin\3ChangingProperties\PropertiesPlugInDemo.dpr
  cd JvPlugin\3ChangingProperties
  $(DCCH) $&.dpr
  cd ..\..

ExceptionPlugin.dll: JvPlugin\4ApplicationHook\ExceptionPlugin.dpr
  cd JvPlugin\4ApplicationHook
  $(DCCH) $&.dpr
  cd ..\..

ExceptionPlugInDemo.exe: JvPlugin\4ApplicationHook\ExceptionPlugInDemo.dpr
  cd JvPlugin\4ApplicationHook
  $(DCCH) $&.dpr
  cd ..\..

DataPlugin.dll: JvPlugin\5DataAware\DataPlugin.dpr
  cd JvPlugin\5DataAware
  $(DCCH) $&.dpr
  cd ..\..

DataPlugInDemo.exe: JvPlugin\5DataAware\DataPlugInDemo.dpr
  cd JvPlugin\5DataAware
  $(DCCH) $&.dpr
  cd ..\..

JvScreenCaptureProj.exe: JvScreenCapture\JvScreenCaptureProj.dpr
  cd JvScreenCapture
  $(DCC) $&.dpr
  cd ..

JvSearchFileProj.exe: JvSearchFile\JvSearchFileProj.dpr
  cd JvSearchFile
  $(DCC) $&.dpr
  cd ..

JvShellHookDemo.exe: JvShellHookDemo\JvShellHookDemo.dpr
  cd JvShellHookDemo
  $(DCC) $&.dpr
  cd ..

ShFileOpDemo.exe: JvShFileOperation\ShFileOpDemo.dpr
  cd JvShFileOperation
  $(DCC) $&.dpr
  cd ..

SpecialProgressTestPrj.exe: JvSpecialProgress\SpecialProgressTestPrj.dpr
  cd JvSpecialProgress
  $(DCC) $&.dpr
  cd ..

JvSystemPopupProj.exe: JvSystemPopup\JvSystemPopupProj.dpr
  cd JvSystemPopup
  $(DCC) $&.dpr
  cd ..

SystemPopupTest.exe: JvSystemPopUp2\SystemPopupTest.dpr
  cd JvSystemPopUp2
  $(DCC) $&.dpr
  cd ..

JvThreadProj.exe: JvThread\JvThreadProj.dpr
  cd JvThread
  $(DCC) $&.dpr
  cd ..

JvThumbnailDemo.exe: JvThumbnail\JvThumbnailDemo.dpr
  cd JvThumbnail
  $(DCC) $&.dpr
  cd ..

JvTranslatorProj.exe: JvTranslator\JvTranslatorProj.dpr
  cd JvTranslator
  $(DCC) $&.dpr
  cd ..

JvTreeViewAsMenu.exe: JvTreeViewAsMenu\JvTreeViewAsMenu.dpr
  cd JvTreeViewAsMenu
  $(DCC) $&.dpr
  cd ..

JvWinDialogsDemo.exe: JvWinDialogs\JvWinDialogsDemo.dpr
  cd JvWinDialogs
  $(DCC) $&.dpr
  cd ..

JvWindowsTitleProj.exe: JvWindowsTitle\JvWindowsTitleProj.dpr
  cd JvWindowsTitle
  $(DCC) $&.dpr
  cd ..

JvZoomProj.exe: JvZoom\JvZoomProj.dpr
  cd JvZoom
  $(DCC) $&.dpr
  cd ..

WndProcHookDemo.exe: JvWndProcHookDemo\WndProcHookDemo.dpr
  cd JvWndProcHookDemo
  $(DCC) $&.dpr
  cd ..

ListCombDemo.exe: ListComb\ListCombDemo.dpr
  cd ListComb
  $(DCC) $&.dpr
  cd ..

MailExample.exe: MailExample\MailExample.dpr
  cd MailExample
  $(DCC) $&.dpr
  cd ..

MonthCalendarDemo.exe: MonthCalendar\MonthCalendarDemo.dpr
  cd MonthCalendar
  $(DCC) $&.dpr
  cd ..

JvObjPickerDemo.exe: ObjectPickerDemo\JvObjPickerDemo.dpr
  cd ObjectPickerDemo
  $(DCC) $&.dpr
  cd ..

OLBarDemo.exe: OLBar\OLBarDemo.dpr
  cd OLBar
  $(DCC) $&.dpr
  cd ..

ProfilerDemo.exe: Profiler32\ProfilerDemo.dpr
  cd Profiler32
  $(DCC) $&.dpr
  cd ..

RAControls.exe: RALib\RaControls\RAControls.dpr
  cd RALib\RaControls
  $(DCCH) $&.dpr
  cd ..\..

DBMove.exe: RALib\RaDBMove\DBMove.dpr
  cd RALib\RaDBMove
  $(DCCH) $&.dpr
  cd ..\..

LineNumbers.exe: RALib\RaEditor\LineNumbers.dpr
  cd RALib\RaEditor
  $(DCCH) $&.dpr
  cd ..\..

RAEditorTest.exe: RALib\RaEditor\RAEditorTest.dpr
  cd RALib\RaEditor
  $(DCCH) $&.dpr
  cd ..\..

RAHLEdPropDlgTest.exe: RALib\RaEditor\RAHLEdPropDlgTest.dpr
  cd RALib\RaEditor
  $(DCCH) $&.dpr
  cd ..\..

ColorHintsTest.exe: RALib\RaHtHints\ColorHintsTest.dpr
  cd RALib\RaHtHints
  $(DCCH) $&.dpr
  cd ..\..

JvInterpreterTest.exe: RALib\RaInterpreter\JvInterpreterTest.dpr
  cd RALib\RaInterpreter
  $(DCCH) $&.dpr
  cd ..\..

JvInterpreterEndUser.exe: RALib\RaInterpreterEndUser\JvInterpreterEndUser.dpr
  cd RALib\RaInterpreterEndUser
  $(DCCH) $&.dpr
  cd ..\..

MDIapp.exe: RALib\RaInterpreterMDI\MDIapp.dpr
  cd RALib\RaInterpreterMDI
  $(DCCH) $&.dpr
  cd ..\..

RANotepad.exe: RALib\RaInterpreterNotepad\RANotepad.dpr
  cd RALib\RaInterpreterNotepad
  $(DCCH) $&.dpr
  cd ..\..

RegEditDemo.exe: RegTV\RegEditDemo.dpr
  cd RegTV
  $(DCC) $&.dpr
  cd ..

RunDLL32Demo.exe: RunDll32\RunDLL32Demo.dpr
  cd RunDll32
  $(DCC) $&.dpr
  cd ..

DBEXPL32.exe: RxDBExplorer\DBEXPL32.DPR
  cd RxDBExplorer
  $(DCC) $&.dpr
  cd ..

Rxdemo.exe: RxDemo\Rxdemo.dpr
  cd RxDemo
  $(DCC) $&.dpr
  cd ..

RxGIFAnm.exe: RxGIFAnimator\RxGIFAnm.dpr
  cd RxGIFAnimator
  $(DCC) $&.dpr
  cd ..

ScrollWinDemo.exe: ScrollWin\ScrollWinDemo.dpr
  cd ScrollWin
  $(DCC) $&.dpr
  cd ..

TimeLineDemo.exe: Timeline\TimeLineDemo.dpr
  cd Timeline
  $(DCC) $&.dpr
  cd ..

TipsDemo.exe: TipOfDay\TipsDemo.dpr
  cd TipOfDay
  $(DCC) $&.dpr
  cd ..

SimpleTLTest1.exe: TMTimeLine\SimpleTLTest1.dpr
  cd TMTimeLine
  $(DCC) $&.dpr
  cd ..

TransparentButtonDemo.exe: TransBtn\TransparentButtonDemo.dpr
  cd TransBtn
  $(DCC) $&.dpr
  cd ..

JvLogFileDemo.exe: JvLogFile\JvLogFileDemo.dpr
  cd JvLogFile
  $(DCC) $&.dpr
  cd ..
JvTrayIconDemo.exe: JvTrayIcon\JvTrayIconDemo.dpr
  cd JvTrayIcon
  $(DCC) $&.dpr
  cd ..

JvZLibMultipleDemo.exe: JvZLibMultiple\JvZLibMultipleDemo.dpr
  cd JvZLibMultiple
  $(DCC) $&.dpr
  cd ..

JvFormatEditDemo.exe: JvValidateEdit\JvFormatEditDemo.dpr
  cd JvValidateEdit
  $(DCC) $&.dpr
  cd ..

JvProgressDialogDemo.exe: JvProgressDialog\JvProgressDialogDemo.dpr
  cd JvProgressDialog
  $(DCC) $&.dpr
  cd ..
