#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# JVCL Examples                                                                                     #
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
ArrowButtonDemo.exe \
BmpAnimDemo.exe \
CaptionButtonDemo.exe \
ChangeNotifyDemo.exe \
ColorButtonDemo.exe \
ContentScrollerDemo.exe \
DBDTPDemo.exe \
FileDirDemo.exe \
FindReplaceDemo.exe \
ImageWindowDemo.exe \
Install2LabelDemo.exe \
JvAniProj.exe \
JvBrowserFolder.exe \
JvClipboardViewerProj.exe \
JvDataEmbeddedProj.exe \
JvDialogs.exe \
JvEdits.exe \
JvHtmlParserProj.exe \
JvLinkLabelDemo.exe \
JvMousePositionnerProj.exe \
JvMruList.exe \
JvPlayListProj.exe \
JvScreenCaptureProj.exe \
JvSearchFileProj.exe \
JvSystemPopupProj.exe \
JvThreadProj.exe \
JvTreeViewAsMenu.exe \
JvWinDialogsDemo.exe \
JvWindowsTitleProj.exe \
JvZoomProj.exe \
ListCombDemo.exe \
MonthCalendarDemo.exe \
OLBarDemo.exe \
ProfilerDemo.exe \
RegEditDemo.exe \
ScrollWinDemo.exe \
SimpleTLTest1.exe \
TimeLineDemo.exe \
TipsDemo.exe \
TransparentButtonDemo.exe \
UseCase.exe \
WebMapper.exe \
AppDdeCmdExample.exe \
ControlsExample.exe \
CreateProcessExample.exe \ 
EvnironmentList.exe \
MailExample.exe \
DBexpl32.exe \
RxDemo.exe \
RxGIFAnm.exe \
JvTranslatorProj.exe \
JvThumbnailDemo.exe \
BasicDemo.exe \
CollectionDemo.exe \
SimpleHIDWrite.exe \
GreyMouser.exe \
InspectorDBExample.exe \
InspExample.exe \
JvOutlookPanelProj.exe \
RunDLL32Demo.exe \
SpecialProgressTestPrj.exe \
SystemPopupTest.exe \
ShFileOpDemo.exe \
RAControls.exe \
DBMove.exe \
DBTree.exe \
RAEditorTest.exe \
LineNumbers.exe \
ColorHintsTest.exe \
JvInterpreterTest.exe \
JvInterpreterEndUser.exe \
MDIapp.exe \
RANotepad.exe \
JVCLDemo.exe \
prjControls.exe \
EventViewer.exe

 
#---------------------------------------------------------------------------------------------------

ArrowButtonDemo.exe: ArrowButton\ArrowButtonDemo.dpr
  cd ArrowButton
  $(DCC) $&.dpr
  cd ..

BmpAnimDemo.exe: BMPAnim\BmpAnimDemo.dpr
  cd BMPAnim
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

DBDTPDemo.exe: JvDBDateTimePicker\DBDTPDemo.dpr
  cd JvDBDateTimePicker
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

ImageWindowDemo.exe: ImageWindow\ImageWindowDemo.dpr
  cd ImageWindow
  $(DCC) $&.dpr
  cd ..

InstallLabelDemo.exe: InstallLabel\InstallLabelDemo.dpr
  cd InstallLabel
  $(DCC) $&.dpr
  cd ..

JvAniProj.exe: JvAni\JvAniProj.dpr
  cd JvAni
  $(DCC) $&.dpr
  cd ..

JvBrowserFolder.exe: JvBrowseFolder\JvBrowserFolder.dpr
  cd JvBrowseFolder
  $(DCC) $&.dpr
  cd ..

Install2LabelDemo.exe: InstallLabel\Install2LabelDemo.dpr
  cd InstallLabel
  $(DCC) $&.dpr
  cd ..

JvClipboardViewerProj.exe: JvClipboardViewer\JvClipboardViewerProj.dpr
  cd JvClipboardViewer
  $(DCC) $&.dpr
  cd ..

JvDataEmbeddedProj.exe: JvDataEmbedded\JvDataEmbeddedProj.dpr
  cd JvDataEmbedded
  $(DCC) $&.dpr
  cd ..

JvDialogs.exe: JvDialogs\JvDialogs.dpr
  cd JvDialogs
  $(DCC) $&.dpr
  cd ..

JvEdits.exe: JvEdits\JvEdits.dpr
  cd JvEdits
  $(DCC) $&.dpr
  cd ..

JvHTMLParserProj.exe: JvHTMLParser\JvHTMLParserProj.dpr
  cd JvHTMLParser
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

JvMruList.exe: JvMruList/JvMruList.dpr
  cd JvMruList
  $(DCC) $&.dpr
  cd ..

JvPlayListProj.exe: JvPlayList\JvPlayListProj.dpr
  cd JvPlayList
  $(DCC) $&.dpr
  cd ..

JvScreenCaptureProj.exe: JvScreenCapture\JvScreenCaptureProj.dpr
  cd JvScreenCapture
  $(DCC) $&.dpr
  cd ..

JvSearchFileProj.exe: JvSearchFile/JvSearchFileProj.dpr
  cd JvSearchFile
  $(DCC) $&.dpr
  cd ..

JvSystemPopupProj.exe: JvSystemPopup\JvSystemPopupProj.dpr
  cd JvSystemPopup
  $(DCC) $&.dpr
  cd ..

JvThreadProj.exe: JvThread\JvThreadProj.dpr
  cd  JvThread
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

ListCombDemo.exe: ListComb\ListCombDemo.dpr
  cd ListComb
  $(DCC) $&.dpr
  cd ..

MonthCalendarDemo.exe: MonthCalendar\MonthCalendarDemo.dpr
  cd MonthCalendar
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

RegEditDemo.exe: RegTV\RegEditDemo.dpr
  cd RegTV
  $(DCC) $&.dpr
  cd ..

ScrollWinDemo.exe: ScrollWin\ScrollWinDemo.dpr
  cd ScrollWin
  $(DCC) $&.dpr
  cd ..

SimpleTLTest1.exe: TMTimeLine\SimpleTLTest1.dpr
  cd TMTimeLine
  $(DCC) $&.dpr
  cd ..

TimeLineDemo.exe: TimeLine\TimeLineDemo.dpr
  cd TimeLine
  $(DCC) $&.dpr
  cd ..

TipsDemo.exe: TipOfDay\TipsDemo.dpr
  cd TipOfDay
  $(DCC) $&.dpr
  cd ..

TransparentButtonDemo.exe: TransBtn\TransparentButtonDemo.dpr
  cd TransBtn
  $(DCC) $&.dpr
  cd ..

UseCase.exe: Diagram2UseCaseEditor\UseCase.dpr
  cd Diagram2UseCaseEditor
  $(DCC) $&.dpr
  cd ..

WebMapper.exe: Diagram1WebSiteScanner\WebMapper.dpr
  cd Diagram1WebSiteScanner
  $(DCC) $&.dpr
  cd ..

AppDdeCmdExample.exe: AppDdeCmdExample\AppDdeCmdExample.dpr
  cd AppDdeCmdExample
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
  
EvnironmentList.exe: EvnironmentList\EvnironmentList.dpr
  cd EvnironmentList
  $(DCC) $&.dpr
  cd ..
  
MailExample.exe: MailExample\MailExample.dpr
  cd MailExample
  $(DCC) $&.dpr
  cd ..

DBexpl32.exe: RxDBExplorer\DBexpl32.dpr
  cd RxDBExplorer
  $(DCC) $&.dpr
  cd ..

RxDemo.exe: RxDemo\RxDemo.dpr
  cd RxDemo
  $(DCC) $&.dpr
  cd ..

RxGIFAnm.exe: RxGIFAnimator\RxGIFAnm.dpr
  cd RxGIFAnimator
  $(DCC) $&.dpr
  cd ..

JvTranslatorProj.exe: JvTranslator\JvTranslatorProj.dpr
  cd JvTranslator
  $(DCC) $&.dpr
  cd ..

JvThumbnailDemo.exe: JvThumbnail\JvThumbnailDemo.dpr
  cd JvThumbnail
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

GreyMouser.exe: HID\ThreadDemo\GreyMouser.dpr
  cd HID\ThreadDemo
  $(DCCH) $&.dpr
  cd ..\..

InspectorDBExample.exe: JvInspectorDBDemo\InspectorDBExample.dpr
  cd JvInspectorDBDemo
  $(DCC) $&.dpr
  cd ..

InspExample.exe: JvInspectorDemo\InspExample.dpr
  cd JvInspectorDemo
  $(DCC) $&.dpr
  cd ..

JvOutlookPanelProj.exe: JvOutlookPanel\JvOutlookPanelProj.dpr
  cd JvOutlookPanel
  $(DCC) $&.dpr
  cd ..

RunDLL32Demo.exe: RunDll32\RunDLL32Demo.dpr
  cd RunDll32
  $(DCC) $&.dpr
  cd ..

SpecialProgressTestPrj.exe: JvSpecialProgress\SpecialProgressTestPrj.dpr
  cd JvSpecialProgress
  $(DCC) $&.dpr
  cd ..

SystemPopupTest.exe: JvSystemPopUp2\SystemPopupTest.dpr
  cd JvSystemPopUp2
  $(DCC) $&.dpr
  cd ..

ShFileOpDemo.exe: JvShFileOperation\ShFileOpDemo.dpr
  cd JvShFileOperation
  $(DCC) $&.dpr
  cd ..

RAControls.exe: RaLib\RaControls\RAControls.dpr
  cd RaLib\RaControls
  $(DCCH) $&.dpr
  cd ..\..

DBMove.exe: RaLib\RaDBMove\DBMove.dpr
  cd RaLib\RaDBMove
  $(DCCH) $&.dpr
  cd ..\..
  
DBTree.exe: RaLib\RaDBTreeView\DBTree.dpr
  cd RaLib\RaDBTreeView
  $(DCCH) $&.dpr
  cd ..\..

RAEditorTest.exe: RaLib\RaEditor\RAEditorTest.dpr
  cd RaLib\RaEditor
  $(DCCH) $&.dpr
  cd ..\..

LineNumbers.exe: RaLib\RaEditor\LineNumbers.dpr
  cd RaLib\RaEditor
  $(DCCH) $&.dpr
  cd ..\..
  
ColorHintsTest.exe: RaLib\RaHtHints\ColorHintsTest.dpr
  cd RaLib\RaHtHints
  $(DCCH) $&.dpr
  cd ..\..

JvInterpreterTest.exe: RaLib\RaInterpreter\JvInterpreterTest.dpr
  cd RaLib\RaInterpreter
  $(DCCH) $&.dpr
  cd ..\..

DynamicLoad.exe: RaLib\RaInterpreterDynamicLoad\DynamicLoad.dpr
  cd RaLib\RaInterpreterDynamicLoad
  $(DCCH) $&.dpr
  cd ..\..

JvInterpreterEndUser.exe: RaLib\RaInterpreterEndUser\JvInterpreterEndUser.dpr
  cd RaLib\RaInterpreterEndUser
  $(DCCH) $&.dpr
  cd ..\..

MDIapp.exe: RaLib\RaInterpreterMDI\MDIapp.dpr
  cd RaLib\RaInterpreterMDI
  $(DCCH) $&.dpr
  cd ..\..

RANotepad.exe: RaLib\RaInterpreterNotepad\RANotepad.dpr
  cd RaLib\RaInterpreterNotepad
  $(DCCH) $&.dpr
  cd ..\..

JVCLDemo.exe: JVCLMegaDemo\JVCLDemo.dpr
  cd JVCLMegaDemo
  $(DCC) $&.dpr
  cd ..
  
prjControls.exe: JVFooterAndGroupHeader\prjControls.dpr
  cd JVFooterAndGroupHeader
  $(DCC) $&.dpr
  cd ..

EventViewer.exe: JvNTEventLog\EventViewer.dpr
  cd JvNTEventLog
  $(DCC) $&.dpr
  cd ..
  
  
