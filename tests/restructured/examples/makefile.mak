#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# JVCL Examples                                                                                     #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif
#---------------------------------------------------------------------------------------------------
SRC = ..\..\Source
DCU = ..\..\Dcu
BIN = ..\..\Bin
JCL = ..\..\..\JCL\source
DRC = $&.drc
#---------------------------------------------------------------------------------------------------
MAKE = $(ROOT)\bin\make.exe -$(MAKEFLAGS) -f$**
DCC = $(ROOT)\bin\dcc32.exe -e$(BIN) -i$(SRC) -i$(DCU) -n$(DCU) -r$(SRC) -q -u$(SRC) -u$(JCL) -w
DCCU = $(ROOT)\bin\dcc32.exe -e$(BIN) -i$(SRC) -i$(DCU) -n$(DCU) -q -w $**
BRCC = $(ROOT)\bin\brcc32.exe $**
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
JvOutlookPanelProj.exe \
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
JvTranslatorProj.exe
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

JvOutlookPanelProj.exe: JvOutlookPanel\JvOutlookPanelProj.dpr
  cd JvOutlookPanel
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

JvTranslatorProj.exe: JvTranslator\JvTranslatorProj.dpr
  cd JvTranslator
  $(DCC) $&.dpr
  cd ..
