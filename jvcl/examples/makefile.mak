#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# JVCL Examples                                                                                    #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)
!endif
#---------------------------------------------------------------------------------------------------
RUN = ..\..\run
DSGN = ..\..\design
ARCH = ..\..\archive
COM = ..\..\common
BIN = ..\..\bin
DCU = ..\..\dcu
JCL = ..\..\..\..\JCL\source;..\..\..\..\JCL\source\common;..\..\..\..\JCL\source\windows;..\..\..\..\JCL\source\vcl
DRC = $&.drc
SRCP = $(RUN);$(DSGN);$(COM);$(JCL);$(ARCH);$(DCU)
SRCH = ..\$(RUN);..\$(DSGN);..\$(COM);..\$(JCL);..\$(ARCH);..\$(DCU)
SRCHH = ..\..\$(RUN);..\..\$(DSGN);..\..\$(COM);..\..\$(JCL);..\..\$(ARCH);..\..\$(DCU)
SRCHHH = ..\..\..\$(RUN);..\..\..\$(DSGN);..\..\..\$(COM);..\..\..\$(JCL);..\..\..\$(ARCH);..\..\..\$(DCU)
#---------------------------------------------------------------------------------------------------
MAKE = $(ROOT)\make.exe -$(MAKEFLAGS) -f$**
DCC  = $(ROOT)\dcc32.exe -e$(BIN) -i$(SRCP) -n$(DCU) -r$(SRCP) -u$(SRCP) -q -w -m
DCCH = $(ROOT)\dcc32.exe -e..\$(BIN) -i$(SRCH) -n..\$(DCU) -r$(SRCH) -u$(SRCH) -q -w -m
DCCHH = $(ROOT)\dcc32.exe -e..\..\$(BIN) -i$(SRCHH) -n..\..\$(DCU) -r$(SRCHH) -u$(SRCHH) -q -w -m
DCCHHH = $(ROOT)\dcc32.exe -e..\..\..\$(BIN) -i$(SRCHHH) -n..\..\..\$(DCU) -r$(SRCHHH) -u$(SRCHHH) -q -w -m
BRCC = $(ROOT)\brcc32.exe $**
#---------------------------------------------------------------------------------------------------
default: \
EditorDemo.exe \
MessengerDemo.exe \
Philosophers.exe \
Balls.exe \
JvAppInstDemo.exe \
JvDbMaskEditDemo.exe \
JvComboListBoxDemo.exe \
JvChartDemo.exe \
JvErrorIndicatorDemo.exe \
JvSpellCheckerDemo.exe \
JvUrlListGrabberDemo.exe \
JvZoomProj.exe \
JvZLibMultipleDemo.exe \
WndProcHookDemo.exe \
JvWindowsTitleProj.exe \
JvWinDialogsDemo.exe \
JvFormatEditDemo.exe \
JvTreeViewAsMenu.exe \
JvTrayIconDemo.exe \
TransparentButtonDemo.exe \
JvTranslatorProj.exe \
SimpleTLTest1.exe \
TipsDemo.exe \
JvAniProj.exe \
JvThumbnailDemo.exe \
JvThreadProj.exe \
SystemPopupTest.exe \
JvSystemPopupProj.exe \
SpecialProgressTestPrj.exe \
JvSimpleXMLDemo.exe \
ShFileOpDemo.exe \
JvShellHookDemo.exe \
JvSearchFileProj.exe \
ScrollWinDemo.exe \
JvScreenCaptureProj.exe \
RunDLL32Demo.exe \
RegEditDemo.exe \
JvProgressDialogDemo.exe \
JvPlayListProj.exe \
JvPanelDemo.exe \
OLBarDemo.exe \
JvObjPickerDemo.exe \
EventViewer.exe \
JvMruListDemo.exe \
MonthCalendarDemo.exe \
MailExample.exe \
JvLogFileDemo.exe \
ListCombDemo.exe \
JvLinkLabelDemo.exe \
Install2LabelDemo.exe \
InspectorDBExample.exe \
InspectorSimpleExample.exe \
InspExample.exe \
ImageWindowDemo.exe \
JvID3v2Demo.exe \
JvID3v1Demo.exe \
JvHtmlParserProj.exe \
BasicDemo.exe \
CollectionDemo.exe \
SimpleHIDWrite.exe \
UsagesDemo.exe \
prjControls.exe \
FindReplaceDemo.exe \
FileDirDemo.exe \
JvEdits.exe \
DSAExamples.exe \
MessageDlgEditor.exe \
JvDomainUpDownDemo.exe \
JvDialogsDemo.exe \
StoredProc.exe \
Script.exe \
MetaData.exe \
Query.exe \
QueryStream.exe \
Restore.exe \
QuickScript.exe \
BlobStream.exe \
api1.exe \
api2.exe \
api3.exe \
api4.exe \
api5.exe \
api6.exe \
api7.exe \
api8.exe \
api10.exe \
StartBackup.exe \
StartRestore.exe \
DataPump.exe \
cursor.exe \
BlobSample.exe \
Backup.exe \
Server.exe \
Client.exe \
UIB.dll \
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
plgTest.bpl \
JvPlgMainApp.exe \
 
# -- These doesn't compile at the moment --
#Rxdemo.exe \
#RxGIFAnm.exe \
#RAControls.exe \
#DBMove.exe \
#DBTree.exe \
#LineNumbers.exe \
#RAEditorTest.exe \
#RAHLEdPropDlgTest.exe \
#ColorHintsTest.exe \
#SampleProject1.exe \
#JvInterpreterTest.exe \
#DynamicLoad.exe \
#JvInterpreterEndUser.exe \
#MDIapp.exe \
#RANotepad.exe 

#---------------------------------------------------------------------------------------------------
EditorDemo.exe: JvRichEdit\EditorDemo.dpr
  cd JvRichEdit
  $(DCC) $&.dpr
  cd ..

MessengerDemo.exe: JvRichEdit\MessengerDemo.dpr
  cd JvRichEdit
  $(DCC) $&.dpr
  cd ..

Philosophers.exe: JvManagedThreads\Philosophers\Philosophers.dpr
  cd JvManagedThreads\Philosophers
  $(DCCH) $&.dpr
  cd ..\..

Balls.exe: JvManagedThreads\Balls\Balls.dpr
  cd JvManagedThreads\Balls
  $(DCCH) $&.dpr
  cd ..\..

JvAppInstDemo.exe: JvAppInstances\JvAppInstDemo.dpr
  cd JvAppInstances
  $(DCC) $&.dpr
  cd ..

JvDbMaskEditDemo.exe: JvDbMaskEdit\JvDbMaskEditDemo.dpr
  cd JvDbMaskEdit
  $(DCC) $&.dpr
  cd ..

JvComboListBoxDemo.exe: JvComboListBox\JvComboListBoxDemo.dpr
  cd JvComboListBox
  $(DCC) $&.dpr
  cd ..

JvChartDemo.exe: JvChartDemo\JvChartDemo.dpr
  cd JvChartDemo
  $(DCC) $&.dpr
  cd ..

JvErrorIndicatorDemo.exe: JvErrorIndicator\JvErrorIndicatorDemo.dpr
  cd JvErrorIndicator
  $(DCC) $&.dpr
  cd ..

JvSpellCheckerDemo.exe: JvSpellChecker\JvSpellCheckerDemo.dpr
  cd JvSpellChecker
  $(DCC) $&.dpr
  cd ..

JvUrlListGrabberDemo.exe: JvUrlListGrabber\JvUrlListGrabberDemo.dpr
  cd JvUrlListGrabber
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

LineNumbers.exe: RaLib\RaEditor\LineNumbers.dpr
  cd RaLib\RaEditor
  $(DCCH) $&.dpr
  cd ..\..

RAEditorTest.exe: RaLib\RaEditor\RAEditorTest.dpr
  cd RaLib\RaEditor
  $(DCCH) $&.dpr
  cd ..\..

RAHLEdPropDlgTest.exe: RaLib\RaEditor\RAHLEdPropDlgTest.dpr
  cd RaLib\RaEditor
  $(DCCH) $&.dpr
  cd ..\..

ColorHintsTest.exe: RaLib\RaHtHints\ColorHintsTest.dpr
  cd RaLib\RaHtHints
  $(DCCH) $&.dpr
  cd ..\..

SampleProject1.exe: RaLib\RaInterpreter\samples\project1\SampleProject1.dpr
  cd RaLib\RaInterpreter\samples\project1
  $(DCCHH) $&.dpr
  cd ..\..\..\..

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

JvZoomProj.exe: JvZoom\JvZoomProj.dpr
  cd JvZoom
  $(DCC) $&.dpr
  cd ..

JvZLibMultipleDemo.exe: JvZLibMultiple\JvZLibMultipleDemo.dpr
  cd JvZLibMultiple
  $(DCC) $&.dpr
  cd ..

WndProcHookDemo.exe: JvWndProcHook\WndProcHookDemo.dpr
  cd JvWndProcHook
  $(DCC) $&.dpr
  cd ..

JvWindowsTitleProj.exe: JvWindowsTitle\JvWindowsTitleProj.dpr
  cd JvWindowsTitle
  $(DCC) $&.dpr
  cd ..

JvWinDialogsDemo.exe: JvWinDialogs\JvWinDialogsDemo.dpr
  cd JvWinDialogs
  $(DCC) $&.dpr
  cd ..

JvFormatEditDemo.exe: JvValidateEdit\JvFormatEditDemo.dpr
  cd JvValidateEdit
  $(DCC) $&.dpr
  cd ..

JvTreeViewAsMenu.exe: JvTreeViewAsMenu\JvTreeViewAsMenu.dpr
  cd JvTreeViewAsMenu
  $(DCC) $&.dpr
  cd ..

JvTrayIconDemo.exe: JvTrayIcon\JvTrayIconDemo.dpr
  cd JvTrayIcon
  $(DCC) $&.dpr
  cd ..

TransparentButtonDemo.exe: JvTransparentButton\TransparentButtonDemo.dpr
  cd JvTransparentButton
  $(DCC) $&.dpr
  cd ..

JvTranslatorProj.exe: JvTranslator\JvTranslatorProj.dpr
  cd JvTranslator
  $(DCC) $&.dpr
  cd ..

SimpleTLTest1.exe: JvTMTimeLine\SimpleTLTest1.dpr
  cd JvTMTimeLine
  $(DCC) $&.dpr
  cd ..

TipsDemo.exe: JvTipOfDay\TipsDemo.dpr
  cd JvTipOfDay
  $(DCC) $&.dpr
  cd ..

JvAniProj.exe: JvAni\JvAniProj.dpr
  cd JvAni
  $(DCC) $&.dpr
  cd ..

JvThumbnailDemo.exe: JvThumbnail\JvThumbnailDemo.dpr
  cd JvThumbnail
  $(DCC) $&.dpr
  cd ..

JvThreadProj.exe: JvThread\JvThreadProj.dpr
  cd JvThread
  $(DCC) $&.dpr
  cd ..

SystemPopupTest.exe: JvSystemPopUp2\SystemPopupTest.dpr
  cd JvSystemPopUp2
  $(DCC) $&.dpr
  cd ..

JvSystemPopupProj.exe: JvSystemPopup\JvSystemPopupProj.dpr
  cd JvSystemPopup
  $(DCC) $&.dpr
  cd ..

SpecialProgressTestPrj.exe: JvSpecialProgress\SpecialProgressTestPrj.dpr
  cd JvSpecialProgress
  $(DCC) $&.dpr
  cd ..

JvSimpleXMLDemo.exe: JvSimpleXML\JvSimpleXMLDemo.dpr
  cd JvSimpleXML
  $(DCC) $&.dpr
  cd ..

ShFileOpDemo.exe: JvSHFileOperation\ShFileOpDemo.dpr
  cd JvSHFileOperation
  $(DCC) $&.dpr
  cd ..

JvShellHookDemo.exe: JvShellHook\JvShellHookDemo.dpr
  cd JvShellHook
  $(DCC) $&.dpr
  cd ..

JvSearchFileProj.exe: JvSearchFile\JvSearchFileProj.dpr
  cd JvSearchFile
  $(DCC) $&.dpr
  cd ..

ScrollWinDemo.exe: JvScrollingWindow\ScrollWinDemo.dpr
  cd JvScrollingWindow
  $(DCC) $&.dpr
  cd ..

JvScreenCaptureProj.exe: JvScreenCapture\JvScreenCaptureProj.dpr
  cd JvScreenCapture
  $(DCC) $&.dpr
  cd ..

Rxdemo.exe: RxLib\Rxdemo.dpr
  cd RxLib
  $(DCC) $&.dpr
  cd ..

RunDLL32Demo.exe: JvRunDll32\RunDLL32Demo.dpr
  cd JvRunDll32
  $(DCC) $&.dpr
  cd ..

RegEditDemo.exe: JvRegistryTreeView\RegEditDemo.dpr
  cd JvRegistryTreeView
  $(DCC) $&.dpr
  cd ..

JvProgressDialogDemo.exe: JvProgressDialog\JvProgressDialogDemo.dpr
  cd JvProgressDialog
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

JvPlgMainApp.exe: JvPlugin\6PluginPackage\PlgMainApp\JvPlgMainApp.dpr
  cd JvPlugin\6PluginPackage\PlgMainApp
  $(DCCHH) $&.dpr
  cd ..\..\..

plgTest.bpl: JvPlugin\6PluginPackage\PlgPlugin\plgTest.dpk
  cd JvPlugin\6PluginPackage\PlgPlugin
  $(DCCHH) $&.dpk
  cd ..\..\..

JvPlayListProj.exe: JvPlayList\JvPlayListProj.dpr
  cd JvPlayList
  $(DCC) $&.dpr
  cd ..

JvPanelDemo.exe: JvPanel\JvPanelDemo.dpr
  cd JvPanel
  $(DCC) $&.dpr
  cd ..

OLBarDemo.exe: JvOutlookBar\OLBarDemo.dpr
  cd JvOutlookBar
  $(DCC) $&.dpr
  cd ..

JvObjPickerDemo.exe: JvObjectPicker\JvObjPickerDemo.dpr
  cd JvObjectPicker
  $(DCC) $&.dpr
  cd ..

EventViewer.exe: JvNTEventLog\EventViewer.dpr
  cd JvNTEventLog
  $(DCC) $&.dpr
  cd ..

JvMruListDemo.exe: JvMRUList\JvMruListDemo.dpr
  cd JvMRUList
  $(DCC) $&.dpr
  cd ..

MonthCalendarDemo.exe: JvMonthCalendar\MonthCalendarDemo.dpr
  cd JvMonthCalendar
  $(DCC) $&.dpr
  cd ..

MailExample.exe: JvMail\MailExample.dpr
  cd JvMail
  $(DCC) $&.dpr
  cd ..

JvLogFileDemo.exe: JvLogFile\JvLogFileDemo.dpr
  cd JvLogFile
  $(DCC) $&.dpr
  cd ..

ListCombDemo.exe: JvListComb\ListCombDemo.dpr
  cd JvListComb
  $(DCC) $&.dpr
  cd ..

JvLinkLabelDemo.exe: JvLinkLabel\JvLinkLabelDemo.dpr
  cd JvLinkLabel
  $(DCC) $&.dpr
  cd ..

Install2LabelDemo.exe: JvInstallLabel\Install2LabelDemo.dpr
  cd JvInstallLabel
  $(DCC) $&.dpr
  cd ..

InspectorDBExample.exe: JvInspectorDB\InspectorDBExample.dpr
  cd JvInspectorDB
  $(DCC) $&.dpr
  cd ..

InspectorSimpleExample.exe: JvInspector\InspectorSimpleExample.dpr
  cd JvInspector
  $(DCC) $&.dpr
  cd ..

InspExample.exe: JvInspector\InspExample.dpr
  cd JvInspector
  $(DCC) $&.dpr
  cd ..

ImageWindowDemo.exe: JvImageWindow\ImageWindowDemo.dpr
  cd JvImageWindow
  $(DCC) $&.dpr
  cd ..

JvID3v2Demo.exe: JvID3v2\JvID3v2Demo.dpr
  cd JvID3v2
  $(DCC) $&.dpr
  cd ..

JvID3v1Demo.exe: JvID3v1\JvID3v1Demo.dpr
  cd JvID3v1
  $(DCC) $&.dpr
  cd ..

JvHtmlParserProj.exe: JvHTMLParser\JvHtmlParserProj.dpr
  cd JvHTMLParser
  $(DCC) $&.dpr
  cd ..

BasicDemo.exe: JvHIDController\BasicDemo\BasicDemo.dpr
  cd JvHIDController\BasicDemo
  $(DCCH) $&.dpr
  cd ..\..

CollectionDemo.exe: JvHIDController\CollectionDemo\CollectionDemo.dpr
  cd JvHIDController\CollectionDemo
  $(DCCH) $&.dpr
  cd ..\..

SimpleHIDWrite.exe: JvHIDController\ReadWriteDemo\SimpleHIDWrite.dpr
  cd JvHIDController\ReadWriteDemo
  $(DCCH) $&.dpr
  cd ..\..

UsagesDemo.exe: JvHIDController\UsagesDemo\UsagesDemo.dpr
  cd JvHIDController\UsagesDemo
  $(DCCH) $&.dpr
  cd ..\..

RxGIFAnm.exe: JvGIFAnimator\RxGIFAnm.dpr
  cd JvGIFAnimator
  $(DCC) $&.dpr
  cd ..

prjControls.exe: JvFooterAndGroupHeader\prjControls.dpr
  cd JvFooterAndGroupHeader
  $(DCC) $&.dpr
  cd ..

FindReplaceDemo.exe: JvFindReplace\FindReplaceDemo.dpr
  cd JvFindReplace
  $(DCC) $&.dpr
  cd ..

FileDirDemo.exe: JvFileListBox\FileDirDemo.dpr
  cd JvFileListBox
  $(DCC) $&.dpr
  cd ..

JvEdits.exe: JvEdits\JvEdits.dpr
  cd JvEdits
  $(DCC) $&.dpr
  cd ..

DSAExamples.exe: JvDSADialogs\DSAExamples.dpr
  cd JvDSADialogs
  $(DCC) $&.dpr
  cd ..

MessageDlgEditor.exe: JvDSADialogs\MessageDlgEditor.dpr
  cd JvDSADialogs
  $(DCC) $&.dpr
  cd ..

JvDomainUpDownDemo.exe: JvDomainUpDown\JvDomainUpDownDemo.dpr
  cd JvDomainUpDown
  $(DCC) $&.dpr
  cd ..

JvDialogsDemo.exe: JvDialogs\JvDialogsDemo.dpr
  cd JvDialogs
  $(DCC) $&.dpr
  cd ..

StoredProc.exe: JvUIB\Component\StoredProc\StoredProc.dpr
  cd JvUIB\Component\StoredProc
  $(DCCHH) $&.dpr
  cd ..\..\..

Script.exe: JvUIB\Component\Script\Script.dpr
  cd JvUIB\Component\Script
  $(DCCHH) $&.dpr
  cd ..\..\..

MetaData.exe: JvUIB\Component\Metadata\MetaData.dpr
  cd JvUIB\Component\Metadata
  $(DCCHH) $&.dpr
  cd ..\..\..

Query.exe: JvUIB\Component\ThreadedQueries\Query.dpr
  cd JvUIB\Component\ThreadedQueries
  $(DCCHH) $&.dpr
  cd ..\..\..

QueryStream.exe: JvUIB\Component\Stream\QueryStream.dpr
  cd JvUIB\Component\Stream
  $(DCCHH) $&.dpr
  cd ..\..\..

Restore.exe: JvUIB\Component\Restore\Restore.dpr
  cd JvUIB\Component\Restore
  $(DCCHH) $&.dpr
  cd ..\..\..

QuickScript.exe: JvUIB\Component\QuickScript\QuickScript.dpr
  cd JvUIB\Component\QuickScript
  $(DCCHH) $&.dpr
  cd ..\..\..

BlobStream.exe: JvUIB\Component\Blob\AsStream\BlobStream.dpr
  cd JvUIB\Component\Blob\AsStream
  $(DCCHHH) $&.dpr
  cd ..\..\..\..

api1.exe: JvUIB\API\api1.dpr
  cd JvUIB\API
  $(DCCH) $&.dpr
  cd ..\..

api10.exe: JvUIB\API\api10.dpr
  cd JvUIB\API
  $(DCCH) $&.dpr
  cd ..\..

api2.exe: JvUIB\API\api2.dpr
  cd JvUIB\API
  $(DCCH) $&.dpr
  cd ..\..

api3.exe: JvUIB\API\api3.dpr
  cd JvUIB\API
  $(DCCH) $&.dpr
  cd ..\..

api4.exe: JvUIB\API\api4.dpr
  cd JvUIB\API
  $(DCCH) $&.dpr
  cd ..\..

api5.exe: JvUIB\API\api5.dpr
  cd JvUIB\API
  $(DCCH) $&.dpr
  cd ..\..

api6.exe: JvUIB\API\api6.dpr
  cd JvUIB\API
  $(DCCH) $&.dpr
  cd ..\..

api7.exe: JvUIB\API\api7.dpr
  cd JvUIB\API
  $(DCCH) $&.dpr
  cd ..\..

api8.exe: JvUIB\API\api8.dpr
  cd JvUIB\API
  $(DCCH) $&.dpr
  cd ..\..

StartBackup.exe: JvUIB\API\StartBackup.dpr
  cd JvUIB\API
  $(DCCH) $&.dpr
  cd ..\..

StartRestore.exe: JvUIB\API\StartRestore.dpr
  cd JvUIB\API
  $(DCCH) $&.dpr
  cd ..\..

DataPump.exe: JvUIB\Component\DataPump\DataPump.dpr
  cd JvUIB\Component\DataPump
  $(DCCHH) $&.dpr
  cd ..\..\..

cursor.exe: JvUIB\Component\Cursor\cursor.dpr
  cd JvUIB\Component\Cursor
  $(DCCHH) $&.dpr
  cd ..\..\..

BlobSample.exe: JvUIB\Component\Blob\AsString\BlobSample.dpr
  cd JvUIB\Component\Blob\AsString
  $(DCCHHH) $&.dpr
  cd ..\..\..\..

Backup.exe: JvUIB\Component\Backup\Backup.dpr
  cd JvUIB\Component\Backup
  $(DCCHH) $&.dpr
  cd ..\..\..

Server.exe: JvUIB\ClientServer\Server\Server.dpr
  cd JvUIB\ClientServer
  $(DCCH) $&.dpr
  cd ..\..

Client.exe: JvUIB\ClientServer\Client\Client.dpr
  cd JvUIB\ClientServer
  $(DCCH) $&.dpr
  cd ..\..

UIB.dll: JvUIB\Automation\UIB.dpr
  cd JvUIB\Automation
  $(DCCH) $&.dpr
  cd ..\..

