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
DRC = $&.drc
#---------------------------------------------------------------------------------------------------
MAKE = $(ROOT)\bin\make.exe -$(MAKEFLAGS) -f$**
DCC = $(ROOT)\bin\dcc32.exe -e$(BIN) -i$(SRC) -n$(DCU) -r$(SRC) -q -u$(SRC) -w
DCCU = $(ROOT)\bin\dcc32.exe -e$(BIN) -i$(SRC) -n$(DCU) -q -w $**
BRCC = $(ROOT)\bin\brcc32.exe $**
#---------------------------------------------------------------------------------------------------
default: \
  UseCase.exe \
  WebMapper.exe \
  JvAniProj.exe \
  JvBrowserFolder.exe \
  JvClipboardViewerProj.exe \
  JvDataEmbeddedProj.exe \
  JvDialogs.exe \
  JvEdits.exe \
  JvInstallProj.exe \
  JvMousePositionnerProj.exe \
  JvMruList.exe \
  JvOutlookPanelProj.exe \
  JvPlayListProj.exe \
  JvScreenCaptureProj.exe \
  JvSearchFileProj.exe \
  JvSystemPopupProj.exe \
  JvThreadProj.exe \
  JvTreeViewAsMenu.exe \
  JvWindowsTitleProj.exe \
  JvZoomProj.exe
#---------------------------------------------------------------------------------------------------

UseCase.exe: DiagramUseCaseEditor\UseCase.dpr
  cd DiagramUseCaseEditor
  $(DCC) $&.dpr
  cd ..

WebMapper.exe: DiagramWebSiteScanner\WebMapper.dpr
  cd DiagramWebSiteScanner
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

JvClipboardViewerProj.exe: JvClipboardViewer\JvClipboardViewerProj.dpr
  cd JvClipboardViewer
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

JvDialogs.exe: JvDialogs\JvDialogs.dpr
  cd JvDialogs
  $(DCC) $&.dpr
  cd ..

JvEdits.exe: JvEdits\JvEdits.dpr
  cd JvEdits
  $(DCC) $&.dpr
  cd ..

JvInstallProj.exe: JvInstall\JvInstallProj.dpr
  cd JvInstall
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

JvWindowsTitleProj.exe: JvWindowsTitle\JvWindowsTitleProj.dpr
  cd JvWindowsTitle
  $(DCC) $&.dpr
  cd ..

JvZoomProj.exe: JvZoom\JvZoomProj.dpr
  cd JvZoom
  $(DCC) $&.dpr
  cd ..