{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBandsReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvCtrlsReg;

interface

{$IFDEF MSWINDOWS}
{$DEFINE USEWINDOWS}
{$ENDIF MSWINDOWS}

procedure Register;

implementation

uses
  Classes,
  Controls, ImgList, ActnList,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvDsgnConsts,
  {$IFDEF VCL}
  JvCaptionButton, JvDriveCtrls, JvListComb, JvRegistryTreeView, JvPlaylist,
  JvPageScroller,
  {$ENDIF VCL}
  {$IFDEF USEWINDOWS}
  JvUninstallControls, JvCharMap,
  {$ENDIF USEWINDOWS}
  JvDsgnIntf,
  {$IFDEF VisualCLX}
  QTypes,
  {$ENDIF VisualCLX}
  JvZoom, JvBehaviorLabel, JvArrowButton, JvaScrollText, JvClock,
  JvContentScroller, JvColorBox, JvColorButton, JvDice, JvFooter,
  JvGroupHeader, JvHint, JvHtControls, JvInstallLabel, JvItemsPanel,
  JvRollOut, JvScrollPanel, JvScrollText, JvSpacer, JvSpeedBar,
  JvSpeedbarSetupForm, JvSwitch, JvSplit, JvSplitter, JvSyncSplitter,
  JvTransparentButton, JvColorForm, JvImageDrawThread, JvWinampLabel,
  JvComponentPanel, JvButtons, JvCaptionPanel, JvScrollMax, JvMovableBevel,
  JvComboListBox, JvOfficeColorButton, JvOfficeColorPanel,
  JvNetscapeSplitter,
  JvDsgnEditors, JvScrollMaxEditor, JvBehaviorLabelEditor, JvGroupHeaderEditor,
  JvFooterEditor, JvSpeedbarForm, JvTransparentButtonEditors, JvRollOutEditor;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvCtrlsReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvCtrlsReg.dcr}
{$ENDIF LINUX}

procedure Register;
begin
  {$IFDEF COMPILER7_UP}
  GroupDescendentsWith(TJvHint, TControl);
  {$ENDIF COMPILER7_UP}

  RegisterComponents(RsPaletteButton, [TJvTransparentButton,
    TJvTransparentButton2, TJvArrowButton,
    {$IFDEF VCL}
    TJvCaptionButton,
    {$ENDIF VCL}
    TJvColorButton,  TJvOfficeColorButton, TJvOfficeColorPanel,
    TJvHTButton, TJvSpacer, TJvSwitch]);
  RegisterComponents(RsPaletteBarPanel, [TJvSpeedBar, TJvCaptionPanel,
    TJvItemsPanel, TJvMovableBevel, TJvRollOut, TJvFooter, TJvGroupHeader,
    TJvComponentPanel]);
  RegisterComponents(RsPaletteLabel, [TJvBehaviorLabel, TJvInstallLabel,
    TJvHTLabel, TJvWinampLabel]);


  RegisterComponents(RsPaletteListComboTree, [{$IFDEF VCL}TJvImageComboBox, TJvImageListBox, TJvComboListBox, {$ENDIF VCL} TJvHTListBox, TJvHTComboBox]);

  {$IFDEF USEWINDOWS}
  RegisterComponents(RsPaletteListComboTree, [TJvUninstallComboBox, TJvUninstallListBox]);
  {$ENDIF USEWINDOWS}
  {$IFDEF VCL}
  RegisterComponents(RsPaletteListComboTree, [TJvDriveCombo, TJvDriveList,
    TJvFileListBox, TJvDirectoryListBox, TJvPlaylist, TJvRegistryTreeView]);
  {$ENDIF VCL}

  RegisterComponents(RsPaletteScrollerTracker, [TJvScrollMax, TJvaScrollText,
    TJvContentScroller,
    {$IFDEF VCL}
    TJvPageScroller,
    {$ENDIF VCL}
    TJvScrollingWindow, TJvScrollText]);
  RegisterComponents(RsPaletteSliderSplitter, [TJvSplitter, TJvxSplitter,
    TJvSyncSplitter, TJvNetscapeSplitter]);
  RegisterComponents(RsPaletteVisual, [TJvClock, TJvZoom, TJvDice, TJvCharMap]);
  RegisterComponents(RsPaletteNonVisual, [TJvHint]);

  RegisterPropertyEditor(TypeInfo(TCaption), TJvHTLabel, 'Caption', TJvHintProperty);
  RegisterPropertyEditor(TypeInfo(TJvLabelBehaviorName), TJvBehaviorLabel, 'Behavior', TJvLabelBehaviorProperty);
  RegisterPropertyEditor(TypeInfo(TCursor), TJvxSplitter, 'Cursor', nil);
  // RegisterPropertyEditor(TypeInfo(TDateTime), TJvAlarmInfo, 'Date', TJvDateTimeExProperty);
  // RegisterPropertyEditor(TypeInfo(TDateTime), TJvAlarmInfo, 'Date', TJvDateTimeExProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvSpeedItem, 'BtnCaption', TStringProperty);

  RegisterPropertyEditor(TypeInfo(Integer), TJvTransparentButton2, 'ActiveIndex', TJvTBImagesProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TJvTransparentButton2, 'DisabledIndex', TJvTBImagesProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TJvTransparentButton2, 'DownIndex', TJvTBImagesProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TJvTransparentButton2, 'GrayIndex', TJvTBImagesProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvRollOutImageOptions, '', TJvRollOutOptionsImagesProperty);

  RegisterComponentEditor(TJvScrollMax, TJvScrollMaxEditor);
  RegisterComponentEditor(TJvRollOut, TJvRollOutDefaultEditor);
  RegisterComponentEditor(TJvGroupHeader, TJvGroupHeaderEditor);
  RegisterComponentEditor(TJvFooter, TJvFooterEditor);
  {$IFDEF VCL}
  RegisterComponentEditor(TJvImageListBox, TJvStringsEditor);
  RegisterComponentEditor(TJvImageComboBox, TJvStringsEditor);
  {$ENDIF VCL}
  RegisterComponentEditor(TJvSpeedBar, TJvSpeedbarCompEditor);

  RegisterNoIcon([TJvSpeedItem, TJvSpeedbarSection]);
  RegisterClass(TJvScrollMaxBand);
  RegisterClass(TJvFooterBtn);
  RegisterActions(RsJVCLActionsCategory, [TJvRollOutAction], nil);
end;

end.
