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

Last Modified: 2003-11-09

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCtrlsReg;

interface

procedure Register;

implementation

uses
  Classes, Controls, ImgList, 
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvConsts, JvZoom, JvBehaviorLabel, JvArrowButton,
  JvaScrollText, JvCaptionButton, JvClock, JvContentScroller, JvColorBox,
  JvColorButton, JvDice, JvDriveCtrls, JvFooter, JvGroupHeader, JvHint,
  JvHtControls, JvInstallLabel, JvItemsPanel, JvListComb, JvPageScroller,
  JvRegistryTreeView, JvRollOut, JvScrollPanel, JvScrollText, JvSpacer,
  JvSpeedBar, JvSplit, JvSplitter, JvSwitch, JvSyncSplitter,
  JvTransparentButton, JvSpeedbarSetupForm, JvColorForm, JvDsgnIntf,
  JvImageDrawThread, JvWinampLabel, JvPlaylist, JvComponentPanel, JvButtons,
  JvCaptionPanel, JvScrollMax, JvUninstallControls, JvMovableBevel,
  JvComboListBox, JvCharMap, JvScrollMaxEditor, JvBehaviorLabelEditor,
  JvGroupHeaderEditor, JvFooterEditor, JvSpeedbarForm, JvDsgnEditors,
  JvTransparentButtonEditors, JvRollOutEditor;

{$R ..\resources\JvCtrlsReg.dcr}

procedure Register;
begin
  RegisterComponents(SPaletteButton, [TJvTransparentButton,
    TJvTransparentButton2, TJvArrowButton, TJvCaptionButton, TJvColorButton,
    TJvHTButton, TJvSpacer, TJvSwitch]);
  RegisterComponents(SPaletteBarPanel, [TJvSpeedBar, TJvCaptionPanel,
    TJvItemsPanel, TJvMovableBevel, TJvRollOut, TJvFooter, TJvGroupHeader,
    TJvComponentPanel]);
  RegisterComponents(SPaletteLabel, [TJvBehaviorLabel, TJvInstallLabel,
    TJvHTLabel, TJvWinampLabel]);
  RegisterComponents(SPaletteListComboTree, [TJvImageComboBox, TJvImageListBox,
    TJvComboListBox, TJvHTListBox, TJvHTComboBox, TJvUninstallComboBox,
    TJvUninstallListBox, TJvDriveCombo, TJvDriveList, TJvFileListBox,
    TJvDirectoryListBox, TJvRegistryTreeView, TJvPlaylist]);
  RegisterComponents(SPaletteScrollerTracker, [TJvScrollMax, TJvaScrollText,
    TJvContentScroller, TJvPageScroller, TJvScrollingWindow, TJvScrollText]);
  RegisterComponents(SPaletteSliderSplitter, [TJvSplitter, TJvxSplitter,
    TJvSyncSplitter]);
  RegisterComponents(SPaletteVisual, [TJvClock,
    TJvZoom, TJvDice, TJvCharMap]);
  RegisterComponents(SPaletteNonVisual, [TJvHint {, TJvRegAuto}]);

  RegisterPropertyEditor(TypeInfo(TCaption), TJvHTLabel, 'Caption', TJvHintProperty);
  RegisterPropertyEditor(TypeInfo(TJvLabelBehaviorName), TJvBehaviorLabel, 'Behavior', TJvLabelBehaviorProperty);
  RegisterPropertyEditor(TypeInfo(TCursor), TJvxSplitter, 'Cursor', nil);
  //RegisterPropertyEditor(TypeInfo(TDateTime),TJvAlarmInfo, 'Date', TJvDateTimeExProperty);
  //RegisterPropertyEditor(TypeInfo(TDateTime),TJvAlarmInfo, 'Date', TJvDateTimeExProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvSpeedItem, 'BtnCaption', TStringProperty);

  RegisterPropertyEditor(TypeInfo(integer), TJvTransparentButton2, 'ActiveIndex', TJvTBImagesProperty);
  RegisterPropertyEditor(TypeInfo(integer), TJvTransparentButton2, 'DisabledIndex', TJvTBImagesProperty);
  RegisterPropertyEditor(TypeInfo(integer), TJvTransparentButton2, 'DownIndex', TJvTBImagesProperty);
  RegisterPropertyEditor(TypeInfo(integer), TJvTransparentButton2, 'GrayIndex', TJvTBImagesProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvRollOutImageOptions, '', TJvRollOutOptionsImagesProperty);

  RegisterComponentEditor(TJvScrollMax, TJvScrollMaxEditor);
  RegisterComponentEditor(TJvGroupHeader, TJvGroupHeaderEditor);
  RegisterComponentEditor(TJvFooter, TJvFooterEditor);
  RegisterComponentEditor(TJvImageListBox, TJvStringsEditor);
  RegisterComponentEditor(TJvImageComboBox, TJvStringsEditor);
  RegisterComponentEditor(TJvSpeedBar, TJvSpeedbarCompEditor);
  //  RegisterComponentEditor(TJvRegAuto, TJvRegAutoEditor);

  RegisterNoIcon([TJvSpeedItem, TJvSpeedbarSection]);
  RegisterClass(TJvScrollMaxBand);
  RegisterClass(TJvFooterBtn);
end;

end.
