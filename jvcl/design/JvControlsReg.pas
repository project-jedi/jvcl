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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvControlsReg;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  {$IFDEF HAS_UNIT_SYSTEM_ACTIONS}
  System.Actions,
  {$ENDIF HAS_UNIT_SYSTEM_ACTIONS}
  Classes,
  Controls, ImgList, ActnList,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  DesignEditors, DesignIntf,
  JvDsgnConsts, JvDsgnIntf,
  JvCaptionButton, JvDriveCtrls, JvRegistryTreeView, JvPlaylist,
  JvPageScroller, JvUninstallControls, JvCharMap,
  JvZoom, JvBehaviorLabel, JvArrowButton, JvaScrollText, JvClock,
  JvContentScroller, JvColorBox, JvColorButton, JvDice, JvFooter,
  JvGroupHeader, JvHint, JvHtControls, JvInstallLabel, JvItemsPanel,
  JvRollOut, JvScrollPanel, JvScrollText, JvSpacer, JvSpeedBar,
  JvSpeedbarSetupForm, JvSwitch, JvSplit, JvSplitter, JvSyncSplitter,
  JvTransparentButton, JvColorForm, JvImageDrawThread, JvWinampLabel,
  JvComponentPanel, JvButtons, JvCaptionPanel, JvScrollMax, JvMovableBevel,
  JvComboListBox, JvOfficeColorButton, JvOfficeColorPanel,
  JvNetscapeSplitter, JvListComb, JvRuler,
  JvDsgnEditors, JvScrollMaxEditor, JvBehaviorLabelEditor, JvGroupHeaderEditor,
  JvControlActions, JvControlActnResForm,
  JvFooterEditor, JvSpeedbarForm, JvTransparentButtonEditors, JvRollOutEditor,
  JvVersionControlActions, JvXMLBrowser;

{$R JvControlsReg.dcr}

procedure Register;
begin
  {$IFDEF COMPILER7_UP}
  GroupDescendentsWith(TJvHint, TControl);
  {$ENDIF COMPILER7_UP}

  RegisterComponents(RsPaletteButton, [TJvTransparentButton, TJvArrowButton,
    TJvCaptionButton, TJvColorButton,  TJvOfficeColorButton, TJvOfficeColorPanel,
    TJvHTButton, TJvSpacer, TJvSwitch]);
  RegisterComponents(RsPaletteBarPanel, [TJvSpeedBar, TJvCaptionPanel,
    TJvItemsPanel, TJvMovableBevel, TJvRollOut, TJvFooter, TJvGroupHeader,
    TJvComponentPanel, TJvRuler]);
  RegisterComponents(RsPaletteLabel, [TJvBehaviorLabel, TJvInstallLabel,
    TJvHTLabel, TJvWinampLabel]);
  RegisterComponents(RsPaletteListComboTree, [TJvImageComboBox, TJvImageListBox,
    TJvComboListBox, TJvHTListBox, TJvHTComboBox]);
  RegisterComponents(RsPaletteListComboTree, [TJvUninstallComboBox, TJvUninstallListBox]);
  RegisterComponents(RsPaletteListComboTree, [TJvDriveCombo, TJvDriveList,
    TJvFileListBox, TJvDirectoryListBox, TJvPlaylist, TJvRegistryTreeView]);

  RegisterComponents(RsPaletteScrollerTracker, [TJvScrollMax, TJvaScrollText,
    TJvContentScroller, TJvPageScroller, TJvScrollingWindow, TJvScrollText]);
  RegisterComponents(RsPaletteSliderSplitter, [TJvSplitter, TJvxSplitter,
    TJvSyncSplitter, TJvNetscapeSplitter]);
  RegisterComponents(RsPaletteVisual, [TJvClock, TJvCharMap, TJvZoom, TJvDice, TJvXMLBrowserControl]);
  RegisterComponents(RsPaletteNonVisual, [TJvHint, TJvControlActionList]);

  RegisterPropertyEditor(TypeInfo(TCaption), TJvHTLabel, 'Caption', TJvHintProperty);
  RegisterPropertyEditor(TypeInfo(TJvLabelBehaviorName), TJvBehaviorLabel, 'Behavior', TJvLabelBehaviorProperty);
  RegisterPropertyEditor(TypeInfo(TCursor), TJvxSplitter, 'Cursor', nil);
  // RegisterPropertyEditor(TypeInfo(TDateTime), TJvAlarmInfo, 'Date', TJvDateTimeExProperty);
  // RegisterPropertyEditor(TypeInfo(TDateTime), TJvAlarmInfo, 'Date', TJvDateTimeExProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvSpeedItem, 'BtnCaption', TStringProperty);

  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvTransparentButtonImages, '', TJvTBImagesProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvRollOutImageOptions, '', TJvRollOutOptionsImagesProperty);

  RegisterComponentEditor(TJvScrollMax, TJvScrollMaxEditor);
  RegisterComponentEditor(TJvRollOut, TJvRollOutDefaultEditor);
  RegisterComponentEditor(TJvGroupHeader, TJvGroupHeaderEditor);
  RegisterComponentEditor(TJvFooter, TJvFooterEditor);
  RegisterComponentEditor(TJvImageListBox, TJvItemsEditor);
  RegisterComponentEditor(TJvImageComboBox, TJvItemsEditor);
  RegisterComponentEditor(TJvSpeedBar, TJvSpeedbarEditor);

  RegisterNoIcon([TJvSpeedItem, TJvSpeedbarSection]);
  RegisterClass(TJvScrollMaxBand);
  RegisterClass(TJvFooterBtn);
  RegisterActions(RsJVCLActionsCategory, [TJvRollOutAction], nil);
  RegisterActions(RsJVCLControlActionsCategory, [TJvControlCommonAction, TJvControlCollapseAction, TJvControlExpandAction, TJvControlExportAction,
    TJvControlOptimizeColumnsAction, TJvControlCustomizeColumnsAction, TJvControlPrintAction, TJvControlCustomizeAction],
    TJvControlActionsDM);
    
  RegisterComponents(RsPaletteNonVisual, [TJvVersionControlActionList]);
  RegisterActions(RsJVCLActionsCategory, [TJvVersionControlCommonAction,
    TJvVersionControlAddAction, TJvVersionControlAddSandboxAction, TJvVersionControlExploreAction, 
    TJvVersionControlDiffAction, TJvVersionControlContextMenuAction, TJvVersionControlCommitSandboxAction,
    TJvVersionControlCommitAction, TJvVersionControlCheckoutSandboxAction, TJvVersionControlBranchSandboxAction,
    TJvVersionControlBranchAction, TJvVersionControlBlameAction, TJvVersionControlGraphAction,
    TJvVersionControlLogAction, TJvVersionControlLogSandboxAction, TJvVersionControlExploreSandboxAction,
    TJvVersionControlLockAction, TJvVersionControlRenameAction, TJvVersionControlRepoBrowserAction, 
    TJvVersionControlRevertAction, TJvVersionControlStatusAction,   TJvVersionControlTagAction, 
    TJvVersionControlUnlockAction, TJvVersionControlUpdateToAction, TJvVersionControlUpdateAction, 
    TJvVersionControlMergeAction, TJvVersionControlPropertiesAction, TJvVersionControlLockSandboxAction,
    TJvVersionControlMergeSandboxAction, TJvVersionControlPropertiesSandboxAction, TJvVersionControlRenameSandboxAction,
    TJvVersionControlRevertSandboxAction, TJvVersionControlStatusSandboxAction, TJvVersionControlTagSandboxAction, 
    TJvVersionControlUpdateSandboxAction, TJvVersionControlUpdateSandboxToAction, TJvVersionControlUnlockSandboxAction], nil);
end;

end.