{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

unit JvQCtrlsReg;

interface

{$IFDEF MSWINDOWS}
{$DEFINE USEWINDOWS}
{$ENDIF MSWINDOWS}

procedure Register;

implementation

uses
  Classes,
  QControls, QImgList, QActnList, 
  DesignEditors, DesignIntf, 
  JvQDsgnConsts, 
  {$IFDEF USEWINDOWS}
  JvQUninstallControls, JvQCharMap,
  {$ENDIF USEWINDOWS}
  JvQDsgnIntf, 
  QTypes, 
  JvQZoom, JvQBehaviorLabel, JvQArrowButton, JvQaScrollText, JvQClock,
  JvQContentScroller, JvQColorBox, JvQColorButton, JvQDice, JvQFooter,
  JvQGroupHeader, JvQHint, JvQHtControls, JvQInstallLabel, JvQItemsPanel,
  JvQRollOut, JvQScrollPanel, JvQScrollText, JvQSpacer, JvQSpeedBar,
  JvQSpeedbarSetupForm, JvQSwitch, JvQSplit, JvQSplitter, JvQSyncSplitter,
  JvQTransparentButton, JvQColorForm, JvQImageDrawThread, JvQWinampLabel,
  JvQComponentPanel, JvQButtons, JvQCaptionPanel, JvQScrollMax, JvQMovableBevel,
  JvQComboListBox, JvQOfficeColorButton, JvQOfficeColorPanel,
  JvQNetscapeSplitter, JvQListComb,
  JvQDsgnEditors, JvQScrollMaxEditor, JvQBehaviorLabelEditor, JvQGroupHeaderEditor,
  JvQFooterEditor, JvQSpeedbarForm, JvQTransparentButtonEditors, JvQRollOutEditor;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvCtrlsReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvCtrlsReg.dcr}
{$ENDIF LINUX}

procedure Register;
begin 
  GroupDescendentsWith(TJvHint, TControl); 

  RegisterComponents(RsPaletteButton, [TJvTransparentButton,
    TJvTransparentButton2, TJvArrowButton, 
    TJvColorButton,  TJvOfficeColorButton, TJvOfficeColorPanel,
    TJvHTButton, TJvSpacer, TJvSwitch]);
  RegisterComponents(RsPaletteBarPanel, [TJvSpeedBar, TJvCaptionPanel,
    TJvItemsPanel, TJvMovableBevel, TJvRollOut, TJvFooter, TJvGroupHeader,
    TJvComponentPanel]);
  RegisterComponents(RsPaletteLabel, [TJvBehaviorLabel, TJvInstallLabel,
    TJvHTLabel, TJvWinampLabel]);
  RegisterComponents(RsPaletteListComboTree, [TJvImageComboBox, TJvImageListBox,
    TJvComboListBox, TJvHTListBox, TJvHTComboBox]);
  {$IFDEF USEWINDOWS}
  RegisterComponents(RsPaletteListComboTree, [TJvUninstallComboBox, TJvUninstallListBox]);
  {$ENDIF USEWINDOWS}

  RegisterComponents(RsPaletteScrollerTracker, [TJvScrollMax, TJvaScrollText,
    TJvContentScroller,
    TJvScrollingWindow, TJvScrollText]);
  RegisterComponents(RsPaletteSliderSplitter, [TJvSplitter, TJvxSplitter,
    TJvSyncSplitter, TJvNetscapeSplitter]);
  RegisterComponents(RsPaletteVisual, [TJvClock, TJvZoom, TJvDice
  {$IFDEF USEWINDOWS}
  ,TJvCharMap
  {$ENDIF USEWINDOWS}
  ]);
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
  RegisterComponentEditor(TJvImageListBox, TJvStringsEditor);
  RegisterComponentEditor(TJvImageComboBox, TJvStringsEditor);
  RegisterComponentEditor(TJvSpeedBar, TJvSpeedbarCompEditor);

  RegisterNoIcon([TJvSpeedItem, TJvSpeedbarSection]);
  RegisterClass(TJvScrollMaxBand);
  RegisterClass(TJvFooterBtn);
  RegisterActions(RsJVCLActionsCategory, [TJvRollOutAction], nil);
end;

end.
