{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudickar att oratool dott de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens dott fudickar att oratool dott de]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDynControlEngineIntf;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  ActnList, Graphics, ComCtrls, ImgList,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QActnList, QGraphics, QComCtrls, QImgList,
  {$ENDIF UNIX}
  {$IFDEF CLR}
  Variants,
  {$ENDIF CLR}
  Classes, Controls, Forms, StdCtrls, ExtCtrls, Buttons, Dialogs,
  FileCtrl, SysUtils;

type
  {$IFDEF CLR}
  IUnknown = IInterface;
  {$ENDIF CLR}

  IJvDynControl = interface
    ['{E5A52F18-A7B2-4BE8-BAB6-D4F70A0999B3}']
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: integer);
    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);
  end;

  IJvDynControlAction = interface
    ['{8AB9511C-A03A-4388-A00A-AB95B7041133}']
    procedure ControlSetAction(Value: TCustomAction);
  end;

  IJvDynControlData = interface
    ['{569BFBFD-DFFF-44CF-AAD9-C67A0E48EE15}']
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
    property ControlValue: Variant Read ControlGetValue Write ControlSetValue;
  end;

  IJvDynControlReadOnly = interface
    ['{24E45D23-AC66-4644-8403-81FF81E28B89}']
    procedure ControlSetReadOnly(Value: boolean);
  end;

  IJvDynControlAutoSize = interface
    ['{8807045B-5FDB-4173-827D-B527D8018870}']
    procedure ControlSetAutoSize(Value: boolean);
  end;

  IJvDynControlAlign = interface
    ['{03FF9D1F-6169-443C-A6AB-8FB1F6D1CA30}']
    procedure ControlSetAlign(Value: TAlign);
  end;

  IJvDynControlAlignment = interface
    ['{BBF3775F-61A5-4455-8C54-43DCDA05E149}']
    procedure ControlSetAlignment(Value: TAlignment);
  end;

  IJvDynControlColor = interface
    ['{D6E907A4-0E6F-4AB7-98D5-F9C7660660F0}']
    procedure ControlSetColor(Value: TColor);
    procedure ControlSetParentColor(Value: boolean);
  end;

  IJvDynControlBevelBorder = interface
    ['{20DAC6BE-340D-456B-87C6-0BD71C4AA4E8}']
    procedure ControlSetBevelInner(Value: TBevelCut);
    procedure ControlSetBevelKind(Value: TBevelKind);
    procedure ControlSetBevelOuter(Value: TBevelCut);
    procedure ControlSetBorderStyle(Value: TBorderStyle);
    procedure ControlSetBorderWidth(Value: integer);
  end;

  IJvDynControlItems = interface
    ['{A4391F0B-67AD-4937-B6D9-A6DBEECDFAE8}']
    procedure ControlSetSorted(Value: boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;
    property ControlItems: TStrings Read ControlGetItems Write ControlSetItems;
  end;

  IJvDynControlEdit = interface
    ['{8E70DDD2-2D22-4EA9-B8E2-A25DE3162942}']
    procedure ControlSetPasswordChar(Value: char);
    procedure ControlSetEditMask(const Value: string);
  end;

  IJvDynControlLabel = interface
    ['{247D29CD-ABA4-4F87-A25D-4987BD950F0C}']
    procedure ControlSetFocusControl(Value: TWinControl);
    procedure ControlSetWordWrap(Value: boolean);
  end;

  TJvDynControlFileNameDialogKind = (jdkOpen, jdkOpenPicture, jdkSave, jdkSavePicture);

  IJvDynControlFileName = interface
    ['{2F75D45F-6837-4482-9BE5-499450B7350A}']
    procedure ControlSetInitialDir(const Value: string);
    procedure ControlSetDefaultExt(const Value: string);
    procedure ControlSetDialogTitle(const Value: string);
    procedure ControlSetDialogOptions(Value: TOpenOptions);
    procedure ControlSetFilter(const Value: string);
    procedure ControlSetFilterIndex(Value: integer);
    procedure ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);
  end;

  IJvDynControlDirectory = interface
    ['{1EAC8D4D-F839-43FD-B859-627874E41874}']
    procedure ControlSetInitialDir(const Value: string);
    procedure ControlSetDialogTitle(const Value: string);
    {$IFDEF VCL}
    procedure ControlSetDialogOptions(Value: TSelectDirOpts);
    {$ENDIF VCL}
  end;

  IJvDynControlComboBox = interface
    ['{9E9B46D8-2BAD-4BAA-BFDC-88FA0F3C847D}']
    procedure ControlSetNewEntriesAllowed(Value: boolean);
  end;

  IJvDynControlDate = interface
    ['{AB9EBBAB-5158-4371-A2CF-07F6D0AB86BB}']
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(const Value: string);
  end;

  IJvDynControlTime = interface
    ['{E4FF3356-62C4-4C80-B9D6-2C956D21058F}']
    procedure ControlSetFormat(const Value: string);
  end;

  IJvDynControlRadioGroup = interface
    ['{ED143973-5D21-41CF-85E1-5EE84E58BCEF}']
    procedure ControlSetColumns(Value: integer);
  end;

  IJvDynControlSpin = interface
    ['{7E178DEE-6AC2-47F3-B2F8-D5D68B4EC579}']
    procedure ControlSetIncrement(Value: integer);
    procedure ControlSetMinValue(Value: double);
    procedure ControlSetMaxValue(Value: double);
    procedure ControlSetUseForInteger(Value: boolean);
  end;

  IJvDynControlPanel = interface
    ['{EB2435FE-D9A6-4D33-9F01-589D0C93C6AC}']
    procedure ControlSetBorder(ABevelInner: TPanelBevel;
      ABevelOuter: TPanelBevel; ABevelWidth: integer; ABorderStyle: TBorderStyle;
      ABorderWidth: integer);
  end;

  IJvDynControlImage = interface
    ['{2E07C9CD-A351-4F86-91F1-45E043455669}']
    procedure ControlSetAutoSize(Value: boolean);
    procedure ControlSetIncrementalDisplay(Value: boolean);
    procedure ControlSetCenter(Value: boolean);
    {$IFDEF VCL}
    procedure ControlSetProportional(Value: boolean);
    {$ENDIF VCL}
    procedure ControlSetStretch(Value: boolean);
    procedure ControlSetTransparent(Value: boolean);
    procedure ControlSetPicture(Value: TPicture);
    procedure ControlSetGraphic(Value: TGraphic);
    function ControlGetPicture: TPicture;
  end;

  IJvDynControlButton = interface
    ['{65193802-7E31-47FD-A4B8-E1201E0A2F38}']
    procedure ControlSetDefault(Value: boolean);
    procedure ControlSetCancel(Value: boolean);
    procedure ControlSetGlyph(Value: TBitmap);
    procedure ControlSetNumGlyphs(Value: integer);
    procedure ControlSetLayout(Value: TButtonLayout);
  end;

  IJvDynControlButtonEdit = interface
    ['{F5A108E0-0B89-4CD7-9FAE-1547F00CEF62}']
    procedure ControlSetOnButtonClick(Value: TNotifyEvent);
    procedure ControlSetButtonCaption(const Value: string);
  end;

  IJvDynControlMemo = interface
    ['{3AF11540-A5D5-4C9D-9977-DD3D78F1F94F}']
    procedure ControlSetWantTabs(Value: boolean);
    procedure ControlSetWantReturns(Value: boolean);
    procedure ControlSetWordWrap(Value: boolean);
    procedure ControlSetScrollbars(Value: TScrollStyle);
  end;

  IJvDynControlDblClick = interface
    ['{EB2435FE-D9A6-4D33-9F01-589D0C93C6AC}']
    procedure ControlSetOnDblClick(Value: TNotifyEvent);
  end;

  IJvDynControlCheckListBox = interface
    ['{9C50DD6C-E147-4719-A4E9-7F11AD45606C}']
    procedure ControlSetAllowGrayed(Value: boolean);
    procedure ControlSetChecked(Index: integer; Value: boolean);
    procedure ControlSetItemEnabled(Index: integer; Value: boolean);
    procedure ControlSetHeader(Index: integer; Value: boolean);
    procedure ControlSetState(Index: integer; Value: TCheckBoxState);
    function ControlGetChecked(Index: integer): boolean;
    function ControlGetItemEnabled(Index: integer): boolean;
    function ControlGetHeader(Index: integer): boolean;
    function ControlGetState(Index: integer): TCheckBoxState;
  end;

  IJvDynControlCheckBox = interface
    ['{632BF70D-5F9F-4164-8137-4E344A5C41A3}']
    procedure ControlSetAllowGrayed(Value: boolean);
    procedure ControlSetState(Value: TCheckBoxState);
    function ControlGetState: TCheckBoxState;
    property ControlState : TCheckBoxState read ControlGetState write ControlSetState;
  end;

  IJvDynControlTreeView = interface
    ['{8DFBBAB2-C9C4-4709-A71F-E522D3998650}']
    procedure ControlSetAutoExpand(Value: boolean);
    procedure ControlSetHotTrack(Value: boolean);
    procedure ControlSetShowHint(Value: boolean);
    procedure ControlSetShowLines(Value: boolean);
    procedure ControlSetShowRoot(Value: boolean);
    procedure ControlSetToolTips(Value: boolean);
    procedure ControlSetItems(Value: TTreeNodes);
    function ControlGetItems: TTreeNodes;
    property ControlItems: TTreeNodes read ControlGetItems write ControlSetItems;
    procedure ControlSetImages(Value: TCustomImageList);
    procedure ControlSetStateImages(Value: TCustomImageList);
    function ControlGetSelected: TTreeNode;
    procedure ControlSetOnChange(Value: TTVChangedEvent);
    procedure ControlSetSortType(Value: TSortType);
  end;

  IJvDynControlProgressbar = interface
    ['{BAC5B6CD-3B65-4EBA-910A-49D152671B06}']
    procedure ControlSetMax(Value: integer);
    procedure ControlSetMin(Value: integer);
    procedure ControlSetOrientation(Value: TProgressBarOrientation);
    procedure ControlSetPosition(Value: integer);
    procedure ControlSetSmooth(Value: boolean);
    procedure ControlSetStep(Value: integer);
  end;

  IJvDynControlTabControl = interface
    ['{1C9FA637-14CC-4329-886F-696FD08AE951}']
    procedure ControlCreateTab (const AName : string);
    procedure ControlSetOnChangeTab (OnChangeEvent: TNotifyEvent);
    procedure ControlSetOnChangingTab (OnChangingEvent: TTabChangingEvent);
    procedure ControlSetTabIndex (Index : integer);
    function ControlGetTabIndex : integer;
    property ControlTabIndex : integer read ControlGetTabIndex write ControlSetTabIndex;
    procedure ControlSetMultiLine (Value : boolean);
    procedure ControlSetScrollOpposite (Value : boolean);
    procedure ControlSetHotTrack (Value : boolean);
    procedure ControlSetRaggedRight (Value : boolean);
  end;

  IJvDynControlPageControl = interface
    ['{6FCC9619-EA8D-43E6-BB66-D754A01B0720}']
    function ControlGetPage (const PageName : String) : TWinControl;
  end;



{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL:$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation


{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

