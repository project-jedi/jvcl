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
located at http://jvcl.delphi-jedi.org

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
  ActnList, Graphics, ComCtrls, ImgList,
  Classes, Controls, Forms, StdCtrls, ExtCtrls, Buttons, Dialogs,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  FileCtrl, SysUtils, Grids;

type
  IJvDynControl = interface
    ['{E5A52F18-A7B2-4BE8-BAB6-D4F70A0999B3}']
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);
  end;

  IJvDynControlCaption = interface
    ['{4D666A7B-5982-401F-915A-69FFD8264276}']
    procedure ControlSetCaption(const Value: string);
    function ControlGetCaption: string;
    property ControlCaption : string read ControlGetCaption write ControlSetCaption;
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
    property ControlValue: Variant read ControlGetValue write ControlSetValue;
  end;

  IJvDynControlFont = interface
    ['{7D628739-6C27-4641-A116-5898B630AEF6}']
    procedure ControlSetFont (Value: TFont);
    function ControlGetFont: TFont;
    property ControlFont: TFont read ControlGetFont write ControlSetFont;
  end;

  IJvDynControlReadOnly = interface
    ['{24E45D23-AC66-4644-8403-81FF81E28B89}']
    procedure ControlSetReadOnly(Value: Boolean);
  end;

  IJvDynControlAutoSize = interface
    ['{8807045B-5FDB-4173-827D-B527D8018870}']
    procedure ControlSetAutoSize(Value: Boolean);
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
    procedure ControlSetParentColor(Value: Boolean);
  end;

  IJvDynControlBevelBorder = interface
    ['{20DAC6BE-340D-456B-87C6-0BD71C4AA4E8}']
    procedure ControlSetBevelInner(Value: TBevelCut);
    procedure ControlSetBevelKind(Value: TBevelKind);
    procedure ControlSetBevelOuter(Value: TBevelCut);
    procedure ControlSetBorderStyle(Value: TBorderStyle);
    procedure ControlSetBorderWidth(Value: Integer);
  end;

  IJvDynControlItems = interface
    ['{A4391F0B-67AD-4937-B6D9-A6DBEECDFAE8}']
    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;
    property ControlItems: TStrings read ControlGetItems write ControlSetItems;
  end;

  IJvDynControlCheckComboBox = interface
    ['{86D2DD54-5114-42B2-9E44-1D32ECFDA5D0}']
    procedure ControlSetDelimiter(Value: string);
    function ControlGetDelimiter: string;
    property Delimiter: string read ControlGetDelimiter write ControlSetDelimiter;
  end;

  IJvDynControlItemIndex = interface
    ['{C4C80378-EC64-4DE0-B4D0-6BE1E09B06A1}']
    function ControlGetItemIndex: Integer;
    procedure ControlSetItemIndex(const Value: Integer);
    property ControlItemIndex: Integer read ControlGetItemIndex write
        ControlSetItemIndex;
  end;

  IJvDynControlEdit = interface
    ['{8E70DDD2-2D22-4EA9-B8E2-A25DE3162942}']
    procedure ControlSetPasswordChar(Value: char);
    procedure ControlSetEditMask(const Value: string);
  end;

  IJvDynControlLabel = interface
    ['{247D29CD-ABA4-4F87-A25D-4987BD950F0C}']
    procedure ControlSetFocusControl(Value: TWinControl);
    procedure ControlSetWordWrap(Value: Boolean);
  end;

  TJvDynControlFileNameDialogKind = (jdkOpen, jdkOpenPicture, jdkSave, jdkSavePicture);

  IJvDynControlFileName = interface
    ['{2F75D45F-6837-4482-9BE5-499450B7350A}']
    procedure ControlSetInitialDir(const Value: string);
    procedure ControlSetDefaultExt(const Value: string);
    procedure ControlSetDialogTitle(const Value: string);
    procedure ControlSetDialogOptions(Value: TOpenOptions);
    procedure ControlSetFilter(const Value: string);
    procedure ControlSetFilterIndex(Value: Integer);
    procedure ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);
  end;

  IJvDynControlDirectory = interface
    ['{1EAC8D4D-F839-43FD-B859-627874E41874}']
    procedure ControlSetInitialDir(const Value: string);
    procedure ControlSetDialogTitle(const Value: string);
    procedure ControlSetDialogOptions(Value: TSelectDirOpts);
  end;

  IJvDynControlComboBox = interface
    ['{9E9B46D8-2BAD-4BAA-BFDC-88FA0F3C847D}']
    procedure ControlSetNewEntriesAllowed(Value: Boolean);
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
    procedure ControlSetColumns(Value: Integer);
  end;

  IJvDynControlSpin = interface
    ['{7E178DEE-6AC2-47F3-B2F8-D5D68B4EC579}']
    procedure ControlSetIncrement(Value: Integer);
    procedure ControlSetMinValue(Value: double);
    procedure ControlSetMaxValue(Value: double);
    procedure ControlSetUseForInteger(Value: Boolean);
  end;

  IJvDynControlPanel = interface
    ['{EB2435FE-D9A6-4D33-9F01-589D0C93C6AC}']
    procedure ControlSetBorder(ABevelInner: TPanelBevel;
      ABevelOuter: TPanelBevel; ABevelWidth: Integer; ABorderStyle: TBorderStyle;
      ABorderWidth: Integer);
  end;

  IJvDynControlImage = interface
    ['{2E07C9CD-A351-4F86-91F1-45E043455669}']
    procedure ControlSetAutoSize(Value: Boolean);
    procedure ControlSetIncrementalDisplay(Value: Boolean);
    procedure ControlSetCenter(Value: Boolean);
    procedure ControlSetProportional(Value: Boolean);
    procedure ControlSetStretch(Value: Boolean);
    procedure ControlSetTransparent(Value: Boolean);
    procedure ControlSetPicture(Value: TPicture);
    procedure ControlSetGraphic(Value: TGraphic);
    function ControlGetPicture: TPicture;
  end;

  IJvDynControlButton = interface
    ['{65193802-7E31-47FD-A4B8-E1201E0A2F38}']
    procedure ControlSetDefault(Value: Boolean);
    procedure ControlSetCancel(Value: Boolean);
    procedure ControlSetGlyph(Value: TBitmap);
    procedure ControlSetNumGlyphs(Value: Integer);
    procedure ControlSetLayout(Value: TButtonLayout);
  end;

  IJvDynControlButtonEdit = interface
    ['{F5A108E0-0B89-4CD7-9FAE-1547F00CEF62}']
    procedure ControlSetOnButtonClick(Value: TNotifyEvent);
    procedure ControlSetButtonCaption(const Value: string);
  end;

  IJvDynControlMemo = interface
    ['{3AF11540-A5D5-4C9D-9977-DD3D78F1F94F}']
    procedure ControlSetWantTabs(Value: Boolean);
    procedure ControlSetWantReturns(Value: Boolean);
    procedure ControlSetWordWrap(Value: Boolean);
    procedure ControlSetScrollbars(Value: TScrollStyle);
  end;

  IJvDynControlDblClick = interface
    ['{B9BAAC73-15BE-4E94-A063-034E2938065D}']
    procedure ControlSetOnDblClick(Value: TNotifyEvent);
  end;

  IJvDynControlCheckListBox = interface
    ['{9C50DD6C-E147-4719-A4E9-7F11AD45606C}']
    procedure ControlSetAllowGrayed(Value: Boolean);
    procedure ControlSetChecked(Index: Integer; Value: Boolean);
    procedure ControlSetItemEnabled(Index: Integer; Value: Boolean);
    procedure ControlSetHeader(Index: Integer; Value: Boolean);
    procedure ControlSetState(Index: Integer; Value: TCheckBoxState);
    function ControlGetChecked(Index: Integer): Boolean;
    function ControlGetItemEnabled(Index: Integer): Boolean;
    function ControlGetHeader(Index: Integer): Boolean;
    function ControlGetState(Index: Integer): TCheckBoxState;
  end;

  IJvDynControlCheckBox = interface
    ['{632BF70D-5F9F-4164-8137-4E344A5C41A3}']
    procedure ControlSetAllowGrayed(Value: Boolean);
    procedure ControlSetState(Value: TCheckBoxState);
    function ControlGetState: TCheckBoxState;
    property ControlState: TCheckBoxState read ControlGetState write ControlSetState;
  end;

  IJvDynControlTreeView = interface
    ['{8DFBBAB2-C9C4-4709-A71F-E522D3998650}']
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetAutoExpand(Value: Boolean);
    procedure ControlSetHotTrack(Value: Boolean);
    procedure ControlSetShowHint(Value: Boolean);
    procedure ControlSetShowLines(Value: Boolean);
    procedure ControlSetShowRoot(Value: Boolean);
    procedure ControlSetToolTips(Value: Boolean);
    procedure ControlSetItems(Value: TTreeNodes);
    function ControlGetItems: TTreeNodes;
    function ControlGetSelected: TTreeNode;
    procedure ControlSetSelected(const Value: TTreeNode);
    procedure ControlSetImages(Value: TCustomImageList);
    procedure ControlSetStateImages(Value: TCustomImageList);
    procedure ControlSetOnChange(Value: TTVChangedEvent);
    procedure ControlSetOnChanging(Value: TTVChangingEvent);
    procedure ControlSetSortType(Value: TSortType);
    procedure ControlSortItems;
    property ControlItems: TTreeNodes read ControlGetItems write ControlSetItems;
    property ControlSelected: TTreeNode read ControlGetSelected write
        ControlSetSelected;
  end;

  IJvDynControlProgressbar = interface
    ['{BAC5B6CD-3B65-4EBA-910A-49D152671B06}']
    procedure ControlSetMarquee(Value: Boolean);
    procedure ControlSetMax(Value: Integer);
    procedure ControlSetMin(Value: Integer);
    procedure ControlSetOrientation(Value: TProgressBarOrientation);
    procedure ControlSetPosition(Value: Integer);
    procedure ControlSetSmooth(Value: Boolean);
    procedure ControlSetStep(Value: Integer);
  end;

  IJvDynControlTabControl = interface
    ['{1C9FA637-14CC-4329-886F-696FD08AE951}']
    procedure ControlCreateTab(const AName: string);
    procedure ControlSetOnChangeTab(OnChangeEvent: TNotifyEvent);
    procedure ControlSetOnChangingTab(OnChangingEvent: TTabChangingEvent);
    procedure ControlSetTabIndex(Index: Integer);
    function ControlGetTabIndex: Integer;
    property ControlTabIndex: Integer read ControlGetTabIndex write ControlSetTabIndex;
    procedure ControlSetMultiLine(Value: Boolean);
    procedure ControlSetScrollOpposite(Value: Boolean);
    procedure ControlSetHotTrack(Value: Boolean);
    procedure ControlSetRaggedRight(Value: Boolean);
  end;

  IJvDynControlPageControl = interface
    ['{6FCC9619-EA8D-43E6-BB66-D754A01B0720}']
    function ControlGetPage(const PageName: string): TWinControl;
  end;

  TJvDynControlInspectorControlOnTranslatePropertyNameEvent = function(const aPropertyName : String) : string of object;
  TJvDynControlInspectorControlOnDisplayPropertyEvent = function(const
      aPropertyName : String): boolean of object;
  TJvDynControlInspectorControlOnPropertyChangeEvent = procedure(var OldPropertyName, NewPropertyName : string) of object;

  IJvDynControlRTTIInspectorControl = interface
    ['{D7C445BF-1ED9-467B-BD01-7D40513016B4}']
    function ControlGetCurrentPropertyName: string;
    function ControlGetInspectedObject: TObject;
    function ControlGetOnDisplayProperty:
        TJvDynControlInspectorControlOnDisplayPropertyEvent;
    function ControlGetOnTranslatePropertyName:
        TJvDynControlInspectorControlOnTranslatePropertyNameEvent;
    function ControlGetVisibleItemsCount: Integer;
    function ControlIsPropertySupported(const aPropertyName : string): Boolean;
    procedure ControlSaveEditorValues;
    procedure ControlSetInspectedObject(const Value: TObject);
    procedure ControlSetOnDisplayProperty(const Value:
        TJvDynControlInspectorControlOnDisplayPropertyEvent);
    procedure ControlSetOnTranslatePropertyName(const Value:
        TJvDynControlInspectorControlOnTranslatePropertyNameEvent);
    function GetControlDividerWidth: Integer;
    function GetControlOnPropertyChange:
        TJvDynControlInspectorControlOnPropertyChangeEvent;
    procedure SetControlDividerWidth(const Value: Integer);
    procedure SetControlOnPropertyChange(const Value:
        TJvDynControlInspectorControlOnPropertyChangeEvent);
    property ControlDividerWidth: Integer read GetControlDividerWidth write
        SetControlDividerWidth;
    property ControlInspectedObject: TObject read ControlGetInspectedObject write
        ControlSetInspectedObject;
    property ControlOnDisplayProperty:
        TJvDynControlInspectorControlOnDisplayPropertyEvent read
        ControlGetOnDisplayProperty write ControlSetOnDisplayProperty;
    property ControlOnPropertyChange:
        TJvDynControlInspectorControlOnPropertyChangeEvent read
        GetControlOnPropertyChange write SetControlOnPropertyChange;
    property ControlOnTranslatePropertyName:
        TJvDynControlInspectorControlOnTranslatePropertyNameEvent read
        ControlGetOnTranslatePropertyName write ControlSetOnTranslatePropertyName;
  end;

  IJvDynControlColorComboBoxControl = interface
    ['{B95DDBED-DFB0-47D7-AA0C-1AB879EAD392}']
    function ControlGetColorName(AColor: TColor): string;
    function ControlGetSelectedColor: TColor;
    procedure ControlSetSelectedColor(const Value: TColor);
    function GetControlDefaultColor: TColor; stdcall;
    procedure SetControlDefaultColor(const Value: TColor); stdcall;
    property ControlSelectedColor: TColor read ControlGetSelectedColor write
        ControlSetSelectedColor;
    property ControlDefaultColor: TColor read GetControlDefaultColor write
        SetControlDefaultColor;

  end;

  IJvDynControlKey= interface
    ['{BE648BE4-857C-4423-A229-3484E3686ABD}']
    function ControlGetOnKeyDown: TKeyEvent;
    function ControlGetOnKeyPress: TKeyPressEvent;
    function ControlGetOnKeyUp: TKeyEvent;
    procedure ControlSetOnKeyDown(const Value: TKeyEvent);
    procedure ControlSetOnKeyPress(const Value: TKeyPressEvent);
    procedure ControlSetOnKeyUp(const Value: TKeyEvent);
    property ControlOnKeyDown: TKeyEvent read ControlGetOnKeyDown write ControlSetOnKeyDown;
    property ControlOnKeyPress: TKeyPressEvent read ControlGetOnKeyPress write ControlSetOnKeyPress;
    property ControlOnKeyUp: TKeyEvent read ControlGetOnKeyUp write ControlSetOnKeyUp;
  end;

  IJvDynControlMouse= interface
    ['{032FFE48-C7B8-4388-A63D-A275FF2FF619}']
    function ControlGetOnMouseDown: TMouseEvent;
    function ControlGetOnMouseEnter: TNotifyEvent;
    function ControlGetOnMouseLeave: TNotifyEvent;
    function ControlGetOnMouseMove: TMouseMoveEvent;
    function ControlGetOnMouseUp: TMouseEvent;
    procedure ControlSetOnMouseDown(const Value: TMouseEvent);
    procedure ControlSetOnMouseEnter(const Value: TNotifyEvent);
    procedure ControlSetOnMouseLeave(const Value: TNotifyEvent);
    procedure ControlSetOnMouseMove(const Value: TMouseMoveEvent);
    procedure ControlSetOnMouseUp(const Value: TMouseEvent);
    property ControlOnMouseDown: TMouseEvent read ControlGetOnMouseDown write ControlSetOnMouseDown;
    property ControlOnMouseEnter : TNotifyEvent read ControlGetOnMouseEnter write ControlSetOnMouseEnter;
    property ControlOnMouseLeave : TNotifyEvent read ControlGetOnMouseLeave write ControlSetOnMouseLeave;
    property ControlOnMousePress: TMouseMoveEvent read ControlGetOnMouseMove write ControlSetOnMouseMove;
    property ControlOnMouseUp: TMouseEvent read ControlGetOnMouseUp write ControlSetOnMouseUp;
  end;

  IJvDynControlStringGrid= interface
    ['{83E40A5E-BD52-49F1-8DBC-E960CC1238C3}']
    function GetControlCells(ACol, ARow: Integer): string;
    function GetControlCol: Integer;
    function GetControlColCount: Integer; stdcall;
    function GetControlColWidths(Index: Integer): Integer;
    function GetControlFixedCols: Integer;
    function GetControlFixedRows: Integer;
    function GetControlObjects(ACol, ARow: Integer): TObject;
    function GetControlOptions: TGridOptions;
    function GetControlRow: Integer;
    function GetControlRowCount: Integer; stdcall;
    function GetControlRowHeights(Index: Integer): Integer;
    procedure SetControlCells(ACol, ARow: Integer; const Value: string);
    procedure SetControlCol(const Value: Integer);
    procedure SetControlColCount(const Value: Integer);
    procedure SetControlColWidths(Index: Integer; const Value: Integer);
    procedure SetControlFixedCols(const Value: Integer);
    procedure SetControlFixedRows(const Value: Integer);
    procedure SetControlObjects(ACol, ARow: Integer; Value: TObject);
    procedure SetControlOptions(const Value: TGridOptions);
    procedure SetControlRow(const Value: Integer);
    procedure SetControlRowCount(const Value: Integer); stdcall;
    procedure SetControlRowHeights(Index: Integer; const Value: Integer);
    property ControlCells[ACol, ARow: Integer]: string read GetControlCells write SetControlCells;
    property ControlCol: Integer read GetControlCol write SetControlCol;
    property ControlColCount: Integer read GetControlColCount write SetControlColCount;
    property ControlColWidths[Index: Integer]: Integer read GetControlColWidths write SetControlColWidths;
    property ControlFixedCols: Integer read GetControlFixedCols write SetControlFixedCols;
    property ControlFixedRows: Integer read GetControlFixedRows write SetControlFixedRows;
    property ControlObjects[ACol, ARow: Integer]: TObject read GetControlObjects write SetControlObjects;
    property ControlOptions: TGridOptions read GetControlOptions write SetControlOptions;
    property ControlRow: Integer read GetControlRow write SetControlRow;
    property ControlRowCount: Integer read GetControlRowCount write SetControlRowCount;
    property ControlRowHeights[Index: Integer]: Integer read GetControlRowHeights write SetControlRowHeights;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
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
