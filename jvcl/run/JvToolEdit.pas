{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvToolEdit.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributers:
  Rob den Braasem [rbraasem att xs4all dott nl]
  Polaris Software
  rblaurindo
  Andreas Hausladen

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
  (rb) Move button related functionality from TJvCustomComboEdit to TJvEditButton
-----------------------------------------------------------------------------}
// $Id$

unit JvToolEdit;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows, Messages, ShellAPI, ActiveX,
  {$ENDIF MSWINDOWS}
  Types,
  ShlObj,
  Variants,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Menus,
  Buttons, FileCtrl, Mask, ImgList, ActnList, ExtDlgs,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvConsts,
  JvExControls, JvSpeedButton, JvTypes, JvExMask,
  JvDataSourceIntf, JvBrowseFolder;

const
  scAltDown = scAlt + VK_DOWN;
  DefEditBtnWidth = 21;

{$IFNDEF COMPILER7_UP}
// Autocomplete stuff for Delphi 5 and 6. (missing in ShlObj)
type
  IAutoComplete = interface(IUnknown)
    ['{00bb2762-6a77-11d0-a535-00c04fd7d062}']
    function Init(hwndEdit: THandle; punkACL: IUnknown;
      pwszRegKeyPath: LPCWSTR; pwszQuickComplete: LPCWSTR): HRESULT; stdcall;
    function Enable(fEnable: BOOL): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IAutoComplete}

const
  { IAutoComplete2 options }
  ACO_NONE = 0;
  ACO_AUTOSUGGEST = $1;
  ACO_AUTOAPPEND = $2;
  ACO_SEARCH = $4;
  ACO_FILTERPREFIXES = $8;
  ACO_USETAB = $10;
  ACO_UPDOWNKEYDROPSLIST = $20;
  ACO_RTLREADING = $40;

type
  IAutoComplete2 = interface(IAutoComplete)
    ['{EAC04BC0-3791-11d2-BB95-0060977B464C}']
    function SetOptions(dwFlag: DWORD): HRESULT; stdcall;
    function GetOptions(var dwFlag: DWORD): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IAutoComplete2}
  
  // To avoid ambiguities, we include shldisp.h and define the _di_ interfaces ourselves
  {$HPPEMIT '#include "shldisp.h"'}
  {$HPPEMIT 'typedef DelphiInterface<IAutoComplete> _di_IAutoComplete;'}
  {$HPPEMIT 'typedef DelphiInterface<IAutoComplete2> _di_IAutoComplete2;'}
{$ENDIF !COMPILER7_UP}

// C++ Builder needs this HPPEMIT in order for the generated header to compile.
{$HPPEMIT 'typedef DelphiInterface<IEnumString> _di_IEnumString;'}

type
  TFileExt = type string;

  TCloseUpEvent = procedure(Sender: TObject; Accept: Boolean) of object;
  TPopupAlign = (epaRight, epaLeft);

  TJvPopupWindowBase = TJvExCustomControl;

  TJvPopupWindow = class(TJvPopupWindowBase)
  private
    FEditor: TWinControl;
    FCloseUp: TCloseUpEvent;
    procedure WMMouseActivate(var Msg: TMessage); message WM_MOUSEACTIVATE;
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
  protected
    FActiveControl: TWinControl;
    FIsFocusable: Boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(const Value: Variant); virtual; abstract;
    procedure InvalidateEditor;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure CloseUp(Accept: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function GetPopupText: string; virtual;
    procedure Hide;
    procedure Show(Origin: TPoint); virtual;
    { Determines the ctrl that receives the keyboard input if the dropdown
      window is showing, but the combo edit still has focus }
    property ActiveControl: TWinControl read FActiveControl;
    { Determines whether the popup window may be activated }
    property IsFocusable: Boolean read FIsFocusable;
    property OnCloseUp: TCloseUpEvent read FCloseUp write FCloseUp;
  end;

  TJvEditButton = class(TJvImageSpeedButton)
  private
    FNoAction: Boolean;
    FPopupVisible: Boolean;
    procedure WMContextMenu(var Msg: TWMContextMenu); message WM_CONTEXTMENU;
    function GetGlyph: TBitmap;
    function GetNumGlyphs: TJvNumGlyphs;
    function GetUseGlyph: Boolean;
    procedure SetGlyph(const Value: TBitmap);
    procedure SetNumGlyphs(Value: TJvNumGlyphs);
    procedure SetPopupVisible(const Value: Boolean);
  protected
    {$IFDEF JVCLThemesEnabled}
    FDrawThemedDropDownBtn: Boolean;
    FDrawThemedDatePickerBtn: Boolean;
    {$ENDIF JVCLThemesEnabled}
    FStandard: Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintImage(Canvas: TCanvas; ARect: TRect; const Offset: TPoint;
      AState: TJvButtonState; DrawMark: Boolean; PaintOnGlass: Boolean); override;
    procedure Paint; override;
    property PopupVisible: Boolean read FPopupVisible write SetPopupVisible;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;

    property UseGlyph: Boolean read GetUseGlyph;// write FDrawGlyph;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs: TJvNumGlyphs read GetNumGlyphs write SetNumGlyphs;
  end;

  TGlyphKind = (gkCustom, gkDefault, gkDropDown, gkEllipsis);
  TJvImageKind = (ikCustom, ikDefault, ikDropDown, ikEllipsis, ikDatePicker);

  TJvCustomComboEdit = class;

  TJvCustomComboEditActionLink = class(TWinControlActionLink)
  protected
    function IsCaptionLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    function IsShortCutLinked: Boolean; override;
    procedure SetHint(const Value: THintString); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
    procedure SetShortCut(Value: TShortCut); override;
  end;

  TJvCustomComboEditActionLinkClass = class of TJvCustomComboEditActionLink;

  TJvAutoCompleteOption = (acoAutoSuggest, acoAutoAppend, acoSearch,
    acoFilterPrefixes, acoUseTab, acoUpDownKeyDropsList, acoRTLReading);
  TJvAutoCompleteOptions = set of TJvAutoCompleteOption;
  TJvAutoCompleteFileOption = (acfFileSystem, acfFileSysDirs, acfURLHistory, acfURLMRU);
  TJvAutoCompleteFileOptions = set of TJvAutoCompleteFileOption;

  TJvCustomComboEditDataConnector = class(TJvFieldDataConnector)
  private
    FEdit: TJvCustomComboEdit;
  protected
    procedure RecordChanged; override;
    procedure UpdateData; override;
    property Control: TJvCustomComboEdit read FEdit;
  public
    constructor Create(AEdit: TJvCustomComboEdit);
  end;

  TJvCustomComboEdit = class(TJvExCustomMaskEdit)
  private
    FOnButtonClick: TNotifyEvent;
    FOnPopupShown: TNotifyEvent;
    FOnPopupHidden: TNotifyEvent;
    FClickKey: TShortCut;
    FReadOnly: Boolean;
    FDirectInput: Boolean;
    FAlwaysEnableButton: Boolean;
    FAlwaysShowPopup: Boolean;
    FPopupAlign: TPopupAlign;
    FGroupIndex: Integer;
    FDisabledColor: TColor;
    FDisabledTextColor: TColor;
    FOnKeyDown: TKeyEvent;
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    FImageKind: TJvImageKind;
    FNumGlyphs: Integer;
    FStreamedButtonWidth: Integer;
    FStreamedFixedWidth: Boolean;
    FOnEnabledChanged: TNotifyEvent;
    { We hide the button by setting its width to 0, thus we have to store the
      width the button should have when shown again in FSavedButtonWidth: }
    FSavedButtonWidth: Integer;
    FDataConnector: TJvCustomComboEditDataConnector;
    FAlignment: TAlignment;
    FAutoCompleteIntf: IAutoComplete;
    FAutoCompleteItems: TStrings;
    FAutoCompleteOptions: TJvAutoCompleteOptions;
    FTextChanged: Boolean;
    FInCMExit: Integer;
    FCheckOnExit: Boolean;
    FOnPopupChange: TNotifyEvent;
    FOnPopupValueAccepted: TNotifyEvent;
    procedure SetAutoCompleteItems(Strings: TStrings);
    procedure SetAutoCompleteOptions(const Value: TJvAutoCompleteOptions);
    procedure SetAlignment(Value: TAlignment);
    function GetFlat: Boolean;
    procedure ReadCtl3D(Reader: TReader);
    procedure ReadParentCtl3D(Reader: TReader);
    procedure SetFlat(const Value: Boolean);
    function GetParentFlat: Boolean;
    procedure SetParentFlat(const Value: Boolean);
    function IsFlatStored: Boolean;
    function BtnWidthStored: Boolean;
    function GetButtonFlat: Boolean;
    function GetButtonHint: string;
    function GetButtonWidth: Integer;
    function GetDirectInput: Boolean;
    function GetGlyph: TBitmap;
    function GetGlyphKind: TGlyphKind;
    function GetMinHeight: Integer;
    function GetNumGlyphs: TNumGlyphs;
    function GetPopupVisible: Boolean;
    function GetShowButton: Boolean;
    function GetTextHeight: Integer;
    function IsImageIndexStored: Boolean;
    function IsCustomGlyph: Boolean;
    procedure EditButtonClick(Sender: TObject);
    procedure ReadGlyphKind(Reader: TReader);
    procedure RecreateGlyph;
    procedure SetButtonFlat(const Value: Boolean);
    procedure SetButtonHint(const Value: string);
    procedure SetButtonWidth(Value: Integer);
    procedure SetGlyph(Value: TBitmap);
    procedure SetGlyphKind(Value: TGlyphKind);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImageKind(const Value: TJvImageKind);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetNumGlyphs(const Value: TNumGlyphs);
    procedure SetShowButton(const Value: Boolean);
    procedure SetDataConnector(const Value: TJvCustomComboEditDataConnector);
    procedure UpdateBtnBounds(var NewLeft, NewTop, NewWidth, NewHeight: Integer);
    procedure UpdateGroup;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure CMBiDiModeChanged(var Msg: TMessage); message CM_BIDIMODECHANGED;
    procedure CMCancelMode(var Msg: TCMCancelMode); message CM_CANCELMODE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CNCtlColor(var Msg: TMessage); message CN_CTLCOLOREDIT;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure CMPopupCloseup(var Msg: TMessage); message CM_POPUPCLOSEUP;
    {$IFDEF JVCLThemesEnabled}
    procedure WMNCPaint(var Msg: TWMNCPaint); message WM_NCPAINT;
    {$ENDIF JVCLThemesEnabled}
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure CMFixCaretPosition(var Msg: TMessage); message CM_FIXCARETPOSITION;
    procedure SetAlwaysEnableButton(const Value: Boolean);
    procedure UpdateButtonEnabled;
  protected
    FButton: TJvEditButton;
    FBtnControl: TWinControl;
    FPopupVisible: Boolean;
    FFocused: Boolean;
    FPopup: TWinControl;
    function CreateDataConnector: TJvCustomComboEditDataConnector; virtual;
    procedure CustomAlignPosition(Control: TControl; var NewLeft, NewTop, NewWidth,
      NewHeight: Integer; var AlignRect: TRect; AlignInfo: TAlignInfo); override;
    procedure WndProc(var Msg: TMessage); override;
    procedure WMClear(var Msg: TMessage); message WM_CLEAR;
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure AdjustSize; override;
    procedure FocusKilled(NextWnd: THandle); override;
    procedure FocusSet(PrevWnd: THandle); override;
    procedure EnabledChanged; override;
    procedure FontChanged; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    {$IFDEF JVCLThemesEnabled}
    procedure AutoSizeEditButton;
    function GetDatePickerThemeButtonWidth: Integer;
    function GetDatePickerThemeButtonMinTextSize: Integer; virtual; // overridden by TJvCustomDatePickerEdit
    procedure Resize; override;
    {$ENDIF JVCLThemesEnabled}
    procedure DoCtl3DChanged; virtual;
    function DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean; override;
    { Repositions the child controls; checkbox }
    procedure UpdateControls; virtual;
    { Updates the margins of the edit box }
    procedure UpdateMargins; dynamic;
    { Returns the margins of the edit box }
    procedure GetInternalMargins(var ALeft, ARight: Integer); virtual;
    procedure CreatePopup; virtual;
    procedure HidePopup; virtual; // (ahuser): WARNING: Do not release or free the component in HidePopup -> else AV in MouseUp
    procedure ShowPopup(Origin: TPoint); virtual;
    function AcceptPopup(var Value: Variant): Boolean; virtual;
    function EditCanModify: Boolean; override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetPopupValue: Variant; virtual;
    function GetReadOnly: Boolean; virtual;
    function GetSettingCursor: Boolean;
    procedure AcceptValue(const Value: Variant); virtual;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure AdjustHeight;
    procedure ButtonClick; dynamic;
    procedure Change; override;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateAutoComplete; virtual;
    procedure DestroyWnd; override;
    procedure DestroyAutoComplete; virtual;
    procedure UpdateAutoComplete; virtual;
    function GetAutoCompleteSource: IEnumString; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoChange; virtual; //virtual Polaris
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure LocalKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PopupChange; virtual;
    procedure ResetPopupValue; virtual;
    procedure PopupCloseUp(Sender: TObject; Accept: Boolean); virtual;
    procedure AsyncPopupCloseUp(Accept: Boolean); virtual;
    procedure PopupDropDown(DisableEdit: Boolean); virtual;
    procedure SetClipboardCommands(const Value: TJvClipboardCommands); override;
    procedure SetDirectInput(Value: Boolean);
    procedure SetDisabledColor(const Value: TColor); virtual;
    procedure SetDisabledTextColor(const Value: TColor); virtual;
    procedure SetGroupIndex(const Value: Integer);
    procedure SetPopupValue(const Value: Variant); virtual;
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetShowCaret;
    procedure UpdatePopupVisible;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AlwaysEnableButton: Boolean read FAlwaysEnableButton write SetAlwaysEnableButton default False;
    property AlwaysShowPopup: Boolean read FAlwaysShowPopup write FAlwaysShowPopup default False;
    property AutoCompleteItems: TStrings read FAutoCompleteItems write SetAutoCompleteItems;
    property AutoCompleteOptions: TJvAutoCompleteOptions read FAutoCompleteOptions write SetAutoCompleteOptions default [];
    property Button: TJvEditButton read FButton;
    property ButtonFlat: Boolean read GetButtonFlat write SetButtonFlat default False;
    property ButtonHint: string read GetButtonHint write SetButtonHint;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth stored BtnWidthStored;
    property ClickKey: TShortCut read FClickKey write FClickKey default scAltDown;
    property DirectInput: Boolean read GetDirectInput write SetDirectInput default True;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clWindow;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText;
    property Flat: Boolean read GetFlat write SetFlat   stored IsFlatStored;
    property ParentFlat: Boolean read GetParentFlat write SetParentFlat default True;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsCustomGlyph;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default -1;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex stored IsImageIndexStored default -1;
    property ImageKind: TJvImageKind read FImageKind write SetImageKind default ikCustom;
    property Images: TCustomImageList read FImages write SetImages;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property PopupAlign: TPopupAlign read FPopupAlign write FPopupAlign default epaRight;
    property PopupVisible: Boolean read GetPopupVisible;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SettingCursor: Boolean read GetSettingCursor;
    property ShowButton: Boolean read GetShowButton write SetShowButton default True;
      // CheckOnExit disables the ValidateEdit call that TCustomMaskEdit executes when
      // it receives a CM_EXIT message. If you set it to False, you should call ValidateEdit
      // yourself when you want to validate the value (like in the OK button of a dialog).
    property CheckOnExit: Boolean read FCheckOnExit write FCheckOnExit default True;
    property OnEnabledChanged: TNotifyEvent read FOnEnabledChanged write FOnEnabledChanged;
    property OnPopupShown: TNotifyEvent read FOnPopupShown write FOnPopupShown;
    property OnPopupHidden: TNotifyEvent read FOnPopupHidden write FOnPopupHidden;
      // OnPopupChange is triggered when the edit text is changed while the popup is visible.
    property OnPopupChange: TNotifyEvent read FOnPopupChange write FOnPopupChange;
      // OnPopupValueAccepted is triggered when the value from the popup is accepted and written to
      // the edit's text property. It is not triggered if the new value is the same as the old value.
    property OnPopupValueAccepted: TNotifyEvent read FOnPopupValueAccepted write FOnPopupValueAccepted;

    property DataConnector: TJvCustomComboEditDataConnector read FDataConnector write SetDataConnector;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ValidateEdit; override;
    class function DefaultImageIndex: TImageIndex; virtual;
    class function DefaultImages: TCustomImageList; virtual;
    procedure DoClick;
    procedure SelectAll;
    { Backwards compatibility; moved to public&published; eventually remove }
    property GlyphKind: TGlyphKind read GetGlyphKind write SetGlyphKind;
    property Ctl3D;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvComboEdit = class(TJvCustomComboEdit)
  public
    property Button;
  published
    property Action;
    property Align;
    property Alignment;
    property AlwaysEnableButton;
    property AlwaysShowPopup;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property AutoCompleteItems;
    property AutoCompleteOptions;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property Flat;
    property ImeMode;
    property ImeName;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentFlat;
    property OnEndDock;
    property OnStartDock;
    property BorderStyle;
    property ButtonFlat;
    property ButtonHint;
    property ButtonWidth;
    property CharCase;
    property ClickKey;
    property ClipboardCommands;
    property Color;
    property Constraints;
    property DirectInput;
    property DisabledColor;
    property DisabledTextColor;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property Glyph;
    property HideSelection;
    property ImageIndex;
    property ImageKind;
    property Images;
    property MaxLength;
    property NumGlyphs;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property ShowButton;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

    property DataConnector;
    property OnPopupShown;
    property OnPopupHidden;
    property OnPopupChange;
    property OnPopupValueAccepted;

    {$IFDEF COMPILER12_UP}
    property NumbersOnly;
    {$ENDIF}
    {$IFDEF COMPILER14_UP}
    property Touch;
    {$ENDIF COMPILER14_UP}
    property TextHint;
  end;

  { TJvFileDirEdit }
  { The common parent of TJvFilenameEdit and TJvDirectoryEdit      }
  { For internal use only; it's not intended to be used separately }

type
  TExecOpenDialogEvent = procedure(Sender: TObject; var AName: string; var AAction: Boolean) of object;

  TJvFileDirEdit = class(TJvCustomComboEdit)
  private
    FErrMode: Cardinal;
    FMultipleDirs: Boolean;
    FOnDropFiles: TNotifyEvent;
    FOnBeforeDialog: TExecOpenDialogEvent;
    FOnAfterDialog: TExecOpenDialogEvent;
    FAcceptFiles: Boolean;
    FMRUList: IUnknown;
    FHistoryList: IUnknown;
    FFileSystemList: IUnknown;
    FAutoCompleteFileOptions: TJvAutoCompleteFileOptions;
    FAutoCompleteSourceIntf: IEnumString;
    procedure SetAutoCompleteFileOptions(const Value: TJvAutoCompleteFileOptions);
    procedure SetDragAccept(Value: Boolean);
    procedure SetAcceptFiles(Value: Boolean);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    {$IFDEF JVCLThemesEnabled}
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    {$ENDIF JVCLThemesEnabled}
  protected
    procedure CreateHandle; override;
    procedure DestroyWindowHandle; override;
    procedure DestroyAutoComplete; override;
    procedure UpdateAutoComplete; override;
    function GetAutoCompleteSource: IEnumString; override;
    function GetLongName: string; virtual; abstract;
    function GetShortName: string; virtual; abstract;
    function GetLocalizedName: string; virtual; abstract;
    procedure DoAfterDialog(var FileName: string; var Action: Boolean); dynamic;
    procedure DoBeforeDialog(var FileName: string; var Action: Boolean); dynamic;
    procedure ReceptFileDir(const AFileName: string); virtual; abstract;
    procedure ClearFileList; virtual;
    procedure Change; override;
    procedure DisableSysErrors;
    procedure EnableSysErrors;
    property AutoCompleteFileOptions: TJvAutoCompleteFileOptions read FAutoCompleteFileOptions write
      SetAutoCompleteFileOptions;
    property AutoCompleteOptions default [acoAutoSuggest];
    property ImageKind default ikDefault;
    property MaxLength;
  public
    constructor Create(AOwner: TComponent); override;
    property LongName: string read GetLongName;
    property ShortName: string read GetShortName;
    property LocalizedName: string read GetLocalizedName;
  published
    property AcceptFiles: Boolean read FAcceptFiles write SetAcceptFiles default True;
    property AlwaysEnableButton default True;
    property AlwaysShowPopup default True;
    property OnBeforeDialog: TExecOpenDialogEvent read FOnBeforeDialog write FOnBeforeDialog;
    property OnAfterDialog: TExecOpenDialogEvent read FOnAfterDialog write FOnAfterDialog;
    property OnDropFiles: TNotifyEvent read FOnDropFiles write FOnDropFiles;
    property OnButtonClick;
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    {$IFDEF UNICODE}
    property OEMConvert default False; // Mantis 4454
    {$ELSE}
    property OEMConvert default True; // Mantis 3621
    {$ENDIF UNICODE}

    {$IFDEF COMPILER14_UP}
    property Touch;
    {$ENDIF COMPILER14_UP}
    property TextHint;
  end;

  TFileDialogKind = (dkOpen, dkSave, dkOpenPicture, dkSavePicture);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvFilenameEdit = class(TJvFileDirEdit)
  private
    FDialog: TOpenDialog;
    FDialogKind: TFileDialogKind;
    FAddQuotes: Boolean;
    FPhysicalFileName: string;
    FDisplayLocalizedName: Boolean;
    procedure CreateEditDialog;
    function GetFileName: TFileName;
    function GetDefaultExt: TFileExt;
    function GetFileEditStyle: TFileEditStyle;
    function GetFilter: string;
    function GetFilterIndex: Integer;
    function GetInitialDir: string;
    function GetHistoryList: TStrings;
    function GetOptions: TOpenOptions;
    function GetDialogTitle: string;
    function GetDialogFiles: TStrings;
    procedure SetDialogKind(Value: TFileDialogKind);
    procedure SetFileName(const Value: TFileName);
    procedure SetDefaultExt(Value: TFileExt);
    procedure SetFileEditStyle(Value: TFileEditStyle);
    procedure SetFilter(const Value: string);
    procedure SetFilterIndex(Value: Integer);
    procedure SetInitialDir(const Value: string);
    procedure SetHistoryList(Value: TStrings);
    procedure SetOptions(Value: TOpenOptions);
    procedure SetDialogTitle(const Value: string);
    function IsCustomTitle: Boolean;
    function IsCustomFilter: Boolean;
    procedure SetDisplayLocalizedName(const Value: Boolean);
  protected
    procedure PopupDropDown(DisableEdit: Boolean); override;
    procedure ReceptFileDir(const AFileName: string); override;
    procedure ClearFileList; override;
    function GetLongName: string; override;
    function GetShortName: string; override;
    function GetLocalizedName: string; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure WndProc(var Msg: TMessage); override;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function DefaultImageIndex: TImageIndex; override;
    property Dialog: TOpenDialog read FDialog;
    property DialogFiles: TStrings read GetDialogFiles;
  published
    property Action;
    property Align;
    property AutoSize;
    property AddQuotes: Boolean read FAddQuotes write FAddQuotes default True;
    property DialogKind: TFileDialogKind read FDialogKind write SetDialogKind default dkOpen;
    property DisplayLocalizedName: Boolean read FDisplayLocalizedName write SetDisplayLocalizedName default False;
    property DefaultExt: TFileExt read GetDefaultExt write SetDefaultExt;
    property AutoCompleteOptions;
    property AutoCompleteFileOptions default [acfFileSystem];
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Flat;
    property ParentFlat;
    { (rb) Obsolete; added 'stored False', eventually remove }
    property FileEditStyle: TFileEditStyle read GetFileEditStyle write SetFileEditStyle stored False;
    property FileName: TFileName read GetFileName write SetFileName stored False;
    property Filter: string read GetFilter write SetFilter stored IsCustomFilter;
    property FilterIndex: Integer read GetFilterIndex write SetFilterIndex default 1;
    property InitialDir: string read GetInitialDir write SetInitialDir;
    { (rb) Obsolete; added 'stored False', eventually remove }
    property HistoryList: TStrings read GetHistoryList write SetHistoryList stored False;
    property DialogOptions: TOpenOptions read GetOptions write SetOptions default [ofHideReadOnly];
    property DialogTitle: string read GetDialogTitle write SetDialogTitle stored IsCustomTitle;
    property AutoSelect;
    property ButtonHint;
    property ButtonFlat;
    property BorderStyle;
    property CharCase;
    property ClickKey;
    property Color;
    property DirectInput;
    property DragCursor;
    property BiDiMode;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property OnEndDock;
    property OnStartDock;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property Glyph;
    property GroupIndex;
    property ImageIndex;
    property Images;
    property ImageKind;
    property NumGlyphs;
    property ButtonWidth;
    property HideSelection;
    property Anchors;
    property Constraints;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property ShowButton;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
  end;

  TDirDialogKind = (dkVCL, dkWin32);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDirectoryEdit = class(TJvFileDirEdit)
  private
    FOptions: TSelectDirOpts;
    FInitialDir: string;
    FDialogText: string;
    FDialogKind: TDirDialogKind;
    FPhysicalDirectory: string;
    FDisplayLocalizedName: Boolean;
    FOptionsWin32: TOptionsDir;

    procedure SetDirectory(const Value: string);
    function GetDirectory: string;
    procedure SetDisplayLocalizedName(const Value: Boolean);
  protected
    FMultipleDirs: Boolean;
    procedure PopupDropDown(DisableEdit: Boolean); override;
    procedure ReceptFileDir(const AFileName: string); override;
    function GetLongName: string; override;
    function GetShortName: string; override;
    function GetLocalizedName: string; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure WndProc(var Msg: TMessage); override;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function DefaultImageIndex: TImageIndex; override;
    property Directory: string read GetDirectory write SetDirectory;
  published
    property Action;
    property Align;
    property AutoSize;
    property DialogKind: TDirDialogKind read FDialogKind write FDialogKind default dkVCL;
    property DialogText: string read FDialogText write FDialogText;
    property DisplayLocalizedName: Boolean read FDisplayLocalizedName write SetDisplayLocalizedName default False;
    property AutoCompleteOptions;
    property AutoCompleteFileOptions default [acfFileSystem, acfFileSysDirs];
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Flat;
    property ParentFlat;
    property DialogOptions: TSelectDirOpts read FOptions write FOptions default [sdAllowCreate];
    property DialogOptionsWin32: TOptionsDir read FOptionsWin32 write FOptionsWin32 default DefaultJvBrowseFolderDialogOptions;
    property InitialDir: string read FInitialDir write FInitialDir;
    property MultipleDirs: Boolean read FMultipleDirs write FMultipleDirs default False;
    property AutoSelect;
    property ButtonHint;
    property ButtonFlat;
    property BorderStyle;
    property CharCase;
    property ClickKey;
    property Color;
    property DirectInput;
    property DragCursor;
    property BiDiMode;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property DragKind;
    property OnEndDock;
    property OnStartDock;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property Glyph;
    property GroupIndex;
    property ImageIndex;
    property Images;
    property ImageKind;
    property NumGlyphs;
    property ButtonWidth;
    property HideSelection;
    property Anchors;
    property Constraints;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property ShowButton;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
  end;

  TCalendarStyle = (csPopup, csDialog);
  TYearDigits = (dyDefault, dyFour, dyTwo);

const
  {$IFDEF DEFAULT_POPUP_CALENDAR}
  dcsDefault = csPopup;
  {$ELSE}
  dcsDefault = csDialog;
  {$ENDIF DEFAULT_POPUP_CALENDAR}

type
  TExecDateDialog = procedure(Sender: TObject; var ADate: TDateTime;
    var Action: Boolean) of object;
  TJvInvalidDateEvent = procedure(Sender: TObject; const DateString: string;
    var NewDate: TDateTime; var Accept: Boolean) of object;
  TPreferredDateFormat = (pdLocale, pdLocaleOnly, pdCustom, pdCustomOnly);

  TJvCustomDateEditDataConnector = class(TJvCustomComboEditDataConnector)
  private
    FDefaultDate: TDateTime;
    FDefaultDateIsNow: Boolean;
    procedure SetDefaultDateIsNow(const Value: Boolean);
    function IsDefaultDateStored: Boolean;
  protected
    procedure RecordChanged; override;
    procedure UpdateData; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property DefaultDate: TDateTime read FDefaultDate write FDefaultDate stored IsDefaultDateStored;
    property DefaultDateIsNow: Boolean read FDefaultDateIsNow write SetDefaultDateIsNow default False;
  end;

  TJvCustomDateEdit = class(TJvCustomComboEdit)
  private
    FMinDate: TDateTime;
    FMaxDate: TDateTime;
    FTitle: string;
    FOnAcceptDate: TExecDateDialog;
    FOnInvalidDate: TJvInvalidDateEvent;
    FDefaultToday: Boolean;
    FPopupColor: TColor;
    FBlanksChar: Char;
    FCalendarHints: TStringList;
    FStartOfWeek: TDayOfWeekName;
    FWeekends: TDaysOfWeek;
    FWeekendColor: TColor;
    FCustomDateFormat: string;
    FYearDigits: TYearDigits;
    FDateFormatPreferred: TPreferredDateFormat;
    FDateFormat: string;
    FDateFormat2: string;
    FFormatting: Boolean;
    FShowNullDate: Boolean;
    procedure SetMinDate(Value: TDateTime);
    procedure SetMaxDate(Value: TDateTime);
    function GetDate: TDateTime;
    procedure SetCustomDateFormat(const Value: string);
    procedure SetDateFormatPreferred(Value: TPreferredDateFormat);
    function IsDateFormatStored: Boolean;
    function IsDateFormatPreferredStored: Boolean;
    procedure SetYearDigits(Value: TYearDigits);
    function GetPopupColor: TColor;
    procedure SetPopupColor(Value: TColor);
    function GetDialogTitle: string;
    procedure SetDialogTitle(const Value: string);
    function IsCustomTitle: Boolean;
    function IsDateStored: Boolean;
    function GetCalendarStyle: TCalendarStyle;
    procedure SetCalendarStyle(Value: TCalendarStyle);
    function GetCalendarHints: TStrings;
    procedure SetCalendarHints(Value: TStrings);
    procedure CalendarHintsChanged(Sender: TObject);
    procedure SetWeekendColor(Value: TColor);
    procedure SetWeekends(Value: TDaysOfWeek);
    procedure SetStartOfWeek(Value: TDayOfWeekName);
    procedure SetBlanksChar(Value: Char);
    function TextStored: Boolean;
    function StoreMinDate: Boolean;
    function StoreMaxDate: Boolean;
    function FourDigitYear: Boolean;
    procedure WMContextMenu(var Msg: TWMContextMenu); message WM_CONTEXTMENU;
    procedure SetShowNullDate(const Value: Boolean);
  protected
    FDateAutoBetween: Boolean;
    procedure SetDate(Value: TDateTime); virtual;
    function DoInvalidDate(const DateString: string; var ANewDate: TDateTime): Boolean; virtual;
    procedure SetDateAutoBetween(Value: Boolean); virtual;
    procedure TestDateBetween(var Value: TDateTime); virtual;
    procedure DoExit; override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    function AcceptPopup(var Value: Variant): Boolean; override;
    procedure AcceptValue(const Value: Variant); override;
    procedure SetPopupValue(const Value: Variant); override;
    function GetDateFormat: string;
    procedure ApplyDate(Value: TDateTime); virtual;
    procedure UpdateFormat;
    procedure UpdatePopup;
    procedure PopupDropDown(DisableEdit: Boolean); override;
    procedure SetParent(AParent: TWinControl); override;
    function GetDefaultDateFormat: string; virtual;
    function GetDefaultDateFormatPreferred: TPreferredDateFormat; virtual;
    function CreateDataConnector: TJvCustomComboEditDataConnector; override;
    function DisplayNullDateAsEmptyText: Boolean; virtual;

    property BlanksChar: Char read FBlanksChar write SetBlanksChar default ' ';
    property CalendarHints: TStrings read GetCalendarHints write SetCalendarHints;
    property CheckOnExit default False;
    property DefaultToday: Boolean read FDefaultToday write FDefaultToday default False;
    property DialogTitle: string read GetDialogTitle write SetDialogTitle stored IsCustomTitle;
    property EditMask stored False;
    property Formatting: Boolean read FFormatting;
    property ImageKind default ikDefault;
    property PopupColor: TColor read GetPopupColor write SetPopupColor default clMenu;
    property CalendarStyle: TCalendarStyle read GetCalendarStyle
      write SetCalendarStyle default dcsDefault;
    property StartOfWeek: TDayOfWeekName read FStartOfWeek write SetStartOfWeek default Mon;
    property Weekends: TDaysOfWeek read FWeekends write SetWeekends default [Sun];
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clRed;
    property DateFormat: string read FCustomDateFormat write SetCustomDateFormat stored IsDateFormatStored;
    property DateFormatPreferred: TPreferredDateFormat read FDateFormatPreferred
      write SetDateFormatPreferred stored IsDateFormatPreferredStored;
    property YearDigits: TYearDigits read FYearDigits write SetYearDigits default dyDefault;
    property OnAcceptDate: TExecDateDialog read FOnAcceptDate write FOnAcceptDate;
    property OnInvalidDate: TJvInvalidDateEvent read FOnInvalidDate write FOnInvalidDate;
    property MaxLength stored False;
    { Text is already stored via Date property }
    property Text stored False;
    property ShowNullDate: Boolean read FShowNullDate write SetShowNullDate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function DefaultImageIndex: TImageIndex; override;

    procedure ValidateEdit; override;
    procedure CheckValidDate; virtual;
    function GetDateMask: string;
    procedure UpdateMask; virtual;

    property Date: TDateTime read GetDate write SetDate stored IsDateStored;
    property PopupVisible;
    property DateAutoBetween: Boolean read FDateAutoBetween write SetDateAutoBetween default True;
    property MinDate: TDateTime read FMinDate write SetMinDate stored StoreMinDate;
    property MaxDate: TDateTime read FMaxDate write SetMaxDate stored StoreMaxDate;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDateEdit = class(TJvCustomDateEdit)
  protected
    procedure SetDate(Value: TDateTime); override;
  public
    constructor Create(AOwner: TComponent); override;
    property EditMask;
  published
    property Date;
    property DateFormat;
    property DateFormatPreferred;
    property DateAutoBetween;
    property MinDate;
    property MaxDate;
    property Align; 
    property Action;
    property AutoSelect;
    property AutoSize;
    property BlanksChar;
    property BorderStyle;
    property ButtonHint;
    property ButtonFlat;
    property CalendarHints;
    property CheckOnExit;
    property ClickKey;
    property Color;
    property DefaultToday;
    property DialogTitle;
    property DirectInput;
    property DragCursor;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property DragKind;
    property Flat;
    property ParentBiDiMode;
    property ParentFlat;
    property ImeMode;
    property ImeName;
    property OnEndDock;
    property OnStartDock;
    property DragMode;
    property Enabled;
    property Font;
    property Glyph;
    property GroupIndex;
    property ImageIndex;
    property Images;
    property ImageKind;
    property NumGlyphs;
    property ButtonWidth;
    property HideSelection;
    property Anchors;
    property Constraints;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupAlign;
    property PopupColor;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property ShowButton;
    property CalendarStyle;
    property ShowNullDate;
    property StartOfWeek;
    property Weekends;
    property WeekendColor;
    property YearDigits;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnAcceptDate;
    property OnInvalidDate;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    property OnKeyDown;
    property OnPopupHidden;
    property OnPopupShown;

    property DataConnector;

    {$IFDEF COMPILER14_UP}
    property Touch;
    {$ENDIF COMPILER14_UP}
    property TextHint;
  end;

  EComboEditError = class(EJVCLException);

{ Utility routines }

procedure DateFormatChanged;

function EditorTextMargins(Editor: TCustomEdit): TPoint;

function PaintComboEdit(Editor: TJvCustomComboEdit; const AText: string;
  AAlignment: TAlignment; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Msg: TWMPaint): Boolean;
function PaintEdit(Editor: TCustomEdit; const AText: string;
  AAlignment: TAlignment; PopupVisible: Boolean;
  DisabledTextColor: TColor; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Msg: TWMPaint): Boolean;

function LoadDefaultBitmap(Bmp: TBitmap; Item: Integer): Boolean;

function IsInWordArray(Value: Word; const A: array of Word): Boolean;

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

uses
  RTLConsts, Math, MaskUtils,
  MultiMon,
  {$IFDEF JVCLThemesEnabled}
  {$IFDEF HAS_UNIT_UXTHEME}
  UxTheme,
  {$ENDIF HAS_UNIT_UXTHEME}
  {$ENDIF JVCLThemesEnabled}
  {$IFDEF HAS_UNIT_VCL_THEMES} // VCL-Styles support
  Vcl.Themes,
  {$ENDIF HAS_UNIT_VCL_THEMES}
  JclSysInfo, JclFileUtils, JclStrings,
  JvPickDate, JvJCLUtils, JvJVCLUtils,
  JvThemes, JvResources, JclSysUtils;

{$R JvToolEdit.res}

type
  TCustomMaskEditAccessPrivate = class(TCustomEdit)
  protected
    {$IFDEF RTL310_UP}
      {$MESSAGE WARN 'Check if Vcl.Mask.TCustomMaskEdit still has the exact same fields and adjust the IFDEF'}
    {$ENDIF}
    // Do not remove these fields, although they are not used.
    FEditMask: TEditMask;
    FMaskBlank: Char;
    FMaxChars: Integer;
    FMaskSave: Boolean;
    FMaskState: TMaskedState;
    FCaretPos: Integer;
    FBtnDownX: Integer;
    FOldValue: string;
    FSettingCursor: Boolean; // << this field can't be read
  end;

  TCustomEditAccessProtected = class(TCustomEdit);
  TCustomFormAccessProtected = class(TCustomForm);
  TWinControlAccessProtected = class(TWinControl);

{$IFDEF JVCLThemesEnabled}
{$IFNDEF HAS_UNIT_UXTHEME}
  HTHEME = THANDLE;
{$ENDIF ~HAS_UNIT_UXTHEME}
{$ENDIF JVCLThemesEnabled}

const
  sDirBmp = 'JvDirectoryEditGLYPH';    { Directory editor button glyph }
  sFileBmp = 'JvFilenameEditGLYPH';    { Filename editor button glyph }
  sDateBmp = 'JvCustomDateEditGLYPH';  { Date editor button glyph }

  {$IFDEF JVCLThemesEnabled}
  // (rb) should/can these be put in a separate resource file?
  sDirXPBmp = 'JvDirectoryEditXPGLYPH';
  sFileXPBmp = 'JvFilenameEditXPGLYPH';
  {$ENDIF JVCLThemesEnabled}

const
  ACLO_NONE            = 0;   // don't enumerate anything
  ACLO_CURRENTDIR      = 1;   // enumerate current directory
  ACLO_MYCOMPUTER      = 2;   // enumerate MyComputer
  ACLO_DESKTOP         = 4;   // enumerate Desktop Folder
  ACLO_FAVORITES       = 8;   // enumerate Favorites Folder
  ACLO_FILESYSONLY     = 16;  // enumerate only the file system
  ACLO_FILESYSDIRS     = 32;  // enumerate only the file system dirs, UNC shares, and UNC servers.

  //IID_IAutoCompList: TGUID = (D1:$00BB2760; D2:$6A77; D3:$11D0; D4:($A5, $35, $00, $C0, $4F, $D7, $D0, $62));
  //IID_IObjMgr: TGUID = (D1:$00BB2761; D2:$6A77; D3:$11D0; D4:($A5, $35, $00, $C0, $4F, $D7, $D0, $62));
  //IID_IACList: TGUID = (D1:$77A130B0; D2:$94FD; D3:$11D0; D4:($A5, $44, $00, $C0, $4F, $D7, $d0, $62));
  //IID_IACList2: TGUID = (D1:$470141a0; D2:$5186; D3:$11d2; D4:($bb, $b6, $00, $60, $97, $7b, $46, $4c));
  //IID_ICurrentWorkingDirectory: TGUID = (D1:$91956d21; D2:$9276; D3:$11d1; D4:($92, $1a, $00, $60, $97, $df, $5b, $d4));

  CLSID_AutoComplete: TGUID = (D1:$00BB2763; D2:$6A77; D3:$11D0; D4:($A5, $35, $00, $C0, $4F, $D7, $D0, $62));
  CLSID_ACLHistory: TGUID = (D1:$00BB2764; D2:$6A77; D3:$11D0; D4:($A5, $35, $00, $C0, $4F, $D7, $D0, $62));
  CLSID_ACListISF: TGUID = (D1:$03C036F1; D2:$A186; D3:$11D0; D4:($82, $4A, $00, $AA, $00, $5B, $43, $83));
  CLSID_ACLMRU: TGUID = (D1:$6756a641; D2:$de71; D3:$11d0; D4:($83, $1b, $0, $aa, $0, $5b, $43, $83));
  CLSID_ACLMulti: TGUID = (D1:$00BB2765; D2:$6A77; D3:$11D0; D4:($A5, $35, $00, $C0, $4F, $D7, $D0, $62));

  //#if (_WIN32_IE >= 0x0600)
  //CLSID_ACLCustomMRU: TGUID = (D1:$6935db93; D2:$21e8; D3:$4ccc; D4:($be, $b9, $9f, $e3, $c7, $7a, $29, $7a));
  //#endif

type
  TAutoCompleteSource = class(TInterfacedObject, IEnumString)
  private
    FComboEdit: TJvCustomComboEdit;
    FCurrentIndex: Integer;
  protected
    { IEnumString }
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HRESULT; stdcall;
    function Skip(celt: Longint): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out enm: IEnumString): HRESULT; stdcall;
  public
    constructor Create(AComboEdit: TJvCustomComboEdit; const StartIndex: Integer); virtual;
  end;

  IACList = interface(IUnknown)
    ['{77A130B0-94FD-11D0-A544-00C04FD7d062}']
    function Expand(pszExpand: string): HRESULT; stdcall;
  end;

  IACList2 = interface(IACList)
    ['{470141a0-5186-11d2-bbb6-0060977b464c}']
    function SetOptions(dwFlag: DWORD): HRESULT; stdcall;
    function GetOptions(var pdwFlag: DWORD): HRESULT; stdcall;
  end;

  IObjMgr = interface(IUnknown)
    ['{00BB2761-6A77-11D0-a535-00c04fd7d062}']
    function Append(punk: IUnknown): HRESULT; stdcall;
    function Remove(punk: IUnknown): HRESULT; stdcall;
  end;

type
  { TDateHook is used to only have 1 hook per application for monitoring
    date changes;

    We can't use WM_WININICHANGE or CM_WININICHANGE in the controls
    itself, because it comes too early. (The Application object does the
    changing on receiving WM_WININICHANGE; The Application object receives it
    later than the forms, controls etc.
  }

  TDateHook = class(TObject)
  private
    FCount: Integer;
    FHooked: Boolean;
    FWinIniChangeReceived: Boolean;
  protected
    function FormatSettingsChange(var Msg: TMessage): Boolean;
    procedure Hook;
    procedure UnHook;
  public
    procedure Add;
    procedure Delete;
  end;

var
  GDateHook: TDateHook = nil;
  GDateImageIndex: TImageIndex = -1;
  GDefaultComboEditImagesList: TImageList = nil;
  GDirImageIndex: TImageIndex = -1;
  GFileImageIndex: TImageIndex = -1;
  {$IFDEF JVCLThemesEnabled}
  GDirImageIndexXP: TImageIndex = -1;
  GFileImageIndexXP: TImageIndex = -1;
  GDatePickerThemeDataAvailable: Integer = -1;
  {$IFDEF HAS_UNIT_VCL_THEMES}
  GDatePickerThemeData: HTHEME;
  {$ENDIF HAS_UNIT_VCL_THEMES}
  {$ENDIF JVCLThemesEnabled}
  GCoInitialized: Integer = 0;

{$IFDEF JVCLThemesEnabled}
const
  DefDatePickerThemeButtonWidth = 34;
{$ENDIF JVCLThemesEnabled}

//=== Local procedures =======================================================

function DateHook: TDateHook;
begin
  if GDateHook = nil then
    GDateHook := TDateHook.Create;
  Result := GDateHook;
end;

{$IFDEF JVCLThemesEnabled}
  {$IFNDEF HAS_UNIT_UXTHEME}
const
  // Vista+
  VSCLASS_DATEPICKER = 'DATEPICKER';
  DP_SHOWCALENDARBUTTONRIGHT = 3;
  DPSCBR_NORMAL   = 1;
  DPSCBR_HOT      = 2;
  DPSCBR_PRESSED  = 3;
  DPSCBR_DISABLED = 4;
  {$ENDIF ~HAS_UNIT_UXTHEME}

function IsDatePickerThemeDataAvailable: Boolean;
begin
  {$IFDEF HAS_UNIT_VCL_THEMES}
  if (GDatePickerThemeDataAvailable = -1) and StyleServices.Available and Assigned(OpenThemeData) then
  begin
    GDatePickerThemeData := OpenThemeData(Application.Handle, VSCLASS_DATEPICKER);
    if GDatePickerThemeData = 0 then
      GDatePickerThemeDataAvailable := 0
    else
      GDatePickerThemeDataAvailable := 1;
  end;
  Result := (GDatePickerThemeDataAvailable = 1) and StyleServices.IsSystemStyle; // CustomStyles don't support the DatePicker theme
  {$ELSE}
  Result := False;
  {$ENDIF ~HAS_UNIT_VCL_THEMES}
end;
{$ENDIF JVCLThemesEnabled}

function ClipFilename(const FileName: string; const Clip: Boolean): string;
var
  Params: string;
begin
  if FileExists(FileName) then
    Result := FileName
  else
  if DirectoryExists(FileName) then
    Result := IncludeTrailingPathDelimiter(FileName)
  else
  if Clip then
    SplitCommandLine(FileName, Result, Params)
  else
    Result := FileName;
end;

function ExtFilename(const FileName: string): string;
begin
  if (Pos(' ', FileName) > 0) and (FileName[1] <> '"') then
    Result := Format('"%s"', [FileName])
  else
    Result := FileName;
end;

function NvlDate(DateValue, DefaultValue: TDateTime): TDateTime;
begin
  if DateValue = NullDate then
    Result := DefaultValue
  else
    Result := DateValue;
end;

function ParentFormVisible(AControl: TControl): Boolean;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(AControl);
  Result := Assigned(Form) and Form.Visible;
end;

//=== Global procedures ======================================================

procedure DateFormatChanged;

  procedure IterateControls(AControl: TWinControl);
  var
    I: Integer;
  begin
    with AControl do
      for I := 0 to ControlCount - 1 do
      begin
        if Controls[I] is TJvCustomDateEdit then
          TJvCustomDateEdit(Controls[I]).UpdateMask
        else
        if Controls[I] is TWinControl then
          IterateControls(TWinControl(Controls[I]));
      end;
  end;

var
  I: Integer;
begin
  if Screen <> nil then
    for I := 0 to Screen.FormCount - 1 do
      IterateControls(Screen.Forms[I]);
end;

function EditorTextMargins(Editor: TCustomEdit): TPoint;
var
  I: Integer;
  ed: TCustomEditAccessProtected;
begin
  ed := TCustomEditAccessProtected(Editor);
  if ed.BorderStyle = bsNone then
    I := 0
  else
  if ed.Ctl3D then
    I := 1
  else
    I := 2;
  if GetWindowLong(ed.Handle, GWL_STYLE) and ES_MULTILINE = 0 then
    Result.X := (SendMessage(ed.Handle, EM_GETMARGINS, 0, 0) and $0000FFFF) + I
  else
    Result.X := I;
  Result.Y := I;
end;

function IsInWordArray(Value: Word; const A: array of Word): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to High(A) do
    if A[I] = Value then
      Exit;
  Result := False;
end;

function LoadDefaultBitmap(Bmp: TBitmap; Item: Integer): Boolean;
begin
  Bmp.Handle := LoadBitmap(0, PChar(Item));
  Result := Bmp.Handle <> 0;
end;

function PaintComboEdit(Editor: TJvCustomComboEdit; const AText: string;
  AAlignment: TAlignment; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Msg: TWMPaint): Boolean;
begin
  if not (csDestroying in Editor.ComponentState) then
  begin
    Result := PaintEdit(Editor, AText, AAlignment, Editor.PopupVisible,
      Editor.FDisabledTextColor, StandardPaint, ACanvas, Msg);
  end
  else
    Result := True;
end;

function PaintEdit(Editor: TCustomEdit; const AText: string;
  AAlignment: TAlignment; PopupVisible: Boolean;
  DisabledTextColor: TColor; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Msg: TWMPaint): Boolean;
type
  TEd = TCustomEditAccessProtected;
const
  AlignStyle: array [Boolean, TAlignment] of DWORD =
    ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
  {$IFDEF COMPILER16_UP}
  ColorStates: array[Boolean] of TStyleColor = (scEditDisabled, scEdit);
  FontColorStates: array[Boolean] of TStyleFont = (sfEditBoxTextDisabled, sfEditBoxTextNormal);
  {$ENDIF COMPILER16_UP}
var
  LTextWidth, X: Integer;
  EditRect: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  ExStyle: DWORD;
begin
  Result := True;
  if csDestroying in Editor.ComponentState then
    Exit;
  if TEd(Editor).UseRightToLeftAlignment then
    ChangeBiDiModeAlignment(AAlignment);
  if StandardPaint and not (csPaintCopy in TEd(Editor).ControlState) then
  begin
    if SysLocale.MiddleEast and TEd(Editor).HandleAllocated and TEd(Editor).IsRightToLeft then
    begin { This keeps the right aligned text, right aligned }
      ExStyle := DWORD(GetWindowLong(TEd(Editor).Handle, GWL_EXSTYLE)) and not (WS_EX_RIGHT or WS_EX_RTLREADING or WS_EX_LEFTSCROLLBAR);
      if TEd(Editor).UseRightToLeftReading then
        ExStyle := ExStyle or WS_EX_RTLREADING;
      if TEd(Editor).UseRightToLeftScrollBar then
        ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
      ExStyle := ExStyle or AlignStyle[TEd(Editor).UseRightToLeftAlignment, AAlignment];
      if DWORD(GetWindowLong(TEd(Editor).Handle, GWL_EXSTYLE)) <> ExStyle then
        SetWindowLong(TEd(Editor).Handle, GWL_EXSTYLE, ExStyle);
    end;
    Result := False;
    { return false if we need to use standard paint handler }
    Exit;
  end;
  { Since edit controls do not handle justification unless multi-line (and
    then only poorly) we will draw right and center justify manually unless
    the edit has the focus. }
  if ACanvas = nil then
  begin
    ACanvas := TControlCanvas.Create;
    ACanvas.Control := Editor;
  end;
  DC := Msg.DC;
  if DC = 0 then
    DC := BeginPaint(TEd(Editor).Handle, PS);
  ACanvas.Handle := DC;
  try
    ACanvas.Font := TEd(Editor).Font;
    with ACanvas do
    begin
      SendRectMessage(Editor.Handle, EM_GETRECT, 0, EditRect);
      if not TEd(Editor).Ctl3D and (TEd(Editor).BorderStyle = bsSingle) then
      begin
        Brush.Color := clWindowFrame;
        FrameRect(TEd(Editor).ClientRect);
      end;
      S := AText;
      LTextWidth := TextWidth(S);
      if PopupVisible then
        X := EditRect.Left
      else
      begin
        case AAlignment of
          taLeftJustify:
            X := EditRect.Left;
          taRightJustify:
            X := EditRect.Right - LTextWidth;
        else
          X := (EditRect.Right + EditRect.Left - LTextWidth) div 2;
        end;
      end;
      if SysLocale.MiddleEast then
        UpdateTextFlags;
      if not TEd(Editor).Enabled then
      begin
        // if PS.fErase then // (p3) fErase is not set to true when control is disabled
        TEd(Editor).Perform(WM_ERASEBKGND, ACanvas.Handle, 0);

        SaveDC(ACanvas.Handle);
        try
          ACanvas.Brush.Style := bsClear;
          {$IFDEF COMPILER16_UP}
          if StyleServices.Enabled and not StyleServices.IsSystemStyle then
          begin
            ACanvas.Brush.Color := StyleServices.GetStyleColor(ColorStates[Editor.Enabled]);
            ACanvas.Font.Color := StyleServices.GetStyleFontColor(FontColorStates[Editor.Enabled]);
          end
          else
          {$ENDIF COMPILER16_UP}
          ACanvas.Font.Color := DisabledTextColor;
          ACanvas.TextRect(EditRect, X, EditRect.Top, S);
        finally
          RestoreDC(ACanvas.Handle, -1);
        end;
      end
      else
      begin
        {$IFDEF COMPILER16_UP}
        if StyleServices.Enabled and not StyleServices.IsSystemStyle then
        begin
          ACanvas.Brush.Color := StyleServices.GetStyleColor(ColorStates[Editor.Enabled]);
          ACanvas.Font.Color := StyleServices.GetStyleFontColor(FontColorStates[Editor.Enabled]);
        end
        else
        {$ENDIF COMPILER16_UP}
        Brush.Color := TEd(Editor).Color;
        ACanvas.TextRect(EditRect, X, EditRect.Top, S);
      end;
    end;
  finally
    ACanvas.Handle := 0;
    if Msg.DC = 0 then
      EndPaint(TEd(Editor).Handle, PS);
  end;
end;

//=== { TAutoCompleteSource } ================================================

constructor TAutoCompleteSource.Create(AComboEdit: TJvCustomComboEdit; const StartIndex: Integer);
begin
  inherited Create;
  FComboEdit := AComboEdit;
  FCurrentIndex := StartIndex;
end;

function TAutoCompleteSource.Clone(out enm: IEnumString): HRESULT;
begin
  { Save state }
  try
    enm := TAutoCompleteSource.Create(FComboEdit, FCurrentIndex);
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TAutoCompleteSource.Next(celt: Integer; out elt;
  pceltFetched: PLongint): HRESULT;
var
  Fetched: Integer;
  S: string;
  Ptr: POleStr;
  Size: Integer;
begin
  if Pointer(elt) = nil then
  begin
    Result := E_FAIL;
    Exit;
  end;

  Fetched := 0;

  while (Fetched < celt) and (FCurrentIndex < FComboEdit.AutoCompleteItems.Count) do
  begin
    S := FComboEdit.AutoCompleteItems[FCurrentIndex];
    Size := (Length(S) + 1) * SizeOf(WideChar);
    Ptr := CoTaskMemAlloc(Size);
    if Ptr = nil then
    begin
      Result := E_OUTOFMEMORY;
      Exit;
    end;
    StringToWideChar(S, Ptr, Size);

    TOleStrList(elt)[Fetched] := Ptr;

    Inc(FCurrentIndex);
    Inc(Fetched);
  end;

  if Assigned(pceltFetched) then
    pceltFetched^ := Fetched;

  if Fetched = celt then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TAutoCompleteSource.Reset: HRESULT;
begin
  FCurrentIndex := 0;
  Result := S_OK;
end;

function TAutoCompleteSource.Skip(celt: Integer): HRESULT;
begin
  Inc(FCurrentIndex, celt);
  if FCurrentIndex < FComboEdit.AutoCompleteItems.Count then
    Result := S_OK
  else
  begin
    Result := S_FALSE;
    FCurrentIndex := FComboEdit.AutoCompleteItems.Count;
  end;
end;

//=== { TDateHook } ==========================================================

procedure TDateHook.Add;
begin
  if FCount = 0 then
    Hook;
  Inc(FCount);
end;

procedure TDateHook.Delete;
begin
  if FCount > 0 then
    Dec(FCount);
  if FCount = 0 then
    UnHook;
end;

function TDateHook.FormatSettingsChange(var Msg: TMessage): Boolean;
begin
  Result := False;
  if (Msg.Msg = WM_WININICHANGE) and Application.UpdateFormatSettings then
  begin
    // Let the application obj do the changing; we receive the message
    // before the application obj, thus jump over it:
    PostMessage(Application.Handle, WM_NULL, 0, 0);
    FWinIniChangeReceived := True;
  end
  else
  if (Msg.Msg = WM_NULL) and FWinIniChangeReceived then
  begin
    FWinIniChangeReceived := False;
    DateFormatChanged;
  end;
end;

procedure TDateHook.Hook;
begin
  if not FHooked then
  begin
    Application.HookMainWindow(FormatSettingsChange);
    FHooked := True;
  end;
end;

procedure TDateHook.UnHook;
begin
  if FHooked then
  begin
    Application.UnhookMainWindow(FormatSettingsChange);
    FHooked := False;
  end;
end;

//=== { TJvCustomComboEditDataConnector } ====================================

constructor TJvCustomComboEditDataConnector.Create(AEdit: TJvCustomComboEdit);
begin
  inherited Create;
  FEdit := AEdit;
end;

procedure TJvCustomComboEditDataConnector.RecordChanged;
begin
  if Field.IsValid then
  begin
    FEdit.ReadOnly := not Field.CanModify;
    FEdit.Text := Field.AsString;
  end
  else
  begin
    FEdit.Text := '';
    FEdit.ReadOnly := True;
  end;
end;

procedure TJvCustomComboEditDataConnector.UpdateData;
begin
  Field.AsString := FEdit.Text;
  FEdit.Text := Field.AsString; // update to stored value
end;

type
  TJvBtnWinControl = class(TWinControl)
  protected
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  end;

procedure TJvBtnWinControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  {$IFDEF HAS_UNIT_VCL_THEMES}
  if ((Owner as TJvCustomComboEdit).ImageKind in [ikDropDown, ikDatePicker]) and
     StyleServices.Enabled and not TJvCustomComboEdit(Owner).ButtonFlat then
    Message.Result := 1 // the button is alClient and paints everything, reduces flicker
  else
  {$ENDIF HAS_UNIT_VCL_THEMES}
    inherited;
end;

//=== { TJvCustomComboEdit } =================================================

constructor TJvCustomComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataConnector := CreateDataConnector;
  ControlStyle := ControlStyle + [csCaptureMouse];
  Height := 21;
  FDirectInput := True;
  FClickKey := scAltDown;
  FPopupAlign := epaRight;
  FBtnControl := TJvBtnWinControl.Create(Self);
  with FBtnControl do
    ControlStyle := ControlStyle + [csReplicatable];
  FBtnControl.Width := DefEditBtnWidth;
  FBtnControl.Height := 17;
  FBtnControl.Visible := True;
  FBtnControl.Parent := Self;
  FBtnControl.Align := alCustom;
  FButton := TJvEditButton.Create(Self); // this happens too late in some cases, causing a crash.
  FButton.SetBounds(0, 0, FBtnControl.Width, FBtnControl.Height);
  FButton.Visible := True;
  FButton.Align := alClient;
  FButton.OnClick := EditButtonClick;
  FButton.Parent := FBtnControl;

  FAlwaysEnableButton := False;
  FDisabledColor := clWindow;
  FDisabledTextColor := clGrayText;
  FGroupIndex := -1;
  FStreamedButtonWidth := -1;
  FImageKind := ikCustom;
  FImageIndex := -1;
  FNumGlyphs := 1;
  FAutoCompleteItems := TStringList.Create;
  FAutoCompleteOptions := [];
  FCheckOnExit := True;

  // Move to class contructor when Delphi 2010 is the minimum version
  if GCoInitialized >= 0 then
  begin
    Inc(GCoInitialized);
    if GCoInitialized = 1 then
      if not Succeeded(CoInitialize(nil)) then
        GCoInitialized := -1;
  end;
  inherited OnKeyDown := LocalKeyDown;
end;

destructor TJvCustomComboEdit.Destroy;
begin
  PopupCloseUp(Self, False);
  FButton.OnClick := nil;
  DestroyAutoComplete;
  FAutoCompleteItems.Free;
  FDataConnector.Free;
  inherited Destroy;

  // call after WM_DESTROY

  // Move to class destructor when Delphi 2010 is the minimum version
  if GCoInitialized > 0 then
  begin
    Dec(GCoInitialized);
    if GCoInitialized = 0 then
      CoUninitialize;
  end;
end;

function TJvCustomComboEdit.AcceptPopup(var Value: Variant): Boolean;
begin
  Result := True;
end;

procedure TJvCustomComboEdit.AcceptValue(const Value: Variant);
begin
  if Text <> VarToStr(Value) then
  begin
    Text := Value;
    Modified := True;
    UpdatePopupVisible;
    //DoChange; (ahuser) "Text := Value" triggers Change;
  end;
  if Assigned(FOnPopupValueAccepted) then
    FOnPopupValueAccepted(Self);
end;

procedure TJvCustomComboEdit.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or not Assigned(Self.Images) then
        Self.Images := TCustomImageList(ActionList.Images);
      if not CheckDefaults or Self.Enabled then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.HelpContext = 0) then
        Self.HelpContext := HelpContext;
      if not CheckDefaults or (Self.Hint = '') then
        Self.ButtonHint := Hint;
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
      if not CheckDefaults or (Self.ClickKey = scNone) then
        Self.ClickKey := ShortCut;
      if not CheckDefaults or Self.Visible then
        Self.Visible := Visible;
      if not CheckDefaults or not Assigned(Self.OnButtonClick) then
        Self.OnButtonClick := OnExecute;
    end;
end;

procedure TJvCustomComboEdit.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  // Get text height
  DC := GetDC(HWND_DESKTOP);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(HWND_DESKTOP, DC);

  // If necessary reserve space for border
  I := 0;
  if BorderStyle <> bsNone then
  begin
    if not Flat then
      I := 8
    else
      I := 6;
    I := GetSystemMetrics(SM_CYBORDER) * I;
  end;

  if Height < Metrics.tmHeight + I then
    Height := Metrics.tmHeight + I;
end;

procedure TJvCustomComboEdit.AdjustSize;
var
  MinHeight: Integer;
begin
  inherited AdjustSize;
  if not (csLoading in ComponentState) then
  begin
    MinHeight := GetMinHeight;
    { text edit bug: if size to less than MinHeight, then edit ctrl does
      not display the text }
    if Height < MinHeight then
    begin
      Height := MinHeight;
      Exit;
    end;
  end
  else
  begin
    if (FPopup <> nil) and (csDesigning in ComponentState) then
      FPopup.SetBounds(0, Height + 1, 10, 10);
  end;
  UpdateControls;
  UpdateMargins;
end;

{$IFDEF JVCLThemesEnabled}
function TJvCustomComboEdit.GetDatePickerThemeButtonMinTextSize: Integer;
begin
  Result := 0;
end;

function TJvCustomComboEdit.GetDatePickerThemeButtonWidth: Integer;
begin
  if Width - DefDatePickerThemeButtonWidth <= GetDatePickerThemeButtonMinTextSize + 8 then
    Result := GetSystemMetrics(SM_CXVSCROLL)
  else
    Result := DefDatePickerThemeButtonWidth;
end;

procedure TJvCustomComboEdit.AutoSizeEditButton;
var
  W: Integer;
begin
  if (FImageKind = ikDatePicker) and StyleServices.Enabled and IsDatePickerThemeDataAvailable then
  begin
    if ShowButton and not (csLoading in ComponentState) then
    begin
      W := GetDatePickerThemeButtonWidth;

      // Set button width without RecreateWnd
      FSavedButtonWidth := W;
      FButton.Width := W;
      FBtnControl.SetBounds(FBtnControl.Left + FBtnControl.Width - W, FBtnControl.Top, W, FBtnControl.Height);
      FButton.ControlStyle := FButton.ControlStyle + [csFixedWidth];
      UpdateControls;
      UpdateMargins;
    end;
  end;
end;

procedure TJvCustomComboEdit.Resize;
begin
  inherited Resize;
  AutoSizeEditButton;
end;
{$ENDIF JVCLThemesEnabled}

procedure TJvCustomComboEdit.AsyncPopupCloseUp(Accept: Boolean);
begin
  if not (csDestroying in ComponentState) then
    PostMessage(Handle, CM_POPUPCLOSEUP, Ord(Accept), 0);
end;

function TJvCustomComboEdit.BtnWidthStored: Boolean;
begin
  if (FImageKind = ikDefault) and (DefaultImages <> nil) and (DefaultImageIndex >= 0) then
    Result := ButtonWidth <> Max(DefaultImages.Width + 6, DefEditBtnWidth)
  else
  if FImageKind = ikDatePicker then
    Result := False
  else
  if FImageKind = ikDropDown then
    Result := ButtonWidth <> GetSystemMetrics(SM_CXVSCROLL)
  else
    Result := ButtonWidth <> DefEditBtnWidth;
end;

procedure TJvCustomComboEdit.ButtonClick;
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self);

  if (FPopup <> nil) and FPopupVisible then
    PopupCloseUp(FPopup, True)
  else
    PopupDropDown(True);
end;

procedure TJvCustomComboEdit.Change;
begin
  DataConnector.Modify;
  if not PopupVisible then
    DoChange
  else
    PopupChange;
end;

procedure TJvCustomComboEdit.CMBiDiModeChanged(var Msg: TMessage);
begin
  inherited;
  if FPopup <> nil then
    FPopup.BiDiMode := BiDiMode;
end;

procedure TJvCustomComboEdit.CMCancelMode(var Msg: TCMCancelMode);
begin
  if (Msg.Sender <> Self) and (Msg.Sender <> FPopup) and
    (Msg.Sender <> FButton) and ((FPopup <> nil) and
    not FPopup.ContainsControl(Msg.Sender)) then
    PopupCloseUp(FPopup, False);
end;

procedure TJvCustomComboEdit.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  DoCtl3DChanged;
end;

procedure TJvCustomComboEdit.CMExit(var Message: TCMExit);
begin
  Inc(FInCMExit); // used for FCheckOnExit
  try
    inherited;
  finally
    Dec(FInCMExit);
  end;
end;

procedure TJvCustomComboEdit.CMPopupCloseup(var Msg: TMessage);
begin
  PopupCloseUp(Self, Boolean(Msg.WParam));
end;

procedure TJvCustomComboEdit.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  { Ignore tabs when popup is visible }
  if PopupVisible and (Msg.CharCode = VK_TAB) then
    Msg.Result := 1;
end;

procedure TJvCustomComboEdit.CNCtlColor(var Msg: TMessage);
var
  TextColor: Longint;
begin
  inherited;
  TextColor := ColorToRGB(Font.Color);
  if not Enabled and (ColorToRGB(Color) <> ColorToRGB(clGrayText)) then
    TextColor := ColorToRGB(clGrayText);
  SetTextColor(Msg.WParam, TextColor);
end;

procedure TJvCustomComboEdit.CreateAutoComplete;
begin
  if HandleAllocated and not (csDesigning in ComponentState) and
    {not Assigned(FAutoCompleteIntf) and} (AutoCompleteOptions <> []) then
  begin
    { Create the autocomplete object. }
    if Succeeded(CoCreateInstance(CLSID_AutoComplete, nil, CLSCTX_INPROC_SERVER, IAutoComplete,
      FAutoCompleteIntf)) then
    begin
      { Initialize the autocomplete object. }
      FAutoCompleteIntf.Init(Self.Handle, GetAutoCompleteSource, nil, nil);
    end
    else
      FAutoCompleteIntf := nil;
  end;
end;

procedure TJvCustomComboEdit.DestroyAutoComplete;
begin
  FAutoCompleteIntf := nil;
end;

procedure TJvCustomComboEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array [TAlignment] of LongWord = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or
    ES_MULTILINE or WS_CLIPCHILDREN or Alignments[FAlignment];
end;

procedure TJvCustomComboEdit.CreatePopup;
begin
  { Notification }
end;

procedure TJvCustomComboEdit.CreateWnd;
begin
  inherited CreateWnd;
  UpdateControls;
  UpdateMargins;
  if AutoCompleteOptions <> [] then
  begin
    CreateAutoComplete;
    UpdateAutoComplete;
  end;
end;

procedure TJvCustomComboEdit.CustomAlignPosition(Control: TControl;
  var NewLeft, NewTop, NewWidth, NewHeight: Integer;
  var AlignRect: TRect; AlignInfo: TAlignInfo);
begin
  if Control = FBtnControl then
    UpdateBtnBounds(NewLeft, NewTop, NewWidth, NewHeight);
end;

class function TJvCustomComboEdit.DefaultImageIndex: TImageIndex;
begin
  Result := -1;
end;

class function TJvCustomComboEdit.DefaultImages: TCustomImageList;
begin
  if GDefaultComboEditImagesList = nil then
    GDefaultComboEditImagesList := TImageList.CreateSize(14, 12);
  Result := TCustomImageList(GDefaultComboEditImagesList);
end;

procedure TJvCustomComboEdit.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('GlyphKind', ReadGlyphKind, nil, False);
  Filer.DefineProperty('Ctl3D', ReadCtl3D, nil, False);
  Filer.DefineProperty('ParentCtl3D', ReadParentCtl3D, nil, False);
end;

procedure TJvCustomComboEdit.DestroyWnd;
begin
  inherited DestroyWnd;
  { Mantis #3642 }
  DestroyAutoComplete;
end;

procedure TJvCustomComboEdit.DoChange;
begin
  inherited Change;
end;

procedure TJvCustomComboEdit.DoClick;
begin
  EditButtonClick(Self);
end;

procedure TJvCustomComboEdit.WMClear(var Msg: TMessage);
begin
  Text := '';
end;

procedure TJvCustomComboEdit.WMCut(var Msg: TMessage);
begin
  if FDirectInput and not ReadOnly then
    inherited;
end;

procedure TJvCustomComboEdit.WMPaste(var Msg: TMessage);
begin
  if FDirectInput and not ReadOnly then
    inherited;
  UpdateGroup;
end;

procedure TJvCustomComboEdit.DoCtl3DChanged;
begin
  UpdateMargins;
  UpdateControls;
end;

procedure TJvCustomComboEdit.CMFixCaretPosition(var Msg: TMessage);
begin
  SelStart := SendMessage(Handle, EM_CHARFROMPOS, 0, Msg.LParam);
  inherited;
end;

procedure TJvCustomComboEdit.DoEnter;
var
  Pt: TPoint;
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll
  else
  if IsMasked and (csLButtonDown in ControlState) and
     (GetWindowLong(Handle, GWL_STYLE) and ES_MULTILINE <> 0) then
  begin
    { ES_MULTILINE causes the edit to place the caret at the wrong location. }
    if GetCursorPos(Pt) then
    begin
      Pt := ScreenToClient(Pt);
      PostMessage(Handle, CM_FIXCARETPOSITION, 0, MakeLong(Word(Pt.X), Word(Pt.Y)));
    end;
  end;
  inherited DoEnter;
end;

procedure TJvCustomComboEdit.DoExit;
begin
  DataConnector.UpdateRecord;
  inherited DoExit;
end;

procedure TJvCustomComboEdit.FocusKilled(NextWnd: THandle);
var
  Sender: TWinControl;
begin
  inherited FocusKilled(NextWnd);
  FFocused := Screen.ActiveControl <> Self;
  if not FFocused then
  begin
    Sender := FindControl(NextWnd);
    if (Sender <> Self) and (Sender <> FPopup) and
      {(Sender <> FButton)} ((FPopup <> nil) and
      not FPopup.ContainsControl(Sender)) then
    begin
      { MSDN : While processing this message (WM_KILLFOCUS), do not make any
               function calls that display or activate a window.
      }
      AsyncPopupCloseUp(False);
    end;
  end;
end;

function TJvCustomComboEdit.DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean;
begin
  Result := True;
  if csDestroying in ComponentState then
    { (rb) Implementation diffs; some return True other False }
    Exit;
  if Enabled or (FDisabledColor = clNone) or (FDisabledColor = clDefault) or (FDisabledColor = clWindow) then
    Result := inherited DoEraseBackground(Canvas, Param)
  else
  begin
    {$IFDEF COMPILER16_UP}
    if StyleServices.Enabled and not StyleServices.IsSystemStyle then
    begin
      // Ignore FDisabledColor. The Style dictates the color
      Result := inherited DoEraseBackground(Canvas, Param);
    end
    else
    {$ENDIF COMPILER16_UP}
    begin
      Canvas.Brush.Color := FDisabledColor;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(ClientRect);
    end;
  end;
end;

procedure TJvCustomComboEdit.FocusSet(PrevWnd: THandle);
begin
  inherited FocusSet(PrevWnd); // triggers OnExit and OnEnter => Focus could be changed
  FFocused := Screen.ActiveControl = Self;
  if FFocused then
    SetShowCaret;
end;

procedure TJvCustomComboEdit.EditButtonClick(Sender: TObject);
begin
  if not FReadOnly or AlwaysEnableButton then
    ButtonClick;
end;

function TJvCustomComboEdit.EditCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

procedure TJvCustomComboEdit.EnabledChanged;
begin
  inherited EnabledChanged;
  Invalidate;
  UpdateButtonEnabled;
  if Assigned(FOnEnabledChanged) then
    FOnEnabledChanged(Self);
end;

procedure TJvCustomComboEdit.FontChanged;
begin
  inherited FontChanged;
  if HandleAllocated then
    UpdateMargins;
end;

function TJvCustomComboEdit.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TJvCustomComboEditActionLink;
end;

function TJvCustomComboEdit.GetAutoCompleteSource: IEnumString;
begin
  Result := TAutoCompleteSource.Create(Self, 0);
end;

function TJvCustomComboEdit.GetButtonFlat: Boolean;
begin
  Result := FButton.Flat;
end;

function TJvCustomComboEdit.GetButtonHint: string;
begin
  Result := FButton.Hint;
end;

function TJvCustomComboEdit.GetButtonWidth: Integer;
begin
  if ShowButton then
    Result := FButton.Width
  else
    Result := FSavedButtonWidth;
end;

function TJvCustomComboEdit.GetDirectInput: Boolean;
begin
  Result := FDirectInput;
end;

function TJvCustomComboEdit.GetFlat: Boolean;
begin
  Result := not Ctl3D;
end;

function TJvCustomComboEdit.GetParentFlat: Boolean;
begin
  Result := ParentCtl3D;
end;

procedure TJvCustomComboEdit.SetFlat(const Value: Boolean);
begin
  Ctl3D := not Value;
end;

procedure TJvCustomComboEdit.SetParentFlat(const Value: Boolean);
begin
  ParentCtl3D := Value;
end;

function TJvCustomComboEdit.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

function TJvCustomComboEdit.GetGlyphKind: TGlyphKind;
begin
  Result := TGlyphKind(FImageKind);
end;

procedure TJvCustomComboEdit.GetInternalMargins(var ALeft, ARight: Integer);
begin
  ARight := ARight + FBtnControl.Width;
end;

function TJvCustomComboEdit.GetMinHeight: Integer;
var
  I: Integer;
begin
  I := GetTextHeight;
  if BorderStyle = bsSingle then
    I := I + GetSystemMetrics(SM_CYBORDER) * 4 + 1;
  Result := I;
end;

function TJvCustomComboEdit.GetNumGlyphs: TNumGlyphs;
begin
  if ImageKind <> ikCustom then
    Result := FNumGlyphs
  else
    Result := FButton.NumGlyphs;
end;

function TJvCustomComboEdit.GetPopupValue: Variant;
begin
  if FPopup is TJvPopupWindow then
    Result := TJvPopupWindow(FPopup).GetValue
  else
    Result := '';
end;

function TJvCustomComboEdit.GetPopupVisible: Boolean;
begin
  Result := (FPopup <> nil) and FPopupVisible;
end;

function TJvCustomComboEdit.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TJvCustomComboEdit.GetSettingCursor: Boolean;
begin
  Result := TCustomMaskEditAccessPrivate(Self).FSettingCursor;
end;

function TJvCustomComboEdit.GetShowButton: Boolean;
begin
  Result := FBtnControl.Visible;
end;

function TJvCustomComboEdit.GetTextHeight: Integer;
var
  DC: HDC;
  SaveFont: HFONT;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(HWND_DESKTOP);
  try
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(HWND_DESKTOP, DC);
  end;
  Result := Metrics.tmHeight;
end;

procedure TJvCustomComboEdit.HidePopup;
begin
  if FPopup is TJvPopupWindow then
  begin
    TJvPopupWindow(FPopup).Hide;
    if Assigned(FOnPopupHidden) then
      FOnPopupHidden(Self);
  end;
end;

function TJvCustomComboEdit.IsCustomGlyph: Boolean;
begin
  Result := Assigned(Glyph) and (ImageKind = ikCustom);
end;

function TJvCustomComboEdit.IsFlatStored: Boolean;
begin
  { Same as IsCtl3DStored }
  Result := not ParentCtl3D;
end;

function TJvCustomComboEdit.IsImageIndexStored: Boolean;
begin
  Result :=
    not (ActionLink is TJvCustomComboEditActionLink) or
    not (ActionLink as TJvCustomComboEditActionLink).IsImageIndexLinked;
end;

procedure TJvCustomComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Form: TCustomForm;
begin
  UpdateGroup;

  Form := GetParentForm(Self);
  if (ssCtrl in Shift) then
    case Key of
      VK_RETURN:
        if (Form <> nil) {and Form.KeyPreview} then
        begin
          TWinControlAccessProtected(Form).KeyDown(Key, Shift);
          Key := 0;
        end;
      VK_TAB:
        if (Form <> nil) {and Form.KeyPreview} then
        begin
          TWinControlAccessProtected(Form).KeyDown(Key, Shift);
          Key := 0;
        end;
    end;
  //Original
  inherited KeyDown(Key, Shift);
  if (FClickKey = ShortCut(Key, Shift)) and (ButtonWidth > 0) then
  begin
    EditButtonClick(Self);
    Key := 0;
  end;
end;

procedure TJvCustomComboEdit.KeyPress(var Key: Char);
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);

  if (Key = Cr) or (Key = Esc) or ((Key = Lf) and PopupVisible) then
  begin
    if PopupVisible then
    begin
      PopupCloseUp(FPopup, Key <> Esc);
      Key := #0;
    end
    else
    begin
      { must catch and remove this, since is actually multi-line }
      if (Form <> nil) and (Form.Perform(CM_DIALOGKEY, Byte(Key), 0) <> 0) then
      begin
        Key := #0;
        Exit;
      end;
      if Key = Cr then
      begin
        inherited KeyPress(Key);
        Key := #0;
        Exit;
      end;
    end;
  end;
  if (Key = Tab) or (Key = Lf) then
  begin
    Key := #0;
    { (rb) Next code has no use because Key = #0? }
    if (Form <> nil) {and Form.KeyPreview} then
      TWinControlAccessProtected(Form).KeyPress(Key);
  end;
  inherited KeyPress(Key);

  if (Key = #27) and DataConnector.Active then
  begin
    DataConnector.Reset;
    Key := #0;
  end;
end;

procedure TJvCustomComboEdit.Loaded;
begin
  inherited Loaded;
  if FStreamedButtonWidth >= 0 then
  begin
    SetButtonWidth(FStreamedButtonWidth);
    if FStreamedFixedWidth then
      with FButton do
        ControlStyle := ControlStyle + [csFixedWidth];
  end;

  UpdateButtonEnabled;
  UpdateControls;
  {$IFDEF JVCLThemesEnabled}
  AutoSizeEditButton;
  {$ENDIF JVCLThemesEnabled}
  UpdateMargins;
end;

procedure TJvCustomComboEdit.LocalKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  UpdateGroup;
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Sender, Key, Shift);
end;

procedure TJvCustomComboEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (FPopup <> nil) and (Button = mbLeft) then
  begin
    if CanFocus then
      SetFocus;
    if not FFocused then
      Exit;
    if FPopupVisible then
      PopupCloseUp(FPopup, False);
    {else
     if (not ReadOnly or AlwaysEnable) and (not DirectInput) then
       PopupDropDown(True);}
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvCustomComboEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;
  if (Operation = opRemove) and (AComponent = FPopup) then
    FPopup := nil;
end;

procedure TJvCustomComboEdit.PopupChange;
begin
  if Assigned(FOnPopupChange) then
    FOnPopupChange(Self);
end;

procedure TJvCustomComboEdit.ResetPopupValue;
begin
  Reset;
end;

procedure TJvCustomComboEdit.PopupCloseUp(Sender: TObject; Accept: Boolean);
var
  AValue: Variant;
begin
  if (FPopup <> nil) and FPopupVisible then
  begin
    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    AValue := GetPopupValue;
    if FButton <> nil then
      FButton.PopupVisible := False;
    HidePopup;
    try
      try
        if CanFocus and ParentFormVisible(Self) then
        begin
          SetFocus;
          if GetFocus = Handle then
            SetShowCaret;
        end;
      except
        { ignore exceptions }
      end;
      SetDirectInput(DirectInput);
      Invalidate;
      try
        if Accept and AcceptPopup(AValue) and EditCanModify then
        begin
          AcceptValue(AValue);
          if FFocused then
            inherited SelectAll;
        end
        else
          ResetPopupValue;
      except
        ResetPopupValue;
        raise;
      end;
    finally
      FPopupVisible := False;
    end;
  end;
end;

procedure TJvCustomComboEdit.PopupDropDown(DisableEdit: Boolean);
type
  TJvSizeRect = record
    Top: Integer;
    Left: Integer;
    Width: Integer;
    Height: Integer;
  end;
var
  P: TPoint;
  Y: Integer;
  SR: TJvSizeRect;
  Monitor: TMonitor;
  Rect: TRect;
begin
  if not ((ReadOnly and not FAlwaysShowPopup) or FPopupVisible) then
  begin
    CreatePopup;
    if FPopup = nil then
      Exit;

    P := Parent.ClientToScreen(Point(Left, Top));
    Monitor := FindMonitor(MonitorFromWindow(Handle, MONITOR_DEFAULTTONEAREST));
    Rect := GetWorkAreaRect(Monitor);
    SR.Top := Rect.Top;
    SR.Left := Rect.Left;
    SR.Width := Rect.Right - Rect.Left;
    SR.Height := Rect.Bottom - Rect.Top;
    Y := P.Y + Height;
    if Y + FPopup.Height > SR.Top + SR.Height then
      Y := P.Y - FPopup.Height;
    case FPopupAlign of
      epaRight:
        begin
          Dec(P.X, FPopup.Width - Width);
          if P.X < SR.Left then
            Inc(P.X, FPopup.Width - Width);
        end;
      epaLeft:
        if P.X + FPopup.Width > SR.Left + SR.Width then
          Dec(P.X, FPopup.Width - Width);
    end;
    if P.X < SR.Left then
      P.X := SR.Left
    else
    if P.X + FPopup.Width > SR.Left + SR.Width then
      P.X := SR.Left + SR.Width - FPopup.Width;
    if Text <> '' then
      SetPopupValue(Text)
    else
      SetPopupValue(Null);
    if CanFocus then
      SetFocus;
    ShowPopup(Point(P.X, Y));
    if FButton <> nil then
      FButton.PopupVisible := True;
    FPopupVisible := True;
    if DisableEdit then
    begin
      inherited ReadOnly := True;
      HideCaret(Handle);
    end;
  end;
end;

procedure TJvCustomComboEdit.ReadCtl3D(Reader: TReader);
begin
  Flat := not Reader.ReadBoolean;
end;

procedure TJvCustomComboEdit.ReadParentCtl3D(Reader: TReader);
begin
  ParentFlat := Reader.ReadBoolean;
end;

procedure TJvCustomComboEdit.ReadGlyphKind(Reader: TReader);
const
  sEnumValues: array [TGlyphKind] of string =
    ('gkCustom', 'gkDefault', 'gkDropDown', 'gkEllipsis');
var
  S: string;
  GlyphKind: TGlyphKind;
begin
  { No need to drag in TypInfo.pas }
  S := Reader.ReadIdent;
  for GlyphKind := Low(TGlyphKind) to High(TGlyphKind) do
    if SameText(S, sEnumValues[GlyphKind]) then
    begin
      ImageKind := TJvImageKind(GlyphKind);
      Break;
    end;
end;

procedure TJvCustomComboEdit.RecreateGlyph;
var
  NewGlyph: TBitmap;

  function CreateEllipsisGlyph: TBitmap;
  var
    W, g, I: Integer;
  begin
    Result := TBitmap.Create;
    with Result do
    try
      Monochrome := True;
      Width := Max(1, FButton.Width - 6);
      Height := 4;
      W := 2;
      g := (Result.Width - 3 * W) div 2;
      if g <= 0 then
        g := 1;
      if g > 3 then
        g := 3;
      I := (Width - 3 * W - 2 * g) div 2;
      PatBlt(Canvas.Handle, I, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + g + W, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + 2 * g + 2 * W, 1, W, W, BLACKNESS);
    except
      Free;
      raise;
    end;
  end;

begin
  { Delay until button is shown }
  if not ShowButton then
    Exit;

  if FImageKind in [ikDropDown, ikEllipsis, ikDatePicker] then
  begin
    FButton.ImageIndex := -1;
    FButton.NumGlyphs := 1;
  end;
  {$IFDEF JVCLThemesEnabled}
  FButton.FDrawThemedDropDownBtn := FImageKind = ikDropDown;
  FButton.FDrawThemedDatePickerBtn := FImageKind = ikDatePicker;
  {$ENDIF JVCLThemesEnabled}

  case FImageKind of
    ikDropDown, ikDatePicker: // DatePicker uses the same Glyph if not supported by the OS
      begin
        {$IFDEF JVCLThemesEnabled}
        { When XP Themes are enabled, ButtonFlat = False, GlyphKind = gkDropDown then
          the glyph is the default themed dropdown button. When ButtonFlat = True, we
          can't use that default dropdown button (because we then use toolbar buttons,
          and there is no themed dropdown toolbar button) }
        FButton.FDrawThemedDropDownBtn :=
          StyleServices.Enabled and not ButtonFlat;
        if FButton.FDrawThemedDropDownBtn then
        begin
          FButton.ButtonGlyph.Glyph := nil;
          FButton.Invalidate;
        end
        else
        {$ENDIF JVCLThemesEnabled}
        begin
          LoadDefaultBitmap(FButton.ButtonGlyph.Glyph, OBM_COMBO);
          FButton.Invalidate;
        end;
      end;
    ikEllipsis:
      begin
        NewGlyph := CreateEllipsisGlyph;
        try
          FButton.ButtonGlyph.Glyph := NewGlyph;
          FButton.Invalidate;
        finally
          NewGlyph.Destroy;
        end;
      end;
  else
//    FButton.ButtonGlyph.Glyph := nil;
    FButton.Invalidate;
  end;
end;

procedure TJvCustomComboEdit.SelectAll;
begin
  if DirectInput then
    inherited SelectAll;
end;

procedure TJvCustomComboEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomComboEdit.SetAlwaysEnableButton(const Value: Boolean);
begin
  if Value <> FAlwaysEnableButton then
  begin
    FAlwaysEnableButton := Value;
    UpdateButtonEnabled;
  end;
end;

procedure TJvCustomComboEdit.UpdateButtonEnabled;
begin
  if FButton <> nil then
    FButton.Enabled := Enabled and (not FReadOnly or AlwaysEnableButton);
end;

procedure TJvCustomComboEdit.SetAutoCompleteItems(Strings: TStrings);
begin
  FAutoCompleteItems.Assign(Strings);
end;

procedure TJvCustomComboEdit.SetAutoCompleteOptions(const Value: TJvAutoCompleteOptions);
begin
  if Value <> FAutoCompleteOptions then
  begin
    FAutoCompleteOptions := Value;

    if not Assigned(FAutoCompleteIntf) then
      CreateAutoComplete;
    UpdateAutoComplete;
  end;
end;

procedure TJvCustomComboEdit.SetButtonFlat(const Value: Boolean);
begin
  FButton.Flat := Value;
  {$IFDEF JVCLThemesEnabled}
  { If XP Themes are enabled, ButtonFlat = False, GlyphKind = gkDropDown then
    the glyph is the default themed dropdown button. When ButtonFlat = True, we
    can't use that default dropdown button, so we have to recreate the glyph
    in this special case }
  if StyleServices.Enabled and (ImageKind in [ikDropDown, ikDatePicker]) then
    RecreateGlyph;
  {$ENDIF JVCLThemesEnabled}
end;

procedure TJvCustomComboEdit.SetButtonHint(const Value: string);
begin
  FButton.Hint := Value;
end;

procedure TJvCustomComboEdit.SetButtonWidth(Value: Integer);
begin
  if csLoading in ComponentState then
  begin
    FStreamedButtonWidth := Value;
    FStreamedFixedWidth := False;
  end
  else
  if not ShowButton then
    FSavedButtonWidth := Value
  else
  if (ButtonWidth <> Value) {or ((Value > 0) <> ShowButton)} then
  begin
    {$IFDEF JVCLThemesEnabled}
    if (ImageKind = ikDatePicker) and (Value = DefDatePickerThemeButtonWidth) then
      Value := GetDatePickerThemeButtonWidth;
    {$ENDIF JVCLThemesEnabled}

    if Value > 1 then
      FBtnControl.Visible := True
    else
    begin
      FSavedButtonWidth := ButtonWidth;
      FBtnControl.Visible := False;
    end;
    if csCreating in ControlState then
    begin
      FBtnControl.Width := Value;
      FButton.Width := Value;
      with FButton do
        ControlStyle := ControlStyle - [csFixedWidth];
      { Some glyphs are size dependant (ellipses), thus recreate on size changes }
      RecreateGlyph;
    end
    else
    if (Value <> ButtonWidth) and
      ((Assigned(Parent) and (Value < ClientWidth)) or
      (not Assigned(Parent) and (Value < Width))) then
    begin
      FBtnControl.SetBounds(FBtnControl.Left + FBtnControl.Width - Value,
        FBtnControl.Top, Value, FBtnControl.Height);
      FButton.Width := Value;
      with FButton do
        ControlStyle := ControlStyle - [csFixedWidth];
      if HandleAllocated then
        RecreateWnd;
      { Some glyphs are size dependant (ellipses), thus recreate on size changes }
      RecreateGlyph;
    end;
  end;
end;

procedure TJvCustomComboEdit.SetClipboardCommands(const Value: TJvClipboardCommands);
begin
  if ClipboardCommands <> Value then
  begin
    inherited SetClipboardCommands(Value);
    if ReadOnly and not (ClipboardCommands <= [caCopy]) then
      ClipboardCommands := [caCopy];
  end;
end;

procedure TJvCustomComboEdit.SetDirectInput(Value: Boolean);
begin
  inherited ReadOnly := not Value or FReadOnly;
  FDirectInput := Value;
end;

procedure TJvCustomComboEdit.SetDisabledColor(const Value: TColor);
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    if not Enabled then
      Invalidate;
  end;
end;

procedure TJvCustomComboEdit.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor <> Value then
  begin
    FDisabledTextColor := Value;
    if not Enabled then
      Invalidate;
  end;
end;

procedure TJvCustomComboEdit.SetGlyph(Value: TBitmap);
begin
  ImageKind := ikCustom;
  FButton.Glyph := Value;
  FNumGlyphs := FButton.NumGlyphs;
end;

procedure TJvCustomComboEdit.SetGlyphKind(Value: TGlyphKind);
begin
  ImageKind := TJvImageKind(Value);
end;

procedure TJvCustomComboEdit.SetGroupIndex(const Value: Integer);
begin
  FGroupIndex := Value;
  UpdateGroup;
end;

procedure TJvCustomComboEdit.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if FImageKind = ikCustom then
      FButton.ImageIndex := FImageIndex;
  end;
end;

procedure TJvCustomComboEdit.SetImageKind(const Value: TJvImageKind);
var
  SysButtonWidth: Integer;
begin
  if FImageKind <> Value then
  begin
    FImageKind := Value;
    RecreateGlyph;
    case FImageKind of
      ikCustom:
        begin
          FButton.Images := FImages;
          FButton.ImageIndex := FImageIndex;
          FButton.NumGlyphs := FNumGlyphs;
        end;
      ikDefault:
        begin
          FButton.Images := DefaultImages;
          FButton.ImageIndex := DefaultImageIndex;
          { Default glyphs have a default width }
          if Assigned(FButton.Images) and (FButton.ImageIndex >= 0) then
            ButtonWidth := Max(FButton.Images.Width + 6, FButton.Width)
        end;
      ikDropDown, ikDatePicker:
        begin
          SysButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
          {$IFDEF JVCLThemesEnabled}
          if (FImageKind = ikDatePicker) and StyleServices.Enabled and IsDatePickerThemeDataAvailable then
            SysButtonWidth := DefDatePickerThemeButtonWidth;
          {$ENDIF JVCLThemesEnabled}

          if csLoading in ComponentState then
          begin
            if (FStreamedButtonWidth < 0) or FStreamedFixedWidth then
            begin
              ButtonWidth := SysButtonWidth;
              { Setting ButtonWidth will set FStreamedFixedWidth to False, thus
                reapply it. }
              FStreamedFixedWidth := True;
            end;
          end
          else
          begin
            ButtonWidth := SysButtonWidth;
            { Setting ButtonWidth will remove the csFixedWidth flag, thus
              reapply it. }
            with FButton do
              ControlStyle := ControlStyle + [csFixedWidth];
          end;
        end;
      ikEllipsis: ;
    end;
  end;
end;

procedure TJvCustomComboEdit.SetImages(const Value: TCustomImageList);
begin
  ReplaceComponentReference(Self, Value, TComponent(FImages));
  if FImages = nil then
    SetImageIndex(-1);
  if ImageKind = ikCustom then
  begin
    FButton.Images := FImages;
    FButton.ImageIndex := FImageIndex;
  end;
end;

procedure TJvCustomComboEdit.SetNumGlyphs(const Value: TNumGlyphs);
begin
  //if FGlyphKind in [gkDropDown, gkEllipsis] then
  //  FButton.NumGlyphs := 1
  //else
  //if FGlyphKind = gkDefault then
  //  FButton.NumGlyphs := FDefNumGlyphs
  //else
  FNumGlyphs := Value;
  if ImageKind = ikCustom then
    FButton.NumGlyphs := Value;
end;

procedure TJvCustomComboEdit.SetPopupValue(const Value: Variant);
begin
  if FPopup is TJvPopupWindow then
    TJvPopupWindow(FPopup).SetValue(Value);
end;

procedure TJvCustomComboEdit.SetReadOnly(Value: Boolean);
begin
  if Value <> FReadOnly then
  begin
    FReadOnly := Value;
    UpdateButtonEnabled;
    inherited ReadOnly := Value or not FDirectInput;
  end;
end;

procedure TJvCustomComboEdit.SetShowButton(const Value: Boolean);
begin
  if ShowButton <> Value then
  begin
    if Value then
    begin
      { FBtnControl needs to be visible first, otherwise only FSavedButtonWidth
        is changed when setting ButtonWidth }
      FBtnControl.Visible := True;
      ButtonWidth := FSavedButtonWidth
    end
    else
      ButtonWidth := 0;
  end;
end;

procedure TJvCustomComboEdit.SetDataConnector(const Value: TJvCustomComboEditDataConnector);
begin
  if Value <> FDataConnector then
    FDataConnector.Assign(Value);
end;

function TJvCustomComboEdit.CreateDataConnector: TJvCustomComboEditDataConnector;
begin
  Result := TJvCustomComboEditDataConnector.Create(Self);
end;

procedure TJvCustomComboEdit.SetShowCaret;
const
  CaretWidth: array [Boolean] of Integer = (1, 2);
begin
  CreateCaret(Handle, 0, CaretWidth[fsBold in Font.Style], GetTextHeight);
  ShowCaret(Handle);
end;

procedure TJvCustomComboEdit.ShowPopup(Origin: TPoint);
begin
  if FPopup is TJvPopupWindow then
  begin
    TJvPopupWindow(FPopup).Show(Origin);
    if Assigned(FOnPopupShown) then
      FOnPopupShown(Self);
  end;
end;


procedure TJvCustomComboEdit.UpdateAutoComplete;
const
  cAutoCompleteOptionValues: array [TJvAutoCompleteOption] of DWORD =
    (ACO_AUTOSUGGEST, ACO_AUTOAPPEND,
     ACO_SEARCH, ACO_FILTERPREFIXES, ACO_USETAB, ACO_UPDOWNKEYDROPSLIST,
     ACO_RTLREADING);
var
  Flags: DWORD;
  Option: TJvAutoCompleteOption;
  AutoComplete2: IAutoComplete2;
begin
  if HandleAllocated and not (csDesigning in ComponentState) then
  begin
    if Supports(FAutoCompleteIntf, IAutoComplete2, AutoComplete2) then
    begin
      { Set the options of the autocomplete object. }
      Flags := 0;
      for Option := Low(TJvAutoCompleteOption) to High(TJvAutoCompleteOption) do
        if Option in AutoCompleteOptions then
          Inc(Flags, cAutoCompleteOptionValues[Option]);

      AutoComplete2.SetOptions(Flags);
    end;
  end;
end;

procedure TJvCustomComboEdit.UpdateBtnBounds(var NewLeft, NewTop, NewWidth, NewHeight: Integer);
var
  BtnRect: TRect;
  {$IFDEF JVCLThemesEnabled}
  StyleExtraBorder: Integer;
  {$ENDIF JVCLThemesEnabled}
begin
  if not Assigned(FButton) then
    Exit;

  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled then
  begin
    if BorderStyle = bsSingle then
    begin
       // Work around the VCL Style engine bug where edit controls are painted smaller than they are (see also WMNCCalcSize)
      StyleExtraBorder := 0;
      if Ctl3D then
        StyleExtraBorder := 2;

      if Ctl3D then
        BtnRect := Bounds(Width - FButton.Width - 2 - StyleExtraBorder, 0, FButton.Width, Height - 2 - StyleExtraBorder)
      else
        BtnRect := Bounds(Width - FButton.Width - 1 - StyleExtraBorder, 1, FButton.Width, Height - 2 - StyleExtraBorder);
    end
    else
      BtnRect := Bounds(Width - FButton.Width, 0, FButton.Width, Height);
  end
  else
  {$ENDIF JVCLThemesEnabled}
  begin
    if BorderStyle = bsSingle then
    begin
      if not Flat then
        BtnRect := Bounds(Width - FButton.Width - 4, 0, FButton.Width, Height - 4)
      else
        BtnRect := Bounds(Width - FButton.Width - 2, 1, FButton.Width, Height - 4)
    end
    else
      BtnRect := Bounds(Width - FButton.Width, 0, FButton.Width, Height);
  end;

  // Mantis 4754: Bevels must be taken into account
  if BevelKind <> bkNone then
  begin
    if BevelInner <> bvNone then
    begin
      Dec(BtnRect.Left, 2);
      Dec(BtnRect.Right, 2);
      Dec(BtnRect.Bottom, 2);
    end;

    if BevelOuter <> bvNone then
    begin
      Dec(BtnRect.Left, 2);
      Dec(BtnRect.Right, 2);
      Dec(BtnRect.Bottom, 2);
    end;
  end;


  NewLeft := BtnRect.Left;
  NewTop := BtnRect.Top;
  NewWidth := BtnRect.Right - BtnRect.Left;
  NewHeight := BtnRect.Bottom - BtnRect.Top;
end;

procedure TJvCustomComboEdit.UpdateControls;
begin
  { Notification }
end;

procedure TJvCustomComboEdit.UpdateGroup;
var
  I: Integer;
begin
  if (FGroupIndex <> -1) and (Owner <> nil) then
    for I := 0 to Owner.ComponentCount - 1 do
      if Owner.Components[I] is TJvCustomComboEdit then
        with TJvCustomComboEdit(Owner.Components[I]) do
          if (Name <> Self.Name) and (FGroupIndex = Self.FGroupIndex) then
            Clear;
end;

procedure TJvCustomComboEdit.UpdateMargins;
const
  CPixelsBetweenEditAndButton = 3;
var
  LLeft, LRight, LTop: Integer;
  Loc: TRect;
begin
  { Delay until Loaded and Handle is created }
  if (csLoading in ComponentState) or not HandleAllocated then
    Exit;

  {UpdateMargins gets called whenever the layout of child controls changes.
   It uses GetInternalMargins to determine the left and right margins of the
   actual text area.}

  AdjustHeight;

  LTop := 0;
  LLeft := 0;
  LRight := 0;
  {$IFDEF JVCLThemesEnabled}
  { If flat and themes are enabled, move the left edge of the edit rectangle
    to the right, otherwise the theme edge paints over the border }
  { (rb) This was for a specific font/language; check if this is still necessary }
  if StyleServices.Enabled then
  begin
    if BorderStyle = bsSingle then
    begin
      if not Ctl3D then
        LLeft := 3
      else
      begin
        LLeft := 1;
        LTop := 1;
      end;
    end;
  end;
  if StyleServices.Enabled then
  begin
    if BorderStyle = bsSingle then
      if Ctl3D then
        LRight := 1
      else
        LRight := -1;
  end
  else
  {$ENDIF JVCLThemesEnabled}
  if (BorderStyle = bsSingle) and not Flat then
    LTop := 2;

  GetInternalMargins(LLeft, LRight);

  SetRect(Loc, LLeft, LTop, Width - LRight - CPixelsBetweenEditAndButton, ClientHeight - 1);
  SendRectMessage(Handle, EM_SETRECTNP, 0, Loc);
  // (rb) EM_SETMARGINS necessary?
  //SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN or EC_LEFTMARGIN, MakeLong(LLeft, LRight));
end;

procedure TJvCustomComboEdit.UpdatePopupVisible;
begin
  FPopupVisible := (FPopup <> nil) and FPopup.Visible;
end;

procedure TJvCustomComboEdit.ValidateEdit;
begin
  if CheckOnExit or (FInCMExit = 0) then
    inherited ValidateEdit;
end;

procedure TJvCustomComboEdit.WMNCHitTest(var Msg: TWMNCHitTest);
var
  P: TPoint;
begin
  inherited;
  if (Msg.Result = HTCLIENT) and not (csDesigning in ComponentState) and ShowButton then
  begin
    P := Point(FBtnControl.Left, FBtnControl.Top);
    Windows.ClientToScreen(Handle, P);
    if Msg.XPos > P.X then
      Msg.Result := HTBORDER; {HTCAPTION;}
  end;
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvCustomComboEdit.WMNCPaint(var Msg: TWMNCPaint);
var
  DC: HDC;
  DrawRect: TRect;
  Details: TThemedElementDetails;
begin
  if StyleServices.Enabled and Ctl3D and (BorderStyle = bsSingle) and
     not JclCheckWinVersion(6, 0) then // Vista draws the border animated and not with teEditTextNormal
  begin
    DC := GetWindowDC(Handle);
    try
      GetWindowRect(Handle, DrawRect);
      OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
      ExcludeClipRect(DC, DrawRect.Left + 2, DrawRect.Top + 2, DrawRect.Right - 2, DrawRect.Bottom - 2);

      Details := StyleServices.GetElementDetails(teEditTextNormal);
      StyleServices.DrawElement(DC, Details, DrawRect);
    finally
      ReleaseDC(Handle, DC);
    end;
    Msg.Result := 0;
  end
  else
    inherited;

  // This fixes the problem that the Button looses its border
  if StyleServices.Enabled and Ctl3D and (BorderStyle = bsSingle) then
    if (FBtnControl <> nil) and FBtnControl.Visible and FBtnControl.HandleAllocated then
      Windows.InvalidateRect(FBtnControl.Handle, nil, False);
end;
{$ENDIF JVCLThemesEnabled}

procedure TJvCustomComboEdit.WMPaint(var Msg: TWMPaint);
var
  Canvas: TControlCanvas;
begin
  if csDestroying in ComponentState then
    Exit;
  if Enabled then
    inherited
  else
  begin
    Canvas := nil;
    if not PaintEdit(Self, Text, FAlignment, PopupVisible,
      DisabledTextColor, Focused and not PopupVisible, Canvas, Msg) then
      inherited;
    Canvas.Free;
  end;
end;

procedure TJvCustomComboEdit.WndProc(var Msg: TMessage);
begin
  if (((Msg.Msg >= WM_KEYFIRST) and (Msg.Msg <= WM_KEYLAST)) or (Msg.Msg = WM_CONTEXTMENU)) and
     not SettingCursor and PopupVisible and
    (FPopup is TJvPopupWindow) and Assigned(TJvPopupWindow(FPopup).ActiveControl) then
  begin
    // Mantis 4872: Avoid stack overflow.
    if (Msg.Msg <> WM_CONTEXTMENU) or
       (ControlAtPos(ScreenToClient(SmallPointToPoint(TWMContextMenu(Msg).Pos)), False) = TJvPopupWindow(FPopup).ActiveControl) then
    begin
      with Msg do
        Result := TJvPopupWindow(FPopup).ActiveControl.Perform(Msg, WParam, LParam);

      if Msg.Result = 0 then
        Exit;
    end;
  end;

  { The AutoComplete functionality sends a WM_SETTEXT. But only SetTextBuf()
    generates the required CM_TEXTCHANGED message after the WM_SETTEXT which is
    now missing in this case. The following code ignores the SetTextBuf()
    generated CM_TEXTCHANGE and performs it's own CM_TEXTCHANGE message after
    each WM_SETTEXT. }
  if (Msg.Msg = CM_TEXTCHANGED) and FTextChanged then // ignore the message generated by TControl.SetTextBuf()
  begin
    FTextChanged := False;
    Exit;
  end;

  inherited WndProc(Msg);

  if Msg.Msg = WM_SETTEXT then
  begin
    FTextChanged := False;
    Perform(CM_TEXTCHANGED, 0, 0); // generate our own CM_TEXTCHANGED message
    FTextChanged := True;
  end;
end;

//=== { TJvCustomComboEditActionLink } =======================================

function TJvCustomComboEditActionLink.IsCaptionLinked: Boolean;
begin
  Result := False;
end;

function TJvCustomComboEditActionLink.IsHintLinked: Boolean;
begin
  Result := (Action is TCustomAction) and (FClient is TJvCustomComboEdit) and
    ((FClient as TJvCustomComboEdit).ButtonHint = (Action as TCustomAction).Hint);
end;

function TJvCustomComboEditActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and (FClient is TJvCustomComboEdit) and
    ((FClient as TJvCustomComboEdit).ImageIndex = (Action as TCustomAction).ImageIndex);
end;

function TJvCustomComboEditActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := (Action is TCustomAction) and (FClient is TJvCustomComboEdit) and
    (@(FClient as TJvCustomComboEdit).OnButtonClick = @Action.OnExecute);
end;

function TJvCustomComboEditActionLink.IsShortCutLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and (FClient is TJvCustomComboEdit) and
    ((FClient as TJvCustomComboEdit).ClickKey = (Action as TCustomAction).ShortCut);
end;

procedure TJvCustomComboEditActionLink.SetHint(const Value: THintString);
begin
  if IsHintLinked then
    (FClient as TJvCustomComboEdit).ButtonHint := Value;
end;

procedure TJvCustomComboEditActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    (FClient as TJvCustomComboEdit).ImageIndex := Value;
end;

procedure TJvCustomComboEditActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then
    (FClient as TJvCustomComboEdit).OnButtonClick := Value;
end;

procedure TJvCustomComboEditActionLink.SetShortCut(Value: TShortCut);
begin
  if IsShortCutLinked then
    (FClient as TJvCustomComboEdit).ClickKey := Value;
end;

//=== { TJvCustomDateEditDataConnector } =====================================

procedure TJvCustomDateEditDataConnector.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvCustomDateEditDataConnector then
  begin
    FDefaultDate := TJvCustomDateEditDataConnector(Source).FDefaultDate;
    FDefaultDateIsNow := TJvCustomDateEditDataConnector(Source).DefaultDateIsNow;
  end;
end;

function TJvCustomDateEditDataConnector.IsDefaultDateStored: Boolean;
begin
  Result := FDefaultDate <> 0;
end;

procedure TJvCustomDateEditDataConnector.RecordChanged;
begin
  if Field.IsValid then
  begin
    Control.ReadOnly := not Field.CanModify;
    TJvCustomDateEdit(Control).Date := Field.AsDateTime;
  end
  else
    inherited RecordChanged;
end;

procedure TJvCustomDateEditDataConnector.SetDefaultDateIsNow(const Value: Boolean);
begin
  if Value <> FDefaultDateIsNow then
    FDefaultDateIsNow := Value;
end;

procedure TJvCustomDateEditDataConnector.UpdateData;
begin
  if TJvCustomDateEdit(Control).Date = 0 then
  begin
    if DefaultDateIsNow then
      Field.AsDateTime := Now
    else
    if NullDate <> 0 then
      Field.AsDateTime := DefaultDate
    else
      Field.Clear;
  end
  else
    Field.AsDateTime := TJvCustomDateEdit(Control).Date;
  TJvCustomDateEdit(Control).Date := Field.AsDateTime; // update
end;

//=== { TJvCustomDateEdit } ==================================================

constructor TJvCustomDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDateAutoBetween := True;
  FMinDate := NullDate;
  FMaxDate := NullDate;

  FCheckOnExit := False;
  FBlanksChar := ' ';
  FTitle := RsDateDlgCaption;
  FPopupColor := clMenu;
  //  FDefNumGlyphs := 2;
  FStartOfWeek := Mon;
  FWeekends := [Sun];
  FWeekendColor := clRed;
  FYearDigits := dyDefault;
  FCalendarHints := TStringList.Create;
  FCalendarHints.OnChange := CalendarHintsChanged;
  DateHook.Add;
  FCustomDateFormat := GetDefaultDateFormat;
  FDateFormatPreferred := GetDefaultDateFormatPreferred;

  ControlState := ControlState + [csCreating];
  try
    UpdateFormat;
    {$IFDEF DEFAULT_POPUP_CALENDAR}
    FPopup := TJvPopupWindow(CreatePopupCalendar(Self,  BiDiMode, FMinDate, FMaxDate));
    TJvPopupWindow(FPopup).OnCloseUp := PopupCloseUp;
    TJvPopupWindow(FPopup).Color := FPopupColor;
    {$ENDIF DEFAULT_POPUP_CALENDAR}
    ImageKind := ikDefault; { force update }
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

destructor TJvCustomDateEdit.Destroy;
begin
  DateHook.Delete;

  if FPopup is TJvPopupWindow then
  begin
    TJvPopupWindow(FPopup).OnCloseUp := nil;
    FPopup.Parent := nil;
  end;
  FPopup.Free;
  FPopup := nil;
  FCalendarHints.OnChange := nil;
  FCalendarHints.Free;
  FCalendarHints := nil;
  inherited Destroy;
end;

function TJvCustomDateEdit.DisplayNullDateAsEmptyText: Boolean;
begin
  Result := not ShowNullDate;
end;

function TJvCustomDateEdit.CreateDataConnector: TJvCustomComboEditDataConnector;
begin
  Result := TJvCustomDateEditDataConnector.Create(Self);
end;

function TJvCustomDateEdit.AcceptPopup(var Value: Variant): Boolean;
var
  D: TDateTime;
begin
  Result := True;
  if Assigned(FOnAcceptDate) then
  begin
    if VarIsNullEmpty(Value) then
      D := NullDate
    else
    try
      D := VarToDateTime(Value);
    except
      if DefaultToday then
        D := SysUtils.Date
      else
        D := NullDate;
    end;
    FOnAcceptDate(Self, D, Result);
    if Result then
      Value := VarFromDateTime(D);
  end;
end;

procedure TJvCustomDateEdit.AcceptValue(const Value: Variant);
begin
  SetDate(VarToDateTime(Value));
  UpdatePopupVisible;
  if Modified then
    inherited Change;
  if Assigned(FOnPopupValueAccepted) then
    FOnPopupValueAccepted(Self);
end;

procedure TJvCustomDateEdit.ApplyDate(Value: TDateTime);
begin
  SetDate(Value);
  SelectAll;
end;

procedure TJvCustomDateEdit.CalendarHintsChanged(Sender: TObject);
begin
  FCalendarHints.OnChange := nil;
  try
    while CalendarHints.Count > 4 do
      CalendarHints.Delete(CalendarHints.Count - 1);
  finally
    FCalendarHints.OnChange := CalendarHintsChanged;
  end;
  if not (csDesigning in ComponentState) then
    UpdatePopup;
end;

procedure TJvCustomDateEdit.Change;
begin
  if not FFormatting then
    inherited Change;
end;

procedure TJvCustomDateEdit.CheckValidDate;
var
  ADate: TDateTime;
begin
  if TextStored then
  try
    FFormatting := True;
    try
      SetDate(StrToDateFmt(FDateFormat, Text));
    finally
      FFormatting := False;
    end;
  except
    if FDateFormat2 <> '' then
    try
      FFormatting := True;
      try
        SetDate(StrToDateFmt(FDateFormat2, Text));
      finally
        FFormatting := False;
      end;
    except
      if CanFocus then
        SetFocus;
      ADate := Self.Date;
      if DoInvalidDate(Text,ADate) then
        Self.Date := ADate
      else
        raise;
    end
    else
    begin
      if CanFocus then
        SetFocus;
      ADate := Self.Date;
      if DoInvalidDate(Text,ADate) then
        Self.Date := ADate
      else
        raise;
    end;
  end;
end;

procedure TJvCustomDateEdit.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  if Handle <> 0 then
    UpdateMask;
end;

class function TJvCustomDateEdit.DefaultImageIndex: TImageIndex;
var
  Bmp: TBitmap;
begin
  if GDateImageIndex < 0 then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.LoadFromResourceName(HInstance, sDateBmp);
      GDateImageIndex := DefaultImages.AddMasked(Bmp, clFuchsia);
    finally
      Bmp.Free;
    end;
  end;

  Result := GDateImageIndex;
end;

procedure TJvCustomDateEdit.DoExit;
begin
  if not (csDesigning in ComponentState) and CheckOnExit then
    CheckValidDate;
  inherited DoExit;
end;

function TJvCustomDateEdit.DoInvalidDate(const DateString: string; var ANewDate: TDateTime): Boolean;
begin
  Result := False;
  if Assigned(FOnInvalidDate) then
    FOnInvalidDate(Self, DateString, ANewDate, Result);
end;

function TJvCustomDateEdit.FourDigitYear: Boolean;
begin
  Result := (FYearDigits = dyFour) or ((FYearDigits = dyDefault) and IsFourDigitYear);
end;

function TJvCustomDateEdit.GetCalendarHints: TStrings;
begin
  Result := FCalendarHints;
end;

function TJvCustomDateEdit.GetCalendarStyle: TCalendarStyle;
begin
  if FPopup <> nil then
    Result := csPopup
  else
    Result := csDialog;
end;

function TJvCustomDateEdit.GetDate: TDateTime;
begin
  if DefaultToday then
    Result := SysUtils.Date
  else
    Result := NullDate;
  Result := StrToDateFmtDef(FDateFormat, Text, Result);
end;

function TJvCustomDateEdit.GetDateFormat: string;
begin
  Result := FDateFormat;
end;

function TJvCustomDateEdit.GetDateMask: string;
begin
  Result := DefDateMask(FBlanksChar, FourDigitYear);
end;

function TJvCustomDateEdit.GetDialogTitle: string;
begin
  Result := FTitle;
end;

function TJvCustomDateEdit.GetPopupColor: TColor;
begin
  if FPopup is TJvPopupWindow then
    Result := TJvPopupWindow(FPopup).Color
  else
    Result := FPopupColor;
end;

function TJvCustomDateEdit.IsCustomTitle: Boolean;
begin
  Result := (AnsiCompareStr(RsDateDlgCaption, DialogTitle) <> 0) and (DialogTitle <> '');
end;

function TJvCustomDateEdit.IsDateStored: Boolean;
begin
  Result := not DefaultToday;
end;

procedure TJvCustomDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not ReadOnly then
  begin
    if IsInWordArray(Key, [VK_PRIOR, VK_NEXT, VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN,
      VK_ADD, VK_SUBTRACT, VK_RETURN]) and PopupVisible then
    begin
      if FPopup is TJvPopupWindow then
        TJvPopupWindow(FPopup).KeyDown(Key, Shift);
      Key := 0;
    end
    else
    if (Shift = []) and DirectInput then
    begin
      case Key of
        VK_ADD:
          begin
            ApplyDate(NvlDate(Date, Now) + 1);
            Key := 0;
          end;
        VK_SUBTRACT:
          begin
            ApplyDate(NvlDate(Date, Now) - 1);
            Key := 0;
          end;
      end;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TJvCustomDateEdit.KeyPress(var Key: Char);
begin
  if not ReadOnly then
  begin
    if CharInSet(Key, ['T', 't', '+', '-']) and PopupVisible then
    begin
      if FPopup is TJvPopupWindow then
        TJvPopupWindow(FPopup).KeyPress(Key);
      Key := #0;
    end
    else
    if DirectInput then
      case Key of
        'T', 't':
          begin
            ApplyDate(Trunc(Now));
            Key := #0;
          end;
        '+', '-':
          Key := #0;
      end;
  end;
  inherited KeyPress(Key);
end;

procedure TJvCustomDateEdit.PopupDropDown(DisableEdit: Boolean);
var
  D: TDateTime;
  Action: Boolean;
begin
  if CalendarStyle = csDialog then
  begin
    D := Self.Date;
    Action := SelectDate(Self, D, DialogTitle, StartOfWeek, Weekends,
      WeekendColor, CalendarHints, MinDate, MaxDate);
    if CanFocus then
      SetFocus;
    if Action then
    begin
      if Assigned(FOnAcceptDate) then
        FOnAcceptDate(Self, D, Action);
      if Action then
      begin
        Self.Date := D;
        if FFocused then
          inherited SelectAll;
      end;
    end;
  end
  else
    inherited PopupDropDown(DisableEdit);
end;

procedure TJvCustomDateEdit.SetParent(AParent: TWinControl);
begin
  // This is here to help debugging parenting issues such as Mantis 3042
  inherited SetParent(AParent);
end;

procedure TJvCustomDateEdit.SetBlanksChar(Value: Char);
begin
  if Value <> FBlanksChar then
  begin
    if Value < ' ' then
      Value := ' ';
    FBlanksChar := Value;
    UpdateMask;
  end;
end;

procedure TJvCustomDateEdit.SetCalendarHints(Value: TStrings);
begin
  FCalendarHints.Assign(Value);
end;

procedure TJvCustomDateEdit.SetCalendarStyle(Value: TCalendarStyle);
begin
  if Value <> CalendarStyle then
    case Value of
      csPopup:
        begin
          if FPopup = nil then
            FPopup := TJvPopupWindow(CreatePopupCalendar(Self,  BiDiMode,
              FMinDate, FMaxDate));
          TJvPopupWindow(FPopup).OnCloseUp := PopupCloseUp;
          TJvPopupWindow(FPopup).Color := FPopupColor;
          UpdatePopup;
        end;
      csDialog:
        FreeAndNil(FPopup);
    end;
end;

procedure TJvCustomDateEdit.SetDate(Value: TDateTime);
var
  D: TDateTime;
  SavedModified: Boolean;
begin
  if not ValidDate(Value) or (Value = NullDate) then
    if DefaultToday then
      Value := SysUtils.Date
    else
      Value := NullDate;
  D := Self.Date;
  SavedModified := Modified;
  TestDateBetween(Value);
  if (Value = NullDate) and DisplayNullDateAsEmptyText then
    Text := ''
  else
    Text := FormatDateTime(FDateFormat, Value);
  Modified := SavedModified or (D <> Self.Date);
end;

procedure TJvCustomDateEdit.SetDateAutoBetween(Value: Boolean);
var
  D: TDateTime;
begin
  if Value <> FDateAutoBetween then
  begin
    FDateAutoBetween := Value;
    if Value then
    begin
      D := Date;
      TestDateBetween(D);
      if D <> Date then
        Date := D;
    end;
    Invalidate;
  end;
end;

procedure TJvCustomDateEdit.SetDialogTitle(const Value: string);
begin
  FTitle := Value;
end;

procedure TJvCustomDateEdit.SetMaxDate(Value: TDateTime);
begin
  if Value <> FMaxDate then
  begin
    //Check unacceptable MaxDate < MinDate
    if (Value <> NullDate) and (FMinDate <> NullDate) and (Value < FMinDate) then
      if FDateAutoBetween then
        SetMinDate(Value)
      else
        raise EJVCLException.CreateResFmt(@RsEDateMaxLimit, [DateToStr(FMinDate)]);
    FMaxDate := Value;
    UpdatePopup;
    if FDateAutoBetween then
      SetDate(Date);
  end;
end;

procedure TJvCustomDateEdit.SetMinDate(Value: TDateTime);
begin
  if Value <> FMinDate then
  begin
    //!!!!! Necessarily check

    // Check unacceptable MinDate > MaxDate [Translated]
    if (Value <> NullDate) and (FMaxDate <> NullDate) and (Value > FMaxDate) then
      if FDateAutoBetween then
        SetMaxDate(Value)
      else
        raise EJVCLException.CreateResFmt(@RsEDateMinLimit, [DateToStr(FMaxDate)]);
    FMinDate := Value;
    UpdatePopup;
    if FDateAutoBetween then
      SetDate(Date);
  end;
end;

procedure TJvCustomDateEdit.SetPopupColor(Value: TColor);
begin
  if Value <> PopupColor then
  begin
    if FPopup is TJvPopupWindow then
      TJvPopupWindow(FPopup).Color := Value;
    FPopupColor := Value;
  end;
end;

procedure TJvCustomDateEdit.SetPopupValue(const Value: Variant);
begin
  inherited SetPopupValue(StrToDateFmtDef(FDateFormat, VarToStr(Value), SysUtils.Date));
end;

procedure TJvCustomDateEdit.SetShowNullDate(const Value: Boolean);
begin
  if FShowNullDate <> Value then
  begin
    FShowNullDate := Value;
    SetDate(Date);
  end;
end;

procedure TJvCustomDateEdit.SetStartOfWeek(Value: TDayOfWeekName);
begin
  if Value <> FStartOfWeek then
  begin
    FStartOfWeek := Value;
    UpdatePopup;
  end;
end;

procedure TJvCustomDateEdit.SetWeekendColor(Value: TColor);
begin
  if Value <> FWeekendColor then
  begin
    FWeekendColor := Value;
    UpdatePopup;
  end;
end;

procedure TJvCustomDateEdit.SetWeekends(Value: TDaysOfWeek);
begin
  if Value <> FWeekends then
  begin
    FWeekends := Value;
    UpdatePopup;
  end;
end;

function TJvCustomDateEdit.GetDefaultDateFormat: string;
begin
  Result := '';
end;

function TJvCustomDateEdit.IsDateFormatStored: Boolean;
begin
  Result := (FCustomDateFormat <> GetDefaultDateFormat);
end;

function TJvCustomDateEdit.GetDefaultDateFormatPreferred: TPreferredDateFormat;
begin
  Result := pdLocaleOnly;
end;

function TJvCustomDateEdit.IsDateFormatPreferredStored: Boolean;
begin
  Result := (FDateFormatPreferred <> GetDefaultDateFormatPreferred);
end;

procedure TJvCustomDateEdit.SetDateFormatPreferred(Value: TPreferredDateFormat);
begin
  if FDateFormatPreferred <> Value then
  begin
    FDateFormatPreferred := Value;
    if not (csLoading in ComponentState) then
      UpdateMask;
  end;
end;

procedure TJvCustomDateEdit.SetCustomDateFormat(const Value: string);
begin
  if FCustomDateFormat <> Value then
  begin
    FCustomDateFormat := Value;
    if not (csLoading in ComponentState) then
    begin
      if FDateFormatPreferred in [pdCustom, pdCustomOnly] then
        UpdateMask
      else
        UpdateFormat;
    end;
  end;
end;

procedure TJvCustomDateEdit.SetYearDigits(Value: TYearDigits);
begin
  if FYearDigits <> Value then
  begin
    FYearDigits := Value;
    UpdateMask;
  end;
end;

function TJvCustomDateEdit.StoreMaxDate: Boolean;
begin
  Result := FMaxDate <> NullDate;
end;

function TJvCustomDateEdit.StoreMinDate: Boolean;
begin
  Result := FMinDate <> NullDate;
end;

procedure TJvCustomDateEdit.TestDateBetween(var Value: TDateTime);
begin
  if FDateAutoBetween then
  begin
    if (FMinDate <> NullDate) and (Value <> NullDate) and (Value < FMinDate) then
      Value := FMinDate;
    if (FMaxDate <> NullDate) and (Value <> NullDate) and (Value > FMaxDate) then
      Value := FMaxDate;
  end;
end;

function TJvCustomDateEdit.TextStored: Boolean;
begin
  Result := not IsEmptyStr(Text, [#0, ' ', JclFormatSettings.DateSeparator, FBlanksChar]);
end;

procedure TJvCustomDateEdit.UpdateFormat;
begin
  if (FDateFormatPreferred in [pdLocale, pdLocaleOnly]) or (FCustomDateFormat = '') then
  begin
    FDateFormat := DefDateFormat(FourDigitYear);
    if FDateFormatPreferred = pdLocale then
      FDateFormat2 := FCustomDateFormat
    else
      FDateFormat2 := '';
  end
  else
  begin
    FDateFormat := FCustomDateFormat;
    if FDateFormatPreferred = pdCustom then
      FDateFormat2 := DefDateFormat(FourDigitYear)
    else
      FDateFormat2 := '';
  end;
end;

procedure TJvCustomDateEdit.UpdateMask;
var
  DateValue: TDateTime;
  OldFormat: string;
begin
  DateValue := GetDate;
  OldFormat := FDateFormat;
  UpdateFormat;
  if (GetDateMask <> EditMask) or (OldFormat <> FDateFormat) then
  begin
    { force update }
    EditMask := '';
    EditMask := GetDateMask;
  end;
  UpdatePopup;
  SetDate(DateValue);
end;

procedure TJvCustomDateEdit.UpdatePopup;
begin
  if FPopup <> nil then
    SetupPopupCalendar(FPopup, StartOfWeek, Weekends, WeekendColor,
      CalendarHints, FourDigitYear, MinDate, MaxDate);
end;

procedure TJvCustomDateEdit.ValidateEdit;
begin
  if TextStored and CheckOnExit then
    CheckValidDate;
end;

procedure TJvCustomDateEdit.WMContextMenu(var Msg: TWMContextMenu);
begin
  if not PopupVisible then
    inherited;
end;

//=== { TJvDateEdit } ========================================================

// (rom) unusual not to have it implemented in the Custom base class

constructor TJvDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UpdateMask;
end;

procedure TJvDateEdit.SetDate(Value: TDateTime);
begin
  if not FDateAutoBetween then
    if Value <> NullDate then
    begin
      if ((FMinDate <> NullDate) and (FMaxDate <> NullDate) and
        ((Value < FMinDate) or (Value > FMaxDate))) then
        raise EJVCLException.CreateResFmt(@RsEDateOutOfRange, [FormatDateTime(FDateFormat, Value),
          FormatDateTime(FDateFormat, FMinDate), FormatDateTime(FDateFormat, FMaxDate)])
      else
      if (FMinDate <> NullDate) and (Value < FMinDate) then
        raise EJVCLException.CreateResFmt(@RsEDateOutOfMin, [FormatDateTime(FDateFormat, Value),
          FormatDateTime(FDateFormat, FMinDate)])
      else
      if (FMaxDate <> NullDate) and (Value > FMaxDate) then
        raise EJVCLException.CreateResFmt(@RsEDateOutOfMax, [FormatDateTime(FDateFormat, Value),
          FormatDateTime(FDateFormat, FMaxDate)]);
    end;
  inherited SetDate(Value);
end;

//=== { TJvDirectoryEdit } ===================================================

constructor TJvDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [sdAllowCreate];
  FOptionsWin32 := DefaultJvBrowseFolderDialogOptions;
  FAutoCompleteFileOptions := [acfFileSystem, acfFileSysDirs];
  FDialogKind := dkWin32;
end;

class function TJvDirectoryEdit.DefaultImageIndex: TImageIndex;
var
  Bmp: TBitmap;
begin
  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled then
  begin
    if GDirImageIndexXP < 0 then
    begin
      Bmp := TBitmap.Create;
      try
        Bmp.LoadFromResourceName(HInstance, sDirXPBmp);
        GDirImageIndexXP := DefaultImages.AddMasked(Bmp, clFuchsia);
      finally
        Bmp.Free;
      end;
    end;
    Result := GDirImageIndexXP;
    Exit;
  end;
  {$ENDIF JVCLThemesEnabled}

  if GDirImageIndex < 0 then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.LoadFromResourceName(HInstance, sDirBmp);
      GDirImageIndex := DefaultImages.AddMasked(Bmp, clFuchsia);
    finally
      Bmp.Free;
    end;
  end;
  Result := GDirImageIndex;
end;

function TJvDirectoryEdit.GetLongName: string;
var
  Txt, Temp: string;
  Pos: Integer;
begin
  Txt := Directory;
  if not MultipleDirs then
    Result := ShortToLongPath(Txt)
  else
  begin
    Result := '';
    Pos := 1;
    while Pos <= Length(Txt) do
    begin
      Temp := ShortToLongPath(ExtractSubstr(Txt, Pos, [PathSep]));
      if (Result <> '') and (Temp <> '') then
        Result := Result + PathSep;
      Result := Result + Temp;
    end;
  end;
end;

function TJvDirectoryEdit.GetShortName: string;
var
  Txt, Temp: string;
  Pos: Integer;
begin
  Txt := Directory;
  if not MultipleDirs then
    Result := LongToShortPath(Txt)
  else
  begin
    Result := '';
    Pos := 1;
    while Pos <= Length(Txt) do
    begin
      Temp := LongToShortPath(ExtractSubstr(Txt, Pos, [PathSep]));
      if (Result <> '') and (Temp <> '') then
        Result := Result + PathSep;
      Result := Result + Temp;
    end;
  end;
end;

function TJvDirectoryEdit.GetLocalizedName: string;
var
  Txt, Temp: string;
  Pos: Integer;
begin
  Txt := Directory;
  if not MultipleDirs then
    Result := PathGetLocalizedPath(Txt)
  else
  begin
    Result := '';
    Pos := 1;
    while Pos <= Length(Txt) do
    begin
      Temp := PathGetLocalizedPath(ExtractSubstr(Txt, Pos, [PathSep]));
      if (Result <> '') and (Temp <> '') then
        Result := Result + PathSep;
      Result := Result + Temp;
    end;
  end;
end;

procedure TJvDirectoryEdit.DoEnter;
begin
  Text := FPhysicalDirectory;
  inherited DoEnter;
end;

procedure TJvDirectoryEdit.DoExit;
var
  Txt: string;
begin
  inherited DoExit;
  if DisplayLocalizedName then
  begin
    Txt := Text;
    Text := LocalizedName;
    FPhysicalDirectory := Txt; // by using "Text:=" the WM_SETTEXT handler changes the PhysicalDirectory
  end;
end;

procedure TJvDirectoryEdit.WndProc(var Msg: TMessage);
begin
  inherited WndProc(Msg);
  if Msg.Msg = WM_SETTEXT then
    FPhysicalDirectory := Text;
end;

procedure TJvDirectoryEdit.Change;
begin
  inherited Change;
  FPhysicalDirectory := Text;
end;

procedure TJvDirectoryEdit.SetDirectory(const Value: string);
begin
  if not FDisplayLocalizedName or Focused then
    Text := Value
  else
  begin
    FPhysicalDirectory := Value; // is used in GetLocalizedName
    Text := LocalizedName;
  end;
  FPhysicalDirectory := Value; // must be set after "Text:="
end;

function TJvDirectoryEdit.GetDirectory: string;
begin
  if not FDisplayLocalizedName or Focused then
    Result := Text
  else
    Result := FPhysicalDirectory;
end;

procedure TJvDirectoryEdit.SetDisplayLocalizedName(const Value: Boolean);
var
  Txt: string;
begin
  if Value <> FDisplayLocalizedName then
  begin
    if FDisplayLocalizedName and not Focused then
      Text := FPhysicalDirectory;

    FDisplayLocalizedName := Value;

    if FDisplayLocalizedName and not Focused then
    begin
      Txt := Text;
      Text := LocalizedName;
      FPhysicalDirectory := Txt; // must be set after "Text:="
    end;
  end;
end;

procedure TJvDirectoryEdit.PopupDropDown(DisableEdit: Boolean);
var
  Temp, Txt: string;
  Action: Boolean;
  BrowseForFolder: TJvBrowseForFolderDialog;
begin
  Temp := Directory;
  Action := True;
  DoBeforeDialog(Temp, Action);
  if not Action then
    Exit;
  if Temp = '' then
  begin
    if InitialDir <> '' then
      Temp := InitialDir
    else
      Temp := PathDelim;
  end;

  if not DirectoryExists(Temp) then
    Temp := PathDelim;

  case DialogKind of
    dkVCL:
      begin
        DisableSysErrors;
        try
          Action := SelectDirectory(Temp, FOptions, Self.HelpContext);
        finally
          EnableSysErrors;
        end;
      end;
    dkWin32:
      begin
        BrowseForFolder := TJvBrowseForFolderDialog.Create(Self);
        try
          BrowseForFolder.Options := DialogOptionsWin32;
          BrowseForFolder.Directory := Temp;
          BrowseForFolder.StatusText := DialogText;
          Action := BrowseForFolder.Execute;
          Temp := BrowseForFolder.Directory;
        finally
          BrowseForFolder.Free;
        end;
      end;
  end;

  if CanFocus then
    SetFocus;
  DoAfterDialog(Temp, Action);
  if Action then
  begin
    SelText := '';
    if (Text = '') or not MultipleDirs then
      Txt := Temp
    else
      Txt := Directory + PathSep + Temp;
    Text := Txt;
    FPhysicalDirectory := Txt; // Must be set after "Text:="
    if (Temp <> '') and DirectoryExists(Temp) then
      InitialDir := Temp;
  end;
end;

procedure TJvDirectoryEdit.ReceptFileDir(const AFileName: string);
var
  Temp: string;
begin
  if FileExists(AFileName) then
    Temp := StrEnsureNoSuffix(PathDelim, ExtractFilePath(AFileName))
  else
    Temp := StrEnsureNoSuffix(PathDelim, AFileName);
  if (Text = '') or not MultipleDirs then
    Text := Temp
  else
    Text := Text + PathSep + Temp;
end;

//=== { TJvEditButton } ======================================================

constructor TJvEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStandard := True;
  ControlStyle := ControlStyle + [csReplicatable];
  ParentShowHint := True;
end;

procedure TJvEditButton.Click;
begin
  if not FNoAction then
    inherited Click
  else
    FNoAction := False;
end;

function TJvEditButton.GetGlyph: TBitmap;
begin
  Result := ButtonGlyph.Glyph;
end;

function TJvEditButton.GetNumGlyphs: TJvNumGlyphs;
begin
  Result := ButtonGlyph.NumGlyphs;
end;

function TJvEditButton.GetUseGlyph: Boolean;
begin
  Result := not Assigned(Images) or (ImageIndex < 0);
end;

procedure TJvEditButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and (Owner <> nil) then
    with TJvCustomComboEdit(Owner) do
    begin
      FNoAction := (FPopup <> nil) and FPopupVisible;
      if not FPopupVisible then
      begin
        if TabStop and CanFocus and (GetFocus <> Handle) then
          SetFocus;
      end
      else
        PopupCloseUp(FPopup, FStandard);
    end;
end;

procedure TJvEditButton.Paint;
{$IFDEF JVCLThemesEnabled}
var
  {$IFDEF HAS_UNIT_VCL_THEMES}
  DrawState: TJvButtonState;
  {$ENDIF HAS_UNIT_VCL_THEMES}
  ThemedState: TThemedComboBox;
  Details: TThemedElementDetails;
  R: TRect;
{$ENDIF JVCLThemesEnabled}
begin
  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled then
  begin
    if FDrawThemedDatePickerBtn and IsDatePickerThemeDataAvailable then
    begin
      {$IFDEF HAS_UNIT_VCL_THEMES}
      DrawState := FState;
      if FPopupVisible then
        DrawState := rbsDown;
    
      Details.Part := DP_SHOWCALENDARBUTTONRIGHT;

      if not Enabled then
        Details.State := DPSCBR_DISABLED
      else
      if DrawState in [rbsDown, rbsExclusive] then
        Details.State := DPSCBR_PRESSED
      else
      if MouseOver or IsDragging then
        Details.State := DPSCBR_HOT
      else
        Details.State := DPSCBR_NORMAL;

      R := ClientRect;
      if Enabled then
        FillRect(Canvas.Handle, R, HBRUSH(COLOR_WINDOW + 1))
      else
        FillRect(Canvas.Handle, R, HBRUSH(COLOR_BTNFACE + 1));
      if Width < DefDatePickerThemeButtonWidth then
        R.Left := R.Right - 15; // paint without the dropdown arrow
      DrawThemeBackground(GDatePickerThemeData, Canvas.Handle, Details.Part, Details.State, R, nil);
      {$ENDIF HAS_UNIT_VCL_THEMES}
    end
    else
    if FDrawThemedDropDownBtn then
    begin
      if not Enabled then
        ThemedState := tcDropDownButtonDisabled
      else
      if FState in [rbsDown, rbsExclusive] then
        ThemedState := tcDropDownButtonPressed
      else
      if MouseOver or IsDragging then
        ThemedState := tcDropDownButtonHot
      else
        ThemedState := tcDropDownButtonNormal;
      R := ClientRect;
      Details := StyleServices.GetElementDetails(ThemedState);
      StyleServices.DrawElement(Canvas.Handle, Details, R);
    end
    else
      inherited Paint;
  end
  else
  {$ENDIF JVCLThemesEnabled}
  begin
    inherited Paint;
    if FState <> rbsDown then
    begin
      Canvas.Pen.Color := clBtnFace;
      Canvas.MoveTo(0, 0);
      Canvas.LineTo(0, Height - 1);
      Canvas.Pen.Color := clBtnHighlight;
      Canvas.MoveTo(1, 1);
      Canvas.LineTo(1, Height - 2);
    end;
  end;
end;

procedure TJvEditButton.PaintImage(Canvas: TCanvas; ARect: TRect;
  const Offset: TPoint; AState: TJvButtonState; DrawMark: Boolean; PaintOnGlass: Boolean);
begin
  if UseGlyph then
    ButtonGlyph.Draw(Canvas, ARect, Offset, '', Layout,
      Margin, Spacing, False, AState, 0, PaintOnGlass)
  else
    inherited PaintImage(Canvas, ARect, Offset, AState, DrawMark, PaintOnGlass);
end;

procedure TJvEditButton.SetGlyph(const Value: TBitmap);
begin
  ButtonGlyph.Glyph := Value;
  Invalidate;
end;

procedure TJvEditButton.SetNumGlyphs(Value: TJvNumGlyphs);
begin
  if Value < 0 then
    Value := 1
  else
  if Value > Ord(High(TJvButtonState)) + 1 then
    Value := Ord(High(TJvButtonState)) + 1;
  if Value <> ButtonGlyph.NumGlyphs then
  begin
    ButtonGlyph.NumGlyphs := Value;
    Invalidate;
  end;
end;

procedure TJvEditButton.SetPopupVisible(const Value: Boolean);
begin
  if Value <> FPopupVisible then
  begin
    FPopupVisible := Value;
    Invalidate;
  end;
end;

procedure TJvEditButton.WMContextMenu(var Msg: TWMContextMenu);
begin
  { (rb) Without this, we get 2 context menu's (1 from the form, another from
         the combo edit; don't know exactly what is causing this. (I guess
         it's related to FBtnControl being a TWinControl) }
  Msg.Result := 1;
end;

//=== { TJvFileDirEdit } =====================================================

constructor TJvFileDirEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFNDEF UNICODE}
  OEMConvert := True;
  {$ENDIF ~UNICODE}
  FAcceptFiles := True;
  AlwaysEnableButton := True;
  AlwaysShowPopup := True;
  FAutoCompleteOptions := [acoAutoSuggest];
  ControlState := ControlState + [csCreating];
  try
    ImageKind := ikDefault; { force update }
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

procedure TJvFileDirEdit.Change;
var
  Ps: Integer;
begin
  // The control becomes confused when the Text property has a #10 or #13 in it.
  Ps := Pos(#10, Text);
  if Ps = 0 then
    Ps := Pos(#13, Text);
  if Ps > 0 then
    Text := Copy(Text, 1, Ps - 1)
  else
    inherited Change;
end;

procedure TJvFileDirEdit.ClearFileList;
begin
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvFileDirEdit.CMSysColorChange(var Msg: TMessage);
begin
  inherited;
  // We use this event to respond to theme changes (no WM_THEMECHANGED are broadcasted
  // to the components)
  // Note that there is a bug in TApplication.WndProc, so the application will not
  // change from non-themed to themed.
  if ImageKind = ikDefault then
    Button.ImageIndex := DefaultImageIndex;
end;
{$ENDIF JVCLThemesEnabled}

procedure TJvFileDirEdit.CreateHandle;
begin
  inherited CreateHandle;

  if FAcceptFiles then
    SetDragAccept(True);
end;

procedure TJvFileDirEdit.DestroyWindowHandle;
begin
  SetDragAccept(False);
  inherited DestroyWindowHandle;
end;

procedure TJvFileDirEdit.DisableSysErrors;
begin
  {$IFDEF MSWINDOWS}
  FErrMode := SetErrorMode(SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
  {$ENDIF MSWINDOWS}
end;

procedure TJvFileDirEdit.DoAfterDialog(var FileName: string; var Action: Boolean);
begin
  if Assigned(FOnAfterDialog) then
    FOnAfterDialog(Self, FileName, Action);
end;

procedure TJvFileDirEdit.DoBeforeDialog(var FileName: string; var Action: Boolean);
begin
  if Assigned(FOnBeforeDialog) then
    FOnBeforeDialog(Self, FileName, Action);
end;

procedure TJvFileDirEdit.EnableSysErrors;
begin
  {$IFDEF MSWINDOWS}
  SetErrorMode(FErrMode);
  {$ENDIF MSWINDOWS}
  FErrMode := 0;
end;

function TJvFileDirEdit.GetAutoCompleteSource: IEnumString;
begin
  if Failed(CoCreateInstance(CLSID_ACLMulti, nil, CLSCTX_INPROC_SERVER, IEnumString, FAutoCompleteSourceIntf)) then
    FAutoCompleteSourceIntf := nil;
  Result := FAutoCompleteSourceIntf;
end;

procedure TJvFileDirEdit.SetAcceptFiles(Value: Boolean);
begin
  if FAcceptFiles <> Value then
  begin
    SetDragAccept(Value);
    FAcceptFiles := Value;
  end;
end;

procedure TJvFileDirEdit.SetAutoCompleteFileOptions(const Value: TJvAutoCompleteFileOptions);
begin
  if FAutoCompleteFileOptions <> Value then
  begin
    FAutoCompleteFileOptions := Value;
    if not (csDesigning in ComponentState) then
      UpdateAutoComplete;
  end;
end;

procedure TJvFileDirEdit.SetDragAccept(Value: Boolean);
begin
  if not (csDesigning in ComponentState) and (Handle <> 0) then
    DragAcceptFiles(Handle, Value);
end;

procedure TJvFileDirEdit.DestroyAutoComplete;
begin
  // Mantis 3112: We drop the references we get to the various interfaces
  // thus avoiding accesses to them triggered by the ancestor(s) destructor(s)
  FMRUList := nil;
  FHistoryList := nil;
  FFileSystemList:= nil;
  FAutoCompleteSourceIntf := nil;

  inherited DestroyAutoComplete;
end;

procedure TJvFileDirEdit.UpdateAutoComplete;
var
  ObjMgr: IObjMgr;
  List2: IACList2;
  Options: DWORD;
begin
  if Supports(FAutoCompleteSourceIntf, IObjMgr, ObjMgr) then
  begin
    if acfURLMRU in AutoCompleteFileOptions then
    begin
      if not Assigned(FMRUList) and
        Succeeded(CoCreateInstance(CLSID_ACLMRU, nil, CLSCTX_INPROC_SERVER, IUnknown, FMRUList)) then
      begin
        ObjMgr.Append(FMRUList);
      end
    end
    else
    if Assigned(FMRUList) then
    begin
      ObjMgr.Remove(FMRUList);
      FMRUList := nil;
    end;

    if acfURLHistory in AutoCompleteFileOptions then
    begin
      if not Assigned(FHistoryList) and
        Succeeded(CoCreateInstance(CLSID_ACLHistory, nil, CLSCTX_INPROC_SERVER, IUnknown, FHistoryList)) then
      begin
        ObjMgr.Append(FHistoryList);
      end;
    end
    else
    if Assigned(FHistoryList) then
    begin
      ObjMgr.Remove(FHistoryList);
      FHistoryList := nil;
    end;

    if [acfFileSystem, acfFileSysDirs] * AutoCompleteFileOptions <> [] then
    begin
      if not Assigned(FFileSystemList) and
        Succeeded(CoCreateInstance(CLSID_ACListISF, nil, CLSCTX_INPROC_SERVER, IUnknown, FFileSystemList)) then
      begin
        ObjMgr.Append(FFileSystemList);
      end;

      Options := ACLO_FILESYSONLY;
      if acfFileSysDirs in AutoCompleteFileOptions then
        Options := Options or ACLO_FILESYSDIRS;

      if Supports(FFileSystemList, IACList2, List2) then
        List2.SetOptions(Options);
    end
    else
    if Assigned(FFileSystemList) then
    begin
      ObjMgr.Remove(FFileSystemList);
      FFileSystemList := nil;
    end;
  end;

  inherited UpdateAutoComplete;
end;

procedure TJvFileDirEdit.WMDropFiles(var Msg: TWMDropFiles);
var
  AFileName: array [0..255] of Char;
  I, Num: Cardinal;
begin
  Msg.Result := 0;
  try
    Num := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
    if Num > 0 then
    begin
      ClearFileList;
      for I := 0 to Num - 1 do
      begin
        DragQueryFile(Msg.Drop, I, PChar(@AFileName[0]), Pred(SizeOf(AFileName)));
        ReceptFileDir(StrPas(AFileName));
        if not FMultipleDirs then
          Break;
      end;
      if Assigned(FOnDropFiles) then
        FOnDropFiles(Self);
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

//=== { TJvFilenameEdit } ====================================================

constructor TJvFilenameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAddQuotes := True;
  FAutoCompleteFileOptions := [acfFileSystem];
  CreateEditDialog;
end;

procedure TJvFilenameEdit.ClearFileList;
begin
  FDialog.Files.Clear;
end;

procedure TJvFilenameEdit.CreateEditDialog;
var
  NewDialog: TOpenDialog;
begin
  case FDialogKind of
    dkOpen:
      NewDialog := TOpenDialog.Create(Self);
    dkOpenPicture:
      NewDialog := TOpenPictureDialog.Create(Self);
    dkSavePicture:
      NewDialog := TSavePictureDialog.Create(Self);
  else { dkSave }
    NewDialog := TSaveDialog.Create(Self);
  end;
  try
    if FDialog <> nil then
    begin
      with NewDialog do
      begin
        DefaultExt := FDialog.DefaultExt;
        FileEditStyle := FDialog.FileEditStyle;
        FileName := FDialog.FileName;
        Filter := FDialog.Filter;
        FilterIndex := FDialog.FilterIndex;
        InitialDir := FDialog.InitialDir;
        HistoryList := FDialog.HistoryList;
        Files.Assign(FDialog.Files);
        Options := FDialog.Options;
        Title := FDialog.Title;
      end;
      FDialog.Free;
    end
    else
    begin
      NewDialog.Title := RsBrowseCaption;
      NewDialog.Filter := RsDefaultFilter;
      NewDialog.Options := [ofHideReadOnly];
    end;
  finally
    FDialog := NewDialog;
  end;
end;

class function TJvFilenameEdit.DefaultImageIndex: TImageIndex;
var
  Bmp: TBitmap;
begin
  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled then
  begin
    if GFileImageIndexXP < 0 then
    begin
      Bmp := TBitmap.Create;
      try
        Bmp.LoadFromResourceName(HInstance, sFileXPBmp);
        GFileImageIndexXP := DefaultImages.AddMasked(Bmp, clFuchsia);
      finally
        Bmp.Free;
      end;
    end;
    Result := GFileImageIndexXP;
    Exit;
  end;
  {$ENDIF JVCLThemesEnabled}

  if GFileImageIndex < 0 then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.LoadFromResourceName(HInstance, sFileBmp);
      GFileImageIndex := DefaultImages.AddMasked(Bmp, clFuchsia);
    finally
      Bmp.Free;
    end;
  end;
  Result := GFileImageIndex;
end;

function TJvFilenameEdit.GetDefaultExt: TFileExt;
begin
  Result := FDialog.DefaultExt;
end;

function TJvFilenameEdit.GetDialogFiles: TStrings;
begin
  Result := FDialog.Files;
end;

function TJvFilenameEdit.GetDialogTitle: string;
begin
  Result := FDialog.Title;
end;

function TJvFilenameEdit.GetFileEditStyle: TFileEditStyle;
begin
  Result := FDialog.FileEditStyle;
end;

function TJvFilenameEdit.GetFilter: string;
begin
  Result := FDialog.Filter;
end;

function TJvFilenameEdit.GetFilterIndex: Integer;
begin
  Result := FDialog.FilterIndex;
end;

function TJvFilenameEdit.GetHistoryList: TStrings;
begin
  Result := FDialog.HistoryList;
end;

function TJvFilenameEdit.GetInitialDir: string;
begin
  Result := FDialog.InitialDir;
end;

function TJvFilenameEdit.GetLongName: string;
begin
  Result := ShortToLongFileName(FileName);
end;

function TJvFilenameEdit.GetOptions: TOpenOptions;
begin
  Result := FDialog.Options;
end;

function TJvFilenameEdit.GetShortName: string;
begin
  Result := LongToShortFileName(FileName);
end;

function TJvFilenameEdit.GetLocalizedName: string;
begin
  Result := PathGetLocalizedPath(FileName);
end;

function TJvFilenameEdit.IsCustomFilter: Boolean;
begin
  Result := AnsiCompareStr(RsDefaultFilter, FDialog.Filter) <> 0;
end;

function TJvFilenameEdit.IsCustomTitle: Boolean;
begin
  Result := AnsiCompareStr(RsBrowseCaption, FDialog.Title) <> 0;
end;

procedure TJvFilenameEdit.DoEnter;
begin
  Text := FPhysicalFileName;
  inherited DoEnter;
end;

procedure TJvFilenameEdit.DoExit;
var
  Txt: string;
begin
  inherited DoExit;
  if DisplayLocalizedName then
  begin
    Txt := Text;
    Text := LocalizedName;
    FPhysicalFileName := Txt; // by using "Text:=" the WM_SETTEXT handler changes the PhysicalFileName
  end;
end;

procedure TJvFilenameEdit.WndProc(var Msg: TMessage);
begin
  inherited WndProc(Msg);
  if Msg.Msg = WM_SETTEXT then
    FPhysicalFileName := Text;
end;

procedure TJvFilenameEdit.Change;
begin
  inherited Change;
  FPhysicalFileName := Text;
end;

procedure TJvFilenameEdit.SetDisplayLocalizedName(const Value: Boolean);
var
  Txt: string;
begin
  if Value <> FDisplayLocalizedName then
  begin
    if FDisplayLocalizedName and not Focused then
      Text := FPhysicalFileName;

    FDisplayLocalizedName := Value;

    if FDisplayLocalizedName and not Focused then
    begin
      Txt := Text;
      Text := LocalizedName;
      FPhysicalFileName := Txt; // must be set after "Text:="
    end;
  end;
end;

procedure TJvFilenameEdit.SetFileName(const Value: TFileName);
var
  Txt: string;
begin
  if (Value = '') or ValidFileName(ClipFilename(Value, AddQuotes)) then
  begin
    if AddQuotes then
      Txt := ExtFilename(Value)
    else
      Txt := Value;

    if not FDisplayLocalizedName or Focused then
      Text := Txt
    else
      Text := PathGetLocalizedPath(Txt);
    FPhysicalFileName := Txt; // must be set after "Text:="

    ClearFileList;
  end
  else
    raise EComboEditError.CreateResFmt(@SInvalidFilename, [Value]);
end;

function TJvFilenameEdit.GetFileName: TFileName;
begin
  if not FDisplayLocalizedName or Focused then
    Result := ClipFilename(Text, AddQuotes)
  else
    Result := ClipFilename(FPhysicalFileName, AddQuotes);
end;

procedure TJvFilenameEdit.PopupDropDown(DisableEdit: Boolean);
var
  Temp: string;
  Action: Boolean;
  Path, Name: string;
begin
  Action := True;
  Temp := FileName;
  DoBeforeDialog(Temp, Action);
  if not Action then
    Exit;
  if ValidFileName(Temp) then
  try
    Path := ExtractFilePath(Temp);
    Name := ExtractFileName(Temp);

    if (Name = '') or not ValidFileName(Name) then
      Temp := '';

    if DirectoryExists(Path) then
    begin
      SetInitialDir(Path);
      // If we are in the FileName's directory we don't have to include the path in the edit field.
      // After FDialog.Execute the FileName property will contains the expanded path.
      if Temp <> '' then
        FDialog.FileName := Name
      else
        FDialog.FileName := '';
    end
    else
      FDialog.FileName := Temp;
  except
    { ignore any exceptions }
  end;
  FDialog.HelpContext := Self.HelpContext;
  DisableSysErrors;
  try
    Action := FDialog.Execute;
  finally
    EnableSysErrors;
  end;
  if Action then
    Temp := FDialog.FileName;
  if CanFocus then
    SetFocus;
  DoAfterDialog(Temp, Action);
  if Action then
  begin
    if AddQuotes then
      inherited Text := ExtFilename(Temp)
    else
      inherited Text := Temp;
    Path := ExtractFilePath(FDialog.FileName);
    SetInitialDir(Path);
  end;
end;

procedure TJvFilenameEdit.ReceptFileDir(const AFileName: string);
begin
  if FMultipleDirs then
  begin
    if FDialog.Files.Count = 0 then
      SetFileName(AFileName);
    FDialog.Files.Add(AFileName);
  end
  else
    SetFileName(AFileName);
end;

procedure TJvFilenameEdit.SetDefaultExt(Value: TFileExt);
begin
  FDialog.DefaultExt := Value;
end;

procedure TJvFilenameEdit.SetDialogKind(Value: TFileDialogKind);
begin
  if FDialogKind <> Value then
  begin
    FDialogKind := Value;
    CreateEditDialog;
  end;
end;

procedure TJvFilenameEdit.SetDialogTitle(const Value: string);
begin
  FDialog.Title := Value;
end;

procedure TJvFilenameEdit.SetFileEditStyle(Value: TFileEditStyle);
begin
  FDialog.FileEditStyle := Value;
end;

procedure TJvFilenameEdit.SetFilter(const Value: string);
begin
  FDialog.Filter := Value;
end;

procedure TJvFilenameEdit.SetFilterIndex(Value: Integer);
begin
  FDialog.FilterIndex := Value;
end;

procedure TJvFilenameEdit.SetHistoryList(Value: TStrings);
begin
  FDialog.HistoryList := Value;
end;

procedure TJvFilenameEdit.SetInitialDir(const Value: string);
begin
  FDialog.InitialDir := Value;
end;

procedure TJvFilenameEdit.SetOptions(Value: TOpenOptions);
begin
  if Value <> FDialog.Options then
  begin
    FDialog.Options := Value;
    FMultipleDirs := ofAllowMultiSelect in FDialog.Options;
    if not FMultipleDirs then
      ClearFileList;
  end;
end;

//=== { TJvPopupWindow } =====================================================

constructor TJvPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEditor := AOwner as TWinControl;
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];

  // If we were to add csAcceptsControls at design time, any attempt
  // to paste a component while a component using TJvPopupWindow is active
  // would lead to the parent of the pasted component being set to the
  // TJvPopupWindow instead of the parent of the selected component.
  // This was reported in issue 3042 and was seen in TJvCustomDateEdit
  // descendents
  if not (csDesigning in ComponentState) then
    ControlStyle := ControlStyle + [csAcceptsControls];
  Visible := False;
  Ctl3D := False;
  ParentCtl3D := False;
  Parent := FEditor;
  // use same size on small and large font:
  //Scaled := False;
end;

procedure TJvPopupWindow.CloseUp(Accept: Boolean);
begin
  if Assigned(FCloseUp) then
    FCloseUp(Self, Accept);
end;

procedure TJvPopupWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

function TJvPopupWindow.GetPopupText: string;
begin
  Result := '';
end;

procedure TJvPopupWindow.Hide;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TJvPopupWindow.InvalidateEditor;
var
  R: TRect;
begin
  if FEditor is TJvCustomComboEdit then
    with TJvCustomComboEdit(FEditor) do
      SetRect(R, 0, 0, ClientWidth - FBtnControl.Width {Polaris - 2}, ClientHeight + 1)
  else
    R := FEditor.ClientRect;
  Windows.InvalidateRect(FEditor.Handle, @R, False);
  UpdateWindow(FEditor.Handle);
end;

procedure TJvPopupWindow.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    CloseUp(PtInRect(ClientRect, Point(X, Y)));
end;

procedure TJvPopupWindow.Show(Origin: TPoint);
begin
  SetBounds(Origin.X, Origin.Y, Width, Height);
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
end;

procedure TJvPopupWindow.WMActivate(var Msg: TWMActivate);
begin
  inherited;
  if Msg.Active = WA_INACTIVE then
  begin
    if FEditor is TJvCustomComboEdit then
      TJvCustomComboEdit(FEditor).AsyncPopupCloseUp(False)
    else
      CloseUp(False);
  end;
end;

procedure TJvPopupWindow.WMMouseActivate(var Msg: TMessage);
begin
  if FIsFocusable then
    inherited
  else
    Msg.Result := MA_NOACTIVATE;
end;


initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  FreeAndNil(GDateHook);
  FreeAndNil(GDefaultComboEditImagesList);
  {$IFDEF JVCLThemesEnabled}
  {$IFDEF HAS_UNIT_VCL_THEMES}
  if (GDatePickerThemeData <> 0) and Assigned(CloseThemeData) then
    CloseThemeData(GDatePickerThemeData);
  {$ENDIF HAS_UNIT_VCL_THEMES}
  {$ENDIF JVCLThemesEnabled}
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
