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

Last Modified: 2002-07-04

Contributers:
  Rob den Braasem [rbraasem@xs4all.nl]
  Polaris Software
  rblaurindo
  
You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$I WINDOWSONLY.INC}

unit JvToolEdit;

interface

uses
  Windows, Classes, StdCtrls, Controls, Messages, SysUtils, Forms, Graphics,
  Menus, Buttons, Dialogs, FileCtrl, Mask,
  {$IFDEF COMPILER6_UP}
  RTLConsts, Variants,
  {$ENDIF}
  JvComponent, JvxCtrls, JvDateUtil, JvTypes;

const
  scAltDown = scAlt + VK_DOWN;
  DefEditBtnWidth = 21;

type
  {$IFDEF WIN32}
  TFileExt = type string;
  {$ENDIF}

  TCloseUpEvent = procedure(Sender: TObject; Accept: Boolean) of object;
  TPopupAlign = (epaRight, epaLeft);

  TJvPopupWindow = class(TCustomControl)
  private
    FEditor: TWinControl;
    FCloseUp: TCloseUpEvent;
    procedure WMMouseActivate(var Msg: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    {$IFDEF WIN32}
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(const Value: Variant); virtual; abstract;
    {$ELSE}
    procedure CreateWnd; override;
    function GetValue: string; virtual; abstract;
    procedure SetValue(const Value: string); virtual; abstract;
    {$ENDIF}
    procedure InvalidateEditor;
    procedure PopupMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CloseUp(Accept: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function GetPopupText: string; virtual;
    procedure Hide;
    procedure Show(Origin: TPoint); virtual; // Polaris
    property OnCloseUp: TCloseUpEvent read FCloseUp write FCloseUp;
  end;

  TJvEditButton = class(TJvSpeedButton)
  private
    FNoAction: Boolean;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
  public
    // (rom) renamed from FStandart
    FStandard: Boolean; // Polaris
    {$IFDEF JVCLThemesEnabled}
    FDrawThemedDropDownBtn: Boolean;
    {$ENDIF}
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TGlyphKind = (gkCustom, gkDefault, gkDropDown, gkEllipsis);

  TJvCustomComboEdit = class(TCustomMaskEdit)
  private
    //    FButton: TJvEditButton; // Polaris
    FBtnControl: TWinControl;
    FOnButtonClick: TNotifyEvent;
    FClickKey: TShortCut;
    FReadOnly: Boolean;
    FDirectInput: Boolean;
    FAlwaysEnable: Boolean;
    FAlignment: TAlignment;
    //    FPopupVisible: Boolean; // Polaris
    //    FFocused: Boolean; // Polaris
    FPopupAlign: TPopupAlign;
    FGlyphKind: TGlyphKind;
    (* ++ RDB ++ *)
    FClipboardCommands: TJvClipboardCommands;
    FGroupIndex: Integer;
    FDisabledColor: TColor;
    FDisabledTextColor: TColor;
    FOnKeyDown: TKeyEvent;
    (* -- RDB -- *)
    procedure SetEditRect;
    procedure RecreateGlyph;
    procedure UpdateBtnBounds;
    procedure EditButtonClick(Sender: TObject);
    function GetMinHeight: Integer;
    function GetTextHeight: Integer;
    //    procedure SetShowCaret; // Polaris
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetPopupVisible: Boolean;
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    function GetButtonWidth: Integer;
    procedure SetButtonWidth(Value: Integer);
    function GetButtonHint: string;
    procedure SetButtonHint(const Value: string);
    function GetButtonFlat: Boolean;
    procedure SetButtonFlat(const Value: Boolean);
    function GetDirectInput: Boolean;
    //    procedure SetDirectInput(Value: Boolean);  // Polaris
    procedure SetReadOnly(Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
    function IsCustomGlyph: Boolean;
    function BtnWidthStored: Boolean;
    procedure SetGlyphKind(Value: TGlyphKind);
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMCancelMode(var Msg: TCMCancelMode); message CM_CANCELMODE;
    procedure CMEnter(var Msg: TMessage); message CM_ENTER;
    procedure CNCtlColor(var Msg: TMessage); message
      {$IFDEF WIN32}CN_CTLCOLOREDIT{$ELSE}CN_CTLCOLOR{$ENDIF};
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
    procedure WMPaste(var Msg: TWMPaste); message WM_PASTE;
    procedure WMCut(var Msg: TWMCut); message WM_CUT;
    {$IFDEF WIN32}
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    {$ENDIF}
    {$IFDEF COMPILER4_UP}
    procedure CMBiDiModeChanged(var Msg: TMessage); message CM_BIDIMODECHANGED;
    {$ENDIF}
    (* ++ RDB ++ *)
    procedure UpdateEdit;
    (* -- RDB -- *)
  protected
    FButton: TJvEditButton; // Polaris
    FPopupVisible: Boolean; // Polaris
    FFocused: Boolean; // Polaris
    FPopup: TCustomControl;
    FDefNumGlyphs: TNumGlyphs;
    procedure AdjustHeight;
    function GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap; virtual;
    procedure PopupDropDown(DisableEdit: Boolean); virtual;
    procedure PopupCloseUp(Sender: TObject; Accept: Boolean); virtual; //virtual Polaris
    procedure ShowPopup(Origin: TPoint); virtual;
    procedure HidePopup; virtual;
    procedure UpdatePopupVisible;
    procedure DoChange; virtual; //virtual Polaris
    procedure SetShowCaret; // Polaris
    procedure SetDirectInput(Value: Boolean); // Polaris
    {$IFDEF WIN32}
    function AcceptPopup(var Value: Variant): Boolean; virtual;
    procedure AcceptValue(const Value: Variant); virtual;
    procedure SetPopupValue(const Value: Variant); virtual;
    function GetPopupValue: Variant; virtual;
    {$ELSE}
    function AcceptPopup(var Value: string): Boolean; virtual;
    procedure AcceptValue(const Value: string); virtual;
    procedure SetPopupValue(const Value: string); virtual;
    function GetPopupValue: string; virtual;
    {$ENDIF}
    procedure Change; override;
    procedure PopupChange; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function EditCanModify: Boolean; override;
    function GetReadOnly: Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure ButtonClick; dynamic;
    (* ++ RDB ++ *)
    procedure WMCopy(var Msg: TWMCopy); message WM_COPY;
    procedure WMUndo(var Msg: TWMUndo); message WM_UNDO;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure LocalKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetDisabledColor(const Value: TColor); virtual;
    procedure SetDisabledTextColor(const Value: TColor); virtual;
    procedure SetClipboardCommands(const Value: TJvClipboardCommands);
    procedure SetGroupIndex(const Value: Integer);
    (* -- RDB -- *)
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AlwaysEnable: Boolean read FAlwaysEnable write FAlwaysEnable default False;
    property Button: TJvEditButton read FButton;
    property ClickKey: TShortCut read FClickKey write FClickKey default scAltDown;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsCustomGlyph;
    property GlyphKind: TGlyphKind read FGlyphKind write SetGlyphKind default gkCustom;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth stored BtnWidthStored;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property ButtonHint: string read GetButtonHint write SetButtonHint;
    property DirectInput: Boolean read GetDirectInput write SetDirectInput default True;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property PopupAlign: TPopupAlign read FPopupAlign write FPopupAlign default epaRight;
    property PopupVisible: Boolean read GetPopupVisible;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property ButtonFlat: Boolean read GetButtonFlat write SetButtonFlat;
    (* ++ RDB ++ *)
    property ClipboardCommands: TJvClipboardCommands read FClipboardCommands
      write SetClipboardCommands default [caCopy..caUndo];
    property DisabledTextColor: TColor read FDisabledTextColor write
      SetDisabledTextColor default clGrayText;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor
      default clWindow;
    (*      property GroupIndex: Integer read FGroupIndex write SetGroupIndex; *)
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    (* -- RDB -- *)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoClick;
    procedure SelectAll;
  end;

  TJvComboEdit = class(TJvCustomComboEdit)
  public
    property Button;
  published
    //Polaris
    property Align;
    property Alignment;
    property AutoSelect;
    property BorderStyle;
    property ButtonHint;
    property ButtonFlat;
    property CharCase;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property GlyphKind;
    { Ensure GlyphKind is published before Glyph and ButtonWidth }
    property Glyph;
    property ButtonWidth;
    property HideSelection;
    {$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    {$IFDEF WIN32}
    {$IFNDEF COMPILER2}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    {$ENDIF}
    property MaxLength;
    property NumGlyphs;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
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
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF WIN32}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF COMPILER5_UP}
    property OnContextPopup;
    {$ENDIF}
    {$IFDEF COMPILER4_UP}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
    (* ++ RDB ++ *)
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (*      property GroupIndex; *)
    property OnKeyDown;
    (* -- RDB -- *)
  end;

  { TJvFileDirEdit }
  { The common parent of TJvFilenameEdit and TJvDirectoryEdit          }
  { For internal use only; it's not intended to be used separately }

  {$IFNDEF WIN32}
const
  MaxFileLength = SizeOf(TFileName) - 1;
  {$ENDIF}

type
  TExecOpenDialogEvent = procedure(Sender: TObject; var Name: string;
    var Action: Boolean) of object;

  TJvFileDirEdit = class(TJvCustomComboEdit)
  private
    FErrMode: Cardinal;
    FAcceptFiles: Boolean;
    FMultipleDirs: Boolean;
    FOnDropFiles: TNotifyEvent;
    FOnBeforeDialog: TExecOpenDialogEvent;
    FOnAfterDialog: TExecOpenDialogEvent;
    procedure SetDragAccept(Value: Boolean);
    procedure SetAcceptFiles(Value: Boolean);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  protected
    procedure CreateHandle; override;
    procedure DestroyWindowHandle; override;
    function GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap; override;
    {$IFDEF WIN32}
    function GetLongName: string; virtual; abstract;
    function GetShortName: string; virtual; abstract;
    {$ENDIF}
    procedure DoAfterDialog(var FileName: string; var Action: Boolean); dynamic;
    procedure DoBeforeDialog(var FileName: string; var Action: Boolean); dynamic;
    procedure ReceptFileDir(const AFileName: string); virtual; abstract;
    procedure ClearFileList; virtual;
    procedure DisableSysErrors;
    procedure EnableSysErrors;
    property GlyphKind default gkDefault;
    property MaxLength{$IFNDEF WIN32}default MaxFileLength{$ENDIF};
  public
    constructor Create(AOwner: TComponent); override;
    {$IFDEF WIN32}
    property LongName: string read GetLongName;
    property ShortName: string read GetShortName;
    {$ENDIF}
  published
    property AcceptFiles: Boolean read FAcceptFiles write SetAcceptFiles default True;
    property OnBeforeDialog: TExecOpenDialogEvent read FOnBeforeDialog
      write FOnBeforeDialog;
    property OnAfterDialog: TExecOpenDialogEvent read FOnAfterDialog
      write FOnAfterDialog;
    property OnDropFiles: TNotifyEvent read FOnDropFiles write FOnDropFiles;
    property OnButtonClick;
    (* ++ RDB ++ *)
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (*      property GroupIndex; *)
    property OnKeyDown;
    (* -- RDB -- *)
  end;

  TFileDialogKind =
    (dkOpen, dkSave{$IFDEF COMPILER3_UP}, dkOpenPicture, dkSavePicture{$ENDIF});

  TJvFilenameEdit = class(TJvFileDirEdit)
  private
    FDialog: TOpenDialog;
    FDialogKind: TFileDialogKind;
    procedure CreateEditDialog;
    function GetFileName: string;
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
    procedure SetFileName(const Value: string);
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
  protected
    procedure ButtonClick; override;
    procedure ReceptFileDir(const AFileName: string); override;
    procedure ClearFileList; override;
    {$IFDEF WIN32}
    function GetLongName: string; override;
    function GetShortName: string; override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    property Dialog: TOpenDialog read FDialog;
    property DialogFiles: TStrings read GetDialogFiles;
  published
    //Polaris
    property Align;
    property DialogKind: TFileDialogKind read FDialogKind write SetDialogKind
      default dkOpen;
    property DefaultExt: TFileExt read GetDefaultExt write SetDefaultExt;
    property FileEditStyle: TFileEditStyle read GetFileEditStyle write SetFileEditStyle
      default fsEdit;
    property FileName: string read GetFileName write SetFileName stored False;
    property Filter: string read GetFilter write SetFilter stored IsCustomFilter;
    property FilterIndex: Integer read GetFilterIndex write SetFilterIndex default 1;
    property InitialDir: string read GetInitialDir write SetInitialDir;
    property HistoryList: TStrings read GetHistoryList write SetHistoryList;
    property DialogOptions: TOpenOptions read GetOptions write SetOptions
      default [ofHideReadOnly];
    property DialogTitle: string read GetDialogTitle write SetDialogTitle
      stored IsCustomTitle;
    property AutoSelect;
    property ButtonHint;
    property ButtonFlat;
    property BorderStyle;
    property CharCase;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property GlyphKind;
    { Ensure GlyphKind is declared before Glyph and ButtonWidth }
    property Glyph;
    property ButtonWidth;
    property HideSelection;
    {$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    {$IFDEF WIN32}
    {$IFDEF COMPILER3_UP}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    {$ENDIF}
    property NumGlyphs;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
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
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF WIN32}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF COMPILER5_UP}
    property OnContextPopup;
    {$ENDIF}
    {$IFDEF COMPILER4_UP}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
  end;

  {$IFDEF WIN32}
  TDirDialogKind = (dkVCL, dkWin32);
  {$ENDIF}

  TJvDirectoryEdit = class(TJvFileDirEdit)
  private
    FOptions: TSelectDirOpts;
    FInitialDir: string;
    {$IFDEF WIN32}
    FDialogText: string;
    FDialogKind: TDirDialogKind;
    {$ENDIF}
  protected
    FMultipleDirs: Boolean; // Polaris (???)
    procedure ButtonClick; override;
    procedure ReceptFileDir(const AFileName: string); override;
    {$IFDEF WIN32}
    function GetLongName: string; override;
    function GetShortName: string; override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  published
    //Polaris
    property Align;
    {$IFDEF WIN32}
    property DialogKind: TDirDialogKind read FDialogKind write FDialogKind default dkVCL;
    property DialogText: string read FDialogText write FDialogText;
    {$ENDIF}
    property DialogOptions: TSelectDirOpts read FOptions write FOptions default [];
    property InitialDir: string read FInitialDir write FInitialDir;
    property MultipleDirs: Boolean read FMultipleDirs write FMultipleDirs default False;
    property AutoSelect;
    property ButtonHint;
    property ButtonFlat;
    property BorderStyle;
    property CharCase;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property GlyphKind;
    { Ensure GlyphKind is declared before Glyph and ButtonWidth }
    property Glyph;
    property NumGlyphs;
    property ButtonWidth;
    property HideSelection;
    {$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    {$IFDEF WIN32}
    {$IFDEF COMPILER3_UP}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    {$ENDIF}
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
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
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF WIN32}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF COMPILER5_UP}
    property OnContextPopup;
    {$ENDIF}
    {$IFDEF COMPILER4_UP}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
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

  TJvCustomDateEdit = class(TJvCustomComboEdit)
  private
    FMinDate: TDateTime; // Polaris
    FMaxDate: TDateTime; // Polaris
    FTitle: string;
    FOnAcceptDate: TExecDateDialog;
    FDefaultToday: Boolean;
    FHooked: Boolean;
    FPopupColor: TColor;
    FCheckOnExit: Boolean;
    FBlanksChar: Char;
    FCalendarHints: TStrings;
    FStartOfWeek: TDayOfWeekName;
    FWeekends: TDaysOfWeek;
    FWeekendColor: TColor;
    FYearDigits: TYearDigits;
    FDateFormat: string[10];
    FFormatting: Boolean;
    // Polaris
    procedure SetMinDate(Value: TDateTime);
    procedure SetMaxDate(Value: TDateTime);
    // Polaris
    function GetDate: TDateTime;
    //    procedure SetDate(Value: TDateTime);
    procedure SetYearDigits(Value: TYearDigits);
    function GetPopupColor: TColor;
    procedure SetPopupColor(Value: TColor);
    function GetDialogTitle: string;
    procedure SetDialogTitle(const Value: string);
    function IsCustomTitle: Boolean;
    function GetCalendarStyle: TCalendarStyle;
    procedure SetCalendarStyle(Value: TCalendarStyle);
    procedure SetCalendarHints(Value: TStrings);
    procedure CalendarHintsChanged(Sender: TObject);
    procedure SetWeekendColor(Value: TColor);
    procedure SetWeekends(Value: TDaysOfWeek);
    procedure SetStartOfWeek(Value: TDayOfWeekName);
    procedure SetBlanksChar(Value: Char);
    function TextStored: Boolean;
    // Polaris
    function StoreMinDate: Boolean;
    function StoreMaxDate: Boolean;
    // Polaris
    function FourDigitYear: Boolean;
    function FormatSettingsChange(var Msg: TMessage): Boolean;
    procedure CMExit(var Msg: TCMExit); message CM_EXIT;
    procedure WMContextMenu(var Message: TWMContextMenu);message WM_CONTEXTMENU;
  protected
    // Polaris
    FDateAutoBetween: Boolean;
    procedure SetDate(Value: TDateTime); virtual;
    procedure SetDateAutoBetween(Value: Boolean); virtual;
    procedure TestDateBetween(var Value: TDateTime); virtual;
    // Polaris
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DestroyWindowHandle; override;
    {$IFDEF WIN32}
    function AcceptPopup(var Value: Variant): Boolean; override;
    procedure AcceptValue(const Value: Variant); override;
    procedure SetPopupValue(const Value: Variant); override;
    {$ELSE}
    function AcceptPopup(var Value: string): Boolean; override;
    {$ENDIF}
    function GetDateFormat: string;
    procedure ApplyDate(Value: TDateTime); virtual;
    function GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap; override;
    procedure UpdateFormat;
    procedure UpdatePopup;
    procedure ButtonClick; override;
    property BlanksChar: Char read FBlanksChar write SetBlanksChar default ' ';
    property CalendarHints: TStrings read FCalendarHints write SetCalendarHints;
    property CheckOnExit: Boolean read FCheckOnExit write FCheckOnExit default False;
    property DefaultToday: Boolean read FDefaultToday write FDefaultToday
      default False;
    property DialogTitle: string read GetDialogTitle write SetDialogTitle
      stored IsCustomTitle;
    property EditMask stored False;
    property Formatting: Boolean read FFormatting;
    property GlyphKind default gkDefault;
    property PopupColor: TColor read GetPopupColor write SetPopupColor
      default clMenu;
    property CalendarStyle: TCalendarStyle read GetCalendarStyle
      write SetCalendarStyle default dcsDefault;
    property StartOfWeek: TDayOfWeekName read FStartOfWeek write SetStartOfWeek default Mon;
    property Weekends: TDaysOfWeek read FWeekends write SetWeekends default [Sun];
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clRed;
    property YearDigits: TYearDigits read FYearDigits write SetYearDigits default dyDefault;
    property OnAcceptDate: TExecDateDialog read FOnAcceptDate write FOnAcceptDate;
    property MaxLength stored False;
    property Text stored TextStored;
  public
    // Polaris
    property DateAutoBetween: Boolean read FDateAutoBetween write SetDateAutoBetween default True;
    property MinDate: TDateTime read FMinDate write SetMinDate stored StoreMinDate;
    property MaxDate: TDateTime read FMaxDate write SetMaxDate stored StoreMaxDate;
    {$IFDEF COMPILER4_UP}
    procedure ValidateEdit; override;
    {$ENDIF}
    // Polaris
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CheckValidDate;
    function GetDateMask: string;
    procedure UpdateMask; virtual;
    property Date: TDateTime read GetDate write SetDate;
    property PopupVisible;
  end;

  TJvDateEdit = class(TJvCustomDateEdit)
    // Polaris
  protected
    procedure SetDate(Value: TDateTime); override;
    // Polaris
  public
    constructor Create(AOwner: TComponent); override;
    property EditMask;
  published
    // Polaris
    property DateAutoBetween;
    property MinDate;
    property MaxDate;
    property Align;
    // Polaris
    property AutoSelect;
    property BlanksChar;
    property BorderStyle;
    property ButtonHint;
    property ButtonFlat;
    property CalendarHints;
    property CheckOnExit;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DefaultToday;
    property DialogTitle;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GlyphKind;
    { Ensure GlyphKind is declared before Glyph and ButtonWidth }
    property Glyph;
    property ButtonWidth;
    property HideSelection;
    {$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    {$IFDEF WIN32}
    {$IFDEF COMPILER3_UP}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    {$ENDIF}
    property MaxLength;
    property NumGlyphs;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupAlign;
    property PopupColor;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property CalendarStyle;
    property StartOfWeek;
    property Weekends;
    property WeekendColor;
    property YearDigits;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnAcceptDate;
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
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF WIN32}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF COMPILER5_UP}
    property OnContextPopup;
    {$ENDIF}
    {$IFDEF COMPILER4_UP}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
    (* ++ RDB ++ *)
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (*      property GroupIndex; *)
    property OnKeyDown;
    (* -- RDB -- *)
  end;

  EComboEditError = class(EJVCLException);

  { Utility routines }

procedure DateFormatChanged;

function EditorTextMargins(Editor: TCustomEdit): TPoint;
function PaintComboEdit(Editor: TJvCustomComboEdit; const AText: string;
  AAlignment: TAlignment; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Msg: TWMPaint): Boolean;
function PaintEdit(Editor: TCustomEdit; const AText: string;
  AAlignment: TAlignment; PopupVisible: Boolean; ButtonWidth: Integer;
  DisabledTextColor: TColor; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Msg: TWMPaint): Boolean;

implementation

uses
  ShellAPI, Consts, Math,
  {$IFDEF JVCLThemesEnabled}
  Themes,
  {$ENDIF}
  {$IFDEF COMPILER3_UP}
  ExtDlgs,
  {$ENDIF}
  JvxRConst, JvVCLUtils, JvStrUtils, JvFileUtil, JvPickDate, JvBrowseFolder;

{$IFDEF WIN32}
{$R *.Res}
{$ELSE}
{$R *.R16}
{$ENDIF}

const
  sFileBmp = 'JV_FEDITBMP'; { Filename and directory editor button glyph }
  sDateBmp = 'JV_DEDITBMP'; { Date editor button glyph }

  { Utility routines }

type
  TCustomEditAccess = class(TCustomEdit);

function EditorTextMargins(Editor: TCustomEdit): TPoint;
var
  DC: HDC;
  SaveFont: HFONT;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  with TCustomEditAccess(Editor) do
  begin
    {$IFDEF WIN32}
    if NewStyleControls then
    begin
      if BorderStyle = bsNone then
        I := 0
      else if Ctl3D then
        I := 1
      else
        I := 2;
      if GetWindowLong(Handle, GWL_STYLE) and ES_MULTILINE = 0 then
        Result.X := (SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF) + I
      else
        Result.X := I;
      Result.Y := I;
    end
    else
    {$ENDIF}
    begin
      if BorderStyle = bsNone then
        I := 0
      else
      begin
        DC := GetDC(0);
        GetTextMetrics(DC, SysMetrics);
        SaveFont := SelectObject(DC, Font.Handle);
        GetTextMetrics(DC, Metrics);
        SelectObject(DC, SaveFont);
        ReleaseDC(0, DC);
        I := SysMetrics.tmHeight;
        if I > Metrics.tmHeight then
          I := Metrics.tmHeight;
        I := I div 4;
      end;
      Result.X := I;
      Result.Y := I;
    end;
  end;
end;

function PaintComboEdit(Editor: TJvCustomComboEdit; const AText: string;
  AAlignment: TAlignment; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Msg: TWMPaint): Boolean;
var
  ButtonWidth: Integer;
  R: TRect;
begin
  SendMessage(Editor.Handle, EM_GETRECT, 0, Integer(@R));
  {$IFDEF COMPILER4_UP}
  if Editor.BiDiMode = bdRightToLeft then
    ButtonWidth := R.Left - 1
  else
  {$ENDIF COMPILER4_UP}
    ButtonWidth := Editor.ClientWidth - R.Right - 2;
  if ButtonWidth < 0 then ButtonWidth := 0;

  Result := PaintEdit(Editor, AText, AAlignment, Editor.PopupVisible,
    ButtonWidth, Editor.FDisabledTextColor, StandardPaint, ACanvas, Msg);
end;

function PaintEdit(Editor: TCustomEdit; const AText: string;
  AAlignment: TAlignment; PopupVisible: Boolean; ButtonWidth: Integer;
  DisabledTextColor: TColor; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Msg: TWMPaint): Boolean;
var
  AWidth, ALeft: Integer;
  Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  {$IFDEF COMPILER4_UP}
  ExStyle: DWORD;
const
  AlignStyle: array[Boolean, TAlignment] of DWORD =
  ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
  {$ENDIF}
begin
  Result := True;
  with TCustomEditAccess(Editor) do
  begin
    {$IFDEF COMPILER4_UP}
    if UseRightToLeftAlignment then
      ChangeBiDiModeAlignment(AAlignment);
    {$ENDIF}
    if StandardPaint{$IFDEF WIN32} and not (csPaintCopy in ControlState){$ENDIF} then
    begin
      {$IFDEF COMPILER4_UP}
      if SysLocale.MiddleEast and HandleAllocated and (IsRightToLeft) then
      begin { This keeps the right aligned text, right aligned }
        ExStyle := DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) and (not WS_EX_RIGHT) and
          (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
        if UseRightToLeftReading then
          ExStyle := ExStyle or WS_EX_RTLREADING;
        if UseRightToLeftScrollBar then
          ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
        ExStyle := ExStyle or
          AlignStyle[UseRightToLeftAlignment, AAlignment];
        if DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) <> ExStyle then
          SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
      end;
      {$ENDIF COMPILER4_UP}
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
      TControlCanvas(ACanvas).Control := Editor;
    end;
    DC := Msg.DC;
    if DC = 0 then
      DC := BeginPaint(Handle, PS);
    ACanvas.Handle := DC;
    try
      ACanvas.Font := Font;
      with ACanvas do
      begin
        R := ClientRect;
        if {$IFDEF WIN32} not (NewStyleControls and Ctl3D) and {$ENDIF}(BorderStyle = bsSingle) then
        begin
          Brush.Color := clWindowFrame;
          FrameRect(R);
          InflateRect(R, -1, -1);
        end;
        S := AText;
        AWidth := TextWidth(S);
        Margins := EditorTextMargins(Editor);
        if PopupVisible then
          ALeft := Margins.X
        else
        begin
          if ButtonWidth > 0 then
            Inc(AWidth);
          case AAlignment of
            taLeftJustify:
              ALeft := Margins.X;
            taRightJustify:
              ALeft := ClientWidth - ButtonWidth - AWidth - Margins.X - 1 {Polaris - 2};
          else
            ALeft := (ClientWidth - ButtonWidth - AWidth) div 2;
          end;
        end;
        {$IFDEF COMPILER4_UP}
        if SysLocale.MiddleEast then
          UpdateTextFlags;
        {$ENDIF}
        if not Enabled then
        begin
          if PS.fErase then
            Perform(WM_ERASEBKGND, ACanvas.Handle, 0);

          SaveDC(ACanvas.Handle);
          try
            ACanvas.Brush.Style := bsClear;
            ACanvas.Font.Color := DisabledTextColor;
            ACanvas.TextRect(R, ALeft, Margins.Y, S);
          finally
            RestoreDC(ACanvas.Handle, -1);
          end;
        end
        else
        begin
          Brush.Color := Color;
          ACanvas.TextRect(R, ALeft, Margins.Y, S);
        end;
      end;
    finally
      ACanvas.Handle := 0;
      if Msg.DC = 0 then
        EndPaint(Handle, PS);
    end;
  end;
end;

//=== TJvEditButton ==========================================================

constructor TJvEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStandard := True; // Polaris
  {$IFDEF WIN32}
  ControlStyle := ControlStyle + [csReplicatable];
  {$ELSE}
  Style := bsWin31;
  {$ENDIF}
  ParentShowHint := True;
end;

procedure TJvEditButton.Paint;
{$IFDEF JVCLThemesEnabled}
var
  ThemedState: TThemedComboBox;
  Details: TThemedElementDetails;
  R: TRect;
{$ENDIF}
begin
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
  begin
    if FDrawThemedDropDownBtn then
    begin
      if not Enabled then
        ThemedState := tcDropDownButtonDisabled
      else
      if FState = rbsDown then
        ThemedState := tcDropDownButtonPressed
      else
      if MouseInControl then
        ThemedState := tcDropDownButtonHot
      else
        ThemedState := tcDropDownButtonNormal;
      R := BoundsRect;
      Details := ThemeServices.GetElementDetails(ThemedState);
      ThemeServices.DrawElement(Canvas.Handle, Details, R);
    end
    else
      inherited Paint;
  end
  else
  {$ENDIF}
  begin
    inherited Paint;
    if FState <> rbsDown then
      with Canvas do
      begin
        if NewStyleControls then
          Pen.Color := clBtnFace
        else
          Pen.Color := clBtnShadow;
        MoveTo(0, 0);
        LineTo(0, Self.Height - 1);
        Pen.Color := clBtnHighlight;
        MoveTo(1, 1);
        LineTo(1, Self.Height - 2);
      end;
  end;
end;

procedure TJvEditButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
    with TJvCustomComboEdit(Owner) do
    begin
      FNoAction := (FPopup <> nil) and FPopupVisible;
      if not FPopupVisible then
      begin
        if TabStop and CanFocus and (GetFocus <> Handle) then
          SetFocus;
      end
      else
        PopupCloseUp(FPopup, FStandard); // Polaris
    end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvEditButton.Click;
begin
  if not FNoAction then
    inherited Click
  else
    FNoAction := False;
end;

//=== TJvPopupWindow =========================================================

constructor TJvPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditor := TWinControl(AOwner);
  {$IFDEF WIN32}
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable, csAcceptsControls];
  {$ELSE}
  ControlStyle := ControlStyle + [csAcceptsControls];
  {$ENDIF}
  Ctl3D := False;
  ParentCtl3D := False;
  Visible := False;
  Parent := FEditor;
  OnMouseUp := PopupMouseUp;
end;

procedure TJvPopupWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER or WS_CLIPCHILDREN;
    {$IFDEF WIN32}
    ExStyle := WS_EX_TOOLWINDOW;
    {$ENDIF}
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

{$IFNDEF WIN32}

procedure TJvPopupWindow.CreateWnd;
begin
  inherited CreateWnd;
  if csDesigning in ComponentState then
    SetParent(nil);
end;
{$ENDIF}

procedure TJvPopupWindow.WMMouseActivate(var Msg: TMessage);
begin
  Msg.Result := MA_NOACTIVATE;
end;

function TJvPopupWindow.GetPopupText: string;
begin
  Result := '';
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
  InvalidateRect(FEditor.Handle, @R, False);
  UpdateWindow(FEditor.Handle);
end;

procedure TJvPopupWindow.PopupMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(Self.ClientRect, Point(X, Y)));
end;

procedure TJvPopupWindow.CloseUp(Accept: Boolean);
begin
  if Assigned(FCloseUp) then
    FCloseUp(Self, Accept);
end;

procedure TJvPopupWindow.Hide;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TJvPopupWindow.Show(Origin: TPoint);
begin
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
end;

//=== TJvCustomComboEdit =====================================================

constructor TJvCustomComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF COMPILER3_UP}
  ControlStyle := ControlStyle + [csCaptureMouse];
  {$ENDIF}
  //  AutoSize := False;   // Polaris
  FDirectInput := True;
  FClickKey := scAltDown;
  FPopupAlign := epaRight;
  FBtnControl := TWinControl.Create(Self);
  {$IFDEF WIN32}
  with FBtnControl do
    ControlStyle := ControlStyle + [csReplicatable];
  {$ENDIF}
  FBtnControl.Width := DefEditBtnWidth;
  FBtnControl.Height := 17;
  FBtnControl.Visible := True;
  FBtnControl.Parent := Self;
  FButton := TJvEditButton.Create(Self);
  FButton.SetBounds(0, 0, FBtnControl.Width, FBtnControl.Height);
  FButton.Visible := True;
  FButton.Parent := FBtnControl;
  TJvEditButton(FButton).OnClick := EditButtonClick;
  Height := 21;
  FDefNumGlyphs := 1;
  FGlyphKind := gkCustom;
  (* ++ RDB ++ *)
  FDisabledColor := clWindow;
  FDisabledTextColor := clGrayText;
  FClipboardCommands := [caCopy..caUndo];
  FGroupIndex := -1;
  inherited OnKeyDown := LocalKeyDown;
  (* -- RDB -- *)
end;

destructor TJvCustomComboEdit.Destroy;
begin
  FButton.OnClick := nil;
  inherited Destroy;
end;

(* ++ RDB ++ *)

procedure TJvCustomComboEdit.LocalKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateEdit;
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Sender, Key, Shift);
end;

procedure TJvCustomComboEdit.WMEraseBkgnd(var Msg: TWmEraseBkgnd);
var
  Canvas: TCanvas;
begin
  if Enabled then
    inherited
  else
  begin
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := Msg.DC;
      SaveDC(Msg.DC);
      try
        Canvas.Brush.Color := FDisabledColor;
        Canvas.Brush.Style := bsSolid;
        Canvas.FillRect(ClientRect);
        Msg.Result := 1;
      finally
        RestoreDC(Msg.DC, -1);
      end;
    finally
      Canvas.Free
    end;
  end;
end;

procedure TJvCustomComboEdit.SetClipboardCommands(
  const Value: TJvClipboardCommands);
begin
  if FClipboardCommands <> Value then
  begin
    FClipboardCommands := Value;
    ReadOnly := FClipboardCommands <= [caCopy];
  end;
end;

procedure TJvCustomComboEdit.SetGroupIndex(const Value: Integer);
begin
  FGroupIndex := Value;
  UpdateEdit;
end;

procedure TJvCustomComboEdit.UpdateEdit;
var
  I: Integer;
begin
  for I := 0 to Self.Owner.ComponentCount - 1 do
    if Self.Owner.Components[I] is TJvCustomComboEdit then
      if ((Self.Owner.Components[I].Name <> Self.Name) and
        ((Self.Owner.Components[I] as TJvCustomComboEdit).FGroupIndex <> -1) and
        ((Self.Owner.Components[I] as TJvCustomComboEdit).FGroupIndex = Self.FGroupIndex)) then
        (Self.Owner.Components[I] as TJvCustomComboEdit).Caption := '';
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

procedure TJvCustomComboEdit.WMCopy(var Msg: TWMCopy);
begin
  if caCopy in ClipboardCommands then
    inherited;
end;

procedure TJvCustomComboEdit.WMUndo(var Msg: TWMUndo);
begin
  if caUndo in ClipboardCommands then
    inherited;
end;

procedure TJvCustomComboEdit.WMPaint(var Msg: TWMPaint);
var
  Canvas: TControlCanvas;
begin
  if Enabled then
    inherited
  else
  begin
    Canvas := nil;
    if not PaintComboEdit(Self, Text, FAlignment, Focused and not PopupVisible,
       Canvas, Msg) then
      inherited;
    Canvas.Free;
  end;
end;

(* -- RDB -- *)

procedure TJvCustomComboEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array [TAlignment] of Longword = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN
    or Alignments[FAlignment];
end;

procedure TJvCustomComboEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TJvCustomComboEdit.HidePopup;
begin
  TJvPopupWindow(FPopup).Hide;
end;

procedure TJvCustomComboEdit.ShowPopup(Origin: TPoint);
begin
  TJvPopupWindow(FPopup).Show(Origin);
end;

procedure TJvCustomComboEdit.PopupDropDown(DisableEdit: Boolean);
var
  P: TPoint;
  Y: Integer;
begin
  if (FPopup <> nil) and not (ReadOnly or FPopupVisible) then
  begin
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FPopup.Height > Screen.Height then
      Y := P.Y - FPopup.Height;
    case FPopupAlign of
      epaRight:
        begin
          Dec(P.X, FPopup.Width - Width);
          if P.X < 0 then
            Inc(P.X, FPopup.Width - Width);
        end;
      epaLeft:
        begin
          if P.X + FPopup.Width > Screen.Width then
            Dec(P.X, FPopup.Width - Width);
        end;
    end;
    if P.X < 0 then
      P.X := 0
    else if P.X + FPopup.Width > Screen.Width then
      P.X := Screen.Width - FPopup.Width;
    {$IFDEF WIN32}
    if Text <> '' then
      SetPopupValue(Text)
    else
      SetPopupValue(NULL);
    {$ELSE}
    SetPopupValue(Text);
    {$ENDIF}
    if CanFocus then
      SetFocus;
    ShowPopup(Point(P.X, Y));
    FPopupVisible := True;
    if DisableEdit then
    begin
      inherited ReadOnly := True;
      HideCaret(Handle);
    end;
  end;
end;

procedure TJvCustomComboEdit.PopupCloseUp(Sender: TObject; Accept: Boolean);
var
  {$IFDEF WIN32}
  AValue: Variant;
  {$ELSE}
  AValue: string;
  {$ENDIF}
begin
  if (FPopup <> nil) and FPopupVisible then
  begin
    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    AValue := GetPopupValue;
    HidePopup;
    try
      try
        if CanFocus then
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
      if Accept and AcceptPopup(AValue) and EditCanModify then
      begin
        AcceptValue(AValue);
        if FFocused then
          inherited SelectAll;
      end;
    finally
      FPopupVisible := False;
    end;
  end;
end;

procedure TJvCustomComboEdit.DoChange;
begin
  inherited Change;
end;

{$IFDEF WIN32}

function TJvCustomComboEdit.GetPopupValue: Variant;
{$ELSE}

function TJvCustomComboEdit.GetPopupValue: string;
{$ENDIF}
begin
  if FPopup <> nil then
    Result := TJvPopupWindow(FPopup).GetValue
  else
    Result := '';
end;

{$IFDEF WIN32}

procedure TJvCustomComboEdit.SetPopupValue(const Value: Variant);
{$ELSE}

procedure TJvCustomComboEdit.SetPopupValue(const Value: string);
{$ENDIF}
begin
  if FPopup <> nil then
    TJvPopupWindow(FPopup).SetValue(Value);
end;

{$IFDEF WIN32}

procedure TJvCustomComboEdit.AcceptValue(const Value: Variant);
begin
  if Text <> VarToStr(Value) then
  begin
    {$ELSE}

procedure TJvCustomComboEdit.AcceptValue(const Value: string);
begin
  if Text <> Value then
  begin
    {$ENDIF}
    Text := Value;
    Modified := True;
    UpdatePopupVisible;
    DoChange;
  end;
end;

{$IFDEF WIN32}

function TJvCustomComboEdit.AcceptPopup(var Value: Variant): Boolean;
{$ELSE}

function TJvCustomComboEdit.AcceptPopup(var Value: string): Boolean;
{$ENDIF}
begin
  Result := True;
end;

function TJvCustomComboEdit.EditCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

procedure TJvCustomComboEdit.Change;
begin
  if not PopupVisible then
    DoChange
  else
    PopupChange;
end;

procedure TJvCustomComboEdit.PopupChange;
begin
end;

type
  TCrackWin = class(TWinControl);

procedure TJvCustomComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
//Polaris
var
  Form: TCustomForm;
begin
  //Polaris
  Form := GetParentForm(Self);
  if (ssCtrl in Shift) then
    case Key of
      VK_RETURN:
        if (Form <> nil) {and Form.KeyPreview} then
        begin
          TCrackWin(Form).KeyDown(Key, Shift);
          Key := 0;
        end;
      VK_TAB:
        if (Form <> nil) {and Form.KeyPreview} then
        begin
          TCrackWin(Form).KeyDown(Key, Shift);
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

  //Polaris  if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) then
  if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) or ((Key = #10) and PopupVisible) then
  begin
    if PopupVisible then
    begin
      //Polaris      PopupCloseUp(FPopup, Key = Char(VK_RETURN));
      PopupCloseUp(FPopup, Key <> Char(VK_ESCAPE));
      Key := #0;
    end
    else
    begin
      { must catch and remove this, since is actually multi-line }
      GetParentForm(Self).Perform(CM_DIALOGKEY, Byte(Key), 0);
      if Key = Char(VK_RETURN) then
      begin
        inherited KeyPress(Key);
        Key := #0;
        Exit;
      end;
    end;
  end;
  //Polaris
  if Key in [#10, #9] then
  begin
    Key := #0;
    if (Form <> nil) {and Form.KeyPreview} then
      TCrackWin(Form).KeyPress(Key);
  end;
  //Polaris
  inherited KeyPress(Key);
end;

procedure TJvCustomComboEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (FPopup <> nil) and (Button = mbLeft) then
  begin
    if CanFocus then
      SetFocus;
    if not FFocused then
      Exit;
    if FPopupVisible then
      PopupCloseUp(FPopup, False);
    {else if (not ReadOnly or AlwaysEnable) and (not DirectInput) then
      PopupDropDown(True);}
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

function TJvCustomComboEdit.GetButtonWidth: Integer;
begin
  Result := FButton.Width;
end;

procedure TJvCustomComboEdit.SetButtonWidth(Value: Integer);
begin
  if ButtonWidth <> Value then
  begin
    FBtnControl.Visible := Value > 1;
    if csCreating in ControlState then
    begin
      FBtnControl.Width := Value;
      FButton.Width := Value;
      with FButton do
        ControlStyle := ControlStyle - [csFixedWidth];
      RecreateGlyph;
    end
      //    else if (Value <> ButtonWidth) and (Value < ClientWidth) then begin
      //Polaris
    else if (Value <> ButtonWidth) and
      ((Assigned(Parent) and (Value < ClientWidth)) or
      (not Assigned(Parent) and (Value < Width))) then
    begin
      FButton.Width := Value;
      with FButton do
        ControlStyle := ControlStyle - [csFixedWidth];
      if HandleAllocated then
        RecreateWnd;
      RecreateGlyph;
    end;
  end;
end;

function TJvCustomComboEdit.GetButtonHint: string;
begin
  Result := FButton.Hint;
end;

procedure TJvCustomComboEdit.SetButtonHint(const Value: string);
begin
  FButton.Hint := Value;
end;

function TJvCustomComboEdit.GetButtonFlat: Boolean;
begin
  Result := FButton.Flat;
end;

procedure TJvCustomComboEdit.SetButtonFlat(const Value: Boolean);
begin
  FButton.Flat := Value;
  {$IFDEF JVCLThemesEnabled}
  { When XP Themes are enabled, ButtonFlat = False, GlyphKind = gkDropDown then
    the glyph is the default themed dropdown button. When ButtonFlat = True, we
    can't use that default dropdown button, so we have to recreate the glyph
    in this special case }
  if ThemeServices.ThemesEnabled and (GlyphKind = gkDropDown) then
    RecreateGlyph;
  {$ENDIF}
end;

function TJvCustomComboEdit.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TJvCustomComboEdit.SetGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
  FGlyphKind := gkCustom;
end;

function TJvCustomComboEdit.GetNumGlyphs: TNumGlyphs;
begin
  Result := FButton.NumGlyphs;
end;

procedure TJvCustomComboEdit.SetNumGlyphs(Value: TNumGlyphs);
begin
  if FGlyphKind in [gkDropDown, gkEllipsis] then
    FButton.NumGlyphs := 1
  else if FGlyphKind = gkDefault then
    FButton.NumGlyphs := FDefNumGlyphs
  else
    FButton.NumGlyphs := Value;
end;

procedure TJvCustomComboEdit.SetEditRect;
var
  Loc: TRect;
  LLeft: Integer;
begin
  AdjustHeight;
  {$IFDEF JVCLThemesEnabled}
  { If flat and themes are enabled, move the left edge of the edit rectangle
    to the right, otherwise the theme edge paints over the border }
  if not Ctl3D and (BorderStyle = bsSingle) and ThemeServices.ThemesEnabled then
    LLeft := 3
  else
  {$ENDIF}
    LLeft := 0;
  SetRect(Loc, LLeft, 0, ClientWidth - FBtnControl.Width {Polaris - 2}, ClientHeight + 1);
  SendMessage(Handle, EM_SETRECTNP, 0, Longint(@Loc));
  //Polaris
  //  SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN, MakeLong(0, FBtnControl.Width));
end;

procedure TJvCustomComboEdit.UpdateBtnBounds;
var
  BtnRect: TRect;
begin
  {$IFDEF WIN32}
  if NewStyleControls then
    {$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
    begin
      if Ctl3D and (BorderStyle = bsSingle) then
        { Actually same rectangle as without xp themes }
        BtnRect := Bounds(Width - FButton.Width - 4, 0,
          FButton.Width, Height - 4)
      else
        BtnRect := Bounds(Width - FButton.Width - 1, 1,
          FButton.Width, Height - 2)
    end
    else
    {$ENDIF JVCLThemesEnabled}
    begin
      if Ctl3D and (BorderStyle = bsSingle) then
        BtnRect := Bounds(Width - FButton.Width - 4, 0,
          FButton.Width, Height - 4)
      else
      begin
        if BorderStyle = bsSingle then
          BtnRect := Bounds(Width - FButton.Width - 2, 2,
            FButton.Width, Height - 4)
        else
          BtnRect := Bounds(Width - FButton.Width, 0,
            FButton.Width, Height);
      end;
    end
  else
    BtnRect := Bounds(Width - FButton.Width, 0, FButton.Width, Height);
  {$ELSE}
  BtnRect := Bounds(Width - FButton.Width, 0, FButton.Width, Height);
  {$ENDIF WIN32}
  with BtnRect do
    FBtnControl.SetBounds(Left, Top, Right - Left, Bottom - Top);
  FButton.Height := FBtnControl.Height;
  SetEditRect;
end;

{$IFDEF WIN32}

procedure TJvCustomComboEdit.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  UpdateBtnBounds;
end;
{$ENDIF}

procedure TJvCustomComboEdit.WMSize(var Msg: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
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
  UpdateBtnBounds;
end;

procedure TJvCustomComboEdit.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if NewStyleControls then
  begin
    if Ctl3D then
      I := 8
    else
      I := 6;
    I := GetSystemMetrics(SM_CYBORDER) * I;
  end
  else
  begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then
      I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  Height := Metrics.tmHeight + I;
end;

function TJvCustomComboEdit.GetTextHeight: Integer;
var
  DC: HDC;
  SaveFont: HFONT;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
  //  Result := Min(SysMetrics.tmHeight, Metrics.tmHeight);  // Polaris
  Result := Metrics.tmHeight; // Polaris
end;

function TJvCustomComboEdit.GetMinHeight: Integer;
var
  I: Integer;
begin
  I := GetTextHeight;
  if BorderStyle = bsSingle then
    I := I + GetSystemMetrics(SM_CYBORDER) * 4 + 1 {$IFNDEF WIN32} + (I div 4){$ENDIF};
  Result := I;
end;

procedure TJvCustomComboEdit.UpdatePopupVisible;
begin
  FPopupVisible := (FPopup <> nil) and FPopup.Visible;
end;

function TJvCustomComboEdit.GetPopupVisible: Boolean;
begin
  Result := (FPopup <> nil) and FPopupVisible;
end;

procedure TJvCustomComboEdit.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  if HandleAllocated then
    SetEditRect;
end;

procedure TJvCustomComboEdit.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  (* ++ RDB ++ *)
  Invalidate;
  (* -- RDB -- *)
  FButton.Enabled := Enabled;
end;

procedure TJvCustomComboEdit.CMCancelMode(var Msg: TCMCancelMode);
begin
  if (Msg.Sender <> Self) and (Msg.Sender <> FPopup) and
    (Msg.Sender <> FButton) and ((FPopup <> nil) and
    not FPopup.ContainsControl(Msg.Sender)) then
    PopupCloseUp(FPopup, False);
end;

procedure TJvCustomComboEdit.CMEnter(var Msg: TMessage);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TJvCustomComboEdit.CNCtlColor(var Msg: TMessage);
var
  TextColor: Longint;
begin
  inherited;
  if NewStyleControls then
  begin
    TextColor := ColorToRGB(Font.Color);
    if not Enabled and (ColorToRGB(Color) <> ColorToRGB(clGrayText)) then
      TextColor := ColorToRGB(clGrayText);
    SetTextColor(Msg.WParam, TextColor);
  end;
end;

procedure TJvCustomComboEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  FFocused := False;
  PopupCloseUp(FPopup, False);
end;

procedure TJvCustomComboEdit.WMSetFocus(var Msg: TMessage);
begin
  inherited;
  FFocused := True;
  SetShowCaret;
end;

{$IFDEF COMPILER4_UP}

procedure TJvCustomComboEdit.CMBiDiModeChanged(var Msg: TMessage);
begin
  inherited;
  if FPopup <> nil then
    FPopup.BiDiMode := BiDiMode;
end;
{$ENDIF}

procedure TJvCustomComboEdit.SetShowCaret;
const
  CaretWidth: array [Boolean] of Byte = (1, 2);
begin
  CreateCaret(Handle, 0, CaretWidth[fsBold in Font.Style], GetTextHeight);
  ShowCaret(Handle);
end;

procedure TJvCustomComboEdit.EditButtonClick(Sender: TObject);
begin
  if (not FReadOnly) or AlwaysEnable then
    ButtonClick;
end;

procedure TJvCustomComboEdit.DoClick;
begin
  EditButtonClick(Self);
end;

procedure TJvCustomComboEdit.ButtonClick;
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self);
  if FPopup <> nil then
  begin
    if FPopupVisible then
      PopupCloseUp(FPopup, True)
    else
      PopupDropDown(True);
  end;
end;

procedure TJvCustomComboEdit.SelectAll;
begin
  if DirectInput then
    inherited SelectAll;
end;

function TJvCustomComboEdit.GetDirectInput: Boolean;
begin
  Result := FDirectInput;
end;

procedure TJvCustomComboEdit.SetDirectInput(Value: Boolean);
begin
  inherited ReadOnly := not Value or FReadOnly;
  FDirectInput := Value;
end;

procedure TJvCustomComboEdit.WMPaste(var Msg: TWMPaste);
begin
  if not FDirectInput or ReadOnly then
    Exit;
  UpdateEdit;
  inherited;
end;

procedure TJvCustomComboEdit.WMCut(var Msg: TWMCut);
begin
  if not FDirectInput or ReadOnly then
    Exit;
  inherited;
end;

function TJvCustomComboEdit.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TJvCustomComboEdit.SetReadOnly(Value: Boolean);
begin
  if Value <> FReadOnly then
  begin
    FReadOnly := Value;
    inherited ReadOnly := Value or not FDirectInput;
  end;
end;

procedure TJvCustomComboEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

function TJvCustomComboEdit.BtnWidthStored: Boolean;
begin
  if (FGlyphKind = gkDefault) and (Glyph <> nil) then
    Result := ButtonWidth <> Max(Glyph.Width div FButton.NumGlyphs + 6,
      DefEditBtnWidth)
  else if FGlyphKind = gkDropDown then
    Result := ButtonWidth <> GetSystemMetrics(SM_CXVSCROLL){$IFNDEF WIN32} + 1{$ENDIF}
  else
    Result := ButtonWidth <> DefEditBtnWidth;
end;

function TJvCustomComboEdit.IsCustomGlyph: Boolean;
begin
  Result := FGlyphKind = gkCustom;
end;

procedure TJvCustomComboEdit.SetGlyphKind(Value: TGlyphKind);
begin
  if FGlyphKind <> Value then
  begin
    FGlyphKind := Value;
    if (FGlyphKind = gkCustom) and (csReading in ComponentState) then
    begin
      Glyph := nil;
    end;
    RecreateGlyph;
    if (FGlyphKind = gkDefault) and (Glyph <> nil) then
      ButtonWidth := Max(Glyph.Width div FButton.NumGlyphs + 6, FButton.Width)
    else if FGlyphKind = gkDropDown then
    begin
      ButtonWidth := GetSystemMetrics(SM_CXVSCROLL){$IFNDEF WIN32} + 1{$ENDIF};
      with FButton do
        ControlStyle := ControlStyle + [csFixedWidth];
    end;
  end;
end;

function TJvCustomComboEdit.GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap;
begin
  Result := nil;
end;

procedure TJvCustomComboEdit.RecreateGlyph;
var
  NewGlyph: TBitmap;
  DestroyNeeded: Boolean;

  function CreateEllipsisGlyph: TBitmap;
  var
    W, G, I: Integer;
  begin
    Result := TBitmap.Create;
    with Result do
    try
      Monochrome := True;
      Width := Max(1, FButton.Width - 6);
      Height := 4;
      W := 2;
      G := (Result.Width - 3 * W) div 2;
      if G <= 0 then
        G := 1;
      if G > 3 then
        G := 3;
      I := (Width - 3 * W - 2 * G) div 2;
      PatBlt(Canvas.Handle, I, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + G + W, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + 2 * G + 2 * W, 1, W, W, BLACKNESS);
    except
      Free;
      raise;
    end;
  end;

begin
  {$IFDEF JVCLThemesEnabled}
  { When XP Themes are enabled, ButtonFlat = False, GlyphKind = gkDropDown then
    the glyph is the default themed dropdown button. When ButtonFlat = True, we
    can't use that default dropdown button (because we then use toolbar buttons,
    and there is no themed dropdown toolbar button) }
  if ThemeServices.ThemesEnabled then
    FButton.FDrawThemedDropDownBtn := (FGlyphKind = gkDropDown) and not ButtonFlat;
  {$ENDIF}

  case FGlyphKind of
    gkDefault:
      begin
        DestroyNeeded := False;
        NewGlyph := GetDefaultBitmap(DestroyNeeded);
        try
          FButton.Glyph.Assign(NewGlyph);
          NumGlyphs := FDefNumGlyphs;
        finally
          if DestroyNeeded then
            NewGlyph.Destroy;
        end;
      end;
    gkDropDown:
      {$IFDEF JVCLThemesEnabled}
      if ButtonFlat or not ThemeServices.ThemesEnabled then
      {$ENDIF}
      begin
        FButton.Glyph.Handle := LoadBitmap(0, PChar(32738));
        NumGlyphs := 1;
      end;
    gkEllipsis:
      begin
        NewGlyph := CreateEllipsisGlyph;
        try
          FButton.Glyph := NewGlyph;
          NumGlyphs := 1;
        finally
          NewGlyph.Destroy;
        end;
      end;
  end;
end;

//=== TJvFileDirEdit =========================================================

// (rom) changed to var
var
  FileBitmap: TBitmap = nil;
  DateBitmap: TBitmap = nil;

constructor TJvFileDirEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OEMConvert := True;
  FAcceptFiles := True;
  {$IFNDEF WIN32}
  MaxLength := MaxFileLength;
  {$ENDIF}
  ControlState := ControlState + [csCreating];
  try
    GlyphKind := gkDefault; { force update }
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

function TJvFileDirEdit.GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap;
begin
  DestroyNeeded := False;
  if FileBitmap = nil then
  begin
    FileBitmap := TBitmap.Create;
    FileBitmap.Handle := LoadBitmap(HInstance, sFileBmp);
  end;
  Result := FileBitmap;
end;

procedure TJvFileDirEdit.DoBeforeDialog(var FileName: string;
  var Action: Boolean);
begin
  if Assigned(FOnBeforeDialog) then
    FOnBeforeDialog(Self, FileName, Action);
end;

procedure TJvFileDirEdit.DoAfterDialog(var FileName: string;
  var Action: Boolean);
begin
  if Assigned(FOnAfterDialog) then
    FOnAfterDialog(Self, FileName, Action);
end;

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

procedure TJvFileDirEdit.SetDragAccept(Value: Boolean);
begin
  if not (csDesigning in ComponentState) and (Handle <> 0) then
    DragAcceptFiles(Handle, Value);
end;

procedure TJvFileDirEdit.SetAcceptFiles(Value: Boolean);
begin
  if FAcceptFiles <> Value then
  begin
    SetDragAccept(Value);
    FAcceptFiles := Value;
  end;
end;

procedure TJvFileDirEdit.DisableSysErrors;
begin
  FErrMode := SetErrorMode(SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
end;

procedure TJvFileDirEdit.EnableSysErrors;
begin
  SetErrorMode(FErrMode);
  FErrMode := 0;
end;

procedure TJvFileDirEdit.WMDropFiles(var Msg: TWMDropFiles);
var
  AFileName: array [0..255] of Char;
  I, Num: Cardinal;
begin
  Msg.Result := 0;
  try
    {$IFDEF WIN32}
    Num := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
    {$ELSE}
    Num := DragQueryFile(Msg.Drop, $FFFF, nil, 0);
    {$ENDIF}
    if Num > 0 then
    begin
      ClearFileList;
      for I := 0 to Num - 1 do
      begin
        DragQueryFile(Msg.Drop, I, PChar(@AFileName), Pred(SizeOf(AFileName)));
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

procedure TJvFileDirEdit.ClearFileList;
begin
end;

//=== TJvFilenameEdit ========================================================

function ClipFilename(const FileName: string): string;
var
  Params: string;
begin
  if FileExists(FileName) then
    Result := FileName
  else
    SplitCommandLine(FileName, Result, Params);
end;

function ExtFilename(const FileName: string): string;
begin
  if (Pos(' ', FileName) > 0) and (FileName[1] <> '"') then
    Result := Format('"%s"', [FileName])
  else
    Result := FileName;
end;

constructor TJvFilenameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateEditDialog;
end;

procedure TJvFilenameEdit.CreateEditDialog;
var
  NewDialog: TOpenDialog;
begin
  case FDialogKind of
    dkOpen:
      NewDialog := TOpenDialog.Create(Self);
    {$IFDEF COMPILER3_UP}
    dkOpenPicture: NewDialog := TOpenPictureDialog.Create(Self);
    dkSavePicture: NewDialog := TSavePictureDialog.Create(Self);
    {$ENDIF}
  else {dkSave}
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
      NewDialog.Title := SBrowse;
      NewDialog.Filter := SDefaultFilter;
      NewDialog.Options := [ofHideReadOnly];
    end;
  finally
    FDialog := NewDialog;
  end;
end;

function TJvFilenameEdit.IsCustomTitle: Boolean;
begin
  Result := CompareStr(SBrowse, FDialog.Title) <> 0;
end;

function TJvFilenameEdit.IsCustomFilter: Boolean;
begin
  Result := CompareStr(SDefaultFilter, FDialog.Filter) <> 0;
end;

procedure TJvFilenameEdit.ButtonClick;
var
  Temp: string;
  Action: Boolean;
begin
  inherited ButtonClick;
  Temp := inherited Text;
  Action := True;
  Temp := ClipFilename(Temp);
  DoBeforeDialog(Temp, Action);
  if not Action then
    Exit;
  if ValidFileName(Temp) then
  try
    if DirExists(ExtractFilePath(Temp)) then
      SetInitialDir(ExtractFilePath(Temp));
    if (ExtractFileName(Temp) = '') or
      not ValidFileName(ExtractFileName(Temp)) then
      Temp := '';
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
    inherited Text := ExtFilename(Temp);
    SetInitialDir(ExtractFilePath(FDialog.FileName));
  end;
end;

function TJvFilenameEdit.GetFileName: string;
begin
  Result := ClipFilename(inherited Text);
end;

procedure TJvFilenameEdit.SetFileName(const Value: string);
begin
  if (Value = '') or ValidFileName(ClipFilename(Value)) then
  begin
    inherited Text := ExtFilename(Value);
    ClearFileList;
  end
  else
    raise EComboEditError.CreateFmt(ResStr(SInvalidFilename), [Value]);
end;

{$IFDEF WIN32}

function TJvFilenameEdit.GetLongName: string;
begin
  Result := ShortToLongFileName(FileName);
end;

function TJvFilenameEdit.GetShortName: string;
begin
  Result := LongToShortFileName(FileName);
end;

{$ENDIF WIN32}

procedure TJvFilenameEdit.ClearFileList;
begin
  FDialog.Files.Clear;
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

function TJvFilenameEdit.GetDialogFiles: TStrings;
begin
  Result := FDialog.Files;
end;

function TJvFilenameEdit.GetDefaultExt: TFileExt;
begin
  Result := FDialog.DefaultExt;
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

function TJvFilenameEdit.GetInitialDir: string;
begin
  Result := FDialog.InitialDir;
end;

function TJvFilenameEdit.GetHistoryList: TStrings;
begin
  Result := FDialog.HistoryList;
end;

function TJvFilenameEdit.GetOptions: TOpenOptions;
begin
  Result := FDialog.Options;
end;

function TJvFilenameEdit.GetDialogTitle: string;
begin
  Result := FDialog.Title;
end;

procedure TJvFilenameEdit.SetDialogKind(Value: TFileDialogKind);
begin
  if FDialogKind <> Value then
  begin
    FDialogKind := Value;
    CreateEditDialog;
  end;
end;

procedure TJvFilenameEdit.SetDefaultExt(Value: TFileExt);
begin
  FDialog.DefaultExt := Value;
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

procedure TJvFilenameEdit.SetInitialDir(const Value: string);
begin
  FDialog.InitialDir := Value;
end;

procedure TJvFilenameEdit.SetHistoryList(Value: TStrings);
begin
  FDialog.HistoryList := Value;
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

procedure TJvFilenameEdit.SetDialogTitle(const Value: string);
begin
  FDialog.Title := Value;
end;

//=== TJvDirectoryEdit =======================================================

constructor TJvDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [];
end;

procedure TJvDirectoryEdit.ButtonClick;
var
  Temp: string;
  Action: Boolean;
begin
  inherited ButtonClick;
  Temp := Text;
  Action := True;
  DoBeforeDialog(Temp, Action);
  if not Action then
    Exit;
  if Temp = '' then
  begin
    if InitialDir <> '' then
      Temp := InitialDir
    else
      Temp := '\';
  end;
  if not DirExists(Temp) then
    Temp := '\';
  DisableSysErrors;
  try
    if NewStyleControls and (DialogKind = dkWin32) then
      Action := BrowseForFolder(FDialogText, True, Temp, Self.HelpContext)
      //BrowseDirectory(Temp, FDialogText, Self.HelpContext)
    else
      Action := SelectDirectory(Temp, FOptions, Self.HelpContext);
  finally
    EnableSysErrors;
  end;
  if CanFocus then
    SetFocus;
  DoAfterDialog(Temp, Action);
  if Action then
  begin
    SelText := '';
    if (Text = '') or not MultipleDirs then
      Text := Temp
    else
      Text := Text + ';' + Temp;
    if (Temp <> '') and DirExists(Temp) then
      InitialDir := Temp;
  end;
end;

procedure TJvDirectoryEdit.ReceptFileDir(const AFileName: string);
var
  Temp: string;
begin
  if FileExists(AFileName) then
    Temp := ExtractFilePath(AFileName)
  else
    Temp := AFileName;
  if (Text = '') or not MultipleDirs then
    Text := Temp
  else
    Text := Text + ';' + Temp;
end;

{$IFDEF WIN32}

function TJvDirectoryEdit.GetLongName: string;
var
  Temp: string;
  Pos: Integer;
begin
  if not MultipleDirs then
    Result := ShortToLongPath(Text)
  else
  begin
    Result := '';
    Pos := 1;
    while Pos <= Length(Text) do
    begin
      Temp := ShortToLongPath(ExtractSubstr(Text, Pos, [';']));
      if (Result <> '') and (Temp <> '') then
        Result := Result + ';';
      Result := Result + Temp;
    end;
  end;
end;

function TJvDirectoryEdit.GetShortName: string;
var
  Temp: string;
  Pos: Integer;
begin
  if not MultipleDirs then
    Result := LongToShortPath(Text)
  else
  begin
    Result := '';
    Pos := 1;
    while Pos <= Length(Text) do
    begin
      Temp := LongToShortPath(ExtractSubstr(Text, Pos, [';']));
      if (Result <> '') and (Temp <> '') then
        Result := Result + ';';
      Result := Result + Temp;
    end;
  end;
end;

{$ENDIF WIN32}

//=== TJvCustomDateEdit ======================================================

function NvlDate(DateValue, DefaultValue: TDateTime): TDateTime;
begin
  if DateValue = NullDate then
    Result := DefaultValue
  else
    Result := DateValue;
end;

constructor TJvCustomDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Polaris
  FDateAutoBetween := True;
  FMinDate := NullDate;
  FMaxDate := NullDate;

  FBlanksChar := ' ';
  FTitle := SDateDlgTitle;
  FPopupColor := clMenu;
  FDefNumGlyphs := 2;
  FStartOfWeek := Mon;
  FWeekends := [Sun];
  FWeekendColor := clRed;
  FYearDigits := dyDefault;
  FCalendarHints := TStringList.Create;
  TStringList(FCalendarHints).OnChange := CalendarHintsChanged;
  ControlState := ControlState + [csCreating];
  try
    UpdateFormat;
    {$IFDEF DEFAULT_POPUP_CALENDAR}
    FPopup := TJvPopupWindow(CreatePopupCalendar(Self,
      {$IFDEF COMPILER4_UP}BiDiMode, {$ENDIF}
      // Polaris
      FMinDate, FMaxDate));
    TJvPopupWindow(FPopup).OnCloseUp := PopupCloseUp;
    TJvPopupWindow(FPopup).Color := FPopupColor;
    {$ENDIF DEFAULT_POPUP_CALENDAR}
    GlyphKind := gkDefault; { force update }
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

destructor TJvCustomDateEdit.Destroy;
begin
  if FHooked then
  begin
    Application.UnhookMainWindow(FormatSettingsChange);
    FHooked := False;
  end;
  if FPopup <> nil then
    TJvPopupWindow(FPopup).OnCloseUp := nil;
  FPopup.Free;
  FPopup := nil;
  TStringList(FCalendarHints).OnChange := nil;
  FCalendarHints.Free;
  FCalendarHints := nil;
  inherited Destroy;
end;

procedure TJvCustomDateEdit.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  if Handle <> 0 then
  begin
    UpdateMask;
    if not (csDesigning in ComponentState) and not (IsLibrary or FHooked) then
    begin
      Application.HookMainWindow(FormatSettingsChange);
      FHooked := True;
    end;
  end;
end;

procedure TJvCustomDateEdit.DestroyWindowHandle;
begin
  if FHooked then
  begin
    Application.UnhookMainWindow(FormatSettingsChange);
    FHooked := False;
  end;
  inherited DestroyWindowHandle;
end;

procedure TJvCustomDateEdit.UpdateFormat;
begin
  FDateFormat := DefDateFormat(FourDigitYear);
end;

function TJvCustomDateEdit.GetDateFormat: string;
begin
  Result := FDateFormat;
end;

function TJvCustomDateEdit.TextStored: Boolean;
begin
  Result := not IsEmptyStr(Text, [#0, ' ', DateSeparator, FBlanksChar]);
end;

// Polaris

function TJvCustomDateEdit.StoreMinDate: Boolean;
begin
  Result := FMinDate <> NullDate;
end;

function TJvCustomDateEdit.StoreMaxDate: Boolean;
begin
  Result := FMaxDate <> NullDate;
end;
// Polaris

procedure TJvCustomDateEdit.CheckValidDate;
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
    if CanFocus then
      SetFocus;
    raise;
  end;
end;

procedure TJvCustomDateEdit.Change;
begin
  if not FFormatting then
    inherited Change;
end;

procedure TJvCustomDateEdit.CMExit(var Msg: TCMExit);
begin
  if not (csDesigning in ComponentState) and CheckOnExit then
    CheckValidDate;
  inherited;
end;

procedure TJvCustomDateEdit.WMContextMenu(var Message: TWMContextMenu);
begin
  if not PopupVisible then
    inherited;
end;

function TJvCustomDateEdit.GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap;
begin
  DestroyNeeded := False;
  if DateBitmap = nil then
  begin
    DateBitmap := TBitmap.Create;
    DateBitmap.Handle := LoadBitmap(HInstance, sDateBmp);
  end;
  Result := DateBitmap;
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

procedure TJvCustomDateEdit.UpdateMask;
var
  DateValue: TDateTime;
  OldFormat: string[10];
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

function TJvCustomDateEdit.FormatSettingsChange(var Msg: TMessage): boolean;
begin
  Result := false;
{$IFDEF WIN32}
  if (Msg.Msg = WM_WININICHANGE) and Application.UpdateFormatSettings then
{$ELSE}
  if Msg.Msg = WM_WININICHANGE then
{$ENDIF}
  begin
    GetFormatSettings;
    UpdateMask;
  end;
end;

function TJvCustomDateEdit.FourDigitYear: Boolean;
begin
  Result := (FYearDigits = dyFour) or ((FYearDigits = dyDefault) and
    (JvDateUtil.FourDigitYear));
end;

function TJvCustomDateEdit.GetDateMask: string;
begin
  Result := DefDateMask(FBlanksChar, FourDigitYear);
end;

// Polaris

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
        raise Exception.CreateFmt(SDateMinLimit, [DateToStr(FMaxDate)]);
    FMinDate := Value;
    UpdatePopup;
    if FDateAutoBetween then
      SetDate(Date);
  end;
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
        raise Exception.CreateFmt(SDateMaxLimit, [DateToStr(FMinDate)]);
    FMaxDate := Value;
    UpdatePopup;
    if FDateAutoBetween then
      SetDate(Date);
  end;
end;

function TJvCustomDateEdit.GetDate: TDateTime;
begin
  if DefaultToday then
    Result := SysUtils.Date
  else
    Result := NullDate;
  Result := StrToDateFmtDef(FDateFormat, Text, Result);
end;

{$IFDEF COMPILER4_UP}

procedure TJvCustomDateEdit.ValidateEdit;
begin
  if TextStored then
    CheckValidDate;
end;
{$ENDIF}
// Polaris

procedure TJvCustomDateEdit.SetDate(Value: TDateTime);
var
  D: TDateTime;
begin
  if not ValidDate(Value) or (Value = NullDate) then
  begin
    if DefaultToday then
      Value := SysUtils.Date
    else
      Value := NullDate;
  end;
  D := self.Date;
  TestDateBetween(Value); // Polaris
  if Value = NullDate then
    Text := ''
  else
    Text := FormatDateTime(FDateFormat, Value);
  Modified := D <> self.Date;
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

procedure TJvCustomDateEdit.ApplyDate(Value: TDateTime);
begin
  SetDate(Value);
  SelectAll;
end;

function TJvCustomDateEdit.GetDialogTitle: string;
begin
  Result := FTitle;
end;

procedure TJvCustomDateEdit.SetDialogTitle(const Value: string);
begin
  FTitle := Value;
end;

function TJvCustomDateEdit.IsCustomTitle: Boolean;
begin
  Result := (CompareStr(SDateDlgTitle, DialogTitle) <> 0) and
    (DialogTitle <> EmptyStr); // Polaris
end;

procedure TJvCustomDateEdit.UpdatePopup;
begin
  if FPopup <> nil then
    SetupPopupCalendar(FPopup, FStartOfWeek,
      FWeekends, FWeekendColor, FCalendarHints, FourDigitYear,
      FMinDate, FMaxDate); // Polaris
end;

procedure TJvCustomDateEdit.SetYearDigits(Value: TYearDigits);
begin
  if FYearDigits <> Value then
  begin
    FYearDigits := Value;
    UpdateMask;
  end;
end;

function TJvCustomDateEdit.GetPopupColor: TColor;
begin
  if FPopup <> nil then
    Result := TJvPopupWindow(FPopup).Color
  else
    Result := FPopupColor;
end;

procedure TJvCustomDateEdit.SetPopupColor(Value: TColor);
begin
  if Value <> PopupColor then
  begin
    if FPopup <> nil then
      TJvPopupWindow(FPopup).Color := Value;
    FPopupColor := Value;
  end;
end;

function TJvCustomDateEdit.GetCalendarStyle: TCalendarStyle;
begin
  if FPopup <> nil then
    Result := csPopup
  else
    Result := csDialog;
end;

procedure TJvCustomDateEdit.SetCalendarStyle(Value: TCalendarStyle);
begin
  if Value <> CalendarStyle then
  begin
    case Value of
      csPopup:
        begin
          if FPopup = nil then
            FPopup := TJvPopupWindow(CreatePopupCalendar(Self
              {$IFDEF COMPILER4_UP}, BiDiMode{$ENDIF},
              FMinDate, FMaxDate)); // Polaris
          TJvPopupWindow(FPopup).OnCloseUp := PopupCloseUp;
          TJvPopupWindow(FPopup).Color := FPopupColor;
          UpdatePopup;
        end;
      csDialog:
        begin
          FPopup.Free;
          FPopup := nil;
        end;
    end;
  end;
end;

procedure TJvCustomDateEdit.SetCalendarHints(Value: TStrings);
begin
  FCalendarHints.Assign(Value);
end;

procedure TJvCustomDateEdit.CalendarHintsChanged(Sender: TObject);
begin
  TStringList(FCalendarHints).OnChange := nil;
  try
    while FCalendarHints.Count > 4 do
      FCalendarHints.Delete(FCalendarHints.Count - 1);
  finally
    TStringList(FCalendarHints).OnChange := CalendarHintsChanged;
  end;
  if not (csDesigning in ComponentState) then
    UpdatePopup;
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

procedure TJvCustomDateEdit.SetStartOfWeek(Value: TDayOfWeekName);
begin
  if Value <> FStartOfWeek then
  begin
    FStartOfWeek := Value;
    UpdatePopup;
  end;
end;

procedure TJvCustomDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_PRIOR, VK_NEXT, VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN,
    VK_ADD, VK_SUBTRACT, VK_RETURN]) and PopupVisible then
  begin
    TJvPopupWindow(FPopup).KeyDown(Key, Shift);
    Key := 0;
  end
  else if (Shift = []) and DirectInput then
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
  inherited KeyDown(Key, Shift);
end;

procedure TJvCustomDateEdit.KeyPress(var Key: Char);
begin
  if (Key in ['T', 't', '+', '-']) and PopupVisible then
  begin
    TJvPopupWindow(FPopup).KeyPress(Key);
    Key := #0;
  end
  else if DirectInput then
    case Key of
      'T', 't':
        begin
          ApplyDate(Trunc(Now));
          Key := #0;
        end;
      '+', '-':
        Key := #0;
    end;
  inherited KeyPress(Key);
end;

procedure TJvCustomDateEdit.ButtonClick;
var
  D: TDateTime;
  Action: Boolean;
begin
  inherited ButtonClick;
  if CalendarStyle = csDialog then
  begin
    D := Self.Date;
    Action := SelectDate(Self, D, DialogTitle, FStartOfWeek, FWeekends, // Polaris (Self added)
      FWeekendColor, FCalendarHints,
      FMinDate, FMaxDate); // Polaris
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
  end;
end;

{$IFDEF WIN32}

function TJvCustomDateEdit.AcceptPopup(var Value: Variant): Boolean;
{$ELSE}

function TJvCustomDateEdit.AcceptPopup(var Value: string): Boolean;
{$ENDIF}
var
  D: TDateTime;
begin
  Result := True;
  if Assigned(FOnAcceptDate) then
  begin
    {$IFDEF WIN32}
    if VarIsNull(Value) or VarIsEmpty(Value) then
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
    {$ELSE}
    if DefaultToday then
      D := SysUtils.Date
    else
      D := NullDate;
    D := StrToDateDef(Value, D);
    {$ENDIF}
    FOnAcceptDate(Self, D, Result);
    {$IFDEF WIN32}
    if Result then
      Value := VarFromDateTime(D);
    {$ELSE}
    if Result then
      Value := FormatDateTime(FDateFormat, D);
    {$ENDIF}
  end;
end;

{$IFDEF WIN32}

procedure TJvCustomDateEdit.SetPopupValue(const Value: Variant);
begin
  inherited SetPopupValue(StrToDateFmtDef(FDateFormat, VarToStr(Value),
    SysUtils.Date));
end;

procedure TJvCustomDateEdit.AcceptValue(const Value: Variant);
begin
  SetDate(VarToDateTime(Value));
  UpdatePopupVisible;
  if Modified then
    inherited Change;
end;

{$ENDIF}

//=== TJvCustomDateEdit ======================================================

// (rom) unusual not to have it implemented in the Cusotm base class

constructor TJvDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UpdateMask;
end;

// Polaris

procedure TJvDateEdit.SetDate(Value: TDateTime);
begin
  if not FDateAutoBetween then
    if Value <> NullDate then
    begin
      if ((FMinDate <> NullDate) and (FMaxDate <> NullDate) and
        ((Value < FMinDate) or (Value > FMaxDate))) then
        raise Exception.CreateFmt(SDateOutOfRange, [FormatDateTime(FDateFormat, Value),
          FormatDateTime(FDateFormat, FMinDate), FormatDateTime(FDateFormat, FMaxDate)])
      else if (FMinDate <> NullDate) and (Value < FMinDate) then
        raise Exception.CreateFmt(SDateOutOfMin, [FormatDateTime(FDateFormat, Value),
          FormatDateTime(FDateFormat, FMinDate)])
      else if (FMaxDate <> NullDate) and (Value > FMaxDate) then
        raise Exception.CreateFmt(SDateOutOfMax, [FormatDateTime(FDateFormat, Value),
          FormatDateTime(FDateFormat, FMaxDate)]);
    end;
  inherited SetDate(Value);
end;
// Polaris

{ Utility routines }

procedure DateFormatChanged;
var
  I: Integer;

  procedure IterateControls(AControl: TWinControl);
  var
    I: Integer;
  begin
    with AControl do
      for I := 0 to ControlCount - 1 do
      begin
        if Controls[I] is TJvCustomDateEdit then
          TJvCustomDateEdit(Controls[I]).UpdateMask
        else if Controls[I] is TWinControl then
          IterateControls(TWinControl(Controls[I]));
      end;
  end;

begin
  if Screen <> nil then
    for I := 0 to Screen.FormCount - 1 do
      IterateControls(Screen.Forms[I]);
end;

procedure DestroyLocals; far;
begin
  FileBitmap.Free;
  FileBitmap := nil;
  DateBitmap.Free;
  DateBitmap := nil;
end;

{$IFDEF WIN32}
initialization

finalization
  DestroyLocals;

  {$ELSE}
initialization
  AddExitProc(DestroyLocals);
  {$ENDIF}

end.

