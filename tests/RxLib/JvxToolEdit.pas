{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxToolEdit.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvxToolEdit;

interface


uses Windows, RTLConsts, Variants, Classes,
  StdCtrls, Controls, Messages, SysUtils, Forms, Graphics, Menus, Buttons,
  Dialogs, JvxCtrls, FileCtrl, Mask, JvxDateUtil;

const
  scAltDown = scAlt + vk_Down;
  DefEditBtnWidth = 21;

type
{$IFDEF WIN32}
  TFileExt = type string;
{$ENDIF}

{ TJvxPopupWindow }

  TCloseUpEvent = procedure (Sender: TObject; Accept: Boolean) of object;
  TPopupAlign = (epaRight, epaLeft);

  TJvxPopupWindow = class(TCustomControl)
  private
    FEditor: TWinControl;
    FCloseUp: TCloseUpEvent;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
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
    procedure Show(Origin: TPoint);
    property OnCloseUp: TCloseUpEvent read FCloseUp write FCloseUp;
  end;

{ TJvxCustomComboEdit }

  TJvxEditButton = class(TJvxSpeedButton)
  private
    FNoAction: Boolean;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
{$IFDEF WIN32}
    procedure Paint; override;
{$ENDIF WIN32}
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TGlyphKind = (gkCustom, gkDefault, gkDropDown, gkEllipsis);

  TJvxCustomComboEdit = class(TCustomMaskEdit)
  private
    FButton: TJvxEditButton;
    FBtnControl: TWinControl;
    FOnButtonClick: TNotifyEvent;
    FClickKey: TShortCut;
    FReadOnly: Boolean;
    FDirectInput: Boolean;
    FAlwaysEnable: Boolean;
    FAlignment: TAlignment;
    FPopupVisible: Boolean;
    FFocused: Boolean;
    FPopupAlign: TPopupAlign;
    FGlyphKind: TGlyphKind;
    procedure SetEditRect;
    procedure RecreateGlyph;
    procedure UpdateBtnBounds;
    procedure EditButtonClick(Sender: TObject);
    function GetMinHeight: Integer;
    function GetTextHeight: Integer;
    procedure SetShowCaret;
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetPopupVisible: Boolean;
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    function GetButtonWidth: Integer;
    procedure SetButtonWidth(Value: Integer);
    function GetButtonHint: string;
    procedure SetButtonHint(const Value: string);
    function GetDirectInput: Boolean;
    procedure SetDirectInput(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
    function IsCustomGlyph: Boolean;
    function BtnWidthStored: Boolean;
    procedure SetGlyphKind(Value: TGlyphKind);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMEnter(var Message: TMessage); message CM_ENTER;
    procedure CNCtlColor(var Message: TMessage); message
      {$IFDEF WIN32} CN_CTLCOLOREDIT {$ELSE} CN_CTLCOLOR {$ENDIF};
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
{$IFDEF WIN32}
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
{$ENDIF}
{$IFDEF Delphi4_Up}
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
{$ENDIF}
  protected
    FPopup: TCustomControl;
    FDefNumGlyphs: TNumGlyphs;
    function GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap; virtual;
    procedure PopupDropDown(DisableEdit: Boolean); virtual;
    procedure PopupCloseUp(Sender: TObject; Accept: Boolean);
    procedure ShowPopup(Origin: TPoint); virtual;
    procedure HidePopup; virtual;
    procedure UpdatePopupVisible;
    procedure DoChange;
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
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AlwaysEnable: Boolean read FAlwaysEnable write FAlwaysEnable default False;
    property Button: TJvxEditButton read FButton;
    property ClickKey: TShortCut read FClickKey write FClickKey
      default scAltDown;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsCustomGlyph;
    property GlyphKind: TGlyphKind read FGlyphKind write SetGlyphKind default gkCustom;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth
      stored BtnWidthStored;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property ButtonHint: string read GetButtonHint write SetButtonHint;
    property DirectInput: Boolean read GetDirectInput write SetDirectInput default True;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property PopupAlign: TPopupAlign read FPopupAlign write FPopupAlign default epaRight;
    property PopupVisible: Boolean read GetPopupVisible;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoClick;
    procedure SelectAll;
  end;

{ TJvxComboEdit }

  TJvxComboEdit = class(TJvxCustomComboEdit)
  public
    property Button;
  published
    property Alignment;
    property AutoSelect;
    property BorderStyle;
    property ButtonHint;
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
{$IFDEF Delphi4_Up}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFDEF WIN32}
  {$IFNDEF VER90}
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
{$IFDEF Delphi5_Up}
    property OnContextPopup;
{$ENDIF}
{$IFDEF Delphi4_Up}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

{ TJvxFileDirEdit }
{ The common parent of TJvxFilenameEdit and TJvxDirectoryEdit          }
{ For internal use only; it's not intended to be used separately }

{$IFNDEF WIN32}
const
  MaxFileLength = SizeOf(TFileName) - 1;
{$ENDIF}

type
  TExecOpenDialogEvent = procedure(Sender: TObject; var Name: string;
    var Action: Boolean) of object;

  TJvxFileDirEdit = class(TJvxCustomComboEdit)
  private
    FErrMode: Cardinal;
    FAcceptFiles: Boolean;
    FOnDropFiles: TNotifyEvent;
    FOnBeforeDialog: TExecOpenDialogEvent;
    FOnAfterDialog: TExecOpenDialogEvent;
    procedure SetDragAccept(Value: Boolean);
    procedure SetAcceptFiles(Value: Boolean);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  protected
    FMultipleDirs: Boolean;
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
    property MaxLength {$IFNDEF WIN32} default MaxFileLength {$ENDIF};
  public
    constructor Create(AOwner: TComponent); override;
{$IFDEF WIN32}
    property LongName: string read GetLongName;
    property ShortName: string read GetShortName;
{$ENDIF}
  published
    property AcceptFiles: Boolean read FAcceptFiles write SetAcceptFiles default False;
    property OnBeforeDialog: TExecOpenDialogEvent read FOnBeforeDialog
      write FOnBeforeDialog;
    property OnAfterDialog: TExecOpenDialogEvent read FOnAfterDialog
      write FOnAfterDialog;
    property OnDropFiles: TNotifyEvent read FOnDropFiles write FOnDropFiles;
    property OnButtonClick;
  end;

{ TJvxFilenameEdit }

  TFileDialogKind = (dkOpen, dkSave {$IFDEF Delphi3_Up}, dkOpenPicture,
    dkSavePicture {$ENDIF});

  TJvxFilenameEdit = class(TJvxFileDirEdit)
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
{$IFDEF Delphi4_Up}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFDEF WIN32}
  {$IFNDEF VER90}
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
{$IFDEF Delphi5_Up}
    property OnContextPopup;
{$ENDIF}
{$IFDEF Delphi4_Up}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

{ TJvxDirectoryEdit }

{$IFDEF WIN32}
  TDirDialogKind = (dkVCL, dkWin32);
{$ENDIF}

  TJvxDirectoryEdit = class(TJvxFileDirEdit)
  private
    FOptions: TSelectDirOpts;
    FInitialDir: string;
{$IFDEF WIN32}
    FDialogText: string;
    FDialogKind: TDirDialogKind;
{$ENDIF}
  protected
    procedure ButtonClick; override;
    procedure ReceptFileDir(const AFileName: string); override;
{$IFDEF WIN32}
    function GetLongName: string; override;
    function GetShortName: string; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  published
{$IFDEF WIN32}
    property DialogKind: TDirDialogKind read FDialogKind write FDialogKind
      default dkVCL;
    property DialogText: string read FDialogText write FDialogText;
{$ENDIF}
    property DialogOptions: TSelectDirOpts read FOptions write FOptions default [];
    property InitialDir: string read FInitialDir write FInitialDir;
    property MultipleDirs: Boolean read FMultipleDirs write FMultipleDirs default False;
    property AutoSelect;
    property ButtonHint;
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
{$IFDEF Delphi4_Up}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFDEF WIN32}
  {$IFNDEF VER90}
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
{$IFDEF Delphi5_Up}
    property OnContextPopup;
{$ENDIF}
{$IFDEF Delphi4_Up}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

{ TJvxCustomDateEdit }

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

  TJvxCustomDateEdit = class(TJvxCustomComboEdit)
  private
    FTitle: String;
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
    function GetDate: TDateTime;
    procedure SetDate(Value: TDateTime);
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
    function FourDigitYear: Boolean;
    function FormatSettingsChange(var Message: TMessage): Boolean;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
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
      default clBtnFace;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CheckValidDate;
    function GetDateMask: string;
    procedure UpdateMask; virtual;
    property Date: TDateTime read GetDate write SetDate;
    property PopupVisible;
  end;

{ TJvxDateEdit }

  TJvxDateEdit = class(TJvxCustomDateEdit)
  public
    constructor Create(AOwner: TComponent); override;
    property EditMask;
  published
    property AutoSelect;
    property BlanksChar;
    property BorderStyle;
    property ButtonHint;
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
{$IFDEF Delphi4_Up}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFDEF WIN32}
  {$IFNDEF VER90}
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
{$IFDEF Delphi5_Up}
    property OnContextPopup;
{$ENDIF}
{$IFDEF Delphi4_Up}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

  EComboEditError = class(Exception);

{ Utility routines }

procedure DateFormatChanged;

function EditorTextMargins(Editor: TJvxCustomComboEdit): TPoint;
function PaintComboEdit(Editor: TJvxCustomComboEdit; const AText: string;
  AAlignment: TAlignment; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Message: TWMPaint): Boolean;

implementation

uses ShellAPI, Consts, {$IFDEF Delphi3_Up} ExtDlgs, {$ENDIF} JvxCConst, JvxVCLUtils,
  JvxStrUtils, JvxFileUtil, JvxPickDate, JvxMaxMin;

{$IFDEF WIN32}
 {$R *.Res}
{$ELSE}
 {$R *.R16}
{$ENDIF}

const
  sFileBmp = 'FEDITBMP'; { Filename and directory editor button glyph }
  sDateBmp = 'DEDITBMP'; { Date editor button glyph }

{ Utility routines }

function EditorTextMargins(Editor: TJvxCustomComboEdit): TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  with Editor do begin
{$IFDEF WIN32}
    if NewStyleControls then begin
      if BorderStyle = bsNone then I := 0
      else if Ctl3D then I := 1
      else I := 2;
      Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
      Result.Y := I;
    end
    else begin
{$ENDIF}
      if BorderStyle = bsNone then I := 0
      else begin
        DC := GetDC(0);
        GetTextMetrics(DC, SysMetrics);
        SaveFont := SelectObject(DC, Font.Handle);
        GetTextMetrics(DC, Metrics);
        SelectObject(DC, SaveFont);
        ReleaseDC(0, DC);
        I := SysMetrics.tmHeight;
        if I > Metrics.tmHeight then I := Metrics.tmHeight;
        I := I div 4;
      end;
      Result.X := I;
      Result.Y := I;
{$IFDEF WIN32}
    end;
{$ENDIF}
  end;
end;

function PaintComboEdit(Editor: TJvxCustomComboEdit; const AText: string;
  AAlignment: TAlignment; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Message: TWMPaint): Boolean;
var
  AWidth, ALeft: Integer;
  Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
{$IFDEF Delphi4_Up}
  ExStyle: DWORD;
const
  AlignStyle: array[Boolean, TAlignment] of DWORD =
   ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
{$ENDIF}
begin
  Result := True;
  with Editor do begin
{$IFDEF Delphi4_Up}
    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
{$ENDIF}
    if StandardPaint {$IFDEF WIN32} and not
      (csPaintCopy in ControlState) {$ENDIF} then
    begin
{$IFDEF Delphi4_Up}
      if SysLocale.MiddleEast and HandleAllocated and (IsRightToLeft) then
      begin { This keeps the right aligned text, right aligned }
        ExStyle := DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) and (not WS_EX_RIGHT) and
          (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
        if UseRightToLeftReading then
          ExStyle := ExStyle or WS_EX_RTLREADING;
        if UseRightToLeftScrollbar then
          ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
        ExStyle := ExStyle or
          AlignStyle[UseRightToLeftAlignment, AAlignment];
        if DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) <> ExStyle then
          SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
      end;
{$ENDIF Delphi4_Up}
      Result := False;
      { return false if we need to use standard paint handler }
      Exit;
    end;
    { Since edit controls do not handle justification unless multi-line (and
      then only poorly) we will draw right and center justify manually unless
      the edit has the focus. }
    if ACanvas = nil then begin
      ACanvas := TControlCanvas.Create;
      ACanvas.Control := Editor;
    end;
    DC := Message.DC;
    if DC = 0 then DC := BeginPaint(Handle, PS);
    ACanvas.Handle := DC;
    try
      ACanvas.Font := Font;
      if not Enabled and NewStyleControls and not
        (csDesigning in ComponentState) and
        (ColorToRGB(Color) <> ColorToRGB(clGrayText)) then
        ACanvas.Font.Color := clGrayText;
      with ACanvas do begin
        R := ClientRect;
        if {$IFDEF WIN32} not (NewStyleControls and Ctl3D) and {$ENDIF}
          (BorderStyle = bsSingle) then
        begin
          Brush.Color := clWindowFrame;
          FrameRect(R);
          InflateRect(R, -1, -1);
        end;
        Brush.Color := Color;
        S := AText;
        AWidth := TextWidth(S);
        Margins := EditorTextMargins(Editor);
        if PopupVisible then ALeft := Margins.X
        else begin
          if ButtonWidth > 0 then Inc(AWidth);
          case AAlignment of
            taLeftJustify:
              ALeft := Margins.X;
            taRightJustify:
              ALeft := ClientWidth - ButtonWidth - AWidth - Margins.X - 2;
            else
              ALeft := (ClientWidth - ButtonWidth - AWidth) div 2;
          end;
        end;
{$IFDEF Delphi4_Up}
        if SysLocale.MiddleEast then UpdateTextFlags;
{$ENDIF}
        TextRect(R, ALeft, Margins.Y, S);
      end;
    finally
      ACanvas.Handle := 0;
      if Message.DC = 0 then EndPaint(Handle, PS);
    end;
  end;
end;

{ TJvxEditButton }

constructor TJvxEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF WIN32}
  ControlStyle := ControlStyle + [csReplicatable];
{$ELSE}
  Style := bsWin31;
{$ENDIF}
  ParentShowHint := True;
end;

{$IFDEF WIN32}

procedure TJvxEditButton.Paint;
begin
  inherited Paint;
  if (FState <> rbsDown) then
    with Canvas do begin
      if NewStyleControls then Pen.Color := clBtnFace
      else Pen.Color := clBtnShadow;
      MoveTo(0, 0);
      LineTo(0, Self.Height - 1);
      Pen.Color := clBtnHighlight;
      MoveTo(1, 1);
      LineTo(1, Self.Height - 2);
    end;
end;

{$ENDIF WIN32}

procedure TJvxEditButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) then
    with TJvxCustomComboEdit(Owner) do begin
      FNoAction := (FPopup <> nil) and FPopupVisible;
      if not FPopupVisible then begin
        if TabStop and CanFocus and (GetFocus <> Handle) then SetFocus;
      end
      else PopupCloseUp(FPopup, True);
    end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvxEditButton.Click;
begin
  if not FNoAction then inherited Click else FNoAction := False;
end;

{ TJvxPopupWindow }

constructor TJvxPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditor := TWinControl(AOwner);
{$IFDEF WIN32}
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable,
    csAcceptsControls];
{$ELSE}
  ControlStyle := ControlStyle + [csAcceptsControls];
{$ENDIF}
  Ctl3D := False;
  ParentCtl3D := False;
  Visible := False;
  Parent := FEditor;
  OnMouseUp := PopupMouseUp;
end;

procedure TJvxPopupWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := WS_POPUP or WS_BORDER or WS_CLIPCHILDREN;
{$IFDEF WIN32}
    ExStyle := WS_EX_TOOLWINDOW;
{$ENDIF}
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

{$IFNDEF WIN32}
procedure TJvxPopupWindow.CreateWnd;
begin
  inherited CreateWnd;
  if (csDesigning in ComponentState) then SetParent(nil);
end;
{$ENDIF}

procedure TJvxPopupWindow.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

function TJvxPopupWindow.GetPopupText: string;
begin
  Result := '';
end;

procedure TJvxPopupWindow.InvalidateEditor;
var
  R: TRect;
begin
  if (FEditor is TJvxCustomComboEdit) then begin
    with TJvxCustomComboEdit(FEditor) do
      SetRect(R, 0, 0, ClientWidth - FBtnControl.Width - 2, ClientHeight + 1);
  end
  else R := FEditor.ClientRect;
  InvalidateRect(FEditor.Handle, @R, False);
  UpdateWindow(FEditor.Handle);
end;

procedure TJvxPopupWindow.PopupMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then CloseUp(PtInRect(Self.ClientRect, Point(X, Y)));
end;

procedure TJvxPopupWindow.CloseUp(Accept: Boolean);
begin
  if Assigned(FCloseUp) then FCloseUp(Self, Accept);
end;

procedure TJvxPopupWindow.Hide;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TJvxPopupWindow.Show(Origin: TPoint);
begin
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
end;

{ TJvxCustomComboEdit }

constructor TJvxCustomComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF Delphi3_Up}
  ControlStyle := ControlStyle + [csCaptureMouse];
{$ENDIF}
  AutoSize := False;
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
  FButton := TJvxEditButton.Create(Self);
  FButton.SetBounds(0, 0, FBtnControl.Width, FBtnControl.Height);
  FButton.Visible := True;
  FButton.Parent := FBtnControl;
  TJvxEditButton(FButton).OnClick := EditButtonClick;
  Height := 21;
  FDefNumGlyphs := 1;
  FGlyphKind := gkCustom;
end;

destructor TJvxCustomComboEdit.Destroy;
begin
  FButton.OnClick := nil;
  inherited Destroy;
end;

procedure TJvxCustomComboEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Longword = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN
    or Alignments[FAlignment];
end;

procedure TJvxCustomComboEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TJvxCustomComboEdit.HidePopup;
begin
  TJvxPopupWindow(FPopup).Hide;
end;

procedure TJvxCustomComboEdit.ShowPopup(Origin: TPoint);
begin
  TJvxPopupWindow(FPopup).Show(Origin);
end;

procedure TJvxCustomComboEdit.PopupDropDown(DisableEdit: Boolean);
var
  P: TPoint;
  Y: Integer;
begin
  if (FPopup <> nil) and not (ReadOnly or FPopupVisible) then begin
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FPopup.Height > Screen.Height then
      Y := P.Y - FPopup.Height;
    case FPopupAlign of
      epaRight:
        begin
          Dec(P.X, FPopup.Width - Width);
          if P.X < 0 then Inc(P.X, FPopup.Width - Width);
        end;
      epaLeft:
        begin
          if P.X + FPopup.Width > Screen.Width then
            Dec(P.X, FPopup.Width - Width);
        end;
    end;
    if P.X < 0 then P.X := 0
    else if P.X + FPopup.Width > Screen.Width then
      P.X := Screen.Width - FPopup.Width;
{$IFDEF WIN32}
    if Text <> '' then SetPopupValue(Text)
    else SetPopupValue(Null);
{$ELSE}
    SetPopupValue(Text);
{$ENDIF}
    if CanFocus then SetFocus;
    ShowPopup(Point(P.X, Y));
    FPopupVisible := True;
    if DisableEdit then begin
      inherited ReadOnly := True;
      HideCaret(Handle);
    end;
  end;
end;

procedure TJvxCustomComboEdit.PopupCloseUp(Sender: TObject; Accept: Boolean);
var
{$IFDEF WIN32}
  AValue: Variant;
{$ELSE}
  AValue: string;
{$ENDIF}
begin
  if (FPopup <> nil) and FPopupVisible then begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    AValue := GetPopupValue;
    HidePopup;
    try
      try
        if CanFocus then begin
          SetFocus;
          if GetFocus = Handle then SetShowCaret;
        end;
      except
        { ignore exceptions }
      end;
      SetDirectInput(DirectInput);
      Invalidate;
      if Accept and AcceptPopup(AValue) and EditCanModify then begin
        AcceptValue(AValue);
        if FFocused then inherited SelectAll;
      end;
    finally
      FPopupVisible := False;
    end;
  end;
end;

procedure TJvxCustomComboEdit.DoChange;
begin
  inherited Change;
end;

{$IFDEF WIN32}
function TJvxCustomComboEdit.GetPopupValue: Variant;
{$ELSE}
function TJvxCustomComboEdit.GetPopupValue: string;
{$ENDIF}
begin
  if FPopup <> nil then Result := TJvxPopupWindow(FPopup).GetValue
  else Result := '';
end;

{$IFDEF WIN32}
procedure TJvxCustomComboEdit.SetPopupValue(const Value: Variant);
{$ELSE}
procedure TJvxCustomComboEdit.SetPopupValue(const Value: string);
{$ENDIF}
begin
  if FPopup <> nil then TJvxPopupWindow(FPopup).SetValue(Value);
end;

{$IFDEF WIN32}
procedure TJvxCustomComboEdit.AcceptValue(const Value: Variant);
begin
  if Text <> VarToStr(Value) then begin
{$ELSE}
procedure TJvxCustomComboEdit.AcceptValue(const Value: string);
begin
  if Text <> Value then begin
{$ENDIF}
    Text := Value;
    Modified := True;
    UpdatePopupVisible;
    DoChange;
  end;
end;

{$IFDEF WIN32}
function TJvxCustomComboEdit.AcceptPopup(var Value: Variant): Boolean;
{$ELSE}
function TJvxCustomComboEdit.AcceptPopup(var Value: string): Boolean;
{$ENDIF}
begin
  Result := True;
end;

function TJvxCustomComboEdit.EditCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

procedure TJvxCustomComboEdit.Change;
begin
  if not PopupVisible then DoChange
  else PopupChange;
end;

procedure TJvxCustomComboEdit.PopupChange;
begin
end;

procedure TJvxCustomComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (FClickKey = ShortCut(Key, Shift)) and (ButtonWidth > 0) then begin
    EditButtonClick(Self);
    Key := 0;
  end;
end;

procedure TJvxCustomComboEdit.KeyPress(var Key: Char);
begin
  if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) then
  begin
    if PopupVisible then begin
      PopupCloseUp(FPopup, Key = Char(VK_RETURN));
      Key := #0;
    end
    else begin
      { must catch and remove this, since is actually multi-line }
      GetParentForm(Self).Perform(CM_DIALOGKEY, Byte(Key), 0);
      if Key = Char(VK_RETURN) then begin
        inherited KeyPress(Key);
        Key := #0;
        Exit;
      end;
    end;
  end;
  inherited KeyPress(Key);
end;

procedure TJvxCustomComboEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (FPopup <> nil) and (Button = mbLeft) then begin
    if CanFocus then SetFocus;
    if not FFocused then Exit;
    if FPopupVisible then PopupCloseUp(FPopup, False);
    {else if (not ReadOnly or AlwaysEnable) and (not DirectInput) then
      PopupDropDown(True);}
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

function TJvxCustomComboEdit.GetButtonWidth: Integer;
begin
  Result := FButton.Width;
end;

procedure TJvxCustomComboEdit.SetButtonWidth(Value: Integer);
begin
  if ButtonWidth <> Value then begin
    FBtnControl.Visible := Value > 1;
    if (csCreating in ControlState) then begin
      FBtnControl.Width := Value;
      FButton.Width := Value;
      with FButton do
        ControlStyle := ControlStyle - [csFixedWidth];
      RecreateGlyph;
    end
    else if (Value <> ButtonWidth) and (Value < ClientWidth) then begin
      FButton.Width := Value;
      with FButton do
        ControlStyle := ControlStyle - [csFixedWidth];
      if HandleAllocated then RecreateWnd;
      RecreateGlyph;
    end;
  end;
end;

function TJvxCustomComboEdit.GetButtonHint: string;
begin
  Result := FButton.Hint;
end;

procedure TJvxCustomComboEdit.SetButtonHint(const Value: string);
begin
  FButton.Hint := Value;
end;

function TJvxCustomComboEdit.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TJvxCustomComboEdit.SetGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
  FGlyphKind := gkCustom;
end;

function TJvxCustomComboEdit.GetNumGlyphs: TNumGlyphs;
begin
  Result := FButton.NumGlyphs;
end;

procedure TJvxCustomComboEdit.SetNumGlyphs(Value: TNumGlyphs);
begin
  if FGlyphKind in [gkDropDown, gkEllipsis] then FButton.NumGlyphs := 1
  else if FGlyphKind = gkDefault then FButton.NumGlyphs := FDefNumGlyphs
  else FButton.NumGlyphs := Value;
end;

procedure TJvxCustomComboEdit.SetEditRect;
var
  Loc: TRect;
begin
  SetRect(Loc, 0, 0, ClientWidth - FBtnControl.Width - 2, ClientHeight + 1);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
end;

procedure TJvxCustomComboEdit.UpdateBtnBounds;
var
  BtnRect: TRect;
begin
{$IFDEF WIN32}
  if NewStyleControls then begin
    if Ctl3D and (BorderStyle = bsSingle) then
      BtnRect := Bounds(Width - FButton.Width - 4, 0,
        FButton.Width, Height - 4)
    else begin
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
{$ENDIF}
  with BtnRect do
    FBtnControl.SetBounds(Left, Top, Right - Left, Bottom - Top);
  FButton.Height := FBtnControl.Height;
  SetEditRect;
end;

{$IFDEF WIN32}
procedure TJvxCustomComboEdit.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  UpdateBtnBounds;
end;
{$ENDIF}

procedure TJvxCustomComboEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  if not (csLoading in ComponentState) then begin
    MinHeight := GetMinHeight;
    { text edit bug: if size to less than MinHeight, then edit ctrl does
      not display the text }
    if Height < MinHeight then begin
      Height := MinHeight;
      Exit;
    end;
  end
  else begin
    if (FPopup <> nil) and (csDesigning in ComponentState) then
      FPopup.SetBounds(0, Height + 1, 10, 10);
  end;
  UpdateBtnBounds;
end;

function TJvxCustomComboEdit.GetTextHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
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
  Result := Min(SysMetrics.tmHeight, Metrics.tmHeight);
end;

function TJvxCustomComboEdit.GetMinHeight: Integer;
var
  I: Integer;
begin
  I := GetTextHeight;
  Result := I + GetSystemMetrics(SM_CYBORDER) * 4 +
    1 {$IFNDEF WIN32} + (I div 4) {$ENDIF};
end;

procedure TJvxCustomComboEdit.UpdatePopupVisible;
begin
  FPopupVisible := (FPopup <> nil) and FPopup.Visible;
end;

function TJvxCustomComboEdit.GetPopupVisible: Boolean;
begin
  Result := (FPopup <> nil) and FPopupVisible;
end;

procedure TJvxCustomComboEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then SetEditRect;
end;

procedure TJvxCustomComboEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FButton.Enabled := Enabled;
end;

procedure TJvxCustomComboEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FPopup) and
    (Message.Sender <> FButton) and ((FPopup <> nil) and
    not FPopup.ContainsControl(Message.Sender)) then
    PopupCloseUp(FPopup, False);
end;

procedure TJvxCustomComboEdit.CMEnter(var Message: TMessage);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then SelectAll;
  inherited;
end;

procedure TJvxCustomComboEdit.CNCtlColor(var Message: TMessage);
var
  TextColor: Longint;
begin
  inherited;
  if NewStyleControls then begin
    TextColor := ColorToRGB(Font.Color);
    if not Enabled and (ColorToRGB(Color) <> ColorToRGB(clGrayText)) then
      TextColor := ColorToRGB(clGrayText);
    SetTextColor(Message.WParam, TextColor);
  end;
end;

procedure TJvxCustomComboEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FFocused := False;
  PopupCloseUp(FPopup, False);
end;

procedure TJvxCustomComboEdit.WMSetFocus(var Message: TMessage);
begin
  inherited;
  FFocused := True;
  SetShowCaret;
end;

{$IFDEF Delphi4_Up}
procedure TJvxCustomComboEdit.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FPopup <> nil then FPopup.BiDiMode := BiDiMode;
end;
{$ENDIF}

procedure TJvxCustomComboEdit.SetShowCaret;
const
  CaretWidth: array[Boolean] of Byte = (1, 2);
begin
  CreateCaret(Handle, 0, CaretWidth[fsBold in Font.Style], GetTextHeight);
  ShowCaret(Handle);
end;

procedure TJvxCustomComboEdit.EditButtonClick(Sender: TObject);
begin
  if (not FReadOnly) or AlwaysEnable then ButtonClick;
end;

procedure TJvxCustomComboEdit.DoClick;
begin
  EditButtonClick(Self);
end;

procedure TJvxCustomComboEdit.ButtonClick;
begin
  if Assigned(FOnButtonClick) then FOnButtonClick(Self);
  if FPopup <> nil then begin
    if FPopupVisible then PopupCloseUp(FPopup, True) else PopupDropDown(True);
  end;
end;

procedure TJvxCustomComboEdit.SelectAll;
begin
  if DirectInput then inherited SelectAll;
end;

function TJvxCustomComboEdit.GetDirectInput: Boolean;
begin
  Result := FDirectInput;
end;

procedure TJvxCustomComboEdit.SetDirectInput(Value: Boolean);
begin
  inherited ReadOnly := not Value or FReadOnly;
  FDirectInput := Value;
end;

procedure TJvxCustomComboEdit.WMPaste(var Message: TWMPaste);
begin
  if not FDirectInput or ReadOnly then Exit;
  inherited;
end;

procedure TJvxCustomComboEdit.WMCut(var Message: TWMCut);
begin
  if not FDirectInput or ReadOnly then Exit;
  inherited;
end;

function TJvxCustomComboEdit.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TJvxCustomComboEdit.SetReadOnly(Value: Boolean);
begin
  if Value <> FReadOnly then begin
    FReadOnly := Value;
    inherited ReadOnly := Value or not FDirectInput;
  end;
end;

procedure TJvxCustomComboEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

function TJvxCustomComboEdit.BtnWidthStored: Boolean;
begin
  if (FGlyphKind = gkDefault) and (Glyph <> nil) then
    Result := ButtonWidth <> Max(Glyph.Width div FButton.NumGlyphs + 6,
      DefEditBtnWidth)
  else if FGlyphKind = gkDropDown then
    Result := ButtonWidth <> GetSystemMetrics(SM_CXVSCROLL)
      {$IFNDEF WIN32} + 1{$ENDIF}
  else Result := ButtonWidth <> DefEditBtnWidth;
end;

function TJvxCustomComboEdit.IsCustomGlyph: Boolean;
begin
  Result := FGlyphKind = gkCustom;
end;

procedure TJvxCustomComboEdit.SetGlyphKind(Value: TGlyphKind);
begin
  if FGlyphKind <> Value then begin
    FGlyphKind := Value;
    if (FGlyphKind = gkCustom) and (csReading in ComponentState) then begin
      Glyph := nil;
    end;
    RecreateGlyph;
    if (FGlyphKind = gkDefault) and (Glyph <> nil) then
      ButtonWidth := Max(Glyph.Width div FButton.NumGlyphs + 6, FButton.Width)
    else if FGlyphKind = gkDropDown then begin
      ButtonWidth := GetSystemMetrics(SM_CXVSCROLL){$IFNDEF WIN32} + 1{$ENDIF};
      with FButton do
        ControlStyle := ControlStyle + [csFixedWidth];
    end;
  end;
end;

function TJvxCustomComboEdit.GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap;
begin
  Result := nil;
end;

procedure TJvxCustomComboEdit.RecreateGlyph;

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
      if G <= 0 then G := 1;
      if G > 3 then G := 3;
      I := (Width - 3 * W - 2 * G) div 2;
      PatBlt(Canvas.Handle, I, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + G + W, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + 2 * G + 2 * W, 1, W, W, BLACKNESS);
    except
      Free;
      raise;
    end;
  end;

var
  NewGlyph: TBitmap;
  DestroyNeeded: Boolean;
begin
  case FGlyphKind of
    gkDefault:
      begin
        DestroyNeeded := False;
        NewGlyph := GetDefaultBitmap(DestroyNeeded);
        try
          FButton.Glyph.Assign(NewGlyph);
          NumGlyphs := FDefNumGlyphs;
        finally
          if DestroyNeeded then NewGlyph.Destroy;
        end;
      end;
    gkDropDown:
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

const
  FileBitmap: TBitmap = nil;
  DateBitmap: TBitmap = nil;

{ TJvxFileDirEdit }

constructor TJvxFileDirEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OEMConvert := True;
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

function TJvxFileDirEdit.GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap;
begin
  DestroyNeeded := False;
  if FileBitmap = nil then begin
    FileBitmap := TBitmap.Create;
    FileBitmap.Handle := LoadBitmap(hInstance, sFileBmp);
  end;
  Result := FileBitmap;
end;

procedure TJvxFileDirEdit.DoBeforeDialog(var FileName: string;
  var Action: Boolean);
begin
  if Assigned(FOnBeforeDialog) then FOnBeforeDialog(Self, FileName, Action);
end;

procedure TJvxFileDirEdit.DoAfterDialog(var FileName: string;
  var Action: Boolean);
begin
  if Assigned(FOnAfterDialog) then FOnAfterDialog(Self, FileName, Action);
end;

procedure TJvxFileDirEdit.CreateHandle;
begin
  inherited CreateHandle;
  if FAcceptFiles then SetDragAccept(True);
end;

procedure TJvxFileDirEdit.DestroyWindowHandle;
begin
  SetDragAccept(False);
  inherited DestroyWindowHandle;
end;

procedure TJvxFileDirEdit.SetDragAccept(Value: Boolean);
begin
  if not (csDesigning in ComponentState) and (Handle <> 0) then
    DragAcceptFiles(Handle, Value);
end;

procedure TJvxFileDirEdit.SetAcceptFiles(Value: Boolean);
begin
  if FAcceptFiles <> Value then begin
    SetDragAccept(Value);
    FAcceptFiles := Value;
  end;
end;

procedure TJvxFileDirEdit.DisableSysErrors;
begin
  FErrMode := SetErrorMode(SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
end;

procedure TJvxFileDirEdit.EnableSysErrors;
begin
  SetErrorMode(FErrMode);
  FErrMode := 0;
end;

procedure TJvxFileDirEdit.WMDropFiles(var Msg: TWMDropFiles);
var
  AFileName: array[0..255] of Char;
  I, Num: Cardinal;
begin
  Msg.Result := 0;
  try
{$IFDEF WIN32}
    Num := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
{$ELSE}
    Num := DragQueryFile(Msg.Drop, $FFFF, nil, 0);
{$ENDIF}
    if Num > 0 then begin
      ClearFileList;
      for I := 0 to Num - 1 do begin
        DragQueryFile(Msg.Drop, I, PChar(@AFileName), Pred(SizeOf(AFileName)));
        ReceptFileDir(StrPas(AFileName));
        if not FMultipleDirs then Break;
      end;
      if Assigned(FOnDropFiles) then FOnDropFiles(Self);
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

procedure TJvxFileDirEdit.ClearFileList;
begin
end;

{ TJvxFilenameEdit }

function ClipFilename(const FileName: string): string;
var
  Params: string;
begin
  if FileExists(FileName) then Result := FileName
  else SplitCommandLine(FileName, Result, Params);
end;

function ExtFilename(const FileName: string): string;
begin
  if (Pos(' ', FileName) > 0) and (FileName[1] <> '"') then
    Result := Format('"%s"', [FileName])
  else Result := FileName;
end;

constructor TJvxFilenameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateEditDialog;
end;

procedure TJvxFilenameEdit.CreateEditDialog;
var
  NewDialog: TOpenDialog;
begin
  case FDialogKind of
    dkOpen: NewDialog := TOpenDialog.Create(Self);
{$IFDEF Delphi3_Up}
    dkOpenPicture: NewDialog := TOpenPictureDialog.Create(Self);
    dkSavePicture: NewDialog := TSavePictureDialog.Create(Self);
{$ENDIF}
    else {dkSave} NewDialog := TSaveDialog.Create(Self);
  end;
  try
    if FDialog <> nil then begin
      with NewDialog do begin
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
    else begin
      NewDialog.Title := LoadStr(SBrowse);
      NewDialog.Filter := LoadStr(SDefaultFilter);
      NewDialog.Options := [ofHideReadOnly];
    end;
  finally
    FDialog := NewDialog;
  end;
end;

function TJvxFilenameEdit.IsCustomTitle: Boolean;
begin
  Result := CompareStr(LoadStr(SBrowse), FDialog.Title) <> 0;
end;

function TJvxFilenameEdit.IsCustomFilter: Boolean;
begin
  Result := CompareStr(LoadStr(SDefaultFilter), FDialog.Filter) <> 0;
end;

procedure TJvxFilenameEdit.ButtonClick;
var
  Temp: string;
  Action: Boolean;
begin
  inherited ButtonClick;
  Temp := inherited Text;
  Action := True;
  Temp := ClipFilename(Temp);
  DoBeforeDialog(Temp, Action);
  if not Action then Exit;
  if ValidFileName(Temp) then
    try
      if DirExists(ExtractFilePath(Temp)) then
        SetInitialDir(ExtractFilePath(Temp));
      if (ExtractFileName(Temp) = '') or
        not ValidFileName(ExtractFileName(Temp)) then Temp := '';
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
  if Action then Temp := FDialog.FileName;
  if CanFocus then SetFocus;
  DoAfterDialog(Temp, Action);
  if Action then begin
    inherited Text := ExtFilename(Temp);
    SetInitialDir(ExtractFilePath(FDialog.FileName));
  end;
end;

function TJvxFilenameEdit.GetFileName: string;
begin
  Result := ClipFilename(inherited Text);
end;

procedure TJvxFilenameEdit.SetFileName(const Value: string);
begin
  if (Value = '') or ValidFileName(ClipFilename(Value)) then begin
    inherited Text := ExtFilename(Value);
    ClearFileList;
  end
  else raise EComboEditError.CreateFmt(ResStr(SInvalidFilename), [Value]);
end;

{$IFDEF WIN32}

function TJvxFilenameEdit.GetLongName: string;
begin
  Result := ShortToLongFileName(FileName);
end;

function TJvxFilenameEdit.GetShortName: string;
begin
  Result := LongToShortFileName(FileName);
end;

{$ENDIF WIN32}

procedure TJvxFilenameEdit.ClearFileList;
begin
  FDialog.Files.Clear;
end;

procedure TJvxFilenameEdit.ReceptFileDir(const AFileName: string);
begin
  if FMultipleDirs then begin
    if FDialog.Files.Count = 0 then SetFileName(AFileName);
    FDialog.Files.Add(AFileName);
  end
  else SetFileName(AFileName);
end;

function TJvxFilenameEdit.GetDialogFiles: TStrings;
begin
  Result := FDialog.Files;
end;

function TJvxFilenameEdit.GetDefaultExt: TFileExt;
begin
  Result := FDialog.DefaultExt;
end;

function TJvxFilenameEdit.GetFileEditStyle: TFileEditStyle;
begin
  Result := FDialog.FileEditStyle;
end;

function TJvxFilenameEdit.GetFilter: string;
begin
  Result := FDialog.Filter;
end;

function TJvxFilenameEdit.GetFilterIndex: Integer;
begin
  Result := FDialog.FilterIndex;
end;

function TJvxFilenameEdit.GetInitialDir: string;
begin
  Result := FDialog.InitialDir;
end;

function TJvxFilenameEdit.GetHistoryList: TStrings;
begin
  Result := FDialog.HistoryList;
end;

function TJvxFilenameEdit.GetOptions: TOpenOptions;
begin
  Result := FDialog.Options;
end;

function TJvxFilenameEdit.GetDialogTitle: string;
begin
  Result := FDialog.Title;
end;

procedure TJvxFilenameEdit.SetDialogKind(Value: TFileDialogKind);
begin
  if FDialogKind <> Value then begin
    FDialogKind := Value;
    CreateEditDialog;
  end;
end;

procedure TJvxFilenameEdit.SetDefaultExt(Value: TFileExt);
begin
  FDialog.DefaultExt := Value;
end;

procedure TJvxFilenameEdit.SetFileEditStyle(Value: TFileEditStyle);
begin
  FDialog.FileEditStyle := Value;
end;

procedure TJvxFilenameEdit.SetFilter(const Value: string);
begin
  FDialog.Filter := Value;
end;

procedure TJvxFilenameEdit.SetFilterIndex(Value: Integer);
begin
  FDialog.FilterIndex := Value;
end;

procedure TJvxFilenameEdit.SetInitialDir(const Value: string);
begin
  FDialog.InitialDir := Value;
end;

procedure TJvxFilenameEdit.SetHistoryList(Value: TStrings);
begin
  FDialog.HistoryList := Value;
end;

procedure TJvxFilenameEdit.SetOptions(Value: TOpenOptions);
begin
  if Value <> FDialog.Options then begin
    FDialog.Options := Value;
    FMultipleDirs := ofAllowMultiSelect in FDialog.Options;
    if not FMultipleDirs then ClearFileList;
  end;
end;

procedure TJvxFilenameEdit.SetDialogTitle(const Value: string);
begin
  FDialog.Title := Value;
end;

{ TJvxDirectoryEdit }

constructor TJvxDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [];
end;

procedure TJvxDirectoryEdit.ButtonClick;
var
  Temp: string;
  Action: Boolean;
begin
  inherited ButtonClick;
  Temp := Text;
  Action := True;
  DoBeforeDialog(Temp, Action);
  if not Action then Exit;
  if (Temp = '') then begin
    if (InitialDir <> '') then Temp := InitialDir
    else Temp := '\';
  end;
  if not DirExists(Temp) then Temp := '\';
  DisableSysErrors;
  try
{$IFDEF WIN32}
    if NewStyleControls and (DialogKind = dkWin32) then
      Action := BrowseDirectory(Temp, FDialogText, Self.HelpContext)
    else Action := SelectDirectory(Temp, FOptions, Self.HelpContext);
{$ELSE}
    Action := SelectDirectory(Temp, FOptions, Self.HelpContext);
{$ENDIF}
  finally
    EnableSysErrors;
  end;
  if CanFocus then SetFocus;
  DoAfterDialog(Temp, Action);
  if Action then begin
    SelText := '';
    if (Text = '') or not MultipleDirs then Text := Temp
    else Text := Text + ';' + Temp;
    if (Temp <> '') and DirExists(Temp) then InitialDir := Temp;
  end;
end;

procedure TJvxDirectoryEdit.ReceptFileDir(const AFileName: string);
var
  Temp: string;
begin
  if FileExists(AFileName) then Temp := ExtractFilePath(AFileName)
  else Temp := AFileName;
  if (Text = '') or not MultipleDirs then Text := Temp
  else Text := Text + ';' + Temp;
end;

{$IFDEF WIN32}

function TJvxDirectoryEdit.GetLongName: string;
var
  Temp: string;
  Pos: Integer;
begin
  if not MultipleDirs then Result := ShortToLongPath(Text)
  else begin
    Result := '';
    Pos := 1;
    while Pos <= Length(Text) do begin
      Temp := ShortToLongPath(ExtractSubstr(Text, Pos, [';']));
      if (Result <> '') and (Temp <> '') then Result := Result + ';';
      Result := Result + Temp;
    end;
  end;
end;

function TJvxDirectoryEdit.GetShortName: string;
var
  Temp: string;
  Pos: Integer;
begin
  if not MultipleDirs then Result := LongToShortPath(Text)
  else begin
    Result := '';
    Pos := 1;
    while Pos <= Length(Text) do begin
      Temp := LongToShortPath(ExtractSubstr(Text, Pos, [';']));
      if (Result <> '') and (Temp <> '') then Result := Result + ';';
      Result := Result + Temp;
    end;
  end;
end;

{$ENDIF WIN32}

{ TJvxCustomDateEdit }

function NvlDate(DateValue, DefaultValue: TDateTime): TDateTime;
begin
  if DateValue = NullDate then Result := DefaultValue
  else Result := DateValue;
end;

constructor TJvxCustomDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBlanksChar := ' ';
  FTitle := LoadStr(SDateDlgTitle);
  FPopupColor := clBtnFace;
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
    FPopup := TJvxPopupWindow(CreatePopupCalendar(Self
      {$IFDEF Delphi4_Up}, BiDiMode {$ENDIF}));
    TJvxPopupWindow(FPopup).OnCloseUp := PopupCloseUp;
    TJvxPopupWindow(FPopup).Color := FPopupColor;
{$ENDIF DEFAULT_POPUP_CALENDAR}
    GlyphKind := gkDefault; { force update }
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

destructor TJvxCustomDateEdit.Destroy;
begin
  if FHooked then begin
    Application.UnhookMainWindow(FormatSettingsChange);
    FHooked := False;
  end;
  if FPopup <> nil then TJvxPopupWindow(FPopup).OnCloseUp := nil;
  FPopup.Free;
  FPopup := nil;
  TStringList(FCalendarHints).OnChange := nil;
  FCalendarHints.Free;
  FCalendarHints := nil;
  inherited Destroy;
end;

procedure TJvxCustomDateEdit.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  if Handle <> 0 then begin
    UpdateMask;
    if not (csDesigning in ComponentState) and not (IsLibrary or FHooked) then
    begin
      Application.HookMainWindow(FormatSettingsChange);
      FHooked := True;
    end;
  end;
end;

procedure TJvxCustomDateEdit.DestroyWindowHandle;
begin
  if FHooked then begin
    Application.UnhookMainWindow(FormatSettingsChange);
    FHooked := False;
  end;
  inherited DestroyWindowHandle;
end;

procedure TJvxCustomDateEdit.UpdateFormat;
begin
  FDateFormat := DefDateFormat(FourDigitYear);
end;

function TJvxCustomDateEdit.GetDateFormat: string;
begin
  Result := FDateFormat;
end;

function TJvxCustomDateEdit.TextStored: Boolean;
begin
  Result := not IsEmptyStr(Text, [#0, ' ', DateSeparator, FBlanksChar]);
end;

procedure TJvxCustomDateEdit.CheckValidDate;
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
      if CanFocus then SetFocus;
      raise;
    end;
end;

procedure TJvxCustomDateEdit.Change;
begin
  if not FFormatting then inherited Change;
end;

procedure TJvxCustomDateEdit.CMExit(var Message: TCMExit);
begin
  if not (csDesigning in ComponentState) and CheckOnExit then
    CheckValidDate;
  inherited;
end;

function TJvxCustomDateEdit.GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap;
begin
  DestroyNeeded := False;
  if DateBitmap = nil then begin
    DateBitmap := TBitmap.Create;
    DateBitmap.Handle := LoadBitmap(hInstance, sDateBmp);
  end;
  Result := DateBitmap;
end;

procedure TJvxCustomDateEdit.SetBlanksChar(Value: Char);
begin
  if Value <> FBlanksChar then begin
    if (Value < ' ') then Value := ' ';
    FBlanksChar := Value;
    UpdateMask;
  end;
end;

procedure TJvxCustomDateEdit.UpdateMask;
var
  DateValue: TDateTime;
  OldFormat: string[10];
begin
  DateValue := GetDate;
  OldFormat := FDateFormat;
  UpdateFormat;
  if (GetDateMask <> EditMask) or (OldFormat <> FDateFormat) then begin
    { force update }
    EditMask := '';
    EditMask := GetDateMask;
  end;
  UpdatePopup;
  SetDate(DateValue);
end;

function TJvxCustomDateEdit.FormatSettingsChange(var Message: TMessage): Boolean;
begin
  Result := False;
  if (Message.Msg = WM_WININICHANGE)
    {$IFDEF WIN32} and Application.UpdateFormatSettings {$ENDIF} then
    UpdateMask;
end;

function TJvxCustomDateEdit.FourDigitYear: Boolean;
begin
  Result := (FYearDigits = dyFour) or ((FYearDigits = dyDefault) and
    (JvxDateUtil.FourDigitYear));
end;

function TJvxCustomDateEdit.GetDateMask: string;
begin
  Result := DefDateMask(FBlanksChar, FourDigitYear);
end;

function TJvxCustomDateEdit.GetDate: TDateTime;
begin
  if DefaultToday then Result := SysUtils.Date
  else Result := NullDate;
  Result := StrToDateFmtDef(FDateFormat, Text, Result);
end;

procedure TJvxCustomDateEdit.SetDate(Value: TDateTime);
var
  D: TDateTime;
begin
  if not ValidDate(Value) or (Value = NullDate) then begin
    if DefaultToday then Value := SysUtils.Date
    else Value := NullDate;
  end;
  D := Date;
  if Value = NullDate then Text := ''
  else Text := FormatDateTime(FDateFormat, Value);
  Modified := D <> Date;
end;

procedure TJvxCustomDateEdit.ApplyDate(Value: TDateTime);
begin
  SetDate(Value);
  SelectAll;
end;

function TJvxCustomDateEdit.GetDialogTitle: string;
begin
  Result := FTitle;
end;

procedure TJvxCustomDateEdit.SetDialogTitle(const Value: string);
begin
  FTitle := Value;
end;

function TJvxCustomDateEdit.IsCustomTitle: Boolean;
begin
  Result := (CompareStr(LoadStr(SDateDlgTitle), DialogTitle) <> 0) and (FTitle <> EmptyStr);
end;

procedure TJvxCustomDateEdit.UpdatePopup;
begin
  if FPopup <> nil then SetupPopupCalendar(FPopup, FStartOfWeek,
    FWeekends, FWeekendColor, FCalendarHints, FourDigitYear);
end;

procedure TJvxCustomDateEdit.SetYearDigits(Value: TYearDigits);
begin
  if FYearDigits <> Value then begin
    FYearDigits := Value;
    UpdateMask;
  end;
end;

function TJvxCustomDateEdit.GetPopupColor: TColor;
begin
  if FPopup <> nil then Result := TJvxPopupWindow(FPopup).Color
  else Result := FPopupColor;
end;

procedure TJvxCustomDateEdit.SetPopupColor(Value: TColor);
begin
  if Value <> PopupColor then begin
    if FPopup <> nil then TJvxPopupWindow(FPopup).Color := Value;
    FPopupColor := Value;
  end;
end;

function TJvxCustomDateEdit.GetCalendarStyle: TCalendarStyle;
begin
  if FPopup <> nil then Result := csPopup
  else Result := csDialog;
end;

procedure TJvxCustomDateEdit.SetCalendarStyle(Value: TCalendarStyle);
begin
  if Value <> CalendarStyle then begin
    case Value of
      csPopup:
        begin
          if FPopup = nil then
            FPopup := TJvxPopupWindow(CreatePopupCalendar(Self
              {$IFDEF Delphi4_Up}, BiDiMode {$ENDIF}));
          TJvxPopupWindow(FPopup).OnCloseUp := PopupCloseUp;
          TJvxPopupWindow(FPopup).Color := FPopupColor;
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

procedure TJvxCustomDateEdit.SetCalendarHints(Value: TStrings);
begin
  FCalendarHints.Assign(Value);
end;

procedure TJvxCustomDateEdit.CalendarHintsChanged(Sender: TObject);
begin
  TStringList(FCalendarHints).OnChange := nil;
  try
    while (FCalendarHints.Count > 4) do
      FCalendarHints.Delete(FCalendarHints.Count - 1);
  finally
    TStringList(FCalendarHints).OnChange := CalendarHintsChanged;
  end;
  if not (csDesigning in ComponentState) then UpdatePopup;
end;

procedure TJvxCustomDateEdit.SetWeekendColor(Value: TColor);
begin
  if Value <> FWeekendColor then begin
    FWeekendColor := Value;
    UpdatePopup;
  end;
end;

procedure TJvxCustomDateEdit.SetWeekends(Value: TDaysOfWeek);
begin
  if Value <> FWeekends then begin
    FWeekends := Value;
    UpdatePopup;
  end;
end;

procedure TJvxCustomDateEdit.SetStartOfWeek(Value: TDayOfWeekName);
begin
  if Value <> FStartOfWeek then begin
    FStartOfWeek := Value;
    UpdatePopup;
  end;
end;

procedure TJvxCustomDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_PRIOR, VK_NEXT, VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN,
    VK_ADD, VK_SUBTRACT]) and
    PopupVisible then
  begin
    TJvxPopupWindow(FPopup).KeyDown(Key, Shift);
    Key := 0;
  end
  else if (Shift = []) and DirectInput then begin
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

procedure TJvxCustomDateEdit.KeyPress(var Key: Char);
begin
  if (Key in ['T', 't', '+', '-']) and PopupVisible then begin
    TJvxPopupWindow(FPopup).KeyPress(Key);
    Key := #0;
  end
  else if DirectInput then begin
    case Key of
      'T', 't':
        begin
          ApplyDate(Trunc(Now));
          Key := #0;
        end;
      '+', '-':
        begin
          Key := #0;
        end;
    end;
  end;
  inherited KeyPress(Key);
end;

procedure TJvxCustomDateEdit.ButtonClick;
var
  D: TDateTime;
  Action: Boolean;
begin
  inherited ButtonClick;
  if CalendarStyle = csDialog then begin
    D := Self.Date;
    Action := SelectDate(D, DialogTitle, FStartOfWeek, FWeekends,
      FWeekendColor, FCalendarHints);
    if CanFocus then SetFocus;
    if Action then begin
      if Assigned(FOnAcceptDate) then FOnAcceptDate(Self, D, Action);
      if Action then begin
        Self.Date := D;
        if FFocused then inherited SelectAll;
      end;
    end;
  end;
end;

{$IFDEF WIN32}
function TJvxCustomDateEdit.AcceptPopup(var Value: Variant): Boolean;
{$ELSE}
function TJvxCustomDateEdit.AcceptPopup(var Value: string): Boolean;
{$ENDIF}
var
  D: TDateTime;
begin
  Result := True;
  if Assigned(FOnAcceptDate) then begin
{$IFDEF WIN32}
    if VarIsNull(Value) or VarIsEmpty(Value) then D := NullDate
    else
      try
        D := VarToDateTime(Value);
      except
        if DefaultToday then D := SysUtils.Date else D := NullDate;
      end;
{$ELSE}
    if DefaultToday then D := SysUtils.Date else D := NullDate;
    D := StrToDateDef(Value, D);
{$ENDIF}
    FOnAcceptDate(Self, D, Result);
{$IFDEF WIN32}
    if Result then Value := VarFromDateTime(D);
{$ELSE}
    if Result then Value := FormatDateTime(FDateFormat, D);
{$ENDIF}
  end;
end;

{$IFDEF WIN32}
procedure TJvxCustomDateEdit.SetPopupValue(const Value: Variant);
begin
  inherited SetPopupValue(StrToDateFmtDef(FDateFormat, VarToStr(Value),
    SysUtils.Date));
end;

procedure TJvxCustomDateEdit.AcceptValue(const Value: Variant);
begin
  SetDate(VarToDateTime(Value));
  UpdatePopupVisible;
  if Modified then inherited Change;
end;
{$ENDIF}

{ TJvxDateEdit }

constructor TJvxDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UpdateMask;
end;

{ Utility routines }

procedure DateFormatChanged;

  procedure IterateControls(AControl: TWinControl);
  var
    I: Integer;
  begin
    with AControl do
      for I := 0 to ControlCount - 1 do begin
        if Controls[I] is TJvxCustomDateEdit then
          TJvxCustomDateEdit(Controls[I]).UpdateMask
        else if Controls[I] is TWinControl then
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
