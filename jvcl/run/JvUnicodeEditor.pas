{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvEditor.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):
Burov Dmitry, translation of russian text.
Andreas Hausladen
Peter Thцrnqvist
Remko Bonte

Last Modified: 2003-09-20

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : TJvWideEditor
description : 'Delphi IDE'-like Editor (unicode)

Known Issues:
  Some russian comments were translated to english; these comments are marked
  with [translated]
-----------------------------------------------------------------------------}

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvUnicodeEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  ExtCtrls, StdCtrls, Clipbrd,
  JvJCLUtils, JvFixedEditPopup, JclUnicode,
  JvUnicodeCanvas, JvComponent, JvExControls;

const
  Max_X = 1024; {max symbols per row}
  Max_X_Scroll = Max_X;
  {max symbols per row for scrollbar}
  GutterRightMargin = 2;

  {$IFDEF VCL}
  WM_EDITCOMMAND = WM_USER + $101;
  {$ELSE}
  WM_EDITCOMMAND = CM_BASE + $101;
  {$ENDIF VCL}

const
  sWideLineBreak = WideString(#13#10);
  sWideLineBreakLen = Length(sWideLineBreak);

type
  TCellRect = record
    Width: Integer;
    Height: Integer;
  end;

  TLineAttr = record
    FC: TColor;
    BC: TColor;
    Style: TFontStyles;
  end;

  TJvCustomWideEditor = class;

  TLineAttrs = array [0..Max_X] of TLineAttr;
  TOnGetLineAttr = procedure(Sender: TObject; var Line: WideString; Index: Integer;
    var Attrs: TLineAttrs) of object;
  TOnChangeStatus = TNotifyEvent;

  TJvWideEditorStrings = class(TWideStringList)
  private
    FJvEditor: TJvCustomWideEditor;
    procedure StringsChanged(Sender: TObject);
    procedure SetInternal(Index: Integer; Value: WideString);
    procedure ReLine;
    procedure SetLockText(const Text: WideString);
  protected
    procedure Put(Index: Integer; const S: WideString); override;
    procedure InternalPut(Index: Integer; const Value: WideString);
    function GetText: WideString; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetText(const Value: WideString); override;
    function Add(const S: WideString): Integer; override;
    procedure Insert(Index: Integer; const S: WideString); override;
    procedure DeleteText(BegX, BegY, EndX, EndY: Integer);
    procedure InsertText(X, Y: Integer; const Text: WideString);
    procedure DeleteColumnText(BegX, BegY, EndX, EndY: Integer);
    procedure InsertColumnText(X, Y: Integer; const Text: WideString);
    property Internal[Index: Integer]: WideString write SetInternal;
  end;

  { TAnsiToWideStrings is a TStrings derived class that gets and sets its data
    from/to a TWideStrings list. }
  TAnsiToWideStrings = class(TStrings)
  private
    FWideStrings: TWideStrings;
    FDestroying: Boolean;
  protected
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
  public
    constructor Create(AWideStrings: TWideStrings);
    destructor Destroy; override;

    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Move(CurIndex, NewIndex: Integer); override;

    property WideStrings: TWideStrings read FWideStrings;
  end;

  TModifiedAction = (maAll, maInsert, maDelete, maInsertColumn, maDeleteColumn,
                     maReplace);

  TBookMark = record
    X: Integer;
    Y: Integer;
    Valid: Boolean;
  end;
  TBookMarkNum = 0..9;
  TBookMarks = array [TBookMarkNum] of TBookMark;

  TJvCompletion = class;
  TOnCompletion = procedure(Sender: TObject; var Cancel: Boolean) of object;
  TOnCompletionApply = procedure(Sender: TObject; const OldString: WideString; var NewString: WideString) of object;

  { Borland Block Type:
    00 - inclusive;
    01 - line;
    02 - column;
    03 - noninclusive; }

  TSelBlockFormat = (bfInclusive, bfLine, bfColumn, bfNonInclusive);


  TJvWideEditorClient = class(TObject)
  private
    FJvEditor: TJvCustomWideEditor;
    Top: Integer;
    function Left: Integer;
    function Height: Integer;
    function Width: Integer;
    function ClientWidth: Integer;
    function ClientHeight: Integer;
    function ClientRect: TRect;
    function BoundsRect: TRect;
    function GetCanvas: TJvUnicodeCanvas;
    property Canvas: TJvUnicodeCanvas read GetCanvas;
  end;

  TJvGutter = class(TObject)
  private
    FJvEditor: TJvCustomWideEditor;
  public
    procedure Paint;
    procedure Invalidate;
  end;
  TOnPaintGutter = procedure(Sender: TObject; Canvas: TCanvas) of object;
  TOnGutterClick = procedure(Sender: TObject; Line: Integer) of object;

  TEditCommand = Word;
  TMacro = string; { used as buffer }

  TJvEditKey = class(TObject)
  public
    Key1: Word;
    Key2: Word;
    Shift1: TShiftState;
    Shift2: TShiftState;
    Command: TEditCommand;
    constructor Create(const ACommand: TEditCommand; const AKey1: Word;
      const AShift1: TShiftState);
    constructor Create2(const ACommand: TEditCommand; const AKey1: Word;
      const AShift1: TShiftState; const AKey2: Word;
      const AShift2: TShiftState);
  end;

  TCommand2Event = procedure(Sender: TObject; const Key1: Word; const Shift1: TShiftState;
      const Key2: Word; const Shift2: TShiftState; var Command: TEditCommand) of object;
  TJvKeyboard = class(TObject)
  private
    List: TList;
    FOnCommand2: TCommand2Event;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ACommand: TEditCommand; const AKey1: Word;
      const AShift1: TShiftState);
    procedure Add2(const ACommand: TEditCommand; const AKey1: Word;
      const AShift1: TShiftState; const AKey2: Word;
      const AShift2: TShiftState);
    procedure Add2Ctrl(const ACommand: TEditCommand; const AKey1: Word;
      const AShift1: TShiftState; const AKey2: Word);
    procedure Clear;
    function Command(const AKey: Word; const AShift: TShiftState): TEditCommand;
    function Command2(const AKey1: Word; const AShift1: TShiftState;
      const AKey2: Word; const AShift2: TShiftState): TEditCommand;
    procedure SetDefLayout;

    property OnCommand2: TCommand2Event read FOnCommand2 write FOnCommand2;
  end;

  EJvEditorError = class(Exception);

  TUndoBuffer = class;

  PJvSelectionRec = ^TJvSelectionRec;
  TJvSelectionRec = record
    IsSelected: Boolean; // maybe a function that checks BegX/Y EndX/Y would be better
    Selecting: Boolean;
    SelBlockFormat: TSelBlockFormat;
    SelBegX: Integer;
    SelBegY: Integer;
    SelEndX: Integer;
    SelEndY: Integer;
    SelStartX: Integer;
    SelStartY: Integer;
    SelLineOrgBegX, SelLineOrgEndX: Integer;
  end;

  TUndo = class(TObject)
  private
    FJvEditor: TJvCustomWideEditor;
    FModified: Boolean; // Editor.FModified
    FSelection: PJvSelectionRec;
    function UndoBuffer: TUndoBuffer;
  protected
    property JvEditor: TJvCustomWideEditor read FJvEditor;
  public
    constructor Create(AJvEditor: TJvCustomWideEditor);
    destructor Destroy; override;
    procedure Undo; dynamic; abstract;
    procedure Redo; dynamic; {abstract;}
    procedure SaveSelection;
    procedure RestoreSelection;
  end;

  TUndoBuffer = class(TList)
  private
    FJvEditor: TJvCustomWideEditor;
    FPtr: Integer;
    InUndo: Boolean;
    function LastUndo: TUndo;
    function IsNewGroup(AUndo: TUndo): Boolean;
    function CanRedo: Boolean;
    procedure ClearRedo;
    function IsCaretGroup: Boolean;
  public
    procedure Add(AUndo: TUndo);
    procedure Undo;
    procedure Redo;
    procedure Clear; override;
    procedure Delete;
    function CanUndo: Boolean;
  end;

  TJvControlScrollBar95 = class(TObject)
  private
    FKind: TScrollBarKind;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FSmallChange: TScrollBarInc;
    FLargeChange: TScrollBarInc;
    FPage: Integer;
    FHandle: HWND;
    FOnScroll: TScrollEvent;
    procedure SetParam(Index, Value: Integer);
  protected
    procedure Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer); dynamic;
  public
    constructor Create;
    procedure SetParams(AMin, AMax, APosition, APage: Integer);
    procedure DoScroll(var Msg: TWMScroll);
    property Kind: TScrollBarKind read FKind write FKind default sbHorizontal;
    property SmallChange: TScrollBarInc read FSmallChange write FSmallChange default 1;
    property LargeChange: TScrollBarInc read FLargeChange write FLargeChange default 1;
    property Min: Integer index 0 read FMin write SetParam default 0;
    property Max: Integer index 1 read FMax write SetParam default 100;
    property Position: Integer index 2 read FPosition write SetParam default 0;
    property Page: Integer index 3 read FPage write SetParam;
    property Handle: HWND read FHandle write FHandle;
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
  end;

  TAdjustPersistentBlockMode = (
    amInsert, amDelete, amDeleteLine, amLineConcat, amLineBreak
  );

  TJvCustomWideEditor = class(TJvCustomControl, IFixedPopupIntf)
  private
    { internal objects }
    FLines: TJvWideEditorStrings;
    FLinesAnsi: TAnsiToWideStrings;
    scbHorz: TJvControlScrollBar95;
    scbVert: TJvControlScrollBar95;
    EditorClient: TJvWideEditorClient;
    FGutter: TJvGutter;
    FKeyboard: TJvKeyboard;
    FUpdateLock: Integer;
    FUndoBuffer: TUndoBuffer;
    FGroupUndo: Boolean;
    FUndoAfterSave: Boolean;
    FCompletion: TJvCompletion;

    { internal - Columns and rows attributes }
    FCols, FRows: Integer;
    FLeftCol, FTopRow: Integer;
    // FLeftColMax, FTopRowMax : Integer;
    FLastVisibleCol: Integer;
    FLastVisibleRow: Integer;
    FCaretX: Integer;
    FCaretY: Integer;
    FVisibleColCount: Integer;
    FVisibleRowCount: Integer;

    { internal - other flags and attributes }
    FFontCache: TList;  // collects all used fonts for faster font creation
    FAllRepaint: Boolean;
    FCellRect: TCellRect;
    IgnoreKeyPress: Boolean;
    WaitSecondKey: Boolean;
    Key1: Word;
    Shift1: TShiftState;

    { internal - selection attributes }
    FSelection: TJvSelectionRec;
    FUpdateSelBegY: Integer;
    FUpdateSelEndY: Integer;
    FPersistentBlocksCaretChanged: Boolean;
    FclSelectBC: TColor;
    FclSelectFC: TColor;

    { mouse support }
    TimerScroll: TTimer;
    MouseMoveY: Integer;
    MouseMoveXX: Integer;
    MouseMoveYY: Integer;
    FDoubleClick: Boolean;
    FMouseDowned: Boolean;

    { internal }
    FTabPos: array [0..Max_X] of Boolean;
    FTabStops: WideString;
    MyDi: array [0..1024] of Integer;

    { internal - primary for TIReader support }
    FEditBuffer: WideString;
    FPEditBuffer: PWideChar;
    FEditBufferSize: Integer;

    FCompound: Integer;
    { FMacro - buffer of TEditCommand, each command represents by two chars }
    FMacro: TMacro;
    FDefMacro: TMacro;

    { visual attributes - properties }
    FBorderStyle: TBorderStyle;
    FGutterColor: TColor;
    FGutterWidth: Integer;
    FRightMarginVisible: Boolean;
    FRightMargin: Integer;
    FRightMarginColor: TColor;
    FScrollBars: TScrollStyle;
    FDoubleClickLine: Boolean;
    FSmartTab: Boolean;
    FBackSpaceUnindents: Boolean;
    FAutoIndent: Boolean;
    FKeepTrailingBlanks: Boolean;
    FCursorBeyondEOF: Boolean;
    FBlockOverwrite: Boolean;
    FPersistentBlocks: Boolean;
    FHideCaret: Boolean;

    { non-visual attributes - properties }
    FInsertMode: Boolean;
    FReadOnly: Boolean;
    FModified: Boolean;
    FRecording: Boolean;
    FBeepOnError: Boolean;

    { Events }
    FOnGetLineAttr: TOnGetLineAttr;
    FOnChange: TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnChangeStatus: TOnChangeStatus;
    FOnScroll: TNotifyEvent;
    FOnResize: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnPaintGutter: TOnPaintGutter;
    FOnGutterClick: TOnGutterClick;
    FOnGutterDblClick: TOnGutterClick;
    FOnCompletionIdentifier: TOnCompletion;
    FOnCompletionTemplate: TOnCompletion;
    FOnCompletionDrawItem: TDrawItemEvent;
    FOnCompletionMeasureItem: TMeasureItemEvent;
    FOnCompletionApply: TOnCompletionApply;
    FUseFixedPopup: boolean;

    {$IFDEF VCL}
    { internal message processing }
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    //procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMEditCommand(var Msg: TMessage); message WM_EDITCOMMAND;
    procedure WMCopy(var Msg: TMessage); message WM_COPY;
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure WMUndo(var Msg: TMessage); message WM_UNDO;

    // (p3) added to be compatible with JvFixedEditPopup
    procedure WMClear(var Msg: TMessage); message WM_CLEAR;
    procedure EMSetReadOnly(var Msg: TMessage); message EM_SETREADONLY;
    procedure EMSetSelection(var Msg: TMessage); message EM_SETSEL;
    procedure EMGetSelection(var Msg: TMessage); message EM_GETSEL;
    procedure EMCanUndo(var Msg: TMessage); message EM_CANUNDO;
    procedure WMGetTextLength(var Msg: TMessage); message WM_GETTEXTLENGTH;

    procedure WMGetText(var Msg: TWMGetText); message WM_GETTEXT;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    {$ENDIF VCL}

    procedure UpdateEditorSize;
    procedure DoCompletionIdentifier(var Cancel: Boolean);
    procedure DoCompletionTemplate(var Cancel: Boolean);
    procedure ScrollTimer(Sender: TObject);

    procedure ReLine;
    function GetDefTabStop(X: Integer; Next: Boolean): Integer;
    function GetTabStop(X, Y: Integer; Next: Boolean): Integer;
    function GetBackStop(X, Y: Integer): Integer;

    procedure TextAllChangedInternal(Unselect: Boolean);

    { property }
    procedure SetGutterWidth(AWidth: Integer);
    procedure SetGutterColor(AColor: TColor);
    function GetLines: TWideStrings;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetLines(ALines: TWideStrings);
    function GetSelStart: Integer;
    procedure SetSelStart(ASelStart: Integer);
    procedure SetSelLength(ASelLength: Integer);
    function GetSelLength: Integer;
    procedure SetSelBlockFormat(Value: TSelBlockFormat);
    procedure SetMode(Index: Integer; Value: Boolean);
    procedure SetCaretPosition(Index, Pos: Integer);
    procedure SetCols(ACols: Integer);
    procedure SetRows(ARows: Integer);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetRightMarginVisible(Value: Boolean);
    procedure SetRightMargin(Value: Integer);
    procedure SetRightMarginColor(Value: TColor);
    procedure UpdateEditorView;
    function GetSelBlockFormat: TSelBlockFormat;
    function IsNewSelection: Boolean;
    procedure AdjustPersistentBlockSelection(X, Y: Integer;
      Mode: TAdjustPersistentBlockMode; Args: array of Integer);
    procedure AdjustSelLineMode(Restore: Boolean);
    function GetLinesAnsi: TStrings;
    procedure SetLinesAnsi(Value: TStrings);
  protected
    LineAttrs: TLineAttrs;

    procedure Resize; override;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    {$IFDEF VisualCLX}
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
    {$ENDIF VisualCLX}
    procedure Paint; override;
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var
      ScrollPos: Integer);
    procedure Scroll(Vert: Boolean; ScrollPos: Integer);
    procedure PaintLine(Line: Integer; ColBeg, ColEnd: Integer);
    function FontCacheFind(LA: TLineAttr): TFont;
    procedure FontCacheClear;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: WideChar); reintroduce;
    procedure InsertChar(const Key: WideChar);
    function GetClipboardBlockFormat: TSelBlockFormat;
    procedure SetClipboardBlockFormat(const Value: TSelBlockFormat);
    procedure SetSel(SelX, SelY: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      {$IFDEF VisualCLX} const {$ENDIF} MousePos: TPoint): Boolean;
    procedure DblClick; override;

    procedure DoGetDlgCode(var Code: TDlgCodes); override;
    procedure DoSetFocus(FocusedWnd: HWND); override;
    procedure DoKillFocus(FocusedWnd: HWND); override;

    procedure DoPaste; dynamic;
    procedure DoCopy; dynamic;
    procedure DoCut; dynamic;
    procedure CursorChanged; override;
    procedure FontChanged; override;
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;

    procedure DrawRightMargin;
    procedure PaintSelection;
    procedure SetUnSelected;
    procedure RemoveSelectedBlock;
    procedure PersistentBlocksSetUnSelected;
    procedure SetSelUpdateRegion(BegY, EndY: Integer);
    procedure Mouse2Cell(X, Y: Integer; var CX, CY: Integer);
    procedure Mouse2Caret(X, Y: Integer; var CX, CY: Integer);
    procedure CaretCoord(X, Y: Integer; var CX, CY: Integer);
    function PosFromMouse(X, Y: Integer): Integer;
    procedure SetLockText(const Text: WideString);
    function ExpandTabs(const S: WideString): WideString;
    function ExpandTabsAnsi(const S: AnsiString): AnsiString; // ClipboardPaste
    function GetAutoIndentStop(Y: Integer): Integer;
    function IsEmptySelection: Boolean;

    procedure NotUndoable;
    procedure NotRedoable;
    procedure SetCaretInternal(X, Y: Integer);
    procedure ValidateEditBuffer;

    procedure ChangeBookMark(BookMark: TBookMarkNum; Valid: Boolean);
    procedure BeginRecord;
    procedure EndRecord(var AMacro: TMacro);
    procedure PlayMacro(const AMacro: TMacro);

    { triggers for descendants }
    procedure Changed; dynamic;
    procedure TextAllChanged; dynamic;
    procedure StatusChanged; dynamic;
    procedure SelectionChanged; dynamic;
    procedure GetLineAttr(var Str: WideString; Line, ColBeg, ColEnd: Integer); virtual;
    procedure GetAttr(Line, ColBeg, ColEnd: Integer); virtual;
    procedure ChangeAttr(Line, ColBeg, ColEnd: Integer); virtual;
    procedure GutterPaint(Canvas: TCanvas); dynamic;
    procedure GutterClick(Line: Integer); dynamic;
    procedure GutterDblClick(Line: Integer); dynamic;
    procedure BookmarkChanged(BookMark: Integer); dynamic;
    procedure CompletionIdentifier(var Cancel: Boolean); dynamic;
    procedure CompletionTemplate(var Cancel: Boolean); dynamic;
    function GetPopupMenu: TPopupMenu; override;
    { TextModified is called when the editor content has changed. }
    procedure TextModified(ACaretX, ACaretY: Integer; Action: TModifiedAction;
      const Text: WideString); dynamic;
    property Gutter: TJvGutter read FGutter;

   { IFixedPopupIntf method assignment }
    procedure IFixedPopupIntf.Cut = ClipboardCut;
    procedure IFixedPopupIntf.Copy = ClipboardCopy;
    procedure IFixedPopupIntf.Paste = ClipboardPaste;
    procedure IFixedPopupIntf.Delete = DeleteSelected;
  public
    BookMarks: TBookMarks;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetLeftTop(ALeftCol, ATopRow: Integer);

    function CanUndo: Boolean; { IFixedPopupIntf }
    function CanCopy: Boolean; { IFiexedPopupIntf }
    function CanPaste: Boolean; { IFixedPopupIntf }
    function CanCut: Boolean;  { IFixedPopupIntf }
    function CanSelectAll: Boolean; { IFixedPopupIntf }

    procedure Undo;
    procedure Redo;
    function CanRedo: Boolean;

    procedure ClipboardCopy;
    procedure ClipboardPaste;
    procedure ClipboardCut;
    procedure DeleteSelected;
    procedure SelectAll; { IFixedPopupIntf }
    procedure ClearSelection;
    function HasSelection: Boolean; { IFixedPopupIntf }

    procedure SelectRange(BegX, BegY, EndX, EndY: Integer);
    function CalcCellRect(X, Y: Integer): TRect;
    procedure SetCaret(X, Y: Integer);
    procedure CaretFromPos(Pos: Integer; var X, Y: Integer);
    function PosFromCaret(X, Y: Integer): Integer;
    procedure PaintCaret(bShow: Boolean);
    function GetTextLen: Integer;
    function GetSelText: WideString;
    procedure SetSelText(const AValue: WideString);
    function GetWordOnCaret: WideString;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure MakeRowVisible(ARow: Integer);

    procedure Command(ACommand: TEditCommand); virtual;
    procedure PostCommand(ACommand: TEditCommand);
    procedure InsertText(const Text: WideString);
    procedure InsertColumnText(X, Y: Integer; const Text: WideString);
    procedure ReplaceWord(const NewString: WideString);
    procedure ReplaceWord2(const NewString: WideString);
    procedure IndentColumns(X: Integer; BegY, EndY: Integer);
    procedure UnIndentColumns(X: Integer; BegY, EndY: Integer);
    procedure IndentLines(UnIndent: Boolean; BegY, EndY: Integer);
    procedure IndentSelLines(UnIndent: Boolean);

    procedure BeginCompound;
    procedure EndCompound;

    function GetText(Position: Longint; Buffer: PWideChar; Count: Longint): Longint;
    property LeftCol: Integer read FLeftCol;
    property TopRow: Integer read FTopRow;
    property VisibleColCount: Integer read FVisibleColCount;
    property VisibleRowCount: Integer read FVisibleRowCount;
    property LastVisibleCol: Integer read FLastVisibleCol;
    property LastVisibleRow: Integer read FLastVisibleRow;
    property Cols: Integer read FCols write SetCols;
    property Rows: Integer read FRows write SetRows;
    property CaretX: Integer index 0 read FCaretX write SetCaretPosition;
    property CaretY: Integer index 1 read FCaretY write SetCaretPosition;
    property Modified: Boolean read FModified write FModified;
    property SelBlockFormat: TSelBlockFormat read GetSelBlockFormat write SetSelBlockFormat default bfNonInclusive;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelText: WideString read GetSelText write SetSelText;
    property Keyboard: TJvKeyboard read FKeyboard;
    property CellRect: TCellRect read FCellRect;
    property UndoBuffer: TUndoBuffer read FUndoBuffer;
    property GroupUndo: Boolean read FGroupUndo write FGroupUndo default True;
    property UndoAfterSave: Boolean read FUndoAfterSave write FUndoAfterSave;
    property Recording: Boolean read FRecording;
    property UseFixedPopup:boolean read FUseFixedPopup write FUseFixedPopup;

  public { published in descendants }
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Lines: TWideStrings read GetLines write SetLines;
    property LinesAnsi: TStrings read GetLinesAnsi write SetLinesAnsi stored False; // this is only a redirected list
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property Cursor default crIBeam;
    property Color default clWindow;

    property GutterWidth: Integer read FGutterWidth write SetGutterWidth;
    property GutterColor: TColor read FGutterColor write SetGutterColor default clBtnFace;
    property RightMarginVisible: Boolean read FRightMarginVisible write SetRightMarginVisible default True;
    property RightMargin: Integer read FRightMargin write SetRightMargin default 80;
    property RightMarginColor: TColor read FRightMarginColor write SetRightMarginColor default clBtnFace;
    property InsertMode: Boolean index 0 read FInsertMode write SetMode default True;
    property ReadOnly: Boolean index 1 read FReadOnly write SetMode default False;
    property DoubleClickLine: Boolean read FDoubleClickLine write FDoubleClickLine default False;
    property Completion: TJvCompletion read FCompletion write FCompletion;
    property TabStops: WideString read FTabStops write FTabStops;
    property SmartTab: Boolean read FSmartTab write FSmartTab default True;
    property BackSpaceUnindents: Boolean read FBackSpaceUnindents write FBackSpaceUnindents default True;
    property AutoIndent: Boolean read FAutoIndent write FAutoIndent default True;
    property KeepTrailingBlanks: Boolean read FKeepTrailingBlanks write FKeepTrailingBlanks default False;
    property CursorBeyondEOF: Boolean read FCursorBeyondEOF write FCursorBeyondEOF default False;
    property BlockOverwrite: Boolean read FBlockOverwrite write FBlockOverwrite default True;
    property PersistentBlocks: Boolean read FPersistentBlocks write FPersistentBlocks default False;
    property SelForeColor: TColor read FclSelectFC write FclSelectFC;
    property SelBackColor: TColor read FclSelectBC write FclSelectBC;
    property HideCaret: Boolean read FHideCaret write FHideCaret default False;

    property OnGetLineAttr: TOnGetLineAttr read FOnGetLineAttr write FOnGetLineAttr;
    property OnChangeStatus: TOnChangeStatus read FOnChangeStatus write FOnChangeStatus;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnPaintGutter: TOnPaintGutter read FOnPaintGutter write FOnPaintGutter;
    property OnGutterClick: TOnGutterClick read FOnGutterClick write FOnGutterClick;
    property OnGutterDblClick: TOnGutterClick read FOnGutterDblClick write FOnGutterDblClick;
    property OnCompletionIdentifier: TOnCompletion read FOnCompletionIdentifier write FOnCompletionIdentifier;
    property OnCompletionTemplate: TOnCompletion read FOnCompletionTemplate write FOnCompletionTemplate;
    property OnCompletionDrawItem: TDrawItemEvent read FOnCompletionDrawItem write FOnCompletionDrawItem;
    property OnCompletionMeasureItem: TMeasureItemEvent read FOnCompletionMeasureItem write FOnCompletionMeasureItem;
    property OnCompletionApply: TOnCompletionApply read FOnCompletionApply write FOnCompletionApply;
    property DockManager;
  end;

  TJvWideEditor = class(TJvCustomWideEditor)
  published
    property BeepOnError;
    property BorderStyle;
    property Lines;
    property LinesAnsi;
    property ScrollBars;
    property GutterWidth;
    property GutterColor;
    property RightMarginVisible;
    property RightMargin;
    property RightMarginColor;
    property InsertMode;
    property ReadOnly;
    property DoubleClickLine;
    property HideCaret;
    property Completion;
    property TabStops;
    property SmartTab;
    property BackSpaceUnindents;
    property AutoIndent;
    property KeepTrailingBlanks;
    property CursorBeyondEOF;
    property SelForeColor;
    property SelBackColor;
    property SelBlockFormat;

    property OnGetLineAttr;
    property OnChangeStatus;
    property OnScroll;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnChange;
    property OnSelectionChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property OnPaintGutter;
    property OnCompletionIdentifier;
    property OnCompletionTemplate;
    property OnCompletionDrawItem;
    property OnCompletionMeasureItem;
    property OnCompletionApply;

    { TCustomControl }
    property Align;
    property Enabled;
    property Color;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property Visible;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragKind;
    property ParentBiDiMode;
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
  end;

  TCompletionList = (cmIdentifiers, cmTemplates);

  TJvCompletion = class(TPersistent)
  private
    FJvEditor: TJvCustomWideEditor;
    FPopupList: TListBox;
    FIdentifiers: TWideStrings;
    FTemplates: TWideStrings;
    FItems: TWideStringList;
    FItemIndex: Integer;
    FMode: TCompletionList;
    FDefMode: TCompletionList;
    FItemHeight: Integer;
    FTimer: TTimer;
    FEnabled: Boolean;
    FVisible: Boolean;
    FDropDownCount: Integer;
    FDropDownWidth: Integer;
    FListBoxStyle: TListBoxStyle;
    FCaretChar: WideChar;
    FCRLF: WideString;
    FSeparator: WideString;
    function DoKeyDown(Key: Word; Shift: TShiftState): Boolean;
    procedure DoKeyPress(Key: WideChar);
    procedure OnTimer(Sender: TObject);
    procedure FindSelItem(var Eq: Boolean);
    procedure ReplaceWord(const NewString: WideString);

    procedure SetWideStrings(Index: Integer; AValue: TWideStrings);
    function GetItemIndex: Integer;
    procedure SetItemIndex(AValue: Integer);
    function GetInterval: Cardinal;
    procedure SetInterval(AValue: Cardinal);
    procedure MakeItems;
    function GetItems: TWideStrings;
  public
    constructor Create(AJvEditor: TJvCustomWideEditor);
    destructor Destroy; override;
    procedure DropDown(const AMode: TCompletionList; const ShowAlways: Boolean);
    procedure DoCompletion(const AMode: TCompletionList);
    procedure CloseUp(const Apply: Boolean);
    procedure SelectItem;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Visible: Boolean read FVisible write FVisible;
    property Mode: TCompletionList read FMode write FMode;
    property Items: TWideStringList read FItems;
  published
    property DropDownCount: Integer read FDropDownCount write FDropDownCount
      default 6;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth
      default 300;
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property Identifiers: TWideStrings index 0 read FIdentifiers write SeTWideStrings;
    property Templates: TWideStrings index 1 read FTemplates write SeTWideStrings;
    property ItemHeight: Integer read FItemHeight write FItemHeight;
    property Interval: Cardinal read GetInterval write SetInterval;
    property ListBoxStyle: TListBoxStyle read FListBoxStyle write FListBoxStyle;
    property CaretChar: WideChar read FCaretChar write FCaretChar;
    property CRLF: WideString read FCRLF write FCRLF;
    property Separator: WideString read FSeparator write FSeparator;
  end;

const
  { Editor commands }
  { When add new commands, please add them into JvInterpreter_JvEditor.pas unit also ! }
  ecCharFirst = $00;
  ecCharLast = $FF;
  ecCommandFirst = $100;
  ecIntern = $1000; { use on internal updates }
  ecUser = $8000; { use this for descendants }

  {Cursor}
  ecLeft = ecCommandFirst + 1;
  ecUp = ecLeft + 1;
  ecRight = ecLeft + 2;
  ecDown = ecLeft + 3;
  {Cursor with select}
  ecSelLeft = ecCommandFirst + 9;
  ecSelUp = ecSelLeft + 1;
  ecSelRight = ecSelLeft + 2;
  ecSelDown = ecSelLeft + 3;
  {Cursor with column select}
  ecSelColumnLeft = ecIntern + 0;
  ecSelColumnUp = ecSelColumnLeft + 1;
  ecSelColumnRight = ecSelColumnLeft + 2;
  ecSelColumnDown = ecSelColumnLeft + 3;
  {Cursor On words [translated] }
  ecPrevWord = ecSelDown + 1;
  ecNextWord = ecPrevWord + 1;
  ecSelPrevWord = ecPrevWord + 2;
  ecSelNextWord = ecPrevWord + 3;
  ecSelWord = ecPrevWord + 4;

  ecWindowTop = ecSelWord + 1;
  ecWindowBottom = ecWindowTop + 1;
  ecPrevPage = ecWindowTop + 2;
  ecNextPage = ecWindowTop + 3;
  ecSelPrevPage = ecWindowTop + 4;
  ecSelNextPage = ecWindowTop + 5;

  ecBeginLine = ecSelNextPage + 1;
  ecEndLine = ecBeginLine + 1;
  ecBeginDoc = ecBeginLine + 2;
  ecEndDoc = ecBeginLine + 3;
  ecSelBeginLine = ecBeginLine + 4;
  ecSelEndLine = ecBeginLine + 5;
  ecSelBeginDoc = ecBeginLine + 6;
  ecSelEndDoc = ecBeginLine + 7;
  ecSelAll = ecBeginLine + 8;

  ecScrollLineUp = ecSelAll + 1;
  ecScrollLineDown = ecScrollLineUp + 1;

  ecInclusiveBlock = ecCommandFirst + 100;
  ecLineBlock = ecCommandFirst + 101;
  ecColumnBlock = ecCommandFirst + 102;
  ecNonInclusiveBlock = ecCommandFirst + 103;

  ecInsertPara = ecCommandFirst + 121;
  ecBackspace = ecInsertPara + 1;
  ecDelete = ecInsertPara + 2;
  ecChangeInsertMode = ecInsertPara + 3;
  ecTab = ecInsertPara + 4;
  ecBackTab = ecInsertPara + 5;
  ecIndent = ecInsertPara + 6;
  ecUnindent = ecInsertPara + 7;
  ecBackspaceWord = ecIntern + 10;

  ecDeleteSelected = ecInsertPara + 10;
  ecClipboardCopy = ecInsertPara + 11;
  ecClipboardCut = ecClipboardCopy + 1;
  ecClipboardPaste = ecClipboardCopy + 2;

  ecDeleteLine = ecClipboardPaste + 1;
  ecDeleteWord = ecDeleteLine + 1;

  ecToUpperCase = ecDeleteLine + 2;
  ecToLowerCase = ecToUpperCase + 1;
  ecChangeCase = ecToUpperCase + 2;

  ecUndo = ecChangeCase + 1;
  ecRedo = ecUndo + 1;
  ecBeginCompound = ecUndo + 2;
  ecEndCompound = ecUndo + 3;

  ecBeginUpdate = ecUndo + 4;
  ecEndUpdate = ecUndo + 5;

  ecSetBookmark0 = ecEndUpdate + 1;
  ecSetBookmark1 = ecSetBookmark0 + 1;
  ecSetBookmark2 = ecSetBookmark0 + 2;
  ecSetBookmark3 = ecSetBookmark0 + 3;
  ecSetBookmark4 = ecSetBookmark0 + 4;
  ecSetBookmark5 = ecSetBookmark0 + 5;
  ecSetBookmark6 = ecSetBookmark0 + 6;
  ecSetBookmark7 = ecSetBookmark0 + 7;
  ecSetBookmark8 = ecSetBookmark0 + 8;
  ecSetBookmark9 = ecSetBookmark0 + 9;

  ecGotoBookmark0 = ecSetBookmark9 + 1;
  ecGotoBookmark1 = ecGotoBookmark0 + 1;
  ecGotoBookmark2 = ecGotoBookmark0 + 2;
  ecGotoBookmark3 = ecGotoBookmark0 + 3;
  ecGotoBookmark4 = ecGotoBookmark0 + 4;
  ecGotoBookmark5 = ecGotoBookmark0 + 5;
  ecGotoBookmark6 = ecGotoBookmark0 + 6;
  ecGotoBookmark7 = ecGotoBookmark0 + 7;
  ecGotoBookmark8 = ecGotoBookmark0 + 8;
  ecGotoBookmark9 = ecGotoBookmark0 + 9;

  ecCompletionIdentifiers = ecGotoBookmark9 + 1;
  ecCompletionTemplates = ecCompletionIdentifiers + 1;

  ecRecordMacro = ecCompletionTemplates + 1;
  ecPlayMacro = ecRecordMacro + 1;
  ecBeginRecord = ecRecordMacro + 2;
  ecEndRecord = ecRecordMacro + 3;

  twoKeyCommand = High(Word);

implementation

uses
  Consts, Math,
  JvThemes, JvConsts, JvTypes, JvResources;

type
  TJvCaretUndo = class(TUndo)
  private
    FCaretX: Integer;
    FCaretY: Integer;
  public
    constructor Create(AJvEditor: TJvCustomWideEditor; ACaretX, ACaretY: Integer);
    procedure Undo; override;
    procedure Redo; override;
  end;

  TJvInsertUndo = class(TJvCaretUndo)
  private
    FText: WideString;
  public
    constructor Create(AJvEditor: TJvCustomWideEditor; ACaretX, ACaretY: Integer;
      const AText: WideString);
    procedure Undo; override;
  end;

  TJvOverwriteUndo = class(TJvCaretUndo)
  private
    FOldText: WideString;
    FNewText: WideString;
  public
    constructor Create(AJvEditor: TJvCustomWideEditor; ACaretX, ACaretY: Integer;
      const AOldText, ANewText: WideString);
    procedure Undo; override;
  end;

  TJvReLineUndo = class(TJvInsertUndo);

  TJvInsertTabUndo = class(TJvInsertUndo);

  TJvInsertColumnUndo = class(TJvInsertUndo)
  public
    procedure Undo; override;
  end;

  TJvDeleteUndo = class(TJvInsertUndo)
  public
    procedure Undo; override;
  end;

  TJvDeleteLineUndo = class(TJvInsertUndo)
  public
    procedure Undo; override;
    procedure Redo; override;
  end;

  TJvDeleteTrailUndo = class(TJvDeleteUndo);

  TJvBackspaceUndo = class(TJvDeleteUndo)
  public
    procedure Undo; override;
  end;

  TJvBackspaceUnindentUndo = class(TJvDeleteUndo)
  public
    procedure Undo; override;
  end;

  TJvReplaceUndo = class(TJvCaretUndo)
  private
    FBegX: Integer;
    FBegY: Integer;
    FText: WideString;
    FNewText: WideString;
  public
    constructor Create(AJvEditor: TJvCustomWideEditor; ACaretX, ACaretY: Integer;
      ABegX, ABegY: Integer; const AText, ANewText: WideString);
    procedure Undo; override;
  end;

  TJvDeleteSelectedUndo = class(TJvDeleteUndo)
  public
    constructor Create(AJvEditor: TJvCustomWideEditor; ACaretX, ACaretY: Integer;
      const AText: WideString);
    procedure Undo; override;
  end;

  TJvSelectUndo = class(TJvCaretUndo)
  public
    constructor Create(AJvEditor: TJvCustomWideEditor; ACaretX, ACaretY: Integer);
    procedure Undo; override;
  end;

  TJvUnselectUndo = class(TJvSelectUndo);

(* make Delphi 5 compiler happy // andreas
  TJvIndentColumnUndo = class(TJvInsertColumnUndo)
  private
    FNewCaretX: Integer;
    FNewCaretY: Integer;
  public
    constructor Create(AJvEditor: TJvCustomWideEditor; ACaretX, ACaretY: Integer;
      ABegX, ABegY: Integer; const AText: WideString);
    procedure Undo; override;
  end;
*)

  TJvUnindentColumnUndo = class(TJvInsertUndo)
  private
    FBegX: Integer;
    FBegY: Integer;
  public
    constructor Create(AJvEditor: TJvCustomWideEditor; ACaretX, ACaretY,
      ABegX, ABegY: Integer; const AText: WideString);
    procedure Undo; override;
  end;

  TJvBeginCompoundUndo = class(TUndo)
  public
    procedure Undo; override;
  end;

  TJvEndCompoundUndo = class(TJvBeginCompoundUndo);

var
  BlockTypeFormat: Integer;

function KeyPressed(VK: Integer): Boolean;
begin
  Result := GetKeyState(VK) and $8000 = $8000;
end;

procedure GetEndPosCaret(const Text: WideString; CaretX, CaretY: Integer;
  var X, Y: Integer);
{ GetEndPosCaret returns the caret position of the last char. For the position
  after the last char of Text you must add 1 to the returned X value. }
begin
  GetXYByPosW(Text, Length(Text), X, Y);
  
  if Y = 0 then
    Inc(X, CaretX)
  else
    Inc(X);

  Dec(X);
  Inc(Y, CaretY);
end;

//=== TJvControlScrollBar95 ==================================================

constructor TJvControlScrollBar95.Create;
begin
  inherited Create;
  FPage := 1;
  FSmallChange := 1;
  FLargeChange := 1;
end;

const
  SBKIND: array [TScrollBarKind] of Integer = (SB_HORZ, SB_VERT);

procedure TJvControlScrollBar95.SetParams(AMin, AMax, APosition, APage: Integer);
var
  ScrollInfo: TScrollInfo;
begin
  if AMax < AMin then
    raise EInvalidOperation.Create(SScrollBarRange);
  if APosition < AMin then
    APosition := AMin;
  if APosition > AMax then
    APosition := AMax;
  if Handle > 0 then
  begin
    with ScrollInfo do
    begin
      cbSize := SizeOf(TScrollInfo);
      fMask := SIF_DISABLENOSCROLL;
      if (AMin >= 0) or (AMax >= 0) then
        fMask := fMask or SIF_RANGE;
      if APosition >= 0 then
        fMask := fMask or SIF_POS;
      if APage >= 0 then
        fMask := fMask or SIF_PAGE;
      nPos := APosition;
      nMin := AMin;
      nMax := AMax;
      nPage := APage;
    end;
    SetScrollInfo(
      Handle, // handle of window with scroll bar
      SBKIND[Kind], // scroll bar flag
      ScrollInfo, // pointer to structure with scroll parameters
      True); // redraw flag
  end;
end;

procedure TJvControlScrollBar95.SetParam(Index, Value: Integer);
begin
  case Index of
    0:
      FMin := Value;
    1:
      FMax := Value;
    2:
      FPosition := Value;
    3:
      FPage := Value;
  end;
  if FMax < FMin then
    raise EInvalidOperation.Create(SScrollBarRange);
  if FPosition < FMin then
    FPosition := FMin;
  if FPosition > FMax then
    FPosition := FMax;
  SetParams(FMin, FMax, FPosition, FPage);
end;

{
procedure TJvControlScrollBar95.SetVisible(Value : Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Handle <> 0 then

  end;
end;
}

procedure TJvControlScrollBar95.DoScroll(var Msg: TWMScroll);
var
  ScrollPos: Integer;
  NewPos: Longint;
  ScrollInfo: TScrollInfo;
begin
  with Msg do
  begin
    NewPos := FPosition;
    case TScrollCode(ScrollCode) of
      scLineUp:
        Dec(NewPos, FSmallChange);
      scLineDown:
        Inc(NewPos, FSmallChange);
      scPageUp:
        Dec(NewPos, FLargeChange);
      scPageDown:
        Inc(NewPos, FLargeChange);
      scPosition, scTrack:
        with ScrollInfo do
        begin
          cbSize := SizeOf(ScrollInfo);
          fMask := SIF_ALL;
          GetScrollInfo(Handle, SBKIND[Kind], ScrollInfo);
          NewPos := nTrackPos;
        end;
      scTop:
        NewPos := FMin;
      scBottom:
        NewPos := FMax;
    end;
    if NewPos < FMin then
      NewPos := FMin;
    if NewPos > FMax then
      NewPos := FMax;
    ScrollPos := NewPos;
    Scroll(TScrollCode(ScrollCode), ScrollPos);
  end;
  Position := ScrollPos;
end;

procedure TJvControlScrollBar95.Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self, ScrollCode, ScrollPos);
end;

//=== TJvWideEditorStrings =======================================================

{ TJvWideEditorStrings }

constructor TJvWideEditorStrings.Create;
begin
  inherited Create;
  NormalizationForm := nfNone;
  OnChange := StringsChanged;
  SaveUnicode := False;
end;

destructor TJvWideEditorStrings.Destroy;
begin
  // workaround for a bug in JclUnicode.pas::TWideStringList.Destroy 
  OnChange := nil;
  OnChanging := nil;
  inherited;
end;

procedure TJvWideEditorStrings.SetText(const Value: WideString);
begin
  inherited SetText(FJvEditor.ExpandTabs(Value));
  if FJvEditor.FUpdateLock = 0 then
    FJvEditor.NotUndoable;
  FJvEditor.TextAllChanged;
end;

procedure TJvWideEditorStrings.StringsChanged(Sender: TObject);
begin
  if FJvEditor.FUpdateLock = 0 then
    FJvEditor.TextAllChanged;
end;

procedure TJvWideEditorStrings.SetLockText(const Text: WideString);
begin
  Inc(FJvEditor.FUpdateLock);
  try
    inherited SetText(Text)
  finally
    Dec(FJvEditor.FUpdateLock);
  end;
end;

procedure TJvWideEditorStrings.SetInternal(Index: Integer; Value: WideString);
begin
  Inc(FJvEditor.FUpdateLock);
  try
    InternalPut(Index, Value);
  finally
    Dec(FJvEditor.FUpdateLock);
  end;
end;

function TJvWideEditorStrings.Add(const S: WideString): Integer;
begin
  //Inc(FJvEditor.FUpdateLock);
  try
    Result := inherited Add(FJvEditor.ExpandTabs(S));
  finally
    //Dec(FJvEditor.FUpdateLock);
  end;
end;

procedure TJvWideEditorStrings.Insert(Index: Integer; const S: WideString);
begin
  //Inc(FJvEditor.FUpdateLock);
  try
    inherited Insert(Index, FJvEditor.ExpandTabs(S));
  finally
    //Dec(FJvEditor.FUpdateLock);
  end;
end;

procedure TJvWideEditorStrings.Put(Index: Integer; const S: WideString);
var
  L: Integer;
begin
  if FJvEditor.FKeepTrailingBlanks then
    inherited Put(Index, S)
  else
  begin
    {--- UNDO ---}
    L := Length(S) - Length(TrimRightW(S));
    if L > 0 then
      TJvDeleteTrailUndo.Create(FJvEditor, Length(S), Index, SpacesW(L));
    {--- /UNDO ---}
    inherited Put(Index, TrimRightW(S));
  end;
end;

procedure TJvWideEditorStrings.ReLine;
var
  L: Integer;
  S: WideString;
begin
  Inc(FJvEditor.FUpdateLock);
  try
    if Count = 0 then
      L := FJvEditor.FCaretX
    else
      L := Length(Strings[Count - 1]);
    while FJvEditor.FCaretY > Count - 1 do
    begin
      {--- UNDO ---}
      TJvReLineUndo.Create(FJvEditor, L, FJvEditor.FCaretY, sWideLineBreak);
      {--- /UNDO ---}
      L := 0;
      Add('');
    end;
    s := Strings[FJvEditor.FCaretY];
    if FJvEditor.FCaretX > Length(S) then
    begin
      L := FJvEditor.FCaretX - Length(S);
      {--- UNDO ---}
{     TJvReLineUndo.Create(FJvEditor, Length(S),
        FJvEditor.FCaretY, SpacesW(L)); } {disabled: will move the caret to wrong undo position }
      {--- /UNDO ---}
      inherited Put(FJvEditor.FCaretY, S + SpacesW(L));
    end;
  finally
    Dec(FJvEditor.FUpdateLock);
  end;
end;

procedure TJvWideEditorStrings.InternalPut(Index: Integer; const Value: WideString);
begin
  if FJvEditor.FKeepTrailingBlanks then
    inherited Put(Index, FJvEditor.ExpandTabs(Value))
  else
    inherited Put(Index, TrimRightW(FJvEditor.ExpandTabs(Value)));
end;

procedure TJvWideEditorStrings.DeleteText(BegX, BegY, EndX, EndY: Integer);
{ delete text from [BegX..EndY] [BegY..EndY] all inclusive.
  BegX,EndX: [0..Max_X] }
var
  BegLine, EndLine: WideString;
  i, L: Integer;
begin
  if BegY < 0 then
  begin
    BegY := 0;
    BegX := 0;
  end;
  if BegY >= Count then Exit; // nothing to delete
  if EndY >= Count then
  begin
    EndY := Count - 1;
    EndX := MaxInt - 1;
  end;
  if BegX < 0 then BegX := 0;

  Inc(FJvEditor.FUpdateLock);
  BeginUpdate;
  try
    BegLine := Strings[BegY];
   // expand BegLine if necessary
    L := (BegX + 1) - Length(BegLine) - 1;
    if L > 0 then BegLine := BegLine + SpacesW(L);

    EndLine := Strings[EndY];

   // delete lines between and end line
    for i := EndY downto BegY + 1 do
      Delete(i);

    System.Delete(BegLine, BegX + 1, MaxInt);
    System.Delete(EndLine, 1, EndX + 1);

    Internal[BegY] := BegLine + EndLine;
  finally
    EndUpdate;
    Dec(FJvEditor.FUpdateLock);
  end;
end;

procedure TJvWideEditorStrings.InsertText(X, Y: Integer; const Text: WideString);
{ insert text on X:[0..Max_X], Y }
var
  BegLine, EndLine: WideString;
  YStart: Integer;
  F, P: PWideChar;
  S, FirstLine: WideString;
  Len: Integer;
begin
  Inc(X); // increment for string functions
  if Y < 0 then Y := 0;
  while Y >= Count do Add('');

  BegLine := Strings[Y];
  EndLine := System.Copy(BegLine, X, MaxInt);
  System.Delete(BegLine, X, MaxInt);

 // line is too small -> expand it with spaces
  Len := Length(BegLine);
  if (Len < X) then
  begin
    SetLength(BegLine, X - 1);
    FillChar(BegLine[Len + 1], X - Len - 1, ' ');
  end;

  Inc(FJvEditor.FUpdateLock);
  BeginUpdate;
  try
    P := PWideChar(Text);
    F := P;
    while (P[0] <> #0) and (P[0] <> #10) and (P[0] <> #13) and
          (P[0] <> WideLineSeparator) do Inc(P);

    SetString(s, F, P - F);

    YStart := Y;
    FirstLine := BegLine + s; // set Internal[YStart] later so we keep the trailing spaces for concat EndLine

    while P[0] <> #0 do
    begin
      if P[0] = #13 then Inc(P);
      if P[0] = #10 then Inc(P);
      F := P;

      while (P[0] <> #0) and (P[0] <> #10) and (P[0] <> #13) and
            (P[0] <> WideLineSeparator) do Inc(P);
      SetString(S, F, P - F);
      Inc(Y);
      Insert(Y, S);
    end;

    if Y = YStart then
      Internal[YStart] := FirstLine + EndLine
    else
    begin
      Internal[YStart] := FirstLine;
      Internal[Y] := Strings[Y] + EndLine;
    end;
  finally
    EndUpdate;
    Dec(FJvEditor.FUpdateLock);
  end;
end;

procedure TJvWideEditorStrings.DeleteColumnText(BegX, BegY, EndX, EndY: Integer);
{ delete column text from [BegX..EndY] [BegY..EndY] all inclusive.
  BegX,EndX: [0..Max_X] }
var
  S: WideString;
  i: Integer;
begin
  if BegY < 0 then
  begin
    BegY := 0;
    BegX := 0;
  end;
  if BegY >= Count then Exit; // nothing to delete
  if EndY >= Count then
  begin
    EndY := Count - 1;
    EndX := MaxInt - 1;
  end;
  if BegX < 0 then BegX := 0;

  Inc(FJvEditor.FUpdateLock);
  BeginUpdate;
  try
    for i := BegY to EndY do
    begin
      S := FJvEditor.FLines[i];
      System.Delete(S, BegX + 1, EndX - BegX + 1);
      FJvEditor.FLines.Internal[i] := S;
    end;
  finally
    EndUpdate;
    Dec(FJvEditor.FUpdateLock);
  end;
end;

procedure TJvWideEditorStrings.InsertColumnText(X, Y: Integer; const Text: WideString);
{ insert column text on X:[0..Max_X], Y }
var
  S, Line: WideString;
  P, F: PWideChar;
  L: Integer;
begin
  Inc(X); // increment for string functions
  if Y < 0 then Y := 0;

  Inc(FJvEditor.FUpdateLock);
  BeginUpdate;
  try
    P := PWideChar(Text);
    F := P;
    while P[0] <> #0 do
    begin
      while (P[0] <> #0) and (P[0] <> #10) and (P[0] <> #13) and
            (P[0] <> WideLineSeparator) do Inc(P);
      SetString(S, F, P - F);

      while Y >= Count do Add('');
      Line := Strings[Y];
      L := (X - 1) - Length(Line);
      if L > 0 then Line := Line + SpacesW(L);
      System.Insert(S, Line, X);
      Internal[Y] := Line;

      if P[0] = #13 then Inc(P);
      if P[0] = #10 then Inc(P);
      F := P;
      Inc(Y);
    end;
  finally
    EndUpdate;
    Dec(FJvEditor.FUpdateLock);
  end;
end;

//=== TJvWideEditorClient ========================================================

function TJvWideEditorClient.GetCanvas: TJvUnicodeCanvas;
begin
  Result := TJvUnicodeCanvas(FJvEditor.Canvas);
end;

function TJvWideEditorClient.Left: Integer;
begin
  Result := FJvEditor.GutterWidth + 2;
end;

function TJvWideEditorClient.Height: Integer;
begin
  Result := FJvEditor.ClientHeight;
end;

function TJvWideEditorClient.Width: Integer;
begin
  Result := Max(FJvEditor.ClientWidth - Left, 0);
end;

function TJvWideEditorClient.ClientWidth: Integer;
begin
  Result := Width;
end;

function TJvWideEditorClient.ClientHeight: Integer;
begin
  Result := Height;
end;

function TJvWideEditorClient.ClientRect: TRect;
begin
  Result := Bounds(Left, Top, Width, Height);
end;

function TJvWideEditorClient.BoundsRect: TRect;
begin
  Result := Bounds(0, 0, Width, Height);
end;

//=== TJvGutter ==============================================================

procedure TJvGutter.Invalidate;
{var
  R : TRect;}
begin
  //  Owner.Invalidate;
  //  R := Bounds(0, 0, FJvEditor.GutterWidth, FJvEditor.Height);
  //  InvalidateRect(FJvEditor.Handle, @R, False);
  Paint;
end;

procedure TJvGutter.Paint;
begin
  with FJvEditor, Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FGutterColor;
    FillRect(Bounds(0, EditorClient.Top, GutterWidth, EditorClient.Height));
    Pen.Width := 1;
    Pen.Color := Color;
    MoveTo(GutterWidth - 2, EditorClient.Top);
    LineTo(GutterWidth - 2, EditorClient.Top + EditorClient.Height);
    Pen.Width := 2;
    MoveTo(GutterWidth + 1, EditorClient.Top);
    LineTo(GutterWidth + 1, EditorClient.Top + EditorClient.Height);
    Pen.Width := 1;
    Pen.Color := clGray;
    MoveTo(GutterWidth - 1, EditorClient.Top);
    LineTo(GutterWidth - 1, EditorClient.Top + EditorClient.Height);
  end;
  with FJvEditor do
    GutterPaint(Canvas);
end;

//=== TJvCustomWideEditor ========================================================

function TJvWideEditorStrings.GetText: WideString;
begin
  Result := GetSeparatedText(sWideLineBreak);
end;

{ TJvCustomWideEditor }

constructor TJvCustomWideEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents {, csOpaque}, csDoubleClicks,
    csReplicatable];
  IncludeThemeStyle(Self, [csNeedsBorderPaint]);
  FInsertMode := True;
  FLines := TJvWideEditorStrings.Create;
  FLines.FJvEditor := Self;
  FKeyboard := TJvKeyboard.Create;
  FRows := 1;
  FCols := 1;
  FUndoBuffer := TUndoBuffer.Create;
  FUndoBuffer.FJvEditor := Self;
  FGroupUndo := True;

  FRightMarginVisible := True;
  FRightMargin := 80;
  FBorderStyle := bsSingle;
  Ctl3D := True;
  Height := 40;
  Width := 150;
  ParentColor := False;
  Cursor := crIBeam;
  TabStop := True;
  FTabStops := '3 5';
  FSmartTab := True;
  FBackSpaceUnindents := True;
  FAutoIndent := True;
  FKeepTrailingBlanks := False;
  FCursorBeyondEOF := False;
  FBlockOverwrite := True;
  FPersistentBlocks := False;
  FBeepOnError := True;

  FScrollBars := ssBoth;
  scbHorz := TJvControlScrollBar95.Create;
  scbVert := TJvControlScrollBar95.Create;
  scbVert.Kind := sbVertical;
  scbHorz.OnScroll := ScrollBarScroll;
  scbVert.OnScroll := ScrollBarScroll;

  Color := clWindow;
  FGutterColor := clBtnFace;
  FclSelectBC := clHighLight;
  FclSelectFC := clHighLightText;
  FRightMarginColor := clSilver;

  EditorClient := TJvWideEditorClient.Create;
  EditorClient.FJvEditor := Self;
  FGutter := TJvGutter.Create;
  FGutter.FJvEditor := Self;

  FLeftCol := 0;
  FTopRow := 0;
  FSelection.IsSelected := False;
  FSelection.Selecting := False;
  FCaretX := 0;
  FCaretY := 0;

  TimerScroll := TTimer.Create(Self);
  TimerScroll.Enabled := False;
  TimerScroll.Interval := 100;
  TimerScroll.OnTimer := ScrollTimer;

  FKeyboard.SetDefLayout;
  FCompletion := TJvCompletion.Create(Self);

  FSelection.SelBlockFormat := bfNonInclusive;
  if BlockTypeFormat = 0 then
    BlockTypeFormat := RegisterClipboardFormat('Borland IDE Block Type');

  { we can change font only after all objects are created }
  Font.Name := 'Courier New';
  Font.Size := 10;

  FFontCache := TList.Create;
end;

destructor TJvCustomWideEditor.Destroy;
begin
  FLines.Free;
  if Assigned(FLinesAnsi) then
    FLinesAnsi.Free;
  scbHorz.Free;
  scbVert.Free;
  EditorClient.Free;
  FKeyboard.Free;
  FUndoBuffer.Free;
  FCompletion.Free;
  FGutter.Free;
  FontCacheClear; // free cached font instances
  FFontCache.Free;
  inherited Destroy;
end;

procedure TJvCustomWideEditor.Loaded;
begin
  inherited Loaded;
  UpdateEditorSize;
end;

{************** Handle otrisovkoj [translated] ***************}

procedure TJvCustomWideEditor.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array [TBorderStyle] of Cardinal = (0, WS_BORDER);
  ScrollStyles: array [TScrollStyle] of Cardinal = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle] or ScrollStyles[FScrollBars];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TJvCustomWideEditor.Resize;
begin
  UpdateEditorSize;
end;

procedure TJvCustomWideEditor.CreateWnd;
begin
  inherited CreateWnd;
  if FScrollBars in [ssHorizontal, ssBoth] then
    scbHorz.Handle := Handle;
  if FScrollBars in [ssVertical, ssBoth] then
    scbVert.Handle := Handle;
  FAllRepaint := True;
end;

procedure TJvCustomWideEditor.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

function TJvCustomWideEditor.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  Result := False; // no background erase
end;

procedure TJvCustomWideEditor.WMSetCursor(var Msg: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if (P.X < GutterWidth) and (Cursor = crIBeam) then
  begin
    Msg.Result := 1;
    Windows.SetCursor(Screen.Cursors[crArrow])
  end
  else
    inherited;
end;

procedure TJvCustomWideEditor.DoSetFocus(FocusedWnd: HWND);
begin
  inherited DoSetFocus(FocusedWnd);
  CreateCaret(Handle, 0, 2, CellRect.Height - 2);
  PaintCaret(True);
end;

procedure TJvCustomWideEditor.DoKillFocus(FocusedWnd: HWND);
begin
  inherited DoKillFocus(FocusedWnd);
  if FCompletion.FVisible then
    FCompletion.CloseUp(False);
  DestroyCaret;
end;

procedure TJvCustomWideEditor.DoGetDlgCode(var Code: TDlgCodes);
begin
  Code := [dcWantArrows, dcWantTab, dcWantChars, dcWantMessage];
end;

procedure TJvCustomWideEditor.WMHScroll(var Msg: TWMHScroll);
begin
  scbHorz.DoScroll(Msg);
end;

procedure TJvCustomWideEditor.WMVScroll(var Msg: TWMVScroll);
begin
  scbVert.DoScroll(Msg);
end;

{
procedure TJvCustomWideEditor.WMMouseWheel(var Msg: TWMMouseWheel);
var Shift: TShiftState;
begin
  Shift := [];
  if Msg.Keys and MK_SHIFT <> 0 then Include(Shift, ssShift);
  if Msg.Keys and MK_CONTROL <> 0 then Include(Shift, ssCtrl);
  if Msg.Keys and MK_LBUTTON <> 0 then Include(Shift, ssLeft);
  if Msg.Keys and MK_RBUTTON <> 0 then Include(Shift, ssRight);
  if Msg.Keys and MK_MBUTTON <> 0 then Include(Shift, ssMiddle);
  DoMouseWheel(Shift, Msg.WheelDelta, Point(Msg.XPos, Msg.YPos));
end;
}

procedure TJvCustomWideEditor.DoCopy;
begin
  PostCommand(ecClipboardCopy);
end;

procedure TJvCustomWideEditor.DoCut;
begin
  if not FReadOnly then
    PostCommand(ecClipboardCut);
end;

procedure TJvCustomWideEditor.DoPaste;
begin
  if not FReadOnly then
    PostCommand(ecClipboardPaste);
end;

procedure TJvCustomWideEditor.CursorChanged;
var
  P: TPoint;
begin
  inherited CursorChanged;
  GetCursorPos(P);
  P := ScreenToClient(P);
  if (P.X < GutterWidth) and (Cursor = crIBeam) then
    SetCursor(Screen.Cursors[crArrow]);
end;

procedure TJvCustomWideEditor.FontChanged;
begin
  inherited FontChanged;
  if HandleAllocated then
    UpdateEditorSize;
end;

{############## Handle otrisovkoj [translated] ###############}

{************** Otrisovka [translated] ***************}

{
function IsRectEmpty(R: TRect): Boolean;
begin
  Result := (R.Top = R.Bottom) and (R.Left = R.Right);
end;
}

function TJvCustomWideEditor.CalcCellRect(X, Y: Integer): TRect;
begin
  Result := Bounds(
    EditorClient.Left + X * FCellRect.Width + 1,
    EditorClient.Top + Y * FCellRect.Height,
    FCellRect.Width,
    FCellRect.Height)
end;

procedure TJvCustomWideEditor.Paint;
var
  I: Integer;
  ECR: TRect;
  BX, EX, BY, EY: Integer;
begin
  if FUpdateLock > 0 then
    Exit;
//  FAllRepaint := True; { no optimized painting }
  { It is optimized - otrisovyvayetsya only necessary part  [translated] }
  PaintCaret(False);

  ECR := EditorClient.Canvas.ClipRect;
  OffsetRect(ECR, -FGutterWidth, 0);
  if FAllRepaint then
    ECR := EditorClient.BoundsRect;
  BX := ECR.Left div FCellRect.Width - 1;
  EX := ECR.Right div FCellRect.Width + 1;
  BY := ECR.Top div FCellRect.Height;
  EY := ECR.Bottom div FCellRect.Height + 1;
  for I := BY to EY do
    PaintLine(FTopRow + I, FLeftCol + BX, FLeftCol + EX);

  PaintCaret(True);
  FGutter.Paint;
  FAllRepaint := False;
end;

procedure TJvCustomWideEditor.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TJvCustomWideEditor.EndUpdate;
begin
  Assert(FUpdateLock > 0); { Error }
  Dec(FUpdateLock);
  if FUpdateLock = 0 then
  begin
    FAllRepaint := True;
    UpdateEditorSize;
    StatusChanged;
    Invalidate;
  end;
end;

procedure TJvCustomWideEditor.UpdateEditorSize;
const
  BiggestSymbol = 'W';
var
  I: Integer;
  //Wi, Ai: Integer;
begin
  if csLoading in ComponentState then
    Exit;
  EditorClient.Canvas.Font := Font;
  FontCacheClear; // clear font cache
  FCellRect.Height := EditorClient.Canvas.TextHeightW(BiggestSymbol) + 1;

  // workaround the bug in Windows-9x
  // fixed by Dmitry Rubinstain
  FCellRect.Width := EditorClient.Canvas.TextWidthW(BiggestSymbol + BiggestSymbol) div 2;

  //Ai := EditorClient.Canvas.TextWidthW('W');
  //EditorClient.Canvas.Font.Style := [fsBold];
  //Wi := EditorClient.Canvas.TextWidthW('w');
  //FCellRect.Width := (Wi+Ai) div 2;

  for I := 0 to 1024 do
    MyDi[I] := FCellRect.Width;

  FVisibleColCount := Trunc(EditorClient.ClientWidth / FCellRect.Width);
  FVisibleRowCount := Trunc(EditorClient.ClientHeight / FCellRect.Height);
  FLastVisibleCol := FLeftCol + FVisibleColCount - 1;
  FLastVisibleRow := FTopRow + FVisibleRowCount - 1;
  Rows := FLines.Count;
  Cols := Max_X_Scroll;
  scbHorz.Page := FVisibleColCount;
  scbVert.Page := FVisibleRowCount;
  scbHorz.LargeChange := Max(FVisibleColCount, 1);
  scbVert.LargeChange := Max(FVisibleRowCount, 1);
  scbVert.Max := Max(1, FRows - 1 + FVisibleRowCount - 1);
  FGutter.Invalidate;
end;

function TJvCustomWideEditor.FontCacheFind(LA: TLineAttr): TFont;
{ find the font resource for LA }
var i: Integer;
begin
 // find the font instance
  for i := 0 to FFontCache.Count - 1 do
  begin
    Result := TFont(FFontCache.Items[i]);
    if (Result.Style = LA.Style) and (Result.Color = LA.FC) then begin
{       if (Result.Size <> EditorClient.Canvas.Font.Size) or // other font-attributes has changed
          (CompareText(Result.Name, EditorClient.Canvas.Font.Name) <> 0) then
        begin
          Result.Free;
          FFontCache.Delete(i);
          Break; // create a new font instance

          **************************************************
          *** handled by FontChanged -> UpdateEditorSize ***
          **************************************************
       end;}
       Exit;
    end;
  end;
 // create a new font instance
  Result := TFont.Create;
  Result.Assign(EditorClient.Canvas.Font); // copy default font
  Result.Style := LA.Style;
  Result.Color := LA.FC;
  FFontCache.Add(Result); { store in FontCache }
end;

procedure TJvCustomWideEditor.FontCacheClear;
{ clear the font resource cache }
var i: Integer;
begin
  for i := 0 to FFontCache.Count - 1 do
    TFont(FFontCache.Items[i]).Free;
  FFontCache.Clear;
end;

procedure TJvCustomWideEditor.PaintSelection;
var
  I: Integer;
begin
  for I := FUpdateSelBegY to FUpdateSelEndY do
    PaintLine(I, -1, -1);
end;

procedure TJvCustomWideEditor.PaintLine(Line: Integer; ColBeg, ColEnd: Integer);
var
  Ch: WideString;
  R: TRect;
  i, iC, jC, SL, MX: Integer;
  S: WideString;
  LA: TLineAttr;
  jCStart: integer;
begin
  if (Line < FTopRow) or (Line > FTopRow + FVisibleRowCount) then
    Exit;
  if ColBeg < FLeftCol then
    ColBeg := FLeftCol;
  if (ColEnd < 0) or (ColEnd > FLeftCol + FVisibleColCount) then
    ColEnd := FLeftCol + FVisibleColCount;
  ColEnd := Min(ColEnd, Max_X - 1);
  i := ColBeg;
  if (Line > -1) and (Line < FLines.Count) {and (Length(FLines[Line]) > 0)} then
    with EditorClient do
    begin
      S := FLines[Line];
      GetLineAttr(S, Line, ColBeg, ColEnd);

      {left line}
      if Canvas.Brush.Color <> LineAttrs[FLeftCol+1].BC then // change GDI object only if necessary
        Canvas.Brush.Color := LineAttrs[FLeftCol + 1].BC;
      Canvas.FillRect(Bounds(EditorClient.Left, (Line - FTopRow) *
        FCellRect.Height, 1, FCellRect.Height));

      {optimized, paint group of chars with identical attributes}
      SL := Length(S);
      { if SL > ColEnd then
         MX := ColEnd
       else
         MX := SL; }
      MX := ColEnd;

      i := ColBeg;
      while i < MX do
        with Canvas do
        begin
          iC := i + 1;
          LA := LineAttrs[iC];
          jC := iC + 1;
          if iC <= SL then
            Ch := S[iC]
          else
            Ch := ' ';
          jCStart := jC;
          while (jC <= MX + 1) and
            CompareMem(@LA, @LineAttrs[jC], SizeOf(LineAttrs[1])) do Inc(jC);
          Ch := Copy(S, jCStart - 1, jC - jCStart + 1);
          if jC > SL then Ch := Ch + SpacesW(jC - SL);

          if Brush.Color <> LA.BC then // change GDI object only if necessary
            Brush.Color := LA.BC;
          Font.Assign(FontCacheFind(LA));

          R := CalcCellRect(i - FLeftCol, Line - FTopRow);
          {bottom line}
          FillRect(Bounds(R.Left, R.Bottom - 1, FCellRect.Width * Length(Ch), 1));

          if (i = ColBeg) and (i < SL) then
          begin
            R.Right := R.Left + FCellRect.Width * Length(Ch);
            Ch := S[i] + Ch;
            TJvUnicodeCanvas(Canvas).TextRectW(R, R.Left - FCellRect.Width, R.Top, Ch);
          end
          else
            TJvUnicodeCanvas(Canvas).ExtTextOutW(R.Left, R.Top, [etoOpaque, etoClipped], nil, Ch, @MyDi[0]);
//            Windows.ExtTextOutW(Canvas.Handle, R.Left, R.Top, 0, nil, PWideChar(Ch), Length(Ch), @MyDi[0]);
          i := jC - 1;
        end;
    end
  else
  begin
    EditorClient.Canvas.Brush.Color := Color;
    EditorClient.Canvas.FillRect(Bounds(EditorClient.Left, (Line - FTopRow) *
      FCellRect.Height, 1, FCellRect.Height));
  end;
  {right part}
  R := Bounds(CalcCellRect(i - FLeftCol, Line - FTopRow).Left,
    (Line - FTopRow) * FCellRect.Height,
    (FLeftCol + FVisibleColCount - i + 2) * FCellRect.Width,
    FCellRect.Height);
  {if the line is selected, paint right empty space with selected background}
  if FSelection.IsSelected and (FSelection.SelBlockFormat in [bfInclusive, bfLine, bfNonInclusive]) and
    (Line >= FSelection.SelBegY) and (Line < FSelection.SelEndY) then
    EditorClient.Canvas.Brush.Color := FclSelectBC
  else
    EditorClient.Canvas.Brush.Color := Color;
  EditorClient.Canvas.FillRect(R);
  DrawRightMargin;
end;

procedure TJvCustomWideEditor.GetLineAttr(var Str: WideString; Line, ColBeg, ColEnd: Integer);
var
  I: Integer;
  S: WideString;

  procedure ChangeSelectedAttr;

    procedure DoChange(const iBeg, iEnd: Integer);
    var
      I: Integer;
    begin
      for I := iBeg to iEnd do
      begin
        LineAttrs[I+1].FC := FclSelectFC;
        LineAttrs[I+1].BC := FclSelectBC;
      end;
    end;

  begin
    with FSelection do
    begin
      if SelBlockFormat = bfColumn then
      begin
        if (Line >= SelBegY) and (Line <= SelEndY) then
          DoChange(SelBegX, SelEndX - 1 + Integer(1 {always Inclusive}))
      end
      else
      begin
        if (Line = SelBegY) and (Line = SelEndY) then
          DoChange(SelBegX, SelEndX - 1 + Integer(SelBlockFormat = bfInclusive))
        else
        begin
          if Line = SelBegY then
            DoChange(SelBegX, LeftCol + SelBegX + FVisibleColCount);
          if (Line > SelBegY) and (Line < SelEndY) then
            DoChange(ColBeg, ColEnd);
          if Line = SelEndY then
            DoChange(ColBeg, SelEndX - 1 + Integer(SelBlockFormat = bfInclusive));
        end;
      end;
    end;
  end;

begin
  if ColBeg < 0 then ColBeg := 0;
  if ColEnd > Max_X then ColEnd := Max_X;
  LineAttrs[ColBeg].FC := Font.Color;
  LineAttrs[ColBeg].Style := Font.Style;
  LineAttrs[ColBeg].BC := Color;
  for I := ColBeg + 1 to ColEnd do
    Move(LineAttrs[ColBeg], LineAttrs[I], SizeOf(LineAttrs[1]));
  S := FLines[Line];
  GetAttr(Line, ColBeg, ColEnd);
  if Assigned(FOnGetLineAttr) then
    FOnGetLineAttr(Self, S, Line, LineAttrs);
  if FSelection.IsSelected then
    ChangeSelectedAttr; { we change the attributes of the chosen block [translated] }
  ChangeAttr(Line, ColBeg, ColEnd);
end;

procedure TJvCustomWideEditor.GetAttr(Line, ColBeg, ColEnd: Integer);
begin
end;

procedure TJvCustomWideEditor.ChangeAttr(Line, ColBeg, ColEnd: Integer);
begin
end;

procedure TJvCustomWideEditor.DrawRightMargin;
var
  F: Integer;
begin
  if FRightMarginVisible and (FRightMargin > FLeftCol) and
    (FRightMargin < FLastVisibleCol + 3) then
    with EditorClient.Canvas do
    begin
      { we paint RightMargin Line [translated] }
      Pen.Color := FRightMarginColor;
      F := CalcCellRect(FRightMargin - FLeftCol, 0).Left;
      MoveTo(F, EditorClient.Top);
      LineTo(F, EditorClient.Top + EditorClient.Height);
    end;
end;

function TJvCustomWideEditor.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  {$IFDEF VisualCLX} const {$ENDIF} MousePos: TPoint): Boolean;
var
  WheelDirection: Integer;
begin
  Result := True;
  if ssShift in Shift then
  begin
   // Shift+Wheel: move caret up and down
    if WheelDelta > 0 then
      Command(ecUp)
    else
      Command(ecDown);
  end
  else
  if ssCtrl in Shift then
  begin
    if WheelDelta < 0 then WheelDirection := -1 else WheelDirection := 1;
   // Ctrl+Wheel: scrollbar large change
    scbVert.Position := scbVert.Position - WheelDirection * scbVert.LargeChange;
    Scroll(True, scbVert.Position);
  end
  else
  if Shift = [] then
  begin
    scbVert.Position := scbVert.Position - WheelDelta div 40;
    Scroll(True, scbVert.Position);
  end;
end;

{$IFDEF VisualCLX}

function TJvCustomWideEditor.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
begin
{  case QEvent_type(Event) of
    QEventType_
  end;}
  Result := inherited EventFilter(Sender, Event);
end;

{$ENDIF VisualCLX}

procedure TJvCustomWideEditor.ScrollBarScroll(Sender: TObject; ScrollCode:
  TScrollCode; var ScrollPos: Integer);
begin
  case ScrollCode of
    scLineUp..scPageDown, {scPosition,} scTrack {, scEndScroll}:
      if Sender = scbVert then
        Scroll(True, ScrollPos)
      else
      if Sender = scbHorz then
        Scroll(False, ScrollPos);
  end;
end;

procedure TJvCustomWideEditor.Scroll(Vert: Boolean; ScrollPos: Integer);
var
  R, RClip, RUpdate: TRect;
  OldFTopRow: Integer;
  OldFLeftCol: Integer;
begin
  if FUpdateLock = 0 then
  begin
    PaintCaret(False);
    if Vert then
    begin {Vertical Scroll}
      { it is optimized [translated] }
      OldFTopRow := FTopRow;
      FTopRow := ScrollPos;
      {$IFDEF VCL}
      if Abs((OldFTopRow - ScrollPos) * FCellRect.Height) < EditorClient.Height
        then
      begin
        R := EditorClient.ClientRect;
        R.Bottom := R.Top + CellRect.Height * (FVisibleRowCount + 1); {??}
        R.Left := 0; // update gutter
        RClip := R;
        ScrollDC(
          EditorClient.Canvas.Handle, // handle of device context
          0, // horizontal scroll units
          (OldFTopRow - ScrollPos) * FCellRect.Height, // vertical scroll units
          R, // address of structure for scrolling rectangle
          RClip, // address of structure for clipping rectangle
          0, // handle of scrolling region
          @RUpdate // address of structure for update rectangle
          );
        InvalidateRect(Handle, @RUpdate, False);
      end
      else
      {$ENDIF VCL}
        Invalidate;
      Update;
    end
    else {Horizontal Scroll}
    begin
      { it is not optimized [translated] }
      OldFLeftCol := FLeftCol;
      FLeftCol := ScrollPos;
      {$IFDEF VCL}
      if Abs((OldFLeftCol - ScrollPos) * FCellRect.Width) < EditorClient.Width then
      begin
        R := EditorClient.ClientRect;
        R.Right := R.Left + CellRect.Width * (FVisibleColCount + 1); {??}
        RClip := R;
        ScrollDC(
          EditorClient.Canvas.Handle, // handle of device context
          (OldFLeftCol - ScrollPos) * FCellRect.Width, // horizontal scroll units
          0, // vertical scroll units
          R, // address of structure for scrolling rectangle
          RClip, // address of structure for clipping rectangle
          0, // handle of scrolling region
          @RUpdate // address of structure for update rectangle
          );
        Inc(RUpdate.Right, FCellRect.Width); // draw italic chars correctly
        InvalidateRect(Handle, @RUpdate, False);
      end
      else
      {$ENDIF VCL}
        Invalidate;
      Update;
    end;
  end
  else { FUpdateLock > 0 }
  begin
    if Vert then
      FTopRow := ScrollPos
    else
      FLeftCol := ScrollPos;
  end;
  FLastVisibleRow := FTopRow + FVisibleRowCount - 1;
  FLastVisibleCol := FLeftCol + FVisibleColCount - 1;
  if FUpdateLock = 0 then
  begin
    DrawRightMargin;
    PaintCaret(True);
  end;
  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

procedure TJvCustomWideEditor.PaintCaret(bShow: Boolean);
var
  R: TRect;
begin
  if FHideCaret then
    Exit;
  if not bShow then
    Windows.HideCaret(Handle)
  else
  if Focused then
  begin
    R := CalcCellRect(FCaretX - FLeftCol, FCaretY - FTopRow);
    if (R.Left >= 0) and (R.Left >= FGutterWidth) then
      SetCaretPos(R.Left - 1, R.Top + 1)
    else
      SetCaretPos(-MAXSHORT, -MAXSHORT); // hide caret without Windows.HideCaret
    ShowCaret(Handle);
  end
end;

procedure TJvCustomWideEditor.SetCaretInternal(X, Y: Integer);
var
  R: TRect;
begin
  if (X = FCaretX) and (Y = FCaretY) then
    Exit;
  // To scroll the image [translated]
  if not FCursorBeyondEOF then
    Y := Min(Y, FLines.Count - 1);
  Y := Max(Y, 0);
  X := Min(X, Max_X);
  X := Max(X, 0);
  if Y < FTopRow then
    SetLeftTop(FLeftCol, Y)
  else
  if Y > Max(FLastVisibleRow, 0) then
    SetLeftTop(FLeftCol, Y - FVisibleRowCount + 1);
  if X < 0 then
    X := 0;
  if X < FLeftCol then
    SetLeftTop(X, FTopRow)
  else
  if X > FLastVisibleCol then
    SetLeftTop(X - FVisibleColCount + 1, FTopRow);

  if Focused then {mac: do not move Caret when not focused!}
  begin
    R := CalcCellRect(X - FLeftCol, Y - FTopRow);
    SetCaretPos(R.Left - 1, R.Top + 1);
  end;

  if Assigned(FOnChangeStatus) and ((FCaretX <> X) or (FCaretY <> Y)) then
  begin
    FCaretX := X;
    FCaretY := Y;
    FOnChangeStatus(Self);
  end;
  FCaretX := X;
  FCaretY := Y;
end;

procedure TJvCustomWideEditor.SetCaret(X, Y: Integer);
begin
  if (X = FCaretX) and (Y = FCaretY) then
    Exit;
  {--- UNDO ---}
  TJvCaretUndo.Create(Self, FCaretX, FCaretY);
  {--- /UNDO ---}
  SetCaretInternal(X, Y);
  if FUpdateLock = 0 then
    StatusChanged;
end;

procedure TJvCustomWideEditor.SetCaretPosition(Index, Pos: Integer);
begin
  if Index = 0 then
    SetCaret(Pos, FCaretY)
  else
    SetCaret(FCaretX, Pos);

 // persistent blocks:
  if FSelection.IsSelected then
  begin
    with FSelection do
      if ((FCaretX < SelBegX) and (CaretY <= SelBegY)) or
         ((FCaretX >= SelEndX) and (CaretY >= SelEndY)) then
        FPersistentBlocksCaretChanged := True;
  end;
end;

procedure TJvCustomWideEditor.SetUnSelected;
begin
  if FSelection.IsSelected then
  begin
    FSelection.IsSelected := False;
    FSelection.Selecting := False;
    {--- UNDO ---}
    TJvUnselectUndo.Create(Self, FCaretX, FCaretY);
    {--- /UNDO ---}
    PaintSelection;
  end;
end;

procedure TJvCustomWideEditor.KeyDown(var Key: Word; Shift: TShiftState);
var
  Com: Word;
begin
  if FCompletion.FVisible then
  begin
    if FCompletion.DoKeyDown(Key, Shift) then
    begin
      IgnoreKeyPress := true;
      Exit;
    end;
  end
  else
    FCompletion.FTimer.Enabled := False;

  if WaitSecondKey then
  begin
    IgnoreKeyPress := True; { Set this before calling FKeyboard.Command2()
                              because in FKeyboard.OnCommand2 the
                              Editor-window can loose focus and the
                              second char is printed. }
    Com := FKeyboard.Command2(Key1, Shift1, Key, Shift);
    WaitSecondKey := False;
    IgnoreKeyPress := True;
  end
  else
  begin
    inherited KeyDown(Key, Shift);
    Key1 := Key;
    Shift1 := Shift;
    Com := FKeyboard.Command(Key, Shift);
    if Com = twoKeyCommand then
    begin
      IgnoreKeyPress := True;
      WaitSecondKey := True;
    end
    else
      IgnoreKeyPress := Com > 0;
  end;

  if (Com > 0) and (Com <> twoKeyCommand) then
  begin
    Command(Com);
    Key := 0;
  end;

  if (Com = ecBackSpace) then
    FCompletion.DoKeyPress(#8);
end;

procedure TJvCustomWideEditor.ReLine;
begin
  FLines.ReLine;
end;

procedure TJvCustomWideEditor.KeyPress(var Key: WideChar);
var Ch: Char;
begin
  if (IgnoreKeyPress) or (FReadOnly) then
  begin
    IgnoreKeyPress := False;
    Exit;
  end;

  PaintCaret(False);
  try
    if Assigned(OnKeyPress) then
    begin
      Ch := Char(Key);
      OnKeyPress(Self, Ch);
      Key := WideChar(Ch);
    end;
//    inherited KeyPress(Key);
    Command(Ord(Key));
  finally
    PaintCaret(True);
  end;
end;

procedure TJvCustomWideEditor.AdjustPersistentBlockSelection(X, Y: Integer;
  Mode: TAdjustPersistentBlockMode; Args: array of Integer);
begin
 // persistent blocks: adjust selection
  if (not FPersistentBlocks) or (not FSelection.IsSelected) then Exit;

  if (FSelection.SelBlockFormat = bfColumn) and
     (not (Mode in [amDeleteLine, amLineConcat, amLineBreak])) then Exit; 

  with FSelection do
  begin
    case Mode of
      amInsert: // X=InsertCaretX, Y=InsertCaretY, Args[0]=char count
        begin
          if (Y = SelBegY) and (X <= SelBegX) then
            Inc(SelBegX, Args[0]);
          if (Y = SelEndY) and (X < SelEndX) then
            Inc(SelEndX, Args[0]);
        end;
      amDelete: // X=InsertCaretX, Y=InsertCaretY, Args[0]=char count
        begin
          if (Y = SelBegY) and (X <= SelBegX) then
            Dec(SelBegX, Args[0]);
          if (Y = SelEndY) and (X <= SelEndX) then
            Dec(SelEndX, Args[0]);
        end;
      amDeleteLine: // Y=line to delete
        begin
          // one line selection
          if (Y = SelBegY) and (SelEndY = SelBegY) then
          begin
            IsSelected := False;
            Selecting := False;
          end
          else
          begin
            if Y < SelBegY then
              Dec(SelBegY);
            if Y < SelEndY then
              Dec(SelEndY);
          end;
        end;
      amLineBreak: // X=BreakCaretX, Y=BreakCaretY
        begin
          if (Y < SelBegY) then
          begin
           // move down
            Inc(SelBegY);
            Inc(SelEndY);
          end
          else if (Y <= SelEndY) then
          begin
            if (Y = SelBegY) and (X <= SelBegX) then
            begin
             // LineBreak in the first line
              Dec(SelBegX, X);
              if (SelBegY = SelEndY) and (SelBlockFormat <> bfColumn) then // one line selection
                Dec(SelEndX, X);
              Inc(SelBegY);
              Inc(SelEndY);
            end
            else if (Y < SelEndY) then
            begin
              // LineBreak in selection
              Inc(SelEndY);
            end
            else if {(Y = SelEndY) and} (X < SelEndX) and (SelBlockFormat <> bfColumn) then
            begin
              // LineBreak in the last line
              SelEndX := SelEndX - X;
              Inc(SelEndY);
            end;
          end;
        end;
      amLineConcat: // X=CaretX, Y=CaretY, Args[0]=ConcatCaretX, Args[1]=ConcatCaretY
        begin
          if (Y < SelBegY) then
          begin
           // move up
            Dec(SelBegY);
            Dec(SelEndY);
          end
          else if (Y <= SelEndY) then
          begin
            if (Y = SelBegY) and (X <= SelBegX) then
            begin
             // LineConcat in the first line
              Dec(SelBegX, X - Args[X]);
              if (SelBegY = SelEndY) and (SelBlockFormat <> bfColumn) then // one line selection
                Inc(SelEndX, X - Args[X]);
              Dec(SelBegY);
              Dec(SelEndY);
            end
            else if (Y < SelEndY) then
            begin
              // LineConcat in selection
              Dec(SelEndY);
            end
            else if {(Y = SelEndY) and} (X <= SelEndX) and (SelBlockFormat <> bfColumn) then
            begin
              // LineConcat in the last line
              Inc(SelEndX, Length(FLines[Args[1]]));
              Dec(SelEndY);
            end;
          end;
        end;
    else
      ;
    end; // case

    if SelBegY < 0 then SelBegY := 0;
    if (SelEndY < SelBegY) or (SelBegY >= FLines.Count) then SetUnSelected;
    if SelBegX < 0 then SelBegX := 0;
    if SelEndX > Max_X then SelEndX := Max_X;
    if (SelEndX < SelBegX) and (SelBegY = SelEndY) then SetUnSelected;

   // set update region
    SetSelUpdateRegion(SelBegY, SelEndY);
  end; // with
end;

procedure TJvCustomWideEditor.InsertChar(const Key: WideChar);
var
  S: WideString;
  X, Y, iBeg: Integer;
  WasSelected: Boolean;
begin
  WasSelected := (FSelection.IsSelected) and (not FPersistentBlocks);
  if UnicodeIsPrintable(UCS4(Key)) then
  begin
    if not HasCharW(Key, JvEditorCompletionChars) then
      FCompletion.DoKeyPress(Key);

    RemoveSelectedBlock;

    ReLine; // need ReLine after DeleteSelection
    S := FLines[FCaretY];
    if FInsertMode then
    begin
      {--- UNDO ---}
      TJvInsertUndo.Create(Self, FCaretX, FCaretY, Key);
      {--- /UNDO ---}
      Insert(Key, S, FCaretX + 1);

      AdjustPersistentBlockSelection(FCaretX, FCaretY, amInsert, [1]);
    end
    else
    begin
      {--- UNDO ---}
      if FCaretX + 1 <= Length(S) then
        TJvOverwriteUndo.Create(Self, FCaretX, FCaretY, S[FCaretX + 1], Key)
      else
        TJvOverwriteUndo.Create(Self, FCaretX, FCaretY, '', Key);
      {--- /UNDO ---}
      if FCaretX + 1 <= Length(S) then
        S[FCaretX + 1] := Key
      else
        S := S + Key
    end;
    FLines.Internal[FCaretY] := S;
    SetCaretInternal(FCaretX + 1, FCaretY);
    TextModified(FCaretX, FCaretY, maInsert, Key);
    PaintLine(FCaretY, -1, -1);
    Changed;

    if HasCharW(Key, JvEditorCompletionChars) then
      FCompletion.DoKeyPress(Key);
  end
  else
  case Key of
//    #32..#255:
    #13:
      begin
        if FInsertMode then
        begin
          if WasSelected then // compound only on selection deletion
            BeginCompound;
          Inc(FUpdateLock);
          try
            RemoveSelectedBlock; // adjusts FCaretX, FCaretY
            X := FCaretX;
            Y := FCaretY;
            { --- UNDO --- }
            TJvInsertUndo.Create(Self, FCaretX, FCaretY, sWideLineBreak);
            { --- /UNDO --- }
            if FLines.Count = 0 then
              FLines.Add('');
            ReLine;

            S := Copy(FLines[Y], X + 1, MaxInt);
            FLines.Insert(Y + 1, S);
            FLines.Internal[Y] := Copy(FLines[Y], 1, X);
            Inc(Y);
            { auto indent }
            if (FAutoIndent) and
              (((Length(FLines[FCaretY]) > 0) and
              (FLines[FCaretY][1] = ' ')) or
              ((TrimW(FLines[FCaretY]) = '') and (X > 0))) then
            begin
              X := GetAutoIndentStop(Y);
              if X > 0 then
              begin
                { --- UNDO --- }
                TJvInsertUndo.Create(Self, 0, Y, SpacesW(X));
                { --- /UNDO --- }
                FLines.Internal[Y] := SpacesW(X) + FLines[Y];
              end;
            end
            else
              X := 0;

           // persistent blocks: adjust selection
            AdjustPersistentBlockSelection(FCaretX, FCaretY, amLineBreak, []);

            UpdateEditorSize;
            TextModified(FCaretX - 1, FCaretY, maInsert, sWideLineBreak);
          finally
            Dec(FUpdateLock);
            if WasSelected then
              EndCompound;
          end;
          Invalidate;
          Changed;
        end
        else // Overwrite-mode
        begin
          if WasSelected then // compound only on selection deletion
            BeginCompound;
          try
            RemoveSelectedBlock;
            X := FCaretX;
            Y := FCaretY;
            Inc(Y);
            if Y >= FLines.Count then
            begin
              Inc(FUpdateLock);
              try
                { --- UNDO --- }
                TJvInsertUndo.Create(Self, FCaretX, FCaretY, sWideLineBreak);
                { --- /UNDO --- }
                FLines.Add('');
              finally
                Dec(FupdateLock);
              end;
              TextModified(0, Y - 1, maInsert, sWideLineBreak);
              UpdateEditorSize;
              Invalidate;
              Changed;
            end;
            if Y < FLines.Count then
            begin
              S := FLines[Y];
              if Length(S) > 0 then
              begin
                iBeg := FindNotBlankCharPosW(S) - 1;
                if iBeg < X then X := iBeg;
              end;
            end;
          finally
            if WasSelected then
              EndCompound;
          end;
        end;
        SetCaretInternal(X, Y);
      end; // #13
  end; // case
end;

procedure TJvCustomWideEditor.PersistentBlocksSetUnSelected;
begin
  FPersistentBlocksCaretChanged := True;
  if not FPersistentBlocks then
    SetUnSelected;
end;

function TJvCustomWideEditor.IsNewSelection: Boolean;
begin
  if FPersistentBlocks then
    Result := (not FSelection.IsSelected) or FPersistentBlocksCaretChanged
  else
    Result := (not FSelection.IsSelected);
end;

procedure TJvCustomWideEditor.Command(ACommand: TEditCommand);
var
  X, Y: Integer;
  CaretUndo: Boolean;

type
  TPr = procedure of object;

  procedure DoAndCorrectXY(Pr: TPr);
  begin
    Pr;
    X := FCaretX;
    Y := FCaretY;
    CaretUndo := False;
  end;

  function Com(const Args: array of TEditCommand): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to High(Args) do
      if Args[I] = ACommand then
      begin
        Result := True;
        Exit;
      end;
    Result := False;
  end;

  procedure SetSel1(X, Y: Integer);
  begin
    SetSel(X, Y);
    CaretUndo := False;
  end;

  procedure SetSelText1(S: WideString);
  begin
    SelText := S;
    CaretUndo := False;
  end;

var
  F: Integer;
  S, S2: WideString;
  B: Boolean;
  iBeg, iEnd: Integer;
  BlockFormat: TSelBlockFormat;
begin
  X := FCaretX;
  Y := FCaretY;
  CaretUndo := True;
  // Inc(FUpdateLock);
  { macro recording }
  if FRecording and not Com([ecRecordMacro, ecBeginCompound]) and (FCompound = 0) then
    FMacro := FMacro + Char(Lo(ACommand)) + Char(Hi(ACommand));
  PaintCaret(False);
  try
    case ACommand of
      { caret movements }
      ecLeft, ecRight, ecSelLeft, ecSelRight:
        begin
          if Com([ecSelLeft, ecSelRight]) and (IsNewSelection) then
            SetSel1(X, Y);
          if Com([ecLeft, ecSelLeft]) then
            Dec(X)
          else
            Inc(X);
          if Com([ecSelLeft, ecSelRight]) then
            SetSel1(X, Y)
          else
            PersistentBlocksSetUnSelected;
        end;
      ecUp, ecDown, ecSelUp, ecSelDown:
        if Com([ecUp, ecSelUp]) or (Y < FRows - 1) or FCursorBeyondEOF then
        begin
          if Com([ecSelUp, ecSelDown]) and IsNewSelection then
            SetSel1(X, Y);
          if Com([ecUp, ecSelUp]) then
            Dec(Y)
          else
            Inc(Y);
          if Com([ecSelUp, ecSelDown]) then
            SetSel1(X, Y)
          else
            PersistentBlocksSetUnSelected;
        end;
      ecSelColumnLeft, ecSelColumnRight, ecSelColumnUp, ecSelColumnDown:
        begin
          FSelection.SelBlockFormat := bfColumn;
          case ACommand of
            ecSelColumnLeft: Command(ecSelLeft);
            ecSelColumnRight: Command(ecSelRight);
            ecSelColumnUp: Command(ecSelUp);
            ecSelColumnDown: Command(ecSelDown);
          end;
          Exit;
        end;
      ecPrevWord, ecSelPrevWord, ecBackspaceWord:
        begin
          if (ACommand = ecSelPrevWord) and IsNewSelection then
            SetSel1(FCaretX, FCaretY);
          S := FLines[Y];
          B := False;
          if FCaretX > Length(s) then
          begin
            X := Length(s);
            SetSel1(X, Y);
          end
          else
          begin
            for F := X - 1 downto 0 do
              if B then
              begin
                if CharInSetW(S[F + 1], Separators) then
                begin
                  X := F + 1;
                  Break;
                end;
              end
              else
              if not CharInSetW(S[F + 1], Separators) then
                B := True;
            if X = FCaretX then
              X := 0;
            if ACommand = ecSelPrevWord then
              SetSel1(X, Y)
            else
              PersistentBlocksSetUnSelected;

            if (ACommand = ecBackspaceWord) and (Y >= 0) and (X <> FCaretX) then
            begin
              if not FReadOnly then
              begin
                BeginCompound;
                try
                  SelectRange(X, FCaretY, FCaretX, FCaretY);
                  DeleteSelected;
                finally
                  EndCompound;
                end;
                ReLine;
              end;
            end;
          end;
        end;
      ecNextWord, ecSelNextWord:
        begin
          if (ACommand = ecSelNextWord) and IsNewSelection then
            SetSel1(FCaretX, FCaretY);
          if Y >= FLines.Count then
          begin
            Y := FLines.Count - 1;
            X := Length(FLines[Y]);
          end;
          S := FLines[Y];
          B := False;
          if FCaretX >= Length(S) then
          begin
            if Y < FLines.Count - 1 then
            begin
              Y := FCaretY + 1;
              X := 0;
              if ACommand = ecSelNextWord then // this code is copied from [ecPrevWord, ecSelPrevWord]
                SetSel1(X, Y)
              else
                PersistentBlocksSetUnSelected;
            end;
          end
          else
          begin
            for F := X to Length(S) - 1 do
              if B then
              begin
                if not CharInSetW(S[F + 1], Separators) then
                begin
                  X := F;
                  Break;
                end
              end
              else
              if CharInSetW(S[F + 1], Separators) then
                B := True;
            if X = FCaretX then
              X := Length(S);
            if ACommand = ecSelNextWord then
              SetSel1(X, Y)
            else
              PersistentBlocksSetUnSelected;
          end;
        end;
      ecScrollLineUp, ecScrollLineDown:
        begin
          if not ((ACommand = ecScrollLineDown) and
            (Y >= FLines.Count - 1) and (Y = FTopRow)) then
          begin
            if ACommand = ecScrollLineUp then
              F := -1
            else
              F := 1;
            scbVert.Position := scbVert.Position + F;
            Scroll(True, scbVert.Position);
          end;
          if Y < FTopRow then
            Y := FTopRow
          else
          if Y > FLastVisibleRow then
            Y := FLastVisibleRow;
        end;
      ecBeginLine, ecSelBeginLine, ecBeginDoc, ecSelBeginDoc,
        ecEndLine, ecSelEndLine, ecEndDoc, ecSelEndDoc:
        begin
          if Com([ecSelBeginLine, ecSelBeginDoc, ecSelEndLine, ecSelEndDoc])
            and IsNewSelection then
            SetSel1(FCaretX, Y);
          if Com([ecBeginLine, ecSelBeginLine]) then
            X := 0
          else
          if Com([ecBeginDoc, ecSelBeginDoc]) then
          begin
            X := 0;
            Y := 0;
            SetLeftTop(0, 0);
          end
          else
          if Com([ecEndLine, ecSelEndLine]) then
            if Cardinal(Y) < Cardinal(FLines.Count) then
              X := Length(FLines[Y])
            else
              X := 0
          else
          if Com([ecEndDoc, ecSelEndDoc]) then
          begin
            Y := FLines.Count - 1;
            if Y >= 0 then
            begin
              X := Length(FLines[Y]);
              SetLeftTop(X - FVisibleColCount, Y - FVisibleRowCount div 2);
            end;
          end;
          if Com([ecSelBeginLine, ecSelBeginDoc, ecSelEndLine, ecSelEndDoc]) then
            SetSel1(X, Y)
          else
            PersistentBlocksSetUnSelected;
        end;
      ecPrevPage:
        begin
          scbVert.Position := scbVert.Position - scbVert.LargeChange;
          Scroll(True, scbVert.Position);
          Y := Y - FVisibleRowCount;
          PersistentBlocksSetUnSelected;
        end;
      ecNextPage:
        begin
          scbVert.Position := scbVert.Position + scbVert.LargeChange;
          Scroll(True, scbVert.Position);
          Y := Y + FVisibleRowCount;
          PersistentBlocksSetUnSelected;
        end;
      ecSelPrevPage:
        begin
          BeginUpdate;
          SetSel1(X, Y);
          scbVert.Position := scbVert.Position - scbVert.LargeChange;
          Scroll(True, scbVert.Position);
          Y := Y - FVisibleRowCount;
          SetSel1(X, Y);
          EndUpdate;
        end;
      ecSelNextPage:
        begin
          BeginUpdate;
          SetSel1(X, Y);
          scbVert.Position := scbVert.Position + scbVert.LargeChange;
          Scroll(True, scbVert.Position);
          Y := Y + FVisibleRowCount;
          if Y <= FLines.Count - 1 then
            SetSel1(X, Y)
          else
            SetSel1(X, FLines.Count - 1);
          EndUpdate;
        end;
      ecSelWord:
        if IsNewSelection and (GetWordOnPosExW(FLines[Y] + ' ', X + 1, iBeg,
          iEnd) <> '') then 
        begin
          SetSel1(iBeg - 1, Y);
          SetSel1(iEnd - 1, Y);
          X := iEnd - 1;
        end;
      ecWindowTop:
        Y := FTopRow;
      ecWindowBottom:
        Y := FTopRow + FVisibleRowCount - 1;
      { editing }
      ecCharFirst..ecCharLast:
        if not FReadOnly then
        begin
          InsertChar(WideChar(ACommand - ecCharFirst));
          Exit;
        end;
      ecInsertPara:
        if not FReadOnly then
        begin
          InsertChar(#13);
          Exit;
        end
        else
        if Y < FLines.Count - 1 then
        begin
          Inc(Y);
          S := FLines[Y];
          if Length(S) > 0 then
          begin
            iBeg := FindNotBlankCharPosW(S) - 1;
            if iBeg < X then X := iBeg;
          end;
        end;
      ecBackspace:
        if not FReadOnly then
          if X > 0 then
          begin
            //{ in the middle of line - в середине строки }
            if (not FPersistentBlocks) and (FSelection.IsSelected) then
              DoAndCorrectXY(RemoveSelectedBlock)
            else
            begin
              ReLine;
              if FBackSpaceUnindents then
                X := GetBackStop(FCaretX, FCaretY)
              else
                X := FCaretX - 1;

              S := Copy(FLines[FCaretY], X + 1, FCaretX - X);

              { --- UNDO --- }
              if X = FCaretX - 1 then
                TJvBackspaceUndo.Create(Self, FCaretX, FCaretY, S)
              else
                TJvBackspaceUnindentUndo.Create(Self, FCaretX, FCaretY, S);
              CaretUndo := False;
              { --- /UNDO --- }

             // persistent blocks: adjust selection
              AdjustPersistentBlockSelection(FCaretX, FCaretY, amDelete, [1]);

              FLines.DeleteText(X, Y, FCaretX - 1, Y);

              TextModified(FCaretX, FCaretY, maDelete, S);
              PaintLine(Y, -1, -1);
            end;
            Changed;
          end
          else
          if Y > 0 then
          begin
            Inc(FUpdateLock);
            try
              //{ at begin of line - в начале строки}
              RemoveSelectedBlock;
              ReLine;

              X := Length(FLines[Y - 1]);

              { --- UNDO --- }
              TJvBackspaceUndo.Create(Self, X + 1, FCaretY - 1, #10);
              CaretUndo := False;
              { --- /UNDO --- }

             // persistent blocks: adjust selection
              AdjustPersistentBlockSelection(FCaretX, FCaretY, amLineConcat, [X, FCaretY - 1]);

              FLines.DeleteText(X, Y - 1, -1, Y);
              Dec(Y);
            finally
              Dec(FUpdateLock);
            end;
            UpdateEditorSize;
            TextModified(X, Y, maDelete, sWideLineBreak);
            Invalidate;
            Changed;
          end;
      ecDelete:
        if not FReadOnly then
        begin
          Inc(FUpdateLock);
          try
            if FLines.Count = 0 then
              FLines.Add('');
          finally
            Dec(FUpdateLock);
          end;
          if (not FPersistentBlocks) and (FSelection.IsSelected) then
            DoAndCorrectXY(RemoveSelectedBlock)
          else
          if X < Length(FLines[Y]) then
          begin
            //{ inside line - в середине строки}
            { --- UNDO --- }
            TJvDeleteUndo.Create(Self, FCaretX, FCaretY, FLines[Y][X + 1]);
            CaretUndo := False;
            { --- /UNDO --- }

            // persistent blocks: adjust selection (before DeleteText)
            AdjustPersistentBlockSelection(FCaretX + 1, FCaretY, amDelete, [1]);

            S := FLines[Y][X + 1];
            FLines.DeleteText(X, Y, X, Y);

            TextModified(FCaretX, FCaretY, maDelete, S);
            PaintLine(FCaretY, -1, -1);
            Changed;
          end
          else
          if (Y >= 0) and (Y <= FLines.Count - 2) then
          begin
            //{ at the end of line - в конце строки}
            { --- UNDO --- }
            TJvDeleteUndo.Create(Self, FCaretX, FCaretY, sWideLineBreak);
            CaretUndo := False;
            { --- /UNDO --- }
            // persistent blocks: adjust selection (before DeleteText)
            AdjustPersistentBlockSelection(0, FCaretY + 1, amLineConcat, [FCaretX, FCaretY]);

            FLines.DeleteText(X, Y, -1, Y + 1);

            UpdateEditorSize;
            TextModified(FCaretX, FCaretY, maDelete, sWideLineBreak);
            Invalidate;
            Changed;
          end;
        end;
      ecTab, ecBackTab:
        begin
          X := GetTabStop(FCaretX, FCaretY, ACommand = ecTab);
          if not FReadOnly then
          begin
            if FSelection.IsSelected then
              if (ACommand = ecTab) and (FInsertMode) then
                DeleteSelected;
            ReLine;
            if (ACommand = ecTab) and FInsertMode then
            begin
              S := FLines[FCaretY];
              S2 := SpacesW(X - FCaretX);
              { --- UNDO --- }
              TJvInsertTabUndo.Create(Self, FCaretX, FCaretY, S2);
              CaretUndo := False;
              { --- /UNDO --- }
              FLines.InsertText(FCaretX, FCaretY, S2);

              TextModified(FCaretX, FCaretY, maInsert, S2);
              PaintLine(FCaretY, -1, -1);
              Changed;
            end;
          end;
          { else }
          { move cursor - oh yes!, it's already moved: X := GetTabStop(..); }
        end;
      ecIndent:
        if not FReadOnly and FSelection.IsSelected then
        begin
          if FSelection.SelBlockFormat = bfColumn then
            IndentColumns(FSelection.SelBegX, FSelection.SelBegY, FSelection.SelEndY)
          else
            IndentSelLines(False);
          Exit;
        end;
      ecUnIndent:
        if not FReadOnly and FSelection.IsSelected then
        begin
          if FSelection.SelBlockFormat = bfColumn then
            UnIndentColumns(FSelection.SelBegX, FSelection.SelBegY, FSelection.SelEndY)
          else
            IndentSelLines(True);
          Exit;
        end;
      ecChangeInsertMode:
        begin
          FInsertMode := not FInsertMode;
          StatusChanged;
        end;
      ecInclusiveBlock..ecNonInclusiveBlock:
        begin
          if FSelection.SelBlockFormat = TSelBlockFormat(ACommand - ecInclusiveBlock) then
            Exit;

          if FSelection.IsSelected then
          begin
           // convert line block to others and visi versa
            if ACommand <> ecLineBlock then
            begin
              if (FSelection.SelBlockFormat = bfLine) then
                AdjustSelLineMode({Restore:=}True);
            end
            else
              AdjustSelLineMode({Restore:=}False);
          end;

          FSelection.SelBlockFormat := TSelBlockFormat(ACommand - ecInclusiveBlock);
          PaintSelection;
          StatusChanged;
        end;
      ecClipboardCut:
        if not FReadOnly then
          DoAndCorrectXY(ClipboardCut);
      ecClipboardCopy:
        ClipboardCopy;
      ecClipboardPaste:
        if not FReadOnly then
          DoAndCorrectXY(ClipboardPaste);
      ecDeleteSelected:
        if not FReadOnly and FSelection.IsSelected then
          DoAndCorrectXY(DeleteSelected);
      ecDeleteWord:
        if not FReadOnly then
        begin
          Command(ecBeginCompound);
          Command(ecBeginUpdate);
          try
            BlockFormat := FSelection.SelBlockFormat;
            FSelection.SelBlockFormat := bfNonInclusive; // no bfLine, bfColumn, bfInclusive
            Command(ecSelNextWord);
            FSelection.SelBlockFormat := BlockFormat;

            Command(ecDeleteSelected);
          finally
            Command(ecEndUpdate);
            Command(ecEndCompound);
          end;
          Exit;
        end;
      ecDeleteLine:
        if not FReadOnly then
        begin
          if (FCaretY >= 0) and (FCaretY < FLines.Count) then
          begin
            S := FLines[FCaretY];
            Inc(FUpdateLock);
            try
              { --- UNDO --- }
              TJvDeleteLineUndo.Create(Self, FCaretX, FCaretY, S);
              { --- /UNDO --- }
              FLines.Delete(FCaretY);
            finally
              Dec(FUpdateLock);
            end;
            AdjustPersistentBlockSelection(FCaretX, FCaretY, amDeleteLine, []);
            TextModified(0, FCaretY, maDelete, S);
            Invalidate;
            Changed;
          end;
          Exit;
        end;
      ecSelAll:
        begin
          SelectAll;
          Exit;
        end;
      ecToUpperCase:
        if not FReadOnly then
          SelText := WideUpperCase(SelText);
      ecToLowerCase:
        if not FReadOnly then
          SelText := WideLowerCase(SelText);
      ecChangeCase:
        if not FReadOnly then
          SelText := WideChangeCase(SelText);
      ecUndo:
        if not FReadOnly then
        begin
          FUndoBuffer.Undo;
          PaintCaret(True);
          Exit;
        end;
      ecRedo:
        if not FReadOnly then
        begin
          FUndoBuffer.Redo;
          PaintCaret(True);
          Exit;
        end;
      ecBeginCompound:
        BeginCompound;
      ecEndCompound:
        EndCompound;
      ecSetBookmark0..ecSetBookmark9:
        ChangeBookMark(ACommand - ecSetBookmark0, True);
      ecGotoBookmark0..ecGotoBookmark9:
        begin
          ChangeBookMark(ACommand - ecGotoBookmark0, False);
          X := FCaretX;
          Y := FCaretY;
        end;
      ecCompletionIdentifiers:
        if not FReadOnly then
        begin
          FCompletion.DoCompletion(cmIdentifiers);
          PaintCaret(True);
          Exit;
        end;
      ecCompletionTemplates:
        if not FReadOnly then
        begin
          FCompletion.DoCompletion(cmTemplates);
          PaintCaret(True);
          Exit;
        end;
      ecBeginUpdate:
        BeginUpdate;
      ecEndUpdate:
        EndUpdate;
      ecRecordMacro:
        if FRecording then
          EndRecord(FDefMacro)
        else
          BeginRecord;
      ecPlayMacro:
        begin
          PlayMacro(FDefMacro);
          Exit;
        end;
    end;

    if CaretUndo then
      SetCaret(X, Y)
    else
      SetCaretInternal(X, Y);
  finally
    // Dec(FUpdateLock);
    PaintCaret(True);
  end;
end;

procedure TJvCustomWideEditor.PostCommand(ACommand: TEditCommand);
begin
  PostMessage(Handle, WM_EDITCOMMAND, ACommand, 0);
end;

procedure TJvCustomWideEditor.WMEditCommand(var Msg: TMessage);
begin
  Command(Msg.WParam);
  Msg.Result := Ord(True);
end;

procedure TJvCustomWideEditor.WMCopy(var Msg: TMessage);
begin
  DoCopy;
  Msg.Result := Ord(True);
end;

procedure TJvCustomWideEditor.WMCut(var Msg: TMessage);
begin
  DoCut;
  Msg.Result := Ord(True);
end;

procedure TJvCustomWideEditor.WMPaste(var Msg: TMessage);
begin
  DoPaste;
  Msg.Result := Ord(True);
end;

procedure TJvCustomWideEditor.ChangeBookMark(BookMark: TBookMarkNum;
  Valid: Boolean);

  procedure SetXY(X, Y: Integer);
  var
    X1, Y1: Integer;
  begin
    X1 := FLeftCol;
    Y1 := FTopRow;
    if (Y < FTopRow) or (Y > FLastVisibleRow) then
      Y1 := Y - (FVisibleRowCount div 2);
    if (X < FLeftCol) or (X > FVisibleColCount) then
      X1 := X - (FVisibleColCount div 2);
    SetLeftTop(X1, Y1);
    SetCaret(X, Y);
  end;

begin
  if Valid then
    if BookMarks[Bookmark].Valid and (BookMarks[Bookmark].Y = FCaretY) then
      BookMarks[Bookmark].Valid := False
    else
    begin
      BookMarks[Bookmark].X := FCaretX;
      BookMarks[Bookmark].Y := FCaretY;
      BookMarks[Bookmark].Valid := True;
    end
  else
  if BookMarks[Bookmark].Valid then
    SetXY(BookMarks[Bookmark].X, BookMarks[Bookmark].Y);
  BookmarkChanged(BookMark);
end;

procedure TJvCustomWideEditor.BookmarkChanged(BookMark: Integer);
begin
  FGutter.Invalidate;
end;

procedure TJvCustomWideEditor.SelectionChanged;
begin
  {abstract}
end;

procedure TJvCustomWideEditor.AdjustSelLineMode(Restore: Boolean);
begin
  with FSelection do
  begin
    if not Restore then
    begin
      SelLineOrgBegX := SelBegX;
      SelLineOrgEndX := SelEndX;
      SelBegX := 0;
      SelEndX := Max_X;
    end
    else
    begin
      SelBegX := SelLineOrgBegX;
      SelEndX := SelLineOrgEndX;
    end;
  end;
end;

procedure TJvCustomWideEditor.SetSel(SelX, SelY: Integer);
var
  LineLen: Integer;

  procedure UpdateSelected;
  var
    iR: Integer;
  begin
    with FSelection do
    begin
      if SelBlockFormat = bfColumn then
      begin
        if FUpdateSelBegY < SelBegY then
          for iR := FUpdateSelBegY to SelBegY do
            PaintLine(iR, -1, -1);
        for iR := SelBegY to SelEndY do
          PaintLine(iR, -1, -1);
        if FUpdateSelEndY > SelEndY then
          for iR := SelEndY to FUpdateSelEndY do
            PaintLine(iR, -1, -1);
      end
      else
      begin
        if FUpdateSelBegY < SelBegY then
          for iR := FUpdateSelBegY to SelBegY do
            PaintLine(iR, -1, -1)
        else
          for iR := SelBegY to FUpdateSelBegY do
            PaintLine(iR, -1, -1);
        if FUpdateSelEndY < SelEndY then
          for iR := FUpdateSelEndY to SelEndY do
            PaintLine(iR, -1, -1)
        else
          for iR := SelEndY to FUpdateSelEndY do
            PaintLine(iR, -1, -1);
      end;

      SelectionChanged;
      if Assigned(FOnSelectionChange) then
        FOnSelectionChange(Self);
    end;
  end;

begin
  with FSelection do
  begin
    {--- UNDO ---}
    TJvSelectUndo.Create(Self, FCaretX, FCaretY);
    {--- /UNDO ---}
    if SelX < 0 then SelX := 0;
    if SelY < 0 then SelY := 0;
    if SelY >= FLines.Count then
    begin
      if FLines.Count = 0 then
        SelY := 0 // select none
      else
      begin
        SelY := FLines.Count - 1;  // select last line
        if (not (FSelection.SelBlockFormat in [bfLine, bfColumn])) then
          SelX := Length(FLines[SelY]); // with all text
      end;
    end;
    if (not (SelBlockFormat in [bfLine, bfColumn])) then
    begin
      if (FLines.Count > 0) and (SelY < FLines.Count) then
      begin
        LineLen := Length(FLines[SelY]);
        if SelX > LineLen then
          SelX := LineLen; // only text not the whole line
      end;
    end;

    if FPersistentBlocks then
    begin
      if FPersistentBlocksCaretChanged then
      begin
        IsSelected := False;
        Selecting := False;
      end;
      FPersistentBlocksCaretChanged := False;
    end;

    if (not Selecting) then
    begin
      SelStartX := SelX;
      SelStartY := SelY;
      SelEndX := SelX;
      SelEndY := SelY;
      SelBegX := SelX;
      SelBegY := SelY;
      IsSelected := False;
      Selecting := True;
      if SelBlockFormat = bfLine then
        AdjustSelLineMode({Restore:=}False);
    end
    else
    begin
      if SelBlockFormat = bfLine then
        AdjustSelLineMode({Restore:=}True);

      FUpdateSelBegY := SelBegY;
      FUpdateSelEndY := SelEndY;

      if SelY <= SelStartY then
      begin
        SelBegY := SelY;
        SelEndY := SelStartY;
      end;
      if SelY >= SelStartY then
      begin
        SelBegY := SelStartY;
        SelEndY := SelY;
      end;

      if (SelY < SelStartY) or ((SelY = SelStartY) and (SelX <= SelStartX)) then
        if (SelBlockFormat = bfColumn) and (SelX > SelStartX) then
        begin
          SelBegX := SelStartX;
          SelEndX := SelX;
        end
        else
        begin
          SelBegX := SelX;
          SelEndX := SelStartX;
        end;
      if (SelY > SelStartY) or ((SelY = SelStartY) and (SelX >= SelStartX)) then
        if (SelBlockFormat = bfColumn) and (SelX < SelStartX) then
        begin
          SelBegX := SelX;
          SelEndX := SelStartX;
        end
        else
        begin
          SelBegX := SelStartX;
          SelEndX := SelX;
        end;


      if SelBlockFormat = bfLine then
      begin
       // save line mode X values
        SelLineOrgBegX := SelBegX;
        SelLineOrgEndX := SelEndX;
        SelBegX := 0;
        SelEndX := Max_X;
      end;

      if (SelBegY < SelEndY) or ((SelBegY = SelEndY) and (SelBegX < SelEndX)) then
        IsSelected := True
      else
        IsSelected := False;
    end;

    if FCompound = 0 then
      UpdateSelected;
    SetSelUpdateRegion(SelBegY, SelEndY);
  end; // with
end;

procedure TJvCustomWideEditor.SetSelUpdateRegion(BegY, EndY: Integer);
begin
  if FUpdateSelBegY > BegY then
    FUpdateSelBegY := BegY;
  if FUpdateSelEndY < EndY then
    FUpdateSelEndY := EndY;
end;

procedure TJvCustomWideEditor.SetSelBlockFormat(Value: TSelBlockFormat);
begin
  Command(ecInclusiveBlock + Integer(Value));
end;

function TJvCustomWideEditor.GetSelBlockFormat: TSelBlockFormat;
begin
  Result := FSelection.SelBlockFormat;
end;

procedure TJvCustomWideEditor.Mouse2Cell(X, Y: Integer; var CX, CY: Integer);
begin
  CX := Round((X - EditorClient.Left) / FCellRect.Width);
  CY := (Y - EditorClient.Top) div FCellRect.Height;
end;

procedure TJvCustomWideEditor.Mouse2Caret(X, Y: Integer; var CX, CY: Integer);
begin
  Mouse2Cell(X, Y, CX, CY);
  if CX < 0 then
    CX := 0;
  if CY < 0 then
    CY := 0;
  CX := CX + FLeftCol;
  CY := CY + FTopRow;
  if CX > FLastVisibleCol then
    CX := FLastVisibleCol;
  if CY > FLines.Count - 1 then
    CY := FLines.Count - 1;
end;

procedure TJvCustomWideEditor.CaretCoord(X, Y: Integer; var CX, CY: Integer);
begin
  CX := X - FLeftCol;
  CY := Y - FTopRow;
  if CX < 0 then
    CX := 0;
  if CY < 0 then
    CY := 0;
  CX := FCellRect.Width * CX;
  CY := FCellRect.Height * CY;
end;

procedure TJvCustomWideEditor.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  XX, YY, SX, SY: Integer;
begin
  if FDoubleClick then
  begin
    FDoubleClick := False;
    Exit;
  end;
  if FCompletion.FVisible then
    FCompletion.CloseUp(False);
  Mouse2Caret(X, Y, XX, YY);
  // if (XX = FCaretX) and (YY = FCaretY) then Exit;

  PaintCaret(False);
  if (Button = mbLeft) and not (ssShift in Shift) then
  begin
    if ssAlt in Shift then
      FSelection.SelBlockFormat := bfColumn
    else
      FSelection.SelBlockFormat := bfNonInclusive; // reset BlockFormat
    SetUnSelected;
  end;
  SetFocus;
  {--- UNDO ---}
  if Button = mbLeft then
    TJvBeginCompoundUndo.Create(Self);
  {--- /UNDO ---}
  if Button = mbLeft then
  begin
    if ssShift in Shift then
    begin
      if not FSelection.IsSelected then
      begin
        SetSel(FCaretX, FCaretY);
      end
      else
      begin
        SX := FSelection.SelStartX;
        SY := FSelection.SelStartY;
        SetUnSelected;
        SetSel(SX, SY);
      end;
      SetSel(XX, YY);
    end;
    SetCaret(XX, YY);
  end;
  PaintCaret(True);
  FMouseDowned := True;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvCustomWideEditor.DblClick;
var
  iBeg, iEnd: Integer;
  pt: TPoint;
  XX, YY: Integer;
begin
  FDoubleClick := True;
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);

  pt := ScreenToClient(Mouse.CursorPos);
  if (pt.X >= 0) and (pt.X < FGutterWidth) then
  begin
    Mouse2Caret(pt.X, pt.Y, XX, YY);
    GutterDblClick(YY);
  end
  else
  if FDoubleClickLine then
  begin
    PaintCaret(False);
    SetSel(0, FCaretY);
    if FCaretY = FLines.Count - 1 then
    begin
      SetSel(Length(FLines[FCaretY]), FCaretY);
      SetCaret(Length(FLines[FCaretY]), FCaretY);
    end
    else
    begin
      SetSel(0, FCaretY + 1);
      SetCaret(0, FCaretY + 1);
    end;
    PaintCaret(True);
  end
  else
  if (FLines.Count > 0) and (TrimW(FLines[FCaretY]) <> '') then
  begin
    iEnd := Length(TrimRightW(FLines[FCaretY]));
    if FCaretX < iEnd then
      while FLines[FCaretY][FCaretX + 1] <= ' ' do
        Inc(FCaretX)
    else
    begin
      FCaretX := iEnd - 1;
      while FLines[FCaretY][FCaretX + 1] <= ' ' do
        Dec(FCaretX);
    end;
    if GetWordOnPosExW(FLines[FCaretY] + ' ', FCaretX + 1, iBeg, iEnd) <> '' then
    begin
      PaintCaret(False);
      SetSel(iBeg - 1, FCaretY);
      SetSel(iEnd - 1, FCaretY);
      SetCaret(iEnd - 1, FCaretY);
      PaintCaret(True);
    end;
  end
end;

procedure TJvCustomWideEditor.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var XX, YY: Integer;
begin
  if FMouseDowned then
    TJvEndCompoundUndo.Create(Self);
  TimerScroll.Enabled := False;
  FMouseDowned := False;
  inherited MouseUp(Button, Shift, X, Y);

 // Gutter click
  if (X >= 0) and (X < FGutterWidth) then
  begin
    Mouse2Caret(X, Y, XX, YY);
    GutterClick(YY);
  end;
end;

procedure TJvCustomWideEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FMouseDowned and (ssLeft in (Shift * [ssShift, ssLeft]) ) then
  begin
//    SetSel(CaretX, CaretY);
    PaintCaret(False);
    MouseMoveY := Y;
    Mouse2Caret(X, Y, MouseMoveXX, MouseMoveYY);

    if MouseMoveYY <= FLastVisibleRow then
    begin
      SetSel(MouseMoveXX, MouseMoveYY);
      SetCaret(MouseMoveXX, MouseMoveYY);
    end;
    TimerScroll.Enabled := (Y < 0) or (Y > ClientHeight);
    PaintCaret(True);
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvCustomWideEditor.ScrollTimer(Sender: TObject);
begin
  if (MouseMoveY < 0) or (MouseMoveY > ClientHeight) then
  begin
    if MouseMoveY < -20 then
      Dec(MouseMoveYY, FVisibleRowCount)
    else
    if MouseMoveY < 0 then
      Dec(MouseMoveYY)
    else
    if MouseMoveY > ClientHeight + 20 then
      Inc(MouseMoveYY, FVisibleRowCount)
    else
    if MouseMoveY > ClientHeight then
      Inc(MouseMoveYY);
    PaintCaret(False);
    SetSel(MouseMoveXX, MouseMoveYY);
    SetCaret(MouseMoveXX, MouseMoveYY);
    PaintCaret(True);
  end;
end;

{############## Mouse [translated] ###############}

function TJvCustomWideEditor.GetClipboardBlockFormat: TSelBlockFormat;
var
  Data: THandle;
begin
  Result := bfNonInclusive;
  if Clipboard.HasFormat(BlockTypeFormat) then
  begin
    Clipboard.Open;
    Data := GetClipboardData(BlockTypeFormat);
    try
      if Data <> 0 then
        Result := TSelBlockFormat(PInteger(GlobalLock(Data))^);
    finally
      if Data <> 0 then
        GlobalUnlock(Data);
      Clipboard.Close;
    end;
  end;
end;

procedure TJvCustomWideEditor.SetClipboardBlockFormat(const Value: TSelBlockFormat);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  Clipboard.Open;
  try
    Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, 1);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(Value, DataPtr^, 1);
        //Adding;
        SetClipboardData(BlockTypeFormat, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Clipboard.Close;
  end;
end;

function TJvCustomWideEditor.GetSelText: WideString;
var
  S: WideString;
  i: Integer;
  Len, CLen: Integer;
  P: PWideChar;
begin
  with FSelection do
  begin
    Len := GetSelLength; // memory size to allocate
    Result := '';
    if Len = 0 then Exit;
    SetLength(Result, Len);
    
    if SelBlockFormat = bfColumn then
    begin
      if Len > 0 then
      begin
        P := Pointer(Result);
        for i := SelBegY to SelEndY do
        begin
          S := FLines[i];
          CLen := Length(S) - SelBegX;
          if CLen < 0 then CLen := 0;
          if CLen > SelEndX - SelBegX + 1 then
            CLen := SelEndX - SelBegX + 1;
          if CLen <> 0 then
          begin
            MoveWideChar(S[SelBegX + 1], P^, CLen);
            Inc(P, CLen);
          end;

          if i < SelEndY then
          begin
            MoveWideChar(sWideLineBreak[1], P^, sWideLineBreakLen);
            Inc(P, sWideLineBreakLen);
          end;
        end;
      end;
    end
    else
    begin
      if SelBegY = SelEndY then
        MoveWideChar(FLines[SelEndY][SelBegX + 1], Result[1], Len)
      else
      begin
        P := PWideChar(Result);

       // first line
        S := FLines[SelBegY];
        CLen := Length(S) - SelBegX;
        if CLen > 0 then
        begin
          MoveWideChar(S[SelBegX + 1], P^, CLen);
          Inc(P, CLen);
        end;

       // line break
        MoveWideChar(sWideLineBreak[1], P^, sWideLineBreakLen);
        Inc(P, sWideLineBreakLen);

       // lines between
        for i := SelBegY + 1 to SelEndY - 1 do
        begin
         // line
          S := FLines[i];
          MoveWideChar(S[1], P^, Length(S));
          Inc(P, Length(S));

         // line break
          MoveWideChar(sWideLineBreak[1], P^, sWideLineBreakLen);
          Inc(P, sWideLineBreakLen);
        end;

       // last line
        S := FLines[SelEndY];
        CLen := SelEndX + Integer(SelBlockFormat = bfInclusive);
        if CLen > Length(S) then CLen := Length(S);
        if CLen > 0 then
          MoveWideChar(S[1], P^, CLen);
      end;
    end;
  end; // with
end;

procedure TJvCustomWideEditor.SetSelText(const AValue: WideString);
begin
  BeginUpdate;
  BeginCompound;
  try
    with FSelection do
    begin
      if IsSelected then
        DeleteSelected
      else
      begin
        SelBegX := FCaretX;
        SelBegY := FCaretY;
      end;
      if FSelection.SelBlockFormat = bfColumn then
        InsertColumnText(FSelection.SelBegX, FSelection.SelBegY, AValue)
      else
        InsertText(AValue);

      IsSelected := Length(AValue) > 0;
      Selecting := False;
      GetEndPosCaret(AValue, SelBegX, SelBegY, SelEndX, SelEndY);
      if IsSelected then Inc(SelEndX);
      SetSelUpdateRegion(SelBegY, SelEndY);
    end; // with
  finally
    EndCompound;
    EndUpdate;
  end;
end;

procedure TJvCustomWideEditor.ClipboardCopy;
var S: AnsiString;
begin
  S := GetSelText; // convert to ANSI
  Clipboard.SetTextBuf(PChar(S));
  SetClipboardBlockFormat(SelBlockFormat);
end;

procedure TJvCustomWideEditor.InsertText(const Text: WideString);
var
  X, Y: Integer;
begin
  PaintCaret(False);
  try
    { --- UNDO --- }
    TJvInsertUndo.Create(Self, FCaretX, FCaretY, Text);
    { --- /UNDO --- }
    FLines.InsertText(FCaretX, FCaretY, Text);
    TextModified(FCaretX, FCaretY, maInsert, Text);

    GetEndPosCaret(Text, FCaretX, FCaretY, X, Y); // get new caret position
    SetCaretInternal(X + 1, Y);

    Changed;
  finally
    PaintCaret(True);
  end;
end;

procedure TJvCustomWideEditor.InsertColumnText(X, Y: Integer; const Text: WideString);
begin
  if X < 0 then X := 0;
  if Y < 0 then Y := 0;
  { --- UNDO --- }
  TJvInsertColumnUndo.Create(Self, X, Y, Text);
  { --- /UNDO --- }
  FLines.InsertColumnText(X, Y, Text);
  TextModified(X, Y, maInsertColumn, Text);
end;

// Substitutes a word in a cursor position on NewString
// string NewString should not contain #13, #10 [translated]

procedure TJvCustomWideEditor.ReplaceWord(const NewString: WideString);
var
  iBeg, iEnd: Integer;
  S, W: WideString;
  X: Integer;
//  F: Integer;

  function GetWordOnPos2(S: WideString; P: Integer): WideString;
  begin
    Result := '';
    if P < 1 then
      Exit;
    if CharInSetW(S[P], Separators) and ((P < 1) or CharInSetW(S[P - 1], Separators)) then
      Inc(P);
    iBeg := P;
    while iBeg >= 1 do
      if CharInSetW(S[iBeg], Separators) then
        Break
      else
        Dec(iBeg);
    Inc(iBeg);
    iEnd := P;
    while iEnd <= Length(S) do
      if CharInSetW(S[iEnd], Separators) then
        Break
      else
        Inc(iEnd);
    if iEnd > iBeg then
      Result := Copy(S, iBeg, iEnd - iBeg)
    else
      Result := S[P];
  end;

begin
  BeginUpdate;
  PaintCaret(False);
  try
    S := FLines[FCaretY];
    while FCaretX > Length(S) do
      S := S + ' ';
    W := TrimW(GetWordOnPos2(S, FCaretX));
    if W = '' then
    begin
      iBeg := FCaretX + 1;
      iEnd := FCaretX
    end;
    NotUndoable;
    //TJvReplaceUndo .Create(Self, FCaretX - Length(W), FCaretY, iBeg, iEnd, W, NewString);
    //  LW := Length(W);
    { (rom) disabled does nothing
    if FSelection.Selected then
    begin
      if (FSelection.SelBegY <= FCaretY) or (FCaretY >= FSelection.SelEndY) then
        // To correct LW .. [translated]
    end;
    }
    Delete(S, iBeg, iEnd - iBeg);
    Insert(NewString, S, iBeg);
    FLines.Internal[FCaretY] := S;
    X := iBeg + Length(NewString) - 1;
    TextModified(FCaretX, FCaretY, maInsert, NewString);
    PaintLine(FCaretY, -1, -1);
    SetCaretInternal(X, FCaretY);
    Changed;
  finally
    PaintCaret(True);
    EndUpdate;
  end;
end;

{ Substitutes a word on the cursor position by NewString [translated] }

procedure TJvCustomWideEditor.ReplaceWord2(const NewString: WideString);
var
  S, W: WideString; { strings are always initialized }
  iBegSX, iEndSX: Integer; { [1..Length] }
  X, Y: Integer;
begin
  if FCaretY < FLines.Count then
    S := FLines[FCaretY];

  W := TrimW(GetWordOnPosExW(S, FCaretX + 1, iBegSX, iEndSX));
  if (W <> NewString) then
  begin
    PaintCaret(False);
    try
      BeginCompound;
      try
        ReLine;
        if Length(W) = 0 then
        begin
          iBegSX := FCaretX + 1;
          iEndSX := FCaretX;
        end;
        { --- UNDO --- }
        TJvReplaceUndo.Create(Self, FCaretX, FCaretY, iBegSX - 1, FCaretY, W, NewString);
        { --- /UNDO --- }

        if iBegSX <= iEndSX then
          FLines.DeleteText(iBegSX - 1, FCaretY, iEndSX - 1, FCaretY);
        FLines.InsertText(iBegSX - 1, FCaretY, NewString);
        TextModified(iBegSX - 1, FCaretY, maReplace, NewString);

        GetEndPosCaret(NewString, iBegSX - 1, FCaretY, X, Y); // get end caret position
        SetCaretInternal(X + 1, Y);
      finally
        EndCompound;
      end;
      Changed;
    finally
      PaintCaret(True);
    end;
  end;
end;

procedure TJvCustomWideEditor.IndentColumns(X, BegY, EndY: Integer);
var
  Y: Integer;
  S: WideString;
begin
  if BegY < 0 then BegY := 0;
  if BegY >= FLines.Count then BegY := FLines.Count - 1;
  if EndY < 0 then EndY := 0;
  if EndY >= FLines.Count then EndY := FLines.Count - 1;
  if EndY < BegY then Exit;
  if X < 0 then X := 0;

  S := SpacesW(2);
  for Y := BegY to EndY - 1 do
    S := S + sWideLineBreak + SpacesW(2);

  InsertColumnText(X, BegY, S);

  Changed;
  if FUpdateLock = 0 then
    Invalidate;
end;

procedure TJvCustomWideEditor.UnIndentColumns(X: Integer; BegY, EndY: Integer);
var
  S, UnindentedText: WideString;
  Y: Integer;
  Len, L: Integer;
begin
  if BegY < 0 then BegY := 0;
  if BegY >= FLines.Count then BegY := FLines.Count - 1;
  if EndY < 0 then EndY := 0;
  if EndY >= FLines.Count then EndY := FLines.Count - 1;
  if EndY < BegY then Exit;
  if X < 0 then X := 0;

  Inc(X); // for string operations

  Inc(FUpdateLock);
  try
    UnindentedText := '';
    for Y := BegY to EndY do
    begin
      S := FLines[Y];
      Len := Length(S);

     // how many spaces to delete
      L := 0;
      while (X + L <= Len) and (L < 2) and (S[X + L] = ' ') do Inc(L);

      if L > 0 then
      begin
        UnindentedText := UnindentedText + SpacesW(L);
        Delete(S, X, L);
        FLines.Internal[Y] := S;
      end;
      if Y < EndY then
        UnindentedText := UnindentedText + sWideLineBreak;
    end;
  finally
    Dec(FUpdateLock);
  end;

  Dec(X); // for caret operations
  if Length(UnindentedText) > 0 then
  begin
    { --- UNDO --- }
    TJvUnindentColumnUndo.Create(Self, FCaretX, FCaretY, X, BegY, UnindentedText);
    { --- /UNDO --- }
    TextModified(X, BegY, maDelete, UnindentedText);

    Changed;
    if FUpdateLock = 0 then
      Invalidate;
  end;
end;

procedure TJvCustomWideEditor.IndentLines(UnIndent: Boolean; BegY, EndY: Integer);
begin
  if UnIndent then
    UnIndentColumns(0, BegY, EndY)
  else
    IndentColumns(0, BegY, EndY);
end;

procedure TJvCustomWideEditor.IndentSelLines(UnIndent: Boolean);
var
  BegNotBlank, EndNotBlank: Integer;
  BegY, EndY: Integer;
begin
  with FSelection do
  begin
    if (not IsSelected) or (SelBlockFormat = bfColumn) then Exit;

    BegY := SelBegY;
    EndY := SelEndY;
    if (SelEndX = 0) then
      Dec(EndY);
    if BegY > EndY then Exit;

    BegNotBlank := FindNotBlankCharPosW(FLines[BegY]) - 1;
    EndNotBlank := FindNotBlankCharPosW(FLines[EndY]) - 1;

    IndentLines(UnIndent, BegY, EndY);

   // to relative values
    BegNotBlank := (FindNotBlankCharPosW(FLines[BegY]) - 1) - BegNotBlank;
    EndNotBlank := (FindNotBlankCharPosW(FLines[EndY]) - 1) - EndNotBlank;

    if UnIndent then
    begin
     // adjust selection
      Inc(SelBegX, BegNotBlank);
      if SelBegX < 0 then
        SelBegX := 0;

      if SelEndX > 0 then
        Inc(SelEndX, EndNotBlank);
      if SelEndX < 0 then
        SelEndX := 0;
    end
    else
    begin
     // adjust selection
      Inc(SelBegX, BegNotBlank);
      if SelBegX > Max_X then
        SelBegX := Max_X;

      if SelEndX > 0 then
        Inc(SelEndX, EndNotBlank);
      if SelEndX > Max_X then
        SelEndX := Max_X;
    end;

   // adjust caret
    if (FCaretY = SelEndY) and (SelEndX > 0) then
      SetCaretInternal(FCaretX + EndNotBlank, FCaretY)
    else
    if (CaretY = SelBegY) then
      SetCaretInternal(FCaretX + BegNotBlank, FCaretY);

    SetSelUpdateRegion(BegY, EndY);
    PaintSelection;
  end;
end;

procedure TJvCustomWideEditor.ClipboardPaste;
var
  ClipS: string;
  Len: Integer;
  H: THandle;
  X, Y, EndX, EndY: Integer;
begin
  if (FCaretY > FLines.Count - 1) and (FLines.Count > 0) then
    if BeepOnError then
      Beep;
  H := Clipboard.GetAsHandle(CF_TEXT);
  Len := GlobalSize(H);
  if Len = 0 then
    Exit;

  BeginUpdate;
  try
    SetLength(ClipS, Len);
    SetLength(ClipS, Clipboard.GetTextBuf(PChar(ClipS), Len));
    ClipS := ExpandTabsAnsi(AdjustLineBreaks(ClipS));
    PaintCaret(False);

    ReLine;
    with FSelection do
    begin
      X := FCaretX;
      Y := FCaretY;
      BeginCompound;
      try
        if IsSelected then
        begin
          if (FBlockOverwrite and not FPersistentBlocks) then
          begin
            X := SelBegX;
            Y := SelBegY;
          end;
          RemoveSelectedBlock;
        end;
        if FLines.Count > 0 then
          ReLine;

       SelBlockFormat := GetClipboardBlockFormat;
       if SelBlockFormat in [bfInclusive, bfNonInclusive, bfLine] then
        begin
         // special line block mode handling
          if SelBlockFormat = bfLine then
          begin
            X := 0;
            if (Clips = '') or (Clips[Length(Clips)] <> #10) then
              Clips := Clips + sWideLineBreak;
          end;

          { --- UNDO --- }
          TJvInsertUndo.Create(Self, X, Y, ClipS);
          { --- /UNDO --- }

          FLines.InsertText(X, Y, ClipS);
          TextModified(X, Y, maInsert, ClipS);

         // get new caret position
          GetEndPosCaret(ClipS, X, Y, EndX, EndY);
          Inc(EndX);

          if FPersistentBlocks then
          begin
            SelBegX := X;
            SelBegY := Y;
           // special line block mode handling
            if SelBlockFormat = bfLine then
            begin
              Dec(EndY);
              SelEndX := Max_X;
            end
            else
              SelEndX := EndX;

            SelEndY := EndY;
            IsSelected := True;
            Selecting := False;
            SetSelUpdateRegion(SelBegY, SelEndY);
          end;
          X := EndX;
          Y := EndY;
        end
        else
        if SelBlockFormat = bfColumn then
        begin
          InsertColumnText(X, Y, ClipS);
          GetEndPosCaret(ClipS, X, Y, X, Y);
          X := FCaretX - 1;
          Inc(X);
        end;
      finally
        EndCompound;
      end;
    end; // with

    SetCaretInternal(X, Y);

    Changed;
  finally
    PaintCaret(True);
    EndUpdate; {!!! Causes copying all [translated] }
  end;
end;

procedure TJvCustomWideEditor.ClipboardCut;
begin
  ClipboardCopy;
  DeleteSelected;
end;

procedure TJvCustomWideEditor.DeleteSelected;
var
  S: WideString;
  X, Y: Integer;
begin
  with FSelection do
  begin
    X := SelBegX;
    Y := SelBegY;
    if IsSelected then
    begin
      BeginUpdate;
      PaintCaret(False);
      try
        S := GetSelText;
        {--- UNDO ---}
        TJvDeleteSelectedUndo.Create(Self, FCaretX, FCaretY, S);
        {--- /UNDO ---}
        IsSelected := False;
        Selecting := False;
        if SelBlockFormat in [bfInclusive, bfNonInclusive, bfLine] then
        begin
          FLines.DeleteText(X, Y, SelEndX - 1 + Integer(SelBlockFormat = bfInclusive), SelEndY);
          TextModified(SelBegX, SelBegY, maDelete, S);
        end
        else
        if SelBlockFormat = bfColumn then
        begin
          Y := FCaretY;
          FLines.DeleteColumnText(SelBegX, SelBegY, SelEndX, SelEndY);
          TextModified(SelBegX, SelBegY, maDeleteColumn, S);
        end;
        SetCaretInternal(X, Y);
        Changed;
      finally
        PaintCaret(True);
        EndUpdate;
      end;
    end;
  end; // with
end;

procedure TJvCustomWideEditor.SelectAll;
begin
  SelectRange(0, 0, Max_X, MaxInt);
end;

procedure TJvCustomWideEditor.ClearSelection;
begin
  SetUnSelected;
end;

function TJvCustomWideEditor.IsEmptySelection: Boolean;
begin
  with FSelection do
    Result := IsSelected and (SelBegX = SelEndX) and (SelBegY = SelEndY);
end;

procedure TJvCustomWideEditor.RemoveSelectedBlock;
begin
  if FSelection.IsSelected then
  begin
    if (FBlockOverwrite) and (not FPersistentBlocks) then
      DeleteSelected
    else
      if not FPersistentBlocks then
        SetUnSelected;
  end;
end;

procedure TJvCustomWideEditor.SelectRange(BegX, BegY, EndX, EndY: Integer);
begin
  { --- UNDO --- }
  TJvSelectUndo.Create(Self, FCaretX, FCaretY);
  { --- /UNDO ---}
  with FSelection do
  begin
    IsSelected := False;
    Selecting := False;

    if BegX < 0 then BegX := 0;
    if BegY < 0 then BegY := 0;
    if EndX > Max_X then EndX := Max_X;
    if (EndY < BegY) or (BegY >= FLines.Count) then Exit;
    if EndY >= FLines.Count then EndY := FLines.Count - 1;
    if EndY < 0 then Exit;

    SelBegX := BegX;
    SelBegY := BegY;
    SelEndX := EndX;
    SelEndY := EndY;
    SelLineOrgBegX := BegX;
    SelLineOrgEndX := BegY;
    IsSelected := not IsEmptySelection;
    Selecting := False;

    SetSelUpdateRegion(SelBegY, SelEndY);
  end;
  if FCompound = 0 then
    PaintSelection;
end;

procedure TJvCustomWideEditor.SetGutterWidth(AWidth: Integer);
begin
  if FGutterWidth <> AWidth then
  begin
    FGutterWidth := AWidth;
    UpdateEditorSize;
    Invalidate;
  end;
end;

procedure TJvCustomWideEditor.SetGutterColor(AColor: TColor);
begin
  if FGutterColor <> AColor then
  begin
    FGutterColor := AColor;
    FGutter.Invalidate;
  end;
end;

function TJvCustomWideEditor.GetLines: TWideStrings;
begin
  Result := FLines;
end;

procedure TJvCustomWideEditor.SetLines(ALines: TWideStrings);
begin
  if ALines <> nil then
    FLines.Assign(ALines);
  {--- UNDO ---}
  NotUndoable;
  {--- /UNDO ---}
end;

procedure TJvCustomWideEditor.TextAllChanged;
begin
  TextAllChangedInternal(True);
end;

procedure TJvCustomWideEditor.TextAllChangedInternal(Unselect: Boolean);
begin
  FontCacheClear;
  if Unselect then
  begin
    FSelection.IsSelected := False;
    FSelection.Selecting := False;
  end;

  TextModified(0, 0, maAll, '');

  UpdateEditorView;
end;

procedure TJvCustomWideEditor.UpdateEditorView;
begin
  UpdateEditorSize;
  if Showing and (FUpdateLock = 0) then
    Invalidate;
end;

procedure TJvCustomWideEditor.SetCols(ACols: Integer);
begin
  if FCols <> ACols then
  begin
    FCols := Max(ACols, 1);
    scbHorz.Max := FCols - 1;
  end;
end;

procedure TJvCustomWideEditor.SetRows(ARows: Integer);
begin
  if FRows <> ARows then
  begin
    FRows := Max(ARows, 1);
    scbVert.Max := Max(1, FRows - 1 + FVisibleRowCount - 1);
  end;
end;

procedure TJvCustomWideEditor.SetLeftTop(ALeftCol, ATopRow: Integer);
begin
  if ALeftCol < 0 then
    ALeftCol := 0;
  if FLeftCol <> ALeftCol then
  begin
    scbHorz.Position := ALeftCol;
    Scroll(False, ALeftCol);
  end;
  if ATopRow < 0 then
    ATopRow := 0;
  if FTopRow <> ATopRow then
  begin
    scbVert.Position := ATopRow;
    Scroll(True, ATopRow);
  end;
end;

procedure TJvCustomWideEditor.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomWideEditor.SetRightMarginVisible(Value: Boolean);
begin
  if FRightMarginVisible <> Value then
  begin
    FRightMarginVisible := Value;
    Invalidate;
  end;
end;

procedure TJvCustomWideEditor.SetRightMargin(Value: Integer);
begin
  if FRightMargin <> Value then
  begin
    FRightMargin := Value;
    Invalidate;
  end;
end;

procedure TJvCustomWideEditor.SetRightMarginColor(Value: TColor);
begin
  if FRightMarginColor <> Value then
  begin
    FRightMarginColor := Value;
    Invalidate;
  end;
end;

function TJvCustomWideEditor.ExpandTabs(const S: WideString): WideString;
var
  ps, i: Integer;
  Sp: WideString;
  Tabs, LenSp: Integer;
  P: PWideChar;
begin
  ps := Pos(#9, S);
  if ps > 0 then
  begin
   // How may Tab chars?
    Tabs := 1;
    for i := ps + 1 to Length(S) do
      if S[i] = #9 then Inc(Tabs);

    Sp := SpacesW(GetDefTabStop(0, True));
    LenSp := Length(Sp);

   // needed memory
    SetLength(Result, Length(S) - Tabs + Tabs * LenSp);
    P := PWideChar(Result);

   // copy the chars before the #9
    if ps > 1 then
    begin
      MoveWideChar(S[1], P[0], ps - 1);
      Inc(P, ps - 1);
    end;

    for i := ps to Length(S) do
    begin
      if S[i] <> #9 then
      begin
        P[0] := S[i];
        Inc(P);
      end
      else if LenSp > 0 then
      begin
        MoveWideChar(Sp[1], P[0], LenSp);
        Inc(P, LenSp);
      end;
    end;
  end
  else
    Result := S;
end;

function TJvCustomWideEditor.ExpandTabsAnsi(const S: AnsiString): AnsiString;
var
  ps, i: Integer;
  Sp: AnsiString;
  Tabs, LenSp: Integer;
  P: PChar;
begin
  ps := Pos(#9, S);
  if ps > 0 then
  begin
   // How may Tab chars?
    Tabs := 1;
    for i := ps + 1 to Length(S) do
      if S[i] = #9 then Inc(Tabs);

    Sp := Spaces(GetDefTabStop(0, True));
    LenSp := Length(Sp);

   // needed memory
    SetLength(Result, Length(S) - Tabs + Tabs * LenSp);
    P := PChar(Result);

   // copy the chars before the #9
    if ps > 1 then
    begin
      Move(S[1], P[0], ps - 1);
      Inc(P, ps - 1);
    end;

    for i := ps to Length(S) do
    begin
      if S[i] <> #9 then
      begin
        P[0] := S[i];
        Inc(P);
      end
      else if LenSp > 0 then
      begin
        Move(Sp[1], P[0], LenSp);
        Inc(P, LenSp);
      end;
    end;
  end
  else
    Result := S;
end;

procedure TJvCustomWideEditor.TextModified(ACaretX, ACaretY: Integer; Action: TModifiedAction; const Text: WideString);
begin
end;

procedure TJvCustomWideEditor.Changed;
begin
  FModified := True;
  FPEditBuffer := nil;
  if Assigned(FOnChange) then
    FOnChange(Self);
  StatusChanged;
end;

procedure TJvCustomWideEditor.StatusChanged;
begin
  if Assigned(FOnChangeStatus) then
    FOnChangeStatus(Self);
end;

procedure TJvCustomWideEditor.CaretFromPos(Pos: Integer; var X, Y: Integer);
{ it returns on the index Of pos - to the number of symbol - its coordinate.
  Returns on index Pos - to number of the character - his(its) coordinates.
  [translated]
}
var
  i, Len, p: Integer;
begin
  X := -1;
  Y := -1;
  if Pos < 0 then Exit;
  p := 0;

  for i := 0 to FLines.Count - 1 do
  begin
    Len := Length(FLines[i]);
    Inc(p, Len);
    if p >= Pos then
    begin
      Dec(p, Len);
      Y := i;
      X := Pos - p;
      Break;
    end;
    Inc(p, sWideLineBreakLen);
  end;
end;

function TJvCustomWideEditor.PosFromCaret(X, Y: Integer): Integer;
{ vice versa [translated] }
var
  I: Integer;
  Len: Integer;
begin
  if (Cardinal(Y) >= Cardinal(FLines.Count)){ or (Y < 0)} then
    Result := -1
  else
  begin
    Result := 0;
    for I := 0 to Y - 1 do
      Inc(Result, Length(FLines[I]) + sWideLineBreakLen {CR/LF});
    Len := Length(FLines[Y]);
    if X < Len then
      Inc(Result, X)
    else
      Inc(Result, Len);
  end;
end;

function TJvCustomWideEditor.PosFromMouse(X, Y: Integer): Integer;
var
  X1, Y1: Integer;
begin
  Mouse2Caret(X, Y, X1, Y1);
  if (X1 < 0) or (Y1 < 0) then
    Result := -1
  else
    Result := PosFromCaret(X1, Y1);
end;

function TJvCustomWideEditor.GetTextLen: Integer;
var
  i, SepSize: Integer;
begin
  SepSize := Length(sWideLineBreak);
  Result := 0;
  for i := 0 to FLines.Count - 1 do
    Result := Result + Length(FLines[i]) + SepSize;
  Dec(Result, SepSize);
end;

function TJvCustomWideEditor.GetSelStart: Integer;
begin
  if FSelection.IsSelected then
    Result := PosFromCaret(FSelection.SelBegX, FSelection.SelBegY)
  else
    Result := PosFromCaret(FCaretX, FCaretY);
end;

procedure TJvCustomWideEditor.SetSelStart(ASelStart: Integer);
begin
  with FSelection do
  begin
    IsSelected := False;
    Selecting := False;
    CaretFromPos(ASelStart, SelBegX, SelBegY);
    SetCaretInternal(SelBegX, SelBegY);
    SetSelLength(0);
    MakeRowVisible(SelBegY);
    //  PaintSelection;
    //  EditorPaint;
  end; // with
end;

procedure TJvCustomWideEditor.MakeRowVisible(ARow: Integer);
begin
  if (ARow < FTopRow) or (ARow > FLastVisibleRow) then
  begin
    ARow := ARow {mac: bugfix - FCaretY} - Trunc(VisibleRowCount / 2);
    if ARow < 0 then
      ARow := 0;
    SetLeftTop(FLeftCol, ARow);
  end;
end;

function TJvCustomWideEditor.GetSelLength: Integer;
var
  i: Integer;
  Len, CLen: Integer;
begin
  Result := 0;
  with FSelection do
  begin
    if (not IsSelected) then Exit;

    if (SelBegY < 0) or (SelBegY > FLines.Count - 1) or (SelEndY < 0) or
      (SelEndY > FLines.Count - 1) then
    begin
      if BeepOnError then
        Beep;
      Exit;
    end;

    if SelBlockFormat = bfColumn then
    begin
      for i := SelBegY to SelEndY do
      begin
        CLen := Length(FLines[i]) - SelBegX;
        if CLen < 0 then CLen := 0;
        if CLen > SelEndX - SelBegX + 1 then
          CLen := SelEndX - SelBegX + 1;

        Inc(Result, CLen + sWideLineBreakLen);
      end;
      if Result > 0 then
        Dec(Result, sWideLineBreakLen);
    end
    else
    begin
      if SelBegY = SelEndY then
      begin
        Result := SelEndX - SelBegX + Integer(SelBlockFormat = bfInclusive);
        if SelBegX + Result > Length(FLines[SelEndY]) then
          Result := Length(FLines[SelEndY]) - SelBegX;
        if Result < 0 then Result := 0;
      end
      else
      begin
        Result := Length(FLines[SelBegY]) - SelBegX;
        if Result < 0 then Result := 0;
        for i := SelBegY + 1 to SelEndY - 1 do
          Inc(Result, sWideLineBreakLen + Length(FLines[i]));

        Len := SelEndX + Integer(SelBlockFormat = bfInclusive);
        if Len > Length(FLines[SelEndY]) then Len := Length(FLines[SelEndY]);
        Result := Result + sWideLineBreakLen + Len;
      end;
    end;
  end;
end;

procedure TJvCustomWideEditor.SetSelLength(ASelLength: Integer);
begin
  with FSelection do
  begin
    IsSelected := ASelLength > 0;
    Selecting := False;
    CaretFromPos(SelStart + ASelLength, SelEndX, SelEndY);
    SetSelUpdateRegion(SelBegY, SelEndY);
    SetCaretInternal(SelEndX, SelEndY);
    //PaintSelection;
    Invalidate;
  end; // with
end;

procedure TJvCustomWideEditor.SetLockText(const Text: WideString);
begin
  FLines.SetLockText(Text);
end;

procedure TJvCustomWideEditor.GutterPaint(Canvas: TCanvas);
begin
  if Assigned(FOnPaintGutter) then
    FOnPaintGutter(Self, Canvas);
end;

procedure TJvCustomWideEditor.GutterClick(Line: Integer);
begin
  if Assigned(FOnGutterClick) then
    FOnGutterClick(Self, Line);
end;

procedure TJvCustomWideEditor.GutterDblClick(Line: Integer);
begin
  if Assigned(FOnGutterDblClick) then
    FOnGutterDblClick(Self, Line);
end;

procedure TJvCustomWideEditor.SetMode(Index: Integer; Value: Boolean);
var
  PB: ^Boolean;
begin
  if Index = 0 then
    PB := @FInsertMode
  else {1 :}
  begin
    PB := @FReadOnly;
    if Value then
      SetWindowLong(Handle,GWL_STYLE,GetWindowLong(Handle,GWL_STYLE) or ES_READONLY)
    else
      SetWindowLong(Handle,GWL_STYLE,GetWindowLong(Handle,GWL_STYLE) and not ES_READONLY);
  end;
  if PB^ <> Value then
  begin
    PB^ := Value;
    StatusChanged;
  end;
end;

function TJvCustomWideEditor.GetWordOnCaret: WideString;
begin
  if CaretY < FLines.Count then
    Result := GetWordOnPosW(FLines[CaretY], CaretX + 1)
  else
    Result := '';
end;

function TJvCustomWideEditor.GetAutoIndentStop(Y: Integer): Integer;
var
  i, Len: Integer;
  S: WideString;
begin
  Result := 0;

 // find non-empty line
  Dec(Y);
  while (Y > 0) do
  begin
    S := FLines[Y];
    if Length(TrimW(S)) > 0 then Break;
    Dec(Y);
  end;
  if Y < 0 then Exit;

  Len := Length(S);
  i := 1;
  while (i <= Len) and (S[i] = ' ') do Inc(i);
  Result := i - 1;
end;

function TJvCustomWideEditor.GetTabStop(X, Y: Integer; Next: Boolean): Integer;
var
  i: Integer;

  procedure UpdateTabStops;
  var
    S: WideString;
    j, i: Integer;
  begin
    FillChar(FTabPos, SizeOf(FTabPos), False);
    if FSmartTab then
    begin
      j := 1;
      i := 1;
      while Y - j >= 0 do
      begin
        S := TrimRightW(FLines[Y - j]);
        if Length(S) > i then
          FTabPos[Length(S)] := True;
        while i <= Length(S) do { Iterate }
        begin
          if CharInSetW(S[i], IdentifierSymbols) then
          begin
            FTabPos[i - 1] := True;
            while (i <= Length(S)) and CharInSetW(S[i], IdentifierSymbols) do
              Inc(i);
          end;
          Inc(i);
        end; { for }

        if i >= Max_X_Scroll then
          Break;
        if j >= FVisibleRowCount * 2 then
          Break;
        Inc(j);
      end;
    end;
  end;

begin
  UpdateTabStops;
  Result := X;
  if Next then
  begin
    for i := X + 1 to High(FTabPos) do
      if FTabPos[i] then
      begin
        Result := i;
        Exit;
      end;
    if (Result = X) then
      Result := GetDefTabStop(X, True);
  end
  else
  begin
    if Result = X then
      Result := GetDefTabStop(X, False);
  end;
end;

function TJvCustomWideEditor.GetDefTabStop(X: Integer; Next: Boolean): Integer;
var
  i: Integer;
  S: WideString;
  A, B: Integer;
begin
  if Next then
  begin
    i := 0;
    S := TrimW(SubStrW(FTabStops, i, ' '));
    A := 0;
    B := 1;
    while S <> '' do
    begin
      A := B;
      B := StrToInt(S) - 1;
      if B > X then
      begin
        Result := B;
        Exit;
      end;
      Inc(i);
      S := TrimW(SubStrW(FTabStops, i, ' '));
    end;
    { after last tab pos }
    Result := X + ((B - A) - ((X - B) mod (B - A)));
  end
  else
  begin
    i := 0;
    S := TrimW(SubStrW(FTabStops, i, ' '));
    A := 0;
    B := 0;
    while S <> '' do
    begin
      A := B;
      B := StrToInt(S) - 1;
      if B >= X then
      begin
        Result := A;
        Exit;
      end;
      Inc(i);
      S := TrimW(SubStrW(FTabStops, i, ' '));
    end;
    { after last tab pos }
    Result := X - ((B - A) - ((X - B) mod (B - A)));
  end;
end;

function TJvCustomWideEditor.GetBackStop(X, Y: Integer): Integer;
var
  i: Integer;
  S: WideString;

  procedure UpdateBackStops;
  var
    S: WideString;
    j, i, k: Integer;
  begin
    j := 1;
    i := X - 1;
    FillChar(FTabPos, SizeOf(FTabPos), False);
    FTabPos[0] := True;
    while Y - j >= 0 do
    begin
      S := FLines[Y - j];
      for k := 1 to Min(Length(S), i) do { Iterate }
        if S[k] <> ' ' then
        begin
          i := k;
          FTabPos[i - 1] := True;
          Break;
        end;
      if i = 1 then
        Break;
      if j >= FVisibleRowCount * 2 then
        Break;
      Inc(j);
    end;
  end;

begin
  Result := X - 1;
  S := TrimRightW(FLines[Y]);
  if (TrimW(Copy(S, 1, X)) = '') and
    ((X + 1 > Length(S)) or (S[X + 1] <> ' ')) then
  begin
    UpdateBackStops;
    for i := X downto 0 do
      if FTabPos[i] then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

procedure TJvCustomWideEditor.BeginCompound;
begin
  Inc(FCompound);
  {--- UNDO ---}
  TJvBeginCompoundUndo.Create(Self);
  {--- /UNDO ---}
end;

procedure TJvCustomWideEditor.EndCompound;
begin
  {--- UNDO ---}
  TJvEndCompoundUndo.Create(Self);
  {--- /UNDO ---}
  Dec(FCompound);
end;

procedure TJvCustomWideEditor.BeginRecord;
begin
  FMacro := '';
  FRecording := True;
  StatusChanged;
end;

procedure TJvCustomWideEditor.EndRecord(var AMacro: TMacro);
begin
  FRecording := False;
  AMacro := FMacro;
  StatusChanged;
end;

procedure TJvCustomWideEditor.PlayMacro(const AMacro: TMacro);
var
  i: Integer;
begin
  BeginUpdate;
  BeginCompound;
  try
    i := 1;
    while i < Length(AMacro) do
    begin
      Command(Byte(AMacro[i]) + Byte(AMacro[i + 1]) shl 8);
      Inc(i, 2);
    end;
  finally
    EndCompound;
    EndUpdate;
  end;
end;

function TJvCustomWideEditor.CanCopy: Boolean;
begin
  Result := FSelection.IsSelected and not IsEmptySelection;
end;

function TJvCustomWideEditor.CanCut: Boolean;
begin
  Result := CanCopy;
end;

function TJvCustomWideEditor.CanPaste: Boolean;
var
  H: THandle;
begin
  Result := False;
  if (FCaretY >= FLines.Count) and (FLines.Count > 0) then
    Exit;

  try
    H := Clipboard.GetAsHandle(CF_TEXT);
    if (H <> 0) then
      Result := (GlobalSize(H) > 0);
  except
    Result := False;
  end;
end;

procedure TJvCustomWideEditor.NotUndoable;
begin
  FUndoBuffer.Clear;
end;

procedure TJvCustomWideEditor.NotRedoable;
begin
  FUndoBuffer.ClearRedo;
end;

procedure TJvCustomWideEditor.CompletionIdentifier(var Cancel: Boolean);
begin
  {abstract}
end;

function TJvCustomWideEditor.GetPopupMenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if (Result = nil) and UseFixedPopup then
    Result := FixedDefaultEditPopup(Self);
end;

procedure TJvCustomWideEditor.CompletionTemplate(var Cancel: Boolean);
begin
  {abstract}
end;

procedure TJvCustomWideEditor.DoCompletionIdentifier(var Cancel: Boolean);
begin
  if not Focused then
    Cancel := True
  else
  begin
    CompletionIdentifier(Cancel);
    if Assigned(FOnCompletionIdentifier) then
      FOnCompletionIdentifier(Self, Cancel);
  end;
end;

procedure TJvCustomWideEditor.DoCompletionTemplate(var Cancel: Boolean);
begin
  if not Focused then
    Cancel := True
  else
  begin
    CompletionTemplate(Cancel);
    if Assigned(FOnCompletionTemplate) then
      FOnCompletionTemplate(Self, Cancel);
  end;
end;

{ TIEditReader support }

procedure TJvCustomWideEditor.ValidateEditBuffer;
begin
  if FPEditBuffer = nil then
  begin
    FEditBuffer := FLines.Text;
    FPEditBuffer := PWideChar(FEditBuffer);
    FEditBufferSize := Length(FEditBuffer);
  end;
end;

function TJvCustomWideEditor.GetText(Position: Longint; Buffer: PWideChar;
  Count: Longint): Longint;
begin
  ValidateEditBuffer;
  if Position <= FEditBufferSize then
  begin
    Result := Min(FEditBufferSize - Position, Count);
    MoveWideChar(FPEditBuffer[Position], Buffer^, Result);
  end
  else
    Result := 0;
end;

//=== TJvEditKey =============================================================

constructor TJvEditKey.Create(const ACommand: TEditCommand; const AKey1: Word;
  const AShift1: TShiftState);
begin
  inherited Create;
  Key1 := AKey1;
  Shift1 := AShift1;
  Command := ACommand;
end;

constructor TJvEditKey.Create2(const ACommand: TEditCommand; const AKey1: Word;
  const AShift1: TShiftState; const AKey2: Word; const AShift2: TShiftState);
begin
  inherited Create;
  Key1 := AKey1;
  Shift1 := AShift1;
  Key2 := AKey2;
  Shift2 := AShift2;
  Command := ACommand;
end;

//=== TJvKeyboard ============================================================

constructor TJvKeyboard.Create;
begin
  inherited Create;
  List := TList.Create;
end;

destructor TJvKeyboard.Destroy;
begin
  Clear;
  List.Free;
  inherited Destroy;
end;

procedure TJvKeyboard.Add(const ACommand: TEditCommand; const AKey1: Word;
  const AShift1: TShiftState);
begin
  List.Add(TJvEditKey.Create(ACommand, AKey1, AShift1));
end;

procedure TJvKeyboard.Add2(const ACommand: TEditCommand; const AKey1: Word;
  const AShift1: TShiftState; const AKey2: Word; const AShift2: TShiftState);
begin
  List.Add(TJvEditKey.Create2(ACommand, AKey1, AShift1, AKey2, AShift2));
end;

procedure TJvKeyboard.Add2Ctrl(const ACommand: TEditCommand;
  const AKey1: Word; const AShift1: TShiftState; const AKey2: Word);
begin
  Add2(Acommand, AKey1, AShift1, AKey2, [ssCtrl]);
  Add2(Acommand, AKey1, AShift1, AKey2, []);
end;

procedure TJvKeyboard.Clear;
var
  i: Integer;
begin
  for i := 0 to List.Count - 1 do
    TObject(List[i]).Free;
  List.Clear;
end;

function TJvKeyboard.Command(const AKey: Word; const AShift: TShiftState):
  TEditCommand;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to List.Count - 1 do
    with TJvEditKey(List[i]) do
      if (Key1 = AKey) and (Shift1 = AShift) then
      begin
        if Key2 = 0 then
          Result := Command
        else
          Result := twoKeyCommand;
        Exit;
      end;
end;

function TJvKeyboard.Command2(const AKey1: Word; const AShift1: TShiftState;
  const AKey2: Word; const AShift2: TShiftState): TEditCommand;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to List.Count - 1 do
    with TJvEditKey(List[i]) do
      if (Key1 = AKey1) and (Shift1 = AShift1) and
        (Key2 = AKey2) and (Shift2 = AShift2) then
      begin
        Result := Command;
        Exit;
      end;
 // no command found: trigger event
  if Assigned(FOnCommand2) then
     FOnCommand2(Self, AKey1, AShift1, AKey2, AShift2, Result);
end;

procedure TJvKeyboard.SetDefLayout;
begin
  Clear;
  Add(ecLeft, VK_LEFT, []);
  Add(ecRight, VK_RIGHT, []);
  Add(ecUp, VK_UP, []);
  Add(ecDown, VK_DOWN, []);
  Add(ecSelLeft, VK_LEFT, [ssShift]);
  Add(ecSelRight, VK_RIGHT, [ssShift]);
  Add(ecSelUp, VK_UP, [ssShift]);
  Add(ecSelDown, VK_DOWN, [ssShift]);
  Add(ecSelColumnLeft, VK_LEFT, [ssShift, ssAlt]);
  Add(ecSelColumnRight, VK_RIGHT, [ssShift, ssAlt]);
  Add(ecSelColumnUp, VK_UP, [ssShift, ssAlt]);
  Add(ecSelColumnDown, VK_DOWN, [ssShift, ssAlt]);

  Add(ecBeginLine, VK_HOME, []);
  Add(ecSelBeginLine, VK_HOME, [ssShift]);
  Add(ecBeginDoc, VK_HOME, [ssCtrl]);
  Add(ecSelBeginDoc, VK_HOME, [ssCtrl, ssShift]);
  Add(ecEndLine, VK_END, []);
  Add(ecSelEndLine, VK_END, [ssShift]);
  Add(ecEndDoc, VK_END, [ssCtrl]);
  Add(ecSelEndDoc, VK_END, [ssCtrl, ssShift]);
  Add(ecPrevWord, VK_LEFT, [ssCtrl]);
  Add(ecNextWord, VK_RIGHT, [ssCtrl]);
  Add(ecSelPrevWord, VK_LEFT, [ssCtrl, ssShift]);
  Add(ecSelNextWord, VK_RIGHT, [ssCtrl, ssShift]);
  Add(ecSelAll, Ord('A'), [ssCtrl]);

  Add(ecWindowTop, VK_PRIOR, [ssCtrl]);
  Add(ecWindowBottom, VK_NEXT, [ssCtrl]);
  Add(ecPrevPage, VK_PRIOR, []);
  Add(ecNextPage, VK_NEXT, []);
  Add(ecSelPrevPage, VK_PRIOR, [ssShift]);
  Add(ecSelNextPage, VK_NEXT, [ssShift]);
  Add(ecScrollLineUp, VK_UP, [ssCtrl]);
  Add(ecScrollLineDown, VK_DOWN, [ssCtrl]);

  Add(ecChangeInsertMode, VK_INSERT, []);

  Add(ecInsertPara, VK_RETURN, []);
  Add(ecBackspace, VK_BACK, []);
  Add(ecBackspace, VK_BACK, [ssShift]);
  Add(ecBackspaceWord, VK_BACK, [ssCtrl]);
  Add(ecDelete, VK_DELETE, []);
  Add(ecTab, VK_TAB, []);
  Add(ecBackTab, VK_TAB, [ssShift]);
  Add(ecDeleteSelected, VK_DELETE, [ssCtrl]);
  Add(ecClipboardCopy, VK_INSERT, [ssCtrl]);
  Add(ecClipboardCut, VK_DELETE, [ssShift]);
  Add(ecClipboardPaste, VK_INSERT, [ssShift]);

  Add(ecClipboardCopy, Ord('C'), [ssCtrl]);
  Add(ecClipboardCut, Ord('X'), [ssCtrl]);
  Add(ecClipboardPaste, Ord('V'), [ssCtrl]);

  Add(ecSetBookmark0, Ord('0'), [ssCtrl, ssShift]);
  Add(ecSetBookmark1, Ord('1'), [ssCtrl, ssShift]);
  Add(ecSetBookmark2, Ord('2'), [ssCtrl, ssShift]);
  Add(ecSetBookmark3, Ord('3'), [ssCtrl, ssShift]);
  Add(ecSetBookmark4, Ord('4'), [ssCtrl, ssShift]);
  Add(ecSetBookmark5, Ord('5'), [ssCtrl, ssShift]);
  Add(ecSetBookmark6, Ord('6'), [ssCtrl, ssShift]);
  Add(ecSetBookmark7, Ord('7'), [ssCtrl, ssShift]);
  Add(ecSetBookmark8, Ord('8'), [ssCtrl, ssShift]);
  Add(ecSetBookmark9, Ord('9'), [ssCtrl, ssShift]);

  Add(ecGotoBookmark0, Ord('0'), [ssCtrl]);
  Add(ecGotoBookmark1, Ord('1'), [ssCtrl]);
  Add(ecGotoBookmark2, Ord('2'), [ssCtrl]);
  Add(ecGotoBookmark3, Ord('3'), [ssCtrl]);
  Add(ecGotoBookmark4, Ord('4'), [ssCtrl]);
  Add(ecGotoBookmark5, Ord('5'), [ssCtrl]);
  Add(ecGotoBookmark6, Ord('6'), [ssCtrl]);
  Add(ecGotoBookmark7, Ord('7'), [ssCtrl]);
  Add(ecGotoBookmark8, Ord('8'), [ssCtrl]);
  Add(ecGotoBookmark9, Ord('9'), [ssCtrl]);

  Add2Ctrl(ecSetBookmark0, Ord('K'), [ssCtrl], Ord('0'));
  Add2Ctrl(ecSetBookmark1, Ord('K'), [ssCtrl], Ord('1'));
  Add2Ctrl(ecSetBookmark2, Ord('K'), [ssCtrl], Ord('2'));
  Add2Ctrl(ecSetBookmark3, Ord('K'), [ssCtrl], Ord('3'));
  Add2Ctrl(ecSetBookmark4, Ord('K'), [ssCtrl], Ord('4'));
  Add2Ctrl(ecSetBookmark5, Ord('K'), [ssCtrl], Ord('5'));
  Add2Ctrl(ecSetBookmark6, Ord('K'), [ssCtrl], Ord('6'));
  Add2Ctrl(ecSetBookmark7, Ord('K'), [ssCtrl], Ord('7'));
  Add2Ctrl(ecSetBookmark8, Ord('K'), [ssCtrl], Ord('8'));
  Add2Ctrl(ecSetBookmark9, Ord('K'), [ssCtrl], Ord('9'));

  Add2Ctrl(ecGotoBookmark0, Ord('Q'), [ssCtrl], Ord('0'));
  Add2Ctrl(ecGotoBookmark1, Ord('Q'), [ssCtrl], Ord('1'));
  Add2Ctrl(ecGotoBookmark2, Ord('Q'), [ssCtrl], Ord('2'));
  Add2Ctrl(ecGotoBookmark3, Ord('Q'), [ssCtrl], Ord('3'));
  Add2Ctrl(ecGotoBookmark4, Ord('Q'), [ssCtrl], Ord('4'));
  Add2Ctrl(ecGotoBookmark5, Ord('Q'), [ssCtrl], Ord('5'));
  Add2Ctrl(ecGotoBookmark6, Ord('Q'), [ssCtrl], Ord('6'));
  Add2Ctrl(ecGotoBookmark7, Ord('Q'), [ssCtrl], Ord('7'));
  Add2Ctrl(ecGotoBookmark8, Ord('Q'), [ssCtrl], Ord('8'));
  Add2Ctrl(ecGotoBookmark9, Ord('Q'), [ssCtrl], Ord('9'));

  Add2Ctrl(ecNonInclusiveBlock, Ord('O'), [ssCtrl], Ord('K'));
  Add2Ctrl(ecInclusiveBlock, Ord('O'), [ssCtrl], Ord('I'));
  Add2Ctrl(ecColumnBlock, Ord('O'), [ssCtrl], Ord('C'));
  Add2Ctrl(ecLineBlock, Ord('O'), [ssCtrl], Ord('L'));

  Add(ecUndo, Ord('Z'), [ssCtrl]);
  Add(ecUndo, VK_BACK, [ssAlt]);
//  Add(ecRedo, Ord('Z'), [ssShift, ssCtrl]);

  Add(ecCompletionIdentifiers, VK_SPACE, [ssCtrl]);
  Add(ecCompletionTemplates, Ord('J'), [ssCtrl]);

  { cursor movement - default and classic }
  Add2Ctrl(ecEndDoc, Ord('Q'), [ssCtrl], Ord('C'));
  Add2Ctrl(ecEndLine, Ord('Q'), [ssCtrl], Ord('D'));
  Add2Ctrl(ecWindowTop, Ord('Q'), [ssCtrl], Ord('E'));
  Add2Ctrl(ecBeginDoc, Ord('Q'), [ssCtrl], Ord('R'));
  Add2Ctrl(ecBeginLine, Ord('Q'), [ssCtrl], Ord('S'));
  Add2Ctrl(ecWindowTop, Ord('Q'), [ssCtrl], Ord('T'));
  Add2Ctrl(ecWindowBottom, Ord('Q'), [ssCtrl], Ord('U'));

  Add(ecDeleteWord, Ord('T'), [ssCtrl]);
  Add(ecInsertPara, Ord('N'), [ssCtrl]);
  Add(ecDeleteLine, Ord('Y'), [ssCtrl]);

  Add2Ctrl(ecSelWord, Ord('K'), [ssCtrl], Ord('T'));
  Add2Ctrl(ecToUpperCase, Ord('K'), [ssCtrl], Ord('O'));
  Add2Ctrl(ecToLowerCase, Ord('K'), [ssCtrl], Ord('N'));
  Add2Ctrl(ecChangeCase, Ord('O'), [ssCtrl], Ord('U'));
  Add2Ctrl(ecIndent, Ord('K'), [ssCtrl], Ord('I'));
  Add2Ctrl(ecUnindent, Ord('K'), [ssCtrl], Ord('U'));

  Add(ecRecordMacro, Ord('R'), [ssCtrl, ssShift]);
  Add(ecPlayMacro, Ord('P'), [ssCtrl, ssShift]);
end;

//=== TUndoBuffer ============================================================

procedure RedoNotImplemented;
begin
  raise EJvEditorError.Create(RsERedoNotYetImplemented);
end;

procedure TUndoBuffer.Add(AUndo: TUndo);
begin
  if InUndo then
    Exit;
  ClearRedo;
  inherited Add(AUndo);
  FPtr := Count - 1;
end;

procedure TUndoBuffer.Undo;
var
  UndoClass: TClass;
  Compound: Integer;
  IsOnlyCaret: Boolean;
  Selection: TJvSelectionRec;
begin
  if InUndo then Exit;

  Selection := FJvEditor.FSelection;

  IsOnlyCaret := True;
  InUndo := True;
  try
    if LastUndo <> nil then
    begin
      Compound := 0;
      UndoClass := LastUndo.ClassType;
      while (LastUndo <> nil) and
        ((UndoClass = LastUndo.ClassType) or
        (LastUndo is TJvDeleteTrailUndo) or
        (LastUndo is TJvReLineUndo) or
        (Compound > 0)) or
        ((UndoClass = TJvBackspaceUndo) and
        (LastUndo is TJvBackspaceUnindentUndo)) do
      begin
        if LastUndo.ClassType = TJvBeginCompoundUndo then
        begin
          Dec(Compound);
          UndoClass := nil;
        end
        else
        if LastUndo.ClassType = TJvEndCompoundUndo then
          Inc(Compound);
        LastUndo.Undo;
        if (LastUndo <> nil) then
        begin
          LastUndo.RestoreSelection;
          FJvEditor.Modified := LastUndo.FModified;
        end;
        Dec(FPtr);
        if (UndoClass = TJvDeleteTrailUndo) or
          (UndoClass = TJvReLineUndo) then
          UndoClass := LastUndo.ClassType;
        if (UndoClass <> TJvCaretUndo) and
          (UndoClass <> TJvSelectUndo) and
          (UndoClass <> TJvUnselectUndo) then
          IsOnlyCaret := False;
        if not FJvEditor.FGroupUndo then
          Break;
      end;
      if not FJvEditor.Modified then IsOnlyCaret := True;

     // paint selection
      if not CompareMem(@Selection, @FJvEditor.FSelection, SizeOf(TJvSelectionRec)) then
        FJvEditor.PaintSelection;

      FJvEditor.UpdateEditorView;
      if FJvEditor.FUpdateLock = 0 then
      begin
        if (not IsOnlyCaret) then
          FJvEditor.Changed;
      end;
    end;
  finally
    InUndo := False;
  end;
end;

procedure TUndoBuffer.Redo;
begin
  if CanRedo then
  begin
    Inc(FPtr);
    LastUndo.Redo;
  end;
end;

procedure TUndoBuffer.Clear;
begin
  while Count > 0 do
  begin
    TUndo(Items[0]).Free;
    inherited Delete(0);
  end;
end;

procedure TUndoBuffer.ClearRedo;
begin
  while (Count > 0) and (FPtr < Count - 1) do
  begin
    TUndo(Items[FPtr + 1]).Free;
    inherited Delete(FPtr + 1);
  end;
end;

procedure TUndoBuffer.Delete;
begin
  if Count > 0 then
  begin
    TUndo(Items[Count - 1]).Free;
    inherited Delete(Count - 1);
  end;
end;

function TUndoBuffer.LastUndo: TUndo;
begin
  if (FPtr >= 0) and (Count > 0) then
    Result := TUndo(Items[FPtr])
  else
    Result := nil;
end;

function TUndoBuffer.IsNewGroup(AUndo: TUndo): Boolean;
begin
  Result := (LastUndo = nil) or (LastUndo.ClassType <> AUndo.ClassType)
end;

function TUndoBuffer.IsCaretGroup: Boolean;
begin
  Result := (LastUndo <> nil) and (LastUndo.ClassType = TJvCaretUndo);
end;

function TUndoBuffer.CanUndo: Boolean;
begin
  Result := (LastUndo <> nil);
end;

function TUndoBuffer.CanRedo: Boolean;
begin
{
  Result := FPtr < Count;
}
  Result := False;
  ClearRedo;
end;

//=== TUndo ==================================================================

constructor TUndo.Create(AJvEditor: TJvCustomWideEditor);
begin
  inherited Create;
  FJvEditor := AJvEditor;
  FModified := FJvEditor.FModified;
  UndoBuffer.Add(Self);
  FSelection := nil;
end;

destructor TUndo.Destroy;
begin
  if Assigned(FSelection) then
    Dispose(FSelection);
end;

procedure TUndo.Redo;
begin
  RedoNotImplemented;
end;

procedure TUndo.RestoreSelection;
begin
  if Assigned(FSelection) then
  begin
    FJvEditor.FSelection := FSelection^;
    FJvEditor.SetSelUpdateRegion(FSelection^.SelBegY, FSelection^.SelEndY);
  end;
end;

procedure TUndo.SaveSelection;
begin
  if not Assigned(FSelection) then
    New(FSelection);
  FSelection^ := FJvEditor.FSelection;
end;

function TUndo.UndoBuffer: TUndoBuffer;
begin
  if FJvEditor <> nil then
    Result := FJvEditor.FUndoBuffer
  else
    Result := nil;
end;

//=== TJvCaretUndo ===========================================================

constructor TJvCaretUndo.Create(AJvEditor: TJvCustomWideEditor;
  ACaretX, ACaretY: Integer);
begin
  inherited Create(AJvEditor);
  FCaretX := ACaretX;
  FCaretY := ACaretY;
end;

procedure TJvCaretUndo.Undo;
begin
  with UndoBuffer do
  begin
    Dec(FPtr);
    while FJvEditor.FGroupUndo and (FPtr >= 0) and not IsNewGroup(Self) do
      Dec(FPtr);
    Inc(FPtr);
    with TJvCaretUndo(Items[FPtr]) do
      FJvEditor.SetCaretInternal(FCaretX, FCaretY);
  end;
end;

procedure TJvCaretUndo.Redo;
begin
  RedoNotImplemented;
end;

//=== TJvInsertUndo ==========================================================

constructor TJvInsertUndo.Create(AJvEditor: TJvCustomWideEditor;
  ACaretX, ACaretY: Integer; const AText: WideString);
begin
  inherited Create(AJvEditor, ACaretX, ACaretY);
  FText := AText;
  if FJvEditor.FPersistentBlocks then
    SaveSelection;
end;

procedure TJvInsertUndo.Undo;
var
  Text: WideString;
  EndX, EndY: Integer;
  du: TJvInsertUndo;
begin
  Text := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := TJvInsertUndo(LastUndo).FText + Text;
      Dec(FPtr);
      if not FJvEditor.FGroupUndo then
        Break;
    end;
    Inc(FPtr);
  end;
  du := TJvInsertUndo(UndoBuffer.Items[UndoBuffer.FPtr]);

  GetEndPosCaret(Text, du.FCaretX, du.FCaretY, EndX, EndY); // get end caret position
  FJvEditor.FLines.DeleteText(du.FCaretX, du.FCaretY, EndX, EndY);
  FJvEditor.TextModified(du.FCaretX, du.FCaretY, maDelete, Text);

  FJvEditor.SetCaretInternal(du.FCaretX, du.FCaretY);
end;

//=== TJvOverwriteUndo =======================================================

constructor TJvOverwriteUndo.Create(AJvEditor: TJvCustomWideEditor;
  ACaretX, ACaretY: Integer; const AOldText, ANewText: WideString);
begin
  inherited Create(AJvEditor, ACaretX, ACaretY);
  FOldText := AOldText;
  FNewText := ANewText;
end;

procedure TJvOverwriteUndo.Undo;
var
  OldText, NewText: WideString;
  EndX, EndY: Integer;
  du: TJvOverwriteUndo;
begin
  OldText := '';
  NewText := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      OldText := TJvOverwriteUndo(LastUndo).FOldText + OldText;
      NewText := TJvOverwriteUndo(LastUndo).FNewText + NewText;
      Dec(FPtr);
      if not FJvEditor.FGroupUndo then
        Break;
    end;
    Inc(FPtr);
  end;
  du := TJvOverwriteUndo(UndoBuffer.Items[UndoBuffer.FPtr]);
  with du do
  begin
    GetEndPosCaret(NewText, du.FCaretX, du.FCaretY, EndX, EndY); // get end caret position
    FJvEditor.FLines.DeleteText(FCaretX, FCaretY, EndX, EndY);
    FJvEditor.FLines.InsertText(FCaretX, FCaretY, OldText);
    FJvEditor.TextModified(FCaretX, FCaretY, maReplace, OldText);

    FJvEditor.SetCaretInternal(FCaretX, FCaretY);
  end;
end;

//=== TJvInsertColumnUndo ====================================================

procedure TJvInsertColumnUndo.Undo;
var
  i: Integer;
  SS: TWideStringList;
  S: WideString;
begin
  { Do not call FJvEditor.FLines.DeleteColumnText() here because it has not
    the functionality needed in this context. It deletes the columns from
    [BegX..EndX] even if the inserted line was not as long as EndX-BegX+1. }

  SS := TWideStringList.Create;
  try
    SS.NormalizationForm := FJvEditor.FLines.NormalizationForm;
    SS.Text := FText;
    for i := 0 to SS.Count - 1 do
    begin
      S := FJvEditor.FLines[FCaretY + i];
      Delete(S, FCaretX + 1, Length(SS[i]));
      FJvEditor.FLines.Internal[FCaretY + i] := S;
    end;
  finally
    SS.Free;
  end;
  FJvEditor.TextModified(FCaretX, FCaretY, maDelete, FText);

  FJvEditor.SetCaretInternal(FCaretX, FCaretY);
end;

//=== TJvIndentColumnUndo ====================================================

(* make Delphi 5 compiler happy // andreas
constructor TJvIndentColumnUndo.Create(AJvEditor: TJvCustomWideEditor; ACaretX,
  ACaretY, ABegX, ABegY: Integer; const AText: WideString);
begin
  inherited Create(AJvEditor, ABegX, ABegY, AText);
  FNewCaretX := ACaretX;
  FNewCaretY := ACaretY;
  SaveSelection;
end;

procedure TJvIndentColumnUndo.Undo;
begin
  inherited Undo;
  RestoreSelection;
  FJvEditor.SetCaretInternal(FNewCaretX, FNewCaretY);
end;
*)

//=== TJvUnindentColumnUndo ====================================================

constructor TJvUnindentColumnUndo.Create(AJvEditor: TJvCustomWideEditor;
  ACaretX, ACaretY, ABegX, ABegY: Integer; const AText: WideString);
begin
  inherited Create(AJvEditor, ACaretX, ACaretY, AText);
  SaveSelection;
  FBegX := ABegX;
  FBegY := ABegY;
end;

procedure TJvUnindentColumnUndo.Undo;
var
  BegX, BegY: Integer;
begin
  BegX := FBegX;
  BegY := FBegY;
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      with TJvUnindentColumnUndo(LastUndo) do
      begin
        FJvEditor.FLines.InsertColumnText(FBegX, FBegY, FText);
        if BegX > FBegX then BegX := FBegX;
        if BegY > FBegY then BegY := FBegY;
      end;
      Dec(FPtr);
      if not FJvEditor.FGroupUndo then
        Break;
    end;
    Inc(FPtr);
  end;
  FJvEditor.TextModified(BegX, BegY, maInsert, FJvEditor.FLines[BegY]);

  RestoreSelection;
  with TJvUnindentColumnUndo(UndoBuffer.LastUndo) do
    FJvEditor.SetCaretInternal(FCaretX, FCaretY);
end;

//=== TJvDeleteUndo ==========================================================

procedure TJvDeleteUndo.Undo;
var
  Text: WideString;
begin
  Text := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := TJvDeleteUndo(LastUndo).FText + Text;
      Dec(FPtr);
      if not FJvEditor.FGroupUndo then
        Break;
    end;
    Inc(FPtr);
  end;
  with TJvDeleteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do
  begin
    FJvEditor.FLines.InsertText(FCaretX, FCaretY, Text);
    FJvEditor.TextModified(FCaretX, FCaretY, maInsert, Text);

    FJvEditor.SetCaretInternal(FCaretX, FCaretY);
  end;
end;


//=== TJvDeleteLineUndo ==========================================================

procedure TJvDeleteLineUndo.Redo;
begin
//  FJvEditor.FLines.Insert(FCaretY, FText);
end;

procedure TJvDeleteLineUndo.Undo;
begin
  Inc(FJvEditor.FUpdateLock);
  try
    FJvEditor.FLines.Insert(FCaretY, FText);
    FJvEditor.TextModified(FCaretX, FCaretY, maInsert, FText);
  finally
    Dec(FJvEditor.FUpdateLock);
  end;
  FJvEditor.SetCaretInternal(FCaretX, FCaretY);
end;

//=== TJvBackspaceUndo =======================================================

procedure TJvBackspaceUndo.Undo;
var
  Text: WideString;
  StartPtr: Integer;
begin
  Text := '';
  with UndoBuffer do
  begin
    StartPtr := FPtr;
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := Text + TJvDeleteUndo(LastUndo).FText;
      Dec(FPtr);
      if not FJvEditor.FGroupUndo then
        Break;
    end;
    Inc(FPtr);
  end;

  with TJvDeleteUndo(UndoBuffer.Items[StartPtr]) do
  begin
    FJvEditor.FLines.InsertText(FCaretX - 1, FCaretY, Text);
    FJvEditor.TextModified(FCaretX - 1, FCaretY, maInsert, Text);
  end;

 // set caret on last backspace undo's position
  with TJvDeleteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do
  begin
    if (FText = #10) or (FText = #13) then // a line was removed by backspace
      FJvEditor.SetCaretInternal(0, FCaretY + 1)
    else
      FJvEditor.SetCaretInternal(FCaretX, FCaretY);
  end;
end;

procedure TJvBackspaceUnindentUndo.Undo;
var
  Text: WideString;
begin
  Text := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := Text + TJvDeleteUndo(LastUndo).FText;
      Dec(FPtr);
      if not FJvEditor.FGroupUndo then
        Break;
    end;
    Inc(FPtr);
  end;

  with TJvDeleteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do
  begin
    FJvEditor.FLines.InsertText(FCaretX - Length(Text), FCaretY, Text);
    FJvEditor.TextModified(FCaretX - Length(Text), FCaretY, maInsert, Text);
 // set caret on last backspace undo's position
    FJvEditor.SetCaretInternal(FCaretX, FCaretY);
  end;
end;

//=== TJvReplaceUndo =========================================================

constructor TJvReplaceUndo.Create(AJvEditor: TJvCustomWideEditor;
  ACaretX, ACaretY: Integer; ABegX, ABegY: Integer; const AText, ANewText: WideString);
begin
  inherited Create(AJvEditor, ACaretX, ACaretY);
  FBegX := ABegX;
  FBegY := ABegY;
  FText := AText;
  FNewText := ANewText;
end;

procedure TJvReplaceUndo.Undo;
var
  EndX, EndY: Integer;
begin
  GetEndPosCaret(FNewText, FBegX, FBegY, EndX, EndY);
  FJvEditor.FLines.DeleteText(FBegX, FBegY, EndX, EndY);
  FJvEditor.FLines.InsertText(FBegX, FBegY, FText);
  FJvEditor.TextModified(FBegX, FBegY, maReplace, FText);

  FJvEditor.SetCaretInternal(FCaretX, FCaretY);
end;

//=== TJvDeleteSelectedUndo ==================================================

constructor TJvDeleteSelectedUndo.Create(AJvEditor: TJvCustomWideEditor;
  ACaretX, ACaretY: Integer; const AText: WideString);
begin
  inherited Create(AJvEditor, ACaretX, ACaretY, AText);
  SaveSelection;
end;

procedure TJvDeleteSelectedUndo.Undo;
var
  S: WideString;
  i: Integer;
begin
  with FSelection^ do
  begin
    if SelBlockFormat in [bfInclusive, bfNonInclusive, bfLine] then
    begin
      FJvEditor.FLines.InsertText(SelBegX, SelBegY, FText);
      FJvEditor.TextModified(SelBegX, SelBegY, maInsert, FText);
    end
    else
    if SelBlockFormat = bfColumn then
    begin
      for i := SelBegY to SelEndY do
      begin
        S := FJvEditor.FLines[i];
        Insert(SubStrW(FText, i - SelBegY, sWideLineBreak), S, SelBegX + 1);
        FJvEditor.FLines.Internal[i] := S;
      end;
      FJvEditor.TextModified(SelBegX, SelBegY, maInsertColumn, FText);
    end;

    RestoreSelection;
    FJvEditor.SetCaretInternal(FCaretX, FCaretY);
  end; // with
end;

//=== TJvSelectUndo ==========================================================

constructor TJvSelectUndo.Create(AJvEditor: TJvCustomWideEditor;
  ACaretX, ACaretY: Integer);
begin
  inherited Create(AJvEditor, ACaretX, ACaretY);
  SaveSelection;
end;

procedure TJvSelectUndo.Undo;
var
  LastSel: TJvSelectUndo;
  LastCaret: TJvCaretUndo;
begin
  LastSel := Self;
  LastCaret := nil;
 { Undo TJvSelectUndo and TJvCaretUndo in one action. This prevents
   unnecessary caret movement with scolling. }
  with UndoBuffer do
  begin
    while (FPtr >= 0) and ((not IsNewGroup(Self)) or (IsCaretGroup)) do
    begin
      if LastUndo.ClassType = TJvCaretUndo then
        LastCaret := TJvCaretUndo(LastUndo)
      else
        LastSel := TJvSelectUndo(LastUndo);
      Dec(FPtr);
      if not FJvEditor.FGroupUndo then
        Break;
    end;
    Inc(FPtr);
  end;

  LastSel.RestoreSelection;

  if LastCaret <> nil then
    LastCaret.Undo
  else
    FJvEditor.SetCaretInternal(LastSel.FCaretX, LastSel.FCaretY);
end;

//=== TJvBeginCompoundUndo ===================================================

procedure TJvBeginCompoundUndo.Undo;
begin
  { nothing }
end;

//=== TJvWideEditorCompletion ====================================================

type
  TJvWideEditorCompletionList = class(TListBox)
  private
    FTimer: TTimer;
    YY: Integer;
    // HintWindow : THintWindow;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure WMCancelMode(var Msg: TMessage); message WM_CancelMode;
    procedure OnTimer(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TJvCompletion.Create(AJvEditor: TJvCustomWideEditor);
begin
  inherited Create;
  FJvEditor := AJvEditor;
  FPopupList := TJvWideEditorCompletionList.Create(FJvEditor);
  FItemHeight := FPopupList.ItemHeight;
  FDropDownCount := 6;
  FDropDownWidth := 300;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 800;
  FTimer.OnTimer := OnTimer;
  FIdentifiers := TWideStringList.Create;
  FIdentifiers.NormalizationForm := nfNone;
  FTemplates := TWideStringList.Create;
  FTemplates.NormalizationForm := nfNone;
  FItems := TWideStringList.Create;
  FDefMode := cmIdentifiers;
  FCaretChar := '|';
  FCRLF := '/n';
  FSeparator := '=';
end;

destructor TJvCompletion.Destroy;
begin
  inherited Destroy;
  FPopupList.Free;
  FIdentifiers.Free;
  FTemplates.Free;
  FItems.Free;
  FTimer.Free;
end;

function TJvCompletion.GetItems: TWideStrings;
begin
  case FMode of
    cmIdentifiers: Result := FIdentifiers;
    cmTemplates: Result := FTemplates;
  else
    Result := nil;
  end;
end;

function GetNextWordPosEx(const Text: WideString; StartIndex: Integer;
  var iBeg, iEnd: Integer): WideString;
var
  Len: Integer;
begin
  Len := Length(Text);
  Result := '';
  if (StartIndex < 1) or (StartIndex > Len) then
    Exit;
  if CharInSetW(Text[StartIndex], Separators) and
     ((StartIndex < 1) or CharInSetW(Text[StartIndex - 1], Separators)) then
    Inc(StartIndex);
  iBeg := StartIndex;
  while iBeg >= 1 do
    if CharInSetW(Text[iBeg], Separators) then
      Break
    else
      Dec(iBeg);
  Inc(iBeg);
  iEnd := StartIndex;
  while iEnd <= Len do
    if CharInSetW(Text[iEnd], Separators) then
      Break
    else
      Inc(iEnd);
  Dec(iEnd);                                 
  if iEnd >= iBeg then
    Result := Copy(Text, iBeg, iEnd - iBeg)
  else
    Result := Text[StartIndex];

 // go right
  iEnd := iBeg;
  while (iEnd <= Len) and (not CharInSetW(Text[iEnd], Separators)) do Inc(iEnd);
  if iEnd > Len then iEnd := Len else Dec(iEnd);
  Result := Copy(Text, iBeg, iEnd - iBeg + 1);
end;

{ Substitutes word on the cursor position by NewString [translated] }

procedure TJvCompletion.ReplaceWord(const NewString: WideString);
var
  S, W: WideString;
  X, Y: Integer;
  iBegSX, iEndSX: Integer;
  NewCaret: Integer;
begin
  with FJvEditor do
  begin
    if FCaretY < FLines.Count then
      S := FLines[FCaretY];
    W := GetNextWordPosEx(S, FCaretX, iBegSX, iEndSX);
    if W <> NewString then
    begin
      BeginUpdate;
      PaintCaret(False);
      try
        BeginCompound;
        try
          ClearSelection;
          Reline;

          if Length(W) = 0 then
          begin
            iBegSX := FCaretX + 1;
            iEndSX := FCaretX;
          end;
          case FMode of
            cmIdentifiers:
              begin
                S := NewString;
                if Assigned(FOnCompletionApply) then
                  FOnCompletionApply(Self, W, S);
                NewCaret := -1;
              end;
            cmTemplates:
              begin
                S := ReplaceStringW(NewString, FCRLF, sWideLineBreak + SpacesW(FCaretX -
                  Length(W)));
                S := ReplaceStringW(S, FCaretChar, '');
                NewCaret := Pos(FCaretChar, NewString) - 1;
              end;
          else
            raise EJvEditorError.Create(RsEInvalidCompletionMode);
          end;
          {--- UNDO ---}
          TJvReplaceUndo.Create(FJvEditor, FCaretX, FCaretY, iBegSX - 1, FCaretY, W, S);
          {--- /UNDO ---}
          //  LW := Length(W);
          { (rom) disabled does nothing
          if FSelection.Selected then
          begin
            if (FSelBegY <= FCaretY) or (FCaretY >= FSelEndY) then
              // To correct LW .. [translated]
          end;
          }

          if iBegSX <= iEndSX then
            FLines.DeleteText(iBegSX - 1, FCaretY, iEndSX - 1, FCaretY);
          FLines.InsertText(iBegSX - 1, FCaretY, S);
          TextModified(iBegSX - 1, FCaretY, maReplace, S);

          if NewCaret >= 0 then
            SetLength(S, NewCaret); // truncate S to the new caret position 
          GetEndPosCaret(S, iBegSX - 1, FCaretY, X, Y);
          SetCaretInternal(X + 1, Y);
        finally
          EndCompound;
        end;
        Changed;
      finally
        PaintCaret(True);
        EndUpdate;
      end;
    end;
  end;
end;

procedure TJvCompletion.DoKeyPress(Key: WideChar);
begin
  if FVisible then
    if HasCharW(Key, JvEditorCompletionChars) then
      SelectItem
    else
      CloseUp(True)
  else
  if FEnabled then
    FTimer.Enabled := True;
end;

function TJvCompletion.DoKeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := True;
  case Key of
    VK_ESCAPE:
      CloseUp(False);
    VK_RETURN:
      CloseUp(True);
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_HOME, VK_END:
      FPopupList.Perform(WM_KEYDOWN, Key, 0);
  else
    Result := False;
  end;
end;

procedure TJvCompletion.DoCompletion(const AMode: TCompletionList);
var
  Eq: Boolean;
  Cancel: Boolean;
begin
  if FJvEditor.FReadOnly then
    Exit;
  if FPopupList.Visible then
    CloseUp(False);
  FMode := AMode;
  case FMode of
    cmIdentifiers:
      DropDown(AMode, True);
    cmTemplates:
      begin
        Cancel := False;
        // FJvEditor.DoCompletionIdentifier(Cancel);
        FJvEditor.DoCompletionTemplate(Cancel);
        if Cancel or (FTemplates.Count = 0) then
          Exit;
        MakeItems;
        FindSelItem(Eq);
        if Eq then
          ReplaceWord(SubStrW(FItems[ItemIndex], 2, FSeparator))
        else
          DropDown(AMode, True);
      end;
  end;
end;

procedure TJvCompletion.DropDown(const AMode: TCompletionList; const ShowAlways:
  Boolean);
var
  ItemCount: Integer;
  P: TPoint;
  Y: Integer;
  PopupWidth, PopupHeight: Integer;
  SysBorderWidth, SysBorderHeight: Integer;
  R: TRect;
  Cancel: Boolean;
  Eq: Boolean;
begin
  CloseUp(False);
  FMode := AMode;
  with FJvEditor do
  begin
    Cancel := False;
    case FMode of
      cmIdentifiers:
        FJvEditor.DoCompletionIdentifier(Cancel);
      cmTemplates:
        FJvEditor.DoCompletionTemplate(Cancel)
    end;
    MakeItems;
    FindSelItem(Eq);
    // Cancel := not Visible and (ItemIndex = -1);
    if Cancel or (FItems.Count = 0) or (((ItemIndex = -1) or Eq) and not ShowAlways) then
      Exit;
    FPopupList.Items.Assign(FItems);
    FPopupList.ItemHeight := FItemHeight;
    FVisible := True;
    SetItemIndex(FItemIndex);
    if FListBoxStyle in [lbStandard] then
      FPopupList.Style := lbOwnerDrawFixed
    else
      FPopupList.Style := FListBoxStyle;
    FPopupList.OnMeasureItem := FJvEditor.FOnCompletionMeasureItem;
    FPopupList.OnDrawItem := FJvEditor.FOnCompletionDrawItem;

    ItemCount := FItems.Count;
    SysBorderWidth := GetSystemMetrics(SM_CXBORDER);
    SysBorderHeight := GetSystemMetrics(SM_CYBORDER);
    R := CalcCellRect(FCaretX - FLeftCol, FCaretY - FTopRow + 1);
    P := R.TopLeft;
    P.X := ClientOrigin.X + P.X;
    P.Y := ClientOrigin.Y + P.Y;
    Dec(P.X, 2 * SysBorderWidth);
    Dec(P.Y, SysBorderHeight);
    if ItemCount > FDropDownCount then
      ItemCount := FDropDownCount;
    PopupHeight := ItemHeight * ItemCount + 2;
    Y := P.Y;
    if (Y + PopupHeight) > Screen.Height then
    begin
      Y := P.Y - PopupHeight - FCellRect.Height + 1;
      if Y < 0 then
        Y := P.Y;
    end;
    PopupWidth := FDropDownWidth;
    if PopupWidth = 0 then
      PopupWidth := Width + 2 * SysBorderWidth;
  end;
  FPopupList.Left := P.X;
  FPopupList.Top := Y;
  FPopupList.Width := PopupWidth;
  FPopupList.Height := PopupHeight;
  SetWindowPos(FPopupList.Handle, HWND_TOP, P.X, Y, 0, 0,
    SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  FPopupList.Visible := True;
end;

procedure TJvCompletion.MakeItems;
var
  i: Integer;
  S: WideString;
begin
  FItems.Clear;
  case FMode of
    cmIdentifiers:
      for i := 0 to FIdentifiers.Count - 1 do
        FItems.Add(FIdentifiers[i]);
    cmTemplates:
      begin
        with FJvEditor do
          if FLines.Count > CaretY then
            S := GetWordOnPosW(FLines[CaretY], CaretX)
          else
            S := '';
        for i := 0 to FTemplates.Count - 1 do
          if StrLICompW2(PWideChar(FTemplates[i]), PWideChar(S), Length(S)) = 0 then
            FItems.Add(FTemplates[i]);
        if FItems.Count = 0 then
          FItems.Assign(FTemplates);
      end;
  end;
end;

procedure TJvCompletion.FindSelItem(var Eq: Boolean);

  function CmpW(const S1, S2: WideString): Boolean;
  begin
    Result := StrICompW(PWideChar(S1), PWideChar(S2)) = 0;
  end;

var
  S: WideString;

  function FindFirst(Ss: TWideStrings; S: WideString): Integer;
  var
    i: Integer;
  begin
    for i := 0 to Ss.Count - 1 do
      if StrLICompW2(PWideChar(Ss[i]), PWideChar(S), Length(S)) = 0 then
      begin
        Result := i;
        Exit;
      end;
    Result := -1;
  end;

begin
  with FJvEditor do
    if FLines.Count > 0 then
      S := GetWordOnPosW(FLines[CaretY], CaretX)
    else
      S := '';
  if TrimW(S) = '' then
    ItemIndex := -1
  else
    ItemIndex := FindFirst(FItems, S);
  Eq := (ItemIndex > -1) and CmpW(TrimW(SubStrW(FItems[ItemIndex], 0, FSeparator)), S);
end;

procedure TJvCompletion.SelectItem;
var
  Cancel: Boolean;
  Param: Boolean;
begin
  FindSelItem(Param);
  Cancel := not Visible and (ItemIndex = -1);
  case FMode of
    cmIdentifiers:
      FJvEditor.DoCompletionIdentifier(Cancel);
    cmTemplates:
      FJvEditor.DoCompletionTemplate(Cancel);
  end;
  if Cancel or (GetItems.Count = 0) then
    CloseUp(False);
end;

procedure TJvCompletion.CloseUp(const Apply: Boolean);
begin
  FItemIndex := ItemIndex;
  FPopupList.Visible := False;
  //  (FPopupList as TJvWideEditorCompletionList). HintWindow.ReleaseHandle;
  FVisible := False;
  FTimer.Enabled := False;
  if Apply and (ItemIndex > -1) then
    case FMode of
      cmIdentifiers:
        ReplaceWord(SubStrW(FItems[ItemIndex], 0, FSeparator));
      cmTemplates:
        ReplaceWord(SubStrW(FItems[ItemIndex], 2, FSeparator));
    end;
end;

procedure TJvCompletion.OnTimer(Sender: TObject);
begin
  DropDown(FDefMode, False);
end;

procedure TJvCompletion.SetWideStrings(Index: Integer; AValue: TWideStrings);
begin
  case Index of
    0:
      FIdentifiers.Assign(AValue);
    1:
      FTemplates.Assign(AValue);
  end;
end;

function TJvCompletion.GetItemIndex: Integer;
begin
  Result := FItemIndex;
  if FVisible then
    Result := FPopupList.ItemIndex;
end;

procedure TJvCompletion.SetItemIndex(AValue: Integer);
begin
  FItemIndex := AValue;
  if FVisible then
    FPopupList.ItemIndex := FItemIndex;
end;

function TJvCompletion.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TJvCompletion.SetInterval(AValue: Cardinal);
begin
  FTimer.Interval := AValue;
end;

//=== TJvWideEditorCompletionList ================================================

constructor TJvWideEditorCompletionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Left := -1000;
  Visible := False;
  TabStop := False;
  ParentFont := False;
  Parent := Owner as TJvCustomWideEditor;
  Ctl3D := False;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 200;
  FTimer.OnTimer := OnTimer;
  Style := lbOwnerDrawFixed;
  ItemHeight := 13;

  //  HintWindow := THintWindow.Create(Self);
end;

destructor TJvWideEditorCompletionList.Destroy;
begin
  FTimer.Free;
  //  HintWindow.Free;
  inherited Destroy;
end;

procedure TJvWideEditorCompletionList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style {or WS_POPUP} or WS_BORDER;
    ExStyle := ExStyle or WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

procedure TJvWideEditorCompletionList.CreateWnd;
begin
  inherited CreateWnd;
  if not (csDesigning in ComponentState) then
    Windows.SetParent(Handle, 0);
  //  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0); {??}
end;

procedure TJvWideEditorCompletionList.DestroyWnd;
begin
  inherited DestroyWnd;
  //  HintWindow.ReleaseHandle;
end;

procedure TJvWideEditorCompletionList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  F: Integer;
begin
  YY := Y;
  F := ItemAtPos(Point(X, Y), True);
  if KeyPressed(VK_LBUTTON) then
  begin
    F := ItemAtPos(Point(X, Y), True);
    if F > -1 then
      ItemIndex := F;
    FTimer.Enabled := (Y < 0) or (Y > ClientHeight);
    if (Y < -ItemHeight) or (Y > ClientHeight + ItemHeight) then
      FTimer.Interval := 50
    else
      FTimer.Interval := 200;
  end;
  if (F > -1) and not FTimer.Enabled then
  begin
    //Application.CancelHint;
   // Hint := Items[F];
  //  HintWindow.ActivateHint(Bounds(ClientOrigin.X + X, ClientOrigin.Y + Y, 300, ItemHeight), Items[F]);
  end;
end;

procedure TJvWideEditorCompletionList.MouseDown(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
var
  F: Integer;
begin
  MouseCapture := True;
  F := ItemAtPos(Point(X, Y), True);
  if F > -1 then
    ItemIndex := F;
end;

procedure TJvWideEditorCompletionList.MouseUp(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
  MouseCapture := False;
  (Owner as TJvCustomWideEditor).FCompletion.CloseUp(
    (Button = mbLeft) and PtInRect(ClientRect, Point(X, Y)));
end;

procedure TJvWideEditorCompletionList.OnTimer(Sender: TObject);
begin
  if YY < 0 then
    Perform(WM_VSCROLL, SB_LINEUP, 0)
  else
  if YY > ClientHeight then
    Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TJvWideEditorCompletionList.WMCancelMode(var Msg: TMessage);
begin
  (Owner as TJvCustomWideEditor).FCompletion.CloseUp(False);
end;

procedure TJvWideEditorCompletionList.CMHintShow(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TJvWideEditorCompletionList.DrawItem(Index: Integer; Rect: TRect; State:
  TOwnerDrawState);
var
  Offset, W: Integer;
  S: WideString;
begin
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else
  begin
    Canvas.FillRect(Rect);
    Offset := 3;
    with (Owner as TJvCustomWideEditor).FCompletion do
      case FMode of
        cmIdentifiers:
          TJvUnicodeCanvas(Canvas).TextOutW(Rect.Left + Offset, Rect.Top, SubStrW(Items[Index], 1,
            Separator));
        cmTemplates:
          begin
            TJvUnicodeCanvas(Canvas).TextOutW(Rect.Left + Offset, Rect.Top, SubStrW(Items[Index], 1,
              Separator));
            Canvas.Font.Style := [fsBold];
            S := SubStrW(Items[Index], 0, Separator);
            W := Canvas.TextWidth(S);
            TJvUnicodeCanvas(Canvas).TextOutW(Rect.Right - 2 * Offset - W, Rect.Top, S);
          end;
      end;
  end;
end;

procedure TJvCustomWideEditor.EMCanUndo(var Msg: TMessage);
begin
  Msg.Result := Ord(FUndoBuffer.CanUndo);
end;

procedure TJvCustomWideEditor.EMGetSelection(var Msg: TMessage);
var
   LSelStart, LSelEnd: Integer;
begin
   LSelStart := SelStart;
   LSelEnd := SelStart + SelLength;
   if Pointer(Msg.WParam) <> nil then
     PLongint(Msg.WParam)^ := LSelStart;
   if Pointer(Msg.LParam) <> nil then
     PLongint(Msg.LParam)^ := LSelEnd;
   if (LSelEnd > 65535) or (LSelStart > 65535) then
     Msg.Result := -1
   else
   begin
     Msg.ResultLo := LongRec(LSelStart).Lo;
     Msg.ResultHi := LongRec(LSelEnd).Lo;
   end;
end;

procedure TJvCustomWideEditor.EMSetReadOnly(var Msg: TMessage);
begin
  ReadOnly := Msg.WParam = 1;
end;

procedure TJvCustomWideEditor.EMSetSelection(var Msg: TMessage);
begin
  if (Msg.wParam = 0) and (Msg.LParam = -1) then
    SelectAll
  else
  begin
    SelStart := Msg.wParam;
    SelLength := Msg.lParam;
  end;
end;

procedure TJvCustomWideEditor.WMClear(var Msg: TMessage);
begin
  if not ReadOnly then
    DeleteSelected;
end;

procedure TJvCustomWideEditor.WMUndo(var Msg: TMessage);
begin
  Undo;
end;

procedure TJvCustomWideEditor.WMGetTextLength(var Msg: TMessage);
begin
  Msg.Result := GetTextLen;
end;

procedure TJvCustomWideEditor.WMGetText(var Msg: TWMGetText);
var S: AnsiString;
begin
  if Msg.Text = nil then
    Msg.Result := 0
  else
  begin
    S := FLines.Text; // convert to ANSI
    Msg.Result := Min(Length(S) + 1, Msg.TextMax);
    if Msg.Result > 0 then
      Move(S[1], Msg.Text^, Msg.Result);
  end;
end;

function TJvCustomWideEditor.CanSelectAll: Boolean;
var
  MaxCol, MaxLine: Integer;
begin
  MaxLine := FLines.Count - 1;
  if MaxLine > 0 then
    MaxCol := Length(FLines[MaxLine])
  else
    MaxCol := 0;
  Result := (FSelection.SelBegX > 0) or (FSelection.SelBegY > 0) or
            (FSelection.SelEndX < MaxCol) or (FSelection.SelEndY < MaxLine);
end;

function TJvCustomWideEditor.CanUndo: Boolean;
begin
  Result := FUndoBuffer.CanUndo;
end;

function TJvCustomWideEditor.HasSelection: Boolean;
begin
  Result := FSelection.IsSelected and not IsEmptySelection;
end;

function TJvCustomWideEditor.CanRedo: Boolean;
begin
  Result := FUndoBuffer.CanRedo;
end;

procedure TJvCustomWideEditor.Redo;
begin
  FUndoBuffer.Redo;
end;

procedure TJvCustomWideEditor.Undo;
begin
  FUndoBuffer.Undo;
end;

function TJvCustomWideEditor.GetLinesAnsi: TStrings;
begin
  if FLinesAnsi = nil then
   // create redirected "Ansi to Unicode" list
    FLinesAnsi := TAnsiToWideStrings.Create(FLines);
  Result := FLinesAnsi;
end;

procedure TJvCustomWideEditor.SetLinesAnsi(Value: TStrings);
begin
  FLines.Assign(Value); // assign to the WideString list
end;

{$IFDEF VCL}
type
  TWinControlAccess = class(TWinControl);

procedure TJvCustomWideEditor.WMChar(var Message: TWMChar);
 // replace TWinControl.WMChar to support Unicode keys
var
  Form: TCustomForm;
  Ch: WideChar;
begin
  Form := GetParentForm(Self);
  if (Form <> nil) and Form.KeyPreview and
    TWinControlAccess(Form).DoKeyPress(Message) then Exit;
  if not (csNoStdEvents in ControlStyle) then
    with Message do
    begin
      Ch := WideChar(CharCode);
      KeyPress(Ch);
      CharCode := Word(Ch);
      if WideChar(CharCode) = #0 then Exit;
    end;
  with TMessage(Message) do
    Message.Result := DefWindowProc(Handle, Msg, WParam, LParam);
end;
{$ENDIF VCL}

{ TAnsiToWideStrings }

constructor TAnsiToWideStrings.Create(AWideStrings: TWideStrings);
begin
  inherited Create;
  FWideStrings := AWideStrings;
end;

destructor TAnsiToWideStrings.Destroy;
begin
  FDestroying := True; // do not clear the linked list
  inherited Destroy;
end;

procedure TAnsiToWideStrings.Clear;
begin
  if not FDestroying then
    FWideStrings.Clear;
end;

procedure TAnsiToWideStrings.Delete(Index: Integer);
begin
  if Assigned(FWideStrings) then
    FWideStrings.Delete(Index);
end;

procedure TAnsiToWideStrings.Exchange(Index1, Index2: Integer);
begin
  FWideStrings.Exchange(Index1, Index2);
end;

function TAnsiToWideStrings.Get(Index: Integer): string;
begin
  Result := FWideStrings.Strings[Index]; // convert Wide to Ansi
end;

function TAnsiToWideStrings.GetCapacity: Integer;
begin
  Result := FWideStrings.Capacity;
end;

function TAnsiToWideStrings.GetCount: Integer;
begin
  Result := FWideStrings.Count;
end;

function TAnsiToWideStrings.GetObject(Index: Integer): TObject;
begin
  Result := FWideStrings.Objects[Index];
end;

function TAnsiToWideStrings.IndexOf(const S: string): Integer;
begin
  Result := FWideStrings.IndexOf(S);
end;

procedure TAnsiToWideStrings.Insert(Index: Integer; const S: string);
begin
  FWideStrings.Insert(Index, S);
end;

procedure TAnsiToWideStrings.Move(CurIndex, NewIndex: Integer);
begin
  FWideStrings.Move(CurIndex, NewIndex);
end;

procedure TAnsiToWideStrings.Put(Index: Integer; const S: string);
begin
  FWideStrings.Strings[Index] := S;
end;

procedure TAnsiToWideStrings.PutObject(Index: Integer; AObject: TObject);
begin
  FWideStrings.Objects[Index] := AObject;
end;

procedure TAnsiToWideStrings.SetCapacity(NewCapacity: Integer);
begin
  FWideStrings.Capacity := NewCapacity;
end;

end.

