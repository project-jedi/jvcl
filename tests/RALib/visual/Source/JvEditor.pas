{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : TJvEditor
description : 'Delphi IDE'-like Editor

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

{ history
 (JVCL Library versions) :
  1.00:
    - first release;
  1.01:
    - reduce caret blinking - method KeyUp;
    - fix bug with setting SelLength to 0;
    - changing SelStart now reset SelLength to 0;
    - very simple tab - two blanks;
  1.02:
    - SmartTab;
    - KeepTrailingBlanks;
    - CursorBeyondEOF;
    - AutoIndent;
    - BackSpaceUnindents;
    - two-key commands;
    - automatically expands tabs when setting Lines property;
  1.04:
    - some bugs fixed in Completion;
    - fix bug with reading SelLength property;
    - new method TJvEditorStrings .SetLockText;
    - new dynamic method TextAllChanged;
  1.11:
    - method StatusChanged;
    - fixed bug with setting Lines.Text property;
    - new method GetText with TIEditReader syntax;
  1.14:
    - selected color intialized with system colors;
  1.17:
    some improvements and bug fixes by Rafal Smotrzyk - rsmotrzyk@mikroplan.com.pl :
    - AutoIndent now worked when SmartTab Off;
    - method GetTextLen for TMemo compatibility;
    - Indent, Unindent commands;
    - WM_COPY, WM_CUT, WM_PASTE message handling;
  1.17.1:
    - painting and scrolling changed:
      bug with scrolling JvEditor if other StayOnTop
      window overlapes JvEditor window  FIXED;
    - right click now not unselect text;
    - changing RightMargin, RightMarginVisible and RightMarginColor
      Invalidates window;
  1.17.2:
   another good stuf by Rafal Smotrzyk - rsmotrzyk@mikroplan.com.pl :
    - fixed bug with backspace pressed when text selected;
    - fixed bug with disabling Backspace Unindents when SmartTab off;
    - fixed bug in GetTabStop method when SmartTab off;
    - new commands: DeleteWord, DeleteLine, ToUpperCase, ToLowerCase;
  1.17.3:
    - TabStops;
  1.17.4:
    - undo for selection modifiers;
    - UndoBuffer.BeginCompound, UndoBuffer.EndCompound for
      compound commands, that must interpreted by UndoBuffer as one operation;
      now not implemented, but must be used for feature compatibility;
    - fixed bug with undoable Delete on end of line;
    - new command ChangeCase;
  1.17.5:
    - UndoBuffer.BeginCompound, UndoBuffer.EndCompound fully implemented;
    - UndoBuffer property in TJvCustomEditor;
  1.17.6:
    - fixed bug with compound undo;
    - fixed bug with scrolling (from v 1.171);
  1.17.7:
    - UndoBuffer.BeginCompound and UndoBuffer.EndCompound moved to TJvCustomEditor;
    - Macro support: BeginRecord, EndRecord, PlayMacro; not complete;
    - additional support for compound operations: prevent updating and other;
  1.17.8:
    - bug fixed with compound commands in macro;
  1.21.2:
    - fixed bug with pressing End-key if CursorBeoyondEOF enabled
      (greetings to Martijn Laan)
  1.21.4:
    - fixed bug in commands ecNextWord and ecPrevWord
      (greetings to Ildar Noureeslamov)
  1.21.6:
    - in OnGetLineAttr now it is possible to change attributes of right
    trailing blanks.
  1.23:
    - fixed bug in completion (range check error)
    (greetings to Willo vd Merwe)
  1.51.1 (JVCL Library 1.51 with Update 1):
    - methods Lines.Add and Lines.Insert now properly updates editor window.
  1.51.2 (JVCL Library 1.51 with Update 2):
    - "Courier New" is default font now.
  1.51.3 (JVCL Library 1.51 with Update 2)::
    - fixed bug: double click on empty editor raise exception;
    - fixed bug: backspace at EOF raise exception;
    - fixed bug: gutter not repainted on vertical scrolling;
  1.53:
    - fixed bug: GetWordOnCaret returns invalid word if caret stays on start of word;
  1.54.1:
    - new: undo now works in overwrite mode;
  1.54.2:
    - fixed bug: double click not selects word on first line;
    - selection work better after consecutive moving to begin_of_line and
      end_of_line, and in other cases;
    - 4 block format supported now: NonInclusive (default), Inclusive,
      Line (initial support), Column;
    - painting was improved;
  1.60:
    - DblClick work better (thanks to Constantin M. Lushnikov);
    - fixed bug: caret moved when mouse moves over JvEditor after
      click on any other windows placed over JvEditor, which loses focus
      after this click; (anyone understand me ? :)
    - bug fixed: accelerator key do not work on window,
      where JvEditor is placed (thanks to Luis David Cardenas Bucio);
  1.61:
    - support for mouse with wheel (thanks to Michael Serpik);
    - ANY font can be used (thanks to Rients Politiek);
    - bug fixed: completion ranges error on first line
      (thanks to Walter Campelo);
    - new functions: CanCopy, CanPaste, CanCut in TJvCustomEditor
      and function CanUndo in TUndoBuffer (TJvCustomEditor.UndoBuffer);
  2.00:
    - removed dependencies from JvUtils.pas unit;
    - bugfixed: TJvDeleteUndo  and TJvBackspaceUndo  do not work always properly
      (thanks to Pavel Chromy);
    - bugfixed: workaround bug with some fonts in Win9x
      (thanks to Dmitry Rubinstain);

}


{
  to do:
   1) добавить событие OnGutterClick(Sender: TObject; Line: Integer);
   2) добавить поддержку <Persistent Block> !!!!!????;
}

unit JvEditor;

{$DEFINE DEBUG}
{$IFNDEF RAEDITOR_NOEDITOR}
{$DEFINE RAEDITOR_EDITOR} {if not RAEDITOR_EDITOR then mode = Viewer}
{$ENDIF}
{$DEFINE RAEDITOR_DEFLAYOT} {set default keyboard layot}
{$IFNDEF RAEDITOR_NOUNDO}
{$DEFINE RAEDITOR_UNDO} {enable undo}
{$ENDIF}
{$IFNDEF RAEDITOR_NOCOMPLETION}
{$DEFINE RAEDITOR_COMPLETION} {enable code completion}
{$ENDIF}

{$IFNDEF RAEDITOR_EDITOR}
{$UNDEF RAEDITOR_DEFLAYOT}
{$UNDEF RAEDITOR_UNDO}
{$UNDEF RAEDITOR_COMPLETION}
{$ENDIF RAEDITOR_EDITOR}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls, ClipBrd;

const
  Max_X = 1024; {max symbols per row}
  Max_X_Scroll = 256;
  {max symbols per row for scrollbar - max кол-во символов в строке для прокрутки}
  GutterRightMargin = 2;

  WM_EDITCOMMAND = WM_USER + $101;
 {$IFNDEF COMPILER3_UP}
  WM_MOUSEWHEEL       = $020A;
 {$ENDIF COMPILER3_UP}

type

 {$IFNDEF COMPILER4_UP}
  TWMMouseWheel = packed record
    Msg: Cardinal;
    Keys: SmallInt;
    WheelDelta: SmallInt;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;
 {$ENDIF COMPILER4_UP}

  TCellRect = record
    Width: integer;
    Height: integer;
  end;

  TLineAttr = record
    FC, BC: TColor;
    Style: TFontStyles;
  end;

  TJvCustomEditor = class;

  TLineAttrs = array[0..Max_X] of TLineAttr;
  TOnGetLineAttr = procedure(Sender: TObject; var Line: string; index: integer;
    var Attrs: TLineAttrs) of object;
  TOnChangeStatus = TNotifyEvent;

  TJvEditorStrings  = class(TStringList)
  private
    FRAEditor: TJvCustomEditor;
    procedure StringsChanged(Sender: TObject);
    procedure SetInternal(index: integer; value: string);
    procedure ReLine;
    procedure SetLockText(Text: string);
  protected
    procedure SetTextStr(const Value: string); override;
    procedure Put(Index: Integer; const S: string); override;
  public
    constructor Create;
    function Add(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    property Internal[index: integer]: string write SetInternal;
  end;

  TModifiedAction = (maInsert, maDelete, maInsertColumn, maDeleteColumn);

  TBookMark = record
    X, Y: integer;
    Valid: boolean;
  end;
  TBookMarkNum = 0..9;
  TBookMarks = array[TBookMarkNum] of TBookMark;

  TJvEditorClient  = class
  private
    FRAEditor: TJvCustomEditor;
    Top: integer;
    function Left: integer;
    function Height: integer;
    function Width: integer;
    function ClientWidth: integer;
    function ClientHeight: integer;
    function ClientRect: TRect;
    function BoundsRect: TRect;
    function GetCanvas: TCanvas;
    property Canvas: TCanvas read GetCanvas;
  end;

  TJvGutter  = class
  private
    FRAEditor: TJvCustomEditor;
  public
    procedure Paint;
    procedure Invalidate;
  end;
  TOnPaintGutter = procedure(Sender: TObject; Canvas: TCanvas) of object;

  TEditCommand = word;
  TMacro = string; { used as buffer }

  TJvEditKey  = class
  public
    Key1, Key2: Word;
    Shift1, Shift2: TShiftState;
    Command: TEditCommand;
    constructor Create(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState);
    constructor Create2(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState; const AKey2: word;
      const AShift2: TShiftState);
  end;

  TJvKeyboard  = class
  private
    List: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState);
    procedure Add2(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState; const AKey2: word;
      const AShift2: TShiftState);
    procedure Clear;
    function Command(const AKey: word; const AShift: TShiftState): TEditCommand;
    function Command2(const AKey1: word; const AShift1: TShiftState;
      const AKey2: word; const AShift2: TShiftState): TEditCommand;
    {$IFDEF RAEDITOR_DEFLAYOT}
    procedure SetDefLayot;
    {$ENDIF RAEDITOR_DEFLAYOT}
  end;

  EJvEditorError  = class(Exception);

  {$IFDEF RAEDITOR_UNDO}
  TUndoBuffer = class;

  TUndo = class
  private
    FRAEditor: TJvCustomEditor;
    function UndoBuffer: TUndoBuffer;
  public
    constructor Create(ARAEditor: TJvCustomEditor);
    procedure Undo; dynamic; abstract;
    procedure Redo; dynamic; abstract;
  end;

  TUndoBuffer = class(TList)
  private
    FRAEditor: TJvCustomEditor;
    FPtr: integer;
    InUndo: boolean;
    function LastUndo: TUndo;
    function IsNewGroup(AUndo: TUndo): boolean;
  public
    procedure Add(AUndo: TUndo);
    procedure Undo;
    procedure Redo;
    procedure Clear; {$IFDEF COMPILER35_Up}override; {$ENDIF}
    procedure Delete;
    function CanUndo: boolean;
  end;
  {$ENDIF RAEDITOR_UNDO}

  {$IFDEF RAEDITOR_COMPLETION}
  TJvCompletion  = class;
  TOnCompletion = procedure(Sender: TObject; var Cancel: boolean) of object;
  TOnCompletionApply = procedure(Sender: TObject; const OldString: string; var NewString: string) of object;
  {$ENDIF RAEDITOR_COMPLETION}

  TTabStop = (tsTabStop, tsAutoIndent);

{ Borland Block Type:
  00 - inclusive;
  01 - line;
  02 - column;
  03 - noninclusive; }
  
  TSelBlockFormat = (bfInclusive, bfLine, bfColumn, bfNonInclusive);


  TJvControlScrollBar95 = class
  private
    FKind: TScrollBarKind;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FSmallChange: TScrollBarInc;
    FLargeChange: TScrollBarInc;
    FPage : integer;
    FHandle : hWnd;
    FOnScroll: TScrollEvent;
    procedure SetParam(index, Value: Integer);
  protected
    procedure Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer); dynamic;
  public
    constructor Create;
    procedure SetParams(AMin, AMax, APosition, APage : integer);
    procedure DoScroll(var Message: TWMScroll);

    property Kind: TScrollBarKind read FKind write FKind default sbHorizontal;
    property SmallChange: TScrollBarInc read FSmallChange write FSmallChange default 1;
    property LargeChange: TScrollBarInc read FLargeChange write FLargeChange default 1;
    property Min  : Integer index 0 read FMin write SetParam default 0;
    property Max  : Integer index 1 read FMax write SetParam default 100;
    property Position : Integer index 2 read FPosition write SetParam default 0;
    property Page : integer index 3 read FPage write SetParam;
    property Handle : hWnd read FHandle write FHandle;
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
  end;

  {*** TJvCustomEditor }

  TJvCustomEditor = class(TCustomControl)
  private
    { internal objects }
    FLines: TJvEditorStrings ;
    scbHorz: TJvControlScrollBar95;
    scbVert: TJvControlScrollBar95;
    EditorClient: TJvEditorClient ;
    FGutter: TJvGutter ;
    FKeyboard: TJvKeyboard ;
    FUpdateLock: integer;
    {$IFDEF RAEDITOR_UNDO}
    FUndoBuffer: TUndoBuffer;
    FGroupUndo: boolean;
    FUndoAfterSave: boolean;
    {$ENDIF RAEDITOR_UNDO}
    {$IFDEF RAEDITOR_COMPLETION}
    FCompletion: TJvCompletion ;
    {$ENDIF RAEDITOR_COMPLETION}

    { internal - Columns and rows attributes }
    FCols, FRows: integer;
    FLeftCol, FTopRow: integer;
    // FLeftColMax, FTopRowMax : integer;
    FLastVisibleCol, FLastVisibleRow: integer;
    FCaretX, FCaretY: integer;
    FVisibleColCount: integer;
    FVisibleRowCount: integer;

    { internal - other flags and attributes }
    FAllRepaint: boolean;
    FCellRect: TCellRect;
    {$IFDEF RAEDITOR_EDITOR}
    IgnoreKeyPress: boolean;
    {$ENDIF RAEDITOR_EDITOR}
    WaitSecondKey: Boolean;
    Key1: Word;
    Shift1: TShiftState;

    { internal - selection attributes }
    FSelected: boolean;
    FSelBlockFormat: TSelBlockFormat;
    FSelBegX, FSelBegY, FSelEndX, FSelEndY: integer;
    FUpdateSelBegY, FUpdateSelEndY: integer;
    FSelStartX, FSelStartY: integer;
    FclSelectBC, FclSelectFC: TColor;

    { mouse support }
    timerScroll: TTimer;
    MouseMoveY, MouseMoveXX, MouseMoveYY: integer;
    FDoubleClick: Boolean;
    FMouseDowned: Boolean;

    { internal }
    FTabPos: array[0..Max_X] of boolean;
    FTabStops: string;
    MyDi: array[0..1024] of Integer;

    { internal - primary for TIReader support }
    FEditBuffer: string;
    FPEditBuffer: PChar;
    FEditBufferSize: Integer;

    FCompound: Integer;
    { FMacro - buffer of TEditCommand, each command represents by two chars }
    FMacro: TMacro;
    FDefMacro: TMacro;

    { visual attributes - properties }
    FBorderStyle: TBorderStyle;
    FGutterColor: TColor;
    FGutterWidth: integer;
    FRightMarginVisible: boolean;
    FRightMargin: integer;
    FRightMarginColor: TColor;
    FScrollBars: TScrollStyle;
    FDoubleClickLine: boolean;
    FSmartTab: Boolean;
    FBackSpaceUnindents: Boolean;
    FAutoIndent: Boolean;
    FKeepTrailingBlanks: Boolean;
    FCursorBeyondEOF: Boolean;
    FHideCaret: Boolean;


    { non-visual attributes - properties }
    FInsertMode: boolean;
    FReadOnly: boolean;
    FModified: boolean;
    FRecording: boolean;

    { Events }
    FOnGetLineAttr: TOnGetLineAttr;
    FOnChange: TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnChangeStatus: TOnChangeStatus;
    FOnScroll: TNotifyEvent;
    FOnResize: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnPaintGutter: TOnPaintGutter;
    {$IFDEF RAEDITOR_COMPLETION}
    FOnCompletionIdentifer: TOnCompletion;
    FOnCompletionTemplate: TOnCompletion;
    FOnCompletionDrawItem: TDrawItemEvent;
    FOnCompletionMeasureItem: TMeasureItemEvent;
    FOnCompletionApply: TOnCompletionApply;
    {$ENDIF RAEDITOR_COMPLETION}

    { internal message processing }
    {$IFNDEF COMPILER4_UP}
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    {$ENDIF COMPILER4_UP}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMEditCommand(var Message: TMessage); message WM_EDITCOMMAND;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;

    procedure UpdateEditorSize;
    {$IFDEF RAEDITOR_COMPLETION}
    procedure DoCompletionIdentifer(var Cancel: boolean);
    procedure DoCompletionTemplate(var Cancel: boolean);
    {$ENDIF RAEDITOR_COMPLETION}
    procedure ScrollTimer(Sender: TObject);

    procedure ReLine;
    function GetDefTabStop(const X: integer; const Next: Boolean): Integer;
    function GetTabStop(const X, Y: Integer; const What: TTabStop;
      const Next: Boolean): Integer;
    function GetBackStop(const X, Y: Integer): Integer;

    procedure TextAllChangedInternal(const Unselect: Boolean);

    { property }
    procedure SetGutterWidth(AWidth: integer);
    procedure SetGutterColor(AColor: TColor);
    function GetLines: TStrings;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetLines(ALines: TStrings);
    function GetSelStart: integer;
    procedure SetSelStart(const ASelStart: integer);
    procedure SetSelLength(const ASelLength: integer);
    function GetSelLength: integer;
    procedure SetSelBlockFormat(const Value: TSelBlockFormat);
    procedure SetMode(index: integer; Value: boolean);
    procedure SetCaretPosition(const index, Pos: integer);
    procedure SetCols(ACols: integer);
    procedure SetRows(ARows: integer);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetRightMarginVisible(Value: boolean);
    procedure SetRightMargin(Value: integer);
    procedure SetRightMarginColor(Value: TColor);
  protected
    LineAttrs: TLineAttrs;
    procedure Resize; {$IFDEF Delphi4_Up}override; {$ELSE}dynamic; {$ENDIF COMPILER4_UP}
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var
      ScrollPos: Integer);
    procedure Scroll(const Vert: boolean; const ScrollPos: integer);
    procedure PaintLine(const Line: integer; ColBeg, ColEnd: integer);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$IFDEF RAEDITOR_EDITOR}
    procedure KeyPress(var Key: Char); override;
    procedure InsertChar(const Key: Char);
    {$ENDIF RAEDITOR_EDITOR}
    function GetClipboardBlockFormat: TSelBlockFormat;
    procedure SetClipboardBlockFormat(const Value: TSelBlockFormat);
    procedure SetSel(const SelX, SelY: integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure DblClick; override;
    procedure DrawRightMargin;
    procedure PaintSelection;
    procedure SetUnSelected;
    procedure Mouse2Cell(const X, Y: integer; var CX, CY: integer);
    procedure Mouse2Caret(const X, Y: integer; var CX, CY: integer);
    procedure CaretCoord(const X, Y: integer; var CX, CY: integer);
    function PosFromMouse(const X, Y: integer): integer;
    procedure SetLockText(const Text: string);
    function ExpandTabs(const S: string): string;
    // add by patofan
   {$IFDEF COMPILER3_UP}
    function CheckDoubleByteChar(var x: integer; y: integer; ByteType : TMbcsByteType; delta_inc: integer): boolean;
   {$ENDIF COMPILER3_UP}
    // ending add by patofan

    {$IFDEF RAEDITOR_UNDO}
    procedure NotUndoable;
    {$ENDIF RAEDITOR_UNDO}
    procedure SetCaretInternal(X, Y: integer);
    procedure ValidateEditBuffer;

    {$IFDEF RAEDITOR_EDITOR}
    procedure ChangeBookMark(const BookMark: TBookMarkNum; const Valid:
      boolean);
    {$ENDIF RAEDITOR_EDITOR}
    procedure BeginRecord;
    procedure EndRecord(var AMacro: TMacro);
    procedure PlayMacro(const AMacro: TMacro);

    { triggers for descendants }
    procedure Changed; dynamic;
    procedure TextAllChanged; dynamic;
    procedure StatusChanged; dynamic;
    procedure SelectionChanged; dynamic;
    procedure GetLineAttr(var Str: string; Line, ColBeg, ColEnd: integer); virtual;
    procedure GetAttr(Line, ColBeg, ColEnd: integer); virtual;
    procedure ChangeAttr(Line, ColBeg, ColEnd: integer); virtual;
    procedure GutterPaint(Canvas: TCanvas); dynamic;
    procedure BookmarkCnanged(BookMark: integer); dynamic;
    {$IFDEF RAEDITOR_COMPLETION}
    procedure CompletionIdentifer(var Cancel: boolean); dynamic;
    procedure CompletionTemplate(var Cancel: boolean); dynamic;
    {$ENDIF RAEDITOR_COMPLETION}
    { don't use method TextModified: see comment at method body }
    procedure TextModified(Pos: integer; Action: TModifiedAction; Text: string);
      dynamic;
    property Gutter: TJvGutter  read FGutter;
  public
    BookMarks: TBookMarks;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetLeftTop(ALeftCol, ATopRow: integer);
    procedure ClipBoardCopy;
    procedure ClipBoardPaste;
    procedure ClipBoardCut;
    function CanCopy: Boolean;
    function CanPaste: Boolean;
    function CanCut: Boolean;
    procedure DeleteSelected;
    function CalcCellRect(const X, Y: integer): TRect;
    procedure SetCaret(X, Y: integer);
    procedure CaretFromPos(const Pos: integer; var X, Y: integer);
    function PosFromCaret(const X, Y: integer): integer;
    procedure PaintCaret(const bShow: boolean);
    function GetTextLen: Integer;
    function GetSelText: string;
    procedure SetSelText(const AValue: string);
    function GetWordOnCaret: string;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure MakeRowVisible(ARow: Integer);

    procedure Command(ACommand: TEditCommand); virtual;
    procedure PostCommand(ACommand: TEditCommand);
    {$IFDEF RAEDITOR_EDITOR}
    procedure InsertText(const Text: string);
    procedure ReplaceWord(const NewString: string);
    procedure ReplaceWord2(const NewString: string);
    {$ENDIF}
    procedure BeginCompound;
    procedure EndCompound;

    function GetText(Position: Longint; Buffer: PChar; Count: Longint): Longint;
    property LeftCol: integer read FLeftCol;
    property TopRow: integer read FTopRow;
    property VisibleColCount: integer read FVisibleColCount;
    property VisibleRowCount: integer read FVisibleRowCount;
    property LastVisibleCol: integer read FLastVisibleCol;
    property LastVisibleRow: integer read FLastVisibleRow;
    property Cols: integer read FCols write SetCols;
    property Rows: integer read FRows write SetRows;
    property CaretX: integer index 0 read FCaretX write SetCaretPosition;
    property CaretY: integer index 1 read FCaretY write SetCaretPosition;
    property Modified: boolean read FModified write FModified;
    property SelBlockFormat: TSelBlockFormat read FSelBlockFormat write SetSelBlockFormat default bfNonInclusive;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelText: string read GetSelText write SetSelText;
    property Keyboard: TJvKeyboard  read FKeyboard;
    property CellRect: TCellRect read FCellRect;
    {$IFDEF RAEDITOR_UNDO}
    property UndoBuffer: TUndoBuffer read FUndoBuffer;
    property GroupUndo: boolean read FGroupUndo write FGroupUndo default True;
    property UndoAfterSave: boolean read FUndoAfterSave write FUndoAfterSave; 
    {$ENDIF RAEDITOR_UNDO}
    property Recording: boolean read FRecording;
  public { published in descendants }
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Lines: TStrings read GetLines write SetLines;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property Cursor default crIBeam;
    property Color default clWindow;

    property GutterWidth: integer read FGutterWidth write SetGutterWidth;
    property GutterColor: TColor read FGutterColor write SetGutterColor default clBtnFace;
    property RightMarginVisible: boolean read FRightMarginVisible write SetRightMarginVisible default true;
    property RightMargin: integer read FRightMargin write SetRightMargin default 80;
    property RightMarginColor: TColor read FRightMarginColor write SetRightMarginColor default clBtnFace;
    property InsertMode: boolean index 0 read FInsertMode write SetMode default true;
    property ReadOnly: boolean index 1 read FReadOnly write SetMode default false;
    property DoubleClickLine: boolean read FDoubleClickLine write FDoubleClickLine default false;
    {$IFDEF RAEDITOR_COMPLETION}
    property Completion: TJvCompletion  read FCompletion write FCompletion;
    {$ENDIF RAEDITOR_COMPLETION}
    property TabStops: string read FTabStops write FTabStops;
    property SmartTab: Boolean read FSmartTab write FSmartTab default true;
    property BackSpaceUnindents: Boolean read FBackSpaceUnindents write FBackSpaceUnindents default true;
    property AutoIndent: Boolean read FAutoIndent write FAutoIndent default true;
    property KeepTrailingBlanks: Boolean read FKeepTrailingBlanks write FKeepTrailingBlanks default false;
    property CursorBeyondEOF: Boolean read FCursorBeyondEOF write FCursorBeyondEOF default false;
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
    {$IFDEF RAEDITOR_COMPLETION}
    property OnCompletionIdentifer: TOnCompletion read FOnCompletionIdentifer write FOnCompletionIdentifer;
    property OnCompletionTemplate: TOnCompletion read FOnCompletionTemplate write FOnCompletionTemplate;
    property OnCompletionDrawItem: TDrawItemEvent read FOnCompletionDrawItem write FOnCompletionDrawItem;
    property OnCompletionMeasureItem: TMeasureItemEvent read FOnCompletionMeasureItem write FOnCompletionMeasureItem;
    property OnCompletionApply: TOnCompletionApply read FOnCompletionApply write FOnCompletionApply;
    {$ENDIF RAEDITOR_COMPLETION}
    {$IFDEF COMPILER4_UP}
    property DockManager;
    {$ENDIF COMPILER4_UP}
  end;

  TJvEditor = class(TJvCustomEditor)
  published
    property BorderStyle;
    property Lines;
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
    {$IFDEF RAEDITOR_COMPLETION}
    property Completion;
    {$ENDIF RAEDITOR_COMPLETION}
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
    {$IFDEF RAEDITOR_COMPLETION}
    property OnCompletionIdentifer;
    property OnCompletionTemplate;
    property OnCompletionDrawItem;
    property OnCompletionMeasureItem;
    property OnCompletionApply;
    {$ENDIF RAEDITOR_COMPLETION}

    { TCustomControl }
    property Align;
    property Enabled;
    property Color;
    property Ctl3D;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property Visible;
    {$IFDEF COMPILER4_UP}
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
    {$ENDIF COMPILER4_UP}
  end;

  {$IFDEF RAEDITOR_COMPLETION}
  TCompletionList = (cmIdentifers, cmTemplates);

  TJvCompletion  = class(TPersistent)
  private
    FRAEditor: TJvCustomEditor;
    FPopupList: TListBox;
    FIdentifers: TStrings;
    FTemplates: TStrings;
    FItems: TStringList;
    FItemIndex: integer;
    FMode: TCompletionList;
    FDefMode: TCompletionList;
    FItemHeight: integer;
    FTimer: TTimer;
    FEnabled: boolean;
    FVisible: boolean;
    FDropDownCount: integer;
    FDropDownWidth: integer;
    FListBoxStyle: TListBoxStyle;
    FCaretChar: char;
    FCRLF: string;
    FSeparator: string;
    function DoKeyDown(Key: Word; Shift: TShiftState): boolean;
    procedure DoKeyPress(Key: Char);
    procedure OnTimer(Sender: TObject);
    procedure FindSelItem(var Eq: boolean);
    procedure ReplaceWord(const NewString: string);

    procedure SetStrings(index: integer; AValue: TStrings);
    function GetItemIndex: integer;
    procedure SetItemIndex(AValue: integer);
    function GetInterval: cardinal;
    procedure SetInterval(AValue: cardinal);
    procedure MakeItems;
    function GetItems: TStrings;
  public
    constructor Create2(ARAEditor: TJvCustomEditor);
    destructor Destroy; override;
    procedure DropDown(const AMode: TCompletionList; const ShowAlways: boolean);
    procedure DoCompletion(const AMode: TCompletionList);
    procedure CloseUp(const Apply: boolean);
    procedure SelectItem;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property Visible: boolean read FVisible write FVisible;
    property Mode: TCompletionList read FMode write FMode;
    property Items: TStringList read FItems;
  published
    property DropDownCount: integer read FDropDownCount write FDropDownCount
      default 6;
    property DropDownWidth: integer read FDropDownWidth write FDropDownWidth
      default 300;
    property Enabled: boolean read FEnabled write FEnabled default false;
    property Identifers: TStrings index 0 read FIdentifers write SetStrings;
    property Templates: TStrings index 1 read FTemplates write SetStrings;
    property ItemHeight: integer read FItemHeight write FItemHeight;
    property Interval: cardinal read GetInterval write SetInterval;
    property ListBoxStyle: TListBoxStyle read FListBoxStyle write FListBoxStyle;
    property CaretChar: char read FCaretChar write FCaretChar;
    property CRLF: string read FCRLF write FCRLF;
    property Separator: string read FSeparator write FSeparator;
  end;
  {$ENDIF RAEDITOR_COMPLETION}

const

 { Editor commands }
 { When add new commands, please add them into JvInterpreter_JvEditor.pas unit also ! }

  ecCharFirst = $00;
  ecCharLast = $FF;
  ecCommandFirst = $100;
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
  {Cursor по словам}
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

  ecDeleteSelected = ecInsertPara + 10;
  ecClipboardCopy = ecInsertPara + 11;
  ecClipboardCut = ecClipboardCopy + 1;
  ecClipBoardPaste = ecClipboardCopy + 2;

  ecDeleteLine = ecClipBoardPaste + 1;
  ecDeleteWord = ecDeleteLine + 1;

  ecToUpperCase = ecDeleteLine + 2;
  ecToLowerCase = ecToUpperCase + 1;
  ecChangeCase = ecToUpperCase + 2;

  ecUndo = ecChangeCase + 1;
  ecRedo = ecUndo + 1;
  ecBeginCompound = ecUndo + 2; { not implemented }
  ecEndCompound = ecUndo + 3; { not implemented }

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

  ecCompletionIdentifers = ecGotoBookmark9 + 1;
  ecCompletionTemplates = ecCompletionIdentifers + 1;

  ecRecordMacro = ecCompletionTemplates + 1;
  ecPlayMacro = ecRecordMacro + 1;
  ecBeginRecord = ecRecordMacro + 2;
  ecEndRecord = ecRecordMacro + 3;

  twoKeyCommand = High(word);

implementation

uses Consts, JvCtlConst, JvStrUtil;

{$IFDEF RAEDITOR_UNDO}

type

  TJvCaretUndo  = class(TUndo)
  private
    FCaretX, FCaretY: integer;
  public
    constructor Create(ARAEditor: TJvCustomEditor; ACaretX, ACaretY: integer);
    procedure Undo; override;
    procedure Redo; override;
  end;

  TJvInsertUndo  = class(TJvCaretUndo )
  private
    FText: string;
  public
    constructor Create(ARAEditor: TJvCustomEditor; ACaretX, ACaretY: integer;
      AText: string);
    procedure Undo; override;
  end;

  TJvOverwriteUndo  = class(TJvCaretUndo )
  private
    FOldText: string;
    FNewText: string;
  public
    constructor Create(ARAEditor: TJvCustomEditor; ACaretX, ACaretY: integer;
      AOldText, ANewText: string);
    procedure Undo; override;
  end;

  TJvReLineUndo  = class(TJvInsertUndo );

  TJvInsertTabUndo  = class(TJvInsertUndo );

  TJvInsertColumnUndo  = class(TJvInsertUndo )
  public
    procedure Undo; override;
  end;

  TJvDeleteUndo  = class(TJvInsertUndo )
  public
    procedure Undo; override;
  end;

  TJvDeleteTrailUndo  = class(TJvDeleteUndo );

  TJvBackspaceUndo  = class(TJvDeleteUndo )
  public
    procedure Undo; override;
  end;

  TJvReplaceUndo  = class(TJvCaretUndo )
  private
    FBeg, FEnd: integer;
    FText, FNewText: string;
  public
    constructor Create(ARAEditor: TJvCustomEditor; ACaretX, ACaretY: integer;
      ABeg, AEnd: integer; AText, ANewText: string);
    procedure Undo; override;
  end;

  TJvDeleteSelectedUndo  = class(TJvDeleteUndo )
  private
    FSelBegX, FSelBegY, FSelEndX, FSelEndY: integer;
    FSelBlockFormat: TSelBlockFormat;
  public
    constructor Create(ARAEditor: TJvCustomEditor; ACaretX, ACaretY: integer;
      AText: string; ASelBlockFormat: TSelBlockFormat;
      ASelBegX, ASelBegY, ASelEndX, ASelEndY: integer);
    procedure Undo; override;
  end;

  TJvSelectUndo  = class(TJvCaretUndo )
  private
    FSelected: boolean;
    FSelBlockFormat: TSelBlockFormat;
    FSelBegX, FSelBegY, FSelEndX, FSelEndY: integer;
  public
    constructor Create(ARAEditor: TJvCustomEditor; ACaretX, ACaretY: integer;
      ASelected: boolean; ASelBlockFormat: TSelBlockFormat;
      ASelBegX, ASelBegY, ASelEndX, ASelEndY: integer);
    procedure Undo; override;
  end;

  TUnselectUndo = class(TJvSelectUndo );

  TJvBeginCompoundUndo  = class(TUndo)
  public
    procedure Undo; override;
  end;

  TJvEndCompoundUndo  = class(TJvBeginCompoundUndo );

  {$ENDIF RAEDITOR_UNDO}

var
  BlockTypeFormat: Integer;

{********************* Debug ***********************}

{
procedure Debug(const S: string);
begin
  Tracer.Writeln(S);
end;

procedure BeginTick;
begin
  Tracer.TimerStart(1);
end;

procedure EndTick;
begin
  Tracer.TimerStop(1);
end;
}
{#################### Debug ######################}

procedure Err;
begin
  MessageBeep(0);
end;

function KeyPressed(VK : integer) : boolean;
begin
  Result := GetKeyState(VK) and $8000 = $8000;
end;

function Max(x,y:integer):integer;
begin
  if x > y then Result := x else Result := y;
end;

function Min(x,y:integer):integer;
begin
  if x < y then Result := x else Result := y;
end;

{$IFDEF COMPILER2}
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,1
        SHR     ECX,1
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;
{$ENDIF COMPILER2}

{************************* TJvControlScrollBar95 ****************************}
constructor TJvControlScrollBar95.Create;
begin
  FPage := 1;
  FSmallChange := 1;
  FLargeChange := 1;
end;

const
  SBKIND : array[TScrollBarKind] of integer = (SB_HORZ, SB_VERT);

procedure TJvControlScrollBar95.SetParams(AMin, AMax, APosition, APage : integer);
var
  SCROLLINFO : TSCROLLINFO;
begin
  if AMax < AMin then
    raise EInvalidOperation.Create(SScrollBarRange);
  if APosition < AMin then APosition := AMin;
  if APosition > AMax then APosition := AMax;
  if Handle > 0 then begin
    with SCROLLINFO do begin
      cbSize := sizeof(TSCROLLINFO);
      fMask := SIF_DISABLENOSCROLL;
      if (AMin >= 0) or (AMax >= 0) then fMask := fMask or SIF_RANGE;
      if APosition >= 0 then fMask := fMask or SIF_POS;
      if APage >= 0 then fMask := fMask or SIF_PAGE;
      nPos := APosition;
      nMin := AMin;
      nMax := AMax;
      nPage := APage;
    end;
    SetScrollInfo(
      Handle,         // handle of window with scroll bar
      SBKIND[Kind] ,  // scroll bar flag
      SCROLLINFO,     // pointer to structure with scroll parameters
      true            // redraw flag
    );
  end;
end;

procedure TJvControlScrollBar95.SetParam(index, Value: Integer);
begin
  case index of
    0 : FMin := Value;
    1 : FMax := Value;
    2 : FPosition := Value;
    3 : FPage := Value;
  end;
  if FMax < FMin then
    raise EInvalidOperation.Create(SScrollBarRange);
  if FPosition < FMin then FPosition := FMin;
  if FPosition > FMax then FPosition := FMax;
  SetParams(FMin, FMax, FPosition, FPage);
end;

{
procedure TJvControlScrollBar95.SetVisible(Value : boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Handle <> 0 then

  end;
end;
}

procedure TJvControlScrollBar95.DoScroll(var Message: TWMScroll);
var
  ScrollPos: Integer;
  NewPos: Longint;
  ScrollInfo: TScrollInfo;
begin
  with Message do
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
    if NewPos < FMin then NewPos := FMin;
    if NewPos > FMax then NewPos := FMax;
    ScrollPos := NewPos;
    Scroll(TScrollCode(ScrollCode), ScrollPos);
  end;
  Position := ScrollPos;
end;

procedure TJvControlScrollBar95.Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Assigned(FOnScroll) then FOnScroll(Self, ScrollCode, ScrollPos);
end;
{######################### TJvControlScrollBar95 #########################}

constructor TJvEditorStrings .Create;
begin
  inherited Create;
  OnChange := StringsChanged;
end;

procedure TJvEditorStrings .SetTextStr(const Value: string);
begin
  inherited SetTextStr(FRAEditor.ExpandTabs(Value));
 {$IFDEF RAEDITOR_UNDO}
  if FRAEditor.FUpdateLock = 0 then FRAEditor.NotUndoable;
 {$ENDIF RAEDITOR_UNDO}
  FRAEditor.TextAllChanged;
end;

procedure TJvEditorStrings .StringsChanged(Sender: TObject);
begin
  if FRAEditor.FUpdateLock = 0 then 
    FRAEditor.TextAllChanged;
end;

procedure TJvEditorStrings .SetLockText(Text: string);
begin
  inc(FRAEditor.FUpdateLock);
  try
    inherited SetTextStr(Text)
  finally
    dec(FRAEditor.FUpdateLock);
  end;
end;

procedure TJvEditorStrings .SetInternal(index: integer; value: string);
begin
  inc(FRAEditor.FUpdateLock);
  try
    inherited Strings[index] := Value;
  finally
    dec(FRAEditor.FUpdateLock);
  end;
end;

function TJvEditorStrings .Add(const S: string): Integer;
begin
  //inc(FRAEditor.FUpdateLock);
  try
    Result := inherited Add(FRAEditor.ExpandTabs(S));
  finally
    //dec(FRAEditor.FUpdateLock);
  end;
end;

procedure TJvEditorStrings .Insert(Index: Integer; const S: string);
begin
  //inc(FRAEditor.FUpdateLock);
  try
    inherited Insert(Index, FRAEditor.ExpandTabs(S));
  finally
    //dec(FRAEditor.FUpdateLock);
  end;
end;

procedure TJvEditorStrings .Put(Index: Integer; const S: string);
{$IFDEF RAEDITOR_UNDO}
var
  L: integer;
  {$ENDIF RAEDITOR_UNDO}
begin
  if FRAEditor.FKeepTrailingBlanks then
    inherited Put(Index, S)
  else
  begin
    {$IFDEF RAEDITOR_UNDO}
    L := Length(S) - Length(TrimRight(S));
    if L > 0 then
      TJvDeleteTrailUndo .Create(FRAEditor, Length(S), Index, Spaces(L));
    {$ENDIF RAEDITOR_UNDO}
    inherited Put(Index, TrimRight(S));
  end;
end;

procedure TJvEditorStrings .ReLine;
var
  L: Integer;
begin
  inc(FRAEditor.FUpdateLock);
  try
    {$IFDEF RAEDITOR_UNDO}
    if Count = 0 then
      L := FRAEditor.FCaretX
    else
      L := Length(Strings[Count - 1]);
    while FRAEditor.FCaretY > Count - 1 do
    begin
      TJvReLineUndo .Create(FRAEditor, L, FRAEditor.FCaretY, #13#10);
      L := 0;
      Add('');
    end;
    {$ENDIF RAEDITOR_UNDO}
    if FRAEditor.FCaretX > Length(Strings[FRAEditor.FCaretY]) then
    begin
      L := FRAEditor.FCaretX - Length(Strings[FRAEditor.FCaretY]);
      {$IFDEF RAEDITOR_UNDO}
      TJvReLineUndo .Create(FRAEditor, Length(Strings[FRAEditor.FCaretY]),
        FRAEditor.FCaretY, Spaces(L));
      {$ENDIF RAEDITOR_UNDO}
      inherited Put(FRAEditor.FCaretY, Strings[FRAEditor.FCaretY] + Spaces(L));
    end;
  finally
    dec(FRAEditor.FUpdateLock);
  end;
end; { ReLine }

function TJvEditorClient .GetCanvas: TCanvas;
begin
  Result := FRAEditor.Canvas;
end;

function TJvEditorClient .Left: integer;
begin
  Result := FRAEditor.GutterWidth + 2;
end;

function TJvEditorClient .Height: integer;
begin
  Result := FRAEditor.ClientHeight;
end;

function TJvEditorClient .Width: integer;
begin
  Result := Max(FRAEditor.ClientWidth - Left, 0);
end;

function TJvEditorClient .ClientWidth: integer;
begin
  Result := Width;
end;

function TJvEditorClient .ClientHeight: integer;
begin
  Result := Height;
end;

function TJvEditorClient .ClientRect: TRect;
begin
  Result := Bounds(Left, Top, Width, Height);
end;

function TJvEditorClient .BoundsRect: TRect;
begin
  Result := Bounds(0, 0, Width, Height);
end;

procedure TJvGutter .Invalidate;
{var
  R : TRect;}
begin
  //  Owner.Invalidate;
  //  R := Bounds(0, 0, FRAEditor.GutterWidth, FRAEditor.Height);
  //  InvalidateRect(FRAEditor.Handle, @R, false);
  Paint;
end;

procedure TJvGutter .Paint;
begin
  with FRAEditor, Canvas do
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
  with FRAEditor do
    GutterPaint(Canvas);
end;



{*********************** TJvCustomEditor ***********************}

constructor TJvCustomEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents {, csOpaque}, csDoubleClicks,
    csReplicatable];
  FInsertMode := true;
  FLines := TJvEditorStrings .Create;
  FLines.FRAEditor := self;
  FKeyboard := TJvKeyboard .Create;
  FRows := 1;
  FCols := 1;
  {$IFDEF RAEDITOR_UNDO}
  FUndoBuffer := TUndoBuffer.Create;
  FUndoBuffer.FRAEditor := Self;
  FGroupUndo := true;
  {$ENDIF RAEDITOR_UNDO}

  FRightMarginVisible := true;
  FRightMargin := 80;
  FBorderStyle := bsSingle;
  Ctl3d := true;
  Height := 40;
  Width := 150;
  ParentColor := false;
  Cursor := crIBeam;
  TabStop := true;
  FTabStops := '3 5';
  FSmartTab := true;
  FBackSpaceUnindents := true;
  FAutoIndent := true;
  FKeepTrailingBlanks := false;
  FCursorBeyondEOF := false;

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

  EditorClient := TJvEditorClient .Create;
  EditorClient.FRAEditor := Self;
  FGutter := TJvGutter .Create;
  FGutter.FRAEditor := Self;

  FLeftCol := 0;
  FTopRow := 0;
  FSelected := false;
  FCaretX := 0;
  FCaretY := 0;

  timerScroll := TTimer.Create(Self);
  timerScroll.Enabled := false;
  timerScroll.Interval := 100;
  timerScroll.OnTimer := ScrollTimer;

  {$IFDEF RAEDITOR_EDITOR}

  {$IFDEF RAEDITOR_DEFLAYOT}
  FKeyboard.SetDefLayot;
  {$ENDIF RAEDITOR_DEFLAYOT}

  {$IFDEF RAEDITOR_COMPLETION}
  FCompletion := TJvCompletion .Create2(Self);
  {$ENDIF RAEDITOR_COMPLETION}

  {$ENDIF RAEDITOR_EDITOR}

  FSelBlockFormat := bfNonInclusive;
  if BlockTypeFormat = 0 then
    BlockTypeFormat := RegisterClipboardFormat('Borland IDE Block Type');

  { we can change font only after all objects are created }
  Font.Name := 'Courier New';
  Font.Size := 10;
end;

destructor TJvCustomEditor.Destroy;
begin
  FLines.Free;
  scbHorz.Free;
  scbVert.Free;
  EditorClient.Free;
  FKeyboard.Free;
  {$IFDEF RAEDITOR_EDITOR}
  {$IFDEF RAEDITOR_UNDO}
  FUndoBuffer.Free;
  {$ENDIF RAEDITOR_UNDO}
  {$IFDEF RAEDITOR_COMPLETION}
  FCompletion.Free;
  {$ENDIF RAEDITOR_COMPLETION}
  {$ENDIF RAEDITOR_EDITOR}
  FGutter.Free;
  inherited Destroy;
end;

procedure TJvCustomEditor.Loaded;
begin
  inherited Loaded;
  UpdateEditorSize;
  {  Rows := FLines.Count;
    Cols := Max_X; }
end;

{************** Управление отрисовкой ***************}

procedure TJvCustomEditor.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of cardinal = (0, WS_BORDER);
  ScrollStyles: array[TScrollStyle] of cardinal = (0, WS_HSCROLL, WS_VSCROLL,
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

{$IFNDEF COMPILER4_UP}

procedure TJvCustomEditor.WMSize(var Message: TWMSize);
begin
  inherited;
  if not (csLoading in ComponentState) then Resize;
end;
{$ENDIF COMPILER4_UP}

procedure TJvCustomEditor.Resize;
begin
  UpdateEditorSize;
end;

procedure TJvCustomEditor.CreateWnd;
begin
  inherited CreateWnd;
  if FScrollBars in [ssHorizontal, ssBoth] then
    scbHorz.Handle := Handle;
  if FScrollBars in [ssVertical, ssBoth] then
    scbVert.Handle := Handle;
  FAllRepaint := true;
end;

procedure TJvCustomEditor.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomEditor.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then UpdateEditorSize;
end;

procedure TJvCustomEditor.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  {$IFDEF RAEDITOR_NOOPTIMIZE}
  inherited;
  Message.Result := 1;
  {$ELSE}
  Message.Result := 0;
  {$ENDIF}
end;

procedure TJvCustomEditor.PaintSelection;
var
  iR: integer;
begin
  for iR := FUpdateSelBegY to FUpdateSelEndY do
    PaintLine(iR, -1, -1);
end;

procedure TJvCustomEditor.SetUnSelected;
begin
  if FSelected then
  begin
    FSelected := false;
    {$IFDEF RAEDITOR_UNDO}
    TUnselectUndo.Create(Self, FCaretX, FCaretY, FSelected, FSelBlockFormat,
      FSelBegX, FSelBegY, FSelEndX, FSelEndY);
    {$ENDIF RAEDITOR_UNDO}
    PaintSelection;
  end;
end;

procedure TJvCustomEditor.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if (P.X < GutterWidth) and (Cursor = crIBeam) then
  begin
    Message.Result := 1;
    Windows.SetCursor(Screen.Cursors[crArrow])
  end
  else
    inherited;
end;
{############## Управление отрисовкой ###############}

{************** Отрисовка ***************}

{
function IsRectEmpty(R: TRect): boolean;
begin
  Result := (R.Top = R.Bottom) and (R.Left = R.Right);
end;
}

function TJvCustomEditor.CalcCellRect(const X, Y: integer): TRect;
begin
  Result := Bounds(
    EditorClient.Left + X * FCellRect.Width + 1,
    EditorClient.Top + Y * FCellRect.Height,
    FCellRect.Width,
    FCellRect.Height)
end;

procedure TJvCustomEditor.Paint;
var
  iR: integer;
  ECR: TRect;
  BX, EX, BY, EY: integer;
begin
  if FUpdateLock > 0 then exit;
  {$IFDEF RAEDITOR_NOOPTIMIZE}
  FAllRepaint := true;
  {$ENDIF}
  {оптимизировано - отрисовывается только необходимая часть}
  PaintCaret(false);

  ECR := EditorClient.Canvas.ClipRect;
  OffsetRect(ECR, -FGutterWidth, 0);
  if FAllRepaint then ECR := EditorClient.BoundsRect;
  BX := ECR.Left div FCellRect.Width - 1;
  EX := ECR.Right div FCellRect.Width + 1;
  BY := ECR.Top div FCellRect.Height;
  EY := ECR.Bottom div FCellRect.Height + 1;
  for iR := BY to EY do
    PaintLine(FTopRow + iR, FLeftCol + BX, FLeftCol + EX);

  PaintCaret(true);
  FGutter.Paint;
  FAllRepaint := false;
end;

procedure TJvCustomEditor.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TJvCustomEditor.EndUpdate;
begin
  if FUpdateLock = 0 then Exit; { Error ? }
  dec(FUpdateLock);
  if FUpdateLock = 0 then
  begin
    FAllRepaint := True;
    UpdateEditorSize;
    StatusChanged;
    Invalidate;
  end;
end;

procedure TJvCustomEditor.UpdateEditorSize;
const
  BiggestSymbol = 'W';
var
  i: integer;
  //Wi, Ai: integer;
begin
  if (csLoading in ComponentState) then exit;
  EditorClient.Canvas.Font := Font;
  FCellRect.Height := EditorClient.Canvas.TextHeight(BiggestSymbol) + 1;

  // workaround the bug in Windows-9x
  // fixed by Dmitry Rubinstain
  FCellRect.Width := EditorClient.Canvas.TextWidth(BiggestSymbol + BiggestSymbol) div 2;

  //Ai := EditorClient.Canvas.TextWidth('W');
  //EditorClient.Canvas.Font.Style := [fsBold];
  //Wi := EditorClient.Canvas.TextWidth('w');
  //FCellRect.Width := (Wi+Ai) div 2;

  for i := 0 to 1024 do
    MyDi[i]:= FCellRect.Width;

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
  scbVert.Max := Max(1, FRows - 1 + FVisibleRowCount -1);
  FGutter.Invalidate;
end;

procedure TJvCustomEditor.PaintLine(const Line: integer; ColBeg, ColEnd:
  integer);
var
  Ch: string;
  R: TRect;
  i, iC, jC, SL, MX: integer;
  S: string;
  LA: TLineAttr;
begin
  if (Line < FTopRow) or (Line > FTopRow + FVisibleRowCount) then exit;
  // Debug('PaintLine '+IntToStr(Line));
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
      Canvas.Brush.Color := LineAttrs[FLeftCol+1].BC;
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
          while (jC <= MX + 1) and
            CompareMem(@LA, @LineAttrs[jC], sizeof(LineAttrs[1])) do
          begin
            if jC <= SL then
              Ch := Ch + S[jC]
            else
              Ch := Ch + ' ';
            inc(jC);
          end;
          Brush.Color := LA.BC;
          Font.Color := LA.FC;
          Font.Style := LA.Style;

          R := CalcCellRect(i - FLeftCol, Line - FTopRow);
          {bottom line}
          FillRect(Bounds(R.Left, R.Bottom - 1, FCellRect.Width * Length(Ch), 1));

          // add by patofan
          if (i = ColBeg) and (i < SL) {$IFDEF COMPILER3_UP} and
              (StrByteType( pchar( s ), i ) = mbTrailByte) {$ENDIF} then
          begin
            R.Right := R.Left + FCellRect.Width * Length(Ch) ;
            Ch :=  S[ i ] + Ch;
            TextRect( R , R.left - FCellRect.Width , R.top , Ch );
          end
          else
          begin
            // ending add by patofan
            //Self.Canvas.TextOut(R.Left, R.Top, Ch);
            ExtTextOut(Canvas.Handle,R.Left,R.Top,0,nil,PChar(Ch),Length(Ch),@MyDi);
            // add by patofan
          end;
          // ending add by patofan
          
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
  if FSelected and (FSelBlockFormat in [bfInclusive, bfLine, bfNonInclusive]) and
     (Line >= FSelBegY) and (Line < FSelEndY) then
    EditorClient.Canvas.Brush.Color := FclSelectBC
  else
    EditorClient.Canvas.Brush.Color := Color;
  EditorClient.Canvas.FillRect(R);
  DrawRightMargin;
end;

procedure TJvCustomEditor.GetLineAttr(var Str: string; Line, ColBeg, ColEnd: integer);

  procedure ChangeSelectedAttr;

    procedure DoChange(const iBeg, iEnd: integer);
    var
      i: integer;
    begin
      for i := iBeg to iEnd do
      begin
        LineAttrs[i + 1].FC := FclSelectFC;
        LineAttrs[i + 1].BC := FclSelectBC;
      end;
    end;

  begin
    if FSelBlockFormat = bfColumn then
    begin
      if (Line >= FSelBegY) and (Line <= FSelEndY) then
        DoChange(FSelBegX, FSelEndX - 1 + Integer(1{always Inclusive}))
    end
    else
    begin
      if (Line = FSelBegY) and (Line = FSelEndY) then
        DoChange(FSelBegX, FSelEndX - 1 + Integer(FSelBlockFormat = bfInclusive))
      else
      begin
        if Line = FSelBegY then DoChange(FSelBegX, FSelBegX + FVisibleColCount);
        if (Line > FSelBegY) and (Line < FSelEndY) then DoChange(ColBeg, ColEnd);
        if Line = FSelEndY then
          DoChange(ColBeg, FSelEndX - 1 + Integer(FSelBlockFormat = bfInclusive));
      end;
    end;
  end;
var
  i: integer;
  S: string;
begin
  LineAttrs[ColBeg].FC := Font.Color;
  LineAttrs[ColBeg].Style := Font.Style;
  LineAttrs[ColBeg].BC := Color;
  for i := ColBeg to ColEnd + 1 do
    Move(LineAttrs[ColBeg], LineAttrs[i], sizeof(LineAttrs[1]));
  S := FLines[Line];
  GetAttr(Line, ColBeg, ColEnd);
  if Assigned(FOnGetLineAttr) then FOnGetLineAttr(Self, S, Line, LineAttrs);
  if FSelected then ChangeSelectedAttr; {изменяем атрибуты выделенного блока}
  ChangeAttr(Line, ColBeg, ColEnd);
end;

procedure TJvCustomEditor.GetAttr(Line, ColBeg, ColEnd: integer);
begin
end;

procedure TJvCustomEditor.ChangeAttr(Line, ColBeg, ColEnd: integer);
begin
end;

procedure TJvCustomEditor.DrawRightMargin;
var
  F: integer;
begin
  if FRightMarginVisible and (FRightMargin > FLeftCol)
    and (FRightMargin < FLastVisibleCol + 3) then
    with EditorClient.Canvas do
    begin
      {рисуем RightMargin Line}
      Pen.Color := FRightMarginColor;
      F := CalcCellRect(FRightMargin - FLeftCol, 0).Left;
      MoveTo(F, EditorClient.Top);
      LineTo(F, EditorClient.Top + EditorClient.Height);
    end;
end;

{************************** Scroll **************************}

procedure TJvCustomEditor.WMHScroll(var Message: TWMHScroll);
begin
  scbHorz.DoScroll(Message);
end;

procedure TJvCustomEditor.WMVScroll(var Message: TWMVScroll);
begin
  scbVert.DoScroll(Message);
end;

procedure TJvCustomEditor.WMMouseWheel(var Message: TWMMouseWheel);
begin
  scbVert.Position := scbVert.Position - Message.WheelDelta div 40;
  Scroll(true, scbVert.Position);
end;

procedure TJvCustomEditor.ScrollBarScroll(Sender: TObject; ScrollCode:
  TScrollCode; var ScrollPos: Integer);
begin
  case ScrollCode of
    scLineUp..scPageDown, {scPosition,} scTrack {, scEndScroll}:
      begin
        if Sender = scbVert then
          Scroll(true, ScrollPos)
        else if Sender = scbHorz then
          Scroll(false, ScrollPos);
      end;
  end;
  //  Tracer.Writeln(IntToStr((Sender as TJvControlScrollBar95).Position));
end;

procedure TJvCustomEditor.Scroll(const Vert: boolean; const ScrollPos: integer);
var
  R, RClip, RUpdate: TRect;
  OldFTopRow: integer;
  //  OldFLeftCol: integer;
begin
  //  BeginTick;
  if FUpdateLock = 0 then
  begin
    PaintCaret(false);
    if Vert then
    begin {Vertical Scroll}
      {оптимизировано}
      OldFTopRow := FTopRow;
      FTopRow := ScrollPos;
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
        Invalidate;
      Update;
    end
    else {Horizontal Scroll}
    begin
      {неоптимизировано}
      FLeftCol := ScrollPos;
      (*  OldFLeftCol := FLeftCol;
        FLeftCol := ScrollPos;
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
          InvalidateRect(Handle, @RUpdate, False);
        end
        else
          Invalidate;
        Update;  *)
      Invalidate;
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
    PaintCaret(true);
  end;
  if Assigned(FOnScroll) then FOnScroll(Self);
  //  EndTick;
end;
{####################### Scroll #########################}
{####################### Painting - Отрисовка #######################}

{************** Caret ***************}

procedure TJvCustomEditor.PaintCaret(const bShow: boolean);
var
  R: TRect;
begin
  // Debug('PaintCaret: ' + IntToStr(integer(bShow)));
  if FHideCaret then
    Exit;
  if not bShow then
    Windows.HideCaret(Handle)
  else if Focused then
  begin
    R := CalcCellRect(FCaretX - FLeftCol, FCaretY - FTopRow);
    SetCaretPos(R.Left - 1, R.Top + 1);
    ShowCaret(Handle)
  end
end;

procedure TJvCustomEditor.SetCaretInternal(X, Y: integer);
var
  R: TRect;
begin
  if (X = FCaretX) and (Y = FCaretY) then exit;
  //прокручивать изображение
  if not FCursorBeyondEOF then
    Y := Min(Y, FLines.Count - 1);
  Y := Max(Y, 0);
  X := Min(X, Max_X);
  X := Max(X, 0);
  if Y < FTopRow then
    SetLeftTop(FLeftCol, Y)
  else if Y > Max(FLastVisibleRow, 0) then
    SetLeftTop(FLeftCol, Y - FVisibleRowCount + 1);
  if X < 0 then X := 0;
  if X < FLeftCol then
    SetLeftTop(X, FTopRow)
  else if X > FLastVisibleCol then
    SetLeftTop(X - FVisibleColCount + 1, FTopRow);

  if Focused then  {mac: do not move Caret when not focused!}
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

procedure TJvCustomEditor.SetCaret(X, Y: integer);
begin
  if (X = FCaretX) and (Y = FCaretY) then exit;
  {$IFDEF RAEDITOR_UNDO}
  TJvCaretUndo .Create(Self, FCaretX, FCaretY);
  {$ENDIF RAEDITOR_UNDO}
  SetCaretInternal(X, Y);
  if FUpdateLock = 0 then StatusChanged;
end;

procedure TJvCustomEditor.SetCaretPosition(const index, Pos: integer);
begin
  if index = 0 then
    SetCaret(Pos, FCaretY)
  else
    SetCaret(FCaretX, Pos)
end;

procedure TJvCustomEditor.WMSetFocus(var Message: TWMSetFocus);
begin
  CreateCaret(Handle, 0, 2, CellRect.Height - 2);
  PaintCaret(true);
end;

procedure TJvCustomEditor.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  {$IFDEF RAEDITOR_COMPLETION}
  if FCompletion.FVisible then FCompletion.CloseUp(false);
  {$ENDIF RAEDITOR_COMPLETION}
  DestroyCaret;
end;

{############### Caret ###############}

{************** Keyboard ***************}

procedure TJvCustomEditor.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTTAB or DLGC_WANTCHARS or DLGC_WANTMESSAGE;
end;

procedure TJvCustomEditor.KeyDown(var Key: Word; Shift: TShiftState);
{$IFDEF RAEDITOR_EDITOR}
var
  Com: word;
{$ENDIF RAEDITOR_EDITOR}
begin
  {$IFDEF RAEDITOR_COMPLETION}
  if FCompletion.FVisible then
    if FCompletion.DoKeyDown(Key, Shift) then
      exit
    else
  else
    FCompletion.FTimer.Enabled := false;
  {$ENDIF RAEDITOR_COMPLETION}
  {$IFDEF RAEDITOR_EDITOR}
  if WaitSecondKey then
  begin
    Com := FKeyboard.Command2(Key1, Shift1, Key, Shift);
    WaitSecondKey := false;
    IgnoreKeyPress := true;
  end
  else
  begin
    inherited KeyDown(Key, Shift);
    Key1 := Key;
    Shift1 := Shift;
    Com := FKeyboard.Command(Key, Shift);
    if Com = twoKeyCommand then
    begin
      IgnoreKeyPress := true;
      WaitSecondKey := true;
    end
    else
      IgnoreKeyPress := Com > 0;
  end;
  if (Com > 0) and (Com <> twoKeyCommand) then
  begin
    Command(Com);
    Key := 0;
  end;
  {$IFDEF RAEDITOR_COMPLETION}
  if (Com = ecBackSpace) then
    FCompletion.DoKeyPress(#8);
  {$ENDIF RAEDITOR_COMPLETION}
  {$ENDIF RAEDITOR_EDITOR}
end;

{$IFDEF RAEDITOR_EDITOR}

procedure TJvCustomEditor.ReLine;
begin
  FLines.ReLine;
end; { ReLine }

procedure TJvCustomEditor.KeyPress(var Key: Char);
begin
  if IgnoreKeyPress then
  begin
    IgnoreKeyPress := false;
    exit
  end;
  if FReadOnly then exit;
  PaintCaret(false);
  inherited KeyPress(Key);

  Command(ord(Key));

  PaintCaret(true);
end;

procedure TJvCustomEditor.InsertChar(const Key: Char);
var
  S: string;
begin
  ReLine;
  case Key of
    #32..#255:
      begin
        {$IFDEF RAEDITOR_COMPLETION}
        if not HasChar(Key, RAEditorCompletionChars) then
          FCompletion.DoKeyPress(Key);
        {$ENDIF RAEDITOR_COMPLETION}
        begin
          DeleteSelected;
          S := FLines[FCaretY];
          if FInsertMode then
          begin
            {$IFDEF RAEDITOR_UNDO}
            TJvInsertUndo .Create(Self, FCaretX, FCaretY, Key);
            {$ENDIF RAEDITOR_UNDO}
            Insert(Key, S, FCaretX + 1);
          end
          else
          begin
            {$IFDEF RAEDITOR_UNDO}
            if FCaretX + 1 <= Length(S) then
              TJvOverwriteUndo .Create(Self, FCaretX, FCaretY, S[FCaretX + 1], Key)
            else
              TJvOverwriteUndo .Create(Self, FCaretX, FCaretY, '', Key);
            {$ENDIF RAEDITOR_UNDO}
            if FCaretX + 1 <= Length(S) then
              S[FCaretX + 1] := Key
            else
              S := S + Key
          end;
          FLines.Internal[FCaretY] := S;
          SetCaretInternal(FCaretX + 1, FCaretY);
          TextModified(SelStart, maInsert, Key);
          PaintLine(FCaretY, -1, -1);
          Changed;
        end;
        {$IFDEF RAEDITOR_COMPLETION}
        if HasChar(Key, RAEditorCompletionChars) then
          FCompletion.DoKeyPress(Key);
        {$ENDIF RAEDITOR_COMPLETION}
      end;
  end;
end;

{$ENDIF RAEDITOR_EDITOR}

{$IFDEF RAEDITOR_EDITOR}
type
  EJvComplete  = class(EAbort);

procedure TJvCustomEditor.Command(ACommand: TEditCommand);
var
  X, Y: integer;
  {$IFDEF RAEDITOR_UNDO}
  CaretUndo: boolean;
  {$ENDIF RAEDITOR_UNDO}
  // add by patofan
  K : integer;
  // ending add by patofan

type
  TPr = procedure of object;

  procedure DoAndCorrectXY(Pr: TPr);
  begin
    Pr;
    X := FCaretX;
    Y := FCaretY;
    {$IFDEF RAEDITOR_COMPLETION}
    CaretUndo := false;
    {$ENDIF RAEDITOR_COMPLETION}
  end;

  function Com(const Args: array of TEditCommand): boolean;
  var
    i: integer;
  begin
    for i := 0 to High(Args) do
      if Args[i] = ACommand then
      begin
        Result := true;
        exit;
      end;
    Result := false;
  end;

  procedure SetSel1(X, Y: integer);
  begin
    SetSel(X, Y);
    {$IFDEF RAEDITOR_UNDO}
    CaretUndo := False;
    {$ENDIF RAEDITOR_UNDO}
  end;

  procedure SetSelText1(S: string);
  begin
    SelText := S;
   {$IFDEF RAEDITOR_UNDO}
    CaretUndo := False;
   {$ENDIF RAEDITOR_UNDO}
  end;

var
  F: integer;
  S, S2: string;
  B: boolean;
  iBeg, iEnd: integer;
  // add by patofan
  deltastep : integer;
  // ending by patofan
begin
  X := FCaretX;
  Y := FCaretY;
  {$IFDEF RAEDITOR_UNDO}
  CaretUndo := true;
  {$ENDIF RAEDITOR_UNDO}
  PaintCaret(false);
  // inc(FUpdateLock);
 { macro recording }
  if FRecording and not Com([ecRecordMacro, ecBeginCompound]) and
     (FCompound = 0) then
    FMacro := FMacro + Char(Lo(ACommand)) + Char(Hi(ACommand));
  try
    // add by patofan
    deltastep := -1;
    // ending add by patofan
    case ACommand of

      { caret movements }

      ecLeft, ecRight, ecSelLeft, ecSelRight:
        begin
          if Com([ecSelLeft, ecSelRight]) and not FSelected then
            SetSel1(X, Y);
          if Com([ecLeft, ecSelLeft]) then
            dec(X)
          else
          begin
            inc(X);
            // add by patofan
            deltastep := 1;
            // ending add by patofan
          end;
          if Com([ecSelLeft, ecSelRight]) then
          begin
            // add by patofan
            {$IFDEF COMPILER3_UP}
            CheckDoubleByteChar( x , y  , mbTrailByte , deltastep );
            {$ENDIF}
            // ending add by patofan
            SetSel1(X, Y);
          end
          else
            SetUnSelected;
        end;
      ecUp, ecDown, ecSelUp, ecSelDown:
        if Com([ecUp, ecSelUp]) or (Y < FRows - 1) or FCursorBeyondEOF then
        begin
          if Com([ecSelUp, ecSelDown]) and not FSelected then SetSel1(X, Y);
          if Com([ecUp, ecSelUp]) then
            dec(Y)
          else
          begin
            inc(Y);
            // add by patofan
            deltastep := 1;
            // ending add by patofan
          end;
          if Com([ecSelUp, ecSelDown]) then
          begin
            // add by patofan
            {$IFDEF COMPILER3_UP}
            CheckDoubleByteChar( x , y  , mbTrailByte , deltastep );
            {$ENDIF COMPILER3_UP}
            // ending add by patofan
            SetSel1(X, Y);
          end
          else
            SetUnSelected;
        end;
      ecPrevWord, ecSelPrevWord:
        begin
          if (ACommand = ecSelPrevWord) and not FSelected then
            SetSel1(FCaretX, FCaretY);
          S := FLines[Y];
          B := false;
          if FCaretX > Length(s) then
          begin
            X:=Length(s);
            SetSel1(X, Y);
          end
          else
          begin
            for F := X - 1 downto 0 do
              if B then
                if (S[F + 1] in Separators) then
                begin
                  X := F + 1;
                  break;
                end
                else
              else if not (S[F + 1] in Separators) then
                B := true;
            if X = FCaretX then
              X := 0;
            if ACommand = ecSelPrevWord then
              SetSel1(X, Y)
            else
              SetUnselected;
          end;    
        end;
      ecNextWord, ecSelNextWord:
        begin
          if (ACommand = ecSelNextWord) and not FSelected then
            SetSel1(FCaretX, FCaretY);
          if Y >= FLines.Count then
          begin
            Y := FLines.Count - 1;
            X := Length(FLines[Y]);
          end;
          S := FLines[Y];
          B := false;
          if FCaretX >= Length(S) then
          begin
            if Y < FLines.Count - 1 then
            begin
              Y := FCaretY + 1;
              X := 0;
              SetSel1(X, Y);
            end;
          end
          else
          begin
            for F := X to Length(S) - 1 do
              if B then
                if not (S[F + 1] in Separators) then
                begin
                  X := F;
                  break;
                end
                else
              else if (S[F + 1] in Separators) then
                B := true;
            if X = FCaretX then
              X := Length(S);
            if ACommand = ecSelNextWord then
              SetSel1(X, Y)
            else
              SetUnselected;
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
            Scroll(true, scbVert.Position);
          end;
          if Y < FTopRow then
            Y := FTopRow
          else if Y > FLastVisibleRow then
            Y := FLastVisibleRow;
          // add by patofan
         {$IFDEF COMPILER3_UP}
          CheckDoubleByteChar( x , y  , mbTrailByte , -1 );
         {$ENDIF COMPILER3_UP}
          // ending add by patofan
        end;
      ecBeginLine, ecSelBeginLine, ecBeginDoc, ecSelBeginDoc,
        ecEndLine, ecSelEndLine, ecEndDoc, ecSelEndDoc:
        begin
          if Com([ecSelBeginLine, ecSelBeginDoc, ecSelEndLine, ecSelEndDoc])
            and not FSelected then
            SetSel1(FCaretX, Y);
          if Com([ecBeginLine, ecSelBeginLine]) then
            X := 0
          else if Com([ecBeginDoc, ecSelBeginDoc]) then
          begin
            X := 0;
            Y := 0;
            SetLeftTop(0, 0);
          end
          else if Com([ecEndLine, ecSelEndLine]) then
            if Y < FLines.Count then
              X := Length(FLines[Y])
             else
              X := 0
          else if Com([ecEndDoc, ecSelEndDoc]) then
          begin
            Y := FLines.Count - 1;
            X := Length(FLines[Y]);
            SetLeftTop(X - FVisibleColCount, Y - FVisibleRowCount div 2);
          end;
          if Com([ecSelBeginLine, ecSelBeginDoc, ecSelEndLine, ecSelEndDoc])
            then
            SetSel1(X, Y)
          else
            SetUnSelected;
        end;
      ecPrevPage:
        begin
          scbVert.Position := scbVert.Position - scbVert.LargeChange;
          Scroll(true, scbVert.Position);
          Y := Y - FVisibleRowCount;
          SetUnSelected;
        end;
      ecNextPage:
        begin
          scbVert.Position := scbVert.Position + scbVert.LargeChange;
          Scroll(true, scbVert.Position);
          Y := Y + FVisibleRowCount;
          SetUnSelected;
        end;
      ecSelPrevPage:
        begin
          BeginUpdate;
          SetSel1(X, Y);
          scbVert.Position := scbVert.Position - scbVert.LargeChange;
          Scroll(true, scbVert.Position);
          Y := Y - FVisibleRowCount;
          // add by patofan
         {$IFDEF COMPILER3_UP}
          CheckDoubleByteChar( x , y  , mbTrailByte , deltastep );
         {$ENDIF COMPILER3_UP}
          // ending add by patofan
          SetSel1(X, Y);
          EndUpdate;
        end;
      ecSelNextPage:
        begin
          BeginUpdate;
          SetSel1(X, Y);
          scbVert.Position := scbVert.Position + scbVert.LargeChange;
          Scroll(true, scbVert.Position);
          Y := Y + FVisibleRowCount;
          if Y <= FLines.Count - 1 then
          begin
            // add by patofan
            {$IFDEF COMPILER3_UP}
            CheckDoubleByteChar( x , y  , mbTrailByte , deltastep );
            {$ENDIF COMPILER3_UP}
            // ending add by patofan
            SetSel1(X, Y);
          end
          else
          begin
            // add by patofan
            {$IFDEF COMPILER3_UP}
            CheckDoubleByteChar( x , FLines.Count - 1  , mbTrailByte , deltastep );
            {$ENDIF COMPILER3_UP}
            // ending add by patofan
            SetSel1(X, FLines.Count - 1);
          end;
          EndUpdate;
        end;
      ecSelWord:
        if not FSelected and (GetWordOnPosEx(FLines[Y] + ' ', X + 1, iBeg,
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

      {$IFDEF RAEDITOR_EDITOR}
      ecCharFirst..ecCharLast:
        if not FReadOnly then
        begin
          InsertChar(Char(ACommand - ecCharFirst));
          Exit;
        end;
      ecInsertPara:
        if not FReadOnly then
        begin
          DeleteSelected;
          {$IFDEF RAEDITOR_UNDO}
          TJvInsertUndo .Create(Self, FCaretX, FCaretY, #13#10);
          CaretUndo := false;
          {$ENDIF RAEDITOR_UNDO}
          inc(FUpdateLock);
          try
            if FLines.Count = 0 then FLines.Add('');
            F := SelStart - 1;
            FLines.Insert(Y + 1, Copy(FLines[Y], X + 1, Length(FLines[Y])));
            FLines.Internal[Y] := Copy(FLines[Y], 1, X);
            inc(Y);
            { smart tab }
            if FAutoIndent and
              (((Length(FLines[FCaretY]) > 0) and
               (FLines[FCaretY][1] = ' ')) or
              ((Trim(FLines[FCaretY]) = '') and (X > 0))) then
            begin
              X := GetTabStop(0, Y, tsAutoIndent, True);
              {$IFDEF RAEDITOR_UNDO}
              TJvInsertUndo .Create(Self, 0, Y, Spaces(X));
              {$ENDIF RAEDITOR_UNDO}
              FLines.Internal[Y] := Spaces(X) + FLines[Y];
            end
            else
              X := 0;
            UpdateEditorSize;
            TextModified(F, maInsert, #13#10);
          finally
            dec(FUpdateLock);
          end;
          Invalidate;
          Changed;
        end;
      ecBackspace:
        if not FReadOnly then
          if X > 0 then
          begin
            { into line - в середине строки }
            if FSelected then
              DoAndCorrectXY(DeleteSelected)
            else
            begin
              ReLine;
              if FBackSpaceUnindents then
                X := GetBackStop(FCaretX, FCaretY)
              else
                X := FCaretX - 1;

              // add by patofan
             {$IFDEF COMPILER3_UP}
              k := x - 1;
              if CheckDoubleByteChar( k , y  , mbLeadByte , 0 ) then
              begin
                X := k;
              end;
             {$ENDIF COMPILER3_UP}
              // ending add by patofan

              S := Copy(FLines[FCaretY], X + 1, FCaretX - X);
              {$IFDEF RAEDITOR_UNDO}
              TJvBackspaceUndo .Create(Self, FCaretX, FCaretY, S);
              CaretUndo := false;
              {$ENDIF RAEDITOR_UNDO}
              F := SelStart - 1;
              FLines.Internal[Y] := Copy(FLines[Y], 1, X) +
                Copy(FLines[Y], FCaretX + 1, Length(FLines[Y]));
              TextModified(F, maDelete, S);
              PaintLine(Y, -1, -1);
            end;
            Changed;
          end
          else if Y > 0 then
          begin
            { on begin of line - в начале строки}
            DeleteSelected;
            ReLine;
            F := SelStart - 2;
            if F <= 0 then
              S:='#$A#$D'
            else
              S := FLines.Text[SelStart - 2] +
                FLines.Text[SelStart - 1];
            X := Length(FLines[Y - 1]);
            {$IFDEF RAEDITOR_UNDO}
            TJvBackspaceUndo .Create(Self, FCaretX, FCaretY, #13);
            CaretUndo := false;
            {$ENDIF RAEDITOR_UNDO}
            FLines.Internal[Y - 1] := FLines[Y - 1] + FLines[Y];
            FLines.Delete(Y);
            dec(Y);
            UpdateEditorSize;
            TextModified(F, maDelete, S);
            Invalidate;
            Changed;
          end;
      ecDelete:
        if not FReadOnly then
        begin
          inc(FUpdateLock);
          try
            if FLines.Count = 0 then FLines.Add('');
          finally
            dec(FUpdateLock);
          end;
          if FSelected then
            DoAndCorrectXY(DeleteSelected)
          else if X < Length(FLines[Y]) then
          begin
            { into line - в середине строки}
            {$IFDEF RAEDITOR_UNDO}
            TJvDeleteUndo .Create(Self, FCaretX, FCaretY, FLines[Y] [X + 1]);
            CaretUndo := false;
            {$ENDIF RAEDITOR_UNDO}

            // add by patofan
            {$IFDEF COMPILER3_UP}
            k := x + 1;
            if CheckDoubleByteChar( k , y  , mbTrailByte , 0 ) then
            begin
              S := FLines[Y] [X + 1] + FLines[Y] [X + 2];
              FLines.Internal[Y] := Copy(FLines[Y], 1, X) +
                Copy(FLines[Y], X + 3, Length(FLines[Y]));
            end else
            {$ENDIF COMPILER3_UP}
            begin
            // ending add by patofan
              S := FLines[Y] [X + 1];
              FLines.Internal[Y] := Copy(FLines[Y], 1, X) +
                Copy(FLines[Y], X + 2, Length(FLines[Y]));
            // add by patofan
            end;
            // ending add by patofan

            TextModified(SelStart, maDelete, S);
            PaintLine(FCaretY, -1, -1);
            Changed;
          end
          else if (Y >= 0) and (Y <= FLines.Count - 2) then
          begin
            { on end of line - в конце строки}
            {$IFDEF RAEDITOR_UNDO}
            TJvDeleteUndo .Create(Self, FCaretX, FCaretY, #13#10);
            CaretUndo := false;
            {$ENDIF RAEDITOR_UNDO}
            S := FLines.Text[SelStart + 1] + FLines.Text[SelStart + 2];
            FLines.Internal[Y] := FLines[Y] + FLines[Y + 1];
            FLines.Delete(Y + 1);
            UpdateEditorSize;
            TextModified(SelStart, maDelete, S);
            Invalidate;
            Changed;
          end;
          // add by patofan
          deltastep := 0;
          // ending add by patofan
        end;
      ecTab, ecBackTab:
        if not FReadOnly then
        begin
          if FSelected then
          begin
            if ACommand = ecTab then
              PostCommand(ecIndent)
            else
              PostCommand(ecUnindent);
          end
          else
          begin
            ReLine;
            X := GetTabStop(FCaretX, FCaretY, tsTabStop, ACommand = ecTab);
            if (ACommand = ecTab) and FInsertMode then
            begin
              S := FLines[FCaretY];
              S2 := Spaces(X - FCaretX);
              {$IFDEF RAEDITOR_UNDO}
              TJvInsertTabUndo .Create(Self, FCaretX, FCaretY, S2);
              CaretUndo := false;
              {$ENDIF RAEDITOR_UNDO}
              Insert(S2, S, FCaretX + 1);
              FLines.Internal[FCaretY] := S;
              TextModified(SelStart, maInsert, S2);
              PaintLine(FCaretY, -1, -1);
              Changed;
            end
            else
              { move cursor - oh yes!, it's allready moved: X := GetTabStop(..); }
          end;
        end;
      ecIndent:
        if not FReadOnly and FSelected and (FSelBegY <> FSelEndY) and
          (FSelBegX = 0) and (FSelEndX = 0) then
        begin
          F := FindNotBlankCharPos(FLines[FCaretY]);
          S2 := Spaces(GetDefTabStop(F, True) - FCaretX);
          S := SelText;
          S := ReplaceString(S, #13#10, #13#10 + S2);
          Delete(S, Length(S) - Length(S2) + 1, Length(S2));
          SetSelText1(S2 + S)
        end;
      ecUnIndent:
        if not FReadOnly and FSelected and (FSelBegY <> FSelEndY) and
          (FSelBegX = 0) and (FSelEndX = 0) then
        begin
          F := FindNotBlankCharPos(FLines[FCaretY]);
          S2 := Spaces(GetDefTabStop(F, True) - FCaretX);
          S := SelText;
          S := ReplaceString(S, #13#10 + S2, #13#10);
          for iBeg := 1 to Length(S2) do
            if S[1] = ' ' then
              Delete(S, 1, 1)
            else
              Break;
          SetSelText1(S);
        end;

      ecChangeInsertMode:
        begin
          FInsertMode := not FInsertMode;
          StatusChanged;
        end;
      ecInclusiveBlock .. ecNonInclusiveBlock:
        begin
          FSelBlockFormat := TSelBlockFormat(ACommand - ecInclusiveBlock);
          PaintSelection;
          StatusChanged;
        end;

      ecClipBoardCut:
        if not FReadOnly then
          DoAndCorrectXY(ClipBoardCut);
      {$ENDIF RAEDITOR_EDITOR}
      ecClipBoardCopy:
        ClipBoardCopy;
      {$IFDEF RAEDITOR_EDITOR}
      ecClipBoardPaste:
        if not FReadOnly then
          DoAndCorrectXY(ClipBoardPaste);
      ecDeleteSelected:
        if not FReadOnly and FSelected then
          DoAndCorrectXY(DeleteSelected);

      ecDeleteWord:
        if not FReadOnly then
        begin
          Command(ecBeginCompound);
          Command(ecBeginUpdate);
          Command(ecSelWord);
          // Command(ecSelNextWord); //???? it should work as in Delphi editor...
          Command(ecDeleteSelected);
          Command(ecEndUpdate);
          Command(ecEndCompound);
          Exit;
        end;
      ecDeleteLine:
        if not FReadOnly then
        begin
          Command(ecBeginCompound);
          Command(ecBeginUpdate);
          Command(ecBeginLine);
          Command(ecSelEndLine);
          Command(ecDelete);
          Command(ecDelete);
          Command(ecEndUpdate);
          Command(ecEndCompound);
          Exit;
        end;
      ecSelAll:
        begin
          Command(ecBeginCompound);
          Command(ecBeginUpdate);
          Command(ecBeginDoc);
          Command(ecSelEndDoc);
          Command(ecEndUpdate);
          Command(ecEndCompound);
          Exit;
        end;
      ecToUpperCase:
        if not FReadOnly then
          SelText := ANSIUpperCase(SelText);
      ecToLowerCase:
        if not FReadOnly then
          SelText := ANSILowerCase(SelText);
      ecChangeCase:
        if not FReadOnly then
          SelText := ANSIChangeCase(SelText);
      {$ENDIF RAEDITOR_EDITOR}
      {$IFDEF RAEDITOR_UNDO}
      ecUndo:
        if not FReadOnly then
        begin
          FUndoBuffer.Undo;
          PaintCaret(true);
          Exit;
        end;
      ecRedo:
        if not FReadOnly then
        begin
          FUndoBuffer.Redo;
          PaintCaret(true);
          Exit;
        end;
      ecBeginCompound:
        BeginCompound;
      ecEndCompound:
        EndCompound;
      {$ENDIF RAEDITOR_UNDO}

      ecSetBookmark0..ecSetBookmark9:
        ChangeBookMark(ACommand - ecSetBookmark0, true);
      ecGotoBookmark0..ecGotoBookmark9:
        begin
          ChangeBookMark(ACommand - ecGotoBookmark0, false);
          X := FCaretX;
          Y := FCaretY;
        end;
      {$IFDEF RAEDITOR_COMPLETION}
      ecCompletionIdentifers:
        if not FReadOnly then
        begin
          FCompletion.DoCompletion(cmIdentifers);
          PaintCaret(true);
          Exit;
        end;
      ecCompletionTemplates:
        if not FReadOnly then
        begin
          FCompletion.DoCompletion(cmTemplates);
          PaintCaret(true);
          Exit;
        end;
      {$ENDIF RAEDITOR_COMPLETION}
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

    // add by patofan
    {$IFDEF COMPILER3_UP}
    CheckDoubleByteChar( x , y  , mbTrailByte , deltastep );
    {$ENDIF COMPILER3_UP}
    // add by patofan

    {$IFDEF RAEDITOR_UNDO}
    if CaretUndo then
      SetCaret(X, Y)
    else
      SetCaretInternal(X, Y);
    {$ELSE}
    SetCaret(X, Y);
    {$ENDIF RAEDITOR_UNDO}
  finally
    // dec(FUpdateLock);
    PaintCaret(true);
  end;
end;
{$ENDIF}

procedure TJvCustomEditor.PostCommand(ACommand: TEditCommand);
begin
  PostMessage(Handle, WM_EDITCOMMAND, ACommand, 0);
end; { PostCommand }

procedure TJvCustomEditor.WMEditCommand(var Message: TMessage);
begin
  Command(Message.WParam);
  Message.Result := ord(True);
end;

procedure TJvCustomEditor.WMCopy(var Message: TMessage);
begin
  PostCommand(ecClipboardCopy);
  Message.Result := ord(True);
end;

{$IFDEF RAEDITOR_EDITOR}

procedure TJvCustomEditor.WMCut(var Message: TMessage);
begin
  if not FReadOnly then
    PostCommand(ecClipboardCut);
  Message.Result := ord(True);
end;

procedure TJvCustomEditor.WMPaste(var Message: TMessage);
begin
  if not FReadOnly then
    PostCommand(ecClipBoardPaste);
  Message.Result := ord(True);
end;
{$ENDIF}

{$IFDEF RAEDITOR_EDITOR}

procedure TJvCustomEditor.ChangeBookMark(const BookMark: TBookMarkNum; const
  Valid: boolean);

  procedure SetXY(X, Y: integer);
  var
    X1, Y1: integer;
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
      BookMarks[Bookmark].Valid := false
    else
    begin
      BookMarks[Bookmark].X := FCaretX;
      BookMarks[Bookmark].Y := FCaretY;
      BookMarks[Bookmark].Valid := true;
    end
  else if BookMarks[Bookmark].Valid then
    SetXY(BookMarks[Bookmark].X, BookMarks[Bookmark].Y);
  BookmarkCnanged(BookMark);
end;
{$ENDIF}

procedure TJvCustomEditor.BookmarkCnanged(BookMark: integer);
begin
  FGutter.Invalidate;
end;


{############### Клавиатура ###############}

procedure TJvCustomEditor.SelectionChanged;
begin
  {abstract}
end;

procedure TJvCustomEditor.SetSel(const SelX, SelY: integer);

  procedure UpdateSelected;
  var
    iR: integer;
  begin
    if FSelBlockFormat = bfColumn then
    begin
      if FUpdateSelBegY < FSelBegY then
        for iR := FUpdateSelBegY to FSelBegY do
          PaintLine(iR, -1, -1);
      for iR := FSelBegY to FSelEndY do
        PaintLine(iR, -1, -1);
      if FUpdateSelEndY > FSelEndY then
        for iR := FSelEndY to FUpdateSelEndY do
          PaintLine(iR, -1, -1);
    end
    else
    begin
      if FUpdateSelBegY < FSelBegY then
        for iR := FUpdateSelBegY to FSelBegY do
          PaintLine(iR, -1, -1)
      else
        for iR := FSelBegY to FUpdateSelBegY do
          PaintLine(iR, -1, -1);
      if FUpdateSelEndY < FSelEndY then
        for iR := FUpdateSelEndY to FSelEndY do
          PaintLine(iR, -1, -1)
      else
        for iR := FSelEndY to FUpdateSelEndY do
          PaintLine(iR, -1, -1);
    end;
    SelectionChanged;
    if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
  end;

begin
  {$IFDEF RAEDITOR_UNDO}
  TJvSelectUndo .Create(Self, FCaretX, FCaretY, FSelected, FSelBlockFormat,
    FSelBegX, FSelBegY, FSelEndX, FSelEndY);
  {$ENDIF RAEDITOR_UNDO}
  if not FSelected then
  begin
    FSelStartX := SelX;
    FSelStartY := SelY;
    FSelEndX := SelX;
    FSelEndY := SelY;
    FSelBegX := SelX;
    FSelBegY := SelY;
    FSelected := true;
  end
  else
  begin
    FUpdateSelBegY := FSelBegY;
    FUpdateSelEndY := FSelEndY;

    if SelY <= FSelStartY then
    begin
      FSelBegY := SelY;
      FSelEndY := FSelStartY;
    end;
    if SelY >= FSelStartY then
    begin
      FSelBegY := FSelStartY;
      FSelEndY := SelY;
    end;
    if (SelY < FSelStartY) or ((SelY = FSelStartY) and (SelX <= FSelStartX)) then
      if (FSelBlockFormat = bfColumn) and (SelX > FSelStartX) then
      begin
        FSelBegX := FSelStartX;
        FSelEndX := SelX;
      end
      else
      begin
        FSelBegX := SelX;
        FSelEndX := FSelStartX;
      end;
    if (SelY > FSelStartY) or ((SelY = FSelStartY) and (SelX >= FSelStartX)) then
      if (FSelBlockFormat = bfColumn) and (SelX < FSelStartX) then
      begin
        FSelBegX := SelX;
        FSelEndX := FSelStartX;
      end
      else
      begin
        FSelBegX := FSelStartX;
        FSelEndX := SelX;
      end;

    FSelected := true;
  end;
  if FCompound = 0 then
    UpdateSelected;
  if FUpdateSelBegY > FSelBegY then FUpdateSelBegY := FSelBegY;
  if FUpdateSelEndY < FSelEndY then FUpdateSelEndY := FSelEndY;
end;

procedure TJvCustomEditor.SetSelBlockFormat(const Value: TSelBlockFormat);
begin
  Command(ecInclusiveBlock + Integer(Value));
end;

{************** Мышь ***************}

procedure TJvCustomEditor.Mouse2Cell(const X, Y: integer; var CX, CY: integer);
begin
  CX := Round((X - EditorClient.Left) / FCellRect.Width);
  CY := (Y - EditorClient.Top) div FCellRect.Height;
end;

procedure TJvCustomEditor.Mouse2Caret(const X, Y: integer; var CX, CY: integer);
begin
  Mouse2Cell(X, Y, CX, CY);
  if CX < 0 then CX := 0;
  if CY < 0 then CY := 0;
  CX := CX + FLeftCol;
  CY := CY + FTopRow;
  if CX > FLastVisibleCol then CX := FLastVisibleCol;
  if CY > FLines.Count - 1 then CY := FLines.Count - 1;
end;

procedure TJvCustomEditor.CaretCoord(const X, Y: integer; var CX, CY: integer);
begin
  CX := X - FLeftCol;
  CY := Y - FTopRow;
  if CX < 0 then CX := 0;
  if CY < 0 then CY := 0;
  CX := FCellRect.Width * CX;
  CY := FCellRect.Height * CY;
end;

procedure TJvCustomEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  XX, YY: integer;
begin
  if FDoubleClick then
  begin
    FDoubleClick := false;
    Exit;
  end;
  {$IFDEF RAEDITOR_COMPLETION}
  if FCompletion.FVisible then FCompletion.CloseUp(false);
  {$ENDIF RAEDITOR_COMPLETION}
  Mouse2Caret(X, Y, XX, YY);
  // if (XX = FCaretX) and (YY = FCaretY) then exit;

  // add by patofan
 {$IFDEF COMPILER3_UP}
  CheckDoubleByteChar( xx , yy  , mbTrailByte , -1 );
 {$ENDIF COMPILER3_UP}
  // ending add by patofan
  
  PaintCaret(false);
  if Button = mbLeft then
  begin
    FSelBlockFormat := bfNonInclusive;
    SetUnSelected;
  end;
  SetFocus;
  if Button = mbLeft then
    SetCaret(XX, YY);
  PaintCaret(true);
  FMouseDowned := True;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvCustomEditor.DblClick;
var
  iBeg, iEnd: integer;
begin
  FDoubleClick := true;
  if Assigned(FOnDblClick) then FOnDblClick(Self);
  if FDoubleClickLine then
  begin
    PaintCaret(false);
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
    PaintCaret(true);
  end
  else if (FLines.Count > 0) and (Trim(FLines[FCaretY]) <> '') then
  begin
    iEnd := Length(TrimRight(FLines[FCaretY]));
    if FCaretX < iEnd then
      while FLines[FCaretY][FCaretX + 1] <= ' ' do
        Inc(FCaretX)
    else
    begin
      FCaretX := iEnd - 1;
      while FLines[FCaretY][FCaretX + 1] <= ' ' do
        Dec(FCaretX)
    end;
    if GetWordOnPosEx(FLines[FCaretY] + ' ', FCaretX + 1, iBeg, iEnd) <> '' then
    begin
      PaintCaret(false);
      SetSel(iBeg - 1, FCaretY);
      SetSel(iEnd - 1, FCaretY);
      SetCaret(iEnd - 1, FCaretY);
      PaintCaret(true);
    end;
  end
end;

procedure TJvCustomEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  timerScroll.Enabled := false;
  FMouseDowned := False;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvCustomEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FMouseDowned and (Shift = [ssLeft]) then
  begin
    PaintCaret(false);
    MouseMoveY := Y;
    Mouse2Caret(X, Y, MouseMoveXX, MouseMoveYY);

    // add by patofan
    {$IFDEF COMPILER3_UP}
    CheckDoubleByteChar( MouseMoveXX, MouseMoveYY  , mbTrailByte , -1 );
    {$ENDIF COMPILER3_UP}
    // ending add by patofan

    if MouseMoveYY <= FLastVisibleRow then
    begin
      SetSel(MouseMoveXX, MouseMoveYY);
      SetCaret(MouseMoveXX, MouseMoveYY);
    end;
    timerScroll.Enabled := (Y < 0) or (Y > ClientHeight);
    PaintCaret(true);
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvCustomEditor.ScrollTimer(Sender: TObject);
begin
  if (MouseMoveY < 0) or (MouseMoveY > ClientHeight) then
  begin
    if (MouseMoveY < -20) then
      dec(MouseMoveYY, FVisibleRowCount)
    else if (MouseMoveY < 0) then
      dec(MouseMoveYY)
    else if (MouseMoveY > ClientHeight + 20) then
      inc(MouseMoveYY, FVisibleRowCount)
    else if (MouseMoveY > ClientHeight) then
      inc(MouseMoveYY);
    PaintCaret(false);
    SetSel(MouseMoveXX, MouseMoveYY);
    SetCaret(MouseMoveXX, MouseMoveYY);
    PaintCaret(true);
  end;
end;

{############## Мышь ###############}

{************** ClipBoard ***************}

function TJvCustomEditor.GetClipboardBlockFormat: TSelBlockFormat;
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
      if Data <> 0 then GlobalUnlock(Data);
      Clipboard.Close;
    end;
  end;
end;

procedure TJvCustomEditor.SetClipboardBlockFormat(const Value: TSelBlockFormat);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  Clipboard.Open;
  try
    Data := GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, 1);
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

function TJvCustomEditor.GetSelText: string;
var
  S1: string;
  i: integer;
begin
  Result := '';
  if not FSelected then exit;
  if (FSelBegY < 0) or (FSelBegY > FLines.Count - 1) or (FSelEndY < 0) or
    (FSelEndY > FLines.Count - 1) then
  begin
    Err;
    Exit;
  end;
  if FSelBlockFormat = bfColumn then
  begin
    for i := FSelBegY to FSelEndY do
    begin
      S1 := Copy(FLines[i], FSelBegX + 1, FSelEndX - FSelBegX + 1);
      S1 := S1 + Spaces((FSelEndX - FSelBegX + 1) - Length(S1)) + #13#10;
      Result := Result + S1;
    end;
  end
  else
  begin
    if FSelBegY = FSelEndY then
      Result := Copy(FLines[FSelEndY], FSelBegX + 1, FSelEndX - FSelBegX +
        Integer(FSelBlockFormat = bfInclusive))
    else
    begin
      Result := Copy(FLines[FSelBegY], FSelBegX + 1, Length(FLines[FSelBegY]));
      for i := FSelBegY + 1 to FSelEndY - 1 do
        Result := Result + #13#10 + FLines[i];
      Result := Result + #13#10 + Copy(FLines[FSelEndY], 1, FSelEndX +
        Integer(FSelBlockFormat = bfInclusive));
    end;
  end;
end;

procedure TJvCustomEditor.SetSelText(const AValue: string);
begin
  BeginUpdate;
  try
    BeginCompound;
    DeleteSelected;
    InsertText(AValue);
    FSelected := True;
    SelStart := PosFromCaret(FSelBegX, FSelBegY);
    SelLength := Length(AValue);
    EndCompound;
  finally
    EndUpdate;
  end;
end;

procedure TJvCustomEditor.ClipBoardCopy;
begin
  ClipBoard.SetTextBuf(PChar(GetSelText));
  SetClipboardBlockFormat(SelBlockFormat);
end;

{$IFDEF RAEDITOR_EDITOR}

procedure TJvCustomEditor.InsertText(const Text: string);
var
  S: string;
  P: integer;
  X, Y: integer;
begin
  PaintCaret(false);
  BeginUpdate;
  S := FLines.Text;
  P := PosFromCaret(FCaretX, FCaretY);
  {$IFDEF RAEDITOR_UNDO}
  TJvInsertUndo .Create(Self, FCaretX, FCaretY, Text);
  //FUndoBuffer.EndGroup;
  {$ENDIF RAEDITOR_UNDO}
  Insert(Text, S, P + 1);
  TextModified(P, maInsert, Text);
  FLines.Text := S; {!!! Вызывает перерисовку всего}
  CaretFromPos(P + Length(Text), X, Y);
  SetCaretInternal(X, Y);
  Changed;
  EndUpdate;
  PaintCaret(true);
end;

{заменяет слово в позиции курсора на NewString}
{ строка NewString не должна содержать #13, #10}

procedure TJvCustomEditor.ReplaceWord(const NewString: string);
var
  iBeg, iEnd: integer;

  function GetWordOnPos2(S: string; P: integer): string;
  begin
    Result := '';
    if P < 1 then exit;
    if (S[P] in Separators) and ((P < 1) or (S[P - 1] in Separators)) then
      inc(P);
    iBeg := P;
    while iBeg >= 1 do
      if S[iBeg] in Separators then
        break
      else
        dec(iBeg);
    inc(iBeg);
    iEnd := P;
    while iEnd <= Length(S) do
      if S[iEnd] in Separators then
        break
      else
        inc(iEnd);
    if iEnd > iBeg then
      Result := Copy(S, iBeg, iEnd - iBeg)
    else
      Result := S[P];
  end;

var
  S, W: string;
  X: integer;
  F: integer;
begin
  PaintCaret(false);
  BeginUpdate;
  F := PosFromCaret(FCaretX, FCaretY);
  S := FLines[FCaretY];
  while FCaretX > Length(S) do
    S := S + ' ';
  W := Trim(GetWordOnPos2(S, FCaretX));
  if W = '' then
  begin
    iBeg := FCaretX + 1;
    iEnd := FCaretX
  end;
  {$IFDEF RAEDITOR_UNDO}
  NotUndoable;
  //TJvReplaceUndo .Create(Self, FCaretX - Length(W), FCaretY, iBeg, iEnd, W, NewString);
  {$ENDIF RAEDITOR_UNDO}
  //  LW := Length(W);
  if FSelected then
  begin
    if (FSelBegY <= FCaretY) or (FCaretY >= FSelEndY) then
      // скорректировать LW ..
  end;
  Delete(S, iBeg, iEnd - iBeg);
  Insert(NewString, S, iBeg);
  FLines.Internal[FCaretY] := S;
  X := iBeg + Length(NewString) - 1;
  TextModified(F, maInsert, NewString);
  PaintLine(FCaretY, -1, -1);
  SetCaretInternal(X, FCaretY);
  Changed;
  EndUpdate;
  PaintCaret(true);
end;

{заменяет слово в позиции курсора на NewString}

procedure TJvCustomEditor.ReplaceWord2(const NewString: string);
var
  S, S1, W: string;
  P, X, Y: integer;
  iBeg, iEnd: integer;
  NewCaret: integer;
begin
  PaintCaret(false);
  if FCaretX > Length(FLines[FCaretY]) then
    FLines.Internal[FCaretY] := FLines[FCaretY] +
      Spaces(FCaretX - Length(FLines[FCaretY]));
  S := FLines.Text;
  P := PosFromCaret(FCaretX, FCaretY);
  W := Trim(GetWordOnPosEx(S, P, iBeg, iEnd));
  if W = '' then
  begin
    iBeg := P + 1;
    iEnd := P
  end;
  S1 := NewString;
  NewCaret := Length(NewString);
  {$IFDEF RAEDITOR_UNDO}
  TJvReplaceUndo .Create(Self, FCaretX, FCaretY, iBeg, iEnd, W, S1);
  {$ENDIF RAEDITOR_UNDO}
  //  LW := Length(W);
  if FSelected then
  begin
    if (FSelBegY <= FCaretY) or (FCaretY >= FSelEndY) then
      // скорректировать LW ..
  end;
  Delete(S, iBeg, iEnd - iBeg);
  Insert(S1, S, iBeg);
  FLines.Text := S; {!!! Вызывает перерисовку всего}
  CaretFromPos(iBeg + NewCaret - 1, X, Y);
  SetCaretInternal(X, Y);
  Changed;
  PaintCaret(true);
end;
{$ENDIF RAEDITOR_EDITOR}

procedure TJvCustomEditor.ClipBoardPaste;
var
  S, ClipS: string;
  Len, P: integer;
  H: THandle;
  X, Y: integer;
  SS: TStringList;
  i: Integer;
begin
  if (FCaretY > FLines.Count - 1) and (FLines.Count > 0) then Err;
  BeginUpdate;
  H := ClipBoard.GetAsHandle(CF_TEXT);
  Len := GlobalSize(H);
  if Len = 0 then exit;
  SetLength(ClipS, Len);
  SetLength(ClipS, ClipBoard.GetTextBuf(PChar(ClipS), Len));
  ClipS := ExpandTabs(AdjustLineBreaks(ClipS));
  PaintCaret(false);

  BeginCompound;
  try
    DeleteSelected;
    if FLines.Count > 0 then
      ReLine;
    FSelBlockFormat := GetClipBoardBlockFormat;
    if FSelBlockFormat in [bfInclusive, bfNonInclusive] then
    begin
      S := FLines.Text;
      if FLines.Count > 0 then
        P := PosFromCaret(FCaretX, FCaretY)
      else
        P := 0;
     {$IFDEF RAEDITOR_UNDO}
      TJvInsertUndo .Create(Self, FCaretX, FCaretY, ClipS);
     {$ENDIF RAEDITOR_UNDO}
      Insert(ClipS, S, P + 1);
      FLines.SetLockText(S);
      TextModified(P, maInsert, ClipS);
      CaretFromPos(P + Length(ClipS), X, Y);
    end
    else if FSelBlockFormat = bfColumn then
    begin
     {$IFDEF RAEDITOR_UNDO}
      TJvInsertColumnUndo .Create(Self, FCaretX, FCaretY, ClipS);
      //NotUndoable;
     {$ENDIF RAEDITOR_UNDO}
      SS := TStringList.Create;
      try
        SS.Text := ClipS;
        for i := 0 to SS.Count - 1 do
        begin
          if FCaretY + i > FLines.Count - 1 then
            FLines.Add(Spaces(FCaretX));
          S := FLines[FCaretY + i];
          Insert(SS[i], S, FCaretX + 1);
          FLines[FCaretY + i] := S;
        end;
        X := FCaretX;
        Y := FCaretY + SS.Count;
      finally
        SS.Free;
      end;
    end;
  finally
    EndCompound;
  end;

  SetCaretInternal(X, Y);
  Changed;
  EndUpdate; {!!! Вызывает перерисовку всего}
  PaintCaret(true);
end;

procedure TJvCustomEditor.ClipBoardCut;
begin
  ClipBoardCopy;
  DeleteSelected;
end;

procedure TJvCustomEditor.DeleteSelected;
var
  S, S1: string;
  i, iBeg, iEnd, X, Y: integer;
begin
  if FSelected then
  begin
    PaintCaret(false);
    BeginUpdate;
    {$IFDEF RAEDITOR_UNDO}
    TJvDeleteSelectedUndo .Create(Self, FCaretX, FCaretY, GetSelText,
      FSelBlockFormat, FSelBegX, FSelBegY, FSelEndX, FSelEndY);
    {$ENDIF RAEDITOR_UNDO}
    if FSelBlockFormat in [bfInclusive, bfNonInclusive] then
    begin
      S := FLines.Text;
      iBeg := PosFromCaret(FSelBegX, FSelBegY);
      iEnd := PosFromCaret(FSelEndX + Integer(FSelBlockFormat = bfInclusive), FSelEndY);
      Delete(S, iBeg + 1, iEnd - iBeg);
      S1 := GetSelText;
      FSelected := false;
      FLines.SetLockText(S);
      TextModified(iBeg, maDelete, S1);
      CaretFromPos(iBeg, X, Y);
    end
    else if FSelBlockFormat = bfColumn then
    begin
      Y := FCaretY;
      X := FSelBegX;
      iBeg := PosFromCaret(FSelBegX, FSelBegY);
      for i := FSelBegY to FSelEndY do
      begin
        S := FLines[i];
        Delete(S, FSelBegX + 1, FSelEndX - FSelBegX + 1);
        FLines.Internal[i] := S;
      end;
      FSelected := false;
      TextModified(iBeg, maDeleteColumn, S1);
    end;
    SetCaretInternal(X, Y);
    Changed;
    EndUpdate;
    PaintCaret(true);
  end;
end;
{############### ClipBoard ###############}

procedure TJvCustomEditor.SetGutterWidth(AWidth: integer);
begin
  if FGutterWidth <> AWidth then
  begin
    FGutterWidth := AWidth;
    UpdateEditorSize;
    Invalidate;
  end;
end;

procedure TJvCustomEditor.SetGutterColor(AColor: TColor);
begin
  if FGutterColor <> AColor then
  begin
    FGutterColor := AColor;
    FGutter.Invalidate;
  end;
end;

function TJvCustomEditor.GetLines: TStrings;
begin
  Result := FLines;
end;

procedure TJvCustomEditor.SetLines(ALines: TStrings);
begin
  if ALines <> nil then FLines.Assign(ALines);
 {$IFDEF RAEDITOR_UNDO}
  NotUndoable;
 {$ENDIF RAEDITOR_UNDO}
end;

procedure TJvCustomEditor.TextAllChanged;
begin
  TextAllChangedInternal(True);
end;

procedure TJvCustomEditor.TextAllChangedInternal(const Unselect: Boolean);
begin
  if Unselect then
    FSelected := false;
  TextModified(0, maInsert, FLines.Text);
  UpdateEditorSize;
  if Showing and (FUpdateLock = 0) then Invalidate;
end;

procedure TJvCustomEditor.SetCols(ACols: integer);
begin
  if FCols <> ACols then
  begin
    FCols := Max(ACols, 1);
    scbHorz.Max := FCols - 1;
  end;
end;

procedure TJvCustomEditor.SetRows(ARows: integer);
begin
  if FRows <> ARows then
  begin
    FRows := Max(ARows, 1);
    scbVert.Max := Max(1, FRows - 1 + FVisibleRowCount - 1);
  end;
end;

procedure TJvCustomEditor.SetLeftTop(ALeftCol, ATopRow: integer);
begin
  if ALeftCol < 0 then ALeftCol := 0;
  if (FLeftCol <> ALeftCol) then
  begin
    scbHorz.Position := ALeftCol;
    Scroll(false, ALeftCol);
  end;
  if ATopRow < 0 then ATopRow := 0;
  if (FTopRow <> ATopRow) then
  begin
    scbVert.Position := ATopRow;
    Scroll(true, ATopRow);
  end;
end;

procedure TJvCustomEditor.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomEditor.SetRightMarginVisible(Value: boolean);
begin
  if FRightMarginVisible <> Value then
  begin
    FRightMarginVisible := Value;
    Invalidate;
  end;
end;

procedure TJvCustomEditor.SetRightMargin(Value: integer);
begin
  if FRightMargin <> Value then
  begin
    FRightMargin := Value;
    Invalidate;
  end;
end;

procedure TJvCustomEditor.SetRightMarginColor(Value: TColor);
begin
  if FRightMarginColor <> Value then
  begin
    FRightMarginColor := Value;
    Invalidate;
  end;
end;

function TJvCustomEditor.ExpandTabs(const S: string): string;
var
  i: integer;
  Sp: string;
begin
  { very slow and not complete implementation - NEED TO OPTIMIZE ! }
  if Pos(#9, S) > 0 then
  begin
    Sp := Spaces(GetDefTabStop(0 {!!}, True));
    Result := '';
    for i := 1 to Length(S) do
      if S[i] = #9 then
        Result := Result + Sp
      else
        Result := Result + S[i];
  end
  else
    Result := S;
end;

// add by patofan
{$IFDEF COMPILER3_UP}
function TJvCustomEditor.CheckDoubleByteChar(  var x : integer ; y : integer ; ByteType : TMbcsByteType ;
  delta_inc : integer ):boolean;
var
  CurByteType : TMbcsByteType;
begin
  result := false;
  try
      if ( y >= 0 ) and ( x >= 0 ) and ( y < Flines.Count ) then
      begin
        CurByteType := StrByteType( pchar( FLines[ y ] ) , x  );
        if (CurByteType = ByteType ) then
        begin
          x := x  + delta_inc;
          result := true;
        end;
      end;
  except
    on E:EStringListError do
  end;
end;
{$ENDIF COMPILER3_UP}
// ending add by patofan

{ ************************ triggers ************************ }

procedure TJvCustomEditor.TextModified(Pos: integer; Action: TModifiedAction;
  Text: string);
begin
  { This method don't called at all cases, when text is modified,
    so don't use it. Later I may be complete it. }
end;

procedure TJvCustomEditor.Changed;
begin
  FModified := true;
  FPEditBuffer := nil;
  if Assigned(FOnChange) then FOnChange(self);
  StatusChanged;
end;

procedure TJvCustomEditor.StatusChanged;
begin
  if Assigned(FOnChangeStatus) then FOnChangeStatus(self);
end;

procedure TJvCustomEditor.CaretFromPos(const Pos: integer; var X, Y: integer);
{возвращает по индексу Pos - номеру символа - его координаты}
begin
  GetXYByPos(FLines.Text, Pos, X, Y);
end;

function TJvCustomEditor.PosFromCaret(const X, Y: integer): integer;
{наоборот}
var
  i: integer;
begin
  if (Y > FLines.Count - 1) or (Y < 0) then
    Result := -1
  else
  begin
    Result := 0;
    for i := 0 to Y - 1 do
      inc(Result, Length(FLines[i]) + 2 {CR/LF});
    if X < Length(FLines[Y]) then
      inc(Result, X)
    else
      inc(Result, Length(FLines[Y]))
  end;
end;

function TJvCustomEditor.PosFromMouse(const X, Y: integer): integer;
var
  X1, Y1: integer;
begin
  Mouse2Caret(X, Y, X1, Y1);
  if (X1 < 0) or (Y1 < 0) then
    Result := -1
  else
    Result := PosFromCaret(X1, Y1);
end;

function TJvCustomEditor.GetTextLen: Integer;
begin
  Result := Length(FLines.Text);
end;

function TJvCustomEditor.GetSelStart: integer;
begin
  Result := PosFromCaret(FCaretX, FCaretY);
end;

procedure TJvCustomEditor.SetSelStart(const ASelStart: integer);
begin
  FSelected := true;
  CaretFromPos(ASelStart, FSelBegX, FSelBegY);
 { FCaretX := FSelBegX;
  FCaretY := FSelBegY; }
  SetCaretInternal(FSelBegX, FSelBegY);
  SetSelLength(0);
  MakeRowVisible(FSelBegY);
  //  PaintSelection;
  //  EditorPaint;
end;

procedure TJvCustomEditor.MakeRowVisible(ARow: Integer);
begin
  if (ARow < FTopRow) or (ARow > FLastVisibleRow) then
  begin
    ARow := ARow {mac: bugfix - FCaretY} - Trunc(VisibleRowCount / 2);
    if ARow < 0 then ARow := 0;
    SetLeftTop(FLeftCol, ARow);
  end;
end;

function TJvCustomEditor.GetSelLength: integer;
begin
  Result := Length(GetSelText);
end;

procedure TJvCustomEditor.SetSelLength(const ASelLength: integer);
begin
  FSelected := ASelLength > 0;
  CaretFromPos(SelStart + ASelLength, FSelEndX, FSelEndY);
  FUpdateSelBegY := FSelBegY;
  FUpdateSelEndY := FSelEndY;
  SetCaretInternal(FSelEndX, FSelEndY);
  //PaintSelection;
  Invalidate;
end;

procedure TJvCustomEditor.SetLockText(const Text: string);
begin
  FLines.SetLockText(Text);
end;

procedure TJvCustomEditor.GutterPaint(Canvas: TCanvas);
begin
  if Assigned(FOnPaintGutter) then FOnPaintGutter(Self, Canvas);
end;

procedure TJvCustomEditor.SetMode(index: integer; Value: boolean);
var
  PB: ^boolean;
begin
  case index of
    0: PB := @FInsertMode;
    else {1 :}
      PB := @FReadOnly;
  end;
  if PB^ <> Value then
  begin
    PB^ := Value;
    StatusChanged;
  end;
end;

function TJvCustomEditor.GetWordOnCaret: string;
begin
  Result := GetWordOnPos(FLines[CaretY], CaretX + 1);
end;

function TJvCustomEditor.GetTabStop(const X, Y: Integer; const What: TTabStop;
  const Next: Boolean): Integer;

  procedure UpdateTabStops;
  var
    S: string;
    j, i: Integer;
  begin
    FillChar(FTabPos, sizeof(FTabPos), False);
    if (What = tsAutoIndent) or FSmartTab then
    begin
      j := 1;
      i := 1;
      while Y - j >= 0 do
      begin
        S := TrimRight(FLines[Y - j]);
        if Length(S) > i then
          FTabPos[Length(S)] := True;
        while i <= Length(S) do { Iterate }
        begin
          if CharInSet(S[i], StIdSymbols) then
          begin
            FTabPos[i - 1] := True;
            while (i <= Length(S)) and CharInSet(S[i], StIdSymbols) do
              inc(i);
          end;
          inc(i);
        end; { for }
        if i >= Max_X_Scroll then Break;
        if j >= FVisibleRowCount * 2 then Break;
        inc(j);
      end;
    end;
  end;

var
  i: integer;
begin
  UpdateTabStops;
  Result := X;
  if Next then
  begin
    for i := X + 1 to High(FTabPos) do { Iterate }
      if FTabPos[i] then
      begin
        Result := i;
        Exit;
      end;
    if Result = X then
      Result := GetDefTabStop(X, True);
  end
  else
  begin
    if Result = X then
      Result := GetDefTabStop(X, False);
  end;
end;

function TJvCustomEditor.GetDefTabStop(const X: integer; const Next: Boolean)
  : Integer;
var
  i: Integer;
  S: string;
  A, B: Integer;
begin
  i := 0;
  S := Trim(SubStr(FTabStops, i, ' '));
  A := 0; B := 1;
  while S <> '' do
  begin
    A := B;
    B := StrToInt(S) - 1;
    if B > X then
    begin
      Result := B;
      Exit;
    end;
    inc(i);
    S := Trim(SubStr(FTabStops, i, ' '));
  end;
  { after last tab pos }
  Result := X + ((B - A) - ((X - B) mod (B - A)));
end;

function TJvCustomEditor.GetBackStop(const X, Y: Integer): Integer;

  procedure UpdateBackStops;
  var
    S: string;
    j, i, k: Integer;
  begin
    j := 1;
    i := X - 1;
    FillChar(FTabPos, sizeof(FTabPos), False);
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
      if i = 1 then Break;
      if j >= FVisibleRowCount * 2 then Break;
      inc(j);
    end;
  end;

var
  i: integer;
  S: string;
begin
  Result := X - 1;
  S := TrimRight(FLines[Y]);
  if (Trim(Copy(S, 1, X)) = '') and
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

procedure TJvCustomEditor.BeginCompound;
begin
  inc(FCompound);
 {$IFDEF RAEDITOR_UNDO}
  TJvBeginCompoundUndo .Create(Self);
 {$ENDIF RAEDITOR_UNDO}
end;

procedure TJvCustomEditor.EndCompound;
begin
 {$IFDEF RAEDITOR_UNDO}
  TJvEndCompoundUndo .Create(Self);
 {$ENDIF RAEDITOR_UNDO}
  dec(FCompound);
end;

procedure TJvCustomEditor.BeginRecord;
begin
  FMacro := '';
  FRecording := True;
  StatusChanged;
end;

procedure TJvCustomEditor.EndRecord(var AMacro: TMacro);
begin
  FRecording := False;
  AMacro := FMacro;
  StatusChanged;
end;

procedure TJvCustomEditor.PlayMacro(const AMacro: TMacro);
var
  i: Integer;
begin
  BeginUpdate;
  BeginCompound;
  try
    i := 1;
    while i < Length(AMacro) do
    begin
      Command(byte(AMacro[i]) + byte(AMacro[i + 1]) shl 8);
      inc(i, 2);
    end;
  finally
    EndCompound;
    EndUpdate;
  end;
end;

function TJvCustomEditor.CanCopy: Boolean;
begin
  Result := (FSelected) and ((FSelBegX<>FSelEndX) or
    (FSelBegY<>FSelEndY));//SeBco 03/10/01
end;

function TJvCustomEditor.CanCut: Boolean;
begin
  Result := CanCopy;
end;

function TJvCustomEditor.CanPaste: Boolean;
var
  H: THandle;
  Len: Integer;
begin
  Result := False;
  if (FCaretY > FLines.Count - 1) and (FLines.Count > 0) then Exit;
  H := ClipBoard.GetAsHandle(CF_TEXT);
  Len := GlobalSize(H);
  Result := (Len > 0);
end;


{************************** TJvEditKey  ***************************}

constructor TJvEditKey .Create(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState);
begin
  Key1 := AKey1;
  Shift1 := AShift1;
  Command := ACommand;
end;

constructor TJvEditKey .Create2(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState; const AKey2: word; const AShift2: TShiftState);
begin
  Key1 := AKey1;
  Shift1 := AShift1;
  Key2 := AKey2;
  Shift2 := AShift2;
  Command := ACommand;
end;

constructor TJvKeyboard .Create;
begin
  List := TList.Create;
end;

destructor TJvKeyboard .Destroy;
begin
  Clear;
  List.Free;
end;

procedure TJvKeyboard .Add(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState);
begin
  List.Add(TJvEditKey .Create(ACommand, AKey1, AShift1));
end;

procedure TJvKeyboard .Add2(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState; const AKey2: word; const AShift2: TShiftState);
begin
  List.Add(TJvEditKey .Create2(ACommand, AKey1, AShift1, AKey2, AShift2));
end;

procedure TJvKeyboard .Clear;
var
  i: integer;
begin
  for i := 0 to List.Count - 1 do
    TObject(List[i]).Free;
  List.Clear;
end;

function TJvKeyboard .Command(const AKey: word; const AShift: TShiftState):
  TEditCommand;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to List.Count - 1 do
    with TJvEditKey (List[i]) do
      if (Key1 = AKey) and (Shift1 = AShift) then
      begin
        if Key2 = 0 then
          Result := Command
        else
          Result := twoKeyCommand;
        Exit;
      end;
end;

function TJvKeyboard .Command2(const AKey1: word; const AShift1: TShiftState;
  const AKey2: word; const AShift2: TShiftState): TEditCommand;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to List.Count - 1 do
    with TJvEditKey (List[i]) do
      if (Key1 = AKey1) and (Shift1 = AShift1) and
        (Key2 = AKey2) and (Shift2 = AShift2) then
      begin
        Result := Command;
        Exit;
      end;
end;

{$IFDEF RAEDITOR_EDITOR}
{$IFDEF RAEDITOR_DEFLAYOT}

procedure TJvKeyboard .SetDefLayot;
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
  Add(ecSelAll, ord('A'), [ssCtrl]);

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
  Add(ecDelete, VK_DELETE, []);
  Add(ecTab, VK_TAB, []);
  Add(ecBackTab, VK_TAB, [ssShift]);
  Add(ecDeleteSelected, VK_DELETE, [ssCtrl]);
  Add(ecClipboardCopy, VK_INSERT, [ssCtrl]);
  Add(ecClipboardCut, VK_DELETE, [ssShift]);
  Add(ecClipBoardPaste, VK_INSERT, [ssShift]);

  Add(ecClipboardCopy, ord('C'), [ssCtrl]);
  Add(ecClipboardCut, ord('X'), [ssCtrl]);
  Add(ecClipBoardPaste, ord('V'), [ssCtrl]);


  Add(ecSetBookmark0, ord('0'), [ssCtrl, ssShift]);
  Add(ecSetBookmark1, ord('1'), [ssCtrl, ssShift]);
  Add(ecSetBookmark2, ord('2'), [ssCtrl, ssShift]);
  Add(ecSetBookmark3, ord('3'), [ssCtrl, ssShift]);
  Add(ecSetBookmark4, ord('4'), [ssCtrl, ssShift]);
  Add(ecSetBookmark5, ord('5'), [ssCtrl, ssShift]);
  Add(ecSetBookmark6, ord('6'), [ssCtrl, ssShift]);
  Add(ecSetBookmark7, ord('7'), [ssCtrl, ssShift]);
  Add(ecSetBookmark8, ord('8'), [ssCtrl, ssShift]);
  Add(ecSetBookmark9, ord('9'), [ssCtrl, ssShift]);

  Add(ecGotoBookmark0, ord('0'), [ssCtrl]);
  Add(ecGotoBookmark1, ord('1'), [ssCtrl]);
  Add(ecGotoBookmark2, ord('2'), [ssCtrl]);
  Add(ecGotoBookmark3, ord('3'), [ssCtrl]);
  Add(ecGotoBookmark4, ord('4'), [ssCtrl]);
  Add(ecGotoBookmark5, ord('5'), [ssCtrl]);
  Add(ecGotoBookmark6, ord('6'), [ssCtrl]);
  Add(ecGotoBookmark7, ord('7'), [ssCtrl]);
  Add(ecGotoBookmark8, ord('8'), [ssCtrl]);
  Add(ecGotoBookmark9, ord('9'), [ssCtrl]);

  Add2(ecSetBookmark0, ord('K'), [ssCtrl], ord('0'), []);
  Add2(ecSetBookmark0, ord('K'), [ssCtrl], ord('0'), [ssCtrl]);
  Add2(ecSetBookmark1, ord('K'), [ssCtrl], ord('1'), []);
  Add2(ecSetBookmark1, ord('K'), [ssCtrl], ord('1'), [ssCtrl]);
  Add2(ecSetBookmark2, ord('K'), [ssCtrl], ord('2'), []);
  Add2(ecSetBookmark2, ord('K'), [ssCtrl], ord('2'), [ssCtrl]);
  Add2(ecSetBookmark3, ord('K'), [ssCtrl], ord('3'), []);
  Add2(ecSetBookmark3, ord('K'), [ssCtrl], ord('3'), [ssCtrl]);
  Add2(ecSetBookmark4, ord('K'), [ssCtrl], ord('4'), []);
  Add2(ecSetBookmark4, ord('K'), [ssCtrl], ord('4'), [ssCtrl]);
  Add2(ecSetBookmark5, ord('K'), [ssCtrl], ord('5'), []);
  Add2(ecSetBookmark5, ord('K'), [ssCtrl], ord('5'), [ssCtrl]);
  Add2(ecSetBookmark6, ord('K'), [ssCtrl], ord('6'), []);
  Add2(ecSetBookmark6, ord('K'), [ssCtrl], ord('6'), [ssCtrl]);
  Add2(ecSetBookmark7, ord('K'), [ssCtrl], ord('7'), []);
  Add2(ecSetBookmark7, ord('K'), [ssCtrl], ord('7'), [ssCtrl]);
  Add2(ecSetBookmark8, ord('K'), [ssCtrl], ord('8'), []);
  Add2(ecSetBookmark8, ord('K'), [ssCtrl], ord('8'), [ssCtrl]);
  Add2(ecSetBookmark9, ord('K'), [ssCtrl], ord('9'), []);
  Add2(ecSetBookmark9, ord('K'), [ssCtrl], ord('9'), [ssCtrl]);

  Add2(ecGotoBookmark0, ord('Q'), [ssCtrl], ord('0'), []);
  Add2(ecGotoBookmark0, ord('Q'), [ssCtrl], ord('0'), [ssCtrl]);
  Add2(ecGotoBookmark1, ord('Q'), [ssCtrl], ord('1'), []);
  Add2(ecGotoBookmark1, ord('Q'), [ssCtrl], ord('1'), [ssCtrl]);
  Add2(ecGotoBookmark2, ord('Q'), [ssCtrl], ord('2'), []);
  Add2(ecGotoBookmark2, ord('Q'), [ssCtrl], ord('2'), [ssCtrl]);
  Add2(ecGotoBookmark3, ord('Q'), [ssCtrl], ord('3'), []);
  Add2(ecGotoBookmark3, ord('Q'), [ssCtrl], ord('3'), [ssCtrl]);
  Add2(ecGotoBookmark4, ord('Q'), [ssCtrl], ord('4'), []);
  Add2(ecGotoBookmark4, ord('Q'), [ssCtrl], ord('4'), [ssCtrl]);
  Add2(ecGotoBookmark5, ord('Q'), [ssCtrl], ord('5'), []);
  Add2(ecGotoBookmark5, ord('Q'), [ssCtrl], ord('5'), [ssCtrl]);
  Add2(ecGotoBookmark6, ord('Q'), [ssCtrl], ord('6'), []);
  Add2(ecGotoBookmark6, ord('Q'), [ssCtrl], ord('6'), [ssCtrl]);
  Add2(ecGotoBookmark7, ord('Q'), [ssCtrl], ord('7'), []);
  Add2(ecGotoBookmark7, ord('Q'), [ssCtrl], ord('7'), [ssCtrl]);
  Add2(ecGotoBookmark8, ord('Q'), [ssCtrl], ord('8'), []);
  Add2(ecGotoBookmark8, ord('Q'), [ssCtrl], ord('8'), [ssCtrl]);
  Add2(ecGotoBookmark9, ord('Q'), [ssCtrl], ord('9'), []);
  Add2(ecGotoBookmark9, ord('Q'), [ssCtrl], ord('9'), [ssCtrl]);

  Add2(ecNonInclusiveBlock, ord('O'), [ssCtrl], ord('K'), [ssCtrl]);
  Add2(ecInclusiveBlock, ord('O'), [ssCtrl], ord('I'), [ssCtrl]);
  Add2(ecColumnBlock, ord('O'), [ssCtrl], ord('C'), [ssCtrl]);

  {$IFDEF RAEDITOR_UNDO}
  Add(ecUndo, ord('Z'), [ssCtrl]);
  Add(ecUndo, VK_BACK, [ssAlt]);
  {$ENDIF RAEDITOR_UNDO}

  {$IFDEF RAEDITOR_COMPLETION}
  Add(ecCompletionIdentifers, VK_SPACE, [ssCtrl]);
  Add(ecCompletionTemplates, ord('J'), [ssCtrl]);
  {$ENDIF RAEDITOR_COMPLETION}

  { cursor movement - default and classic }
  Add2(ecEndDoc, ord('Q'), [ssCtrl], ord('C'), []);
  Add2(ecEndLine, ord('Q'), [ssCtrl], ord('D'), []);
  Add2(ecWindowTop, ord('Q'), [ssCtrl], ord('E'), []);
  Add2(ecLeft, ord('Q'), [ssCtrl], ord('P'), []);
  Add2(ecBeginDoc, ord('Q'), [ssCtrl], ord('R'), []);
  Add2(ecBeginLine, ord('Q'), [ssCtrl], ord('S'), []);
  Add2(ecWindowTop, ord('Q'), [ssCtrl], ord('T'), []);
  Add2(ecWindowBottom, ord('Q'), [ssCtrl], ord('U'), []);
  Add2(ecWindowBottom, ord('Q'), [ssCtrl], ord('X'), []);
  Add2(ecEndDoc, ord('Q'), [ssCtrl], ord('C'), [ssCtrl]);
  Add2(ecEndLine, ord('Q'), [ssCtrl], ord('D'), [ssCtrl]);
  Add2(ecWindowTop, ord('Q'), [ssCtrl], ord('E'), [ssCtrl]);
  Add2(ecLeft, ord('Q'), [ssCtrl], ord('P'), [ssCtrl]);
  Add2(ecBeginDoc, ord('Q'), [ssCtrl], ord('R'), [ssCtrl]);
  Add2(ecBeginLine, ord('Q'), [ssCtrl], ord('S'), [ssCtrl]);
  Add2(ecWindowTop, ord('Q'), [ssCtrl], ord('T'), [ssCtrl]);
  Add2(ecWindowBottom, ord('Q'), [ssCtrl], ord('U'), [ssCtrl]);
  Add2(ecWindowBottom, ord('Q'), [ssCtrl], ord('X'), [ssCtrl]);

  Add(ecDeleteWord, ord('T'), [ssCtrl]);
  Add(ecInsertPara, ord('N'), [ssCtrl]);
  Add(ecDeleteLine, ord('Y'), [ssCtrl]);

  Add2(ecSelWord, ord('K'), [ssCtrl], ord('T'), [ssCtrl]);
  Add2(ecToUpperCase, ord('K'), [ssCtrl], ord('O'), [ssCtrl]);
  Add2(ecToLowerCase, ord('K'), [ssCtrl], ord('N'), [ssCtrl]);
  Add2(ecChangeCase, ord('O'), [ssCtrl], ord('U'), [ssCtrl]);
  Add2(ecIndent, ord('K'), [ssCtrl], ord('I'), [ssCtrl]);
  Add2(ecUnindent, ord('K'), [ssCtrl], ord('U'), [ssCtrl]);

  Add(ecRecordMacro, ord('R'), [ssCtrl, ssShift]);
  Add(ecPlayMacro, ord('P'), [ssCtrl, ssShift]);
end;
{$ENDIF RAEDITOR_DEFLAYOT}


{$IFDEF RAEDITOR_UNDO}

procedure RedoNotImplemented;
begin
  raise EJvEditorError .Create('Redo not yet implemented');
end;

procedure TJvCustomEditor.NotUndoable;
begin
  FUndoBuffer.Clear;
end;

procedure TUndoBuffer.Add(AUndo: TUndo);
begin
  if InUndo then exit;
  while (Count > 0) and (FPtr < Count - 1) do
  begin
    TUndo(Items[FPtr + 1]).Free;
    inherited Delete(FPtr + 1);
  end;
  inherited Add(AUndo);
  FPtr := Count - 1;
end;

procedure TUndoBuffer.Undo;
var
  UndoClass: TClass;
  Compound: Integer;
begin
  InUndo := true;
  try
    if LastUndo <> nil then
    begin
      Compound := 0;
      UndoClass := LastUndo.ClassType;
      while (LastUndo <> nil) and
        ((UndoClass = LastUndo.ClassType) or
        (LastUndo is TJvDeleteTrailUndo ) or
        (LastUndo is TJvReLineUndo ) or
        (Compound > 0)) do
      begin
        if LastUndo.ClassType = TJvBeginCompoundUndo  then
        begin
          dec(Compound);
          UndoClass := nil;
        end
        else if LastUndo.ClassType = TJvEndCompoundUndo  then
          inc(Compound);
        LastUndo.Undo;
        dec(FPtr);
        if (UndoClass = TJvDeleteTrailUndo ) or
          (UndoClass = TJvReLineUndo ) then
          UndoClass := LastUndo.ClassType;
        if not FRAEditor.FGroupUndo then break;
        // FRAEditor.Paint; {DEBUG !!!!!!!!!}
      end;
      if FRAEditor.FUpdateLock = 0 then
      begin
        FRAEditor.TextAllChangedInternal(False);
        FRAEditor.Changed;
      end;
    end;
  finally
    InUndo := false;
  end;
end;

procedure TUndoBuffer.Redo;
begin
  { DEBUG !!!! }
  inc(FPtr);
  LastUndo.Redo;
end;

procedure TUndoBuffer.Clear;
begin
  while Count > 0 do
  begin
    TUndo(Items[0]).Free;
    inherited Delete(0);
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

function TUndoBuffer.IsNewGroup(AUndo: TUndo): boolean;
begin
  Result := (LastUndo = nil) or (LastUndo.ClassType <> AUndo.ClassType)
end;

function TUndoBuffer.CanUndo: boolean;
begin
  Result := (LastUndo <> nil);
end;

{* TUndo}

constructor TUndo.Create(ARAEditor: TJvCustomEditor);
begin
  FRAEditor := ARAEditor;
  UndoBuffer.Add(Self);
end;

function TUndo.UndoBuffer: TUndoBuffer;
begin
  if FRAEditor <> nil then
    Result := FRAEditor.FUndoBuffer
  else
    Result := nil;
end;
{* TUndo}

{* TJvCaretUndo }

constructor TJvCaretUndo .Create(ARAEditor: TJvCustomEditor; ACaretX, ACaretY:
  integer);
begin
  inherited Create(ARAEditor);
  FCaretX := ACaretX;
  FCaretY := ACaretY;
end;

procedure TJvCaretUndo .Undo;
begin
  with UndoBuffer do
  begin
    dec(FPtr);
    while FRAEditor.FGroupUndo and (FPtr >= 0) and not IsNewGroup(Self) do
      dec(FPtr);
    inc(FPtr);
    with TJvCaretUndo (Items[FPtr]) do
      FRAEditor.SetCaretInternal(FCaretX, FCaretY);
  end;
end;

procedure TJvCaretUndo .Redo;
begin
  RedoNotImplemented;
end;
{# TJvCaretUndo }

{* TJvInsertUndo }

constructor TJvInsertUndo .Create(ARAEditor: TJvCustomEditor; ACaretX, ACaretY:
  integer; AText: string);
begin
  inherited Create(ARAEditor, ACaretX, ACaretY);
  FText := AText;
end;

procedure TJvInsertUndo .Undo;
var
  S, Text: string;
  iBeg: integer;
begin
  Text := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := TJvInsertUndo (LastUndo).FText + Text;
      dec(FPtr);
      if not FRAEditor.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  with TJvInsertUndo (UndoBuffer.Items[UndoBuffer.FPtr]) do
  begin
    S := FRAEditor.FLines.Text;
    iBeg := FRAEditor.PosFromCaret(FCaretX, FCaretY);
    Delete(S, iBeg + 1, Length(Text));
    FRAEditor.FLines.SetLockText(S);
    FRAEditor.SetCaretInternal(FCaretX, FCaretY);
  end;
end;

{ TJvInsertColumnUndo  }
procedure TJvInsertColumnUndo .Undo;
var
  SS: TStringList;
  i: Integer;
  S: string;
begin
  { not optimized }
  SS := TStringList.Create;
  try
    SS.Text := FText;
    for i := 0 to SS.Count - 1 do
    begin
      S := FRAEditor.FLines[FCaretY + i];
      Delete(S, FCaretX + 1, Length(SS[i]));
      FRAEditor.FLines[FCaretY + i] := S;
    end;
  finally
    SS.Free;
  end;
  FRAEditor.SetCaretInternal(FCaretX, FCaretY);
end;

constructor TJvOverwriteUndo .Create(ARAEditor: TJvCustomEditor; ACaretX,
  ACaretY: integer; AOldText, ANewText: string);
begin
  inherited Create(ARAEditor, ACaretX, ACaretY);
  FOldText := AOldText;
  FNewText := ANewText;
end;

procedure TJvOverwriteUndo .Undo;
var
  S: string;
begin
  { not optimized }
  S := FRAEditor.Lines[FCaretY];
  S[FCaretX + 1] := FOldText[1];
  FRAEditor.Lines[FCaretY] := S;
  FRAEditor.SetCaretInternal(FCaretX, FCaretY);
end;

{* TJvDeleteUndo }

procedure TJvDeleteUndo .Undo;
var
  S, Text: string;
  iBeg: integer;
begin
  Text := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := TJvDeleteUndo (LastUndo).FText + Text;
      dec(FPtr);
      if not FRAEditor.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  with TJvDeleteUndo (UndoBuffer.Items[UndoBuffer.FPtr]) do
  begin
    S := FRAEditor.FLines.Text;
    iBeg := FRAEditor.PosFromCaret({mac: FRAEditor.}FCaretX, {mac: FRAEditor.}FCaretY);
    Insert(Text, S, iBeg + 1);
    FRAEditor.FLines.SetLockText(S);
    FRAEditor.SetCaretInternal(FCaretX, FCaretY);
  end;
end;
{# TJvDeleteUndo }

{* TJvBackspaceUndo }

procedure TJvBackspaceUndo .Undo;
var
  S, Text: string;
  iBeg: integer;
begin
  Text := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := Text + TJvDeleteUndo (LastUndo).FText;
      dec(FPtr);
      if not FRAEditor.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  with TJvDeleteUndo (UndoBuffer.Items[UndoBuffer.FPtr]) do
  begin
    S := FRAEditor.FLines.Text;
    iBeg := FRAEditor.PosFromCaret({mac: FRAEditor.}FCaretX, {mac: FRAEditor.}FCaretY);
    Insert(Text, S, iBeg + 1);
    FRAEditor.FLines.SetLockText(S);
    FRAEditor.SetCaretInternal(FCaretX, FCaretY);
  end;
end;
{# TJvBackspaceUndo }

{* TJvReplaceUndo }

constructor TJvReplaceUndo .Create(ARAEditor: TJvCustomEditor; ACaretX, ACaretY:
  integer; ABeg, AEnd: integer; AText, ANewText: string);
begin
  inherited Create(ARAEditor, ACaretX, ACaretY);
  FBeg := ABeg;
  FEnd := AEnd;
  FText := AText;
  FNewText := ANewText;
end;

procedure TJvReplaceUndo .Undo;
var
  S: string;
begin
  S := FRAEditor.FLines.Text;
  Delete(S, FBeg, Length(FNewText));
  Insert(FText, S, FBeg);
  FRAEditor.FLines.SetLockText(S);
  FRAEditor.SetCaretInternal(FCaretX, FCaretY);
end;
{# TJvReplaceUndo }

{* TJvDeleteSelectedUndo }

constructor TJvDeleteSelectedUndo .Create(ARAEditor: TJvCustomEditor; ACaretX,
  ACaretY: integer; AText: string; ASelBlockFormat: TSelBlockFormat;
  ASelBegX, ASelBegY, ASelEndX, ASelEndY: integer);
begin
  inherited Create(ARAEditor, ACaretX, ACaretY, AText);
  FSelBlockFormat := ASelBlockFormat;
  FSelBegX := ASelBegX;
  FSelBegY := ASelBegY;
  FSelEndX := ASelEndX;
  FSelEndY := ASelEndY;
end;

procedure TJvDeleteSelectedUndo .Undo;
var
  S: string;
  iBeg: integer;
  i: integer;
begin
    if FSelBlockFormat in [bfInclusive, bfNonInclusive] then
    begin
      S := FRAEditor.FLines.Text;
      iBeg := FRAEditor.PosFromCaret(FSelBegX, FSelBegY);
      Insert(FText, S, iBeg + 1);
      FRAEditor.FLines.SetLockText(S);
    end
    else if FSelBlockFormat = bfColumn then
    begin
      for i := FSelBegY to FSelEndY do
      begin
        S := FRAEditor.FLines[i];
        Insert(SubStr(FText, i - FSelBegY, #13#10), S, FSelBegX + 1);
        FRAEditor.FLines[i] := S;
      end;
    end;
    FRAEditor.FSelBegX := FSelBegX;
    FRAEditor.FSelBegY := FSelBegY;
    FRAEditor.FSelEndX := FSelEndX;
    FRAEditor.FSelEndY := FSelEndY;
    FRAEditor.FSelBlockFormat := FSelBlockFormat;
    FRAEditor.FSelected := Length(FText) > 0;
    FRAEditor.SetCaretInternal(FCaretX, FCaretY);
end;
{# TJvDeleteSelectedUndo }

{* TJvSelectUndo }

constructor TJvSelectUndo .Create(ARAEditor: TJvCustomEditor; ACaretX,
  ACaretY: integer; ASelected: boolean; ASelBlockFormat: TSelBlockFormat;
  ASelBegX, ASelBegY, ASelEndX, ASelEndY: integer);
begin
  inherited Create(ARAEditor, ACaretX, ACaretY);
  FSelected := ASelected;
  FSelBlockFormat := ASelBlockFormat;
  FSelBegX := ASelBegX;
  FSelBegY := ASelBegY;
  FSelEndX := ASelEndX;
  FSelEndY := ASelEndY;
end;

procedure TJvSelectUndo .Undo;
begin
  FRAEditor.FSelected := FSelected;
  FRAEditor.FSelBlockFormat := FSelBlockFormat;
  FRAEditor.FSelBegX := FSelBegX;
  FRAEditor.FSelBegY := FSelBegY;
  FRAEditor.FSelEndX := FSelEndX;
  FRAEditor.FSelEndY := FSelEndY;
  FRAEditor.SetCaretInternal(FCaretX, FCaretY);
end;
{# TJvSelectUndo }

procedure TJvBeginCompoundUndo .Undo;
begin
  { nothing }
end;

{$ENDIF RAEDITOR_UNDO}

{#####################  Undo  #####################}


{*********************  Code Completion  *********************}
{$IFDEF RAEDITOR_COMPLETION}

procedure TJvCustomEditor.CompletionIdentifer(var Cancel: boolean);
begin
  {abstract}
end;

procedure TJvCustomEditor.CompletionTemplate(var Cancel: boolean);
begin
  {abstract}
end;

procedure TJvCustomEditor.DoCompletionIdentifer(var Cancel: boolean);
begin
  CompletionIdentifer(Cancel);
  if Assigned(FOnCompletionIdentifer) then FOnCompletionIdentifer(Self, Cancel);
end;

procedure TJvCustomEditor.DoCompletionTemplate(var Cancel: boolean);
begin
  CompletionTemplate(Cancel);
  if Assigned(FOnCompletionTemplate) then FOnCompletionTemplate(Self, Cancel);
end;

type
  TJvEditorCompletionList = class(TListBox)
  private
    FTimer: TTimer;
    YY: integer;
    // HintWindow : THintWindow;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure WMCancelMode(var Message: TMessage); message WM_CancelMode;
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

constructor TJvCompletion .Create2(ARAEditor: TJvCustomEditor);
begin
  inherited Create;
  FRAEditor := ARAEditor;
  FPopupList := TJvEditorCompletionList.Create(FRAEditor);
  FItemHeight := FPopupList.ItemHeight;
  FDropDownCount := 6;
  FDropDownWidth := 300;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.Interval := 800;
  FTimer.OnTimer := OnTimer;
  FIdentifers := TStringList.Create;
  FTemplates := TStringList.Create;
  FItems := TStringList.Create;
  FDefMode := cmIdentifers;
  FCaretChar := '|';
  FCRLF := '/n';
  FSeparator := '=';
end;

destructor TJvCompletion .Destroy;
begin
  inherited Destroy;
  FPopupList.Free;
  FIdentifers.Free;
  FTemplates.Free;
  FItems.Free;
  FTimer.Free;
end;

function TJvCompletion .GetItems: TStrings;
begin
  case FMode of
    cmIdentifers: Result := FIdentifers;
    cmTemplates: Result := FTemplates;
  else
    Result := nil;
  end;
end;

{заменяет слово в позиции курсора на NewString}

procedure TJvCompletion .ReplaceWord(const NewString: string);
var
  S, S1, W: string;
  P, X, Y: integer;
  iBeg, iEnd: integer;
  NewCaret, LNum, CX, CY, i: integer;
begin
  with FRAEditor do
  begin
    PaintCaret(false);
    BeginUpdate;
    ReLine;
    S := FLines.Text;
    P := PosFromCaret(FCaretX, FCaretY);
    if (P = 0) or (S[P] in Separators) then
      W := ''
    else
      W := Trim(GetWordOnPosEx(S, P, iBeg, iEnd));
    LNum := 0;
    CaretFromPos(iBeg, CX, CY);
    if W = '' then
    begin
      iBeg := P + 1;
      iEnd := P
    end;
    case FMode of
      cmIdentifers:
        begin
          S1 := NewString;
          if Assigned(FOnCompletionApply) then
            FOnCompletionApply(Self, W, S1);
          NewCaret := Length(S1);
        end;
      cmTemplates:
        begin
          S1 := ReplaceString(NewString, FCRLF, #13#10 + Spaces(FCaretX -
            Length(W)));
          S1 := ReplaceString(S1, FCaretChar, '');
          NewCaret := Pos(FCaretChar, NewString) - 1;
          if NewCaret = -1 then NewCaret := Length(S1);
          for i := 1 to NewCaret do
            if S1[i] = #13 then inc(LNum);
        end
    else
      raise EJvEditorError .Create('Invalid JvEditor Completion Mode');
    end;
    {$IFDEF RAEDITOR_UNDO}
    TJvReplaceUndo .Create(FRAEditor, FCaretX, FCaretY, iBeg, iEnd, W, S1);
    {$ENDIF RAEDITOR_UNDO}
    //  LW := Length(W);
    if FSelected then
    begin
      if (FSelBegY <= FCaretY) or (FCaretY >= FSelEndY) then
        // скорректировать LW ..
    end;
    Delete(S, iBeg, iEnd - iBeg);
    Insert(S1, S, iBeg);
    FLines.SetLockText(S);
    CaretFromPos(iBeg - 1 + (CX - 1) * LNum + NewCaret, X, Y);
    SetCaretInternal(X, Y);
    FRAEditor.TextAllChanged; // Invalidate; {!!!}
    Changed;
    EndUpdate;
    PaintCaret(true);
  end;
end;

procedure TJvCompletion .DoKeyPress(Key: Char);
begin
  if FVisible then
    if HasChar(Key, RAEditorCompletionChars) then
      SelectItem
    else
      CloseUp(true)
  else if FEnabled then
    FTimer.Enabled := true;
end;

function TJvCompletion .DoKeyDown(Key: Word; Shift: TShiftState): boolean;
begin
  Result := true;
  case Key of
    VK_ESCAPE: CloseUp(false);
    VK_RETURN: CloseUp(true);
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT:
      FPopupList.Perform(WM_KEYDOWN, Key, 0);
  else
    Result := false;
  end;
end;

procedure TJvCompletion .DoCompletion(const AMode: TCompletionList);
var
  Eq: boolean;
  Cancel: boolean;
begin
  if FRAEditor.FReadOnly then exit;
  if FPopupList.Visible then CloseUp(False);
  FMode := AMode;
  case FMode of
    cmIdentifers:
      DropDown(AMode, true);
    cmTemplates:
      begin
        Cancel := false;
        // FRAEditor.DoCompletionIdentifer(Cancel);
        FRAEditor.DoCompletionTemplate(Cancel);
        if Cancel or (FTemplates.Count = 0) then exit;
        MakeItems;
        FindSelItem(Eq);
        if Eq then
          ReplaceWord(SubStr(FItems[ItemIndex], 2, FSeparator))
        else
          DropDown(AMode, true);
      end;
  end;
end;

procedure TJvCompletion .DropDown(const AMode: TCompletionList; const ShowAlways:
  boolean);
var
  ItemCount: Integer;
  P: TPoint;
  Y: Integer;
  PopupWidth, PopupHeight: Integer;
  SysBorderWidth, SysBorderHeight: Integer;
  R: TRect;
  Cancel: boolean;
  Eq: boolean;
begin
  CloseUp(false);
  FMode := AMode;
  with FRAEditor do
  begin
    Cancel := false;
    case FMode of
      cmIdentifers: FRAEditor.DoCompletionIdentifer(Cancel);
      cmTemplates:
        FRAEditor.DoCompletionTemplate(Cancel)
    end;
    MakeItems;
    FindSelItem(Eq);
    // Cancel := not Visible and (ItemIndex = -1);
    if Cancel or (FItems.Count = 0) or (((ItemIndex = -1) or Eq) and not
      ShowAlways) then exit;
    FPopupList.Items := FItems;
    FPopupList.ItemHeight := FItemHeight;
    FVisible := true;
    SetItemIndex(FItemIndex);
    if FListBoxStyle in [lbStandard] then
      FPopupList.Style := lbOwnerDrawFixed
    else
      FPopupList.Style := FListBoxStyle;
    FPopupList.OnMeasureItem := FRAEditor.FOnCompletionMeasureItem;
    FPopupList.OnDrawItem := FRAEditor.FOnCompletionDrawItem;

    ItemCount := FItems.Count;
    SysBorderWidth := GetSystemMetrics(SM_CXBORDER);
    SysBorderHeight := GetSystemMetrics(SM_CYBORDER);
    R := CalcCellRect(FCaretX - FLeftCol, FCaretY - FTopRow + 1);
    P := R.TopLeft;
    P.X := ClientOrigin.X + P.X;
    P.Y := ClientOrigin.Y + P.Y;
    Dec(P.X, 2 * SysBorderWidth);
    Dec(P.Y, SysBorderHeight);
    if ItemCount > FDropDownCount then ItemCount := FDropDownCount;
    PopupHeight := ItemHeight * ItemCount + 2;
    Y := P.Y;
    if (Y + PopupHeight) > Screen.Height then
    begin
      Y := P.Y - PopupHeight - FCellRect.Height + 1;
      if Y < 0 then Y := P.Y;
    end;
    PopupWidth := FDropDownWidth;
    if PopupWidth = 0 then PopupWidth := Width + 2 * SysBorderWidth;
  end;
  FPopupList.Left := P.X;
  FPopupList.Top := Y;
  FPopupList.Width := PopupWidth;
  FPopupList.Height := PopupHeight;
  SetWindowPos(FPopupList.Handle, HWND_TOP, P.X, Y, 0, 0,
    SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  FPopupList.Visible := true;
end;

procedure TJvCompletion .MakeItems;
var
  i: integer;
  S: string;
begin
  FItems.Clear;
  case FMode of
    cmIdentifers:
      for i := 0 to FIdentifers.Count - 1 do
        FItems.Add(FIdentifers[i]);
    cmTemplates:
      begin
        with FRAEditor do
          if FLines.Count > CaretY then
            S := GetWordOnPos(FLines[CaretY], CaretX)
          else
            S := '';
        for i := 0 to FTemplates.Count - 1 do
          if ANSIStrLIComp(PChar(FTemplates[i]), PChar(S), Length(S)) = 0 then
            FItems.Add(FTemplates[i]);
        if FItems.Count = 0 then FItems.Assign(FTemplates);
      end;
  end;
end;

procedure TJvCompletion .FindSelItem(var Eq: boolean);

  function FindFirst(Ss: TSTrings; S: string): integer;
  var
    i: integer;
  begin
    for i := 0 to Ss.Count - 1 do
      if ANSIStrLIComp(PChar(Ss[i]), PChar(S), Length(S)) = 0 then
      begin
        Result := i;
        exit;
      end;
    Result := -1;
  end;

var
  S: string;
begin
  with FRAEditor do
    if FLines.Count > 0 then
      S := GetWordOnPos(FLines[CaretY], CaretX) else
      S := '';
  if Trim(S) = '' then
    ItemIndex := -1
  else
    ItemIndex := FindFirst(FItems, S);
  Eq := (ItemIndex > -1) and Cmp(Trim(SubStr(FItems[ItemIndex], 0, FSeparator)),
    S);
end;

procedure TJvCompletion .SelectItem;
var
  Cancel: boolean;
  Param: boolean;
begin
  FindSelItem(Param);
  Cancel := not Visible and (ItemIndex = -1);
  case FMode of
    cmIdentifers: FRAEditor.DoCompletionIdentifer(Cancel);
    cmTemplates: FRAEditor.DoCompletionTemplate(Cancel);
  end;
  if Cancel or (GetItems.Count = 0) then CloseUp(false);
end;

procedure TJvCompletion .CloseUp(const Apply: boolean);
begin
  FItemIndex := ItemIndex;
  FPopupList.Visible := false;
  //  (FPopupList as TJvEditorCompletionList). HintWindow.ReleaseHandle;
  FVisible := false;
  FTimer.Enabled := false;
  if Apply and (ItemIndex > -1) then
    case FMode of
      cmIdentifers: ReplaceWord(SubStr(FItems[ItemIndex], 0, FSeparator));
      cmTemplates: ReplaceWord(SubStr(FItems[ItemIndex], 2, FSeparator));
    end;
end;

procedure TJvCompletion .OnTimer(Sender: TObject);
begin
  DropDown(FDefMode, false);
end;

procedure TJvCompletion .SetStrings(index: integer; AValue: TStrings);
begin
  case index of
    0: FIdentifers.Assign(AValue);
    1: FTemplates.Assign(AValue);
  end;
end;

function TJvCompletion .GetItemIndex: integer;
begin
  Result := FItemIndex;
  if FVisible then
    Result := FPopupList.ItemIndex;
end;

procedure TJvCompletion .SetItemIndex(AValue: integer);
begin
  FItemIndex := AValue;
  if FVisible then
    FPopupList.ItemIndex := FItemIndex;
end;

function TJvCompletion .GetInterval: cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TJvCompletion .SetInterval(AValue: cardinal);
begin
  FTimer.Interval := AValue;
end;



{**************  TJvEditorCompletionList  *************}

constructor TJvEditorCompletionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Left := -1000;
  Visible := false;
  TabStop := False;
  ParentFont := false;
  Parent := Owner as TJvCustomEditor;
  Ctl3D := false;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.Interval := 200;
  FTimer.OnTimer := OnTimer;
  Style := lbOwnerDrawFixed;
  ItemHeight := 13;
  //  HintWindow := THintWindow.Create(Self);
end;

destructor TJvEditorCompletionList.Destroy;
begin
  FTimer.Free;
  //  HintWindow.Free;
  inherited Destroy;
end;

procedure TJvEditorCompletionList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style {or WS_POPUP} or WS_BORDER;
    ExStyle := ExStyle or WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

procedure TJvEditorCompletionList.CreateWnd;
begin
  inherited CreateWnd;
  if not (csDesigning in ComponentState) then
    Windows.SetParent(Handle, 0);
  //  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0); {??}
end;

procedure TJvEditorCompletionList.DestroyWnd;
begin
  inherited DestroyWnd;
  //  HintWindow.ReleaseHandle;
end;

procedure TJvEditorCompletionList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  F: integer;
begin
  YY := Y;
  F := ItemAtPos(Point(X, Y), true);
  if KeyPressed(VK_LBUTTON) then
  begin
    F := ItemAtPos(Point(X, Y), true);
    if F > -1 then ItemIndex := F;
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

procedure TJvEditorCompletionList.MouseDown(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
var
  F: integer;
begin
  MouseCapture := true;
  F := ItemAtPos(Point(X, Y), true);
  if F > -1 then ItemIndex := F;
end;

procedure TJvEditorCompletionList.MouseUp(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
  MouseCapture := false;
  (Owner as TJvCustomEditor).FCompletion.CloseUp(
    (Button = mbLeft) and PtInRect(ClientRect, Point(X, Y)));
end;

procedure TJvEditorCompletionList.OnTimer(Sender: TObject);
begin
  if (YY < 0) then
    Perform(WM_VSCROLL, SB_LINEUP, 0)
  else if (YY > ClientHeight) then
    Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TJvEditorCompletionList.WMCancelMode(var Message: TMessage);
begin
  (Owner as TJvCustomEditor).FCompletion.CloseUp(false);
end;

procedure TJvEditorCompletionList.CMHintShow(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TJvEditorCompletionList.DrawItem(Index: Integer; Rect: TRect; State:
  TOwnerDrawState);
var
  Offset, W: Integer;
  S: string;
begin
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else
  begin
    Canvas.FillRect(Rect);
    Offset := 3;
    with (Owner as TJvCustomEditor).FCompletion do
      case FMode of
        cmIdentifers:
          Canvas.TextOut(Rect.Left + Offset, Rect.Top, SubStr(Items[Index], 1,
            Separator));
        cmTemplates:
          begin
            Canvas.TextOut(Rect.Left + Offset, Rect.Top, SubStr(Items[Index], 1,
              Separator));
            Canvas.Font.Style := [fsBold];
            S := SubStr(Items[Index], 0, Separator);
            W := Canvas.TextWidth(S);
            Canvas.TextOut(Rect.Right - 2 * Offset - W, Rect.Top, S);
          end;
      end;
  end;
end;

{$ENDIF RAEDITOR_COMPLETION}

{$ENDIF RAEDITOR_EDITOR}


{ TIEditReader support }

procedure TJvCustomEditor.ValidateEditBuffer;
begin
  if FPEditBuffer = nil then
  begin
    FEditBuffer := Lines.Text;
    FPEditBuffer := PChar(FEditBuffer);
    FEditBufferSize := Length(FEditBuffer);
  end;
end; { ValidateEditBuffer }

function TJvCustomEditor.GetText(Position: Longint; Buffer: PChar;
  Count: Longint): Longint;
begin
  ValidateEditBuffer;
  if Position <= FEditBufferSize then
  begin
    Result := Min(FEditBufferSize - Position, Count);
    Move(FPEditBuffer[Position], Buffer[0], Result);
  end
  else
    Result := 0;
end;

end.

