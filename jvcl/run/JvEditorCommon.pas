{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvEditorCommon.PAS, released on 2004-01-25

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):
Burov Dmitry, translation of russian text.
Andreas Hausladen
Peter Thörnqvist
Remko Bonte

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net
-----------------------------------------------------------------------------}
// $Id$

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
    some improvements and bug fixes by Rafal Smotrzyk - rsmotrzyk att mikroplan dott com dott pl :
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
   another good stuf by Rafal Smotrzyk - rsmotrzyk att mikroplan dott com dott pl :
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
    - fixed bug: GetWordOnCaret returns invalid Word if caret stays on start of Word;
  1.54.1:
    - new: undo now works in overwrite mode;
  1.54.2:
    - fixed bug: double click not selects Word on first line;
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
      and function CanUndo in TJvUndoBuffer (TJvCustomEditor.UndoBuffer);
  2.00:
    - removed dependencies from JvUtils.pas unit;
    - bugfixed: TJvDeleteUndo  and TJvBackspaceUndo  do not work always properly
      (thanks to Pavel Chromy);
    - bugfixed: workaround bug with some fonts in Win9x
      (thanks to Dmitry Rubinstain);
  2.10.2: (changes by Andreas Hausladen)
    - speed optimation (font cache, many Lines.Text references were removed)
    - fixed bug: TJvBackspaceUndo, TJvInsertUndo, TJvDeleteUndo still do not work
      always properly
    - fixed bug: caret movement and selections set Modified to TRUE
    - Undo restores Modified-field
    - added [Ctrl][Backspace] (ecBackspaceWord) and [Shift][Backspace] command
    - added [Shift]+MouseDown selections
    - added [Alt]+MouseDown selections (column)
    - new event TKeyboard.OnCommand2
    - fixed bug: CodeCompletition catches VK_HOME, VK_END
    - fixed bug: on empty editor pressing [Ctrl][End] raises "Index out of
      bounds (-1)"
    - fixed bug: caret moves into gutter on horz. scrolling
    - added OnGutterClick and OnGutterDblClick events
    - renamed all "Identifer" to "Identifier"
  2.10.3 (changes by Andreas Hausladen)
    - added new mouse wheel functionality: [Ctrl]+Wheel and [Shift]+Wheel
    - faster TJvReplaceUndo and ReplaceWord/ReplaceWord2
    - bug fixed: first complete selected line stops drawing selection on cell 80
    - added SelectAll, ClearSelection
    - full support for SelBlockFormat = bfColumn and bfLine
    - improved TJvCompletion.ReplaceWord
    - undo system overworked
    - fixed bug: [Shift][Tab] is the same as [Tab]
    - reduced TextAllChanged() calls
    - added: Un-/IndentColumns, Un-/IndentLines, Un-/IndentSelLines
    - new Undo: TJvUnindentColumnUndo, TJvIndentColumnUndo
    - moved: FSelBegX, FSelBegY, ... FSelected into TJvSelectionRec
    - added GetAutoIndentStop and removed AutoIndent code from GetTabStop
    - fixed bug: CanPaste raises Exception SCannotOpenClipboard (new: catches exception)
    - fixed bug: in readonly mode [Return] does nothing
    - added BlockOverwrite property
    - added PeristentBlocks
  2.10.4 (changed by peter3, Andreas Hausladen)
    - fixed bug where pressing Enter/Return while the completion list is open inserts a line break (andreas)
    - fixed GetNextWordPosEx (andreas)
    - added default popupmenu if none assigned (JvFixedEditPopup)
    - added handling of WM_CLEAR, WM_GETTEXTLENGTH, EM_SETREADONLY, EM_SETSEL, EM_GETSEL and EM_CANUNDO
  3.0 (changes by Andreas Hausladen)
    - speed optimation: GetTextLen is now faster
    - fixed: GetSelStart returned caret position
    - fixed: ecBackspace with BackSpaceUnindents=True may destroy the line
    - fixed a bug in InsertText
    - optimized ExpandTabs

  2004-01-25: file split into JvEditor and JvEditorCommon

  Further history: see CVS
}

unit JvEditorCommon;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, Menus,
  JvFixedEditPopup, JvUnicodeCanvas, JvComponent, JvExControls;

const
  Max_X = 1024; {max symbols per row}
  Max_X_Scroll = Max_X;
  {max symbols per row for scrollbar}
  GutterRightMargin = 2;

  {$IFDEF VCL}
  WM_EDITCOMMAND = WM_USER + $101;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  WM_EDITCOMMAND = CM_BASE + $101;
  {$ENDIF VisualCLX}

type
  EJvEditorError = class(Exception);
  TJvCustomEditorBase = class; // base class for both Ansi and Unicode editor
  TJvCompletionBase = class;

  TCellRect = record
    Width: Integer;
    Height: Integer;
  end;

  TLineAttr = packed record
    FC: TColor;
    BC: TColor;
    Style: TFontStyles;
    Border: TColor;
  end;

  TLineAttrs = array [0..Max_X] of TLineAttr;

  TModifiedAction =
    (maAll, maInsert, maDelete, maInsertColumn, maDeleteColumn, maReplace);

  TBookmark = record
    X: Integer;
    Y: Integer;
    Valid: Boolean;
  end;
  TBookmarkNum = 0..9;
  TBookmarks = array [TBookmarkNum] of TBookmark;

  { Borland Block Type:
    00 - inclusive;
    01 - line;
    02 - column;
    03 - noninclusive; }
  TJvSelBlockFormat = (bfInclusive, bfLine, bfColumn, bfNonInclusive);

  TOnPaintGutter = procedure(Sender: TObject; Canvas: TCanvas) of object;
  TOnGutterClick = procedure(Sender: TObject; Line: Integer) of object;

  TEditCommand = Word;
  TMacro = AnsiString; { used as buffer (array of char) }

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

  TJvKeyboard = class(TPersistent)
  private
    FList: TObjectList;
    FOnCommand2: TCommand2Event;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Add(const ACommand: TEditCommand; const AKey1: Word;
      const AShift1: TShiftState);
    procedure Add2(const ACommand: TEditCommand; const AKey1: Word;
      const AShift1: TShiftState; const AKey2: Word;
      const AShift2: TShiftState);
    procedure Add2Ctrl(const ACommand: TEditCommand; const AKey1: Word;
      const AShift1: TShiftState; const AKey2: Word);
    procedure Remove(const AKey1: Word; const AShift1: TShiftState);
    procedure Remove2(const AKey1: Word; const AShift1: TShiftState;
      const AKey2: Word; const AShift2: TShiftState);
    procedure RemoveCtrl(const ACommand: TEditCommand);
    procedure Clear;
    function Command(const AKey: Word; const AShift: TShiftState): TEditCommand;
    function Command2(const AKey1: Word; const AShift1: TShiftState;
      const AKey2: Word; const AShift2: TShiftState): TEditCommand;
    procedure SetDefLayout;

    property OnCommand2: TCommand2Event read FOnCommand2 write FOnCommand2;
  end;

  { TJvSelectionRec contains all text selection information } 
  PJvSelectionRec = ^TJvSelectionRec;
  TJvSelectionRec = record
    IsSelected: Boolean; // maybe a function that checks BegX/Y EndX/Y would be better
    Selecting: Boolean;
    SelBlockFormat: TJvSelBlockFormat;
    SelBegX: Integer;
    SelBegY: Integer;
    SelEndX: Integer;
    SelEndY: Integer;
    SelStartX: Integer;
    SelStartY: Integer;
    SelLineOrgBegX, SelLineOrgEndX: Integer;
  end;

  TJvLineSelectStyle =
    (lssUnselected, lssBreakpoint, lssDebugPoint, lssErrorPoint);

  TAdjustPersistentBlockMode =
    (amInsert, amDelete, amDeleteLine, amLineConcat, amLineBreak);

  TCompletionList = (cmIdentifiers, cmTemplates);
  TOnCompletion = procedure(Sender: TObject; var Cancel: Boolean) of object;


  TJvUndo = class;

  IJvUndoCompound = interface
    ['{D326A114-0A57-4654-A7F0-16D3BBD0A2CE}']
  end;
  IJvBackspaceUndo = interface
    ['{88BE2C69-2C5C-48C0-AC46-888146DD70AD}']
  end;
  IJvBackspaceUnindentUndo = interface
    ['{A78B524C-684E-43BD-B8A4-A540CD0B022D}']
  end;

  TJvUndoBuffer = class(TList)
  protected
    FJvEditor: TJvCustomEditorBase;
    FPtr: Integer;
    InUndo: Boolean;
    function LastUndo: TJvUndo;
    function IsNewGroup(AUndo: TJvUndo): Boolean;
    function CanRedo: Boolean;
    procedure ClearRedo;
    function IsCaretGroup: Boolean;
  public
    procedure Add(AUndo: TJvUndo);
    procedure Undo;
    procedure Redo;
    procedure Clear; override;
    procedure Delete;
    function CanUndo: Boolean;
  end;

  TJvUndo = class(TInterfacedObject)
  protected
    FJvEditor: TJvCustomEditorBase;
    FModified: Boolean; // Editor.FModified
    FSelection: PJvSelectionRec;
    function UndoBuffer: TJvUndoBuffer;
  protected
    property JvEditor: TJvCustomEditorBase read FJvEditor;
  public
    constructor Create(AJvEditor: TJvCustomEditorBase);
    destructor Destroy; override;
    procedure Undo; dynamic; abstract;
    procedure Redo; dynamic; {abstract;}
    procedure SaveSelection;
    procedure RestoreSelection;
  end;

  TJvCaretUndo = class(TJvUndo)
  protected
    FCaretX: Integer;
    FCaretY: Integer;
    property CaretX: Integer read FCaretX write FCaretX;
    property CaretY: Integer read FCaretY write FCaretY;
  public
    constructor Create(AJvEditor: TJvCustomEditorBase; ACaretX, ACaretY: Integer);
    procedure Undo; override;
  end;

  TJvSelectUndo = class(TJvCaretUndo)
  public
    constructor Create(AJvEditor: TJvCustomEditorBase; ACaretX, ACaretY: Integer);
    procedure Undo; override;
  end;

  TJvUnselectUndo = class(TJvSelectUndo);

  TJvBeginCompoundUndo = class(TJvUndo)
  public
    procedure Undo; override;
  end;

  TJvEndCompoundUndo = class(TJvBeginCompoundUndo);

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

  TJvEditorClient = class(TObject)
  public
    FJvEditor: TJvCustomEditorBase;
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
    FJvEditor: TJvCustomEditorBase;
  public
    procedure Paint;
    procedure Invalidate;
  end;

  TJvLineInformation = class(TObject)
  private
    FLine: Integer;
    FSelectStyle: TJvLineSelectStyle;
    FData: Pointer;
    FEditor: TJvCustomEditorBase;
    procedure SetLine(Value: Integer);
    procedure SetSelectStyle(const Value: TJvLineSelectStyle);
  protected
    procedure RepaintLine(LineNum: Integer); virtual;
    procedure CheckEmpty; virtual; // releases the object if Data=nil and SelectStyle=lssUnselected
  public
    constructor Create(AEditor: TJvCustomEditorBase; ALine: Integer);
    destructor Destroy; override;

    property Line: Integer read FLine write SetLine;
    property SelectStyle: TJvLineSelectStyle read FSelectStyle write SetSelectStyle;
    property Data: Pointer read FData write FData;
    property Editor: TJvCustomEditorBase read FEditor;
  end;

  TJvLineInformationList = class(TObject)
  private
    FEditor: TJvCustomEditorBase;
    FList: TObjectList;
    FDebugColor: TColor;
    FDebugTextColor: TColor;
    FBreakpointColor: TColor;
    FBreakpointTextColor: TColor;
    FErrorPointTextColor: TColor;
    FErrorPointColor: TColor;
    function GetCount: Integer;
    function GetData(Index: Integer): Pointer;
    function GetItems(Index: Integer): TJvLineInformation;
    function GetLineCount: Integer;
    function GetLines(Index: Integer): TJvLineInformation;
    function GetSelectStyle(Index: Integer): TJvLineSelectStyle;
    procedure SetData(Index: Integer; Value: Pointer);
    procedure SetSelectStyle(Index: Integer; const Value: TJvLineSelectStyle);
    procedure SetBreakpointColor(const Value: TColor);
    procedure SetBreakpointTextColor(const Value: TColor);
    procedure SetDebugColor(const Value: TColor);
    procedure SetDebugTextColor(const Value: TColor);
    procedure SetErrorPointColor(const Value: TColor);
    procedure SetErrorPointTextColor(const Value: TColor);
  protected
    function CreateLineInfo(Index: Integer): TJvLineInformation;
      // Returns the line information assoziated with the line or creates a new.
      // If Index not in [0..Count-1] the function raises EListError
  public
    constructor Create(AEditor: TJvCustomEditorBase);
    destructor Destroy; override;

    procedure Clear;
      // Clear() removes all extra line information objects

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJvLineInformation read GetItems;

    property LineCount: Integer read GetLineCount;
      // LineCount returns Editor.Lines.Count
    property Lines[Index: Integer]: TJvLineInformation read GetLines; default;
      // Lines[] returns nil if the line has no extra information
    property SelectStyle[Index: Integer]: TJvLineSelectStyle read GetSelectStyle write SetSelectStyle;
      // SelectStyle[] returns/sets the select style for the line
    property Data[Index: Integer]: Pointer read GetData write SetData;
      // Data[] returns/sets the user defined data for the line

    property DebugPointColor: TColor read FDebugColor write SetDebugColor;
    property DebugPointTextColor: TColor read FDebugTextColor write SetDebugTextColor;
    property BreakpointColor: TColor read FBreakpointColor write SetBreakpointColor;
    property BreakpointTextColor: TColor read FBreakpointTextColor write SetBreakpointTextColor;
    property ErrorPointColor: TColor read FErrorPointColor write SetErrorPointColor;
    property ErrorPointTextColor: TColor read FErrorPointTextColor write SetErrorPointTextColor;

    property Editor: TJvCustomEditorBase read FEditor;
  end;

  TJvBracketHighlighting = class(TPersistent)
  private
    FStart: TRect;
    FStop: TRect;
    
    FActive: Boolean;
    FFontColor: TColor;
    FBorderColor: TColor;
    FColor: TColor;
    FWordPairs: TStrings;
    FCaseSensitiveWordPairs: Boolean;
    procedure SetWordPairs(Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Active: Boolean read FActive write FActive default False;
    property BorderColor: TColor read FBorderColor write FBorderColor default clSilver;
    property Color: TColor read FColor write FColor default clNone;
    property FontColor: TColor read FFontColor write FFontColor default clNone;

    property CaseSensitiveWordPairs: Boolean read FCaseSensitiveWordPairs write FCaseSensitiveWordPairs default True;
    property WordPairs: TStrings read FWordPairs write SetWordPairs;
      { example: "begin=end", "repeat=until", "for=do", "asm=end" }
  end;

  TJvCustomEditorBase = class(TJvCustomControl, IFixedPopupIntf)
  private
    { internal objects }
    FScrollBarHorz: TJvControlScrollBar95;
    FScrollBarVert: TJvControlScrollBar95;
    FEditorClient: TJvEditorClient;
    FCompletion: TJvCompletionBase; // must be initialized by a decendent

    FGutter: TJvGutter;
    FKeyboard: TJvKeyboard;
    FUpdateLock: Integer;
    FUndoBuffer: TJvUndoBuffer;
    FGroupUndo: Boolean;
    FUndoAfterSave: Boolean;
    FBracketHighlighting: TJvBracketHighlighting;

    { internal - Columns and rows attributes }
    FCols: Integer;
    FRows: Integer;
    FLeftCol: Integer;
    FTopRow: Integer;
    // FLeftColMax, FTopRowMax : Integer;
    FLastVisibleCol: Integer;
    FLastVisibleRow: Integer;
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
    FUpdateSelBegY: Integer;
    FUpdateSelEndY: Integer;
    FPersistentBlocksCaretChanged: Boolean;
    FSelBackColor: TColor;
    FSelForeColor: TColor;
    FLineInformations: TJvLineInformationList;

    { mouse support }
    TimerScroll: TTimer;
    MouseMoveY: Integer;
    MouseMoveXX: Integer;
    MouseMoveYY: Integer;
    FDoubleClick: Boolean;
    FMouseDown: Boolean;

    { internal }
    FTabStops: AnsiString; // Tabs string is always an AnsiString

    FCompound: Integer;

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
    FUseFixedPopup: Boolean;

    { events }
    FOnChange: TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnChangeStatus: TNotifyEvent;
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
    
    {$IFDEF VCL}
    { internal message processing }
    procedure WMEditCommand(var Msg: TMessage); message WM_EDITCOMMAND;

    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
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
    {$ENDIF VCL}
  protected
    FMyDi: array [0..1024] of Integer;
    FSelection: TJvSelectionRec;
    FCaretX: Integer;
    FCaretY: Integer;
    FTabPos: array [0..Max_X] of Boolean;
    { FMacro - buffer of TEditCommand, each command represents by two chars }
    FMacro: TMacro;
    FDefMacro: TMacro;

    procedure UpdateEditorSize; virtual;
    procedure UpdateEditorView; virtual;
    procedure ScrollTimer(Sender: TObject);

    function ExpandTabsAnsi(const S: AnsiString): AnsiString; // ClipboardPaste
    function GetDefTabStop(X: Integer; Next: Boolean): Integer; virtual;
    function GetTabStop(X, Y: Integer; Next: Boolean): Integer; virtual; abstract;
    function GetBackStop(X, Y: Integer): Integer; virtual; abstract;
    function GetAutoIndentStop(Y: Integer): Integer; virtual; abstract;
    function GetAnsiTextLine(Y: Integer; out Text: AnsiString): Boolean; virtual; abstract;
    function GetAnsiWordOnCaret: AnsiString; virtual; abstract;

    procedure ReLine; virtual; abstract;
    procedure TextAllChangedInternal(Unselect: Boolean); virtual;

    { triggers for descendants }
    procedure Changed; dynamic;
    procedure TextAllChanged; dynamic;
    procedure StatusChanged; dynamic;
    procedure SelectionChanged; dynamic;
    procedure GetAttr(Line, ColBeg, ColEnd: Integer); virtual;
    procedure ChangeAttr(Line, ColBeg, ColEnd: Integer); virtual;
    procedure GutterPaint(Canvas: TCanvas); dynamic;
    procedure GutterClick(Line: Integer); dynamic;
    procedure GutterDblClick(Line: Integer); dynamic;
    procedure BookmarkChanged(Bookmark: Integer); dynamic;
    procedure CompletionIdentifier(var Cancel: Boolean); dynamic;
    procedure CompletionTemplate(var Cancel: Boolean); dynamic;
    procedure DoCompletionIdentifier(var Cancel: Boolean);
    procedure DoCompletionTemplate(var Cancel: Boolean);
  protected
    procedure Resize; override;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    {$IFDEF VisualCLX}
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
    {$ENDIF VisualCLX}
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var
      ScrollPos: Integer);
    procedure Scroll(Vert: Boolean; ScrollPos: Integer); dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      {$IFDEF VisualCLX} const {$ENDIF} MousePos: TPoint): Boolean; override;
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

    { IFixedPopupIntf method assignment }
    procedure IFixedPopupIntf.Cut = ClipboardCut;
    procedure IFixedPopupIntf.Copy = ClipboardCopy;
    procedure IFixedPopupIntf.Paste = ClipboardPaste;
    procedure IFixedPopupIntf.Delete = DeleteSelected;
  protected
    { get/set methods for properties }
    procedure SetGutterWidth(AWidth: Integer);
    procedure SetGutterColor(AColor: TColor);
    procedure SetBorderStyle(Value: TBorderStyle);
    function GetSelStart: Integer;
    procedure SetSelStart(ASelStart: Integer);
    procedure SetSelLength(ASelLength: Integer);
    function GetSelLength: Integer;
    procedure SetSelBlockFormat(Value: TJvSelBlockFormat);
    function GetSelBlockFormat: TJvSelBlockFormat;
    procedure SetMode(Index: Integer; Value: Boolean);
    procedure SetCaretPosition(Index, Pos: Integer);
    procedure SetCols(ACols: Integer);
    procedure SetRows(ARows: Integer);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetRightMarginVisible(Value: Boolean);
    procedure SetRightMargin(Value: Integer);
    procedure SetRightMarginColor(Value: TColor);
    procedure SetSelBackColor(const Value: TColor);
    procedure SetSelForeColor(const Value: TColor);
    procedure SetBracketHighlighting(Value: TJvBracketHighlighting);
    function GetPopupMenu: TPopupMenu; override;

    function GetLineCount: Integer; virtual; abstract;
    function GetLineLength(Index: Integer): Integer; virtual; abstract;
    function FindNotBlankCharPosInLine(Line: Integer): Integer; virtual; abstract;

    procedure LockUpdate;
    procedure UnlockUpdate;
    property UpdateLock: Integer read FUpdateLock;
    property Compound: Integer read FCompound;
    property EditorClient: TJvEditorClient read FEditorClient;
    property Gutter: TJvGutter read FGutter;
  protected
    function GetClipboardBlockFormat: TJvSelBlockFormat;
    procedure SetClipboardBlockFormat(const Value: TJvSelBlockFormat);
    procedure SetSel(SelX, SelY: Integer);
    function IsNewSelection: Boolean;
    function IsEmptySelection: Boolean;
    procedure PaintSelection;
    procedure SetUnSelected;
    procedure RemoveSelectedBlock;
    procedure PersistentBlocksSetUnSelected;
    procedure SetSelUpdateRegion(BegY, EndY: Integer);
    procedure AdjustSelLineMode(Restore: Boolean);
    procedure AdjustPersistentBlockSelection(X, Y: Integer;
      Mode: TAdjustPersistentBlockMode; Args: array of Integer);
  protected
    LineAttrs: TLineAttrs;

    procedure Paint; override;
    procedure PaintLine(Line: Integer; ColBeg, ColEnd: Integer); overload;
    procedure PaintLineText(Line: Integer; ColBeg, ColEnd: Integer;
      var ColPainted: Integer); virtual; abstract;
    procedure GetBracketHighlightAttr(Line: Integer; var Attrs: TLineAttrs); virtual;
    procedure HighlightBrackets; virtual;
    procedure GetBracketHighlightingWords(var Direction: Integer;
      var Start, Stop: AnsiString; var CaseSensitive: Boolean); virtual;
    function FontCacheFind(LA: TLineAttr): TFont;
    procedure FontCacheClear;
    procedure InsertChar(const Key: Word); virtual; abstract;

    procedure DrawRightMargin;
    procedure Mouse2Cell(X, Y: Integer; var CX, CY: Integer);
    procedure Mouse2Caret(X, Y: Integer; var CX, CY: Integer);
    procedure CaretCoord(X, Y: Integer; var CX, CY: Integer);
    function PosFromMouse(X, Y: Integer): Integer;
    procedure SetCaretInternal(X, Y: Integer);

    procedure NotUndoable;
    procedure NotRedoable;
    procedure ChangeBookmark(Bookmark: TBookmarkNum; Valid: Boolean);
    procedure BeginRecord;
    procedure EndRecord(var AMacro: TMacro);
    procedure PlayMacro(const AMacro: TMacro);

    function DoCommand(ACommand: TEditCommand; var X, Y: Integer;
      var CaretUndo: Boolean): Boolean; virtual; abstract;

    property LineCount: Integer read GetLineCount;
    property LineLength[Index: Integer]: Integer read GetLineLength;

    property Completion: TJvCompletionBase read FCompletion write FCompletion;
  public
    Bookmarks: TBookmarks;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetLeftTop(ALeftCol, ATopRow: Integer);
    procedure PaintLine(Line: Integer); overload;

    function CanUndo: Boolean; { IFixedPopupIntf }
    function CanRedo: Boolean;
    function CanCopy: Boolean; { IFixedPopupIntf }
    function CanPaste: Boolean; { IFixedPopupIntf }
    function CanCut: Boolean;  { IFixedPopupIntf }
    function CanSelectAll: Boolean; { IFixedPopupIntf }
    procedure SelectAll; { IFixedPopupIntf }
    function HasSelection: Boolean; { IFixedPopupIntf }

    procedure ClipboardCopy; dynamic; abstract;
    procedure ClipboardPaste; dynamic; abstract;
    procedure ClipboardCut; dynamic;
    procedure DeleteSelected; dynamic; abstract;
    procedure ClearSelection; dynamic;

    procedure Undo;
    procedure Redo; // not implemented yet

    procedure SelectRange(BegX, BegY, EndX, EndY: Integer);
    function CalcCellRect(X, Y: Integer): TRect;
    procedure SetCaret(X, Y: Integer);
    procedure CaretFromPos(Pos: Integer; var X, Y: Integer);
    function PosFromCaret(X, Y: Integer): Integer;
    procedure PaintCaret(bShow: Boolean);
    function GetTextLen: Integer;
    procedure SelectWordOnCaret; virtual; abstract;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure MakeRowVisible(ARow: Integer);

    procedure Command(ACommand: TEditCommand); virtual;
    procedure PostCommand(ACommand: TEditCommand);

    procedure IndentColumns(X: Integer; BegY, EndY: Integer); virtual; abstract;
    procedure UnIndentColumns(X: Integer; BegY, EndY: Integer); virtual; abstract;
    procedure IndentLines(UnIndent: Boolean; BegY, EndY: Integer);
    procedure IndentSelLines(UnIndent: Boolean);

    procedure BeginCompound;
    procedure EndCompound;

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
    property SelBlockFormat: TJvSelBlockFormat read GetSelBlockFormat write SetSelBlockFormat default bfNonInclusive;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property Keyboard: TJvKeyboard read FKeyboard;
    property CellRect: TCellRect read FCellRect;
    property UndoBuffer: TJvUndoBuffer read FUndoBuffer;
    property GroupUndo: Boolean read FGroupUndo write FGroupUndo default True;
    property UndoAfterSave: Boolean read FUndoAfterSave write FUndoAfterSave;
    property Recording: Boolean read FRecording;
    property UseFixedPopup: Boolean read FUseFixedPopup write FUseFixedPopup;

    property LineInformations: TJvLineInformationList read FLineInformations;
  public
    { published in descendants }
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
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
    property TabStops: AnsiString read FTabStops write FTabStops;
    property SmartTab: Boolean read FSmartTab write FSmartTab default True;
    property BackSpaceUnindents: Boolean read FBackSpaceUnindents write FBackSpaceUnindents default True;
    property AutoIndent: Boolean read FAutoIndent write FAutoIndent default True;
    property KeepTrailingBlanks: Boolean read FKeepTrailingBlanks write FKeepTrailingBlanks default False;
    property CursorBeyondEOF: Boolean read FCursorBeyondEOF write FCursorBeyondEOF default False;
    property BlockOverwrite: Boolean read FBlockOverwrite write FBlockOverwrite default True;
    property PersistentBlocks: Boolean read FPersistentBlocks write FPersistentBlocks default False;
    property BracketHighlighting: TJvBracketHighlighting read FBracketHighlighting write SetBracketHighlighting;
    property SelForeColor: TColor read FSelForeColor write SetSelForeColor;
    property SelBackColor: TColor read FSelBackColor write SetSelBackColor;
    property HideCaret: Boolean read FHideCaret write FHideCaret default False;

    property OnChangeStatus: TNotifyEvent read FOnChangeStatus write FOnChangeStatus;
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

    property DockManager;
  end;

  TJvCompletionBase = class(TPersistent)
  private
    FJvEditor: TJvCustomEditorBase;
    FPopupList: TListBox;
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
    procedure OnTimer(Sender: TObject);
    function GetItemIndex: Integer;
    procedure SetItemIndex(AValue: Integer);
    function GetInterval: Cardinal;
    procedure SetInterval(AValue: Cardinal);
    function GetItems: TStrings;
  protected
    function DoKeyDown(Key: Word; Shift: TShiftState): Boolean; virtual;
    procedure DoKeyPress(Key: Char); virtual;

    procedure FindSelItem(var Eq: Boolean); virtual; abstract;
    procedure MakeItems; virtual; abstract;
    procedure ReplaceWordItemIndex(SubStrStart: Integer); virtual; abstract;
    function GetTemplateCount: Integer; virtual; abstract;
    function GetIdentifierCount: Integer; virtual; abstract;
    function GetAnsiSeparator: AnsiString; virtual; abstract;

    function GetItemCount: Integer;
    property JvEditor: TJvCustomEditorBase read FJvEditor;
    property Items: TStrings read GetItems;
  public
    constructor Create(AJvEditor: TJvCustomEditorBase);
    destructor Destroy; override;
    procedure DropDown(const AMode: TCompletionList; const ShowAlways: Boolean);
    procedure DoCompletion(const AMode: TCompletionList);
    procedure CloseUp(const Apply: Boolean);
    procedure SelectItem;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Visible: Boolean read FVisible write FVisible;
    property Mode: TCompletionList read FMode write FMode;
  published
    property DropDownCount: Integer read FDropDownCount write FDropDownCount default 6;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default 300;
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property ItemHeight: Integer read FItemHeight write FItemHeight;
    property Interval: Cardinal read GetInterval write SetInterval;
    property ListBoxStyle: TListBoxStyle read FListBoxStyle write FListBoxStyle;
  end;

//=== Highligther Editor =====================================================

const
  { Max_Line - maximum line numbers, scanned by editor for comments }
  Max_Line = 64 * 1024;

type
  TJvHighlighter = (hlNone, hlPascal, hlCBuilder, hlSql, hlPython, hlJava, hlVB,
    hlHtml, hlPerl, hlIni, hlCocoR, hlPhp, hlNQC, hlCSharp,
    hlSyntaxHighlighter);
  TLongTokenType = 0..255;

const
  lgNone      = TLongTokenType(0);
  lgComment1  = TLongTokenType(1);
  lgComment2  = TLongTokenType(2);
  lgString    = TLongTokenType(4);
  lgTag       = TLongTokenType(5);
  lgPreproc   = TLongTokenType(6);
  lgPreproc1  = lgPreproc;
  lgPreproc2  = TLongTokenType(7);
  lgUndefined = High(TLongTokenType);

type
  TDelphiColor = record
    ForeColor, BackColor: TColor;
    Style: TFontStyles;
  end;

const
  DelphiColor_Comment: TDelphiColor = (ForeColor: clNavy; BackColor: clWindow; Style: [fsItalic]);
  DelphiColor_Preproc: TDelphiColor = (ForeColor: clGreen; BackColor: clWindow; Style: []);
  DelphiColor_Number: TDelphiColor = (ForeColor: clNavy; BackColor: clWindow; Style: []);
  DelphiColor_Strings: TDelphiColor = (ForeColor: clBlue; BackColor: clWindow; Style: []);
  DelphiColor_Symbol: TDelphiColor = (ForeColor: clBlack; BackColor: clWindow; Style: []);
  DelphiColor_Reserved: TDelphiColor = (ForeColor: clBlack; BackColor: clWindow; Style: [fsBold]);
  DelphiColor_Identifier: TDelphiColor = (ForeColor: clBlack; BackColor: clWindow; Style: []);
  DelphiColor_PlainText: TDelphiColor = (ForeColor: clWindowText; BackColor: clWindow; Style: []);

type
  TJvSymbolColor = class(TPersistent)
  private
    FStyle: TFontStyles;
    FForeColor: TColor;
    FBackColor: TColor;
  public
    constructor Create;
    procedure SetColor(const ForeColor, BackColor: TColor; const Style: TFontStyles);
    procedure Assign(Source: TPersistent); override;
  published
    property Style: TFontStyles read FStyle write FStyle default [];
    property ForeColor: TColor read FForeColor write FForeColor {default clWindowText}; // disabled, otherwise the default values are ignored
    property BackColor: TColor read FBackColor write FBackColor {default clWindow};
  end;

  TJvColors = class(TPersistent)
  private
    FComment: TJvSymbolColor;
    FNumber: TJvSymbolColor;
    FString: TJvSymbolColor;
    FSymbol: TJvSymbolColor;
    FReserved: TJvSymbolColor;
    FIdentifier: TJvSymbolColor;
    FPreproc: TJvSymbolColor;
    FFunctionCall: TJvSymbolColor;
    FDeclaration: TJvSymbolColor;
    FStatement: TJvSymbolColor;
    FPlainText: TJvSymbolColor;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Comment: TJvSymbolColor read FComment write FComment;
    property Number: TJvSymbolColor read FNumber write FNumber;
    property Strings: TJvSymbolColor read FString write FString;
    property Symbol: TJvSymbolColor read FSymbol write FSymbol;
    property Reserved: TJvSymbolColor read FReserved write FReserved;
    property Identifier: TJvSymbolColor read FIdentifier write FIdentifier;
    property Preproc: TJvSymbolColor read FPreproc write FPreproc;
    property FunctionCall: TJvSymbolColor read FFunctionCall write FFunctionCall;
    property Declaration: TJvSymbolColor read FDeclaration write FDeclaration;
    property Statement: TJvSymbolColor read FStatement write FStatement;
    property PlainText: TJvSymbolColor read FPlainText write FPlainText;
  end;

  IJvHLEditor = interface
    ['{E165FE73-AE7E-40A8-AC9B-7FD20D55A15E}']
    function GetColors: TJvColors;
    procedure SetColors(const Value: TJvColors);
    function GetSyntaxHighlighting: Boolean;
    procedure SetSyntaxHighlighting(Value: Boolean);
    function GetHighlighter: TJvHighlighter;
    procedure SetHighlighter(const Value: TJvHighlighter);
    property Colors: TJvColors read GetColors write SetColors;
    property SyntaxHighlighting: Boolean read GetSyntaxHighlighting write SetSyntaxHighlighting;
    property Highlighter: TJvHighlighter read GetHighlighter write SetHighlighter;
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

function KeyPressed(VK: Integer): Boolean;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Consts,
  {$IFDEF HAS_UNIT_RTLCONSTS}
  RTLConsts,
  {$ENDIF HAS_UNIT_RTLCONSTS}
  Math, Clipbrd,
  JvJCLUtils, JvThemes, JvConsts, JvResources;

type
  TJvEditorCompletionList = class(TListBox)
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
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

function KeyPressed(VK: Integer): Boolean;
begin
  Result := GetKeyState(VK) and $8000 = $8000;
end;

//=== { TJvUndoBuffer } ======================================================

procedure RedoNotImplemented;
begin
  raise EJvEditorError.CreateRes(@RsERedoNotYetImplemented);
end;

procedure TJvUndoBuffer.Add(AUndo: TJvUndo);
begin
  if InUndo then
    Exit;
  ClearRedo;
  inherited Add(AUndo);
  FPtr := Count - 1;
end;

procedure TJvUndoBuffer.Undo;
var
  UndoClass: TClass;
  Compound: Integer;
  IsOnlyCaret: Boolean;
  Selection: TJvSelectionRec;

   function IsIntf(AInstance: TObject; IID: TGUID): Boolean; overload;
   begin
     Result := (AInstance <> nil) and (AInstance.GetInterfaceEntry(IID) <> nil);
   end;

   function IsIntf(AClass: TClass; IID: TGUID): Boolean; overload;
   begin
     Result := (AClass <> nil) and (AClass.GetInterfaceEntry(IID) <> nil);
   end;

begin
  if InUndo then
    Exit;

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
        {(LastUndo is TJvDeleteTrailUndo) or
        (LastUndo is TJvReLineUndo) or}
        IsIntf(LastUndo, IJvUndoCompound) or
        (Compound > 0)) or
        {((UndoClass = TJvBackspaceUndo) and
        (LastUndo is TJvBackspaceUnindentUndo)) do}
        IsIntf(UndoClass, IJvBackspaceUndo) and
        IsIntf(LastUndo, IJvBackspaceUnindentUndo) do
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
        if LastUndo <> nil then
        begin
          LastUndo.RestoreSelection;
          FJvEditor.Modified := LastUndo.FModified;
        end;
        Dec(FPtr);
        {if (UndoClass = TJvDeleteTrailUndo) or
          (UndoClass = TJvReLineUndo) then}
        if IsIntf(UndoClass, IJvUndoCompound) then
          UndoClass := LastUndo.ClassType;
        if (UndoClass <> TJvCaretUndo) and
          (UndoClass <> TJvSelectUndo) and
          (UndoClass <> TJvUnselectUndo) then
          IsOnlyCaret := False;
        if not FJvEditor.GroupUndo then
          Break;
      end;
      if not FJvEditor.Modified then
        IsOnlyCaret := True;

      // paint selection
      if not CompareMem(@Selection, @FJvEditor.FSelection, SizeOf(TJvSelectionRec)) then
        FJvEditor.PaintSelection;

      FJvEditor.UpdateEditorView;
      if FJvEditor.FUpdateLock = 0 then
        if not IsOnlyCaret then
          FJvEditor.Changed;
    end;
  finally
    InUndo := False;
  end;
end;

procedure TJvUndoBuffer.Redo;
begin
  if CanRedo then
  begin
    Inc(FPtr);
    LastUndo.Redo;
  end;
end;

procedure TJvUndoBuffer.Clear;
begin
  while Count > 0 do
  begin
    TJvUndo(Items[0]).Free;
    inherited Delete(0);
  end;
end;

procedure TJvUndoBuffer.ClearRedo;
begin
  while (Count > 0) and (FPtr < Count - 1) do
  begin
    TJvUndo(Items[FPtr + 1]).Free;
    inherited Delete(FPtr + 1);
  end;
end;

procedure TJvUndoBuffer.Delete;
begin
  if Count > 0 then
  begin
    TJvUndo(Items[Count - 1]).Free;
    inherited Delete(Count - 1);
  end;
end;

function TJvUndoBuffer.LastUndo: TJvUndo;
begin
  if (FPtr >= 0) and (Count > 0) then
    Result := TJvUndo(Items[FPtr])
  else
    Result := nil;
end;

function TJvUndoBuffer.IsNewGroup(AUndo: TJvUndo): Boolean;
begin
  Result := (LastUndo = nil) or (LastUndo.ClassType <> AUndo.ClassType)
end;

function TJvUndoBuffer.IsCaretGroup: Boolean;
begin
  Result := (LastUndo <> nil) and (LastUndo.ClassType = TJvCaretUndo);
end;

function TJvUndoBuffer.CanUndo: Boolean;
begin
  Result := (LastUndo <> nil);
end;

function TJvUndoBuffer.CanRedo: Boolean;
begin
{
  Result := FPtr < Count;
}
  Result := False;
  ClearRedo;
end;

//=== { TJvUndo } ============================================================

constructor TJvUndo.Create(AJvEditor: TJvCustomEditorBase);
begin
  inherited Create;
  FJvEditor := AJvEditor;
  FModified := FJvEditor.FModified;
  UndoBuffer.Add(Self);
  FSelection := nil;
end;

destructor TJvUndo.Destroy;
begin
  if Assigned(FSelection) then
    Dispose(FSelection);
  // (rom) added inherited Destroy
  inherited Destroy;
end;

procedure TJvUndo.Redo;
begin
  RedoNotImplemented;
end;

procedure TJvUndo.RestoreSelection;
begin
  if Assigned(FSelection) then
  begin
    FJvEditor.FSelection := FSelection^;
    FJvEditor.SetSelUpdateRegion(FSelection^.SelBegY, FSelection^.SelEndY);
  end;
end;

procedure TJvUndo.SaveSelection;
begin
  if not Assigned(FSelection) then
    New(FSelection);
  FSelection^ := FJvEditor.FSelection;
end;

function TJvUndo.UndoBuffer: TJvUndoBuffer;
begin
  if FJvEditor <> nil then
    Result := FJvEditor.FUndoBuffer
  else
    Result := nil;
end;

//=== { TJvCaretUndo } =======================================================

constructor TJvCaretUndo.Create(AJvEditor: TJvCustomEditorBase;
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
    while JvEditor.FGroupUndo and (FPtr >= 0) and not IsNewGroup(Self) do
      Dec(FPtr);
    Inc(FPtr);
    with TJvCaretUndo(Items[FPtr]) do
      JvEditor.SetCaretInternal(FCaretX, FCaretY);
  end;
end;

//=== { TJvSelectUndo } ======================================================

constructor TJvSelectUndo.Create(AJvEditor: TJvCustomEditorBase;
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

//=== { TJvBeginCompoundUndo } ===============================================

procedure TJvBeginCompoundUndo.Undo;
begin
  { nothing }
end;

//=== { TJvControlScrollBar95 } ==============================================

const
  SBKIND: array [TScrollBarKind] of Integer = (SB_HORZ, SB_VERT);

constructor TJvControlScrollBar95.Create;
begin
  inherited Create;
  FPage := 1;
  FSmallChange := 1;
  FLargeChange := 1;
end;

procedure TJvControlScrollBar95.SetParams(AMin, AMax, APosition, APage: Integer);
var
  ScrollInfo: TScrollInfo;
begin
  if AMax < AMin then
    raise EInvalidOperation.CreateRes(@SScrollBarRange);
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
    raise EInvalidOperation.CreateRes(@SScrollBarRange);
  if FPosition < FMin then
    FPosition := FMin;
  if FPosition > FMax then
    FPosition := FMax;
  SetParams(FMin, FMax, FPosition, FPage);
end;

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

//=== { TJvEditKey } =========================================================

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

//=== { TJvKeyboard } ========================================================

constructor TJvKeyboard.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
end;

destructor TJvKeyboard.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TJvKeyboard.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvKeyboard then
  begin
    Clear;
    for I := 0 to TJvKeyboard(Source).FList.Count - 1 do
      with TJvEditKey(TJvKeyboard(Source).FList[I]) do
        Add2(Command, Key1, Shift1, Key2, Shift2);
  end
  else
    inherited Assign(Source);
end;

procedure TJvKeyboard.Add(const ACommand: TEditCommand; const AKey1: Word;
  const AShift1: TShiftState);
begin
  FList.Add(TJvEditKey.Create(ACommand, AKey1, AShift1));
end;

procedure TJvKeyboard.Add2(const ACommand: TEditCommand; const AKey1: Word;
  const AShift1: TShiftState; const AKey2: Word; const AShift2: TShiftState);
begin
  FList.Add(TJvEditKey.Create2(ACommand, AKey1, AShift1, AKey2, AShift2));
end;

procedure TJvKeyboard.Add2Ctrl(const ACommand: TEditCommand;
  const AKey1: Word; const AShift1: TShiftState; const AKey2: Word);
begin
  Add2(ACommand, AKey1, AShift1, AKey2, [ssCtrl]);
  Add2(ACommand, AKey1, AShift1, AKey2, []);
end;

procedure TJvKeyboard.Remove(const AKey1: Word; const AShift1: TShiftState);
begin
  Remove2(AKey1, AShift1, 0, []);
end;

procedure TJvKeyboard.Remove2(const AKey1: Word; const AShift1: TShiftState;
  const AKey2: Word; const AShift2: TShiftState);
var
  I: Integer;
  ek: TJvEditKey;
begin
  for I := FList.Count - 1 downto 0 do
  begin
    ek := TJvEditKey(FList[I]);
    if (ek.Key1 = AKey1) and (ek.Shift1 = AShift1) and
      (ek.Key2 = AKey2) and (ek.Shift2 = AShift2) then
      FList.Delete(I);
  end;
end;

procedure TJvKeyboard.RemoveCtrl(const ACommand: TEditCommand);
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
    if TJvEditKey(FList[I]).Command = ACommand then
      FList.Delete(I);
end;

procedure TJvKeyboard.Clear;
begin
  FList.Clear;
end;

function TJvKeyboard.Command(const AKey: Word; const AShift: TShiftState): TEditCommand;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    with TJvEditKey(FList[I]) do
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
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    with TJvEditKey(FList[I]) do
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
  Add(ecIndent, Ord('I'), [ssShift, ssCtrl]);
  Add(ecUnindent, Ord('U'), [ssShift, ssCtrl]);

  Add(ecRecordMacro, Ord('R'), [ssCtrl, ssShift]);
  Add(ecPlayMacro, Ord('P'), [ssCtrl, ssShift]);
end;

//=== { TJvEditorClient } ====================================================

function TJvEditorClient.GetCanvas: TJvUnicodeCanvas;
begin
  Result := TJvUnicodeCanvas(FJvEditor.Canvas);
end;

function TJvEditorClient.Left: Integer;
begin
  Result := FJvEditor.GutterWidth + 2;
end;

function TJvEditorClient.Height: Integer;
begin
  Result := FJvEditor.ClientHeight;
end;

function TJvEditorClient.Width: Integer;
begin
  Result := Max(FJvEditor.ClientWidth - Left, 0);
end;

function TJvEditorClient.ClientWidth: Integer;
begin
  Result := Width;
end;

function TJvEditorClient.ClientHeight: Integer;
begin
  Result := Height;
end;

function TJvEditorClient.ClientRect: TRect;
begin
  Result := Bounds(Left, Top, Width, Height);
end;

function TJvEditorClient.BoundsRect: TRect;
begin
  Result := Bounds(0, 0, Width, Height);
end;

//=== { TJvGutter } ==========================================================

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
    FillRect(Bounds(0, FEditorClient.Top, GutterWidth, FEditorClient.Height));
    Pen.Width := 1;
    Pen.Color := Color;
    MoveTo(GutterWidth - 2, FEditorClient.Top);
    LineTo(GutterWidth - 2, FEditorClient.Top + FEditorClient.Height);
    Pen.Width := 2;
    MoveTo(GutterWidth + 1, FEditorClient.Top);
    LineTo(GutterWidth + 1, FEditorClient.Top + FEditorClient.Height);
    Pen.Width := 1;
    Pen.Color := clGray;
    MoveTo(GutterWidth - 1, FEditorClient.Top);
    LineTo(GutterWidth - 1, FEditorClient.Top + FEditorClient.Height);

    GutterPaint(Canvas);
  end;
end;


//=== { TJvLineInformation } =================================================

constructor TJvLineInformation.Create(AEditor: TJvCustomEditorBase; ALine: Integer);
begin
  inherited Create;
  FEditor := AEditor;
  FLine := ALine;
  FSelectStyle := lssUnselected;
end;

destructor TJvLineInformation.Destroy;
begin
  if not (csDestroying in Editor.ComponentState) then
  begin
    Editor.FLineInformations.FList.Extract(Self);
    RepaintLine(Line);
  end;
  inherited Destroy;
end;

procedure TJvLineInformation.CheckEmpty;
begin
  if (Data = nil) and (SelectStyle = lssUnselected) then
    Free;
end;

procedure TJvLineInformation.RepaintLine(LineNum: Integer);
begin
  if Assigned(Editor) then
    if (LineNum >= 0) and (LineNum < Editor.LineCount) then
      Editor.PaintLine(Line, 0, Editor.VisibleColCount);
end;

procedure TJvLineInformation.SetLine(Value: Integer);
var
  LastLine: Integer;
begin
  if Value <> FLine then
  begin
    LastLine := FLine;
    FLine := Value;
    RepaintLine(LastLine);
    RepaintLine(Line);
    CheckEmpty;
  end;
end;

procedure TJvLineInformation.SetSelectStyle(const Value: TJvLineSelectStyle);
begin
  if Value <> FSelectStyle then
  begin
    FSelectStyle := Value;
    RepaintLine(Line);
    CheckEmpty;
  end;
end;

//=== { TJvLineInformationList } =============================================

constructor TJvLineInformationList.Create(AEditor: TJvCustomEditorBase);
begin
  inherited Create;
  FEditor := AEditor;
  FList := TObjectList.Create;
  FDebugColor := clNavy;
  FDebugTextColor := clWhite;
  FBreakpointColor := clRed;
  FBreakpointTextColor := clWhite;
  FErrorPointColor := clMaroon;
  FErrorPointTextColor := clWhite;
end;

destructor TJvLineInformationList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TJvLineInformationList.Clear;
begin
  FList.Clear;
end;

function TJvLineInformationList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJvLineInformationList.GetData(Index: Integer): Pointer;
var
  Item: TJvLineInformation;
begin
  Item := Lines[Index];
  if Item <> nil then
    Result := Item.Data
  else
    Result := nil;
end;

function TJvLineInformationList.GetItems(Index: Integer): TJvLineInformation;
begin
  Result := TJvLineInformation(FList[Index]);
end;

function TJvLineInformationList.GetLineCount: Integer;
begin
  Result := Editor.LineCount;
end;

function TJvLineInformationList.GetLines(Index: Integer): TJvLineInformation;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TJvLineInformation(FList[I]);
    if Result.Line = Index then
      Exit;
  end;
  Result := nil;
end;

function TJvLineInformationList.GetSelectStyle(Index: Integer): TJvLineSelectStyle;
var
  Item: TJvLineInformation;
begin
  Item := Lines[Index];
  if Item <> nil then
    Result := Item.SelectStyle
  else
    Result := lssUnselected;
end;

procedure TJvLineInformationList.SetData(Index: Integer; Value: Pointer);
begin
  CreateLineInfo(Index).Data := Value;
end;

procedure TJvLineInformationList.SetSelectStyle(Index: Integer;
  const Value: TJvLineSelectStyle);
begin
  CreateLineInfo(Index).SelectStyle := Value;
end;

function TJvLineInformationList.CreateLineInfo(Index: Integer): TJvLineInformation;
var
  I: Integer;
begin
  if Index < 0 then
    raise EListError.CreateResFmt(@SListIndexError, [LineCount]);
  for I := 0 to Count - 1 do
  begin
    Result := TJvLineInformation(FList[I]);
    if Result.Line = Index then
      Exit;
  end;
  Result := TJvLineInformation.Create(FEditor, Index);
  FList.Add(Result);
end;

procedure TJvLineInformationList.SetBreakpointColor(const Value: TColor);
begin
  if Value <> FBreakpointColor then
  begin
    FBreakpointColor := Value;
    if Count > 0 then
      Editor.Invalidate;
  end;
end;

procedure TJvLineInformationList.SetBreakpointTextColor(const Value: TColor);
begin
  if Value <> FBreakpointTextColor then
  begin
    FBreakpointTextColor := Value;
    if Count > 0 then
      Editor.Invalidate;
  end;
end;

procedure TJvLineInformationList.SetDebugColor(const Value: TColor);
begin
  if Value <> FDebugColor then
  begin
    FDebugColor := Value;
    if Count > 0 then
      Editor.Invalidate;
  end;
end;

procedure TJvLineInformationList.SetDebugTextColor(const Value: TColor);
begin
  if Value <> FDebugTextColor then
  begin
    FDebugTextColor := Value;
    if Count > 0 then
      Editor.Invalidate;
  end;
end;

procedure TJvLineInformationList.SetErrorPointColor(const Value: TColor);
begin
  if Value <> FErrorPointColor then
  begin
    FErrorPointColor := Value;
    if Count > 0 then
      Editor.Invalidate;
  end;
end;

procedure TJvLineInformationList.SetErrorPointTextColor(const Value: TColor);
begin
  if Value <> FErrorPointTextColor then
  begin
    FErrorPointTextColor := Value;
    if Count > 0 then
      Editor.Invalidate;
  end;
end;

//=== { TJvBracketHighlighting } =============================================

constructor TJvBracketHighlighting.Create;
begin
  inherited Create;
  FStart.Left := -1;
  FStop.Left := -1;

  FWordPairs := TStringList.Create;
  FCaseSensitiveWordPairs := True;

  FActive := False;
  FBorderColor := clSilver;
  FColor := clNone;
  FFontColor := clNone;
end;

destructor TJvBracketHighlighting.Destroy;
begin
  FWordPairs.Free;
  inherited Destroy;
end;

procedure TJvBracketHighlighting.Assign(Source: TPersistent);
begin
  if Source is TJvBracketHighlighting then
  begin
    with TJvBracketHighlighting(Source) do
    begin
      Self.FActive := FActive;
      Self.FFontColor := FFontColor;
      Self.FBorderColor := FBorderColor;
      Self.FColor := FColor;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvBracketHighlighting.SetWordPairs(Value: TStrings);
begin
  if Value <> FWordPairs then
    FWordPairs.Assign(Value);
end;


//=== { TJvCustomEditorBase } ================================================

var
  BlockTypeFormat: Integer = 0;

constructor TJvCustomEditorBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents {, csOpaque}, csDoubleClicks,
    csReplicatable];
  IncludeThemeStyle(Self, [csNeedsBorderPaint]);
  FInsertMode := True;
  FKeyboard := TJvKeyboard.Create;
  FRows := 1;
  FCols := 1;
  FUndoBuffer := TJvUndoBuffer.Create;
  FUndoBuffer.FJvEditor := Self;
  FGroupUndo := True;
  FBracketHighlighting := TJvBracketHighlighting.Create;

  FRightMarginVisible := True;
  FRightMargin := 80;
  FBorderStyle := bsSingle;
  Ctl3D := True;
  Height := 100;
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
  FScrollBarHorz := TJvControlScrollBar95.Create;
  FScrollBarVert := TJvControlScrollBar95.Create;
  FScrollBarVert.Kind := sbVertical;
  FScrollBarHorz.OnScroll := ScrollBarScroll;
  FScrollBarVert.OnScroll := ScrollBarScroll;

  Color := clWindow;
  FGutterColor := clBtnFace;
  FSelBackColor := clHighLight;
  FSelForeColor := clHighLightText;
  FRightMarginColor := clSilver;

  FEditorClient := TJvEditorClient.Create;
  FEditorClient.FJvEditor := Self;
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

  FSelection.SelBlockFormat := bfNonInclusive;
  if BlockTypeFormat = 0 then
    BlockTypeFormat := RegisterClipboardFormat('Borland IDE Block Type');

  { we can change font only after all objects are created }
  Font.Name := 'Courier New';
  Font.Size := 10;

  FFontCache := TList.Create;
  FLineInformations := TJvLineInformationList.Create(Self);
end;

destructor TJvCustomEditorBase.Destroy;
begin
  FBracketHighlighting.Free;
  FLineInformations.Free;
  FScrollBarHorz.Free;
  FScrollBarVert.Free;
  FEditorClient.Free;
  FKeyboard.Free;
  FUndoBuffer.Free;
  FGutter.Free;
  FontCacheClear; // free cached font instances
  FFontCache.Free;
  inherited Destroy;
end;

procedure TJvCustomEditorBase.Assign(Source: TPersistent);
var
  Src: TJvCustomEditorBase;
begin
  if Source is TJvCustomEditorBase then
  begin
    BeginUpdate;
    try
      Src := TJvCustomEditorBase(Source);

      FSelForeColor := Src.SelForeColor;
      FSelBackColor := Src.SelBackColor;
      Color := Src.Color;
      RightMarginColor := Src.RightMarginColor;
      { The following options are set directly by JvHLEditorPropertyForm
      FKeyboard.Assign(Src.Keyboard);
      FGroupUndo := Src.GroupUndo;
      FUndoAfterSave := Src.UndoAfterSave;
      FTabStops := Src.TabStops;
      FDoubleClickLine := Src.DoubleClickLine;
      FSmartTab := Src.SmartTab;
      FBackSpaceUnindents := Src.BackSpaceUnindents;
      FAutoIndent := Src.AutoIndent;
      FKeepTrailingBlanks := Src.KeepTrailingBlanks;
      FCursorBeyondEOF := Src.CursorBeyondEOF;
      FBlockOverwrite := Src.BlockOverwrite;
      FPersistentBlocks := Src.PersistentBlocks;}
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvCustomEditorBase.WMEditCommand(var Msg: TMessage);
begin
  Command(Msg.WParam);
  Msg.Result := Ord(True);
end;

procedure TJvCustomEditorBase.WMHScroll(var Msg: TWMHScroll);
begin
  FScrollBarHorz.DoScroll(Msg);
end;

procedure TJvCustomEditorBase.WMVScroll(var Msg: TWMVScroll);
begin
  FScrollBarVert.DoScroll(Msg);
end;

{$IFDEF VCL}

procedure TJvCustomEditorBase.WMSetCursor(var Msg: TWMSetCursor);
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

procedure TJvCustomEditorBase.WMCopy(var Msg: TMessage);
begin
  DoCopy;
  Msg.Result := Ord(True);
end;

procedure TJvCustomEditorBase.WMCut(var Msg: TMessage);
begin
  DoCut;
  Msg.Result := Ord(True);
end;

procedure TJvCustomEditorBase.WMPaste(var Msg: TMessage);
begin
  DoPaste;
  Msg.Result := Ord(True);
end;

procedure TJvCustomEditorBase.WMUndo(var Msg: TMessage);
begin
  Undo;
  Msg.Result := Ord(True);
end;

procedure TJvCustomEditorBase.WMClear(var Msg: TMessage);
begin
  if not ReadOnly then
    DeleteSelected;
  Msg.Result := Ord(ReadOnly);
end;

{$ENDIF VCL}

procedure TJvCustomEditorBase.EMSetReadOnly(var Msg: TMessage);
begin
  ReadOnly := Msg.WParam = 1;
end;

procedure TJvCustomEditorBase.EMSetSelection(var Msg: TMessage);
begin
  if (Msg.WParam = 0) and (Msg.LParam = -1) then
    SelectAll
  else
  begin
    SelStart := Msg.WParam;
    SelLength := Msg.LParam;
  end;
end;

procedure TJvCustomEditorBase.EMGetSelection(var Msg: TMessage);
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

procedure TJvCustomEditorBase.EMCanUndo(var Msg: TMessage);
begin
  Msg.Result := Ord(UndoBuffer.CanUndo);
end;

procedure TJvCustomEditorBase.WMGetTextLength(var Msg: TMessage);
begin
  Msg.Result := GetTextLen;
end;

procedure TJvCustomEditorBase.UpdateEditorSize;
const
  BiggestSymbol = 'W';
var
  I: Integer;
begin
  if (csLoading in ComponentState) or
    not HandleAllocated then // CreateWnd calls this method in this case
    Exit;
  FEditorClient.Canvas.Font := Font;
  FontCacheClear; // clear font cache

  FCellRect.Height := FEditorClient.Canvas.TextHeight(BiggestSymbol) + 1;
  // workaround the bug in Windows-9x
  // fixed by Dmitry Rubinstain
  FCellRect.Width := FEditorClient.Canvas.TextWidth(BiggestSymbol + BiggestSymbol) div 2;

  for I := 0 to 1024 do
    FMyDi[I] := FCellRect.Width;

  FVisibleColCount := Trunc(FEditorClient.ClientWidth / FCellRect.Width);
  FVisibleRowCount := Trunc(FEditorClient.ClientHeight / FCellRect.Height);
  FLastVisibleCol := FLeftCol + FVisibleColCount - 1;
  FLastVisibleRow := FTopRow + FVisibleRowCount - 1;
  Rows := LineCount;
  Cols := Max_X_Scroll;
  FScrollBarHorz.Page := FVisibleColCount;
  FScrollBarVert.Page := FVisibleRowCount;
  FScrollBarHorz.LargeChange := Max(FVisibleColCount, 1);
  FScrollBarVert.LargeChange := Max(FVisibleRowCount, 1);
  FScrollBarVert.Max := Max(1, FRows - 1 + FVisibleRowCount - 1);

  FGutter.Invalidate;
end;

procedure TJvCustomEditorBase.UpdateEditorView;
begin
  UpdateEditorSize;
  if Showing and (UpdateLock = 0) then
    Invalidate;
end;

procedure TJvCustomEditorBase.ScrollTimer(Sender: TObject);
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

function TJvCustomEditorBase.ExpandTabsAnsi(const S: AnsiString): AnsiString;
var
  ps, I: Integer;
  Sp: AnsiString;
  Tabs, LenSp: Integer;
  P: PChar;
begin
  ps := Pos(Tab, S);
  if ps > 0 then
  begin
   // How may Tab chars?
    Tabs := 1;
    for I := ps + 1 to Length(S) do
      if S[I] = Tab then
        Inc(Tabs);

    Sp := Spaces(GetDefTabStop(0, True));
    LenSp := Length(Sp);

   // needed memory
    SetLength(Result, Length(S) - Tabs + Tabs * LenSp);
    P := PChar(Result);

   // copy the chars before the Tab
    if ps > 1 then
    begin
      Move(S[1], P[0], ps - 1);
      Inc(P, ps - 1);
    end;

    for I := ps to Length(S) do
      if S[I] <> Tab then
      begin
        P[0] := S[I];
        Inc(P);
      end
      else
      if LenSp > 0 then
      begin
        Move(Sp[1], P[0], LenSp);
        Inc(P, LenSp);
      end;
  end
  else
    Result := S;
end;

function TJvCustomEditorBase.GetDefTabStop(X: Integer; Next: Boolean): Integer;
var
  I: Integer;
  S: AnsiString;
  A, B: Integer;
begin
  if Next then
  begin
    I := 0;
    S := Trim(SubStr(FTabStops, I, ' '));
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
      Inc(I);
      S := Trim(SubStr(FTabStops, I, ' '));
    end;
    { after last tab pos }
    Result := X + ((B - A) - ((X - B) mod (B - A)));
  end
  else
  begin
    I := 0;
    S := Trim(SubStr(FTabStops, I, ' '));
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
      Inc(I);
      S := Trim(SubStr(FTabStops, I, ' '));
    end;
    { after last tab pos }
    Result := X - ((B - A) - ((X - B) mod (B - A)));
  end;
end;

procedure TJvCustomEditorBase.TextAllChangedInternal(Unselect: Boolean);
begin
  FontCacheClear;
  if Unselect then
  begin
    FSelection.IsSelected := False;
    FSelection.Selecting := False;
  end;
end;

procedure TJvCustomEditorBase.Changed;
begin
  FModified := True;
  if Assigned(FOnChange) then
    FOnChange(Self);
  StatusChanged;
end;

procedure TJvCustomEditorBase.TextAllChanged;
begin
  TextAllChangedInternal(True);
end;

procedure TJvCustomEditorBase.StatusChanged;
begin
  HighlightBrackets;
  if Assigned(FOnChangeStatus) then
    FOnChangeStatus(Self);
end;

procedure TJvCustomEditorBase.SelectionChanged;
begin
end;

procedure TJvCustomEditorBase.GetAttr(Line, ColBeg, ColEnd: Integer);
begin
end;

procedure TJvCustomEditorBase.ChangeAttr(Line, ColBeg, ColEnd: Integer);
begin
end;

procedure TJvCustomEditorBase.GutterPaint(Canvas: TCanvas);
begin
  if Assigned(FOnPaintGutter) then
    FOnPaintGutter(Self, Canvas);
end;

procedure TJvCustomEditorBase.GutterClick(Line: Integer);
begin
  if Assigned(FOnGutterClick) then
    FOnGutterClick(Self, Line);
end;

procedure TJvCustomEditorBase.GutterDblClick(Line: Integer);
begin
  if Assigned(FOnGutterDblClick) then
    FOnGutterDblClick(Self, Line);
end;

procedure TJvCustomEditorBase.BookmarkChanged(Bookmark: Integer);
begin
  Gutter.Invalidate;
end;

procedure TJvCustomEditorBase.CompletionIdentifier(var Cancel: Boolean);
begin
end;

procedure TJvCustomEditorBase.CompletionTemplate(var Cancel: Boolean);
begin
end;

procedure TJvCustomEditorBase.DoCompletionIdentifier(var Cancel: Boolean);
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

procedure TJvCustomEditorBase.DoCompletionTemplate(var Cancel: Boolean);
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

procedure TJvCustomEditorBase.Resize;
begin
  UpdateEditorSize;
end;

procedure TJvCustomEditorBase.CreateWnd;
begin
  inherited CreateWnd;
  if FScrollBars in [ssHorizontal, ssBoth] then
    FScrollBarHorz.Handle := Handle;
  if FScrollBars in [ssVertical, ssBoth] then
    FScrollBarVert.Handle := Handle;
  FAllRepaint := True;
  UpdateEditorSize;
end;

procedure TJvCustomEditorBase.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array [TBorderStyle] of Cardinal =
    (0, WS_BORDER);
  ScrollStyles: array [TScrollStyle] of Cardinal =
    (0, WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL);
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

procedure TJvCustomEditorBase.Loaded;
begin
  inherited Loaded;
  UpdateEditorSize;
end;

{$IFDEF VisualCLX}

function TJvCustomEditorBase.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
begin
{  case QEvent_type(Event) of
    QEventType_
  end;}
  Result := inherited EventFilter(Sender, Event);
end;

{$ENDIF VisualCLX}

procedure TJvCustomEditorBase.ScrollBarScroll(Sender: TObject; ScrollCode:
  TScrollCode; var ScrollPos: Integer);
begin
  case ScrollCode of
    scLineUp..scPageDown, {scPosition,} scTrack {, scEndScroll}:
      if Sender = FScrollBarVert then
        Scroll(True, ScrollPos)
      else
      if Sender = FScrollBarHorz then
        Scroll(False, ScrollPos);
  end;
end;

procedure TJvCustomEditorBase.Scroll(Vert: Boolean; ScrollPos: Integer);
var
  R, RClip, RUpdate: TRect;
  OldFTopRow: Integer;
  OldFLeftCol: Integer;
begin
  if UpdateLock = 0 then
  begin
    PaintCaret(False);
    if Vert then
    begin {Vertical Scroll}
      { optimized scrolling }
      OldFTopRow := FTopRow;
      FTopRow := ScrollPos;
      {$IFDEF VCL}
      if Abs((OldFTopRow - ScrollPos) * FCellRect.Height) < FEditorClient.Height then
      begin
        R := FEditorClient.ClientRect;
        R.Bottom := R.Top + CellRect.Height * FVisibleRowCount;
        R.Left := 0; // update gutter
        RClip := R;
        Inc(RClip.Bottom, CellRect.Height);
        ScrollDC(
          FEditorClient.Canvas.Handle, // handle of device context
          0, // horizontal scroll units
          (OldFTopRow - ScrollPos) * FCellRect.Height, // vertical scroll units
          R, // address of structure for scrolling rectangle
          RClip, // address of structure for clipping rectangle
          0, // handle of scrolling region
          @RUpdate // address of structure for update rectangle
          );
        Inc(RUpdate.Bottom, FCellRect.Height);
        InvalidateRect(Handle, @RUpdate, False);
      end
      else
      {$ENDIF VCL}
        Invalidate;
      Update;
    end
    else {Horizontal Scroll}
    begin
      { optimized scrolling }
      OldFLeftCol := FLeftCol;
      FLeftCol := ScrollPos;
      {$IFDEF VCL}
      if Abs((OldFLeftCol - ScrollPos) * FCellRect.Width) < FEditorClient.Width then
      begin
        R := FEditorClient.ClientRect;
        R.Right := R.Left + CellRect.Width * FVisibleColCount;
        RClip := R;
        Inc(RClip.Right, CellRect.Width);
        ScrollDC(
          FEditorClient.Canvas.Handle, // handle of device context
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
  else { UpdateLock > 0 }
  begin
    if Vert then
      FTopRow := ScrollPos
    else
      FLeftCol := ScrollPos;
  end;
  FLastVisibleRow := FTopRow + FVisibleRowCount - 1;
  FLastVisibleCol := FLeftCol + FVisibleColCount - 1;
  if UpdateLock = 0 then
  begin
    DrawRightMargin;
    PaintCaret(True);
  end;
  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

procedure TJvCustomEditorBase.KeyDown(var Key: Word; Shift: TShiftState);
var
  Com: Word;
begin
  if Completion.Visible then
  begin
    if Completion.DoKeyDown(Key, Shift) then
    begin
      IgnoreKeyPress := True;
      Exit;
    end;
  end
  else
    Completion.FTimer.Enabled := False;

  if not (ssShift in Shift) then
    FSelection.Selecting := False;

  if WaitSecondKey then
  begin
    IgnoreKeyPress := True; { Set this before calling FKeyboard.Command2()
                              because in FKeyboard.OnCommand2 the
                              Editor-window can loose focus and so the
                              second char will be printed. }
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

  if Com = ecBackspace then
    Completion.DoKeyPress(Backspace);
end;

procedure TJvCustomEditorBase.KeyPress(var Key: Char);
var
  Ch: Char;
begin
  if IgnoreKeyPress or FReadOnly then
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
      Key := Char(Ch);
    end;
    // inherited KeyPress(Key);
    Command(Ord(Key));
  finally
    PaintCaret(True);
  end;
end;

procedure TJvCustomEditorBase.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  XX, YY, SX, SY: Integer;
begin
  if FDoubleClick then
  begin
    FDoubleClick := False;
    Exit;
  end;
  FSelection.Selecting := False;
  Completion.CloseUp(False);
  Mouse2Caret(X, Y, XX, YY);
  //if (XX = FCaretX) and (YY = FCaretY) then Exit;

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
  FMouseDown := True;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvCustomEditorBase.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FMouseDown and (ssLeft in (Shift * [ssShift, ssLeft]) ) then
  begin
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

procedure TJvCustomEditorBase.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  XX, YY: Integer;
begin
  if FMouseDown then
    TJvEndCompoundUndo.Create(Self);
  TimerScroll.Enabled := False;
  FMouseDown := False;
  inherited MouseUp(Button, Shift, X, Y);

  // Gutter click
  if (X >= 0) and (X < FGutterWidth) then
  begin
    Mouse2Caret(X, Y, XX, YY);
    GutterClick(YY);
  end;
end;

function TJvCustomEditorBase.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
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
    if WheelDelta < 0 then
      WheelDirection := -1
    else
      WheelDirection := 1;
    // Ctrl+Wheel: scrollbar large change
    FScrollBarVert.Position := FScrollBarVert.Position - WheelDirection * FScrollBarVert.LargeChange;
    Scroll(True, FScrollBarVert.Position);
  end
  else
  if Shift = [] then
  begin
    FScrollBarVert.Position := FScrollBarVert.Position - WheelDelta div 40;
    Scroll(True, FScrollBarVert.Position);
  end;
end;

procedure TJvCustomEditorBase.DblClick;
var
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
    if FCaretY = LineCount - 1 then
    begin
      SetSel(LineLength[FCaretY], FCaretY);
      SetCaret(LineLength[FCaretY], FCaretY);
    end
    else
    begin
      SetSel(0, FCaretY + 1);
      SetCaret(0, FCaretY + 1);
    end;
    PaintCaret(True);
  end
  else
  if LineCount > 0 then
    SelectWordOnCaret;
end;

procedure TJvCustomEditorBase.DoGetDlgCode(var Code: TDlgCodes);
begin
  Code := [dcWantArrows, dcWantTab, dcWantChars, dcWantMessage];
end;

procedure TJvCustomEditorBase.DoSetFocus(FocusedWnd: HWND);
begin
  inherited DoSetFocus(FocusedWnd);
  CreateCaret(Handle, 0, 2, CellRect.Height - 2);
  PaintCaret(True);
end;

procedure TJvCustomEditorBase.DoKillFocus(FocusedWnd: HWND);
begin
  inherited DoKillFocus(FocusedWnd);
  Completion.CloseUp(False);
  DestroyCaret;
end;

procedure TJvCustomEditorBase.DoPaste;
begin
  if not FReadOnly then
    PostCommand(ecClipboardPaste);
end;

procedure TJvCustomEditorBase.DoCopy;
begin
  PostCommand(ecClipboardCopy);
end;

procedure TJvCustomEditorBase.DoCut;
begin
  if not FReadOnly then
    PostCommand(ecClipboardCut);
end;

procedure TJvCustomEditorBase.CursorChanged;
var
  P: TPoint;
begin
  inherited CursorChanged;
  if HandleAllocated then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    if (P.X < GutterWidth) and (Cursor = crIBeam) then
      SetCursor(Screen.Cursors[crArrow]);
  end;
end;

procedure TJvCustomEditorBase.FontChanged;
begin
  inherited FontChanged;
  if HandleAllocated then
    UpdateEditorSize;
end;

function TJvCustomEditorBase.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  Result := False; // no background erase
end;

procedure TJvCustomEditorBase.SetGutterWidth(AWidth: Integer);
begin
  if FGutterWidth <> AWidth then
  begin
    FGutterWidth := AWidth;
    UpdateEditorSize;
    Invalidate;
  end;
end;

procedure TJvCustomEditorBase.SetGutterColor(AColor: TColor);
begin
  if FGutterColor <> AColor then
  begin
    FGutterColor := AColor;
    Gutter.Invalidate;
  end;
end;

procedure TJvCustomEditorBase.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

function TJvCustomEditorBase.GetSelStart: Integer;
begin
  if FSelection.IsSelected then
    Result := PosFromCaret(FSelection.SelBegX, FSelection.SelBegY)
  else
    Result := PosFromCaret(FCaretX, FCaretY);
end;

procedure TJvCustomEditorBase.SetSelStart(ASelStart: Integer);
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
  end; 
end;

procedure TJvCustomEditorBase.SetSelLength(ASelLength: Integer);
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
  end;
end;

function TJvCustomEditorBase.GetSelLength: Integer;
var
  I: Integer;
  Len, CLen: Integer;
begin
  Result := 0;
  with FSelection do
  begin
    if not IsSelected then
      Exit;

    if (SelBegY < 0) or (SelBegY > LineCount - 1) or (SelEndY < 0) or
      (SelEndY > LineCount - 1) then
    begin
      if BeepOnError then
        Beep;
      Exit;
    end;

    if SelBlockFormat = bfColumn then
    begin
      for I := SelBegY to SelEndY do
      begin
        CLen := LineLength[I] - SelBegX;
        if CLen < 0 then
          CLen := 0;
        if CLen > SelEndX - SelBegX + 1 then
          CLen := SelEndX - SelBegX + 1;

        Inc(Result, CLen + sLineBreakLen);
      end;
      if Result > 0 then
        Dec(Result, sLineBreakLen);
    end
    else
    begin
      if SelBegY = SelEndY then
      begin
        Result := SelEndX - SelBegX + Ord(SelBlockFormat = bfInclusive);
        if SelBegX + Result > LineLength[SelEndY] then
          Result := LineLength[SelEndY] - SelBegX;
        if Result < 0 then
          Result := 0;
      end
      else
      begin
        Result := LineLength[SelBegY] - SelBegX;
        if Result < 0 then
          Result := 0;
        for I := SelBegY + 1 to SelEndY - 1 do
          Inc(Result, sLineBreakLen + LineLength[I]);

        Len := SelEndX + Ord(SelBlockFormat = bfInclusive);
        if Len > LineLength[SelEndY] then
          Len := LineLength[SelEndY];
        Result := Result + sLineBreakLen + Len;
      end;
    end;
  end;
end;

procedure TJvCustomEditorBase.SetSelBlockFormat(Value: TJvSelBlockFormat);
begin
  Command(ecInclusiveBlock + Ord(Value));
end;

function TJvCustomEditorBase.GetSelBlockFormat: TJvSelBlockFormat;
begin
  Result := FSelection.SelBlockFormat;
end;

procedure TJvCustomEditorBase.SetMode(Index: Integer; Value: Boolean);
begin
  if Index = 0 then
  begin
    if FInsertMode <> Value then
    begin
      FInsertMode := Value;
      StatusChanged;
    end;
  end
  else {1 :}
  begin
    if Value then
      SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or ES_READONLY)
    else
      SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not ES_READONLY);
    if FReadOnly <> Value then
    begin
      FReadOnly := Value;
      StatusChanged;
    end;
  end;
end;

procedure TJvCustomEditorBase.SetCaretPosition(Index, Pos: Integer);
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

procedure TJvCustomEditorBase.SetCols(ACols: Integer);
begin
  if FCols <> ACols then
  begin
    FCols := Max(ACols, 1);
    FScrollBarHorz.Max := FCols - 1;
  end;
end;

procedure TJvCustomEditorBase.SetRows(ARows: Integer);
begin
  if FRows <> ARows then
  begin
    FRows := Max(ARows, 1);
    FScrollBarVert.Max := Max(1, FRows - 1 + FVisibleRowCount - 1);
  end;
end;

procedure TJvCustomEditorBase.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomEditorBase.SetRightMarginVisible(Value: Boolean);
begin
  if FRightMarginVisible <> Value then
  begin
    FRightMarginVisible := Value;
    Invalidate;
  end;
end;

procedure TJvCustomEditorBase.SetRightMargin(Value: Integer);
begin
  if FRightMargin <> Value then
  begin
    FRightMargin := Value;
    Invalidate;
  end;
end;

procedure TJvCustomEditorBase.SetRightMarginColor(Value: TColor);
begin
  if FRightMarginColor <> Value then
  begin
    FRightMarginColor := Value;
    Invalidate;
  end;
end;

procedure TJvCustomEditorBase.SetSelBackColor(const Value: TColor);
begin
  if Value <> FSelBackColor then
  begin
    FSelBackColor := Value;
    if FSelection.IsSelected then
      Invalidate;
  end;
end;

procedure TJvCustomEditorBase.SetSelForeColor(const Value: TColor);
begin
  if Value <> FSelForeColor then
  begin
    FSelForeColor := Value;
    if FSelection.IsSelected then
      Invalidate;
  end;
end;

function TJvCustomEditorBase.GetPopupMenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if (Result = nil) and UseFixedPopup then
    Result := FixedDefaultEditPopup(Self);
end;

procedure TJvCustomEditorBase.LockUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TJvCustomEditorBase.UnlockUpdate;
begin
  Dec(FUpdateLock);
end;

function TJvCustomEditorBase.GetClipboardBlockFormat: TJvSelBlockFormat;
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
        Result := TJvSelBlockFormat(PInteger(GlobalLock(Data))^);
    finally
      if Data <> 0 then
        GlobalUnlock(Data);
      Clipboard.Close;
    end;
  end;
end;

procedure TJvCustomEditorBase.SetClipboardBlockFormat(const Value: TJvSelBlockFormat);
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

procedure TJvCustomEditorBase.SetSel(SelX, SelY: Integer);
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
    if SelX < 0 then
      SelX := 0;
    if SelY < 0 then
      SelY := 0;
    if SelY >= LineCount then
    begin
      if LineCount = 0 then
        SelY := 0 // select none
      else
      begin
        SelY := LineCount - 1;  // select last line
        if not (FSelection.SelBlockFormat in [bfLine, bfColumn]) then
          SelX := LineLength[SelY]; // with all text
      end;
    end;
    if not (SelBlockFormat in [bfLine, bfColumn]) then
    begin
      if (LineCount > 0) and (SelY < LineCount) then
      begin
        LineLen := LineLength[SelY];
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

    if not Selecting then
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
  end;
end;

function TJvCustomEditorBase.IsNewSelection: Boolean;
begin
  if FPersistentBlocks then
    Result := (not FSelection.IsSelected) or FPersistentBlocksCaretChanged
  else
    Result := (not FSelection.IsSelected);
end;

function TJvCustomEditorBase.IsEmptySelection: Boolean;
begin
  with FSelection do
    Result := IsSelected and (SelBegX = SelEndX) and (SelBegY = SelEndY);
end;

procedure TJvCustomEditorBase.PaintSelection;
var
  I: Integer;
begin
  for I := FUpdateSelBegY to FUpdateSelEndY do
    PaintLine(I, -1, -1);
end;

procedure TJvCustomEditorBase.SetUnSelected;
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

procedure TJvCustomEditorBase.RemoveSelectedBlock;
begin
  if FSelection.IsSelected then
  begin
    if FBlockOverwrite and not FPersistentBlocks then
      DeleteSelected
    else
    if not FPersistentBlocks then
      SetUnSelected;
  end;
end;

procedure TJvCustomEditorBase.PersistentBlocksSetUnSelected;
begin
  FPersistentBlocksCaretChanged := True;
  if not FPersistentBlocks then
    SetUnSelected;
end;

procedure TJvCustomEditorBase.SetSelUpdateRegion(BegY, EndY: Integer);
begin
  if FUpdateSelBegY > BegY then
    FUpdateSelBegY := BegY;
  if FUpdateSelEndY < EndY then
    FUpdateSelEndY := EndY;
end;

procedure TJvCustomEditorBase.AdjustSelLineMode(Restore: Boolean);
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

procedure TJvCustomEditorBase.AdjustPersistentBlockSelection(X, Y: Integer;
  Mode: TAdjustPersistentBlockMode; Args: array of Integer);
begin
  // persistent blocks: adjust selection
  if (not FPersistentBlocks) or (not FSelection.IsSelected) then
    Exit;

  if (FSelection.SelBlockFormat = bfColumn) and
     not (Mode in [amDeleteLine, amLineConcat, amLineBreak]) then
       Exit;

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
          if Y < SelBegY then
          begin
            // move down
            Inc(SelBegY);
            Inc(SelEndY);
          end
          else
          if Y <= SelEndY then
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
            else
            if Y < SelEndY then
            begin
              // LineBreak in selection
              Inc(SelEndY);
            end
            else
            if {(Y = SelEndY) and} (X < SelEndX) and (SelBlockFormat <> bfColumn) then
            begin
              // LineBreak in the last line
              SelEndX := SelEndX - X;
              Inc(SelEndY);
            end;
          end;
        end;
      amLineConcat: // X=CaretX, Y=CaretY, Args[0]=ConcatCaretX, Args[1]=ConcatCaretY
        begin
          if Y < SelBegY then
          begin
            // move up
            Dec(SelBegY);
            Dec(SelEndY);
          end
          else
          if Y <= SelEndY then
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
            else
            if Y < SelEndY then
              // LineConcat in selection
              Dec(SelEndY)
            else
            if {(Y = SelEndY) and} (X <= SelEndX) and (SelBlockFormat <> bfColumn) then
            begin
              // LineConcat in the last line
              Inc(SelEndX, LineLength[Args[1]]);
              Dec(SelEndY);
            end;
          end;
        end;
    end; // case

    if SelBegY < 0 then
      SelBegY := 0;
    if (SelEndY < SelBegY) or (SelBegY >= LineCount) then
      SetUnSelected;
    if SelBegX < 0 then
      SelBegX := 0;
    if SelEndX > Max_X then
      SelEndX := Max_X;
    if (SelEndX < SelBegX) and (SelBegY = SelEndY) then
      SetUnSelected;

    // set update region
    SetSelUpdateRegion(SelBegY, SelEndY);
  end;
end;

procedure TJvCustomEditorBase.Paint;
var
  I: Integer;
  ECR: TRect;
  BX, EX, BY, EY: Integer;
begin
  if UpdateLock > 0 then
    Exit;
  PaintCaret(False);

  ECR := FEditorClient.Canvas.ClipRect;
  OffsetRect(ECR, -FGutterWidth, 0);
  if FAllRepaint then
    ECR := FEditorClient.BoundsRect;
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

procedure TJvCustomEditorBase.PaintLine(Line: Integer; ColBeg, ColEnd: Integer);
var
  R: TRect;
  ColPainted: Integer;
begin
  if (Line < FTopRow) or (Line > FTopRow + FVisibleRowCount) then
    Exit;
  if ColBeg < FLeftCol then
    ColBeg := FLeftCol;
  if (ColEnd < 0) or (ColEnd > FLeftCol + FVisibleColCount) then
    ColEnd := FLeftCol + FVisibleColCount;
  ColEnd := Min(ColEnd, Max_X - 1);

  ColPainted := ColBeg;
  if (Line > -1) and (Line < LineCount) then
    PaintLineText(Line, ColBeg, ColEnd, ColPainted)
  else
  begin
    FEditorClient.Canvas.Brush.Color := Color;
    FEditorClient.Canvas.FillRect(Bounds(FEditorClient.Left, (Line - FTopRow) *
      FCellRect.Height, 1, FCellRect.Height));
  end;
  {right part}
  R := Bounds(CalcCellRect(ColPainted - FLeftCol, Line - FTopRow).Left,
    (Line - FTopRow) * FCellRect.Height,
    (FLeftCol + FVisibleColCount - ColPainted + 2) * FCellRect.Width,
    FCellRect.Height);
  {if the line is selected, paint right empty space with selected background}
  if FSelection.IsSelected and (FSelection.SelBlockFormat in [bfInclusive, bfLine, bfNonInclusive]) and
    (Line >= FSelection.SelBegY) and (Line < FSelection.SelEndY) then
    FEditorClient.Canvas.Brush.Color := FSelBackColor
  else
    FEditorClient.Canvas.Brush.Color := Color;
  FEditorClient.Canvas.FillRect(R);
  DrawRightMargin;
end;

procedure TJvCustomEditorBase.PaintLine(Line: Integer);
begin
  PaintLine(Line, -1, -1);
end;

procedure TJvCustomEditorBase.GetBracketHighlightAttr(Line: Integer; var Attrs: TLineAttrs);

  procedure GetHighlightBeginEnd(const R: TRect);
  var
    i: Integer;
  begin
    if (R.Left >= 0) and // R valid
       (Line >= R.Top) and (Line <= R.Bottom) and (R.Left >= 0) and (R.Right <= Max_X) then
      for i := R.Left to R.Right do
      begin
        if BracketHighlighting.FontColor <> clNone then
          Attrs[i].FC := BracketHighlighting.FontColor;
        if BracketHighlighting.Color <> clNone then
          Attrs[i].BC := BracketHighlighting.Color;
        Attrs[i].Border := BracketHighlighting.BorderColor;
      end;
  end;

begin
  if BracketHighlighting.Active then
  begin
    GetHighlightBeginEnd(FBracketHighlighting.FStart);
    GetHighlightBeginEnd(FBracketHighlighting.FStop);
  end;
end;

procedure TJvCustomEditorBase.HighlightBrackets;
const
  Separators: TSysCharSet = [#0, ' ', '-', #13, #10, '.', ',', '/', '\', '#', '"', '''',
    ':', '+', '%', '*', '(', ')', ';', '=', '{', '}', '[', ']', '{', '}', '<', '>'];
var
  Text: AnsiString;
  X, Y: Integer;
  SearchDir: Integer;
  SearchStart: AnsiString;
  SearchEnd: AnsiString;
  SearchOpen: Integer;
  CaseSensitive: Boolean;
  CmpProc: function(const Str1, Str2: PChar; MaxLen: Cardinal): Integer;
  IsCharCmp: Boolean;
begin
  X := CaretX;
  Y := CaretY;

  { remove last highlighting }
  if BracketHighlighting.FStart.Left > -1 then
  begin
    BracketHighlighting.FStart.Left := -1; // invalidate
    PaintLine(BracketHighlighting.FStart.Top);
  end;
  if FBracketHighlighting.FStop.Left > -1 then
  begin
    BracketHighlighting.FStop.Left := -1; // invalidate
    PaintLine(BracketHighlighting.FStop.Top);
  end;

  if not BracketHighlighting.Active or not Visible or not Enabled then
    Exit;

  if (Y >= 0) and GetAnsiTextLine(Y, Text) and (X >= 0) and (X < Length(Text)) then
  begin
    SearchDir := 0; // nothing to search
    CaseSensitive := False;
    IsCharCmp := True;

    // obtain search direction and end-char
    if Text[X + 1] in ['(', '{', '[', '<'] then
    begin
      SearchDir := +1;
      SearchStart := Text[X + 1];
      case Text[X + 1] of
        '(': SearchEnd := ')';
        '{': SearchEnd := '}';
        '[': SearchEnd := ']';
        '<': SearchEnd := '>';
      end;
    end
    else
    if Text[X + 1] in [')', '}', ']', '>'] then
    begin
      SearchDir := -1;
      SearchStart := Text[X + 1];
      case Text[X + 1] of
        ')': SearchEnd := '(';
        '}': SearchEnd := '{';
        ']': SearchEnd := '[';
        '>': SearchEnd := '<';
      end;
    end
    else
    begin
      IsCharCmp := False;
      // Text search
      SearchStart := GetAnsiWordOnCaret;
      while (X >= 0) and not (Text[X + 1] in Separators) do
        Dec(X);
      Inc(X);

      GetBracketHighlightingWords(SearchDir, SearchStart, SearchEnd, CaseSensitive);
    end;

    if SearchDir <> 0 then
    begin
      BracketHighlighting.FStart.TopLeft := Point(X + 1, Y);
      BracketHighlighting.FStart.BottomRight := Point(X + 1 + Length(SearchStart) - 1, Y);
      PaintLine(Y);

      if CaseSensitive then
        CmpProc := StrLComp
      else
        CmpProc := StrLIComp;

      SearchOpen := 1;
      repeat
        Inc(X, SearchDir);

        // -1 direction
        if X < 0 then
        begin
          Dec(Y);
          if Y < 0 then
            Break;
          GetAnsiTextLine(Y, Text);
          X := Length(Text) - 1;
          if X < 0 then
            Continue;
        end
        else // +1 direction
        if X >= Length(Text) then
        begin
          Inc(Y);
          if not GetAnsiTextLine(Y, Text) then
            Break;
          X := 0;
          if X >= Length(Text) then
            Continue;
        end;

        if IsCharCmp then // it is faster to compare one char 
        begin
          if Text[X + 1] = SearchEnd[1] then 
          begin
            Dec(SearchOpen);
            if SearchOpen = 0 then
            begin
              BracketHighlighting.FStop.TopLeft := Point(X + 1, Y);
              BracketHighlighting.FStop.BottomRight := Point(X + 1, Y);
              PaintLine(Y);
              Break;
            end;
          end
          else
          if Text[X + 1] = SearchStart[1] then
            Inc(SearchOpen);
        end
        else
        begin
          // word pairs
          if CmpProc(PChar(Text) + X, PChar(SearchEnd), Length(SearchEnd)) = 0 then // case sensitive
          begin
            Dec(SearchOpen);
            if SearchOpen = 0 then
            begin
              BracketHighlighting.FStop.TopLeft := Point(X + 1, Y);
              BracketHighlighting.FStop.BottomRight := Point(X + 1 + Length(SearchEnd) - 1, Y);
              PaintLine(Y);
              Break;
            end;
          end
          else
          if CmpProc(PChar(Text) + X, PChar(SearchStart), Length(SearchStart)) = 0 then // case sensitive
            Inc(SearchOpen);
        end;
      until False;
    end;
  end;
end;

procedure TJvCustomEditorBase.GetBracketHighlightingWords(var Direction: Integer;
  var Start, Stop: AnsiString; var CaseSensitive: Boolean);
var
  I, Ps: Integer;
  S: string;
  CmpProc: function(const S1, S2: string): Integer; 
begin
  CaseSensitive := BracketHighlighting.CaseSensitiveWordPairs;
  if CaseSensitive then
    CmpProc := CompareStr
  else
    CmpProc := CompareText;

  for I := 0 to BracketHighlighting.WordPairs.Count - 1 do
  begin
    S := BracketHighlighting.WordPairs[I];
    Ps := Pos('=', S);
    if Ps > 0 then
    begin
      if CmpProc(Copy(S, 1, Ps - 1), Start) = 0 then
      begin
        Stop := Copy(S, Ps + 1, MaxInt);
        Direction := +1;
        Break;
      end;
      if CmpProc(Copy(S, Ps + 1, MaxInt), Start) = 0 then
      begin
        Stop := Copy(S, 1, Ps - 1);
        Direction := -1;
        Break;
      end;
    end;
  end;
end;

{ find the font resource for LA }

function TJvCustomEditorBase.FontCacheFind(LA: TLineAttr): TFont;
var
  I: Integer;
begin
 // find the font instance
  for I := 0 to FFontCache.Count - 1 do
  begin
    Result := TFont(FFontCache.Items[I]);
    if (Result.Style = LA.Style) and (Result.Color = LA.FC) then
       Exit;
  end;
 // create a new font instance
  Result := TFont.Create;
  Result.Assign(FEditorClient.Canvas.Font); // copy default font
  Result.Style := LA.Style;
  Result.Color := LA.FC;
  FFontCache.Add(Result); { store in FontCache }
end;

{ clear the font resource cache }

procedure TJvCustomEditorBase.FontCacheClear;
var
  I: Integer;
begin
  for I := 0 to FFontCache.Count - 1 do
    TFont(FFontCache.Items[I]).Free;
  FFontCache.Clear;
end;

procedure TJvCustomEditorBase.DrawRightMargin;
var
  F: Integer;
begin
  if FRightMarginVisible and (FRightMargin > FLeftCol) and
    (FRightMargin < FLastVisibleCol + 3) then
    with FEditorClient.Canvas do
    begin
      { we paint RightMargin Line [translated] }
      Pen.Color := FRightMarginColor;
      F := CalcCellRect(FRightMargin - FLeftCol, 0).Left;
      MoveTo(F, FEditorClient.Top);
      LineTo(F, FEditorClient.Top + FEditorClient.Height);
    end;
end;

procedure TJvCustomEditorBase.Mouse2Cell(X, Y: Integer; var CX, CY: Integer);
begin
  CX := Round((X - FEditorClient.Left) / FCellRect.Width);
  CY := (Y - FEditorClient.Top) div FCellRect.Height;
end;

procedure TJvCustomEditorBase.Mouse2Caret(X, Y: Integer; var CX, CY: Integer);
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
  if CY > LineCount - 1 then
    CY := LineCount - 1;
end;

procedure TJvCustomEditorBase.CaretCoord(X, Y: Integer; var CX, CY: Integer);
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

function TJvCustomEditorBase.PosFromMouse(X, Y: Integer): Integer;
var
  X1, Y1: Integer;
begin
  Mouse2Caret(X, Y, X1, Y1);
  if (X1 < 0) or (Y1 < 0) then
    Result := -1
  else
    Result := PosFromCaret(X1, Y1);
end;

procedure TJvCustomEditorBase.SetCaretInternal(X, Y: Integer);
var
  R: TRect;
begin
  if (X = FCaretX) and (Y = FCaretY) then
    Exit;
  // To scroll the image [translated]
  if not FCursorBeyondEOF then
    Y := Min(Y, LineCount - 1);
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
    StatusChanged;
  end;
  FCaretX := X;
  FCaretY := Y;
end;

procedure TJvCustomEditorBase.NotUndoable;
begin
  FUndoBuffer.Clear;
end;

procedure TJvCustomEditorBase.NotRedoable;
begin
  FUndoBuffer.ClearRedo;
end;

procedure TJvCustomEditorBase.ChangeBookmark(Bookmark: TBookmarkNum;
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
    if Bookmarks[Bookmark].Valid and (Bookmarks[Bookmark].Y = FCaretY) then
      Bookmarks[Bookmark].Valid := False
    else
    begin
      Bookmarks[Bookmark].X := FCaretX;
      Bookmarks[Bookmark].Y := FCaretY;
      Bookmarks[Bookmark].Valid := True;
    end
  else
  if Bookmarks[Bookmark].Valid then
    SetXY(Bookmarks[Bookmark].X, Bookmarks[Bookmark].Y);
  BookmarkChanged(Bookmark);
end;

procedure TJvCustomEditorBase.BeginRecord;
begin
  FMacro := '';
  FRecording := True;
  StatusChanged;
end;

procedure TJvCustomEditorBase.EndRecord(var AMacro: TMacro);
begin
  FRecording := False;
  AMacro := FMacro;
  StatusChanged;
end;

procedure TJvCustomEditorBase.PlayMacro(const AMacro: TMacro);
var
  I: Integer;
begin
  BeginUpdate;
  BeginCompound;
  try
    I := 1;
    while I < Length(AMacro) do
    begin
      Command(Byte(AMacro[I]) + Byte(AMacro[I + 1]) shl 8);
      Inc(I, 2);
    end;
  finally
    EndCompound;
    EndUpdate;
  end;
end;

procedure TJvCustomEditorBase.SetLeftTop(ALeftCol, ATopRow: Integer);
begin
  if ALeftCol < 0 then
    ALeftCol := 0;
  if FLeftCol <> ALeftCol then
  begin
    FScrollBarHorz.Position := ALeftCol;
    Scroll(False, ALeftCol);
  end;
  if ATopRow < 0 then
    ATopRow := 0;
  if FTopRow <> ATopRow then
  begin
    FScrollBarVert.Position := ATopRow;
    Scroll(True, ATopRow);
  end;
end;

function TJvCustomEditorBase.CanUndo: Boolean;
begin
  Result := FUndoBuffer.CanUndo;
end;

function TJvCustomEditorBase.CanRedo: Boolean;
begin
  Result := FUndoBuffer.CanRedo;
end;

function TJvCustomEditorBase.CanCopy: Boolean;
begin
  Result := FSelection.IsSelected and not IsEmptySelection;
end;

function TJvCustomEditorBase.CanPaste: Boolean;
var
  H: THandle;
begin
  Result := False;
  if (FCaretY >= LineCount) and (LineCount > 0) then
    Exit;

  try
    H := Clipboard.GetAsHandle(CF_TEXT);
    if H <> 0 then
      Result := (GlobalSize(H) > 0);
  except
    Result := False;
  end;
end;

function TJvCustomEditorBase.CanCut: Boolean;
begin
  Result := CanCopy and not ReadOnly;
end;

function TJvCustomEditorBase.CanSelectAll: Boolean;
var
  MaxCol, MaxLine: Integer;
begin
  MaxLine := LineCount - 1;
  if MaxLine > 0 then
    MaxCol := LineLength[MaxLine]
  else
    MaxCol := 0;
  Result := (FSelection.SelBegX > 0) or (FSelection.SelBegY > 0) or
    (FSelection.SelEndX < MaxCol) or (FSelection.SelEndY < MaxLine);
end;

procedure TJvCustomEditorBase.SelectAll;
begin
  SelectRange(0, 0, Max_X, MaxInt);
end;

function TJvCustomEditorBase.HasSelection: Boolean;
begin
  Result := FSelection.IsSelected and not IsEmptySelection;
end;

procedure TJvCustomEditorBase.ClipboardCut;
begin
  ClipboardCopy;
  DeleteSelected;
end;

procedure TJvCustomEditorBase.ClearSelection;
begin
  SetUnSelected;
end;

procedure TJvCustomEditorBase.Redo;
begin
  FUndoBuffer.Redo;
end;

procedure TJvCustomEditorBase.Undo;
begin
  FUndoBuffer.Undo;
end;

procedure TJvCustomEditorBase.SelectRange(BegX, BegY, EndX, EndY: Integer);
begin
  { --- UNDO --- }
  TJvSelectUndo.Create(Self, FCaretX, FCaretY);
  { --- /UNDO ---}
  with FSelection do
  begin
    IsSelected := False;
    Selecting := False;

    if BegX < 0 then
      BegX := 0;
    if BegY < 0 then
      BegY := 0;
    if EndX > Max_X then
      EndX := Max_X;
    if (EndY < BegY) or (BegY >= LineCount) then
      Exit;
    if EndY >= LineCount then
      EndY := LineCount - 1;
    if EndY < 0 then
      Exit;

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

function TJvCustomEditorBase.CalcCellRect(X, Y: Integer): TRect;
begin
  Result := Bounds(
    FEditorClient.Left + X * FCellRect.Width + 1,
    FEditorClient.Top + Y * FCellRect.Height,
    FCellRect.Width,
    FCellRect.Height)
end;

procedure TJvCustomEditorBase.SetCaret(X, Y: Integer);
begin
  if (X = FCaretX) and (Y = FCaretY) then
    Exit;
  {--- UNDO ---}
  TJvCaretUndo.Create(Self, FCaretX, FCaretY);
  {--- /UNDO ---}
  SetCaretInternal(X, Y);
  if UpdateLock = 0 then
    StatusChanged;
end;

{ It returns on the index of pos - to the number of symbol - its coordinate.
  Returns on index Pos - to number of the character - his(its) coordinates.
  [translated]
}
procedure TJvCustomEditorBase.CaretFromPos(Pos: Integer; var X, Y: Integer);
var
  I, Len, P: Integer;
begin
  X := -1;
  Y := -1;
  if Pos < 0 then
    Exit;
  P := 0;

  for I := 0 to LineCount - 1 do
  begin
    Len := LineLength[I];
    Inc(P, Len);
    if P >= Pos then
    begin
      Dec(P, Len);
      Y := I;
      X := Pos - P;
      Break;
    end;
    Inc(P, sLineBreakLen);
  end;
end;

function TJvCustomEditorBase.PosFromCaret(X, Y: Integer): Integer;
var
  I: Integer;
  Len: Integer;
begin
  if Cardinal(Y) >= Cardinal(LineCount) then
    Result := -1
  else
  begin
    Result := 0;
    for I := 0 to Y - 1 do
      Inc(Result, LineLength[I] + sLineBreakLen);
    Len := LineLength[Y];
    if X < Len then
      Inc(Result, X)
    else
      Inc(Result, Len);
  end;
end;

procedure TJvCustomEditorBase.PaintCaret(bShow: Boolean);
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

function TJvCustomEditorBase.GetTextLen: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to LineCount - 1 do
    Inc(Result, LineLength[I] + sLineBreakLen);
  Dec(Result, sLineBreakLen);
end;

procedure TJvCustomEditorBase.BeginUpdate;
begin
  LockUpdate;
end;

procedure TJvCustomEditorBase.EndUpdate;
begin
  Assert(FUpdateLock > 0); { Error }
  UnlockUpdate;
  if UpdateLock = 0 then
  begin
    FAllRepaint := True;
    UpdateEditorSize;
    StatusChanged;
    Invalidate;
  end;
end;

procedure TJvCustomEditorBase.MakeRowVisible(ARow: Integer);
begin
  if (ARow < FTopRow) or (ARow > FLastVisibleRow) then
  begin
    ARow := ARow - Trunc(VisibleRowCount / 2); {mac: bugfix - FCaretY}
    if ARow < 0 then
      ARow := 0;
    SetLeftTop(FLeftCol, ARow);
  end;
end;

procedure TJvCustomEditorBase.Command(ACommand: TEditCommand);
var
  X, Y: Integer;
  CaretUndo: Boolean;
  F: Integer;
  iBeg: Integer;
  BlockFormat: TJvSelBlockFormat;
type
  TPr = procedure of object;

  procedure DoAndCorrectXY(Pr: TPr);
  begin
    Pr;
    X := CaretX;
    Y := CaretY;
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

begin
  X := CaretX;
  Y := CaretY;
  CaretUndo := True;
  // LockUpdate;
  { macro recording }
  if Recording and not Com([ecRecordMacro, ecBeginCompound]) and (Compound = 0) then
    FMacro := FMacro + Char(Lo(ACommand)) + Char(Hi(ACommand));

  PaintCaret(False);
  try
    case ACommand of
      { caret movements }
      ecLeft, ecRight, ecSelLeft, ecSelRight:
        begin
          if Com([ecSelLeft, ecSelRight]) and IsNewSelection then
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
        if Com([ecUp, ecSelUp]) or (Y < Rows - 1) or CursorBeyondEOF then
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
      ecScrollLineUp, ecScrollLineDown:
        begin
          if not ((ACommand = ecScrollLineDown) and
            (Y >= LineCount - 1) and (Y = TopRow)) then
          begin
            if ACommand = ecScrollLineUp then
              F := -1
            else
              F := 1;
            FScrollBarVert.Position := FScrollBarVert.Position + F;
            Scroll(True, FScrollBarVert.Position);
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
          if Com([ecSelBeginLine, ecSelBeginDoc, ecSelEndLine, ecSelEndDoc]) and
            IsNewSelection then
            SetSel1(CaretX, Y);
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
            if Cardinal(Y) < Cardinal(LineCount) then
              X := LineLength[Y]
            else
              X := 0
          else
          if Com([ecEndDoc, ecSelEndDoc]) then
          begin
            Y := LineCount - 1;
            if Y >= 0 then
            begin
              X := LineLength[Y];
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
          FScrollBarVert.Position := FScrollBarVert.Position - FScrollBarVert.LargeChange;
          Scroll(True, FScrollBarVert.Position);
          Y := Y - FVisibleRowCount;
          PersistentBlocksSetUnSelected;
        end;
      ecNextPage:
        begin
          FScrollBarVert.Position := FScrollBarVert.Position + FScrollBarVert.LargeChange;
          Scroll(True, FScrollBarVert.Position);
          Y := Y + FVisibleRowCount;
          PersistentBlocksSetUnSelected;
        end;
      ecSelPrevPage:
        begin
          BeginUpdate;
          SetSel1(X, Y);
          FScrollBarVert.Position := FScrollBarVert.Position - FScrollBarVert.LargeChange;
          Scroll(True, FScrollBarVert.Position);
          Y := Y - FVisibleRowCount;
          SetSel1(X, Y);
          EndUpdate;
        end;
      ecSelNextPage:
        begin
          BeginUpdate;
          SetSel1(X, Y);
          FScrollBarVert.Position := FScrollBarVert.Position + FScrollBarVert.LargeChange;
          Scroll(True, FScrollBarVert.Position);
          Y := Y + FVisibleRowCount;
          if Y <= LineCount - 1 then
            SetSel1(X, Y)
          else
            SetSel1(X, LineCount - 1);
          EndUpdate;
        end;
      ecWindowTop:
        Y := FTopRow;
      ecWindowBottom:
        Y := FTopRow + FVisibleRowCount - 1;
      { editing }
      ecCharFirst..ecCharLast:
        if not FReadOnly then
        begin
          InsertChar(Word(ACommand - ecCharFirst));
          Exit;
        end;
      ecInsertPara:
        if not FReadOnly then
        begin
          InsertChar(13);
          Exit;
        end
        else
        if Y < LineCount - 1 then
        begin
          Inc(Y);
          if LineLength[Y] > 0 then
          begin
            iBeg := FindNotBlankCharPosInLine(Y) - 1;
            if iBeg < X then
              X := iBeg;
          end;
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
      ecUnindent:
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
          if FSelection.SelBlockFormat = TJvSelBlockFormat(ACommand - ecInclusiveBlock) then
            Exit;

          if FSelection.IsSelected then
          begin
           // convert line block to others and visi versa
            if ACommand <> ecLineBlock then
            begin
              if FSelection.SelBlockFormat = bfLine then
                AdjustSelLineMode({Restore:=}True);
            end
            else
              AdjustSelLineMode({Restore:=}False);
          end;

          FSelection.SelBlockFormat := TJvSelBlockFormat(ACommand - ecInclusiveBlock);
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
      ecSelAll:
        begin
          SelectAll;
          Exit;
        end;
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
        ChangeBookmark(ACommand - ecSetBookmark0, True);
      ecGotoBookmark0..ecGotoBookmark9:
        begin
          ChangeBookmark(ACommand - ecGotoBookmark0, False);
          X := CaretX;
          Y := CaretY;
        end;
      ecCompletionIdentifiers:
        if not FReadOnly then
        begin
          Completion.DoCompletion(cmIdentifiers);
          PaintCaret(True);
          Exit;
        end;
      ecCompletionTemplates:
        if not FReadOnly then
        begin
          Completion.DoCompletion(cmTemplates);
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
    else
      if DoCommand(ACommand, X, Y, CaretUndo) then
        Exit;
    end;

    if CaretUndo then
      SetCaret(X, Y)
    else
      SetCaretInternal(X, Y);
  finally
    // UnlockUpdate;
    PaintCaret(True);
  end;
end;

procedure TJvCustomEditorBase.PostCommand(ACommand: TEditCommand);
begin
  PostMessage(Handle, WM_EDITCOMMAND, ACommand, 0);
end;

procedure TJvCustomEditorBase.IndentLines(UnIndent: Boolean; BegY, EndY: Integer);
begin
  if UnIndent then
    UnIndentColumns(0, BegY, EndY)
  else
    IndentColumns(0, BegY, EndY);
end;

procedure TJvCustomEditorBase.IndentSelLines(UnIndent: Boolean);
var
  BegNotBlank, EndNotBlank: Integer;
  BegY, EndY: Integer;
begin
  with FSelection do
  begin
    if (not IsSelected) or (SelBlockFormat = bfColumn) then
      Exit;

    BegY := SelBegY;
    EndY := SelEndY;
    if SelEndX = 0 then
      Dec(EndY);
    if BegY > EndY then
      Exit;

    BegNotBlank := FindNotBlankCharPosInLine(BegY) - 1;
    EndNotBlank := FindNotBlankCharPosInLine(EndY) - 1;

    IndentLines(UnIndent, BegY, EndY);

    // to relative values
    BegNotBlank := (FindNotBlankCharPosInLine(BegY) - 1) - BegNotBlank;
    EndNotBlank := (FindNotBlankCharPosInLine(EndY) - 1) - EndNotBlank;

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
    if (CaretY = SelEndY) and (SelEndX > 0) then
      SetCaretInternal(CaretX + EndNotBlank, CaretY)
    else
    if CaretY = SelBegY then
      SetCaretInternal(CaretX + BegNotBlank, CaretY);

    SetSelUpdateRegion(BegY, EndY);
    PaintSelection;
  end;
end;

procedure TJvCustomEditorBase.BeginCompound;
begin
  Inc(FCompound);
  {--- UNDO ---}
  TJvBeginCompoundUndo.Create(Self);
  {--- /UNDO ---}
end;

procedure TJvCustomEditorBase.EndCompound;
begin
  {--- UNDO ---}
  TJvEndCompoundUndo.Create(Self);
  {--- /UNDO ---}
  Dec(FCompound);
end;

procedure TJvCustomEditorBase.SetBracketHighlighting(Value: TJvBracketHighlighting);
begin
  if Value <> BracketHighlighting then
    BracketHighlighting.Assign(Value);
end;

//=== { TJvEditorCompletionList } ============================================

constructor TJvEditorCompletionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Left := -1000;
  Visible := False;
  TabStop := False;
  ParentFont := False;
  Parent := Owner as TJvCustomEditorBase;
  Ctl3D := False;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
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

procedure TJvEditorCompletionList.MouseDown(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
var
  F: Integer;
begin
  MouseCapture := True;
  F := ItemAtPos(Point(X, Y), True);
  if F > -1 then
    ItemIndex := F;
end;

procedure TJvEditorCompletionList.MouseUp(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
  MouseCapture := False;
  (Owner as TJvCustomEditorBase).Completion.CloseUp(
    (Button = mbLeft) and PtInRect(ClientRect, Point(X, Y)));
end;

procedure TJvEditorCompletionList.OnTimer(Sender: TObject);
begin
  if YY < 0 then
    Perform(WM_VSCROLL, SB_LINEUP, 0)
  else
  if YY > ClientHeight then
    Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TJvEditorCompletionList.WMCancelMode(var Msg: TMessage);
begin
  (Owner as TJvCustomEditorBase).Completion.CloseUp(False);
end;

procedure TJvEditorCompletionList.CMHintShow(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TJvEditorCompletionList.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Offset, W: Integer;
  S: string;
begin
  // this is a ANSI component 
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else
  begin
    Canvas.FillRect(Rect);
    Offset := 3;
    with (Owner as TJvCustomEditorBase).Completion do
      case Mode of
        cmIdentifiers:
          TJvUnicodeCanvas(Canvas).TextOut(Rect.Left + Offset, Rect.Top, SubStr(Items[Index], 1,
            GetAnsiSeparator));
        cmTemplates:
          begin
            TJvUnicodeCanvas(Canvas).TextOut(Rect.Left + Offset, Rect.Top, SubStr(Items[Index], 1,
              GetAnsiSeparator));
            Canvas.Font.Style := [fsBold];
            S := SubStr(Items[Index], 0, GetAnsiSeparator);
            W := Canvas.TextWidth(S);
            TJvUnicodeCanvas(Canvas).TextOut(Rect.Right - 2 * Offset - W, Rect.Top, S);
          end;
      end;
  end;
end;

//=== { TJvEditorCompletion } ================================================

constructor TJvCompletionBase.Create(AJvEditor: TJvCustomEditorBase);
begin
  inherited Create;
  FJvEditor := AJvEditor;
  FPopupList := TJvEditorCompletionList.Create(FJvEditor);
  FItemHeight := FPopupList.ItemHeight;
  FDropDownCount := 6;
  FDropDownWidth := 300;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 800;
  FTimer.OnTimer := OnTimer;
  FDefMode := cmIdentifiers;
end;

destructor TJvCompletionBase.Destroy;
begin
  inherited Destroy;
  // (ahuser) is this necessary ?
  FPopupList.Free;
  FTimer.Free;
end;

procedure TJvCompletionBase.DoKeyPress(Key: Char);
begin
  if FVisible then
    if HasChar(Key, JvEditorCompletionChars) then
      SelectItem
    else
      CloseUp(True)
  else
  if FEnabled then
    FTimer.Enabled := True;
end;

function TJvCompletionBase.DoKeyDown(Key: Word; Shift: TShiftState): Boolean;
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

procedure TJvCompletionBase.DoCompletion(const AMode: TCompletionList);
var
  Eq: Boolean;
  Cancel: Boolean;
begin
  if FJvEditor.ReadOnly then
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
        // JvEditor.DoCompletionIdentifier(Cancel);
        FJvEditor.DoCompletionTemplate(Cancel);
        if Cancel or (GetTemplateCount = 0) then
          Exit;
        MakeItems;
        FindSelItem(Eq);
        if Eq then
          ReplaceWordItemIndex(2)
        else
          DropDown(AMode, True);
      end;
  end;
end;

procedure TJvCompletionBase.DropDown(const AMode: TCompletionList; const ShowAlways:
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
        DoCompletionIdentifier(Cancel);
      cmTemplates:
        DoCompletionTemplate(Cancel)
    end;
    MakeItems;
    FindSelItem(Eq);
    // Cancel := not Visible and (ItemIndex = -1);
    if Cancel or (Items.Count = 0) or (((ItemIndex = -1) or Eq) and not ShowAlways) then
      Exit;
    FPopupList.ItemHeight := FItemHeight;
    FVisible := True;
    SetItemIndex(FItemIndex);
    if FListBoxStyle in [lbStandard] then
      FPopupList.Style := lbOwnerDrawFixed
    else
      FPopupList.Style := FListBoxStyle;
    FPopupList.OnMeasureItem := FJvEditor.OnCompletionMeasureItem;
    FPopupList.OnDrawItem := FJvEditor.OnCompletionDrawItem;

    ItemCount := Items.Count;
    SysBorderWidth := GetSystemMetrics(SM_CXBORDER);
    SysBorderHeight := GetSystemMetrics(SM_CYBORDER);
    R := CalcCellRect(CaretX - LeftCol, CaretY - TopRow + 1);
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
      Y := P.Y - PopupHeight - CellRect.Height + 1;
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

procedure TJvCompletionBase.SelectItem;
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
  if Cancel or (GetItemCount = 0) then
    CloseUp(False);
end;

procedure TJvCompletionBase.CloseUp(const Apply: Boolean);
begin
  if not Visible then
    Exit;

  FItemIndex := ItemIndex;
  FPopupList.Visible := False;
  //  (FPopupList as TJvEditorCompletionList).HintWindow.ReleaseHandle;
  FVisible := False;
  FTimer.Enabled := False;
  if Apply and (ItemIndex > -1) then
    case FMode of
      cmIdentifiers:
        ReplaceWordItemIndex(0);
      cmTemplates:
        ReplaceWordItemIndex(2);
    end;
end;

procedure TJvCompletionBase.OnTimer(Sender: TObject);
begin
  DropDown(FDefMode, False);
end;

function TJvCompletionBase.GetItemIndex: Integer;
begin
  Result := FItemIndex;
  if FVisible then
    Result := FPopupList.ItemIndex;
end;

procedure TJvCompletionBase.SetItemIndex(AValue: Integer);
begin
  FItemIndex := AValue;
  if FVisible then
    FPopupList.ItemIndex := FItemIndex;
end;

function TJvCompletionBase.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TJvCompletionBase.SetInterval(AValue: Cardinal);
begin
  FTimer.Interval := AValue;
end;

function TJvCompletionBase.GetItemCount: Integer;
begin
  case FMode of
    cmIdentifiers:
      Result := GetIdentifierCount;
    cmTemplates:
      Result := GetTemplateCount;
  else
    Result := 0;
  end;
end;

function TJvCompletionBase.GetItems: TStrings;
begin
  Result := FPopupList.Items;
end;


//=== { TJvSymbolColor } =====================================================

constructor TJvSymbolColor.Create;
begin
  inherited Create;
  FStyle :=  [];
  FForeColor := clWindowText;
  FBackColor := clWindow;
end;

procedure TJvSymbolColor.SetColor(const ForeColor, BackColor: TColor; const Style: TFontStyles);
begin
  FForeColor := ForeColor;
  FBackColor := BackColor;
  FStyle := Style;
end;

procedure TJvSymbolColor.Assign(Source: TPersistent);
begin
  if Source is TJvSymbolColor then
  begin
    FForeColor := TJvSymbolColor(Source).FForeColor;
    FBackColor := TJvSymbolColor(Source).FBackColor;
    FStyle := TJvSymbolColor(Source).FStyle;
  end
  else
    inherited Assign(Source);
end;

//=== { TJvColors } ==========================================================

constructor TJvColors.Create;
begin
  inherited Create;
  FComment := TJvSymbolColor.Create;
  FNumber := TJvSymbolColor.Create;
  FString := TJvSymbolColor.Create;
  FSymbol := TJvSymbolColor.Create;
  FReserved := TJvSymbolColor.Create;
  FStatement := TJvSymbolColor.Create;
  FIdentifier := TJvSymbolColor.Create;
  FPreproc := TJvSymbolColor.Create;
  FFunctionCall := TJvSymbolColor.Create;
  FDeclaration := TJvSymbolColor.Create;
  FPlainText := TJvSymbolColor.Create;
  Preproc.SetColor(clGreen, clWindow, []);
  Comment.SetColor(clOlive, clWindow, [fsItalic]);
  Number.SetColor(clNavy, clWindow, []);
  Strings.SetColor(clPurple, clWindow, []);
  Symbol.SetColor(clBlue, clWindow, []);
  Reserved.SetColor(clWindowText, clWindow, [fsBold]);
  Statement.SetColor(clWindowText, clWindow, [fsBold]);
  Identifier.SetColor(clWindowText, clWindow, []);
  FunctionCall.SetColor(clWindowText, clWindow, []);
  Declaration.SetColor(clWindowText, clWindow, []);
  PlainText.SetColor(clWindowText, clWindow, []);
end;

destructor TJvColors.Destroy;
begin
  FComment.Free;
  FNumber.Free;
  FString.Free;
  FSymbol.Free;
  FReserved.Free;
  FStatement.Free;
  FIdentifier.Free;
  FPreproc.Free;
  FFunctionCall.Free;
  FDeclaration.Free;
  FPlainText.Free;
  inherited Destroy;
end;

procedure TJvColors.Assign(Source: TPersistent);
begin
  if Source is TJvColors then
  begin
    Comment.Assign(TJvColors(Source).Comment);
    Number.Assign(TJvColors(Source).Number);
    Strings.Assign(TJvColors(Source).Strings);
    Symbol.Assign(TJvColors(Source).Symbol);
    Reserved.Assign(TJvColors(Source).Reserved);
    Statement.Assign(TJvColors(Source).Statement);
    Identifier.Assign(TJvColors(Source).Identifier);
    Preproc.Assign(TJvColors(Source).Preproc);
    FunctionCall.Assign(TJvColors(Source).FunctionCall);
    Declaration.Assign(TJvColors(Source).Declaration);
    PlainText.Assign(TJvColors(Source).PlainText);
  end
  else
    inherited Assign(Source);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
