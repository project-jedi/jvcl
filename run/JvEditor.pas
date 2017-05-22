{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvEditor.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):
Burov Dmitry, translation of russian text.
Andreas Hausladen
Peter Th�rnqvist
Remko Bonte

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

component   : TJvEditor
description : 'Delphi IDE'-like Editor

Known Issues:
  Some russian comments were translated to english; these comments are marked
  with [translated]
-----------------------------------------------------------------------------}
// $Id$

unit JvEditor;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Controls,
  JvEditorCommon;

type
  TJvCustomEditor = class;

  TJvEditorStrings = class(TStringList)
  private
    FJvEditor: TJvCustomEditor;
    procedure StringsChanged(Sender: TObject);
    procedure SetInternal(Index: Integer; const Value: string);
    procedure ReLine;
    procedure SetLockText(const Text: string);
  protected
    procedure Put(Index: Integer; const S: string); override;
    procedure InternalPut(Index: Integer; const Value: string);

    property Internal[Index: Integer]: string write SetInternal;
    property JvEditor: TJvCustomEditor read FJvEditor;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AddStrings(Strings: TStrings); override;
    procedure SetTextStr(const Value: string); override;
    function Add(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Delete(Index: Integer); override;
    procedure DeleteText(BegX, BegY, EndX, EndY: Integer);
    procedure InsertText(X, Y: Integer; const Text: string);
    procedure DeleteColumnText(BegX, BegY, EndX, EndY: Integer);
    procedure InsertColumnText(X, Y: Integer; const Text: string);
  end;

  TJvCompletion = class;
  TOnCompletionApply = procedure(Sender: TObject; const OldString: string;
    var NewString: string) of object;
  TJvGetLineAttrEvent = procedure(Sender: TObject; var Line: string; Index: Integer;
    var Attrs: TLineAttrs) of object;

  TJvCustomEditor = class(TJvCustomEditorBase)
  private
    { internal objects }
    FLines: TJvEditorStrings;
    { events }
    FOnGetLineAttr: TJvGetLineAttrEvent;
    FOnCompletionApply: TOnCompletionApply;

    { get/set for properties }
    function GetLines: TStrings;
    procedure SetLines(ALines: TStrings);
    function GetCompletion: TJvCompletion;
    procedure SetCompletion(const Value: TJvCompletion);
  protected
    function GetLineCount: Integer; override;
    function GetLineLength(Index: Integer): Integer; override;
    function FindNotBlankCharPosInLine(Line: Integer): Integer; override;

    function GetTextLine(Y: Integer; out Text: string): Boolean; override;
    function InternGetWordOnCaret: string; override;

    procedure ReLine; override;
    function GetTabStop(X, Y: Integer; Next: Boolean): Integer; override;
    function GetBackStop(X, Y: Integer): Integer; override;
    procedure TextAllChangedInternal(Unselect: Boolean); override;
  protected
    procedure PaintLineText(Line: Integer; ColBeg, ColEnd: Integer;
      var ColPainted: Integer); override;
    procedure InsertChar(const Value: Word); override;
  protected
    procedure SetLockText(const Text: string);
    function ExpandTabs(const S: string): string;
    function GetAutoIndentStop(Y: Integer): Integer; override;

    { triggers for descendants }
    procedure GetLineAttr(var Str: string; Line, ColBeg, ColEnd: Integer); virtual;
    function DoCommand(ACommand: TEditCommand; var X, Y: Integer;
      var CaretUndo: Boolean): Boolean; override;
    { TextModified is called when the editor content has changed. }
    procedure TextModified(ACaretX, ACaretY: Integer; Action: TModifiedAction;
      const Text: string); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClipboardCopy; override;
    procedure ClipboardPaste; override;
    procedure DeleteSelected; override;
    procedure Clear;

    function GetSelText: string;
    procedure SetSelText(const AValue: string);
    function GetWordOnCaret: string;
    procedure SelectWordOnCaret; override;
    function GetText: string; override;

    procedure InsertText(const Text: string);
    procedure InsertColumnText(X, Y: Integer; const Text: string);
    procedure ReplaceWord(const NewString: string);
    procedure ReplaceWord2(const NewString: string);
    procedure IndentColumns(X: Integer; BegY, EndY: Integer); override;
    procedure UnIndentColumns(X: Integer; BegY, EndY: Integer); override;

    property SelText: string read GetSelText write SetSelText;
  public
    { published in descendants }
    property Lines: TStrings read GetLines write SetLines;
    property Completion: TJvCompletion read GetCompletion write SetCompletion;
    property OnGetLineAttr: TJvGetLineAttrEvent read FOnGetLineAttr write FOnGetLineAttr;
    property OnCompletionApply: TOnCompletionApply read FOnCompletionApply write FOnCompletionApply;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvEditor = class(TJvCustomEditor)
  published
    property BeepOnError;
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
    property Completion;
    property TabStops;
    property SmartTab;
    property BackSpaceUnindents;
    property AutoIndent;
    property KeepTrailingBlanks;
    property CursorBeyondEOF;
    property CursorBeyondEOL;
    property BracketHighlighting;
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
    property OnCaretChanged;
    property OnSelectionChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property OnPaintGutter;
    property OnGutterClick;
    property OnGutterDblClick;
    property OnCompletionIdentifier;
    property OnCompletionTemplate;
    property OnCompletionDrawItem;
    property OnCompletionMeasureItem;
    property OnCompletionApply;
    property OnLineInserted;
    property OnLineDeleted;

    { TControl }
    property DragMode;
    property DragKind;
    property DragCursor;
    property OnDragOver;
    property OnDragDrop;
    property OnStartDock;
    property OnStartDrag;
    property OnEndDock;
    property OnEndDrag;

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
    property UseDockManager;
    property DockSite;
    property ParentBiDiMode;
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnGetSiteInfo;
    property OnUnDock;
    property OnEnter;
    property OnExit;
  end;

  TJvCompletion = class(TJvCompletionBase)
  private
    FIdentifiers: TStringList;
    FTemplates: TStringList;
    FCaretChar: Char;
    FCRLF: string;
    FSeparator: string;
    function GetStrings(Index: Integer): TStrings;
    procedure SetStrings(Index: Integer; AValue: TStrings);
    procedure ReplaceWord(const NewString: string);
  protected
    procedure FindSelItem(var Eq: Boolean); override;
    procedure MakeItems; override;
    procedure ReplaceWordItemIndex(SubStrStart: Integer); override;
    function GetTemplateCount: Integer; override;
    function GetIdentifierCount: Integer; override;
    function GetSeparator: string; override;
  public
    constructor Create(AJvEditor: TJvCustomEditor);
    destructor Destroy; override;
  published
    property Identifiers: TStrings index 0 read GetStrings write SetStrings;
    property Templates: TStrings index 1 read GetStrings write SetStrings;
    property CaretChar: Char read FCaretChar write FCaretChar default '|';
    property CRLF: string read FCRLF write FCRLF;
    property Separator: string read FSeparator write FSeparator;
  end;

  TJvInsertUndo = class(TJvCaretUndo)
  private
    FText: string;
    function GetEditor: TJvCustomEditor;
  public
    constructor Create(AJvEditor: TJvCustomEditor; ACaretX, ACaretY: Integer;
      const AText: string);
    procedure Undo; override;
  end;

  TJvOverwriteUndo = class(TJvCaretUndo)
  private
    FOldText: string;
    FNewText: string;
    function GetEditor: TJvCustomEditor;
  public
    constructor Create(AJvEditor: TJvCustomEditor; ACaretX, ACaretY: Integer;
      const AOldText, ANewText: string);
    procedure Undo; override;
  end;

  TJvReLineUndo = class(TJvInsertUndo, IJvUndoCompound);

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
  private
    FLastLineDelete: Boolean;
  public
    constructor Create(AJvEditor: TJvCustomEditor; ACaretX, ACaretY: Integer;
      const AText: string; ALastLineDelete: Boolean);
    procedure Undo; override;
    //procedure Redo; override;
  end;

  TJvDeleteTrailUndo = class(TJvDeleteUndo, IJvUndoCompound);

  TJvBackspaceUndo = class(TJvDeleteUndo, IJvBackspaceUndo)
  public
    procedure Undo; override;
  end;

  TJvBackspaceUnindentUndo = class(TJvDeleteUndo, IJvBackspaceUnindentUndo)
  public
    procedure Undo; override;
  end;

  TJvReplaceUndo = class(TJvCaretUndo)
  private
    FBegX: Integer;
    FBegY: Integer;
    FText: string;
    FNewText: string;
    function GetEditor: TJvCustomEditor;
  public
    constructor Create(AJvEditor: TJvCustomEditor; ACaretX, ACaretY: Integer;
      ABegX, ABegY: Integer; const AText, ANewText: string);
    procedure Undo; override;
  end;

  TJvDeleteSelectedUndo = class(TJvDeleteUndo)
  public
    constructor Create(AJvEditor: TJvCustomEditor; ACaretX, ACaretY: Integer;
      const AText: string);
    procedure Undo; override;
  end;

(* // (ahuser) make Delphi 5 compiler happy
  TJvIndentColumnUndo = class(TJvInsertColumnUndo)
  private
    FNewCaretX: Integer;
    FNewCaretY: Integer;
  public
    constructor Create(AJvEditor: TJvCustomEditor; ACaretX, ACaretY: Integer;
      ABegX, ABegY: Integer; const AText: string);
    procedure Undo; override;
  end;
*)

  TJvUnindentColumnUndo = class(TJvInsertUndo)
  private
    FBegX: Integer;
    FBegY: Integer;
  public
    constructor Create(AJvEditor: TJvCustomEditor; ACaretX, ACaretY,
      ABegX, ABegY: Integer; const AText: string);
    procedure Undo; override;
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

uses
  SysUtils, Math, Graphics, Clipbrd,
  {$IFDEF UNICODE}
  Character,
  {$ENDIF UNICODE}
  JvUnicodeCanvas, JvJCLUtils, JvConsts, JvResources;

type
  TJvUndoBufferAccessProtected = class(TJvUndoBuffer);

//=== { TJvEditorStrings } ===================================================

constructor TJvEditorStrings.Create;
begin
  inherited Create;
  OnChange := StringsChanged;
end;

destructor TJvEditorStrings.Destroy;
begin
  OnChange := nil;
  OnChanging := nil;
  inherited Destroy;
end;

procedure TJvEditorStrings.Assign(Source: TPersistent);
begin
  JvEditor.BeginUpdate;
  try
    inherited Assign(Source);
    JvEditor.NotUndoable;
    JvEditor.TextAllChanged;
  finally
    JvEditor.EndUpdate;
  end;
end;

procedure TJvEditorStrings.AddStrings(Strings: TStrings);
begin
  JvEditor.BeginUpdate;
  try
    inherited AddStrings(Strings);
    JvEditor.NotUndoable;
  finally
    JvEditor.EndUpdate;
  end;
end;

procedure TJvEditorStrings.SetTextStr(const Value: string);
begin
  inherited SetTextStr(JvEditor.ExpandTabs(Value));
  if JvEditor.UpdateLock = 0 then
    JvEditor.NotUndoable;
  JvEditor.TextAllChanged;
end;

procedure TJvEditorStrings.StringsChanged(Sender: TObject);
begin
  if JvEditor.UpdateLock = 0 then
    JvEditor.TextAllChanged;
end;

procedure TJvEditorStrings.SetLockText(const Text: string);
begin
  JvEditor.LockUpdate;
  try
    inherited SetTextStr(Text)
  finally
    JvEditor.UnlockUpdate;
  end;
end;

procedure TJvEditorStrings.SetInternal(Index: Integer; const Value: string);
begin
  JvEditor.LockUpdate;
  try
    InternalPut(Index, Value);
  finally
    JvEditor.UnlockUpdate;
  end;
end;

function TJvEditorStrings.Add(const S: string): Integer;
begin
  Result := inherited Add(JvEditor.ExpandTabs(S));
end;

procedure TJvEditorStrings.Insert(Index: Integer; const S: string);
begin
  inherited Insert(Index, JvEditor.ExpandTabs(S));
  JvEditor.LineInserted(Index);
end;

procedure TJvEditorStrings.Delete(Index: Integer);
begin
  inherited Delete(Index);
  JvEditor.LineDeleted(Index);
end;

procedure TJvEditorStrings.Put(Index: Integer; const S: string);
var
  L: Integer;
begin
  if JvEditor.KeepTrailingBlanks then
    inherited Put(Index, S)
  else
  begin
    L := Length(S) - Length(TrimRight(S));
    if L = 0 then
      inherited Put(Index, S)
    else
    begin
      {--- UNDO ---}
      TJvDeleteTrailUndo.Create(JvEditor, Length(S), Index, Spaces(L));
      {--- /UNDO ---}
      inherited Put(Index, TrimRight(S));
    end;
  end;
end;

procedure TJvEditorStrings.ReLine;
var
  L: Integer;
  S: string;
  Y: Integer;
begin
  Y := JvEditor.CaretY; // save because Add('') changes CaretY
  JvEditor.LockUpdate;
  try
    BeginUpdate;
    try
      if Count = 0 then
        L := JvEditor.CaretX
      else
        L := Length(Strings[Count - 1]);
      while Y > Count - 1 do
      begin
        {--- UNDO ---}
        TJvReLineUndo.Create(JvEditor, L, JvEditor.CaretY, sLineBreakStr);
        {--- /UNDO ---}
        L := 0;
        Add('');
      end;
      JvEditor.CaretY := Y; // restore CaretY
      S := Strings[Y];
      if JvEditor.CaretX > Length(S) then
      begin
        L := JvEditor.CaretX - Length(S);
        {--- UNDO ---}
  //     TJvReLineUndo.Create(JvEditor, Length(S), Y, Spaces(L)); {disabled: moves the caret to wrong undo position }
        {--- /UNDO ---}
        inherited Put(Y, S + Spaces(L));
      end;
    finally
      EndUpdate;
    end;
  finally
    JvEditor.UnlockUpdate;
  end;
end;

procedure TJvEditorStrings.InternalPut(Index: Integer; const Value: string);
begin
  if JvEditor.KeepTrailingBlanks then
    inherited Put(Index, JvEditor.ExpandTabs(Value))
  else
    inherited Put(Index, TrimRight(JvEditor.ExpandTabs(Value)));
end;

{ delete text from [BegX..EndY] [BegY..EndY] all inclusive.
  BegX, EndX: [0..Max_X] }

procedure TJvEditorStrings.DeleteText(BegX, BegY, EndX, EndY: Integer);
var
  BegLine, EndLine: string;
  I, L: Integer;
begin
  if BegY < 0 then
  begin
    BegY := 0;
    BegX := 0;
  end;
  if BegY >= Count then
    Exit; // nothing to delete
  if EndY >= Count then
  begin
    EndY := Count - 1;
    EndX := MaxInt - 1;
  end;
  if BegX < 0 then
    BegX := 0;

  JvEditor.LockUpdate;
  BeginUpdate;
  try
    BegLine := Strings[BegY];
   // expand BegLine if necessary
    L := (BegX + 1) - Length(BegLine) - 1;
    if L > 0 then
      BegLine := BegLine + Spaces(L);

    EndLine := Strings[EndY];

    // delete lines between and end line
    for I := EndY downto BegY + 1 do
      Delete(I);

    System.Delete(BegLine, BegX + 1, MaxInt);
    System.Delete(EndLine, 1, EndX + 1);

    Internal[BegY] := BegLine + EndLine;
  finally
    EndUpdate;
    JvEditor.UnlockUpdate;
  end;
end;

{ insert text on X:[0..Max_X], Y }

procedure TJvEditorStrings.InsertText(X, Y: Integer; const Text: string);
var
  BegLine, EndLine: string;
  YStart: Integer;
  F, P: PChar;
  S, FirstLine: string;
  Len: Integer;
begin
  Inc(X); // increment for string functions
  if Y < 0 then
    Y := 0;
  while Y >= Count do
    Add('');

  BegLine := Strings[Y];
  EndLine := System.Copy(BegLine, X, MaxInt);
  System.Delete(BegLine, X, MaxInt);

  // line is too small -> expand it with spaces
  Len := Length(BegLine);
  if Len < X then
  begin
    SetLength(BegLine, X - 1);
    P := PChar(BegLine) + Len;
    Len := X - Len - 1;
    while Len > 0 do
    begin
      P^ := ' ';
      Inc(P);
      Dec(Len);
    end;
  end;

  JvEditor.LockUpdate;
  BeginUpdate;
  try
    P := PChar(Text);
    F := P;
    while (P[0] <> #0) and (P[0] <> Lf) and (P[0] <> Cr) do
      Inc(P);

    SetString(S, F, P - F);

    YStart := Y;
    FirstLine := BegLine + S; // set Internal[YStart] later so we keep the trailing spaces for concat EndLine

    while P[0] <> #0 do
    begin
      if P[0] = Cr then
        Inc(P);
      if P[0] = Lf then
        Inc(P);
      F := P;

      while (P[0] <> #0) and (P[0] <> Lf) and (P[0] <> Cr) do
        Inc(P);
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
    JvEditor.UnlockUpdate;
  end;
end;

{ delete column text from [BegX..EndY] [BegY..EndY] all inclusive.
  BegX, EndX: [0..Max_X] }

procedure TJvEditorStrings.DeleteColumnText(BegX, BegY, EndX, EndY: Integer);
var
  S: string;
  I: Integer;
begin
  if BegY < 0 then
  begin
    BegY := 0;
    BegX := 0;
  end;
  if BegY >= Count then
    Exit; // nothing to delete
  if EndY >= Count then
  begin
    EndY := Count - 1;
    EndX := MaxInt - 1;
  end;
  if BegX < 0 then
    BegX := 0;

  JvEditor.LockUpdate;
  BeginUpdate;
  try
    for I := BegY to EndY do
    begin
      S := JvEditor.FLines[I];
      System.Delete(S, BegX + 1, EndX - BegX + 1);
      JvEditor.FLines.Internal[I] := S;
    end;
  finally
    EndUpdate;
    JvEditor.UnlockUpdate;
  end;
end;

{ insert column text on X:[0..Max_X], Y }

procedure TJvEditorStrings.InsertColumnText(X, Y: Integer; const Text: string);
var
  S, Line: string;
  P, F: PChar;
  L: Integer;
begin
  Inc(X); // increment for string functions
  if Y < 0 then
    Y := 0;

  JvEditor.LockUpdate;
  BeginUpdate;
  try
    P := PChar(Text);
    F := P;
    while P[0] <> #0 do
    begin
      while (P[0] <> #0) and (P[0] <> Lf) and (P[0] <> Cr) do
        Inc(P);
      SetString(S, F, P - F);

      while Y >= Count do
        Add('');
      Line := Strings[Y];
      L := (X - 1) - Length(Line);
      if L > 0 then
        Line := Line + Spaces(L);
      System.Insert(S, Line, X);
      Internal[Y] := Line;

      if P[0] = Cr then
        Inc(P);
      if P[0] = Lf then
        Inc(P);
      F := P;
      Inc(Y);
    end;
  finally
    EndUpdate;
    JvEditor.UnlockUpdate;
  end;
end;

//=== { TJvCustomEditor } ====================================================

constructor TJvCustomEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLines := TJvEditorStrings.Create;
  FLines.FJvEditor := Self;
  FLines.OnChange := DoLinesChange;
  Completion := TJvCompletion.Create(Self);
end;

destructor TJvCustomEditor.Destroy;
begin
  FLines.Free;
  Completion.Free;
  inherited Destroy;
end;

procedure TJvCustomEditor.PaintLineText(Line: Integer; ColBeg, ColEnd: Integer;
  var ColPainted: Integer);
var
  Ch: string;
  iC, jC, SL, MX: Integer;
  R: TRect;
  S: string;
  LA: TLineAttr;
  jCStart, Len: Integer;
  MyDi: TDynIntArray;
  ColCount: Integer;
  {$IFDEF COMPILER12_UP}
  CharSize: Integer;
  {$ENDIF COMPILER12_UP}
begin
  with EditorClient do
  begin
    S := FLines[Line];

    Len := Max(Length(S), Max_X) + 1;
    if Len > Length(LineAttrs) then
      SetLength(LineAttrs, Len)
    else
    if Len + 128 < Length(LineAttrs) then
      SetLength(LineAttrs, Len);

    GetLineAttr(S, Line, ColBeg, ColEnd);

    {left line}
    if Canvas.Brush.Color <> LineAttrs[LeftCol + 1].BC then // change GDI object only if necessary
      Canvas.Brush.Color := LineAttrs[LeftCol + 1].BC;

    Canvas.FillRect(Bounds(EditorClient.Left, (Line - TopRow) *
      CellRect.Height, 1, CellRect.Height));
    {optimized, paint group of chars with identical attributes}
    SL := Length(S);
    MX := ColEnd;

    {TODO: This code hasn't any effect anymore because in the inner loop MyDi[] is changed. Rethink the whole implementation.}
    if Length(FMyDi) < MX then
    begin
      SetLength(MyDi, MX);
      for iC := 0 to High(MyDi) do
        MyDi[iC] := CellRect.Width;
    end
    else
      MyDi := FMyDi;

    while ColPainted < MX do
    begin
      with Canvas do
      begin
        iC := ColPainted + 1;
        LA := LineAttrs[iC];
        jC := iC + 1;
        if iC <= SL then
          Ch := S[iC]
        else
          Ch := ' ';
        jCStart := jC;
        while (jC <= MX + 1) and
          CompareMem(@LA, @LineAttrs[jC], SizeOf(LineAttrs[1])) do
            Inc(jC);
        Ch := Copy(S, jCStart - 1, jC - jCStart + 1);
        if jC > SL + 1 then
          Ch := Ch + Spaces(jC - SL - 1);
        Len := Length(Ch);

        if Brush.Color <> LA.BC then // change GDI object only if necessary
          Brush.Color := LA.BC;
        Font.Assign(FontCacheFind(LA));

        ColCount := 0;
        {$IFDEF COMPILER12_UP}
        for iC := 0 to High(MyDi) - 1 do
        begin
          {TODO: a cache for the TextWidth() call should be used and cleared if the font name changes. }
          if (iC < Len) and (Ch[iC + 1] >= #256) then
          begin
            CharSize := ((EditorClient.Canvas.TextWidth(Ch[iC + 1]) + (CellRect.Width - 1)) div CellRect.Width);
            MyDi[iC] := CellRect.Width * CharSize;
            Inc(ColCount, CharSize - 1);
          end
          else
            MyDi[iC] := CellRect.Width;
        end;
        {$ENDIF COMPILER12_UP}
        Inc(ColCount, Len);


        R := CalcCellRect(ColPainted - LeftCol, Line - TopRow);
        {bottom line}
        FillRect(Bounds(R.Left, R.Bottom - 1, CellRect.Width * ColCount, 1));

        TJvUnicodeCanvas(Canvas).ExtTextOut(R.Left, R.Top, [etoOpaque, etoClipped], nil, Ch, @MyDi[0]);
        ErrorHighlighting.PaintError(Canvas, ColPainted, Line, R, ColCount, MyDi);

        if LA.Border <> clNone then
        begin
          Pen.Color := LA.Border;
          R.Right := R.Left + CellRect.Width * ColCount;
          Dec(R.Left);
          Brush.Style := bsClear;
          Rectangle(R);
          Brush.Style := bsSolid;
        end;

        ColPainted := jC - 1;
      end;
    end;
  end;
end;

procedure TJvCustomEditor.GetLineAttr(var Str: string; Line, ColBeg, ColEnd: Integer);
var
  I: Integer;
begin
  if ColBeg < 0 then
    ColBeg := 0;
  if ColEnd > Max_X then
    ColEnd := Max_X;

  SetLength(LineAttrs, Max(Length(LineAttrs), Max_X + 1));

  LineAttrs[ColBeg].Style := Font.Style;
  LineAttrs[ColBeg].FC := Font.Color;
  LineAttrs[ColBeg].BC := Color;
  LineAttrs[ColBeg].Border := clNone;

{  for I := ColBeg + 1 to ColEnd do
    Move(LineAttrs[ColBeg], LineAttrs[I], SizeOf(LineAttrs[1]));}
  for I := ColBeg + 1 to ColEnd do
    LineAttrs[I] := LineAttrs[ColBeg];

  GetAttr(Line, ColBeg, ColEnd);
  if Assigned(FOnGetLineAttr) then
    FOnGetLineAttr(Self, Str, Line, LineAttrs);
  ChangeAttr(Line, ColBeg, ColEnd);
end;

function TJvCustomEditor.GetTextLine(Y: Integer; out Text: string): Boolean;
begin
  if (Y >= 0) and (Y < Lines.Count) then
  begin
    Text := Lines[Y];
    Result := True;
  end
  else
  begin
    Text := '';
    Result := False;
  end;
end;

function TJvCustomEditor.InternGetWordOnCaret: string;
begin
  Result := GetWordOnCaret;
end;

procedure TJvCustomEditor.ReLine;
begin
  FLines.ReLine;
end;

procedure TJvCustomEditor.InsertChar(const Value: Word);
var
  S: string;
  X, Y, iBeg: Integer;
  WasSelected: Boolean;
  Key: Char;
begin
  Key := Char(Value);
  WasSelected := (FSelection.IsSelected) and (not PersistentBlocks);
  {$IFDEF UNICODE}
  if (Key >= #32) and ((Key <= #$FF) or not TCharacter.IsControl(Char(Value))) then
  {$ELSE}
  if CharInSet(Key, [#32..#255]) then
  {$ENDIF UNICODE}
  begin
    if not HasChar(Key, JvEditorCompletionChars) then
      Completion.DoKeyPress(Key);

    RemoveSelectedBlock;

    ReLine; // need ReLine after DeleteSelection
    S := FLines[CaretY];
    if InsertMode then
    begin
      {--- UNDO ---}
      TJvInsertUndo.Create(Self, CaretX, CaretY, Key);
      {--- /UNDO ---}
      Insert(Key, S, CaretX + 1);

      AdjustPersistentBlockSelection(CaretX, CaretY, amInsert, [1]);
    end
    else
    begin
      {--- UNDO ---}
      if CaretX + 1 <= Length(S) then
        TJvOverwriteUndo.Create(Self, CaretX, CaretY, S[CaretX + 1], Key)
      else
        TJvOverwriteUndo.Create(Self, CaretX, CaretY, '', Key);
      {--- /UNDO ---}
      if CaretX + 1 <= Length(S) then
        S[CaretX + 1] := Key
      else
        S := S + Key
    end;
    FLines.Internal[CaretY] := S;
    SetCaretInternal(CaretX + 1, CaretY);
    TextModified(CaretX, CaretY, maInsert, Key);
    PaintLine(CaretY, -1, -1);
    Changed;

    if HasChar(Key, JvEditorCompletionChars) then
      Completion.DoKeyPress(Key);
  end
  else
  case Key of
    Cr:
      begin
        if InsertMode then
        begin
          if WasSelected then // compound only on selection deletion
            BeginCompound;
          LockUpdate;
          try
            RemoveSelectedBlock; // adjusts CaretX, CaretY
            X := CaretX;
            Y := CaretY;
            { --- UNDO --- }
            TJvInsertUndo.Create(Self, CaretX, CaretY, sLineBreakStr);
            { --- /UNDO --- }
            if FLines.Count = 0 then
              FLines.Add('');
            ReLine;

            S := Copy(FLines[Y], X + 1, MaxInt);
            FLines.Insert(Y + 1, S);
            FLines.Internal[Y] := Copy(FLines[Y], 1, X);
            Inc(Y);
            { auto indent }
            if AutoIndent and
              (((Length(FLines[CaretY]) > 0) and
              (FLines[CaretY][1] = ' ')) or
              ((Trim(FLines[CaretY]) = '') and (X > 0))) then
            begin
              X := GetAutoIndentStop(Y);
              if X > 0 then
              begin
                { --- UNDO --- }
                TJvInsertUndo.Create(Self, 0, Y, Spaces(X));
                { --- /UNDO --- }
                FLines.Internal[Y] := Spaces(X) + FLines[Y];
              end;
            end
            else
              X := 0;

            // persistent blocks: adjust selection
            AdjustPersistentBlockSelection(CaretX, CaretY, amLineBreak, []);

            UpdateEditorSize;
            TextModified(CaretX - 1, CaretY, maInsert, sLineBreakStr);
          finally
            UnlockUpdate;
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
            X := CaretX;
            Y := CaretY;
            Inc(Y);
            if Y >= FLines.Count then
            begin
              LockUpdate;
              try
                { --- UNDO --- }
                TJvInsertUndo.Create(Self, CaretX, CaretY, sLineBreakStr);
                { --- /UNDO --- }
                FLines.Add('');
              finally
                UnlockUpdate;
              end;
              TextModified(0, Y - 1, maInsert, sLineBreakStr);
              UpdateEditorSize;
              Invalidate;
              Changed;
            end;
            if Y < FLines.Count then
            begin
              S := FLines[Y];
              if Length(S) > 0 then
              begin
                iBeg := FindNotBlankCharPos(S) - 1;
                if iBeg < X then
                  X := iBeg;
              end;
            end;
          finally
            if WasSelected then
              EndCompound;
          end;
        end;
        SetCaretInternal(X, Y);
      end;
  end;
end;

procedure TJvCustomEditor.SelectWordOnCaret;
var
  iBeg, iEnd: Integer;
begin
  if (CaretY >= 0) and (CaretY < LineCount) and (Trim(FLines[CaretY]) <> '') then
  begin
    iEnd := Length(TrimRight(FLines[CaretY]));
    if FCaretX < iEnd then
      while FLines[FCaretY][FCaretX + 1] <= ' ' do
        Inc(FCaretX)
    else
    begin
      FCaretX := iEnd - 1;
      while FLines[FCaretY][FCaretX + 1] <= ' ' do
        Dec(FCaretX);
    end;
    if GetWordOnPosEx(FLines[FCaretY] + ' ', FCaretX + 1, iBeg, iEnd) <> '' then
    begin
      PaintCaret(False);
      SetSel(iBeg - 1, FCaretY);
      SetSel(iEnd - 1, FCaretY);
      SetCaret(iEnd - 1, FCaretY);
      PaintCaret(True);
    end;
  end;
end;

function TJvCustomEditor.DoCommand(ACommand: TEditCommand; var X, Y: Integer;
  var CaretUndo: Boolean): Boolean;

type
  TPr = procedure of object;

  procedure DoAndCorrectXY(Pr: TPr);
  begin
    Pr;
    X := CaretX;
    Y := CaretY;
    CaretUndo := False;
  end;

  procedure SetSel1(X, Y: Integer);
  begin
    SetSel(X, Y);
    CaretUndo := False;
  end;

  procedure SetSelText1(const S: string);
  begin
    SelText := S;
    CaretUndo := False;
  end;

var
  F: Integer;
  S, S2: string;
  B: Boolean;
  iBeg, iEnd: Integer;
begin
  Result := True;
  X := CaretX;
  Y := CaretY;
  case ACommand of
    { caret movements }
    ecPrevWord, ecSelPrevWord, ecBackspaceWord:
      begin
        if (ACommand = ecSelPrevWord) and IsNewSelection then
          SetSel1(CaretX, CaretY);
        if Y >= FLines.Count then
          Exit;

        S := FLines[Y];
        B := False;
        if CaretX > Length(S) then
        begin
          X := Length(S);
          SetSel1(X, Y);
        end
        else
        begin
          for F := X - 1 downto 0 do
          begin
            if B then
            begin
              if CharInSet(AnsiChar(S[F + 1]), Separators) then
              begin
                X := F + 1;
                Break;
              end;
            end
            else
            if not CharInSet(AnsiChar(S[F + 1]), Separators) then
              B := True;
          end;

          if X = CaretX then
            X := 0;

          if ACommand <> ecBackspaceWord then
          begin
            { Jump to previous line and last word ending }
            if (X = 0) and (Y > 0) then
            begin
              if (Y > FLines.Count) or (CaretX = 0) or (FLines[Y] = '') or
                 CharInSet(AnsiChar(FLines[Y][1]), Separators) then
              begin
                Y := Y - 1;
                X := Length(FLines[Y]);
              end;
            end;
          end;

          if ACommand = ecSelPrevWord then
            SetSel1(X, Y)
          else
            PersistentBlocksSetUnSelected;

          if (ACommand = ecBackspaceWord) and (Y >= 0) and (X <> CaretX) then
          begin
            if not ReadOnly then
            begin
              BeginCompound;
              try
                SelectRange(X, CaretY, CaretX, CaretY);
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
          SetSel1(CaretX, CaretY);
        if Y >= FLines.Count then
        begin
          Y := FLines.Count - 1;
          if Y < 0 then
            Exit;
          X := Length(FLines[Y]);
        end;
        S := FLines[Y];
        B := False;
        if CaretX >= Length(S) then
        begin
          if Y < FLines.Count - 1 then
          begin
            Y := CaretY + 1;
            X := 0;
            if Y < FLines.Count then
              while (X < Length(FLines[Y])) and (CharInSet(AnsiChar(FLines[Y][X + 1]), Separators)) do
                Inc(X);

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
              if not CharInSet(AnsiChar(S[F + 1]), Separators) then
              begin
                X := F;
                Break;
              end
            end
            else
            if CharInSet(AnsiChar(S[F + 1]), Separators) then
              B := True;
          if X = CaretX then
            X := Length(S);
          if ACommand = ecSelNextWord then
            SetSel1(X, Y)
          else
            PersistentBlocksSetUnSelected;
        end;
      end;
    ecSelWord:
      if IsNewSelection and (GetWordOnPosEx(FLines[Y] + ' ', X + 1, iBeg,
        iEnd) <> '') then
      begin
        SetSel1(iBeg - 1, Y);
        SetSel1(iEnd - 1, Y);
        X := iEnd - 1;
      end;
    ecBackspace:
      if not ReadOnly then
        if X > 0 then
        begin
          // in the middle of line
          if not PersistentBlocks and FSelection.IsSelected then
            DoAndCorrectXY(RemoveSelectedBlock)
          else
          begin
            ReLine;
            if BackSpaceUnindents then
              X := GetBackStop(CaretX, CaretY)
            else
              X := CaretX - 1;

            S := Copy(FLines[CaretY], X + 1, CaretX - X);

            { --- UNDO --- }
            if X = CaretX - 1 then
              TJvBackspaceUndo.Create(Self, CaretX, CaretY, S)
            else
              TJvBackspaceUnindentUndo.Create(Self, CaretX, CaretY, S);
            CaretUndo := False;
            { --- /UNDO --- }

            // persistent blocks: adjust selection
            AdjustPersistentBlockSelection(CaretX, CaretY, amDelete, [1]);

            FLines.DeleteText(X, Y, CaretX - 1, Y);

            TextModified(CaretX, CaretY, maDelete, S);
            PaintLine(Y, -1, -1);
          end;
          Changed;
        end
        else
        if Y > 0 then
        begin
          // at the start of line
          if FSelection.IsSelected then
          begin
            BeginCompound;
            try
              DoAndCorrectXY(RemoveSelectedBlock);
              ReLine;
            finally
              EndCompound;
            end;
          end
          else
          begin
            LockUpdate;
            try
              X := Length(FLines[Y - 1]);

              { --- UNDO --- }
              TJvBackspaceUndo.Create(Self, X + 1, CaretY - 1, Lf);
              CaretUndo := False;
              { --- /UNDO --- }

             // persistent blocks: adjust selection
              AdjustPersistentBlockSelection(CaretX, CaretY, amLineConcat, [X, CaretY - 1]);

              FLines.DeleteText(X, Y - 1, -1, Y);
              Dec(Y);
            finally
              UnlockUpdate;
            end;
            UpdateEditorSize;
            TextModified(X, Y, maDelete, sLineBreakStr);
            Invalidate;
            Changed;
          end;
        end
        else
        if not PersistentBlocks and FSelection.IsSelected then
          DoCommand(ecDelete, X, Y, CaretUndo);
    ecDelete:
      if not ReadOnly then
      begin
        LockUpdate;
        try
          if FLines.Count = 0 then
            FLines.Add('');
        finally
          UnlockUpdate;
        end;
        if not PersistentBlocks and FSelection.IsSelected then
          DoAndCorrectXY(RemoveSelectedBlock)
        else
        if X < Length(FLines[Y]) then
        begin
          //{ inside line - � �������� ������}
          { --- UNDO --- }
          TJvDeleteUndo.Create(Self, CaretX, CaretY, FLines[Y][X + 1]);
          CaretUndo := False;
          { --- /UNDO --- }

          // persistent blocks: adjust selection (before DeleteText)
          AdjustPersistentBlockSelection(CaretX + 1, CaretY, amDelete, [1]);

          S := FLines[Y][X + 1];
          FLines.DeleteText(X, Y, X, Y);

          TextModified(CaretX, CaretY, maDelete, S);
          PaintLine(CaretY, -1, -1);
          Changed;
        end
        else
        if (Y >= 0) and (Y <= FLines.Count - 2) then
        begin
          //{ at the end of line - � ����� ������}
          { --- UNDO --- }
          TJvDeleteUndo.Create(Self, CaretX, CaretY, sLineBreakStr);
          CaretUndo := False;
          { --- /UNDO --- }
          // persistent blocks: adjust selection (before DeleteText)
          AdjustPersistentBlockSelection(0, CaretY + 1, amLineConcat, [CaretX, CaretY]);

          FLines.DeleteText(X, Y, -1, Y + 1);

          UpdateEditorSize;
          TextModified(CaretX, CaretY, maDelete, sLineBreakStr);
          Invalidate;
          Changed;
        end;
      end;
    ecTab, ecBackTab:
      begin
        X := GetTabStop(CaretX, CaretY, ACommand = ecTab);
        if not ReadOnly then
        begin
          if FSelection.IsSelected then
            if (ACommand = ecTab) and InsertMode then
              DeleteSelected;
          ReLine;
          if (ACommand = ecTab) and InsertMode then
          begin
            S := FLines[CaretY];
            S2 := Spaces(X - CaretX);
            { --- UNDO --- }
            TJvInsertTabUndo.Create(Self, CaretX, CaretY, S2);
            CaretUndo := False;
            { --- /UNDO --- }
            FLines.InsertText(CaretX, CaretY, S2);

            TextModified(CaretX, CaretY, maInsert, S2);
            PaintLine(CaretY, -1, -1);
            Changed;
          end;
        end;
        { else }
        { move cursor - oh yes!, it's already moved: X := GetTabStop(..); }
      end;
    ecDeleteLine:
      if not ReadOnly then
      begin
        if (CaretY >= 0) and (CaretY < FLines.Count) then
        begin
          S := FLines[CaretY];
          if (CaretY >= FLines.Count - 1) and (S = '') then
            Exit;

          LockUpdate;
          try
            { --- UNDO --- }
            TJvDeleteLineUndo.Create(Self, CaretX, CaretY, S, CaretY >= FLines.Count - 1);
            { --- /UNDO --- }
            if CaretY < FLines.Count - 1 then
              FLines.Delete(CaretY)
            else
              FLines[CaretY] := '';
            SetCaretInternal(0, CaretY); // set caret to 0/Y when in last line
          finally
            UnlockUpdate;
          end;
          AdjustPersistentBlockSelection(CaretX, CaretY, amDeleteLine, []);
          TextModified(0, CaretY, maDelete, S);
          Invalidate;
          Changed;
        end;
        Exit;
      end;
    ecToUpperCase:
      if not ReadOnly then
        SelText := AnsiUpperCase(SelText);
    ecToLowerCase:
      if not ReadOnly then
        SelText := AnsiLowerCase(SelText);
    ecChangeCase:
      if not ReadOnly then
        SelText := AnsiChangeCase(SelText);
  end;
  Result := False;
end;

function TJvCustomEditor.GetSelText: string;
var
  S: string;
  I: Integer;
  Len, CLen: Integer;
  P: PChar;
begin
  with FSelection do
  begin
    Len := GetSelLength; // memory size to allocate
    Result := '';
    if Len = 0 then
      Exit;
    SetLength(Result, Len);

    if SelBlockFormat = bfColumn then
    begin
      if Len > 0 then
      begin
        P := Pointer(Result);
        for I := SelBegY to SelEndY do
        begin
          S := FLines[I];
          CLen := Length(S) - SelBegX;
          if CLen < 0 then
            CLen := 0;
          if CLen > SelEndX - SelBegX + 1 then
            CLen := SelEndX - SelBegX + 1;
          if CLen <> 0 then
          begin
            Move(S[SelBegX + 1], P^, CLen * SizeOf(Char));
            Inc(P, CLen);
          end;

          if I < SelEndY then
          begin
            Move(sLineBreakStr[1], P^, sLineBreakLen * SizeOf(Char));
            Inc(P, sLineBreakLen);
          end;
        end;
      end;
    end
    else
    begin
      if SelBegY = SelEndY then
        Move(FLines[SelEndY][SelBegX + 1], Result[1], Len * SizeOf(Char))
      else
      begin
        P := PChar(Result);

        // first line
        S := FLines[SelBegY];
        CLen := Length(S) - SelBegX;
        if CLen > 0 then
        begin
          Move(S[SelBegX + 1], P^, CLen * SizeOf(Char));
          Inc(P, CLen);
        end;

        // line break
        Move(sLineBreakStr[1], P^, sLineBreakLen * SizeOf(Char));
        Inc(P, sLineBreakLen);

        // lines between
        for I := SelBegY + 1 to SelEndY - 1 do
        begin
          // line
          S := FLines[I];
          Move(S[1], P^, Length(S) * SizeOf(Char));
          Inc(P, Length(S));

          // line break
          Move(sLineBreakStr[1], P^, sLineBreakLen * SizeOf(Char));
          Inc(P, sLineBreakLen);
        end;

        // last line
        S := FLines[SelEndY];
        CLen := SelEndX + Ord(SelBlockFormat = bfInclusive);
        if CLen > Length(S) then
          CLen := Length(S);
        if CLen > 0 then
          Move(S[1], P^, CLen * SizeOf(Char));
      end;
    end;
  end;
end;

procedure TJvCustomEditor.SetSelText(const AValue: string);
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
        SelBegX := CaretX;
        SelBegY := CaretY;
      end;
      if FSelection.SelBlockFormat = bfColumn then
        InsertColumnText(FSelection.SelBegX, FSelection.SelBegY, AValue)
      else
        InsertText(AValue);

      IsSelected := Length(AValue) > 0;
      Selecting := False;
      GetEndPosCaret(AValue, SelBegX, SelBegY, SelEndX, SelEndY);
      if IsSelected then
        Inc(SelEndX);
      SetSelUpdateRegion(SelBegY, SelEndY);
    end;
  finally
    EndCompound;
    EndUpdate;
  end;
end;

function TJvCustomEditor.GetText: string;
begin
  Result := FLines.Text;
end;

procedure TJvCustomEditor.ClipboardCopy;
var
  S: string;
begin
  S := GetSelText;
  Clipboard.SetTextBuf(PChar(S));
  SetClipboardBlockFormat(SelBlockFormat);
end;

procedure TJvCustomEditor.InsertText(const Text: string);
var
  X, Y: Integer;
begin
  PaintCaret(False);
  try
    { --- UNDO --- }
    TJvInsertUndo.Create(Self, CaretX, CaretY, Text);
    { --- /UNDO --- }
    FLines.InsertText(CaretX, CaretY, Text);
    TextModified(CaretX, CaretY, maInsert, Text);

    GetEndPosCaret(Text, CaretX, CaretY, X, Y); // get new caret position
    SetCaretInternal(X + 1, Y);

    Changed;
  finally
    PaintCaret(True);
  end;
end;

procedure TJvCustomEditor.InsertColumnText(X, Y: Integer; const Text: string);
begin
  if X < 0 then
    X := 0;
  if Y < 0 then
    Y := 0;
  { --- UNDO --- }
  TJvInsertColumnUndo.Create(Self, X, Y, Text);
  { --- /UNDO --- }
  FLines.InsertColumnText(X, Y, Text);
  TextModified(X, Y, maInsertColumn, Text);
end;

// Substitutes a word in a cursor position on NewString
// string NewString should not contain Cr, Lf [translated]

procedure TJvCustomEditor.ReplaceWord(const NewString: string);
var
  iBeg, iEnd: Integer;
  S, W: string;
  X: Integer;
begin
  BeginUpdate;
  PaintCaret(False);
  try
    S := FLines[CaretY];
    while CaretX > Length(S) do
      S := S + ' ';
    W := Trim(GetWordOnPos2(S, CaretX, iBeg, iEnd));
    if W = '' then
    begin
      iBeg := CaretX + 1;
      iEnd := CaretX
    end;
    { --- UNDO --- }
    NotUndoable;
    //TJvReplaceUndo.Create(Self, CaretX, CaretY, iBegSX - 1, CaretY, W, NewString);
    { --- /UNDO --- }
    Delete(S, iBeg, iEnd - iBeg);
    Insert(NewString, S, iBeg);
    FLines.Internal[CaretY] := S;
    X := iBeg + Length(NewString) - 1;
    TextModified(CaretX, CaretY, maInsert, NewString);
    PaintLine(CaretY, -1, -1);
    SetCaretInternal(X, CaretY);
    Changed;
  finally
    PaintCaret(True);
    EndUpdate;
  end;
end;

{ Substitutes a word on the cursor position by NewString [translated] }

procedure TJvCustomEditor.ReplaceWord2(const NewString: string);
var
  S, W: string;
  iBegSX, iEndSX: Integer; { [1..Length] }
  X, Y: Integer;
begin
  S := '';
  if CaretY < FLines.Count then
    S := FLines[CaretY];

  W := Trim(GetWordOnPosEx(S, CaretX + 1, iBegSX, iEndSX));
  if W <> NewString then
  begin
    PaintCaret(False);
    try
      BeginCompound;
      try
        ReLine;
        if Length(W) = 0 then
        begin
          iBegSX := CaretX + 1;
          iEndSX := CaretX;
        end;
        { --- UNDO --- }
        TJvReplaceUndo.Create(Self, CaretX, CaretY, iBegSX - 1, CaretY, W, NewString);
        { --- /UNDO --- }

        if iBegSX <= iEndSX then
          FLines.DeleteText(iBegSX - 1, CaretY, iEndSX - 1, CaretY);
        FLines.InsertText(iBegSX - 1, CaretY, NewString);
        TextModified(iBegSX - 1, CaretY, maReplace, NewString);

        GetEndPosCaret(NewString, iBegSX - 1, CaretY, X, Y); // get end caret position
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

procedure TJvCustomEditor.IndentColumns(X, BegY, EndY: Integer);
var
  Y: Integer;
  S: string;
begin
  if BegY < 0 then
    BegY := 0;
  if BegY >= FLines.Count then
    BegY := FLines.Count - 1;
  if EndY < 0 then
    EndY := 0;
  if EndY >= FLines.Count then
    EndY := FLines.Count - 1;
  if EndY < BegY then
    Exit;
  if X < 0 then
    X := 0;

  S := Spaces(2);
  for Y := BegY to EndY - 1 do
    S := S + sLineBreakStr + Spaces(2);

  InsertColumnText(X, BegY, S);

  Changed;
  if UpdateLock = 0 then
    Invalidate;
end;

procedure TJvCustomEditor.UnIndentColumns(X: Integer; BegY, EndY: Integer);
var
  S, UnindentedText: string;
  Y: Integer;
  Len, L: Integer;
begin
  if BegY < 0 then
    BegY := 0;
  if BegY >= FLines.Count then
    BegY := FLines.Count - 1;
  if EndY < 0 then
    EndY := 0;
  if EndY >= FLines.Count then
    EndY := FLines.Count - 1;
  if EndY < BegY then
    Exit;
  if X < 0 then
    X := 0;

  Inc(X); // for string operations

  LockUpdate;
  try
    UnindentedText := '';
    for Y := BegY to EndY do
    begin
      S := FLines[Y];
      Len := Length(S);

      // how many spaces to delete
      L := 0;
      while (X + L <= Len) and (L < 2) and (S[X + L] = ' ') do
        Inc(L);

      if L > 0 then
      begin
        UnindentedText := UnindentedText + Spaces(L);
        Delete(S, X, L);
        FLines.Internal[Y] := S;
      end;
      if Y < EndY then
        UnindentedText := UnindentedText + sLineBreakStr;
    end;
  finally
    UnlockUpdate;
  end;

  Dec(X); // for caret operations
  if Length(UnindentedText) > 0 then
  begin
    { --- UNDO --- }
    TJvUnindentColumnUndo.Create(Self, CaretX, CaretY, X, BegY, UnindentedText);
    { --- /UNDO --- }
    TextModified(X, BegY, maDelete, UnindentedText);

    Changed;
    if UpdateLock = 0 then
      Invalidate;
  end;
end;

procedure TJvCustomEditor.ClipboardPaste;
var
  ClipS: string;
  Len: Integer;
  H: THandle;
  X, Y, EndX, EndY: Integer;
begin
  if (CaretY > FLines.Count - 1) and (FLines.Count > 0) then
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
    ClipS := ExpandTabs(AdjustLineBreaks(ClipS));
    PaintCaret(False);

    ReLine;
    with FSelection do
    begin
      X := CaretX;
      Y := CaretY;
      BeginCompound;
      try
        if IsSelected then
        begin
          if BlockOverwrite and not PersistentBlocks then
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
            if (ClipS = '') or (ClipS[Length(ClipS)] <> Lf) then
              ClipS := ClipS + sLineBreakStr;
          end;

          { --- UNDO --- }
          TJvInsertUndo.Create(Self, X, Y, ClipS);
          { --- /UNDO --- }

          FLines.InsertText(X, Y, ClipS);
          TextModified(X, Y, maInsert, ClipS);

          // get new caret position
          GetEndPosCaret(ClipS, X, Y, EndX, EndY);
          Inc(EndX);

          if PersistentBlocks then
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
          X := CaretX - 1;
          Inc(X);
        end;
      finally
        EndCompound;
      end;
    end;

    SetCaretInternal(X, Y);

    Changed;
  finally
    PaintCaret(True);
    EndUpdate; {!!! Causes copying all [translated] }
  end;
end;

procedure TJvCustomEditor.DeleteSelected;
var
  S: string;
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
        TJvDeleteSelectedUndo.Create(Self, CaretX, CaretY, S);
        {--- /UNDO ---}
        IsSelected := False;
        Selecting := False;
        if SelBlockFormat in [bfInclusive, bfNonInclusive, bfLine] then
        begin
          FLines.DeleteText(X, Y, SelEndX - 1 + Ord(SelBlockFormat = bfInclusive), SelEndY);
          TextModified(SelBegX, SelBegY, maDelete, S);
        end
        else
        if SelBlockFormat = bfColumn then
        begin
          Y := CaretY;
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
  end;
end;

procedure TJvCustomEditor.Clear;
begin
  FLines.Clear;
end;

function TJvCustomEditor.GetLines: TStrings;
begin
  Result := FLines;
end;

procedure TJvCustomEditor.SetLines(ALines: TStrings);
begin
  if ALines <> nil then
    FLines.Assign(ALines);
  {--- UNDO ---}
  NotUndoable;
  {--- /UNDO ---}
end;

procedure TJvCustomEditor.TextAllChangedInternal(Unselect: Boolean);
begin
  inherited TextAllChangedInternal(Unselect);
  TextModified(0, 0, maAll, '');
  UpdateEditorView;
end;

function TJvCustomEditor.ExpandTabs(const S: string): string;
var
  ps, I: Integer;
  Sp: string;
  Tabs, LenSp: Integer;
  P: PChar;
begin
  // ahuser: I think we should reimplement that function with proper tab handling.
  ps := Pos(Tab, S);
  if ps > 0 then
  begin
    // How many Tab chars?
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
      Move(S[1], P[0], ps * SizeOf(Char));
      Inc(P, ps);
    end;

    for I := ps to Length(S) do
    begin
      if S[I] <> Tab then
      begin
        P[0] := S[I];
        Inc(P);
      end
      else
      if LenSp > 0 then
      begin
        Move(Sp[1], P[0], LenSp * SizeOf(Char));
        Inc(P, LenSp);
      end;
    end;
  end
  else
    Result := S;
end;

procedure TJvCustomEditor.TextModified(ACaretX, ACaretY: Integer; Action: TModifiedAction; const Text: string);
begin
  Invalidate;
end;

function TJvCustomEditor.GetLineCount: Integer;
begin
  Result := FLines.Count;
end;

function TJvCustomEditor.GetLineLength(Index: Integer): Integer;
begin
  Result := Length(FLines[Index]);
end;

function TJvCustomEditor.FindNotBlankCharPosInLine(Line: Integer): Integer;
begin
  Result := FindNotBlankCharPos(FLines[Line]);
end;

procedure TJvCustomEditor.SetLockText(const Text: string);
begin
  FLines.SetLockText(Text);
end;

function TJvCustomEditor.GetWordOnCaret: string;
begin
  if CaretY < FLines.Count then
    Result := GetWordOnPos(FLines[CaretY], CaretX + 1)
  else
    Result := '';
end;

function TJvCustomEditor.GetAutoIndentStop(Y: Integer): Integer;
var
  I, Len: Integer;
  S: string;
begin
  Result := 0;

  // find non-empty line
  Dec(Y);
  while Y > 0 do
  begin
    S := FLines[Y];
    if Length(Trim(S)) > 0 then
      Break;
    Dec(Y);
  end;
  if Y < 0 then
    Exit;

  Len := Length(S);
  I := 1;
  while (I <= Len) and (S[I] = ' ') do
    Inc(I);
  Result := I - 1;
end;

function TJvCustomEditor.GetTabStop(X, Y: Integer; Next: Boolean): Integer;
var
  I: Integer;

  procedure UpdateTabStops;
  var
    S: string;
    J, I: Integer;
  begin
    FillChar(FTabPos, SizeOf(FTabPos), False);
    if SmartTab then
    begin
      J := 1;
      I := 1;
      while Y - J >= 0 do
      begin
        S := TrimRight(FLines[Y - J]);
        if Length(S) > I then
          FTabPos[Length(S)] := True;
        while I <= Length(S) do
        begin
          if CharInSet(S[I], IdentifierSymbols) then
          begin
            FTabPos[I - 1] := True;
            while (I <= Length(S)) and CharInSet(S[I], IdentifierSymbols) do
              Inc(I);
          end;
          Inc(I);
        end;

        if I >= Max_X_Scroll then
          Break;
        if J >= VisibleRowCount * 2 then
          Break;
        Inc(J);
      end;
    end;
  end;

begin
  UpdateTabStops;
  Result := X;
  if Next then
  begin
    for I := X + 1 to High(FTabPos) do
      if FTabPos[I] then
      begin
        Result := I;
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

function TJvCustomEditor.GetBackStop(X, Y: Integer): Integer;
var
  I: Integer;
  S: string;

  procedure UpdateBackStops;
  var
    S: string;
    J, I, K: Integer;
  begin
    J := 1;
    I := X - 1;
    FillChar(FTabPos, SizeOf(FTabPos), False);
    FTabPos[0] := True;
    while Y - J >= 0 do
    begin
      S := FLines[Y - J];
      for K := 1 to Min(Length(S), I) do
        if S[K] <> ' ' then
        begin
          I := K;
          FTabPos[I - 1] := True;
          Break;
        end;
      if I = 1 then
        Break;
      if J >= VisibleRowCount * 2 then
        Break;
      Inc(J);
    end;
  end;

begin
  Result := X - 1;
  S := TrimRight(FLines[Y]);
  if (Trim(Copy(S, 1, X)) = '') and
    ((X + 1 > Length(S)) or (S[X + 1] <> ' ')) then
  begin
    UpdateBackStops;
    for I := X downto 0 do
      if FTabPos[I] then
      begin
        Result := I;
        Exit;
      end;
  end;
end;

//=== { TJvInsertUndo } ======================================================

constructor TJvInsertUndo.Create(AJvEditor: TJvCustomEditor;
  ACaretX, ACaretY: Integer; const AText: string);
begin
  inherited Create(AJvEditor, ACaretX, ACaretY);
  FText := AText;
  if JvEditor.PersistentBlocks then
    SaveSelection;
end;

function TJvInsertUndo.GetEditor: TJvCustomEditor;
begin
  Result := TJvCustomEditor(FJvEditor);
end;

procedure TJvInsertUndo.Undo;
var
  Text: string;
  EndX, EndY: Integer;
  du: TJvInsertUndo;
begin
  Text := '';
  with TJvUndoBufferAccessProtected(UndoBuffer) do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := TJvInsertUndo(LastUndo).FText + Text;
      Dec(FPtr);
      if not JvEditor.GroupUndo then
        Break;
    end;
    Inc(FPtr);

    du := TJvInsertUndo(Items[FPtr]);
  end;

  GetEndPosCaret(Text, du.CaretX, du.CaretY, EndX, EndY); // get end caret position
  TJvCustomEditor(JvEditor).FLines.DeleteText(du.CaretX, du.CaretY, EndX, EndY);
  TJvCustomEditor(JvEditor).TextModified(du.CaretX, du.CaretY, maDelete, Text);

  TJvCustomEditor(JvEditor).SetCaretInternal(du.CaretX, du.CaretY);
end;

//=== { TJvOverwriteUndo } ===================================================

constructor TJvOverwriteUndo.Create(AJvEditor: TJvCustomEditor;
  ACaretX, ACaretY: Integer; const AOldText, ANewText: string);
begin
  inherited Create(AJvEditor, ACaretX, ACaretY);
  FOldText := AOldText;
  FNewText := ANewText;
end;

function TJvOverwriteUndo.GetEditor: TJvCustomEditor;
begin
  Result := TJvCustomEditor(FJvEditor);
end;

procedure TJvOverwriteUndo.Undo;
var
  OldText, NewText: string;
  EndX, EndY: Integer;
  du: TJvOverwriteUndo;
begin
  OldText := '';
  NewText := '';
  with TJvUndoBufferAccessProtected(UndoBuffer) do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      OldText := TJvOverwriteUndo(LastUndo).FOldText + OldText;
      NewText := TJvOverwriteUndo(LastUndo).FNewText + NewText;
      Dec(FPtr);
      if not GetEditor.GroupUndo then
        Break;
    end;
    Inc(FPtr);
    du := TJvOverwriteUndo(Items[FPtr]);
  end;
  with du do
  begin
    GetEndPosCaret(NewText, du.CaretX, du.CaretY, EndX, EndY); // get end caret position
    GetEditor.FLines.DeleteText(CaretX, CaretY, EndX, EndY);
    GetEditor.FLines.InsertText(CaretX, CaretY, OldText);
    GetEditor.TextModified(CaretX, CaretY, maReplace, OldText);

    GetEditor.SetCaretInternal(CaretX, CaretY);
  end;
end;

//=== { TJvInsertColumnUndo } ================================================

procedure TJvInsertColumnUndo.Undo;
var
  I: Integer;
  SS: TStringList;
  S: string;
begin
  { Do not call GetEditor.FLines.DeleteColumnText() here because it has not
    the functionality needed in this context. It deletes the columns from
    [BegX..EndX] even if the inserted line was not as long as EndX-BegX+1. }

  SS := TStringList.Create;
  try
    SS.Text := FText;
    for I := 0 to SS.Count - 1 do
    begin
      S := GetEditor.FLines[CaretY + I];
      Delete(S, CaretX + 1, Length(SS[I]));
      GetEditor.FLines.Internal[CaretY + I] := S;
    end;
  finally
    SS.Free;
  end;
  GetEditor.TextModified(CaretX, CaretY, maDelete, FText);

  GetEditor.SetCaretInternal(CaretX, CaretY);
end;

//=== { TJvUnindentColumnUndo } ==============================================

constructor TJvUnindentColumnUndo.Create(AJvEditor: TJvCustomEditor;
  ACaretX, ACaretY, ABegX, ABegY: Integer; const AText: string);
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
  with TJvUndoBufferAccessProtected(UndoBuffer) do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      with TJvUnindentColumnUndo(LastUndo) do
      begin
        GetEditor.FLines.InsertColumnText(FBegX, FBegY, FText);
        if BegX > FBegX then
          BegX := FBegX;
        if BegY > FBegY then
          BegY := FBegY;
      end;
      Dec(FPtr);
      if not GetEditor.GroupUndo then
        Break;
    end;
    Inc(FPtr);
  end;
  GetEditor.TextModified(BegX, BegY, maInsert, GetEditor.FLines[BegY]);

  RestoreSelection;
  with TJvUnindentColumnUndo(TJvUndoBufferAccessProtected(UndoBuffer).LastUndo) do
    GetEditor.SetCaretInternal(CaretX, CaretY);
end;

//=== { TJvDeleteUndo } ======================================================

procedure TJvDeleteUndo.Undo;
var
  Text: string;
begin
  Text := '';
  with TJvUndoBufferAccessProtected(UndoBuffer) do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := TJvDeleteUndo(LastUndo).FText + Text;
      Dec(FPtr);
      if not GetEditor.GroupUndo then
        Break;
    end;
    Inc(FPtr);

    with TJvDeleteUndo(Items[FPtr]) do
    begin
      GetEditor.FLines.InsertText(CaretX, CaretY, Text);
      GetEditor.TextModified(CaretX, CaretY, maInsert, Text);

      GetEditor.SetCaretInternal(CaretX, CaretY);
    end;
  end;
end;

//=== { TJvDeleteLineUndo } ==================================================

{procedure TJvDeleteLineUndo.Redo;
begin
  GetEditor.FLines.Insert(CaretY, FText);
end;}

constructor TJvDeleteLineUndo.Create(AJvEditor: TJvCustomEditor; ACaretX, ACaretY: Integer;
  const AText: string; ALastLineDelete: Boolean);
begin
  inherited Create(AJvEditor, ACaretX, ACaretY, AText);
  FLastLineDelete := ALastLineDelete;
end;

procedure TJvDeleteLineUndo.Undo;
begin
  GetEditor.LockUpdate;
  try
    if FLastLineDelete then
    begin
      GetEditor.FLines[CaretY] := FText;
      GetEditor.TextModified(CaretX, CaretY, maReplace, FText);
    end
    else
    begin
      GetEditor.FLines.Insert(CaretY, FText);
      GetEditor.TextModified(CaretX, CaretY, maInsert, FText);
    end;
  finally
    GetEditor.UnlockUpdate;
  end;
  GetEditor.SetCaretInternal(CaretX, CaretY);
end;

//=== { TJvBackspaceUndo } ===================================================

procedure TJvBackspaceUndo.Undo;
var
  Text: string;
  StartPtr: Integer;
begin
  Text := '';
  with TJvUndoBufferAccessProtected(UndoBuffer) do
  begin
    StartPtr := FPtr;
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := Text + TJvDeleteUndo(LastUndo).FText;
      Dec(FPtr);
      if not GetEditor.GroupUndo then
        Break;
    end;
    Inc(FPtr);
  end;

  with TJvDeleteUndo(UndoBuffer.Items[StartPtr]) do
  begin
    GetEditor.FLines.InsertText(CaretX - 1, CaretY, Text);
    GetEditor.TextModified(CaretX - 1, CaretY, maInsert, Text);
  end;

  // set caret on last backspace undo's position
  with TJvDeleteUndo(UndoBuffer.Items[TJvUndoBufferAccessProtected(UndoBuffer).FPtr]) do
    if (FText = Lf) or (FText = Cr) then // a line was removed by backspace
      GetEditor.SetCaretInternal(0, CaretY + 1)
    else
      GetEditor.SetCaretInternal(CaretX, CaretY);
end;

procedure TJvBackspaceUnindentUndo.Undo;
var
  Text: string;
begin
  Text := '';
  with TJvUndoBufferAccessProtected(UndoBuffer) do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := Text + TJvDeleteUndo(LastUndo).FText;
      Dec(FPtr);
      if not GetEditor.GroupUndo then
        Break;
    end;
    Inc(FPtr);

    with TJvDeleteUndo(Items[FPtr]) do
    begin
      GetEditor.FLines.InsertText(CaretX - Length(Text), CaretY, Text);
      GetEditor.TextModified(CaretX - Length(Text), CaretY, maInsert, Text);
      // set caret on last backspace undo's position
      GetEditor.SetCaretInternal(CaretX, CaretY);
    end;
  end;
end;

//=== { TJvReplaceUndo } =====================================================

constructor TJvReplaceUndo.Create(AJvEditor: TJvCustomEditor;
  ACaretX, ACaretY: Integer; ABegX, ABegY: Integer; const AText, ANewText: string);
begin
  inherited Create(AJvEditor, ACaretX, ACaretY);
  FBegX := ABegX;
  FBegY := ABegY;
  FText := AText;
  FNewText := ANewText;
end;

function TJvReplaceUndo.GetEditor: TJvCustomEditor;
begin
  Result := TJvCustomEditor(FJvEditor);
end;

procedure TJvReplaceUndo.Undo;
var
  EndX, EndY: Integer;
begin
  GetEndPosCaret(FNewText, FBegX, FBegY, EndX, EndY);
  GetEditor.FLines.DeleteText(FBegX, FBegY, EndX, EndY);
  GetEditor.FLines.InsertText(FBegX, FBegY, FText);
  GetEditor.TextModified(FBegX, FBegY, maReplace, FText);

  GetEditor.SetCaretInternal(CaretX, CaretY);
end;

//=== { TJvDeleteSelectedUndo } ==============================================

constructor TJvDeleteSelectedUndo.Create(AJvEditor: TJvCustomEditor;
  ACaretX, ACaretY: Integer; const AText: string);
begin
  inherited Create(AJvEditor, ACaretX, ACaretY, AText);
  SaveSelection;
end;

procedure TJvDeleteSelectedUndo.Undo;
var
  S: string;
  I: Integer;
begin
  with FSelection^ do
  begin
    if SelBlockFormat in [bfInclusive, bfNonInclusive, bfLine] then
    begin
      GetEditor.FLines.InsertText(SelBegX, SelBegY, FText);
      GetEditor.TextModified(SelBegX, SelBegY, maInsert, FText);
    end
    else
    if SelBlockFormat = bfColumn then
    begin
      for I := SelBegY to SelEndY do
      begin
        S := GetEditor.FLines[I];
        Insert(SubStrBySeparator(FText, I - SelBegY, sLineBreakStr), S, SelBegX + 1);
        GetEditor.FLines.Internal[I] := S;
      end;
      GetEditor.TextModified(SelBegX, SelBegY, maInsertColumn, FText);
    end;

    RestoreSelection;
    GetEditor.SetCaretInternal(CaretX, CaretY);
  end;
end;

//=== { TJvEditorCompletion } ================================================

constructor TJvCompletion.Create(AJvEditor: TJvCustomEditor);
begin
  inherited Create(AJvEditor);
  FIdentifiers := TStringList.Create;
  FTemplates := TStringList.Create;
  FCaretChar := '|';
  FCRLF := '/n';
  FSeparator := '=';
end;

destructor TJvCompletion.Destroy;
begin
  inherited Destroy;
  FIdentifiers.Free;
  FTemplates.Free;
end;

{ Substitutes word on the cursor position by NewString [translated] }

procedure TJvCompletion.ReplaceWord(const NewString: string);
var
  S, W: string;
  X, Y: Integer;
  iBegSX, iEndSX: Integer;
  NewCaret: Integer;
begin
  with TJvCustomEditor(JvEditor) do
  begin
    if CaretY < FLines.Count then
      S := FLines[CaretY];
    W := GetNextWordPosEx(S, CaretX, iBegSX, iEndSX);
    if W <> NewString then
    begin
      BeginUpdate;
      PaintCaret(False);
      try
        BeginCompound;
        try
          Deselect;
          ReLine;

          if Length(W) = 0 then
          begin
            iBegSX := CaretX + 1;
            iEndSX := CaretX;
          end;
          case Mode of
            cmIdentifiers:
              begin
                S := NewString;
                if Assigned(FOnCompletionApply) then
                  FOnCompletionApply(Self, W, S);
                NewCaret := -1;
              end;
            cmTemplates:
              begin
                S := ReplaceString(NewString, FCRLF, sLineBreakStr + Spaces(CaretX -
                  Length(W)));
                S := ReplaceString(S, FCaretChar, '');
                NewCaret := Pos(FCaretChar, ReplaceString(NewString, FCRLF, sLineBreakStr)) - 1;
              end;
          else
            raise EJvEditorError.CreateRes(@RsEInvalidCompletionMode);
          end;
          {--- UNDO ---}
          TJvReplaceUndo.Create(TJvCustomEditor(JvEditor), CaretX, CaretY,
            iBegSX - 1, CaretY, W, S);
          {--- /UNDO ---}
          if iBegSX <= iEndSX then
            FLines.DeleteText(iBegSX - 1, CaretY, iEndSX - 1, CaretY);
          FLines.InsertText(iBegSX - 1, CaretY, S);
          TextModified(iBegSX - 1, CaretY, maReplace, S);

          if NewCaret >= 0 then
            SetLength(S, NewCaret); // truncate S to the new caret position
          GetEndPosCaret(S, iBegSX - 1, CaretY, X, Y);
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

procedure TJvCompletion.MakeItems;
var
  I: Integer;
  S: string;
begin
  Items.Clear;
  case Mode of
    cmIdentifiers:
      for I := 0 to FIdentifiers.Count - 1 do
        Items.Add(FIdentifiers[I]);
    cmTemplates:
      begin
        with TJvCustomEditor(JvEditor) do
          if FLines.Count > CaretY then
            S := GetWordOnPos(FLines[CaretY], CaretX)
          else
            S := '';
        for I := 0 to FTemplates.Count - 1 do
          if StrLIComp(PChar(FTemplates[I]), PChar(S), Length(S)) = 0 then
            Items.Add(FTemplates[I]);
        if Items.Count = 0 then
          Items.Assign(FTemplates);
      end;
  end;
end;

procedure TJvCompletion.FindSelItem(var Eq: Boolean);
var
  S: string;

  function FindFirst(Strs: TStrings; S: string): Integer;
  var
    I: Integer;
  begin
    for I := 0 to Strs.Count - 1 do
      if StrLIComp(PChar(Strs[I]), PChar(S), Length(S)) = 0 then
      begin
        Result := I;
        Exit;
      end;
    Result := -1;
  end;

begin
  with TJvCustomEditor(JvEditor) do
    if FLines.Count > 0 then
      S := GetWordOnPos(FLines[CaretY], CaretX)
    else
      S := '';
  if Trim(S) = '' then
    ItemIndex := -1
  else
    ItemIndex := FindFirst(Items, S);
  Eq := (ItemIndex > -1) and SameText(Trim(SubStrBySeparator(Items[ItemIndex], 0, FSeparator)), S);
end;

function TJvCompletion.GetStrings(Index: Integer): TStrings;
begin
  case Index of
    0:
      Result := FIdentifiers;
    1:
      Result := FTemplates;
  else
    Result := nil;
  end;
end;

procedure TJvCompletion.SetStrings(Index: Integer; AValue: TStrings);
begin
  case Index of
    0:
      FIdentifiers.Assign(AValue);
    1:
      FTemplates.Assign(AValue);
  end;
end;

function TJvCompletion.GetIdentifierCount: Integer;
begin
  Result := FIdentifiers.Count;
end;

function TJvCompletion.GetTemplateCount: Integer;
begin
  Result := FTemplates.Count;
end;

procedure TJvCompletion.ReplaceWordItemIndex(SubStrStart: Integer);
begin
  ReplaceWord(SubStrBySeparator(Items[ItemIndex], SubStrStart, FSeparator));
end;

function TJvCustomEditor.GetCompletion: TJvCompletion;
begin
  Result := TJvCompletion(inherited Completion);
end;

procedure TJvCustomEditor.SetCompletion(const Value: TJvCompletion);
begin
  inherited Completion := Value;
end;

function TJvCompletion.GetSeparator: string;
begin
  Result := FSeparator;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
