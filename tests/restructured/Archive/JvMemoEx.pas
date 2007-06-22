{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMemoEx.pas, released Feb 2000.

The Initial Developer of the Original Code is Anthony Steele [asteele@iafrica.com]
Portions created by Anthony Steele are Copyright (C) 1999-2001 Anthony Steele.
All Rights Reserved.

Contributor(s): Suggesions and code in TCustomAutoVScrollbarMemo
                by Petr Vones and Marcel von Brakel on the Delphi JEDI VCL list


Last Modified: 20002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}



unit JvMemoEx;

{-----------------------------------------------------------------------------
Comments:

 these are VCL extended components for data display/capture

 would fit into the JVCL package

 This is a clean reimplementation of things done for my employer-at-the-time.

 However it is done on my own time, and is not hard (as long as you know it can be done),
 so I feel justified in allowing this component to be open-source
 It will hopefully help programmers everywhere, is not harming anyone's business
 or pinching business secrets

Known Issues:

 Still to do on all these controls:
  - test at design time
  - see comments for each component

-----------------------------------------------------------------------------}


interface

uses StdCtrls, Classes, Messages, Controls, JvComponent;

const
  {
    This is to get around a Win98 bug
    When pasting a lot of text into a memo with Autobar = True
    and MaxLength = 255
    NT works, but 98 crahes the app badly
    The solution is to desychronise the update with the old trick of
    posting a message to your own window handle.

    using WM_USER + 1 doesn't work - someone else is using it??
  }
  WM_AUTOBAR = WM_USER + 43;


{ A Multiline edit with additional useful stuph

 - can set the max number of lines (implemented using delphi overrides)
 - can scroll around (implemented using the existing windows message for text controls EM_LINESCROLL)
 - can read and write the current cursor position (implemented using the
   existing windows messages for text controls EM_LINESCROLL and EM_LINEFROMCHAR )
   see on MSDN on "Edit Control Messages"

 - still to do
  - test some more

 possible extensions
  - max line length?
 }
type
  TCustomMemoEx = class(TJvCustomMemo)
  private
    fiMaxLines: integer;

  protected
    procedure KeyPress(var key: char); override;
    procedure Change; override;

    function GetCurrentLine: integer;
    procedure SetCurrentLine(iNewLine: integer);

    { can publish this  property}
    property MaxLines: integer read fiMaxLines write fiMaxLines;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    { these wrap the windows messages }
    procedure LineScroll(ix, iy: integer);
    function CharOfLine(iLine: integer): integer;

    { this property is runtime only }
    property CurrentLine: integer read GetCurrentLine write SetCurrentLine;

  published
  end;


  { this class can decide to show/hide the v scrollbar
   based on the amount of text in it

   It is written as a sperate class to TCustomMemoEx
    as the functionality is orthogonal - you can use the 2 parts independently

    It doesn't matter if  TCustomMemoEx is the base class of TCustomAutoVScrollbarMemo
    or vice versa, both are hidden by the surface class (see below)

    Still to do
     - test
   }
  TCustomAutoVScrollbarMemo = class(TCustomMemoEx)
  private
    { property implementation vars }
    FbAutoVScrollbar: boolean;

    { internals -  these are (a bit) complex to get,
      don't need to recalc them on every keystroke when they actually don't often change }
    fiLineHeight, fiWindowHeight: integer;

  protected
    { overrides }
    procedure Change; override;

    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;

    { property implementation }
    procedure SetAutoVScrollbar(const bValue: boolean);

    { worker procs }
    function LinesFitOnScreen: boolean;
    function LineHeight: integer;
    function WindowHeight: integer;

    procedure PostAutoBarMessage;
    procedure OnAutoBarMessage(var MSg: TMessage); message WM_AUTOBAR;
    procedure AutoBar;

    { can publish this property }
    property AutoVScrollbar: boolean read FbAutoVScrollbar write SetAutoVScrollbar;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;

  end;


  { Memo restricted to single-line
  the benefit of this class is that now a text field
   can be both single-line and right or center aligned
   This is useful for any edit that captures a number
   Several descendant classes are implemented
  }
  TCustomSingleLineMemo = class(TCustomMemo)
  protected
    procedure KeyPress(var key: char); override;
    procedure Change; override;

  public
    constructor Create(AOwner: TComponent); override;

  end;


  { Implements the AutoSelect property as per TEdit
    on entering the controls, automatically select the entire text

    If need be, this functionality can be taken out of the class heirarchy
    or put in at a different level  }
  TCustomAutoSelectMemo = class(TCustomSingleLineMemo)
  private
    fbAutoSelect: boolean;
  protected
    procedure DoEnter; override;

  public
    constructor Create(AOwner: TComponent); override;

    property AutoSelect: boolean read fbAutoSelect write fbAutoSelect;
  end;


{ as required by JEDI VCL standards }
const
  TMEMOEX_VERSION = 0.8;
  TSINGLELINEMEMO_VERSION = 0.8;

type

  { surface classes to publish properties }

  TJvMemoEx = class(TCustomAutoVScrollbarMemo)
  published
    { new properties }
    property MaxLines;
    property AutoVScrollbar;

    { all properties of TMemo }
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property Lines;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

  end;

  TJvSingleLineMemo = class(TCustomAutoSelectMemo)
  published
    { most properties of TMemo
     no WordWrap property }
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property WantReturns;
    property WantTabs;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

uses
 { delphi } Windows, Forms,
 { JCL } JclStrings,
 { local } JvComponentFunctions;

{ AFS 23 Nov 1999
  These functions are used only by the classes in this unit
  and are not great candidates for reuse
  So I am leaving them hidden in here
}

function GetFontHeight(phFont: hFont): integer;
var
  hWin:     HDC;
  tm:       TTextMetric;
  hOldFont: HFont;
begin
  Result := -1;

  if phFont = 0 then
    exit;

  hWin := GetDC(0); // use the main screen window handle

  { according to the API help for GetDC, a return value of 'null' is error }
  if hWin = 0 then
    exit;

  try
    hOldFont := SelectObject(hWin, phFont);
    GetTextMetrics(hWin, tm);
    SelectObject(hWin, hOldFont);
  finally
    ReleaseDC(0, hWin);
  end;

  Result := tm.tmHeight;
end;

function AddVScrollbar(sb: TScrollStyle): TScrollStyle;
begin
  if sb = ssHorizontal then
    Result := ssBoth
  else
    Result := ssVertical;
end;

function RemoveVScrollbar(sb: TScrollStyle): TScrollStyle;
begin
  if sb = ssBoth then
    Result := ssHorizontal
  else
    Result := ssNone;
end;

{-------------------------------------------------------------------------------
 TCustomMemoEx }

constructor TCustomMemoEx.Create(AOwner: TComponent);
begin
  inherited;
  fiMaxLines := 0;
end;

procedure TCustomMemoEx.Assign(Source: TPersistent);
var
  lcSource: TCustomMemoEx;
begin
  inherited;

  if Source is TCustomMemoEx then
  begin
    lcSource := Source as TCustomMemoEx;
    MaxLines := lcSource.MaxLines;
  end;
end;

{ this just ensures the target is in view, it does not set the caret pos }
procedure TCustomMemoEx.LineScroll(ix, iy: integer);
begin
  Perform(EM_LINESCROLL, ix, iy);
end;

function TCustomMemoEx.GetCurrentLine: integer;
begin
  Result := Perform(EM_LINEFROMCHAR, - 1, 0);
end;

procedure TCustomMemoEx.KeyPress(var key: char);
begin
  { only process if maxlines is set }
  if fiMaxLines > 0 then
  begin
    if Lines.Count >= MaxLines then
    begin
      { take steps to halt the overflow }

      { no returns - that would make another line }
      if CharIsReturn(Key) then
        Key := #0;

      { no text at the end except for delete & backspace }
      if (CurrentLine >= MaxLines) and not (key = AnsiBackSpace) then
        Key := #0;
    end;
  end;

  inherited;
end;

{ This does not recurse it seems }
procedure TCustomMemoEx.Change;
begin
  { only process if maxlines is set - truncate }
  if MaxLines > 0 then
  begin
    while (Lines.Count > MaxLines) do
      Lines.Delete(Lines.Count - 1);
  end;

  inherited;
end;

procedure TCustomMemoEx.SetCurrentLine(iNewLine: integer);
var
  iDelta: integer;
begin
  { truncate the range }
  if iNewLine >= Lines.Count then
    iNewLine := Lines.Count - 1;
  if iNewLine < 0 then
    iNewLine := 0;

  iDelta := iNewLine - CurrentLine;
  { e.g want to be at line 10, currently on line 8, delta = 2
   on want to be on line 5, currently line 15, delta = -10 }
  if iDelta <> 0 then
  begin
    { scroll into view }
    LineScroll(0, iDelta);
    { move caret }
    SelStart := CharOfLine(iNewLine);
  end;
end;

function TCustomMemoEx.CharOfLine(iLine: integer): integer;
begin
  Result := Perform(EM_LINEINDEX, iLine, 0);
end;

{-------------------------------------------------------------------------------
  custom autoscrolling memo  }

constructor TCustomAutoVScrollbarMemo.Create(AOwner: TComponent);
begin
  inherited;

  FbAutoVScrollbar := True;
  fiLineHeight     := -1;
  fiWindowHeight   := -1;
end;

procedure TCustomAutoVScrollbarMemo.Assign(Source: TPersistent);
var
  lcSource: TCustomAutoVScrollbarMemo;
begin
  inherited;

  if Source is TCustomAutoVScrollbarMemo then
  begin
    lcSource       := Source as TCustomAutoVScrollbarMemo;
    AutoVScrollbar := lcSource.AutoVScrollbar;
  end;
end;


procedure TCustomAutoVScrollbarMemo.PostAutoBarMessage;
begin
  if AutoVScrollbar and HandleAllocated and Visible then
    PostMessage(self.Handle, WM_AUTOBAR, 0,0)
end;

procedure TCustomAutoVScrollbarMemo.OnAutoBarMessage(var Msg: TMessage);
begin
  AutoBar;
end;

procedure TCustomAutoVScrollbarMemo.AutoBar;
var
  eNewStyle: TScrollStyle;
  liSelStart, liSelEnd: integer;
  liRes: integer;
begin
  if not AutoVScrollbar then
    exit;

  if (not HandleAllocated) or (not Visible) then
    Exit;

  if (csReading in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  if not Assigned(Lines) then
    exit;

  { if the text doesn't fit in the box then there should be a v scrollbar }
  if LinesFitOnScreen then
    eNewStyle := RemoveVScrollBar(Scrollbars)
  else
    eNewStyle := AddVScrollBar(Scrollbars);

  if Scrollbars <> eNewStyle then
  begin
    { changing the style loses the cursor pos when the window is recreated, so keep it manually }
    liRes := Perform(EM_GETSEL, WPARAM(@liSelStart), LPARAM(@liSelEnd));
    if liRes = 0 then
      exit;

    Scrollbars := eNewStyle;
    HandleNeeded; // to allow the next perform to work during create

    // if it failed then we're f'd
    liRes := Perform(EM_SETSEL, liSelStart, liSelEnd); // back to old pos
    Assert(liRes <> 0);

    Perform(EM_SCROLLCARET, 0, 0); // scroll it into view
  end;
end;

function TCustomAutoVScrollbarMemo.LinesFitOnScreen: boolean;
begin
  { cache the line height. line height changes at runtime are quite rare
    but it must be reset when the font changes }

  if fiLineHeight <= 0 then
    fiLineHeight := LineHeight;

  if fiWindowHeight <= 0 then
    fiWindowHeight := WindowHeight;

  Result := (Lines.Count * fiLineHeight) < fiWindowHeight;
end;

function TCustomAutoVScrollbarMemo.WindowHeight: integer;
var
  liBorderSize: integer;
  lr:           TRect;
  lParams:      TCreateParams;
begin
  SetRect(lr, 0, 0, 0, 0);
  CreateParams(lParams);
  AdjustWindowRectEx(lr, lParams.Style, False, lParams.ExStyle);
  liBorderSize := lr.Bottom - lr.Top;

  Result := ClientHeight - liBorderSize;
end;


{ work out the height of one line of text in this control }
function TCustomAutoVScrollbarMemo.LineHeight: integer;
begin
  Result := -1;
  if (not HandleAllocated) or (not Visible) then
    exit;

  Result := GetFontHeight(Font.Handle);
end; { Function CalcLineHeight }


procedure TCustomAutoVScrollbarMemo.SetAutoVScrollbar(const bValue: boolean);
begin
  if (FbAutoVScrollbar = bValue) then
    exit;

  FbAutoVScrollbar := bValue;
  PostAutoBarMessage;
end;

procedure TCustomAutoVScrollbarMemo.Change;
begin
  inherited;
  PostAutoBarMessage;
end;

procedure TCustomAutoVScrollbarMemo.SetBounds(ALeft, ATop, AWidth,
  AHeight: integer);
begin
  inherited;

  { the old window height is no longer valid }
  fiWindowHeight := -1;
  PostAutoBarMessage;
end;

procedure TCustomAutoVScrollbarMemo.CMFontChanged(var Message: TMessage);
begin
  inherited;

  { the old text height is no longer valid }
  fiLineHeight := -1;
  PostAutoBarMessage;
end;

procedure TCustomAutoVScrollbarMemo.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;

  { the old window height is no longer valid }
  fiWindowHeight := -1;
  PostAutoBarMessage;
end;

{-------------------------------------------------------------------------------
  custom single line memo }

procedure TCustomSingleLineMemo.Change;
begin
  inherited;

  { truncate  to one line }
{
if not (csReading in ComponentState) then
  while Lines.Count > 1 do
    Lines.Delete(Lines.Count - 1);}
end;

constructor TCustomSingleLineMemo.Create(AOwner: TComponent);
const
  DEF_HEIGHT = 21;
  DEF_WIDTH  = 121;
begin
  inherited;
  { by default make it look like an edit box, one line high }
  Height := DEF_HEIGHT;
  Width  := DEF_WIDTH;

  // set some base properties in line with it's role
  WantReturns := False;
  WordWrap    := False;
end;

procedure TCustomSingleLineMemo.KeyPress(var key: char);
begin
  { squash return chars }
  if CharIsReturn(Key) then
    Key := #0;

  if Key <> #0 then
    inherited;
end;

{ ------------------------------------------------------------------------------
  TAutoSelectMemo }

constructor TCustomAutoSelectMemo.Create(AOwner: TComponent);
begin
  inherited;
  fbAutoSelect := True;
end;

procedure TCustomAutoSelectMemo.DoEnter;
begin
  if AutoSelect then
  begin
    SelStart  := 0;
    SelLength := Length(Text);
  end;

  inherited;
end;


end.
