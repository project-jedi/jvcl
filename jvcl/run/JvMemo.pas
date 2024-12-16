{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMemo.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

This unit is a merging of the original TJvMemo, JvDisplayMemo, JvCaretMemo,JvMemoEx.
Merging done 2002-06-11 by Peter Thornqvist [peter3 at sourceforge dot net]

Contributor(s):
  Michael Beck [mbeck att bigfoot dott com]
  Anthony Steele [asteele att iafrica dott com]
  Peter Below [100113 dott 1101 att compuserve dott com]

  MERGE NOTES:
    * TjvCustomMemo has been removed from JvComponent and put here instead.
    * The HotTrack property only works if BorderStyle := bsSingle
    * To simulate the behaviour of JvDisplayMemo, set HideCaret to True,
      Readonly to True, Color to $C0FFFF and Cursor to crArrow
    * The MaxLines property has changed: it stills displays only the selected number of lines,
      but now saves the original content in an internal stringlist that can be restored by
      setting MaxLines to 0.
    * Added ClipboardCommands

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
  Under Vista and upper, the Transparent is working but causes flicker
  This comes from ForceRefreshIfNeeded that recreates the window, but at the
  time of writing, that was the only method that worked to get the control
  to be refreshed. If you find a better way, please do the change.
-----------------------------------------------------------------------------}
// $Id$

unit JvMemo;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JvCaret, JvTypes, JvExStdCtrls;

type
  TJvCustomMemo = class(TJvExCustomMemo)
  private
    FMaxLines: Integer;
    FHotTrack: Boolean;
    FOnHorizontalScroll: TNotifyEvent;
    FOnVerticalScroll: TNotifyEvent;
    FCaret: TJvCaret;
    FHideCaret: Boolean;
    FOrigLines: TStringList;
    FTransparent: Boolean;
    FSelLengthButtonDown: Integer;
    procedure SetHotTrack(Value: Boolean);
    procedure SetCaret(const Value: TJvCaret);
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure CNCtlColorEdit(var Message: TWMCtlColorEdit); message CN_CTLCOLOREDIT;
    procedure CNCtlColorStatic(var Message: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure SetMaxLines(const Value: Integer);
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
    procedure SetHideCaret(const Value: Boolean);
    function GetFlat: Boolean;
    function GetParentFlat: Boolean;
    procedure SetFlat(const Value: Boolean);
    procedure SetParentFlat(const Value: Boolean);
    procedure ForceRefreshIfNeeded;
  protected
    procedure SetClipboardCommands(const Value: TJvClipboardCommands); override;
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure WMClear(var Msg: TMessage); message WM_CLEAR;
    procedure WMUndo(var Msg: TMessage); message WM_UNDO;
    procedure CaretChange(Sender: TObject); dynamic;
    procedure FocusKilled(NextWnd: THandle); override;
    procedure FocusSet(PrevWnd: THandle); override;
    function DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean; override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure KeyPress(var Key: Char); override;
    procedure Change; override;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetCurrentLine: Integer;
    procedure SetCurrentLine(NewLine: Integer);
    procedure SetTransparent(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { these wrap the windows messages }
    procedure LineScroll(X, Y: Integer);
    function CharOfLine(iLine: Integer): Integer;
    procedure DefaultHandler(var Message); override;
    property CurrentLine: Integer read GetCurrentLine write SetCurrentLine;
  protected
    property Caret: TJvCaret read FCaret write SetCaret;
    property HideCaret: Boolean read FHideCaret write SetHideCaret default False;
    property MaxLines: Integer read FMaxLines write SetMaxLines default 0;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property Lines: TStrings read GetLines write SetLines;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property ParentFlat: Boolean read GetParentFlat write SetParentFlat default True;
    property OnVerticalScroll: TNotifyEvent read FOnVerticalScroll write FOnVerticalScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHorizontalScroll write FOnHorizontalScroll;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvMemo = class(TJvCustomMemo)
  published
    property AutoSize default False; // TCustomMemo.Create sets AutoSize:=False
    property Caret;
    property ClipboardCommands;
    property MaxLines;
    property HideCaret;
    property HotTrack;
    property HintColor;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnVerticalScroll;
    property OnHorizontalScroll;

    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Flat;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property Lines;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFlat;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Transparent;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    //    property Text;
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
  JclStrings;

constructor TJvCustomMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOrigLines := TStringList.Create;
  FHotTrack := False;
  // ControlStyle := ControlStyle + [csAcceptsControls];
  FCaret := TJvCaret.Create(Self);
  FCaret.OnChanged := CaretChange;
  FTransparent := False;
end;

destructor TJvCustomMemo.Destroy;
begin
  FOrigLines.Free;
  FCaret.Free;
  inherited Destroy;
end;

procedure TJvCustomMemo.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnHorizontalScroll) then
    FOnHorizontalScroll(Self);
end;

procedure TJvCustomMemo.WMKeyDown(var Message: TWMKeyDown);
var
  SelLengthBefore: Integer;
begin
  SelLengthBefore := SelLength;

  inherited;
  
  if SelLength <> SelLengthBefore then
    ForceRefreshIfNeeded;
end;

procedure TJvCustomMemo.WMLButtonDown(var Message: TWMLButtonDown);
begin
  FSelLengthButtonDown := SelLength;
  
  inherited;
end;

procedure TJvCustomMemo.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;

  if SelLength <> FSelLengthButtonDown then
    ForceRefreshIfNeeded;
end;

procedure TJvCustomMemo.WMNCPaint(var Message: TWMNCPaint);
begin
  // prevent drawing borders when transparent
  if not Transparent then
    inherited;
end;

procedure TJvCustomMemo.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVerticalScroll) then
    FOnVerticalScroll(Self);
end;

procedure TJvCustomMemo.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not MouseOver then
  begin
    if FHotTrack then
      Ctl3D := True;
    inherited MouseEnter(Control);
  end;
end;

procedure TJvCustomMemo.MouseLeave(Control: TControl);
begin
  if MouseOver then
  begin
    if FHotTrack then
      Ctl3D := False;
    inherited MouseLeave(Control);
  end;
end;

procedure TJvCustomMemo.SetHotTrack(Value: Boolean);
begin
  FHotTrack := Value;
  Ctl3D := not FHotTrack;
end;

{ This does not recurse it seems }

procedure TJvCustomMemo.Change;
begin
  { only process if maxlines is set - truncate }
  if MaxLines > 0 then
    while Lines.Count > MaxLines do
      Lines.Delete(Lines.Count - 1);
  inherited Change;
  
  ForceRefreshIfNeeded;
end;

procedure TJvCustomMemo.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Transparent then
  begin
    ControlStyle := ControlStyle - [csOpaque];
    Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT
  end
  else
  begin
    ControlStyle := ControlStyle + [csOpaque];
    Params.ExStyle := Params.ExStyle and not WS_EX_TRANSPARENT;
  end;
end;

function TJvCustomMemo.CharOfLine(iLine: Integer): Integer;
begin
  Result := Perform(EM_LINEINDEX, iLine, 0);
end;

procedure TJvCustomMemo.CNCtlColorEdit(var Message: TWMCtlColorEdit);
var
  NullBrush: HGDIOBJ;
begin
  inherited;
  if Transparent then
  begin
    NullBrush := GetStockObject(NULL_BRUSH);
    SelectObject(Message.ChildDC, NullBrush);
    Message.Result := NullBrush;
    SetBkMode(Message.ChildDC, Windows.TRANSPARENT);
  end
  else
  begin
    SetBkMode(Message.ChildDC, Windows.OPAQUE);
  end;
end;

procedure TJvCustomMemo.CNCtlColorStatic(var Message: TWMCtlColorStatic);
var
  NullBrush: HGDIOBJ;
begin
  inherited;
  if Transparent then
  begin
    NullBrush := GetStockObject(NULL_BRUSH);
    SelectObject(Message.ChildDC, NullBrush);
    Message.Result := NullBrush;
    SetBkMode(Message.ChildDC, Windows.TRANSPARENT);
  end
  else
  begin
    SetBkMode(Message.ChildDC, Windows.OPAQUE);
  end;
end;

function TJvCustomMemo.GetCurrentLine: Integer;
begin
  Result := Perform(EM_LINEFROMCHAR, WPARAM(-1), 0);
end;

function TJvCustomMemo.GetFlat: Boolean;
begin
  Result := not Ctl3D;
end;

procedure TJvCustomMemo.KeyPress(var Key: Char);
begin
  { only process if maxlines is set }
  if MaxLines > 0 then
    if Lines.Count >= MaxLines then
    begin
      { take steps to halt the overflow }

      { no returns - that would make another line }
      if CharIsReturn(Key) then
        Key := #0;

      { no text at the end except for delete & backspace }
      if (CurrentLine >= MaxLines) and not (Key = NativeBackSpace) then
        Key := #0;
    end;

  inherited KeyPress(Key);
end;

procedure TJvCustomMemo.LineScroll(X, Y: Integer);
begin
  Perform(EM_LINESCROLL, X, Y);
end;

procedure TJvCustomMemo.SetCurrentLine(NewLine: Integer);
var
  Delta: Integer;
begin
  { truncate the range }
  if NewLine >= Lines.Count then
    NewLine := Lines.Count - 1;
  if NewLine < 0 then
    NewLine := 0;

  Delta := NewLine - CurrentLine;
  { e.g want to be at line 10, currently on line 8, delta = 2
   on want to be on line 5, currently line 15, delta = -10 }
  if Delta <> 0 then
  begin
    { scroll into view }
    LineScroll(0, Delta);
    { move caret }
    SelStart := CharOfLine(NewLine);
  end;
end;

procedure TJvCustomMemo.SetFlat(const Value: Boolean);
begin
  Ctl3D := not Value;
end;

procedure TJvCustomMemo.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    RecreateWnd;
    Invalidate;
  end;
end;

procedure TJvCustomMemo.CaretChange(Sender: TObject);
begin
  FCaret.CreateCaret;
end;

procedure TJvCustomMemo.SetCaret(const Value: TJvCaret);
begin
  FCaret.Assign(Value);
end;

procedure TJvCustomMemo.FocusSet(PrevWnd: THandle);
begin
  inherited FocusSet(PrevWnd);
  FCaret.CreateCaret;
  if FHideCaret then
    Windows.HideCaret(Handle);
end;

procedure TJvCustomMemo.SetMaxLines(const Value: Integer);
begin
  if FMaxLines <> Value then
  begin
    if FMaxLines = 0 then
      // save original content
      FOrigLines.Assign(Lines);
    FMaxLines := Value;
    if FMaxLines = 0 then
      // restore original content
      Lines.Assign(FOrigLines);
    Change;
  end;
end;

procedure TJvCustomMemo.SetParentFlat(const Value: Boolean);
begin
  ParentCtl3D := Value;
end;

function TJvCustomMemo.GetLines: TStrings;
begin
  Result := inherited Lines;
end;

function TJvCustomMemo.GetParentFlat: Boolean;
begin
  Result := ParentCtl3D;
end;

procedure TJvCustomMemo.SetLines(const Value: TStrings);
begin
  inherited SetLines(Value);
  if MaxLines > 0 then
    // save original content
    FOrigLines.Assign(Value);
  Change;
end;

procedure TJvCustomMemo.SetHideCaret(const Value: Boolean);
begin
  if FHideCaret <> Value then
  begin
    FHideCaret := Value;
    if [csDesigning, csLoading] * ComponentState = [] then
    begin
      if Focused and FCaret.CaretCreated then
      begin
        if FHideCaret then
          Windows.HideCaret(Handle)
        else
          Windows.ShowCaret(Handle);
      end;
    end;
  end;
end;

procedure TJvCustomMemo.FocusKilled(NextWnd: THandle);
begin
  if FHideCaret then
    ShowCaret(Handle);
  FCaret.DestroyCaret;
  inherited FocusKilled(NextWnd);
end;

procedure TJvCustomMemo.DefaultHandler(var Message);

  procedure Scroll(Msg, ScrollCode: Integer);
  begin
    Perform(Msg, ScrollCode, 0);
    Perform(Msg, SB_ENDSCROLL, 0);
  end;

begin
  if FHideCaret and not (csDesigning in ComponentState) then
  begin
    case TMessage(Message).Msg of
      WM_LBUTTONDOWN:
        begin
          if not Focused then
            SetFocus;
          TMessage(Message).Result := 0;
        end;

      WM_LBUTTONUP, WM_MOUSEMOVE, WM_LBUTTONDBLCLK, WM_CHAR, WM_KEYUP:
        TMessage(Message).Result := 0;

      WM_KEYDOWN:
        begin
          case TWMKeyDown(Message).CharCode of
            VK_DOWN:
              Scroll(WM_VSCROLL, SB_LINEDOWN);
            VK_UP:
              Scroll(WM_VSCROLL, SB_LINEUP);
            VK_LEFT:
              Scroll(WM_HSCROLL, SB_LINELEFT);
            VK_RIGHT:
              Scroll(WM_HSCROLL, SB_LINERIGHT);
            VK_NEXT:
              Scroll(WM_VSCROLL, SB_PAGEDOWN);
            VK_PRIOR:
              Scroll(WM_VSCROLL, SB_PAGEUP);
            VK_HOME:
              Scroll(WM_VSCROLL, SB_TOP);
            VK_END:
              Scroll(WM_VSCROLL, SB_BOTTOM);
          end;
          TMessage(Message).Result := 0;
        end;
    else
      inherited DefaultHandler(Message);
    end;
  end
  else
    inherited DefaultHandler(Message);
end;

procedure TJvCustomMemo.ForceRefreshIfNeeded;
begin
  // Calling RecreateWnd is quite ugly but simply calling Repaint is not enough...
  if Transparent then
    RecreateWnd;
end;

procedure TJvCustomMemo.WMPaint(var Msg: TWMPaint);
var
  DC: HDC;
begin
  DC := GetDC(Handle);
  try
    if Transparent then
      SetBkMode(DC, Windows.TRANSPARENT)
    else
      SetBkMode(DC, Windows.OPAQUE);
  finally
    ReleaseDC(Handle, DC);
  end;
  inherited;
end;

function TJvCustomMemo.DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean;
begin
  if not Transparent then
    Result := inherited DoEraseBackground(Canvas, Param)
  else
    Result := False;
end;

procedure TJvCustomMemo.SetClipboardCommands(const Value: TJvClipboardCommands);
begin
  if ClipboardCommands <> Value then
  begin
    inherited SetClipboardCommands(Value);
    ReadOnly := ClipboardCommands <= [caCopy];
  end;
end;

procedure TJvCustomMemo.WMClear(var Msg: TMessage);
begin
  if not ReadOnly then
    inherited;
end;

procedure TJvCustomMemo.WMUndo(var Msg: TMessage);
begin
  if not ReadOnly then
    inherited;
end;

procedure TJvCustomMemo.WMCut(var Msg: TMessage);
begin
  if not ReadOnly then
    inherited;
end;

procedure TJvCustomMemo.WMPaste(var Msg: TMessage);
begin
  if not ReadOnly then
    inherited;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
