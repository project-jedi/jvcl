{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMemo.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

This unit is a merging of the original TJvMemo, JvDisplayMemo, JvCaretMemo,JvMemoEx.
Merging done 2002-06-11 by Peter Thornqvist [peter3@peter3.com]

Contributor(s):
  Michael Beck [mbeck@bigfoot.com]
  Anthony Steele [asteele@iafrica.com]
  Peter Below [100113.1101@compuserve.com]

  MERGE NOTES:
    * TjvCustomMemo has been removed from JvComponent and put here instead.
    * The HotTrack property only works if BorderStyle := bsSingle
    * To simulate the behaviour of JvDisplayMemo, set HideCaret to true,
      Readonly to true, Color to $C0FFFF and Cursor to crArrow
    * The combination of HideCaret and a custom Caret hasn't been tested
    * The MaxLines property has changed: it stills displays only the selected number of lines,
      but now saves the original content in an internal stringlist that can be restored by
      setting MaxLines to 0.
    * Added ClipboardCommands

Last Modified: 2002-06-10

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvMemo;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, JVCLVer, JvComponent;

const
  WM_AUTOBAR = WM_USER + 43;

type
  TJvCustomMemo = class(TCustomMemo)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FMaxLines: integer;
    FHotTrack: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FColor: TColor;
    FSaved: TColor;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOver: Boolean;
    FOnHScroll: TNotifyEvent;
    FOnVScroll: TNotifyEvent;
    FCaret: TJvCaret;
    FHideCaret: boolean;
    FOrigLines: TStrings;
    FClipboardCommands: TJvClipBoardCommands;

    procedure SetHotTrack(const Value: Boolean);
    procedure CaretChanged(sender: TObject); dynamic;
    procedure SetCaret(const Value: TJvCaret);
    procedure WMKillFocus(var msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var msg: TMessage); message WM_SETFOCUS;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMPaste(var Msg:TWMPaste);message WM_PASTE;
    procedure WMCopy(var Msg:TWMCopy);message WM_COPY;
    procedure WMCut(var Msg:TWMCut);message WM_CUT;
    procedure WMUndo(var Msg:TWMUndo);message WM_UNDO;
    procedure SetMaxLines(const Value: integer);
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
    procedure SetHideCaret(const Value: boolean);
    function GetReadOnly: boolean;
    procedure SetReadOnly(const Value: boolean);
    procedure SetClipboardCommands(const Value: TJvClipBoardCommands);
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure KeyPress(var key: char); override;
    procedure Change; override;
    function GetCurrentLine: integer;
    procedure SetCurrentLine(iNewLine: integer);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { these wrap the windows messages }
    procedure LineScroll(ix, iy: integer);
    function CharOfLine(iLine: integer): integer;
    property CurrentLine: integer read GetCurrentLine write SetCurrentLine;
  protected
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property ClipboardCommands:TJvClipBoardCommands read FClipboardCommands write SetClipboardCommands default [caCopy..caUndo];
    property Caret: TJvCaret read FCaret write SetCaret;
    property HideCaret: boolean read FHideCaret write SetHideCaret;
    property MaxLines: integer read FMaxLines write SetMaxLines;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property Lines: TStrings read GetLines write SetLines;
    property ReadOnly:boolean read GetReadOnly write SetReadOnly;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnVerticalScroll: TNotifyEvent read FOnVScroll write FOnVScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHScroll write FOnHScroll;
  end;

  TJvMemo = class(TjvCustomMemo)
  published
    property AboutJVCL;
    property AutoSize;
    property Caret;
    property ClipboardCommands;
    property MaxLines;
    property HideCaret;
    property HotTrack;
    property HintColor;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnCtl3DChanged;
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
    //    property Text;
    property Visible;
    property WantReturns;
    property WantTabs;
    property Wordwrap;
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
  JclStrings;

function GetFontHeight(phFont: hFont): integer;
var
  hWin: HDC;
  tm: TTextMetric;
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

constructor TJvCustomMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOrigLines := TStringlist.Create;
  FColor := clInfoBk;
  FHotTrack := False;
  FOver := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FCaret := TJvCaret.Create(self);
  FCaret.OnChanged := CaretChanged;
  FClipboardCommands := [caCopy..caUndo];
end;

{***********************************************}

procedure TJvCustomMemo.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FonCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvCustomMemo.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FonParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvCustomMemo.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnHScroll) then
    FOnHScroll(Self);
end;

procedure TJvCustomMemo.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVScroll) then
    FOnVScroll(Self);
end;

procedure TJvCustomMemo.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    if FHotTrack then
      Ctl3d := True;
    FOver := True;
  end;
  if Assigned(FonMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvCustomMemo.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FHotTrack then
      Ctl3d := False;
    FOver := False;
  end;
  if Assigned(FonMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvCustomMemo.SetHotTrack(const Value: Boolean);
begin
  FHotTrack := Value;
  Ctl3d := not FHotTrack;
end;

{ This does not recurse it seems }

procedure TJvCustomMemo.Change;
begin
  { only process if maxlines is set - truncate }
  if MaxLines > 0 then
  begin
    while (Lines.Count > MaxLines) do
      Lines.Delete(Lines.Count - 1);
  end;
  inherited;
end;

function TJvCustomMemo.CharOfLine(iLine: integer): integer;
begin
  Result := Perform(EM_LINEINDEX, iLine, 0);
end;

function TJvCustomMemo.GetCurrentLine: integer;
begin
  Result := Perform(EM_LINEFROMCHAR, -1, 0);
end;

procedure TJvCustomMemo.KeyPress(var key: char);
begin
  { only process if maxlines is set }
  if MaxLines > 0 then
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

procedure TJvCustomMemo.LineScroll(ix, iy: integer);
begin
  Perform(EM_LINESCROLL, ix, iy);
end;

procedure TJvCustomMemo.SetCurrentLine(iNewLine: integer);
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

procedure TJvCustomMemo.CaretChanged(sender: TObject);
begin
  FCaret.CreateCaret;
end;

destructor TJvCustomMemo.Destroy;
begin
  if FMaxLines > 0 then
    Lines.Assign(FOrigLines);
  FOrigLines.Free;
  FCaret.Free;
  inherited;
end;

procedure TJvCustomMemo.SetCaret(const Value: TJvCaret);
begin
  FCaret.Assign(Value);
end;

procedure TJvCustomMemo.WMSetFocus(var msg: TMessage);
begin
  inherited;
  FCaret.CreateCaret;
  if FHideCaret then
    Windows.HideCaret(handle);
end;

procedure TJvCustomMemo.SetMaxLines(const Value: integer);
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

function TJvCustomMemo.GetLines: TStrings;
begin
  Result := inherited Lines;
end;

procedure TJvCustomMemo.SetLines(const Value: TStrings);
begin
  inherited SetLines(Value);
  if MaxLines > 0 then
    // save original content
    FOrigLines.Assign(Value);
  Change;
end;

procedure TJvCustomMemo.SetHideCaret(const Value: boolean);
begin
  if FHideCaret <> Value then
    FHideCaret := Value;
end;

procedure TJvCustomMemo.WMKillFocus(var msg: TWMKillFocus);
begin
  if FHideCaret then
    ShowCaret(handle);
end;

procedure TJvCustomMemo.WndProc(var Message: TMessage);
  procedure Scroll(msg, scrollcode: Integer);
  begin
    Perform(msg, scrollcode, 0);
    Perform(msg, SB_ENDSCROLL, 0);
  end;
begin
  if FHideCaret and not (csDesigning in ComponentState) then
  begin
    case Message.Msg of
      WM_LBUTTONDOWN, WM_LBUTTONUP, WM_MOUSEMOVE, WM_LBUTTONDBLCLK,
        WM_CHAR, WM_KEYUP:
        begin
          Message.Result := 0;
          if Message.Msg = WM_LBUTTONDOWN then
            if not Focused then
              SetFocus;
          Exit;
        end;
      WM_KEYDOWN:
        begin
          case Message.WParam of
            VK_DOWN: Scroll(WM_VSCROLL, SB_LINEDOWN);
            VK_UP: Scroll(WM_VSCROLL, SB_LINEUP);
            VK_LEFT: Scroll(WM_HSCROLL, SB_LINELEFT);
            VK_RIGHT: Scroll(WM_HSCROLL, SB_LINERIGHT);
            VK_NEXT: Scroll(WM_VSCROLL, SB_PAGEDOWN);
            VK_PRIOR: Scroll(WM_VSCROLL, SB_PAGEUP);
            VK_HOME: Scroll(WM_VSCROLL, SB_TOP);
            VK_END: Scroll(WM_VSCROLL, SB_BOTTOM);
          end;
          Message.Result := 0;
          Exit;
        end;
    end;
  end;
  inherited;
end;

procedure TJvCustomMemo.WMCopy(var Msg: TWMCopy);
begin
  if caCopy in ClipboardCommands then
    inherited;
end;

procedure TJvCustomMemo.WMCut(var Msg: TWMCut);
begin
  if caCut in ClipboardCommands then
    inherited;
end;

procedure TJvCustomMemo.WMPaste(var Msg: TWMPaste);
begin
  if caPaste in ClipboardCommands then
    inherited;
end;

procedure TJvCustomMemo.WMUndo(var Msg: TWMUndo);
begin
  if caUndo in ClipboardCommands then
    inherited;
end;

function TJvCustomMemo.GetReadOnly: boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TJvCustomMemo.SetReadOnly(const Value: boolean);
begin
  inherited ReadOnly := Value;
  if Value then
    FClipBoardCommands := [caCopy];
end;


procedure TJvCustomMemo.SetClipboardCommands(
  const Value: TJvClipBoardCommands);
begin
  if FClipBoardCommands <> Value then
  begin
    FClipBoardCommands := Value;
    ReadOnly := FClipBoardCommands <= [caCopy];
  end;
end;

end.

