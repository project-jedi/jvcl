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
    * To simulate the behaviour of JvDisplayMemo, set HideCaret to True,
      Readonly to True, Color to $C0FFFF and Cursor to crArrow
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JvCaret, JVCLVer, JvComponent;

const
  WM_AUTOBAR = WM_USER + 43;

type
  TJvCustomMemo = class(TCustomMemo)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FMaxLines: Integer;
    FHotTrack: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FHintColor: TColor;
    FSaved: TColor;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOver: Boolean;
    FOnHorizontalScroll: TNotifyEvent;
    FOnVerticalScroll: TNotifyEvent;
    FCaret: TJvCaret;
    FHideCaret: Boolean;
    FOrigLines: TStrings;
    FClipboardCommands: TJvClipboardCommands;
    procedure SetHotTrack(const Value: Boolean);
    procedure CaretChanged(Sender: TObject); dynamic;
    procedure SetCaret(const Value: TJvCaret);
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMPaste(var Msg: TWMPaste); message WM_PASTE;
    procedure WMCopy(var Msg: TWMCopy); message WM_COPY;
    procedure WMCut(var Msg: TWMCut); message WM_CUT;
    procedure WMUndo(var Msg: TWMUndo); message WM_UNDO;
    procedure SetMaxLines(const Value: Integer);
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
    procedure SetHideCaret(const Value: Boolean);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    procedure SetClipboardCommands(const Value: TJvClipboardCommands);
  protected
    procedure WndProc(var Msg: TMessage); override;
    procedure KeyPress(var Key: Char); override;
    procedure Change; override;
    function GetCurrentLine: Integer;
    procedure SetCurrentLine(iNewLine: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { these wrap the windows messages }
    procedure LineScroll(ix, iy: Integer);
    function CharOfLine(iLine: Integer): Integer;
    property CurrentLine: Integer read GetCurrentLine write SetCurrentLine;
  protected
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property ClipboardCommands: TJvClipboardCommands read FClipboardCommands write SetClipboardCommands default
      [caCopy..caUndo];
    property Caret: TJvCaret read FCaret write SetCaret;
    property HideCaret: Boolean read FHideCaret write SetHideCaret;
    property MaxLines: Integer read FMaxLines write SetMaxLines;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property Lines: TStrings read GetLines write SetLines;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnVerticalScroll: TNotifyEvent read FOnVerticalScroll write FOnVerticalScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHorizontalScroll write FOnHorizontalScroll;
  end;

  TJvMemo = class(TJvCustomMemo)
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

function GetFontHeight(phFont: hFont): Integer;
var
  hWin: HDC;
  tm: TTextMetric;
  hOldFont: HFONT;
begin
  Result := -1;

  if phFont = 0 then
    Exit;

  hWin := GetDC(0); // use the main screen window Handle

  { according to the API help for GetDC, a return value of 'null' is error }
  if hWin = 0 then
    Exit;

  try
    hOldFont := SelectObject(hWin, phFont);
    GetTextMetrics(hWin, tm);
    SelectObject(hWin, hOldFont);
  finally
    ReleaseDC(0, hWin);
  end;

  Result := tm.tmHeight;
end;

function AddVScrollbar(Sb: TScrollStyle): TScrollStyle;
begin
  if Sb = ssHorizontal then
    Result := ssBoth
  else
    Result := ssVertical;
end;

function RemoveVScrollbar(Sb: TScrollStyle): TScrollStyle;
begin
  if Sb = ssBoth then
    Result := ssHorizontal
  else
    Result := ssNone;
end;

constructor TJvCustomMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOrigLines := TStringlist.Create;
  FHintColor := clInfoBk;
  FHotTrack := False;
  FOver := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FCaret := TJvCaret.Create(self);
  FCaret.OnChanged := CaretChanged;
  FClipboardCommands := [caCopy..caUndo];
end;

destructor TJvCustomMemo.Destroy;
begin
  if FMaxLines > 0 then
    Lines.Assign(FOrigLines);
  FOrigLines.Free;
  FCaret.Free;
  inherited Destroy;
end;

procedure TJvCustomMemo.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvCustomMemo.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvCustomMemo.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnHorizontalScroll) then
    FOnHorizontalScroll(Self);
end;

procedure TJvCustomMemo.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVerticalScroll) then
    FOnVerticalScroll(Self);
end;

procedure TJvCustomMemo.CMMouseEnter(var Msg: TMessage);
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FHintColor;
    if FHotTrack then
      Ctl3D := True;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvCustomMemo.CMMouseLeave(var Msg: TMessage);
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FHotTrack then
      Ctl3D := False;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvCustomMemo.SetHotTrack(const Value: Boolean);
begin
  FHotTrack := Value;
  Ctl3D := not FHotTrack;
end;

{ This does not recurse it seems }

procedure TJvCustomMemo.Change;
begin
  { only process if maxlines is set - truncate }
  if MaxLines > 0 then
  begin
    while Lines.Count > MaxLines do
      Lines.Delete(Lines.Count - 1);
  end;
  inherited Change;
end;

function TJvCustomMemo.CharOfLine(iLine: Integer): Integer;
begin
  Result := Perform(EM_LINEINDEX, iLine, 0);
end;

function TJvCustomMemo.GetCurrentLine: Integer;
begin
  Result := Perform(EM_LINEFROMCHAR, -1, 0);
end;

procedure TJvCustomMemo.KeyPress(var Key: Char);
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
      if (CurrentLine >= MaxLines) and not (Key = AnsiBackSpace) then
        Key := #0;
    end;
  end;

  inherited;
end;

procedure TJvCustomMemo.LineScroll(ix, iy: Integer);
begin
  Perform(EM_LINESCROLL, ix, iy);
end;

procedure TJvCustomMemo.SetCurrentLine(iNewLine: Integer);
var
  iDelta: Integer;
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

procedure TJvCustomMemo.CaretChanged(Sender: TObject);
begin
  FCaret.CreateCaret;
end;

procedure TJvCustomMemo.SetCaret(const Value: TJvCaret);
begin
  FCaret.Assign(Value);
end;

procedure TJvCustomMemo.WMSetFocus(var Msg: TMessage);
begin
  inherited;
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

procedure TJvCustomMemo.SetHideCaret(const Value: Boolean);
begin
  if FHideCaret <> Value then
    FHideCaret := Value;
end;

procedure TJvCustomMemo.WMKillFocus(var Msg: TWMKillFocus);
begin
  if FHideCaret then
    ShowCaret(Handle);
  FCaret.DestroyCaret;
  inherited;
end;

procedure TJvCustomMemo.WndProc(var Msg: TMessage);

  procedure Scroll(Msg, ScrollCode: Integer);
  begin
    Perform(Msg, ScrollCode, 0);
    Perform(Msg, SB_ENDSCROLL, 0);
  end;

begin
  if FHideCaret and not (csDesigning in ComponentState) then
  begin
    case Msg.Msg of
      WM_LBUTTONDOWN, WM_LBUTTONUP, WM_MOUSEMOVE, WM_LBUTTONDBLCLK,
        WM_CHAR, WM_KEYUP:
        begin
          Msg.Result := 0;
          if Msg.Msg = WM_LBUTTONDOWN then
            if not Focused then
              SetFocus;
          Exit;
        end;
      WM_KEYDOWN:
        begin
          case Msg.WParam of
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
            VK_HOME: Scroll(WM_VSCROLL, SB_TOP);
            VK_END:
              Scroll(WM_VSCROLL, SB_BOTTOM);
          end;
          Msg.Result := 0;
          Exit;
        end;
    end;
  end;
  inherited WndProc(Msg);
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

function TJvCustomMemo.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TJvCustomMemo.SetReadOnly(const Value: Boolean);
begin
  inherited ReadOnly := Value;
  if Value then
    FClipboardCommands := [caCopy];
end;

procedure TJvCustomMemo.SetClipboardCommands(
  const Value: TJvClipboardCommands);
begin
  if FClipboardCommands <> Value then
  begin
    FClipboardCommands := Value;
    ReadOnly := FClipboardCommands <= [caCopy];
  end;
end;

end.

