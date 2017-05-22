{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113 dott 1101 att compuserve dott com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvPopupMemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  JVCLVer;

type
  TJvPopupMemo = class(TMemo)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FDesigntimeHeight: Integer;
    FFocusedHeight: Integer;
    FMaximumHeight: Integer;
    FCanvas: TControlCanvas;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure SetFocusedHeight(const Value: Integer);
    procedure SetMaximumHeight(const Value: Integer);
    procedure UpdateHeight;
    procedure ChangeScrollbar(Value: TScrollStyle);
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Change; override;
    procedure AdjustHeight;
    property Canvas: TControlCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FocusedHeight: Integer read FFocusedHeight write SetFocusedHeight;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property MaximumHeight: Integer read FMaximumHeight write SetMaximumHeight;
  end;

implementation

constructor TJvPopupMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFocusedHeight := Height;
  FMaximumHeight := 5 * Height;
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
end;

destructor TJvPopupMemo.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvPopupMemo.AdjustHeight;
const
  AlignFlags: array [TAlignment] of DWORD =
    (DT_LEFT, DT_CENTER, DT_RIGHT);
var
  OldRect, NewRect: TRect;
  NewHeight: Integer;
  S: string;
begin
  if not HandleAllocated then
    Exit;

  Perform(EM_GETRECT, 0, lparam(@OldRect));
  S := Text;

  { HACK ALERT! DrawText discards a trailing linebreak for measurement, so if
    the user hits return in the control and the new Line would require a
    larger memo we do not get the correct Value back. To fix that we add a
    blank just for the measurement if the last character is a linefeed. }
  if (Length(S) > 0) and (S[Length(S)] = #10) then
    S := S + ' ';
  Canvas.Font := Font;
  NewRect := OldRect;
  DrawText(Canvas.Handle, Pchar(S), Length(S), NewRect,
    DT_CALCRECT or DT_EDITCONTROL or DT_WORDBREAK or DT_NOPREFIX or
    DT_EXPANDTABS or
    AlignFlags[Alignment]);
  if OldRect.Bottom <> NewRect.Bottom then
  begin
    NewHeight := Height - (OldRect.Bottom - OldRect.Top) +
      (NewRect.Bottom - NewRect.Top);
    if NewHeight > MaximumHeight then
      ChangeScrollbar(ssVertical)
    else
      ChangeScrollbar(ssNone);
    FocusedHeight := NewHeight;
  end;
end;

procedure TJvPopupMemo.Change;
begin
  AdjustHeight;
  inherited Change;
end;

procedure TJvPopupMemo.ChangeScrollbar(Value: TScrollStyle);
var
  OldPos: Integer;
begin
  if ScrollBars <> Value then
  begin
    { Changing the scrollbar recreates the window and looses the caret
      position! }
    OldPos := SelStart;
    ScrollBars := Value;
    SelStart := OldPos;
    Perform(EM_SCROLLCARET, 0, 0);
  end;
end;

procedure TJvPopupMemo.CMTextChanged(var Msg: TMessage);
begin
  AdjustHeight;
  inherited;
end;

procedure TJvPopupMemo.DoEnter;
begin
  inherited DoEnter;
  FDesigntimeHeight := Height;
  UpdateHeight;
  // Height := FFocusedHeight;
end;

procedure TJvPopupMemo.DoExit;
begin
  inherited DoExit;
  Height := FDesigntimeHeight;
end;

procedure TJvPopupMemo.SetFocusedHeight(const Value: Integer);
begin
  if FFocusedHeight <> Value then
  begin
    if Value > MaximumHeight then
      FFocusedHeight := MaximumHeight
    else
      FFocusedHeight := Value;
    if Focused then
      UpdateHeight;
  end;
end;

procedure TJvPopupMemo.SetMaximumHeight(const Value: Integer);
begin
  if FMaximumHeight <> Value then
  begin
    FMaximumHeight := Value;
    if Value < FocusedHeight then
      FocusedHeight := Value;
  end;
end;

procedure TJvPopupMemo.UpdateHeight;
var
  Line: Integer;
begin
  if HandleAllocated and Focused then
  begin
    Height := FocusedHeight;
    if ScrollBars = ssNone then
    begin
      Line := Perform(EM_GETFIRSTVISIBLELINE, 0, 0);
      if Line > 0 then
        Perform(EM_LINESCROLL, 0, -Line);
    end;
  end;
end;

end.

