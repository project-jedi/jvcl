{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, JVCLVer;

type
  TJvPopupMemo = class(TMemo)
  private
    { Private declarations }
    FDesigntimeHeight: Integer;
    FFocusedHeight: Integer;
    FMaximumHeight: Integer;
    FCanvas: TControlCanvas;
    FAboutJVCL: TJVCLAboutInfo;

    procedure CMTextChanged(var msg: TMessage); message CM_TEXTCHANGED;
    procedure SetFocusedHeight(const Value: Integer);
    procedure SetMaximumHeight(const Value: Integer);
    procedure UpdateHeight;
    procedure ChangeScrollbar(value: TScrollStyle);
  protected
    { Protected declarations }
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Change; override;
    procedure AdjustHeight;
    property Canvas: TControlCanvas read FCanvas;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property FocusedHeight: Integer read FFocusedHeight write SetFocusedHeight;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property MaximumHeight: Integer read FMaximumHeight write SetMaximumHeight;
  end;

implementation

{ TJvPopupMemo }

procedure TJvPopupMemo.AdjustHeight;
const
  alignflags: array[TAlignment] of DWORD =
  (DT_LEFT, DT_CENTER, DT_RIGHT);
var
  oldrect, newrect: TRect;
  newheight: Integer;
  S: string;
begin
  if not HandleAllocated then
    Exit;

  Perform(EM_GETRECT, 0, lparam(@oldrect));
  S := Text;

  { HACK ALERT! DrawText discards a trailing linebreak for measurement, so if
    the user hits return in the control and the new line would require a
    larger memo we do not get the correct value back. To fix that we add a
    blank just for the measurement if the last character is a linefeed. }
  if (Length(S) > 0) and (S[Length(S)] = #10) then
    S := S + ' ';
  Canvas.Font := Font;
  newrect := oldrect;
  DrawText(Canvas.Handle, Pchar(S), Length(S), newrect,
    DT_CALCRECT or DT_EDITCONTROL or DT_WORDBREAK or DT_NOPREFIX or
    DT_EXPANDTABS or
    alignflags[Alignment]);
  if oldrect.bottom <> newrect.bottom then
  begin
    newHeight := Height - (oldrect.bottom - oldrect.top) +
      (newrect.bottom - newrect.top);
    if newHeight > MaximumHeight then
      ChangeScrollbar(ssVertical)
    else
      ChangeScrollbar(ssNone);
    FocusedHeight := newHeight;
  end;
end;

procedure TJvPopupMemo.Change;
begin
  AdjustHeight;
  inherited;
end;

procedure TJvPopupMemo.ChangeScrollbar(value: TScrollStyle);
var
  oldpos: Integer;
begin
  if Scrollbars <> value then
  begin
    { Changing the scrollbar recreates the window and looses the caret
      position! }
    oldpos := SelStart;
    Scrollbars := value;
    SelStart := oldpos;
    Perform(EM_SCROLLCARET, 0, 0);
  end;
end;

procedure TJvPopupMemo.CMTextChanged(var msg: TMessage);
begin
  AdjustHeight;
  inherited;
end;

constructor TJvPopupMemo.Create(aOwner: TComponent);
begin
  inherited;
  FFocusedHeight := Height;
  FMaximumHeight := 5 * Height;
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
end;

destructor TJvPopupMemo.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TJvPopupMemo.DoEnter;
begin
  inherited;
  FDesigntimeHeight := Height;
  UpdateHeight;
  // Height := FFocusedHeight;
end;

procedure TJvPopupMemo.DoExit;
begin
  inherited;
  Height := FDesigntimeHeight;
end;

procedure TJvPopupMemo.SetFocusedHeight(const Value: Integer);
begin
  if FFocusedHeight <> Value then
  begin
    if Value > MaximumHeight then
      FFocusedHeight := MaximumHeight
    else
      FFocusedHeight := value;
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
  line: Integer;
begin
  if HandleAllocated and Focused then
  begin
    Height := FocusedHeight;
    if Scrollbars = ssNone then
    begin
      line := Perform(EM_GETFIRSTVISIBLELINE, 0, 0);
      if line > 0 then
        Perform(EM_LINESCROLL, 0, -line);
    end;
  end;
end;

end.
