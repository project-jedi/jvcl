{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSticker.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}
unit JvSticker;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, stdctrls, Forms, Dialogs;

const
  sc_DragMove: Longint = $F012;

type

  TJvStickSizer = class(TCustomControl)
  private
    FControl: TControl;
    FRectList: array[1..8] of TRect;
    FPosList: array[1..8] of Integer;
  protected

  public
    constructor CreateEx(AOwner: TComponent;
      AControl: TControl);
    procedure CreateParams(var Params: TCreateParams);
      override;
    procedure CreateHandle; override;
    procedure WmNcHitTest(var Msg: TWmNcHitTest);
      message wm_NcHitTest;
    procedure WmSize(var Msg: TWmSize);
      message wm_Size;
    procedure WmLButtonDown(var Msg: TWmLButtonDown);
      message wm_LButtonDown;
    procedure WmMove(var Msg: TWmMove);
      message wm_Move;
    procedure Paint; override;
    procedure SizerControlExit(Sender: TObject);
  end;

  TJvSticker = class(TGraphicControl)
  private
    FStickColor: TColor;
    procedure SetStickColor(const Value: TColor);
    function captionDialog(s: string): string;
    { Private declarations }
  protected
    { Protected declarations }
    procedure CMMouseLeave(var Msg: TMessage); message CM_MouseLeave;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MouseEnter;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  public
    { Public declarations }
    constructor create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    { Published declarations }
    property StickColor: TColor read FStickColor write SetStickColor;
    property Align;
    property Caption;
    property Font;
    property popupmenu;
  end;

implementation

{ TJvStickSizer }

// TJvStickSizer methods

constructor TJvStickSizer.CreateEx(
  AOwner: TComponent; AControl: TControl);
var
  R: TRect;
begin
  inherited Create(AOwner);
  FControl := AControl;
  // install the new handler
  OnExit := SizerControlExit;
  // set the size and position
  R := FControl.BoundsRect;
  InflateRect(R, 2, 2);
  BoundsRect := R;
  // set the parent
  Parent := FControl.Parent;
  // create the list of positions
  FPosList[1] := htTopLeft;
  FPosList[2] := htTop;
  FPosList[3] := htTopRight;
  FPosList[4] := htRight;
  FPosList[5] := htBottomRight;
  FPosList[6] := htBottom;
  FPosList[7] := htBottomLeft;
  FPosList[8] := htLeft;
end;

procedure TJvStickSizer.CreateHandle;
begin
  inherited CreateHandle;
  SetFocus;
end;

procedure TJvStickSizer.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle +
    ws_ex_Transparent;
end;

procedure TJvStickSizer.Paint;
var
  I: Integer;
begin
  Canvas.Brush.Color := clBlack;
  for I := 1 to 8 do
    Canvas.Rectangle(FRectList[I].Left, FRectList[I].Top,
      FRectList[I].Right, FRectList[I].Bottom);
end;

procedure TJvStickSizer.WmNcHitTest(var Msg: TWmNcHitTest);
var
  Pt: TPoint;
  I: Integer;
begin
  Pt := Point(Msg.XPos, Msg.YPos);
  Pt := ScreenToClient(Pt);
  Msg.Result := 0;
  for I := 1 to 8 do
    if PtInRect(FRectList[I], Pt) then
      Msg.Result := FPosList[I];
  // if the return value was not set
  if Msg.Result = 0 then
    inherited;
end;

procedure TJvStickSizer.WmSize(var Msg: TWmSize);
var
  R: TRect;
begin
  R := BoundsRect;
  InflateRect(R, -2, -2);
  FControl.BoundsRect := R;
  // setup data structures
  FRectList[1] := Rect(0, 0, 5, 5);
  FRectList[2] := Rect(Width div 2 - 3, 0,
    Width div 2 + 2, 5);
  FRectList[3] := Rect(Width - 5, 0, Width, 5);
  FRectList[4] := Rect(Width - 5, Height div 2 - 3,
    Width, Height div 2 + 2);
  FRectList[5] := Rect(Width - 5, Height - 5,
    Width, Height);
  FRectList[6] := Rect(Width div 2 - 3, Height - 5,
    Width div 2 + 2, Height);
  FRectList[7] := Rect(0, Height - 5, 5, Height);
  FRectList[8] := Rect(0, Height div 2 - 3,
    5, Height div 2 + 2);
end;

procedure TJvStickSizer.SizerControlExit(Sender: TObject);
begin
  Free;
end;

procedure TJvStickSizer.WmLButtonDown(var Msg: TWmLButtonDown);
begin
  Perform(wm_SysCommand, sc_DragMove, 0);
end;

procedure TJvStickSizer.WmMove(var Msg: TWmMove);
var
  R: TRect;
begin
  R := BoundsRect;
  InflateRect(R, -2, -2);
  FControl.Invalidate; // repaint entire surface
  FControl.BoundsRect := R;
end;

{ TJvSticker }

procedure TJvSticker.CMFontChanged(var Message: TMessage);
begin
  invalidate;
end;

procedure TJvSticker.CMMouseEnter(var Msg: TMessage);
begin
  // cursor:=crhandpoint;
end;

procedure TJvSticker.CMMouseLeave(var Msg: TMessage);
begin
  cursor := crdefault;
end;

procedure TJvSticker.CMTextChanged(var Message: TMessage);
begin
  invalidate;
end;

constructor TJvSticker.create(AOwner: TComponent);
begin
  inherited;
  width := 65;
  height := 65;
  FStickColor := clyellow;
end;

function TJvSticker.captionDialog(s: string): string;
var
  frm: TForm;
  mem: Tmemo;
begin
  frm := TForm.Create(self);
  frm.width := 350;
  frm.height := 200;
  frm.BorderStyle := bsdialog;
  frm.caption := 'Edit sticker';
  mem := Tmemo.create(frm);
  with mem do
  begin
    align := alclient;
    font.size := 10;
    scrollbars := ssvertical;
    text := s;
    parent := frm;
  end;
  frm.position := podesktopcenter;
  frm.ShowModal;
  result := mem.text;
  frm.free;
end;

procedure TJvSticker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  h3: integer;
begin
  inherited;
  h3 := height div 3;
  if (button = mbleft) and (ptinrect(rect(0, 0, 20, h3), point(x, y))) then
  begin
    with TJvStickSizer.CreateEx(self.Parent, TControl(self)) do
      parent := self.parent;
  end
  else if (button = mbleft) and (ptinrect(rect(0, h3, 20, 2 * h3), point(x, y))) then
  begin
    caption := captiondialog(caption);
  end
  else if (button = mbleft) and (ptinrect(rect(0, 2 * h3, 20, clientheight), point(x, y))) then
  begin
    with TColordialog.Create(self) do
    begin
      color := FstickColor;
      if execute then StickColor := color;
      free;
    end;
  end
  else if assigned(onMouseDown) then
    onMouseDown(self, button, shift, x, y);
end;

procedure TJvSticker.Paint;
var
  R: TRect;
  s: string;
  h3: integer;
  i: integer;
begin
  inherited;
  h3 := height div 3;
  canvas.brush.color := StickColor;
  canvas.fillrect(Rect(15, 0, width, height));
  // draw grips
  canvas.brush.color := clSilver;
  canvas.fillrect(Rect(0, 0, 15, height));
  // size grip
  for i := 1 to 4 do
  begin
    canvas.pen.color := clwhite;
    canvas.MoveTo(i * 3, 3);
    canvas.lineto(i * 3, h3 - 2);
    canvas.pen.color := clbtnshadow;
    canvas.MoveTo(i * 3 + 1, 3);
    canvas.lineto(i * 3 + 1, h3 - 2);
  end;
  // edit grip
  for i := 1 to 4 do
  begin
    canvas.pen.color := clwhite;
    canvas.MoveTo(i * 3, h3 + 2);
    canvas.lineto(i * 3, 2 * h3 - 2);
    canvas.pen.color := clnavy;
    canvas.MoveTo(i * 3 + 1, h3 + 2);
    canvas.lineto(i * 3 + 1, 2 * h3 - 2);
  end;
  // color grip
  for i := 1 to 4 do
  begin
    canvas.pen.color := clwhite;
    canvas.MoveTo(i * 3, 2 * h3 + 2);
    canvas.lineto(i * 3, height - 3);
    canvas.pen.color := clmaroon;
    canvas.MoveTo(i * 3 + 1, 2 * h3 + 2);
    canvas.lineto(i * 3 + 1, height - 3);
  end;
  R := Rect(15, 0, width, height);
  s := caption;
  canvas.brush.style := bsclear;
  canvas.Font.assign(Font);
  DrawText(canvas.handle, pchar(s), -1, R, DT_WORDBREAK);
end;

procedure TJvSticker.SetStickColor(const Value: TColor);
begin
  FStickColor := Value;
  invalidate;
end;

end.
