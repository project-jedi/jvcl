{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSticker.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSticker;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Forms,
  JvComponent;

type
  TJvStickSizer = class(TJvCustomControl)
  private
    FControl: TControl;
    FRectList: array [1..8] of TRect;
    procedure WMNCHitTest(var Msg: TWMNCHitTest);  message WM_NCHITTEST;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;
  protected
    procedure DoBoundsChanged; override;
  public
    constructor CreateEx(AOwner: TComponent; AControl: TControl);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateHandle; override;
    procedure Paint; override;
    procedure SizerControlExit(Sender: TObject);
  end;

  TJvSticker = class(TJvGraphicControl)
  private
    FStickColor: TColor;
    procedure SetStickColor(const Value: TColor);
    function CaptionDialog(S: string): string;
  protected
    procedure FontChanged; override;
    procedure TextChanged; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property StickColor: TColor read FStickColor write SetStickColor default clYellow;
    property Align;
    property Caption;
    property Font;
    property Height default 65;
    property PopupMenu;
    property Width default 65;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Dialogs,
  JvConsts, JvResources;

//=== { TJvStickSizer } ======================================================

const
  cPosList: array [1..8] of Integer =
    (HTTOPLEFT, HTTOP, HTTOPRIGHT, HTRIGHT,
     HTBOTTOMRIGHT, HTBOTTOM, HTBOTTOMLEFT, HTLEFT);

constructor TJvStickSizer.CreateEx(AOwner: TComponent; AControl: TControl);
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
  // Create the list of positions
end;

procedure TJvStickSizer.CreateHandle;
begin
  inherited CreateHandle;
  SetFocus;
end;

procedure TJvStickSizer.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
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

procedure TJvStickSizer.WMNCHitTest(var Msg: TWMNCHitTest);
var
  Pt: TPoint;
  I: Integer;
begin
  Pt := Point(Msg.XPos, Msg.YPos);
  Pt := ScreenToClient(Pt);
  Msg.Result := 0;
  for I := Low(FRectList) to High(FRectList) do
    if PtInRect(FRectList[I], Pt) then
      Msg.Result := cPosList[I];
  // if the return value was not set
  if Msg.Result = 0 then
    inherited;
end;

procedure TJvStickSizer.DoBoundsChanged;
var
  R: TRect;
begin
  R := BoundsRect;
  InflateRect(R, -2, -2);
  FControl.BoundsRect := R;
  // setup data structures
  FRectList[1] := Rect(0, 0, 5, 5);
  FRectList[2] := Rect(Width div 2 - 3, 0, Width div 2 + 2, 5);
  FRectList[3] := Rect(Width - 5, 0, Width, 5);
  FRectList[4] := Rect(Width - 5, Height div 2 - 3, Width, Height div 2 + 2);
  FRectList[5] := Rect(Width - 5, Height - 5, Width, Height);
  FRectList[6] := Rect(Width div 2 - 3, Height - 5, Width div 2 + 2, Height);
  FRectList[7] := Rect(0, Height - 5, 5, Height);
  FRectList[8] := Rect(0, Height div 2 - 3, 5, Height div 2 + 2);
end;

procedure TJvStickSizer.SizerControlExit(Sender: TObject);
begin
  Free;
end;

procedure TJvStickSizer.WMLButtonDown(var Msg: TWMLButtonDown);
begin
  Perform(WM_SYSCOMMAND, SC_DRAGMOVE, 0);
end;

procedure TJvStickSizer.WMMove(var Msg: TWMMove);
var
  R: TRect;
begin
  R := BoundsRect;
  InflateRect(R, -2, -2);
  FControl.Invalidate; // repaint entire surface
  FControl.BoundsRect := R;
end;

//=== { TJvSticker } =========================================================

constructor TJvSticker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 65;
  Height := 65;
  FStickColor := clYellow;
end;

procedure TJvSticker.FontChanged;
begin
  inherited FontChanged;
  Invalidate;
end;

procedure TJvSticker.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  // Cursor := crHandPoint;
  inherited MouseEnter(Control);
end;

procedure TJvSticker.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  Cursor := crDefault;
  inherited MouseLeave(Control);
end;

procedure TJvSticker.TextChanged;
begin
  inherited TextChanged;
  Invalidate;
end;

function TJvSticker.CaptionDialog(S: string): string;
var
  Form: TForm;
  Memo: TMemo;
begin
  Result := '';
  Form := TForm.Create(Self);
  try
    Form.Width := 350;
    Form.Height := 200;
    Form.BorderStyle := bsDialog;
    Form.Caption := RsEditStickerCaption;
    Memo := TMemo.Create(Form);
    with Memo do
    begin
      Align := alClient;
      Font.Size := 10;
      ScrollBars := ssVertical;
      Text := S;
      Parent := Form;
    end;
    Form.Position := poDesktopCenter;
    Form.ShowModal;
    Result := Memo.Text;
  finally
    Form.Free;
  end;
end;

procedure TJvSticker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  H3: Integer;
begin
  inherited;
  H3 := Height div 3;
  if (Button = mbLeft) and (PtInRect(Rect(0, 0, 20, H3), Point(X, Y))) then
    with TJvStickSizer.CreateEx(Self.Parent, TControl(Self)) do
      Parent := Self.Parent
  else
  if (Button = mbLeft) and (PtInRect(Rect(0, H3, 20, 2 * H3), Point(X, Y))) then
    Caption := CaptionDialog(Caption)
  else
  if (Button = mbLeft) and (PtInRect(Rect(0, 2 * H3, 20, ClientHeight), Point(X, Y))) then
    with TColorDialog.Create(Self) do
    begin
      Color := FStickColor;
      if Execute then
        StickColor := Color;
      Free;
    end
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvSticker.Paint;
var
  R: TRect;
  S: string;
  H3: Integer;
  I: Integer;
begin
  inherited;
  H3 := Height div 3;
  Canvas.Brush.Color := StickColor;
  Canvas.FillRect(Rect(15, 0, Width, Height));
  // draw grips
  Canvas.Brush.Color := clSilver;
  Canvas.FillRect(Rect(0, 0, 15, Height));
  // size grip
  for I := 1 to 4 do
  begin
    Canvas.Pen.Color := clWhite;
    Canvas.MoveTo(I * 3, 3);
    Canvas.LineTo(I * 3, H3 - 2);
    Canvas.Pen.Color := clBtnShadow;
    Canvas.MoveTo(I * 3 + 1, 3);
    Canvas.LineTo(I * 3 + 1, H3 - 2);
  end;
  // edit grip
  for I := 1 to 4 do
  begin
    Canvas.Pen.Color := clWhite;
    Canvas.MoveTo(I * 3, H3 + 2);
    Canvas.LineTo(I * 3, 2 * H3 - 2);
    Canvas.Pen.Color := clNavy;
    Canvas.MoveTo(I * 3 + 1, H3 + 2);
    Canvas.LineTo(I * 3 + 1, 2 * H3 - 2);
  end;
  // Color grip
  for I := 1 to 4 do
  begin
    Canvas.Pen.Color := clWhite;
    Canvas.MoveTo(I * 3, 2 * H3 + 2);
    Canvas.LineTo(I * 3, Height - 3);
    Canvas.Pen.Color := clMaroon;
    Canvas.MoveTo(I * 3 + 1, 2 * H3 + 2);
    Canvas.LineTo(I * 3 + 1, Height - 3);
  end;
  R := Rect(15, 0, Width, Height);
  S := Caption;
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(Font);
  DrawText(Canvas.Handle, PChar(S), -1, R, DT_WORDBREAK);
end;

procedure TJvSticker.SetStickColor(const Value: TColor);
begin
  if FStickColor <> Value then
  begin
    FStickColor := Value;
    Invalidate;
  end;
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
