{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorBox.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ support classes for the (TJvColorButton) }

unit JvColorBox;

{
  TJvColorBox:
    A color selectionbox that works just like the one in Win95/NT 4.0

  TJvDropButton:
    A CustomControl with droparrow:
     assign a TPopUpMenu to the DropDown property and the menu will be shown when the
     button is clicked

  ...combine the two and you get:

  TColorButton:
    A button that looks and behaves like the one in Win95 / NT 4.0
    -> this has been moved to another unit, ColorBtn

  only the TColorButton is installed, but changing the Register procedure
  would allow for the others to be installed too
  It's your choice...

}

interface

uses
  Windows, Messages, Forms, SysUtils, Classes, Graphics, Controls,
  Dialogs, ExtCtrls, StdCtrls, Buttons, Menus, JvComponent;

{ a square with a sunken frame and a color, sets Color when clicked }
{ draws a frame when active (MouseEnter)                            }
type
  TJvColorClickEvent = procedure(Sender: TObject; Button: TMouseButton; Color: TColor) of object;

  TJvColorSquare = class(TJvGraphicControl)
  private
    FColor: TColor;
    FInside: Boolean;
    FBorderStyle: TBorderStyle;
    FOnChange: TNotifyEvent;
    FColorClick: TJvColorClickEvent;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetColor(Value: TColor);
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure DrawFocusFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color: TColor read FColor write SetColor default clWhite;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnColorClick: TJvColorClickEvent read FColorClick write FColorClick;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { a window with 20 Colorsquares and a button that activates a TColorDialog... }
  TJvColorBox = class(TJvCustomControl)
  private
    FColorClick: TJvColorClickEvent;
    FBorderStyle: TBorderStyle;
    FSquares: array [1..20] of TJvColorSquare;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure DrawColorBoxes;
  protected
    procedure Paint; override;
    procedure DoExit; override;
    procedure DoEnter; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ColorClicked(Sender: TObject; Button: TMouseButton; Color: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property Visible;
    property Top;
    property Width;
    property Height;
    property Left;
    property Enabled;
    property OnEnter;
    property OnExit;
    property OnClick;
    property OnColorClick: TJvColorClickEvent read FColorClick write FColorClick;
    property OnKeyDown;
    property OnKeyUp;
  end;

  TJvCustomDropButton = class(TJvCustomControl)
  private
    FAssociate: TPopupMenu;
    FIsDown: Boolean;
    FArrowWidth: Integer;
    procedure SetArrowWidth(Value: Integer);
    procedure SetAssociate(Value: TPopupMenu);
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    property DropDown: TPopupMenu read FAssociate write SetAssociate;
    property ArrowWidth: Integer read FArrowWidth write SetArrowWidth default 13;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvDropButton = class(TJvCustomDropButton)
  published
    property ArrowWidth;
    property DropDown;
    property OnEnter;
    property OnExit;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
  end;

implementation

const
  clBeige = TColor($C6DEC6);
  clSky = TColor($F7CEA5);
  clCream = TColor($F7FFFF);

  Colors: array [1..20] of TColor =
    (clWhite, clBlack, clSilver, clDkGray,
     clRed, clMaroon, clYellow, clOlive,
     clLime, clGreen, clAqua, clTeal,
     clBlue, clNavy, clFuchsia, clPurple,
     clBeige, clSky, clCream, clGray);

procedure DrawLine(Canvas: TCanvas; X, Y, X2, Y2: Integer);
begin
  Canvas.MoveTo(X, Y);
  Canvas.LineTo(X2, Y2);
end;

//=== TJvColorSquare =========================================================

constructor TJvColorSquare.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorderStyle := bsSingle;
  FColor := clWhite;
  Width := 18;
  Height := 18;
  FInside := False;
end;

destructor TJvColorSquare.Destroy;
begin
  inherited Destroy;
end;

procedure TJvColorSquare.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Repaint;
  end;
end;

procedure TJvColorSquare.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
    Repaint;
  end;
end;

procedure TJvColorSquare.Paint;
var
  Rect: TRect;
begin
  Rect := ClientRect;
  if FBorderStyle = bsSingle then
  begin
    Frame3d(Canvas, Rect, clBtnFace, clBtnFace, 1);
    Frame3d(Canvas, Rect, clBtnShadow, clBtnHighLight, 1);
    Frame3d(Canvas, Rect, cl3DDkShadow, clBtnFace, 1);
  end;

  with Canvas do
  begin
    Brush.Color := FColor;
    Brush.Style := bsSolid;
    FillRect(Rect);
  end;
  DrawFocusFrame;
end;

procedure TJvColorSquare.DrawFocusFrame;
var
  Rect: TRect;
begin
  if FInside and Enabled then
  begin
    Rect := ClientRect;
    Frame3d(Canvas, Rect, cl3DDkShadow, cl3DDkShadow, 1);
    Frame3d(Canvas, Rect, clBtnHighLight, clBtnHighLight, 1);
    Frame3d(Canvas, Rect, cl3DDkShadow, cl3DDkShadow, 1);
  end;
end;

procedure TJvColorSquare.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  FInside := True;
  Repaint;
end;

procedure TJvColorSquare.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  FInside := False;
  Repaint;
end;

procedure TJvColorSquare.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  //if True then
  //  ;
  if Assigned(FColorClick) then
    FColorClick(Self, Button, Color);
end;

//=== TJvColorBox ============================================================

constructor TJvColorBox.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  Width := 78;
  Height := 96;
  FBorderStyle := bsSingle;

  for I := 1 to 20 do
  begin
    FSquares[I] := TJvColorSquare.Create(Self);
    FSquares[I].BorderStyle := FBorderStyle;
    FSquares[I].Parent := Self;
    FSquares[I].OnColorClick := ColorClicked;
    FSquares[I].Color := Colors[I];
  end;

end;

procedure TJvColorBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

destructor TJvColorBox.Destroy;
begin
  // don't destroy colorsquares here: they're destroyed automatically
  inherited Destroy;
end;

procedure TJvColorBox.DrawColorBoxes;
var
  I, X, Y, W, H: Integer;
const
  Offset = 3;
begin
  X := Offset;
  Y := Offset;
  W := (Width - 4) div 4;
  H := (Height - 4) div 5;
  for I := 1 to 20 do
  begin
    FSquares[I].SetBounds(X, Y, W, H);
    Inc(X, W);
    if I mod 4 = 0 then
    begin
      Inc(Y, H);
      X := Offset;
    end;
  end;
end;

procedure TJvColorBox.DoExit;
begin
  inherited DoExit;
end;

procedure TJvColorBox.DoEnter;
begin
  inherited DoEnter;
end;

procedure TJvColorBox.ColorClicked(Sender: TObject; Button: TMouseButton; Color: TColor);
begin
  if Assigned(FColorClick) then
    FColorClick(Self, Button, Color);
end;

procedure TJvColorBox.SetBorderStyle(Value: TBorderStyle);
var
  I: Integer;
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    for I := 1 to 20 do
      FSquares[I].BorderStyle := Value;
    Repaint;
  end;
end;

procedure TJvColorBox.Paint;
var
  Rect: TRect;
begin
  Rect := ClientRect;
  { frame }
  if FBorderStyle = bsSingle then
  begin
    Frame3d(Canvas, Rect, clBtnFace, cl3DDkShadow, 1);
    Frame3d(Canvas, Rect, clBtnHighLight, clBtnShadow, 1);
    Frame3d(Canvas, Rect, clBtnFace, clBtnFace, 1);
  end
  else
  begin
    Frame3d(Canvas, Rect, clBtnFace, clBtnFace, 1);
    Frame3d(Canvas, Rect, clBtnShadow, clBtnHighLight, 1);
    Frame3d(Canvas, Rect, cl3DDkShadow, clBtnFace, 1);
  end;

  { colorsquares }
  DrawColorBoxes;
end;

//=== TJvCustomDropButton ====================================================

constructor TJvCustomDropButton.Create(AOWner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csSetCaption];

  FArrowWidth := 13;
  Width := 42;
  Height := 21;
end;

destructor TJvCustomDropButton.Destroy;
begin
  inherited Destroy;
end;

procedure TJvCustomDropButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FIsDown := True;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvCustomDropButton.SetArrowWidth(Value: Integer);
begin
  if FArrowWidth <> Value then
  begin
    FArrowWidth := Value;
    Repaint;
  end;
end;

procedure TJvCustomDropButton.SetAssociate(Value: TPopupMenu);
begin
  FAssociate := Value;
end;

procedure TJvCustomDropButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Pt: TPoint;
begin
  FIsDown := False;
  inherited MouseUp(Button, Shift, X, Y);
  if Assigned(FAssociate) then
  begin
    Pt := ClientToScreen(Point(0, Height));
    FAssociate.PopUp(Pt.X, Pt.Y);
  end;
end;

procedure TJvCustomDropButton.Paint;
var
  Rec: TRect;
  Increment: Integer;
begin
  { Draw the button face }
  DrawButtonFace(Canvas, ClientRect, 1, bsAutoDetect, False, FIsDown, False);

  if FIsDown then
    Increment := 1
  else
    Increment := 0;

  Rec := ClientRect;
  Rec.Left := Width - FArrowWidth;
  OffsetRect(Rec, Increment, Increment);

  { Draw vertical 'bar' }
  Canvas.Pen.Color := clBtnShadow;
  DrawLine(Canvas, Rec.Left, Rec.Top + 4, Rec.Left, Rec.Bottom - 4);
  Canvas.Pen.Color := clBtnHighLight;
  DrawLine(Canvas, Rec.Left + 1, Rec.Top + 4, Rec.Left + 1, Rec.Bottom - 4);

  { Draw arrow }
  if not Enabled then
    Canvas.Pen.Color := clBtnShadow
  else
    Canvas.Pen.Color := cl3DDkShadow;

  Rec.Bottom := (Height div 2) + Increment - 1;
  InflateRect(Rec, -4, 0);

  while Rec.Left < Rec.Right + 1 do
  begin
    DrawLine(Canvas, Rec.Left, Rec.Bottom, Rec.Right, Rec.Bottom);
    InflateRect(Rec, -1, 1);
  end;
end;

procedure TJvCustomDropButton.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJvCustomDropButton.CMSysColorChange(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJvCustomDropButton.WMSize(var Msg: TWMSize);
begin
  inherited;
  Invalidate;
end;

end.

