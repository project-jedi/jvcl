{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorBox.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
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

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvColorBox;

interface

uses
  {$IFDEF VCL}
  Windows, Messages,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types,
  {$ENDIF VisualCLX}
  Classes, Graphics, Controls, Forms, Menus,
  JvComponent;

type
  TJvColorClickEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; Color: TColor) of object;

  { a square with a sunken frame and a color, sets Color when clicked
    draws a frame when active (MouseEnter) }
  TJvColorSquare = class(TJvGraphicControl)
  private
    FInside: Boolean;
    FBorderStyle: TBorderStyle;
    FOnChange: TNotifyEvent;
    FColorClick: TJvColorClickEvent;
    procedure SetBorderStyle(Value: TBorderStyle);
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure ColorChanged; override;
    procedure Paint; override;
    procedure DrawFocusFrame;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Color default clWhite;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnColorClick: TJvColorClickEvent read FColorClick write FColorClick;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { a window with 20 Color squares and a button that activates a TColorDialog... }
  TJvColorBox = class(TJvCustomControl)
  private
    FColorClick: TJvColorClickEvent;
    FBorderStyle: TBorderStyle;
    FSquares: array [1..20] of TJvColorSquare;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure DrawColorBoxes;
  protected
    procedure Paint; override;
    procedure ColorClicked(Sender: TObject; Button: TMouseButton; Shift: TShiftState; Color: TColor);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Visible;
    property Top;
    property Width default 78;
    property Height default 96;
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
    FDropDown: TPopupMenu;
    FIsDown: Boolean;
    FArrowWidth: Integer;
    procedure SetArrowWidth(Value: Integer);
    procedure SetDropDown(Value: TPopupMenu);
    {$IFDEF VCL}
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    {$ENDIF VCL}
  protected
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure EnabledChanged; override;
    procedure Paint; override;
    property DropDown: TPopupMenu read FDropDown write SetDropDown;
    property ArrowWidth: Integer read FArrowWidth write SetArrowWidth default 13;
  public
    constructor Create(AOwner: TComponent); override;
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

uses
  ExtCtrls, Buttons,
  JvThemes;

const
  // (rom) nonstandard colors renamed
  clPrivateBeige = TColor($C6DEC6);
  clPrivateSky = TColor($F7CEA5);
  clPrivateCream = TColor($F7FFFF);

  Colors: array [1..20] of TColor =
   (clWhite, clBlack, clSilver, clDkGray,
    clRed, clMaroon, clYellow, clOlive,
    clLime, clGreen, clAqua, clTeal,
    clBlue, clNavy, clFuchsia, clPurple,
    clPrivateBeige, clPrivateSky, clPrivateCream, clGray);

procedure DrawLine(Canvas: TCanvas; X, Y, X2, Y2: Integer);
begin
  Canvas.MoveTo(X, Y);
  Canvas.LineTo(X2, Y2);
end;

//=== { TJvColorSquare } =====================================================

constructor TJvColorSquare.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorderStyle := bsSingle;
  Color := clWhite;
  Width := 18;
  Height := 18;
  FInside := False;
end;

procedure TJvColorSquare.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TJvColorSquare.Paint;
var
  Rect: TRect;
begin
  Rect := ClientRect;
  if FBorderStyle = bsSingle then
  begin
    Frame3D(Canvas, Rect, clBtnFace, clBtnFace, 1);
    Frame3D(Canvas, Rect, clBtnShadow, clBtnHighLight, 1);
    Frame3D(Canvas, Rect, cl3DDkShadow, clBtnFace, 1);
  end;

  with Canvas do
  begin
    Brush.Color := Self.Color;
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
    Frame3D(Canvas, Rect, cl3DDkShadow, cl3DDkShadow, 1);
    Frame3D(Canvas, Rect, clBtnHighLight, clBtnHighLight, 1);
    Frame3D(Canvas, Rect, cl3DDkShadow, cl3DDkShadow, 1);
  end;
end;

procedure TJvColorSquare.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  FInside := True;
  Invalidate;
end;

procedure TJvColorSquare.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  FInside := False;
  Invalidate;
end;

procedure TJvColorSquare.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Assigned(FColorClick) then
    FColorClick(Self, Button, Shift, Color);
end;

//=== { TJvColorBox } ========================================================

constructor TJvColorBox.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  Width := 78;
  Height := 96;
  FBorderStyle := bsSingle;

  for I := Low(FSquares) to High(FSquares) do
  begin
    FSquares[I] := TJvColorSquare.Create(Self);
    FSquares[I].BorderStyle := FBorderStyle;
    FSquares[I].Parent := Self;
    FSquares[I].OnColorClick := ColorClicked;
    FSquares[I].Color := Colors[I];
  end;
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

procedure TJvColorBox.ColorClicked(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; Color: TColor);
begin
  if Assigned(FColorClick) then
    FColorClick(Self, Button, Shift, Color);
end;

procedure TJvColorBox.SetBorderStyle(Value: TBorderStyle);
var
  I: Integer;
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    for I := Low(FSquares) to High(FSquares) do
      FSquares[I].BorderStyle := Value;
    Invalidate;
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
    Frame3D(Canvas, Rect, clBtnFace, cl3DDkShadow, 1);
    Frame3D(Canvas, Rect, clBtnHighLight, clBtnShadow, 1);
    Frame3D(Canvas, Rect, clBtnFace, clBtnFace, 1);
  end
  else
  begin
    Frame3D(Canvas, Rect, clBtnFace, clBtnFace, 1);
    Frame3D(Canvas, Rect, clBtnShadow, clBtnHighLight, 1);
    Frame3D(Canvas, Rect, cl3DDkShadow, clBtnFace, 1);
  end;

  { color squares }
  DrawColorBoxes;
end;

//=== { TJvCustomDropButton } ================================================

constructor TJvCustomDropButton.Create(AOWner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csSetCaption];
  {$IFDEF VCL}
  IncludeThemeStyle(Self, [csParentBackground]);
  {$ENDIF VCL}
  FArrowWidth := 13;
  Width := 42;
  Height := 21;
end;

procedure TJvCustomDropButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FIsDown := True;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvCustomDropButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Pt: TPoint;
begin
  FIsDown := False;
  inherited MouseUp(Button, Shift, X, Y);
  if Assigned(FDropDown) then
  begin
    Pt := ClientToScreen(Point(0, Height));
    FDropDown.Popup(Pt.X, Pt.Y);
  end;
end;

procedure TJvCustomDropButton.SetArrowWidth(Value: Integer);
begin
  if FArrowWidth <> Value then
  begin
    FArrowWidth := Value;
    Invalidate;
  end;
end;

procedure TJvCustomDropButton.SetDropDown(Value: TPopupMenu);
begin
  FDropDown := Value;
end;

procedure TJvCustomDropButton.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled and Enabled then
    Invalidate;
  {$ENDIF JVCLThemesEnabled}
end;

procedure TJvCustomDropButton.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled and Enabled then
    Invalidate;
  {$ENDIF JVCLThemesEnabled}
end;

procedure TJvCustomDropButton.Paint;
var
  Rec: TRect;
  Increment: Integer;
begin
  { Draw the button face }
  DrawThemedButtonFace(Self, Canvas, ClientRect, 1, bsAutoDetect, False,
    FIsDown, Focused, IsMouseOver(Self));

  Increment := Ord(FIsDown);
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
    Canvas.Pen.Color := clWindowText; // cl3DDkShadow

  Rec.Bottom := (Height div 2) + Increment - 1;
  InflateRect(Rec, -4, 0);

  while Rec.Left < Rec.Right + 1 do
  begin
    DrawLine(Canvas, Rec.Left, Rec.Bottom, Rec.Right, Rec.Bottom);
    InflateRect(Rec, -1, 1);
  end;
end;

procedure TJvCustomDropButton.EnabledChanged;
begin
  inherited EnabledChanged;
  Invalidate;
end;

{$IFDEF VCL}
procedure TJvCustomDropButton.CMSysColorChange(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;
{$ENDIF VCL}

procedure TJvCustomDropButton.Resize;
begin
  inherited Resize;
  Invalidate;
end;

procedure TJvColorSquare.ColorChanged;
begin
  inherited ColorChanged;
  if Assigned(FOnChange) then
    FOnChange(Self);
  Invalidate;
end;

end.

