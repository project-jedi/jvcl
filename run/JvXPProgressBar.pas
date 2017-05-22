{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXPProgress.PAS, released on 2004-04-05.

The Initial Developer of the Original Code is Stefano Pessina [stefano dott pessina sanbiagiomonza dott it]
Portions created by Stefano Pessina are Copyright (C) 2004 Stefano Pessina.
All Rights Reserved.

Contributor(s):

Last Modified: 2004-04-07

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvXPProgressBar;

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes, Graphics,
  JvProgressBar;

type
  TJvCustomXPProgressBar = class(TJvBaseGradientProgressBar)
  private
    procedure DrawBlock(ACanvas: TCanvas; ARect: TRect);
  protected
    procedure DrawLine(ACanvas: TCanvas; X1, Y1, X2, Y2, AColor: TColor);
    procedure DrawBar(ACanvas: TCanvas; BarSize: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvXPProgressBar = class(TJvCustomXPProgressBar)
  published
    property BarColorFrom default $31D329;
    property BarColorTo default $ADEFAD;
    property Max;
    property Min;
    property Orientation;
    property Position;
    property Smooth;

    property Align;
    property Anchors;
    property Color default clWindow;
    property Constraints;
    property DragCursor;
    property DragKind;
    property OnCanResize;
    property OnEndDock;
    property OnStartDock;
    property DragMode;
    property Enabled;
    property Hint;
    property ParentColor default False;
    property PopupMenu;
    property ParentShowHint;
    property ShowHint;
    property Visible;

    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
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
  Controls, Forms, ComCtrls,
  JvJVCLUtils, JvJCLUtils;

//=== { TJvXPProgressBar } ===================================================

constructor TJvCustomXPProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Steps := 7;
  ParentColor := False;
  Color := clWindow;
  BarColorFrom := $31D329;
  BarColorTo := $ADEFAD;
end;

procedure TJvCustomXPProgressBar.DrawLine(ACanvas: TCanvas; X1, Y1, X2, Y2, AColor: TColor);
begin
  ACanvas.Pen.Color := AColor;
  ACanvas.MoveTo(X1, Y1);
  ACanvas.LineTo(X2, Y2);
end;

procedure TJvCustomXPProgressBar.DrawBlock(ACanvas: TCanvas; ARect: TRect);
var
  ARect2: TRect;
begin
  ARect2 := ARect;
  if Orientation = pbHorizontal then
  begin
    Inc(ARect2.Top, RectHeight(ARect) div 2);
    Dec(ARect.Bottom, RectHeight(ARect) div 2);
    GradientFillRect(ACanvas, ARect, BarColorFrom, BarColorTo, fdBottomToTop, 255);
    GradientFillRect(ACanvas, ARect2, BarColorTo, BarColorFrom, fdBottomToTop, 255);
  end
  else
  begin
    Inc(ARect2.Left, RectWidth(ARect) div 2);
    Dec(ARect.Right, RectWidth(ARect) div 2);
    GradientFillRect(ACanvas, ARect, BarColorFrom, BarColorTo, fdRightToLeft, 255);
    GradientFillRect(ACanvas, ARect2, BarColorTo, BarColorFrom, fdRightToLeft, 255);
  end;
end;

type
  TWinControlAccessProtected = class(TWinControl);

procedure TJvCustomXPProgressBar.DrawBar(ACanvas: TCanvas; BarSize: Integer);
const
  cColor1 = $BEBEBE;
  cColor2 = $686868;
  cColor3 = $EFEFEF;
var
  X, Y: Integer;
  R: TRect;
  Bmp: TBitmap;
  AColor: TColor;
  LBlockSize: Integer;
begin
  if Parent <> nil then
    AColor := TWinControlAccessProtected(Parent).Color
  else
  if GetParentForm(Self) <> nil then
    AColor := GetParentForm(Self).Color
  else
    AColor := clBtnFace;
  Bmp := TBitmap.Create;
  try
    Bmp.Width := Width;
    Bmp.Height := Height;
    Bmp.Canvas.Brush.Color := clFuchsia;
    R := ClientRect;
    Bmp.Canvas.FillRect(R);
    InflateRect(R, -3, -2);
    Bmp.Canvas.Brush.Color := Color;
    Bmp.Canvas.FillRect(R);

    // draw the frame
    // left side
    X := 0;
    DrawLine(Bmp.Canvas, X, 1, X, Height - 1, cColor1);
    DrawLine(Bmp.Canvas, X, 2, X, Height - 2, cColor2);

    // right side
    X := Width - 1;
    DrawLine(Bmp.Canvas, X, 1, X, Height - 1, cColor1);
    DrawLine(Bmp.Canvas, X, 2, X, Height - 2, cColor2);

    // left side
    X := 0;
    DrawLine(Bmp.Canvas, X, 1, X, Height - 1, cColor1);
    DrawLine(Bmp.Canvas, X, 2, X, Height - 2, cColor2);
    // right side
    X := Width - 1;
    DrawLine(Bmp.Canvas, X, 1, X, Height - 1, cColor1);
    DrawLine(Bmp.Canvas, X, 2, X, Height - 2, cColor2);

    // left side
    X := 1;
    DrawLine(Bmp.Canvas, X, 0, X, Height, cColor1);
    DrawLine(Bmp.Canvas, X, 1, X, Height - 1, cColor2);
    DrawLine(Bmp.Canvas, X, 2, X, Height - 2, cColor1);
    // right side
    X := Width - 2;
    DrawLine(Bmp.Canvas, X, 0, X, Height, cColor1);
    DrawLine(Bmp.Canvas, X, 1, X, Height - 1, cColor2);
    DrawLine(Bmp.Canvas, X, 2, X, Height - 2, cColor1);

    // left side
    X := 2;
    DrawLine(Bmp.Canvas, X, 0, X, Height, cColor2);
    DrawLine(Bmp.Canvas, X, 1, X, Height - 1, cColor1);
    DrawLine(Bmp.Canvas, X, 3, X, Height - 1, cColor3);
    // right side
    X := Width - 3;
    DrawLine(Bmp.Canvas, X, 0, X, Height, cColor2);
    DrawLine(Bmp.Canvas, X, 1, X, Height - 1, cColor1);
    DrawLine(Bmp.Canvas, X, 3, X, Height - 1, cColor3);

    // top side
    Y := 0;
    DrawLine(Bmp.Canvas, 3, Y, Width - 3, Y, cColor2);
    DrawLine(Bmp.Canvas, 3, Y + 1, Width - 3, Y + 1, cColor1);
    DrawLine(Bmp.Canvas, 3, Y + 2, Width - 3, Y + 2, cColor3);

    // bottom side
    Y := Height - 1;
    DrawLine(Bmp.Canvas, 3, Y, Width - 2, Y, cColor2);
    DrawLine(Bmp.Canvas, 3, Y - 1, Width - 3, Y - 1, cColor3);

    // draw the blocks
    LBlockSize := BlockSize - 3;
    if Orientation = pbHorizontal then
    begin
      if Smooth then
      begin
        R.Right := R.Left + BarSize;
        InflateRect(R, -1, -1);
        if R.Right > Width - 3 then
          R.Right := Width - 3;
        if R.Right > R.Left then
          DrawBlock(Bmp.Canvas, R);
      end
      else
      begin
        R := Rect(2, 2, LBlockSize + 1, Height - 4);
        OffsetRect(R, 2, 1);
        while BarSize > 2 do
        begin
          if R.Right > Width - 3 then
            R.Right := Width - 3;
          if R.Right - R.Left > 0 then
            DrawBlock(Bmp.Canvas, R);
          OffsetRect(R, LBlockSize + 1, 0);
          Dec(BarSize, LBlockSize + 1);
        end;
      end;
    end
    else
    begin
      if Smooth then
      begin
        R.Top := R.Bottom - BarSize;
        if R.Top < 3 then
          R.Top := 3;
        InflateRect(R, -1, -1);
        DrawBlock(Bmp.Canvas, R);
      end
      else
      begin
        R := Rect(2, 2, Width - 4, LBlockSize + 1);
        OffsetRect(R, 1, 0);
        OffsetRect(R, 0, Height - LBlockSize - 4);
        while BarSize > 2 do
        begin
          if R.Top < 3 then
            R.Top := 3;
          if R.Bottom - R.Top > 0 then
            DrawBlock(Bmp.Canvas, R);
          OffsetRect(R, 0, -LBlockSize - 1);
          Dec(BarSize, LBlockSize + 1);
        end;
      end;
    end;
    ACanvas.Brush.Color := AColor;
    with ACanvas do
      BrushCopy( ClientRect, Bmp, ClientRect, clFuchsia);
  finally
    Bmp.Free;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
