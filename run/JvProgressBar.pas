{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProgressBar.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvProgressBar;

{$I jvcl.inc}

interface

uses
  Windows, Messages,
  {$IFDEF VCL}
  CommCtrl,
  {$ENDIF VCL}
  SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  JvExComCtrls;

type
  TJvBaseProgressBar = class(TGraphicControl)
  private
    FBlockSize: Integer;
    FSmooth: Boolean;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FOrientation: TProgressBarOrientation;
    FBarColor: TColor;
    FSteps: Integer;
    FOnChange: TNotifyEvent;
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetOrientation(Value: TProgressBarOrientation);
    procedure SetPosition(Value: Integer);
    procedure SetSmooth(const Value: Boolean);
    procedure SetBlockSize(const Value: Integer);
    procedure SetBarColor(const Value: TColor);
    procedure SetSteps(const Value: Integer);
  protected
    // BarSize is the upper limit of the area covered by the progress bar
    // Derived classes should override this method to provide their own drawing
    // routine. The base class enmulates the look of the standard TProgressBar
    procedure DrawBar(ACanvas: TCanvas; BarSize: Integer); virtual;
    // GetMaxBarSize returns the maximum size of the bar in pixels.
    // For example, if the control has a 2 pixel border, when at Max,
    // GetMaxBarSize should return Self.Width - 4 when horizontal
    // and Self.Height - 4 when vertical. The default implementation returns
    // Self.Width when horizontal and Self.Height when vertical.
    function GetMaxBarSize: Integer; virtual;
    procedure Paint; override;
    procedure Change; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure StepIt; virtual;
    procedure StepBy(Delta: Integer); virtual;
  public
    property Steps: Integer read FSteps write SetSteps default 10;
    property BarColor: TColor read FBarColor write SetBarColor default clHighlight;
    property BlockSize: Integer read FBlockSize write SetBlockSize default 10;
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property Orientation: TProgressBarOrientation read FOrientation write SetOrientation default pbHorizontal;
    property Position: Integer read FPosition write SetPosition default 0;
    property Smooth: Boolean read FSmooth write SetSmooth default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Width default 150;
  end;

  TJvProgressBar = class(TJvExProgressBar)
  private
    {$IFDEF VCL}
    FFillColor: TColor;
    procedure SetFillColor(const Value: TColor);
  protected
    procedure CreateWnd; override;
    {$ENDIF VCL}
  public
    constructor Create(AOwner: TComponent); override;
  published
    {$IFDEF VCL}
    property FillColor: TColor read FFillColor write SetFillColor default clHighlight;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    property FillColor default clHighlight;
    {$ENDIF VisualCLX}
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property Color;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TJvBaseGradientProgressBar = class(TJvBaseProgressBar)
  private
    FBarColorFrom: TColor;
    FBarColorTo: TColor;
    procedure SetBarColorFrom(Value: TColor);
    procedure SetBarColorTo(const Value: TColor);
  public
    property BarColorFrom: TColor read FBarColorFrom write SetBarColorFrom;
    property BarColorTo: TColor read FBarColorTo write SetBarColorTo;
  end;

  TJvCustomGradientProgressBar = class(TJvBaseGradientProgressBar)
  protected
    procedure DrawBar(ACanvas: TCanvas; BarSize: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvGradientProgressBar = class(TJvCustomGradientProgressBar)
  published
    property BarColorFrom default clWhite;
    property BarColorTo default clBlack;
    property Max;
    property Min;
    property Orientation;
    property Position;
    property Smooth;

    property Align;
    property Anchors;
    property Color default clWindow;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Hint;
    property ParentColor default False;
    property PopupMenu;
    property ParentShowHint;
    property ShowHint;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF COMPILER6_UP}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$ENDIF COMPILEr6_UP}
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvJCLUtils, JvJVCLUtils;

//=== { TJvBaseProgressBar } =================================================

constructor TJvBaseProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FMin := 0;
  FMax := 100;
  FOrientation := pbHorizontal;
  FBlockSize := 10;
  FBarColor := clHighlight;
  FSteps := 10;
  Width := 150;
  Height := GetSystemMetrics(SM_CYVSCROLL);
end;

procedure TJvBaseProgressBar.Paint;
var
  ASize, APos: Integer;
begin
  if (Max - Min <= 0) or (Width <= 0) or (Height <= 0) then
    Exit;
  // calculate the size of the bar based on Min, Max, Position and Width or Height
  APos := Position;
  if not Smooth then
    APos := APos - APos mod Steps;
  ASize := GetMaxBarSize * (APos - Min) div (Max - Min);
  DrawBar(Canvas, ASize);
end;

procedure TJvBaseProgressBar.SetMax(Value: Integer);
begin
  if Value < FMin then
    Value := FMin;
  if FPosition > Value then
    FPosition := Value;
  if FMax <> Value then
  begin
    FMax := Value;
    Change;
    Invalidate;
  end;
end;

procedure TJvBaseProgressBar.SetMin(Value: Integer);
begin
  if Value > FMax then
    Value := FMax;
  if FPosition < FMin then
    FPosition := FMin;
  if FMin <> Value then
  begin
    FMin := Value;
    Change;
    Invalidate;
  end;
end;

procedure TJvBaseProgressBar.SetOrientation(Value: TProgressBarOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    if not (csLoading in ComponentState) then // fixes property load order
      SetBounds(Left, Top, Height, Width);
  end;
end;

procedure TJvBaseProgressBar.SetPosition(Value: Integer);
begin
  if Value > FMax then
    Value := FMax;
  if Value < FMin then
    Value := FMin;
  if FPosition <> Value then
  begin
    FPosition := Value;
    Change;
    Invalidate;
  end;
end;

procedure TJvBaseProgressBar.SetSmooth(const Value: Boolean);
begin
  if FSmooth <> Value then
  begin
    FSmooth := Value;
    Invalidate;
  end;
end;

procedure TJvBaseProgressBar.DrawBar(ACanvas: TCanvas; BarSize: Integer);
var
  R: TRect;
begin
  R := ClientRect;
  ACanvas.Brush.Color := Color;
  ACanvas.FillRect(R);
  {$IFDEF VCL}
  DrawEdge(ACanvas.Handle, R, BDR_SUNKENOUTER, BF_ADJUST or BF_RECT);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  DrawEdge(ACanvas, R, esNone, esLowered, ebRect);
  {$ENDIF VisualCLX}
  if BarSize = 0 then
    Exit;
  ACanvas.Brush.Color := BarColor;
  if Orientation = pbHorizontal then
  begin
    if Smooth then
    begin
      R.Right := R.Left + BarSize;
      InflateRect(R, -1, -1);
      if R.Right > Width - 2 then
        R.Right := Width - 2;
      if R.Right > R.Left then
        ACanvas.FillRect(R);
    end
    else
    begin
      R.Right := R.Left + Steps;
      InflateRect(R, -1, -1);
      while BarSize > 0 do
      begin
        if R.Right > Width - 3 then
          R.Right := Width - 3;
        if R.Left >= R.Right then
          Exit;
        ACanvas.FillRect(R);
        OffsetRect(R, RectWidth(R) + 2, 0);
        Dec(BarSize, RectWidth(R) + 2);
      end;
    end;
  end
  else
  begin
    if Smooth then
    begin
      R.Top := R.Bottom - BarSize;
      if R.Top < 2 then
        R.Top := 2;
      InflateRect(R, -1, -1);
      ACanvas.FillRect(R);
    end
    else
    begin
      OffsetRect(R, 0, Height - Steps - 2);
      R.Bottom := R.Top + Steps;
      InflateRect(R, -1, -1);
      while BarSize > 0 do
      begin
        if R.Top < 3 then
          R.Top := 3;
        ACanvas.FillRect(R);
        OffsetRect(R, 0, -Steps);
        Dec(BarSize, Steps);
      end;
    end;
  end;
end;

function TJvBaseProgressBar.GetMaxBarSize: Integer;
begin
  if Orientation = pbHorizontal then
    Result := Width
  else
    Result := Height;
end;

procedure TJvBaseProgressBar.SetSteps(const Value: Integer);
begin
  if FSteps <> Value then
  begin
    FSteps := Value;
    if FSteps < 1 then
      FSteps := 1;
  end;
end;

procedure TJvBaseProgressBar.StepIt;
begin
  StepBy(Steps);
end;

procedure TJvBaseProgressBar.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvBaseProgressBar.StepBy(Delta: Integer);
begin
  if Position + Delta > Max then
    Position := Max
  else
  if Position + Delta < Min then
    Position := Min
  else
    Position := Position + Delta;
end;

procedure TJvBaseProgressBar.SetBlockSize(const Value: Integer);
begin
  if FBlockSize <> Value then
  begin
    FBlockSize := Value;
    if FBlockSize <= 0 then
      FBlockSize := 1;
    Invalidate;
  end;
end;

procedure TJvBaseProgressBar.SetBarColor(const Value: TColor);
begin
  if FBarColor <> Value then
  begin
    FBarColor := Value;
    Invalidate;
  end;
end;

//=== { TJvProgressBar } =====================================================

constructor TJvProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FillColor := clHighlight;
end;

{$IFDEF VCL}

procedure TJvProgressBar.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(FFillColor));
end;

procedure TJvProgressBar.SetFillColor(const Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    if HandleAllocated then
    begin
      SendMessage(Handle, PBM_SETBARCOLOR, 0, ColorToRGB(FFillColor));
      // (rom) Invalidate is not good enough
      Repaint;
    end;
  end;
end;

{$ENDIF VCL}

//=== { TJvBaseGradientProgressBar } =========================================

procedure TJvBaseGradientProgressBar.SetBarColorFrom(Value: TColor);
begin
  if FBarColorFrom <> Value then
  begin
    FBarColorFrom := Value;
    Invalidate;
  end;
end;

procedure TJvBaseGradientProgressBar.SetBarColorTo(const Value: TColor);
begin
  if FBarColorTo <> Value then
  begin
    FBarColorTo := Value;
    Invalidate;
  end;
end;

//=== { TJvGradientProgressBar } =============================================

constructor TJvCustomGradientProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBarColorFrom := clWhite;
  FBarColorTo := clBlack;
  BlockSize := 6;
end;

procedure TJvCustomGradientProgressBar.DrawBar(ACanvas: TCanvas; BarSize: integer);
var
  R: TRect;
  ABlockSize:double;
  i:integer;
begin
  R := ClientRect;
  ACanvas.Brush.Color := Color;
  ACanvas.FillRect(R);
  DrawEdge(ACanvas.Handle, R, BDR_SUNKENOUTER, BF_ADJUST or BF_RECT);
  InflateRect(R, -1, -1);
  if Orientation = pbHorizontal then
  begin
    R.Right := BarSize;
    if R.Right > ClientWidth - 2 then
      R.Right := ClientWidth - 2;
    GradientFillRect(ACanvas, R, BarColorFrom, BarColorTo, fdLeftToRight, 255);
  end
  else
  begin
    R.Top := R.Bottom - BarSize;
    if R.Top < 2 then
      R.Top := 2;
    GradientFillRect(ACanvas, R, BarColorFrom, BarColorTo, fdBottomToTop, 255);
  end;
  if not Smooth then
  begin
    ACanvas.Pen.Color := Color;
    if Position > 0 then
      ABlockSize := (GetMaxBarSize * BlockSize - 4) / 100
    else
      ABlockSize := 0;
    i := 0;
    if Orientation = pbHorizontal then
    begin
      R := ClientRect;
      InflateRect(R, -2, -2);
      R.Right := R.Left + round(ABlockSize);
      while R.Left <= BarSize do
      begin
        ACanvas.MoveTo(R.Left, R.Top);
        ACanvas.LineTo(R.Left, R.Bottom);
        Inc(i);
        R := ClientRect;
        InflateRect(R, -2, -2);
        R.Right := R.Left + round(ABlockSize);
        OffsetRect(R, round(i * ABlockSize), 0);
      end;
    end
    else
    begin
      R := ClientRect;
      InflateRect(R, -2, -2);
      R.Top := R.Bottom - round(ABlockSize);
      while R.Bottom >= GetMaxBarSize - BarSize do
      begin
        ACanvas.MoveTo(R.Left, R.Bottom);
        ACanvas.LineTo(R.Right, R.Bottom);
        Inc(i);
        R := ClientRect;
        InflateRect(R, -2, -2);
        R.Top := R.Bottom - round(ABlockSize);
        OffsetRect(R, 0, -round(i * ABlockSize));
      end;
    end;
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

