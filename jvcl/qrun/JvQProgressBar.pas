{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

unit JvQProgressBar;

{$I jvcl.inc}

interface

uses
  QWindows, QMessages, 
  SysUtils, Classes, QGraphics, QControls, QForms, QComCtrls,
  JvQExComCtrls;

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
  public
    constructor Create(AOwner: TComponent); override;
  published  
    property FillColor default clHighlight; 
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property Color;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

uses
  JvQJCLUtils;

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
  DrawEdge(ACanvas, R, esNone, esLowered, ebRect); 
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



end.

