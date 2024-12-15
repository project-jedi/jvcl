{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSimIndicator.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSimIndicator;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Windows, Graphics, Controls, ExtCtrls,
  JvComponent, JvJVCLUtils;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvSimIndicator = class(TJvGraphicControl)
  private
    FValue: Integer;
    FMaximum: Integer;
    FMinimum: Integer;
    FBarColor: TColor;
    FBackColor: TColor;
    {$IFNDEF COMPILER10_UP}
    FMargins: TJvRect;
    {$ENDIF !COMPILER10_UP}
    procedure SetBarColor(const Value: TColor);
    procedure SetMaximum(const Value: Integer);
    procedure SetMinimum(const Value: Integer);
    procedure SetValue(const Value: Integer);
    procedure SetBackColor(const Value: TColor);
    {$IFNDEF COMPILER10_UP}
    procedure SetMargins(const Value: TJvRect);
    {$ENDIF !COMPILER10_UP}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Value: Integer read FValue write SetValue;
    property Minimum: Integer read FMinimum write SetMinimum default 0;
    property Maximum: Integer read FMaximum write SetMaximum default 100;
    property BarColor: TColor read FBarColor write SetBarColor default clLime;
    property BackColor: TColor read FBackColor write SetBackColor default clSilver;
    property Width default 25;
    property Height default 100;
    {$IFNDEF COMPILER10_UP}
    property Margins: TJvRect read FMargins write SetMargins;
    {$ENDIF !COMPILER10_UP}

    property Align;
    property Anchors;
    property ParentShowHint;
    property ShowHint;
    property Visible;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    {$IFDEF COMPILER9_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF COMPILER9_UP}
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
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


constructor TJvSimIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 25;
  Height := 100;
  FMinimum := 0;
  FMaximum := 100;
  FValue := 50;
  FBarColor := clLime;
  FBackColor := clSilver;

  {$IFNDEF COMPILER10_UP}
  FMargins := TJvRect.Create;
  {$ENDIF !COMPILER10_UP}
end;

destructor TJvSimIndicator.Destroy;
begin
  {$IFNDEF COMPILER10_UP}
  FMargins.Free;
  {$ENDIF !COMPILER10_UP}

  inherited Destroy;
end;

procedure TJvSimIndicator.Paint;
const
  NumberOfBars = 20;
var
  R, Ri: TRect;
  I, n: Integer;
  h, dh: Integer;
begin
  R := ClientRect;
  Canvas.Brush.Color := clSilver;
  Canvas.FillRect(R);
  Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1);

  Dec(R.Top, Margins.Top);
  Dec(R.Left, Margins.Left);
  Dec(R.Bottom, Margins.Bottom);
  Dec(R.Right, Margins.Right);
  Frame3D(Canvas, R, clBtnShadow, clBtnHighlight, 1);

  Canvas.Brush.Color := FBackColor;
  InflateRect(R, -1, -1);
  Canvas.FillRect(R);
  Dec(R.Right);
  h := R.Bottom - R.Top;
  dh := h div NumberOfBars;
  n := Round(NumberOfBars * (FValue - FMinimum)/(FMaximum - FMinimum));
  Canvas.Brush.Color := FBarColor;
  Ri := Rect(R.Left + 1, R.Bottom - dh + 1, R.Right - 1, R.Bottom);
  for I := 1 to n do
  begin
    Canvas.FillRect(Ri);
    Dec(Ri.Top, dh);
    Dec(Ri.Bottom, dh);
  end;
end;

procedure TJvSimIndicator.SetBackColor(const Value: TColor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    Invalidate;
  end;
end;

procedure TJvSimIndicator.SetBarColor(const Value: TColor);
begin
  if FBarColor <> Value then
  begin
    FBarColor := Value;
    Invalidate;
  end;
end;

procedure TJvSimIndicator.SetMaximum(const Value: Integer);
begin
  if FMaximum <> Value then
  begin
    FMaximum := Value;
    Invalidate;
  end;
end;

procedure TJvSimIndicator.SetMinimum(const Value: Integer);
begin
  if FMinimum <> Value then
  begin
    FMinimum := Value;
    Invalidate;
  end;
end;

procedure TJvSimIndicator.SetValue(const Value: Integer);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    Invalidate;
  end;
end;

{$IFNDEF COMPILER10_UP}
procedure TJvSimIndicator.SetMargins(const Value: TJvRect);
begin
  FMargins.Assign(Value);
  Invalidate;
end;
{$ENDIF !COMPILER10_UP}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.