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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSimIndicator;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Windows, Graphics, Controls, ExtCtrls,
  JvComponent;

type
  TJvSimIndicator = class(TJvGraphicControl)
  private
    FValue: Integer;
    FMaximum: Integer;
    FMinimum: Integer;
    FBarColor: TColor;
    FBackColor: TColor;
    procedure SetBarColor(const Value: TColor);
    procedure SetMaximum(const Value: Integer);
    procedure SetMinimum(const Value: Integer);
    procedure SetValue(const Value: Integer);
    procedure SetBackColor(const Value: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property Value: Integer read FValue write SetValue;
    property Minimum: Integer read FMinimum write SetMinimum default 0;
    property Maximum: Integer read FMaximum write SetMaximum default 100;
    property BarColor: TColor read FBarColor write SetBarColor default clLime;
    property BackColor: TColor read FBackColor write SetBackColor default clSilver;
    property Width default 25;
    property Height default 100;
  end;

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

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
end;

procedure TJvSimIndicator.Paint;
var
  R, Ri: TRect;
  I, n: Integer;
  h, dh: Integer;
begin
  R := ClientRect;
  Canvas.Brush.Color := clSilver;
  Canvas.FillRect(R);
  Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1);
  InflateRect(R, -3, -3);
  Frame3D(Canvas, R, clBtnShadow, clBtnHighlight, 1);
  Canvas.Brush.Color := FBackColor;
  InflateRect(R, -1, -1);
  Canvas.FillRect(R);
  Dec(R.Right);
  h := R.Bottom - R.Top;
  dh := h div 20;
  n := Round(h * (FValue - FMinimum) / (FMaximum - FMinimum) / dh);
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
