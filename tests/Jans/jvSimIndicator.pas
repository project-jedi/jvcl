{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSimIndicator.PAS, released on 2002-06-15.

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
unit JvSimIndicator;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  extCtrls;

type
  TJvSimIndicator = class(TGraphicControl)
  private
    Fvalue: integer;
    Fmaximum: integer;
    Fminimum: integer;
    FBarColor: TColor;
    FColor: TColor;
    FBackColor: TColor;
    procedure SetBarColor(const Value: TColor);
    procedure Setmaximum(const Value: integer);
    procedure Setminimum(const Value: integer);
    procedure Setvalue(const Value: integer);
    procedure SetBackColor(const Value: TColor);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor create(AOwner: Tcomponent); override;
    destructor destroy; override;
    procedure paint; override;
  published
    { Published declarations }
    property value: integer read Fvalue write Setvalue;
    property minimum: integer read Fminimum write Setminimum;
    property maximum: integer read Fmaximum write Setmaximum;
    property BarColor: TColor read FColor write SetBarColor;
    property BackColor: TColor read FBackColor write SetBackColor;
  end;

implementation

{ TJvSimIndicator }

constructor TJvSimIndicator.create(AOwner: Tcomponent);
begin
  inherited;
  width := 25;
  height := 100;
  FMinimum := 0;
  FMaximum := 100;
  FValue := 50;
  FBarColor := cllime;
  FBackColor := clSilver;
end;

destructor TJvSimIndicator.destroy;
begin
  inherited;

end;

procedure TJvSimIndicator.paint;
var
  R, Ri: TRect;
  i, n: integer;
  h, dh: integer;
begin
  R := ClientRect;
  canvas.brush.color := clsilver;
  canvas.fillrect(R);
  Frame3D(canvas, R, clbtnhighlight, clbtnshadow, 1);
  inflaterect(R, -3, -3);
  Frame3D(canvas, R, clbtnshadow, clbtnhighlight, 1);
  canvas.brush.color := FBackColor;
  inflaterect(R, -1, -1);
  canvas.fillrect(R);
  inc(R.right, -1);
  h := R.bottom - R.top;
  dh := h div 20;
  n := round(h * (FValue - FMinimum) / (FMaximum - FMinimum) / dh);
  canvas.brush.color := FBarColor;
  Ri := rect(R.left + 1, R.bottom - dh + 1, R.right - 1, R.bottom);
  if n > 0 then
    for i := 1 to n do
    begin
      canvas.fillrect(Ri);
      inc(Ri.top, -dh);
      inc(Ri.bottom, -dh);
    end;
end;

procedure TJvSimIndicator.SetBackColor(const Value: TColor);
begin
  FBackColor := Value;
  invalidate;
end;

procedure TJvSimIndicator.SetBarColor(const Value: TColor);
begin
  FBarColor := Value;
  invalidate;
end;

procedure TJvSimIndicator.Setmaximum(const Value: integer);
begin
  Fmaximum := Value;
  invalidate;
end;

procedure TJvSimIndicator.Setminimum(const Value: integer);
begin
  Fminimum := Value;
  invalidate;
end;

procedure TJvSimIndicator.Setvalue(const Value: integer);
begin
  Fvalue := Value;
  invalidate;
end;

end.
