{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgStepLabel.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgStepLabel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  JvComponent;

type
  TJvgStepLabel = class(TJvGraphicControl)
  private
    FStepCount: Integer;
    FPassiveColor: TColor;
    FActiveColor: TColor;
    procedure SetStepCount(const Value: Integer);
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure SetActiveColor(const Value: TColor);
    procedure SetPassiveColor(const Value: TColor);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property StepCount: Integer read FStepCount write SetStepCount default 4;
    property ActiveColor: TColor read FActiveColor write SetActiveColor default clWindowText;
    property PassiveColor: TColor read FPassiveColor write SetPassiveColor default clSilver;
    property Font;
  end;

implementation

constructor TJvgStepLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStepCount := 4;
  FActiveColor := clWindowText;
  FPassiveColor := clSilver;
end;

procedure TJvgStepLabel.SetActiveColor(const Value: TColor);
begin
  if FActiveColor <> Value then
  begin
    FActiveColor := Value;
    Invalidate;
  end;
end;

procedure TJvgStepLabel.SetPassiveColor(const Value: TColor);
begin
  if FPassiveColor <> Value then
  begin
    FPassiveColor := Value;
    Invalidate;
  end;
end;

procedure TJvgStepLabel.SetStepCount(const Value: Integer);
begin
  if Value >= 1 then
    FStepCount := Value;
end;

procedure TJvgStepLabel.WMPaint(var Msg: TWMPaint);
var
  Caption: string;
  I: Integer;
begin
//  StepCount;
end;

end.
