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
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgStepLabel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TJvgStepLabel = class(TGraphicControl)
  private
    FStepCount: integer;
    FPassiveColor: TColor;
    FActiveColor: TColor;
    procedure SetStepCount(const Value: integer);
    { Private declarations }
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetActiveColor(const Value: TColor);
    procedure SetPassiveColor(const Value: TColor);
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property StepCount: integer read FStepCount write SetStepCount default 4;
    property ActiveColor: TColor default clWindowText read FActiveColor write SetActiveColor;
    property PassiveColor: TColor default clSilver read FPassiveColor write SetPassiveColor;
    property Font;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gl Controls', [TJvgStepLabel]);
end;

{ TJvgStepLabel }

constructor TJvgStepLabel.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TJvgStepLabel.Destroy;
begin
  inherited;
  FStepCount := 4;
  FActiveColor := clWindowText;
  FPassiveColor := clSilver;
end;

procedure TJvgStepLabel.SetActiveColor(const Value: TColor);
begin
  FActiveColor := Value;
end;

procedure TJvgStepLabel.SetPassiveColor(const Value: TColor);
begin
  FPassiveColor := Value;
end;

procedure TJvgStepLabel.SetStepCount(const Value: integer);
begin
  if Value < 1 then exit;
  FStepCount := Value;
end;

procedure TJvgStepLabel.WMPaint(var Message: TWMPaint);
var
  Caption: string;
  i: integer;
begin
  StepCount
end;

end.
