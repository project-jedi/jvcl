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

UNIT JvgStepLabel;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   Classes,
   Graphics,
   Controls,
   Forms,
   Dialogs;

TYPE
   TJvgStepLabel = CLASS(TJvGraphicControl)
   PRIVATE
      FStepCount: integer;
      FPassiveColor: TColor;
      FActiveColor: TColor;
      PROCEDURE SetStepCount(CONST Value: integer);
      { Private declarations }
      PROCEDURE WMPaint(VAR Message: TWMPaint); MESSAGE WM_PAINT;
      PROCEDURE SetActiveColor(CONST Value: TColor);
      PROCEDURE SetPassiveColor(CONST Value: TColor);
   PROTECTED
      { Protected declarations }
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
   PUBLISHED
      PROPERTY StepCount: integer READ FStepCount WRITE SetStepCount DEFAULT 4;
      PROPERTY ActiveColor: TColor READ FActiveColor WRITE SetActiveColor DEFAULT
         clWindowText;
      PROPERTY PassiveColor: TColor READ FPassiveColor WRITE SetPassiveColor
         DEFAULT clSilver;
      PROPERTY Font;

   END;

PROCEDURE Register;

IMPLEMENTATION

PROCEDURE Register;
BEGIN
   RegisterComponents('Gl Controls', [TJvgStepLabel]);
END;

{ TJvgStepLabel }

CONSTRUCTOR TJvgStepLabel.Create(AOwner: TComponent);
BEGIN
   INHERITED;

END;

DESTRUCTOR TJvgStepLabel.Destroy;
BEGIN
   INHERITED;
   FStepCount := 4;
   FActiveColor := clWindowText;
   FPassiveColor := clSilver;
END;

PROCEDURE TJvgStepLabel.SetActiveColor(CONST Value: TColor);
BEGIN
   FActiveColor := Value;
END;

PROCEDURE TJvgStepLabel.SetPassiveColor(CONST Value: TColor);
BEGIN
   FPassiveColor := Value;
END;

PROCEDURE TJvgStepLabel.SetStepCount(CONST Value: integer);
BEGIN
   IF Value < 1 THEN
      exit;
   FStepCount := Value;
END;

PROCEDURE TJvgStepLabel.WMPaint(VAR Message: TWMPaint);
VAR
   Caption                    : STRING;
   i                          : integer;
BEGIN
   StepCount
END;

END.

