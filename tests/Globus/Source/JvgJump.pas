{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgJump.PAS, released on 2003-01-15.

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

// This unit implements the TJvgJumpingComponent joke component. :)

UNIT JvgJump;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   JVComponent,
   Classes,
   Graphics,
   Controls,
   ExtCtrls;                            //MMSystem;

TYPE
   TJvgJumpingComponent = CLASS(TJvComponent)
   PRIVATE
      FStep: word;
      FActiveControl: TControl;
      FTimerInterval: word;
      FEnabled: boolean;
      FOnTimer: TNotifyEvent;
      Timer: TTimer;
      l, t, HShift, VShift: integer;
      HDir, VDir: boolean;
      PROCEDURE SetStep(Value: word);
      PROCEDURE SetTimerInterval(Value: word);
      PROCEDURE SetEnabled(Value: boolean);

      PROCEDURE SetActiveControl(Control: TControl);
      PROCEDURE SetDir(h, v: boolean);
      PROCEDURE OnTimerProc(Sender: TObject);

   PROTECTED
      PROCEDURE Notification(AComponent: TComponent; Operation: TOperation);
         OVERRIDE;
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;

   PUBLISHED
      PROPERTY Step: word READ FStep WRITE SetStep DEFAULT 10;
      PROPERTY ActiveControl: TControl READ FActiveControl WRITE
         SetActiveControl;
      PROPERTY TimerInterval: word READ FTimerInterval WRITE SetTimerInterval
         DEFAULT 10;
      PROPERTY Enabled: boolean READ FEnabled WRITE SetEnabled DEFAULT false;
      PROPERTY OnTimer: TNotifyEvent READ FOnTimer WRITE FOnTimer;
   END;

PROCEDURE Register;

IMPLEMENTATION

PROCEDURE Register;
BEGIN
END;
//---------------------

CONSTRUCTOR TJvgJumpingComponent.Create(AOwner: TComponent);
BEGIN
   SetDir(true, true);

   FStep := 10;
   FTimerInterval := 10;
   Timer := TTimer.Create(self);
   Timer.Interval := FTimerInterval;
   Timer.Enabled := false;
   Timer.OnTimer := OnTimerProc;
   SetDir(true, true);
   INHERITED;
END;
//-----

DESTRUCTOR TJvgJumpingComponent.Destroy;
BEGIN
   Timer.Enabled := false;
   Timer.Free;
   FActiveControl := NIL;
   INHERITED;
END;
//-----

PROCEDURE TJvgJumpingComponent.Notification(AComponent: TComponent; Operation:
   TOperation);
BEGIN
   INHERITED Notification(AComponent, Operation);
   IF (AComponent = FActiveControl) AND (Operation = opRemove) THEN
      ActiveControl := NIL;
END;
//-----

PROCEDURE TJvgJumpingComponent.SetStep(Value: word);
BEGIN
   IF Value <> 0 THEN
   BEGIN
      FStep := Value;
      SetDir(HDir, VDir);
   END;
END;
//-----

PROCEDURE TJvgJumpingComponent.SetActiveControl(Control: TControl);
BEGIN
   IF FActiveControl <> Control THEN
   BEGIN
      FActiveControl := Control;
      IF Control = NIL THEN
      BEGIN
         Timer.Enabled := false
      END
      ELSE
         WITH FActiveControl DO
         BEGIN
            l := left;
            t := top;
         END;
   END;
END;
//-----

PROCEDURE TJvgJumpingComponent.OnTimerProc;
VAR
   f                          : boolean;
   r                          : TRect;
   ParentWidth, ParentHeight  : integer;
BEGIN
   IF FActiveControl = NIL THEN
      exit;
   IF Assigned(FOnTimer) THEN
      FOnTimer(self);
   WITH FActiveControl DO
   BEGIN
      f := false;
      r := parent.ClientRect;
      ParentWidth := r.right - r.left;
      ParentHeight := r.bottom - r.top;
      l := l + HShift;
      t := t + VShift;
      IF l <= 0 THEN
      BEGIN
         HDir := NOT HDir;
         f := true;
      END;
      IF t <= 0 THEN
      BEGIN
         VDir := NOT VDir;
         f := true;
      END;
      IF l + width >= parentWidth THEN
      BEGIN
         HDir := NOT HDir;
         f := true;
      END;
      IF t + height >= parentHeight THEN
      BEGIN
         VDir := NOT VDir;
         f := true;
      END;
      IF f THEN
         SetDir(HDir, VDir)
      ELSE
      BEGIN
         Left := l;
         Top := t;
      END;
   END;
END;
//-----

PROCEDURE TJvgJumpingComponent.SetDir(h, v: boolean);
BEGIN
   HDir := h;
   VDir := v;
   IF h THEN
      HShift := FStep
   ELSE
      HShift := -FStep;
   IF v THEN
      VShift := FStep
   ELSE
      VShift := -FStep;
END;
//-----

PROCEDURE TJvgJumpingComponent.SetTimerInterval(Value: word);
BEGIN
   IF (FTimerInterval = Value) OR (Value < 1) THEN
      exit;
   FTimerInterval := Value;
   Timer.Interval := Value;
END;
//-----

PROCEDURE TJvgJumpingComponent.SetEnabled(Value: boolean);
BEGIN
   IF (Enabled = Value) OR (FActiveControl = NIL) THEN
      exit;

   FEnabled := Value;
   Timer.Enabled := Value;
END;
//-----

END.

