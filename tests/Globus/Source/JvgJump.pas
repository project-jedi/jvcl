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

unit JvgJump;

interface

uses
  Windows,
  Messages,
  SysUtils,
  JVComponent,
  Classes,
  Graphics,
  Controls,
  ExtCtrls; //MMSystem;

type
  TJvgJumpingComponent = class(TJvComponent)
  private
    FStep: word;
    FActiveControl: TControl;
    FTimerInterval: word;
    FEnabled: boolean;
    FOnTimer: TNotifyEvent;
    Timer: TTimer;
    l, t, HShift, VShift: integer;
    HDir, VDir: boolean;
    procedure SetStep(Value: word);
    procedure SetTimerInterval(Value: word);
    procedure SetEnabled(Value: boolean);

    procedure SetActiveControl(Control: TControl);
    procedure SetDir(h, v: boolean);
    procedure OnTimerProc(Sender: TObject);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Step: word read FStep write SetStep default 10;
    property ActiveControl: TControl read FActiveControl write
      SetActiveControl;
    property TimerInterval: word read FTimerInterval write SetTimerInterval
      default 10;
    property Enabled: boolean read FEnabled write SetEnabled default false;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

procedure Register;

implementation

procedure Register;
begin
end;
//---------------------

constructor TJvgJumpingComponent.Create(AOwner: TComponent);
begin
  SetDir(true, true);

  FStep := 10;
  FTimerInterval := 10;
  Timer := TTimer.Create(self);
  Timer.Interval := FTimerInterval;
  Timer.Enabled := false;
  Timer.OnTimer := OnTimerProc;
  SetDir(true, true);
  inherited;
end;
//-----

destructor TJvgJumpingComponent.Destroy;
begin
  Timer.Enabled := false;
  Timer.Free;
  FActiveControl := nil;
  inherited;
end;
//-----

procedure TJvgJumpingComponent.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FActiveControl) and (Operation = opRemove) then
    ActiveControl := nil;
end;
//-----

procedure TJvgJumpingComponent.SetStep(Value: word);
begin
  if Value <> 0 then
  begin
    FStep := Value;
    SetDir(HDir, VDir);
  end;
end;
//-----

procedure TJvgJumpingComponent.SetActiveControl(Control: TControl);
begin
  if FActiveControl <> Control then
  begin
    FActiveControl := Control;
    if Control = nil then
    begin
      Timer.Enabled := false
    end
    else
      with FActiveControl do
      begin
        l := left;
        t := top;
      end;
  end;
end;
//-----

procedure TJvgJumpingComponent.OnTimerProc;
var
  f: boolean;
  r: TRect;
  ParentWidth, ParentHeight: integer;
begin
  if FActiveControl = nil then
    exit;
  if Assigned(FOnTimer) then
    FOnTimer(self);
  with FActiveControl do
  begin
    f := false;
    r := parent.ClientRect;
    ParentWidth := r.right - r.left;
    ParentHeight := r.bottom - r.top;
    l := l + HShift;
    t := t + VShift;
    if l <= 0 then
    begin
      HDir := not HDir;
      f := true;
    end;
    if t <= 0 then
    begin
      VDir := not VDir;
      f := true;
    end;
    if l + width >= parentWidth then
    begin
      HDir := not HDir;
      f := true;
    end;
    if t + height >= parentHeight then
    begin
      VDir := not VDir;
      f := true;
    end;
    if f then
      SetDir(HDir, VDir)
    else
    begin
      Left := l;
      Top := t;
    end;
  end;
end;
//-----

procedure TJvgJumpingComponent.SetDir(h, v: boolean);
begin
  HDir := h;
  VDir := v;
  if h then
    HShift := FStep
  else
    HShift := -FStep;
  if v then
    VShift := FStep
  else
    VShift := -FStep;
end;
//-----

procedure TJvgJumpingComponent.SetTimerInterval(Value: word);
begin
  if (FTimerInterval = Value) or (Value < 1) then
    exit;
  FTimerInterval := Value;
  Timer.Interval := Value;
end;
//-----

procedure TJvgJumpingComponent.SetEnabled(Value: boolean);
begin
  if (Enabled = Value) or (FActiveControl = nil) then
    exit;

  FEnabled := Value;
  Timer.Enabled := Value;
end;
//-----

end.
