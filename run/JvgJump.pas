{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgJump.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  This unit implements the TJvgJumpingComponent joke component. :)

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgJump;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls,
  JvComponent;

type
  TJvgJumpingComponent = class(TJvComponent)
  private
    FLeft: Integer;
    FTop: Integer;
    FStep: Word;
    FActiveControl: TControl;
    FTimerInterval: Word;
    FActive: Boolean;
    FOnTimer: TNotifyEvent;
    FTimer: TTimer;
    FHDirection: Boolean;
    FVDirection: Boolean;
    procedure SetStep(Value: Word);
    procedure SetTimerInterval(Value: Word);
    procedure SetActive(Value: Boolean);
    procedure SetActiveControl(Control: TControl);
    procedure OnTimerProc(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Step: Word read FStep write SetStep default 5;
    property ActiveControl: TControl read FActiveControl write SetActiveControl;
    property TimerInterval: Word read FTimerInterval write SetTimerInterval default 20;
    property Active: Boolean read FActive write SetActive default False;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

constructor TJvgJumpingComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStep := 5;
  FTimerInterval := 20;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := FTimerInterval;
  FTimer.Enabled := False;
  FTimer.OnTimer := OnTimerProc;
  FHDirection := True;
  FVDirection := True;
end;

destructor TJvgJumpingComponent.Destroy;
begin
  ActiveControl := nil;
  inherited Destroy;
end;

procedure TJvgJumpingComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FActiveControl) and (Operation = opRemove) then
    ActiveControl := nil;
end;

procedure TJvgJumpingComponent.SetStep(Value: Word);
begin
  if Value <> 0 then
    FStep := Value;
end;

procedure TJvgJumpingComponent.SetActiveControl(Control: TControl);
begin
  if FActiveControl <> Control then
  begin
    if Control = nil then
      Active := False
    else
    begin
      FLeft := Control.Left;
      FTop := Control.Top;
    end;
    FActiveControl := Control;
  end;
end;

procedure TJvgJumpingComponent.SetActive(Value: Boolean);
begin
  if (Active <> Value) and Assigned(FActiveControl) then
  begin
    FActive := Value;
    FTimer.Enabled := Value;
  end;
  if not Value and Assigned(FActiveControl) and
    not (csDestroying in ComponentState) then
  begin
    FActiveControl.Left := FLeft;
    FActiveControl.Top := FTop;
  end;
end;

procedure TJvgJumpingComponent.OnTimerProc;
var
  R: TRect;
  NL, NT: Integer;
  ParentWidth, ParentHeight: Integer;
begin
  if not Assigned(FActiveControl) then
    Exit;
  if Assigned(FOnTimer) then
    FOnTimer(Self);
  with FActiveControl do
  begin
    R := Parent.ClientRect;
    ParentWidth := R.Right - R.Left;
    ParentHeight := R.Bottom - R.Top;
    if FHDirection then
      NL := Left + Step
    else
      NL := Left - Step;
    if FVDirection then
      NT := Top + Step
    else
      NT := Top - Step;
    if NL < 0 then
    begin
      FHDirection := not FHDirection;
      NL := 0;
    end;
    if NT < 0 then
    begin
      FVDirection := not FVDirection;
      NT := 0;
    end;
    if NL + Width >= ParentWidth then
    begin
      FHDirection := not FHDirection;
      NL := ParentWidth - Width;
    end;
    if NT + Height >= ParentHeight then
    begin
      FVDirection := not FVDirection;
      NT := ParentHeight - Height;
    end;
    SetBounds(NL, NT, Width, Height);
  end;
end;

procedure TJvgJumpingComponent.SetTimerInterval(Value: Word);
begin
  if (FTimerInterval <> Value) and (Value > 0) then
  begin
    FTimerInterval := Value;
    FTimer.Interval := Value;
  end;
end;

end.

