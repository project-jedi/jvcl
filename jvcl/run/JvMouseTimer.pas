{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMouseTimerU.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113 dott 1101 att compuserve dott com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Returns interface to mousetimer singleton. This interface can be used
  by objects relying on CM_MOUSEENTER/CM_MOUSELEAVE messages to make sure
  they get a CM_MOUSELEAVE under all circumstances if the mouse leaves
  their area.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvMouseTimer;

interface

uses
  {$IFDEF VCL}
  Windows, Controls, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, QWindows, QControls, QExtCtrls,
  {$ENDIF VisualCLX}
  SysUtils, Classes;

type
  IMouseTimer = interface
    ['{94757B20-A74B-11D4-8CF8-CABD69ABF116}']
    procedure Attach(AControl: TControl);
    procedure Detach(AControl: TControl);
  end;

function MouseTimer: IMouseTimer;
function IsValidMouseTimer: Boolean;

implementation

type
  TOpenControl = class(TControl);

  TJvMouseTimerNotify = class(TComponent)
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

  TJvMouseTimer = class(TInterfacedObject, IMouseTimer)
  private
    FTimer: TTimer;

    FCurrentControl: TOpenControl;
    FNotify: TJvMouseTimerNotify;
    procedure TimerTick(Sender: TObject);
  protected
    { Methods of the IMouseTimer interface }
    procedure Attach(AControl: TControl);
    procedure Detach(AControl: TControl);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  InternalMouseTimer: IMouseTimer;

function MouseTimer: IMouseTimer;
begin
  if not Assigned(InternalMouseTimer) then
    InternalMouseTimer := TJvMouseTimer.Create;
  { Note: object will be destroyed automatically during unit finalization
    through reference counting. }
  Result := InternalMouseTimer;
end;

function IsValidMouseTimer: Boolean;
begin
  Result := Assigned(InternalMouseTimer);
end;

procedure TJvMouseTimerNotify.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if IsValidMouseTimer and (Operation = opRemove) and (AComponent is TControl) then
    MouseTimer.Detach(TControl(AComponent));
end;


constructor TJvMouseTimer.Create;
begin
  inherited Create;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 200;
  FTimer.OnTimer := TimerTick;
end;

destructor TJvMouseTimer.Destroy;
begin
  FTimer.Free;
  FNotify.Free;
  inherited Destroy;
end;

procedure TJvMouseTimer.Attach(AControl: TControl);
begin
  FTimer.Enabled := False;
  if FCurrentControl <> nil then
  try
    FCurrentControl.RemoveFreeNotification(FNotify);
    {$IFDEF VCL}
    FCurrentControl.Perform(CM_MOUSELEAVE, 0, 0);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    FCurrentControl.MouseLeave(FCurrentControl);
    {$ENDIF VisualCLX}
  except
    { Ignore exception in case control has been destroyed already }
  end;
  FCurrentControl := TOpenControl(AControl);
  if FCurrentControl <> nil then
  begin
    if not Assigned(FNotify) then
      FNotify := TJvMouseTimerNotify.Create(nil);
    FCurrentControl.FreeNotification(FNotify);
    FTimer.Enabled := True;
  end;
end;

procedure TJvMouseTimer.Detach(AControl: TControl);
begin
  if AControl = FCurrentControl then
  begin
    FTimer.Enabled := False;
    if Assigned(FNotify) and (FCurrentControl <> nil) then
      FCurrentControl.RemoveFreeNotification(FNotify);
    FCurrentControl := nil;
  end;
end;

procedure TJvMouseTimer.TimerTick(Sender: TObject);
var
  Pt: TPoint;
  R: TRect;
begin
  try
    { control may have been destroyed, so operations on it may crash.
      trap that and detach the control on exception. }
    if FCurrentControl = nil then
      FTimer.Enabled := False // paranoia
    else
    begin
      GetCursorPos(Pt);
      R := FCurrentControl.BoundsRect;
      if Assigned(FCurrentControl.Parent) then
        MapWindowPoints(FCurrentControl.Parent.Handle, HWND_DESKTOP, R, 2);
      if not PtInRect(R, Pt) then
        {$IFDEF VCL}
        FCurrentControl.Perform(CM_MOUSELEAVE, 0, 0);
        {$ENDIF VCL}
        {$IFDEF VisualCLX}
        FCurrentControl.MouseLeave(FCurrentControl);
        {$ENDIF VisualCLX}
    end;
  except
    Detach(FCurrentControl);
  end;
end;

end.

