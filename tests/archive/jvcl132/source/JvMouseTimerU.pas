{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMouseTimerU.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvMouseTimerU;

interface

uses classes, controls;

type
  IMouseTimer = interface
    ['{94757B20-A74B-11D4-8CF8-CABD69ABF116}']
    procedure Attach(aControl: TControl);
    procedure Detach(aControl: TControl);
  end; { IMouseTimer }

  { Returns interface to mousetimer singleton. This interface can be used
    by objects relying on CM_MOUSEENTER/CM_MOUSELEAVE messages to make sure
    they get a CM_MOUSELEAVE under all circumstances if the mouse leaves
    their area. }
function MouseTimer: IMouseTimer;

implementation
uses Windows, Sysutils, extctrls;

type
  TJvMousetimer = class(TInterfacedObject, IMousetimer)
  private
    FTimer: TTimer;
    FCurrentControl: TControl;

    procedure TimerTick(Sender: TObject);
  protected
    { Methods of the IMousetimer interface }
    procedure Attach(aControl: TControl);
    procedure Detach(aControl: TControl);
  public
    constructor Create;
    destructor Destroy; override;
  end; { TJvMousetimer }

var
  InternalMouseTimer: IMousetimer;

function Mousetimer: IMousetimer;
begin
  if not Assigned(InternalMousetimer) then
    InternalMousetimer := TJvMousetimer.Create;
  { Note: object will be destroyed automatically during unit finalization
    through reference counting. }
  Result := InternalMousetimer;
end;

{ TJvMousetimer }

procedure TJvMousetimer.Attach(aControl: TControl);
begin
  FTimer.Enabled := False;
  if FCurrentControl <> nil then
  try
    FCurrentControl.Perform(CM_MOUSELEAVE, 0, 0);
  except
    { Ignore exception in case control has been destroyed already }
  end;
  FCurrentControl := aControl;
  if FCurrentControl <> nil then
    FTimer.Enabled := true;
end;

constructor TJvMousetimer.Create;
begin
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 200;
  FTimer.OnTimer := TimerTick;
end;

destructor TJvMousetimer.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TJvMousetimer.Detach(aControl: TControl);
begin
  if aControl = FCurrentControl then
  begin
    FTimer.Enabled := False;
    FCurrentControl := nil;
  end;
end;

procedure TJvMousetimer.TimerTick(Sender: TObject);
var
  pt: TPoint;
  r: TRect;
begin
  try
    { control may have been destroyed, so operations on it may crash.
      trap that and detach the control on exception. }
    if FCurrentControl = nil then
      FTimer.Enabled := false // paranoia
    else
    begin
      GetCursorPos(pt);
      r := FCurrentControl.BoundsRect;
      if Assigned(FCurrentControl.Parent) then
        MapWindowPoints(FCurrentControl.Parent.handle, HWND_DESKTOP, r, 2);
      if not PtInRect(r, pt) then
        FCurrentControl.Perform(CM_MOUSELEAVE, 0, 0);
    end;
  except
    Detach(FCurrentControl);
  end;
end;

end.
