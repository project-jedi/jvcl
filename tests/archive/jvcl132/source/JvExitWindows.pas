{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExitWindows.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvExitWindows;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvTypes, Registry, JvComponent;

type
  TJvExitWindows = class(TJvComponent)
  private

  published
    function RestartWindows: Boolean;
    function RebootSystem: Boolean;
    function ForceRebooting: Boolean;
    function LogOffUser: Boolean;
    function PowerOff: Boolean;
    function ShutDown: Boolean;
    procedure ExecOnceRebooted(App: string);
  end;

implementation

resourcestring
  RC_RunOnceKey = 'Software\Microsoft\Windows\CurrentVersion\RunOnce';

  {******************************************************}

function TJvExitWindows.RestartWindows: Boolean;
begin
  Result := ExitWindows(EW_RESTARTWINDOWS, 0);
end;

{******************************************************}

function TJvExitWindows.RebootSystem: Boolean;
begin
  Result := ExitWindows(EW_REBOOTSYSTEM, 0);
end;

{******************************************************}

function TJvExitWindows.ForceRebooting: Boolean;
begin
  Result := ExitwindowsEx(EWX_FORCE, 0);
end;

{******************************************************}

function TJvExitWindows.LogOffUser: Boolean;
begin
  Result := ExitWindowsEx(EWX_LOGOFF, 0);
end;

{******************************************************}

function TJvExitWindows.PowerOff: Boolean;
begin
  Result := ExitWindowsEx(EWX_POWEROFF, 0);
end;

{******************************************************}

function TJvExitWindows.ShutDown: Boolean;
begin
  Result := ExitWindowsEx(EWX_SHUTDOWN, 0);
end;

{******************************************************}

procedure TJvExitWindows.ExecOnceRebooted(App: string);
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey(RC_RunOnceKey, False);
    // (rom) should be improved to use variable key
    // (rom) to allow registering more than one app
    WriteString('ToRun', App);
    Free;
  end;
end;

end.
