{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRunOnStartup.PAS, released on 2001-02-28.

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

unit JvRunOnStartup;

{$OBJEXPORTALL On}

interface

// (rom) the JCL has a function for this

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Registry, JvTypes, JvComponent;

type
  TJvRunOnStartup = class(TJvComponent)
  private
  published
    procedure SetRunOnStartup(Title, CommandLine: string; RunOnce, Run: Boolean);
    function GetRunOnce(Title: string): Boolean;
    function GetRun(Title: string): Boolean;
  end;

implementation

resourcestring
  RC_RunKey = 'Software\Microsoft\Windows\CurrentVersion\Run';
  RC_RunOnceKey = 'Software\Microsoft\Windows\CurrentVersion\RunOnce';

  {**************************************************}

function TJvRunOnStartup.GetRun(Title: string): Boolean;
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey(RC_RunKey, False);
    Result := ValueExists(Title);
    Free;
  end;
end;

{**************************************************}

function TJvRunOnStartup.GetRunOnce(Title: string): Boolean;
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey(RC_RunOnceKey, False);
    Result := ValueExists(Title);
    Free;
  end;
end;

{**************************************************}

rocedure TJvRunOnStartup.SetRunOnStartup(Title, CommandLine: string;
  RunOnce, Run: boolean);
begin
  if RunOnce then
  begin
    with TRegistry.Create do
    try
      Rootkey := HKEY_LOCAL_MACHINE;
      OpenKey(RC_RunOnceKey, false);
      WriteString(Title, CommandLine);
    finally
      free;
    end;
  end
  else
  begin
    with TRegistry.Create do
    try
      Rootkey := HKEY_LOCAL_MACHINE;
      OpenKey(RC_RunOnceKey, false);
      if ValueExists(Title) then
        DeleteValue(Title);
    finally
      free;
    end;
  end;

  if run then
  begin
    with TRegistry.Create do
    try
      Rootkey := HKEY_LOCAL_MACHINE;
      OpenKey(RC_RunKey, false);
      WriteString(Title, CommandLine);
    finally
      free;
    end;
  end
  else
  begin
    with TRegistry.Create do
    try
      Rootkey := HKEY_LOCAL_MACHINE;
      OpenKey(RC_RunKey, false);
      if ValueExists(Title) then
        DeleteValue(Title);
    finally
      free;
    end;
  end;
end;

end.

