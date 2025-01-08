{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvConnectNetwork.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvConnectNetwork;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes,
  JvBaseDlg;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvConnectNetwork = class(TJvCommonDialog)
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvDisconnectNetwork = class(TJvCommonDialog)
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  end;

  TJvNetworkConnect = class(TJvCommonDialog)
  private
    FConnect: Boolean;
  public
    property Connect: Boolean read FConnect write FConnect;
    function Execute(ParentWnd: HWND): Boolean; overload; override;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation


function TJvConnectNetwork.Execute(ParentWnd: HWND): Boolean;
begin
  Result := WNetConnectionDialog(ParentWnd, RESOURCETYPE_DISK) = NO_ERROR;
end;

function TJvDisconnectNetwork.Execute(ParentWnd: HWND): Boolean;
begin
  Result := WNetDisconnectDialog(ParentWnd, RESOURCETYPE_DISK) = NO_ERROR;
end;

function TJvNetworkConnect.Execute(ParentWnd: HWND): Boolean;
begin
  if FConnect then
    Result := WNetConnectionDialog(ParentWnd, RESOURCETYPE_DISK) = NO_ERROR
  else
    Result := WNetDisconnectDialog(ParentWnd, RESOURCETYPE_DISK) = NO_ERROR;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
