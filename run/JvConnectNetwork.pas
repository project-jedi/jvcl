{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvConnectNetwork.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2002-05-23

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvConnectNetwork;

interface

uses
  Forms,
  JvBaseDlg;

type
  TJvConnectNetwork = class(TJvCommonDialog)
  published
    function Execute: Boolean; override;
  end;

  TJvDisconnectNetwork = class(TJvCommonDialog)
  published
    function Execute: Boolean; override;
  end;

  TJvNetworkConnect = class(TJvCommonDialog)
  private
    FConnect: Boolean;
  published
    property Connect: Boolean read FConnect write FConnect;
    function Execute: Boolean; override;
  end;

implementation

uses
  Windows;

function TJvConnectNetwork.Execute: Boolean;
begin
  Result := WNetConnectionDialog(Application.Handle, RESOURCETYPE_DISK) = NO_ERROR;
end;

function TJvDisconnectNetwork.Execute: Boolean;
begin
  Result := WNetDisconnectDialog(Application.Handle, RESOURCETYPE_DISK) = NO_ERROR;
end;

function TJvNetworkConnect.Execute: Boolean;
begin
  if FConnect then
    Result := WNetConnectionDialog(Application.Handle, RESOURCETYPE_DISK) = NO_ERROR
  else
    Result := WNetDisconnectDialog(Application.Handle, RESOURCETYPE_DISK) = NO_ERROR;
end;

end.

