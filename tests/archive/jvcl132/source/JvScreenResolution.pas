{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScreenResolution.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvScreenResolution;

{$ObjExportAll On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls ,JvComponent;

type
  TJvScreenResolution = class(TJvComponent)
  private
  published
    procedure GetSupportedModes(var Modes: array of TDevMode; var Count: Integer);
    function SetMode(Value: TDevMode): Boolean;
  end;

implementation

{************************************************************}

procedure TJvScreenResolution.GetSupportedModes(var Modes: array of TDevMode; var Count: Integer);
var
  i: Integer;
  DevMode: TDevMode;
begin
  i := 0;
  while EnumDisplaySettings(nil, i, DevMode) do
    Inc(i);
  Count := i;
  for i := 0 to Count - 1 do
    EnumDisplaySettings(nil, i, Modes[i])
end;

{************************************************************}

function TJvScreenResolution.SetMode(Value: TDevMode): Boolean;
begin
  Value.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT or DM_DISPLAYFLAGS;
  Result := ChangeDisplaySettings(Value, 0) = DISP_CHANGE_SUCCESSFUL;
end;

end.
