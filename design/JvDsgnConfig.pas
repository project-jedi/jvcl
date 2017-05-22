{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDsgnConfig.pas, released on 2007-12-15.

The Initial Developers of the Original Code is Andreas Hausladen
Portions created by Andreas Hausladen are Copyright (C) 2007 Project JEDI
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDsgnConfig;

{$I jvcl.inc}

interface

var
  JvOptionRegisterGlobalDesignEditors: Boolean = False;

implementation

uses
  SysUtils, Registry, ToolsAPI;

procedure ReadOption(Reg: TRegistry; const OptionName: string; var Option: Boolean);
begin
  if Reg.ValueExists(OptionName) then
    Option := Reg.ReadBool(OptionName);
end;

procedure LoadConfig;
var
  Reg: TRegistry;
  RootKey: string;
  Services: IOTAServices;
begin
  if Supports(BorlandIDEServices, IOTAServices, Services) then
  begin
    RootKey := Services.GetBaseRegistryKey;

    Reg := TRegistry.Create;
    try
      if Reg.OpenKeyReadOnly(RootKey + '\Jedi\JVCL\IDE') then
      begin
        ReadOption(Reg, 'RegisterGlobalDesignEditors', JvOptionRegisterGlobalDesignEditors);
      end;
    finally
      Reg.Free;
    end;
  end;
end;

initialization
  LoadConfig;

end.
