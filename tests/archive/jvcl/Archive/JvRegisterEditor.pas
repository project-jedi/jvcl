{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRegisterEditor.PAS, released on 2001-02-28.

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

unit JvRegisterEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExptIntf,
  JvFormAlarms;

type
  TJvRegisterExpert = class(TIExpert)
  public
    function GetName: string; override; stdcall;
    function GetAuthor: string; override; stdcall;
    function GetStyle: TExpertstyle; override; stdcall;
    function GetIDstring: string; override; stdcall;
    function GetMenuText: string; override; stdcall;
    function GetState: TExpertState; override; stdcall;
    procedure Execute; override; stdcall;
  end;

implementation

{*************************************************}

procedure TJvRegisterExpert.Execute;
type
  TProcedure = procedure;
var
  hDll: THandle;
  ShowAbout: TProcedure;
begin
  hDll := LoadLibrary('RegPack.dll');
  if hDll <> 0 then
  begin
    @ShowAbout := GetProcAddress(hDll, 'ShowAbout');
    if Assigned(ShowAbout) then
      ShowAbout;
    FreeLibrary(hDll);
  end
  else
    ShowMessage('Unable to find RegPack.dll!');
end;

{*************************************************}

function TJvRegisterExpert.GetAuthor: string;
begin
  // (rom) change to J-VCL?
  Result := 'BuyPin Software';
end;

{*************************************************}

function TJvRegisterExpert.GetIDstring: string;
begin
  Result := 'TJvRegisterExpert';
end;

{*************************************************}

function TJvRegisterExpert.GetMenuText: string;
begin
  Result := 'JvPack About';
end;

{*************************************************}

function TJvRegisterExpert.GetName: string;
begin
  Result := 'TJvRegisterExpert';
end;

{*************************************************}

function TJvRegisterExpert.GetState: TExpertState;
begin
  Result := [esEnabled];
end;

{*************************************************}

function TJvRegisterExpert.GetStyle: TExpertstyle;
begin
  Result := esStandard;
end;

end.
