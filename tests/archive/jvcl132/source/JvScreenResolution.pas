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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvScreenResolution;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvComponent;

type
  TJvScreenResolution = class(TJvComponent)
  private
    FCount:integer;
    function GetCount: integer;
    function GetMode(Index: integer): TDevMode;
  public
    procedure GetSupportedModes(var Modes: array of TDevMode; var Count: Integer);
    function SetMode(Value: TDevMode): Boolean;
    // simpler access to DevModes
    property Modes[Index:integer]:TDevMode read GetMode;
    property Count:integer read GetCount;
  end;

implementation
uses
  Jvtypes,
{$IFDEF DELPHI6_UP}
  RTLConsts
{$ELSE}
  Consts
{$ENDIF}
;

{************************************************************}

function TJvScreenResolution.GetCount: integer;
var DevMode:TDevMode;
begin
  if FCount = 0 then
    while EnumDisplaySettings(nil, FCount, DevMode) do
      Inc(FCount);
  Result := FCount;
end;

function TJvScreenResolution.GetMode(Index: integer): TDevMode;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.CreateFmt(SListIndexError,[Index]);
  EnumDisplaySettings(nil,Index,Result);
end;

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
