{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScreenResolution.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvScreenResolution;

interface

// (rom) definitely JCL or Archive

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils;

type
  TJvScreenResolution = class(TObject)
  private
    FCount: Integer;
    function GetCount: Integer;
    function GetMode(Index: Integer): TDevMode;
  public
    procedure GetSupportedModes(var Modes: array of TDevMode; var Count: Integer);
    function SetMode(Value: TDevMode): Boolean;
    // simpler access to DevModes
    property Modes[Index: Integer]: TDevMode read GetMode;
    property Count: Integer read GetCount;
  end;

implementation

uses
  {$IFDEF COMPILER6_UP}
  RTLConsts,
  {$ELSE}
  Consts,
  {$ENDIF COMPILER6_UP}
  JvTypes;

function TJvScreenResolution.GetCount: Integer;
var
  DevMode: TDevMode;
begin
  if FCount = 0 then
    while EnumDisplaySettings(nil, FCount, DevMode) do
      Inc(FCount);
  Result := FCount;
end;

function TJvScreenResolution.GetMode(Index: Integer): TDevMode;
begin
  if (Index < 0) or (Index >= Count) then
    raise EJVCLException.CreateResFmt(@SListIndexError, [Index]);
  EnumDisplaySettings(nil, Index, Result);
end;

procedure TJvScreenResolution.GetSupportedModes(var Modes: array of TDevMode;
  var Count: Integer);
var
  I: Integer;
  DevMode: TDevMode;
begin
  I := 0;
  while EnumDisplaySettings(nil, I, DevMode) do
    Inc(I);
  Count := I;
  for I := 0 to Count - 1 do
    EnumDisplaySettings(nil, I, Modes[I]);
end;

function TJvScreenResolution.SetMode(Value: TDevMode): Boolean;
begin
  Value.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT or DM_DISPLAYFLAGS;
  Result := ChangeDisplaySettings(Value, 0) = DISP_CHANGE_SUCCESSFUL;
end;

end.

