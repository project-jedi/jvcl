{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain A copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSerialMaker.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSerialMaker;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  JvTypes, JvComponent;

type
  TJvSerialMaker = class(TJvComponent)
  private
    FUserName: string;
    FBase: Integer;
    FSerial: string;
    FDummy: string;
    procedure ChangeUser(AUserName: string);
    procedure ChangeBase(ABase: Integer);
  public
    function GiveSerial(ABase: Integer; AUserName: string): string;
    function SerialIsCorrect(ABase: Integer; AUserName: string; Serial: string): Boolean;
  published
    property UserName: string read FUserName write ChangeUser;
    property Base: Integer read FBase write ChangeBase;
    { Do not store dummies }
    property Serial: string read FSerial write FDummy stored False;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvResources;

procedure TJvSerialMaker.ChangeUser(AUserName: string);
begin
  FUserName := AUserName;
  FSerial := GiveSerial(Base, AUserName);
end;

procedure TJvSerialMaker.ChangeBase(ABase: Integer);
begin
  FBase := ABase;
  FSerial := GiveSerial(ABase, UserName);
end;

function TJvSerialMaker.GiveSerial(ABase: Integer; AUserName: string): string;
var
  A: Integer;
begin
  if (ABase <> 0) and (AUserName <> '') then
  begin
    A := ABase * Length(AUserName) + Ord(AUserName[1]) * 666;
    Result := IntToStr(A) + '-';
    A := ABase * Ord(AUserName[1]) * 123;
    Result := Result + IntToStr(A) + '-';
    A := ABase + (Length(AUserName) * Ord(AUserName[1])) * 6613;
    Result := Result + IntToStr(A);
  end
  else
    Result := RsError;
end;

function TJvSerialMaker.SerialIsCorrect(ABase: Integer; AUserName: string; Serial: string): Boolean;
begin
  if (AUserName <> '') and (ABase <> 0) then
    Result := Serial = GiveSerial(ABase, AUserName)
  else
    Result := False;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

