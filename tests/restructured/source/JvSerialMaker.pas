{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSerialMaker.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSerialMaker;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvTypes, JvComponent;

type
  TJvSerialMaker = class(TJvComponent)
  private
    FUsername: string;
    FBase: Integer;
    FSerial: string;
    FBidon: string;
    procedure ChangeUser(User: string);
    procedure ChangeBase(Base: Integer);
  public
    function GiveSerial(Base: Integer; Username: string): string;
    function SerialIsCorrect(Base: Integer; Username: string; Serial: string): Boolean;
  published
    property Username: string read FUsername write ChangeUser;
    property Base: Integer read Fbase write ChangeBase;
    property Serial: string read FSerial write FBidon;
  end;

implementation

{***************************************************}

procedure TJvSerialMaker.ChangeUser(User: string);
begin
  FUsername := User;
  FSerial := GiveSerial(FBase, FUsername);
end;

{***************************************************}

procedure TJvSerialMaker.ChangeBase(Base: Integer);
begin
  FBase := Base;
  FSerial := GiveSerial(FBase, FUsername);
end;

{***************************************************}

function TJvSerialMaker.GiveSerial(Base: Integer; Username: string): string;
var
  st: string;
  a: Integer;
begin
  st := 'Error';
  if (Base <> 0) and (Username <> '') then
  begin
    a := Base * Length(Username) + Ord(Username[1]) * 666;
    st := IntToStr(a) + '-';
    a := Base * Ord(Username[1]) * 123;
    st := st + IntToStr(a) + '-';
    a := Base + (Length(Username) * Ord(Username[1])) * 6613;
    st := st + IntToStr(a);
  end;
  Result := st;
end;

{***************************************************}

function TJvSerialMaker.SerialIsCorrect(Base: Integer; Username: string; Serial: string): Boolean;
begin
  if Username = '' then
    Result := False
  else if Base = 0 then
    Result := False
  else
    Result := Serial = GiveSerial(Base, Username);
end;

end.
