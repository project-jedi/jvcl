{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRegistry.PAS, released on 2001-02-28.

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

unit JvRegistry;

{$OBJEXPORTALL On}

interface

uses
{$IFDEF DELPHI6_UP}
  Variants,
{$ENDIF}
  Windows, Registry;

type
  TJvRegistry = class(TRegistry)
  published
    procedure Write(Field: string; Value: Variant);
    function Read(Field: string): Variant;
  end;

implementation

{********************************************************}

procedure TJvRegistry.Write(Field: string; Value: Variant);
begin
  case varType(Value) of
    varCurrency:
      WriteCurrency(Field, Value);
    varBoolean:
      WriteBool(Field, Value);
    varDate:
      WriteDateTime(Field, Value);
    varSingle, varDouble:
      WriteFloat(Field, Value);
    varInteger:
      WriteInteger(Field, Value);
    varString:
      WriteString(Field, Value);
  end;
end;

{********************************************************}

function TJvRegistry.Read(Field: string): Variant;
begin
  if ValueExists(Field) then
    case varType(Result) of
      varCurrency:
        Result := ReadCurrency(Field);
      varBoolean:
        Result := ReadBool(Field);
      varDate:
        Result := ReadDateTime(Field);
      varSingle, varDouble:
        Result := ReadFloat(Field);
      varInteger:
        Result := ReadInteger(Field);
      varString:
        Result := ReadString(Field);
    end;
end;

end.
