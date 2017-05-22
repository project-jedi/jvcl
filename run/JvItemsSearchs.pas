{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvItemsSearchs.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvItemsSearchs;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF COMPILER9_UP}
  Windows,
  {$ENDIF COMPILER9_UP}
  SysUtils, Classes;

type
  // (rom) made them all class functions
  TJvItemsSearchs = class(TObject)
  public
    class function SearchExactString(Items: TStrings; const Value: string;
      CaseSensitive: Boolean = True; StartIndex: Integer = -1): Integer;
    class function SearchPrefix(Items: TStrings; Value: string;
      CaseSensitive: Boolean = True; StartIndex: Integer = -1): Integer;
    class function SearchSubString(Items: TStrings; Value: string;
      CaseSensitive: Boolean = True; StartIndex: Integer = -1): Integer;
    class function DeleteExactString(Items: TStrings; const Value: string; All: Boolean;
      CaseSensitive: Boolean = True): Integer;
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


class function TJvItemsSearchs.DeleteExactString(Items: TStrings; const Value: string;
  All: Boolean; CaseSensitive: Boolean): Integer;
var
  I: Integer;
begin
  Result := 0;
  Items.BeginUpdate;
  try
    I := SearchExactString(Items, Value, CaseSensitive);
    while I <> -1 do
    begin
      Inc(Result);
      Items.Delete(I);
      if All then
        I := SearchExactString(Items, Value, CaseSensitive)
      else
        Exit;
    end;
  finally
    Items.EndUpdate;
  end;
end;

class function TJvItemsSearchs.SearchExactString(Items: TStrings; const Value: string;
  CaseSensitive: Boolean; StartIndex: Integer): Integer;
var
  I: Integer;
  HasLooped: Boolean;
begin
  Result := -1;
  I := StartIndex + 1;
  HasLooped := False;
  if CaseSensitive then
  begin
    while not HasLooped or (I <= StartIndex) do
      if I >= Items.Count then
      begin
        I := 0;
        HasLooped := True;
      end
      else
      if AnsiCompareStr(Value, Items[I]) = 0 then
      begin
        Result := I;
        Exit;
      end
      else
        Inc(I);
  end
  else
  begin
    while not HasLooped or (I <= StartIndex) do
      if I >= Items.Count then
      begin
        I := 0;
        HasLooped := True;
      end
      else
      if AnsiSameText(Value, Items[I]) then
      begin
        Result := I;
        Exit;
      end
      else
        Inc(I);
  end;
end;

class function TJvItemsSearchs.SearchPrefix(Items: TStrings; Value: string;
  CaseSensitive: Boolean; StartIndex: Integer): Integer;
var
  I: Integer;
  HasLooped: Boolean;
begin
  Result := -1;
  I := StartIndex + 1;
  HasLooped := False;
  if CaseSensitive then
  begin
    while not HasLooped or (I <= StartIndex) do
      if I >= Items.Count then
      begin
        I := 0;
        HasLooped := True;
      end
      else
      if Pos(Value, Items[I]) = 1 then
      begin
        Result := I;
        Exit;
      end
      else
        Inc(I);
  end
  else
  begin
    Value := AnsiUpperCase(Value);
    while not HasLooped or (I <= StartIndex) do
      if I >= Items.Count then
      begin
        I := 0;
        HasLooped := True;
      end
      else
      if Pos(Value, AnsiUpperCase(Items[I])) = 1 then
      begin
        Result := I;
        Exit;
      end
      else
        Inc(I);
  end;
end;

class function TJvItemsSearchs.SearchSubString(Items: TStrings; Value: string;
  CaseSensitive: Boolean; StartIndex: Integer): Integer;
var
  I: Integer;
  HasLooped: Boolean;
begin
  Result := -1;
  I := StartIndex + 1;
  HasLooped := False;
  if CaseSensitive then
  begin
    while not HasLooped or (I <= StartIndex) do
      if I >= Items.Count then
      begin
        I := 0;
        HasLooped := True;
      end
      else
      if Pos(Value, Items[I]) <> 0 then
      begin
        Result := I;
        Exit;
      end
      else
        Inc(I);
  end
  else
  begin
    Value := AnsiUpperCase(Value);
    while not HasLooped or (I <= StartIndex) do
      if I >= Items.Count then
      begin
        I := 0;
        HasLooped := True;
      end
      else
      if Pos(Value, AnsiUpperCase(Items[I])) <> 0 then
      begin
        Result := I;
        Exit;
      end
      else
        Inc(I);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
