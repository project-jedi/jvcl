{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvItemsSearchs.PAS, released on 2001-02-28.

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

unit JvItemsSearchs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms;

type
  TJvItemsSearchs = class(TObject)
  public
    function SearchExactString(Items: TStrings; Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchPrefix(Items: TStrings; Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchSubString(Items: TStrings; Value: string; CaseSensitive: Boolean = True): Integer;
    function DeleteExactString(Items: TStrings; Value: string; All: Boolean;
      CaseSensitive: Boolean = True): Integer;
  end;

implementation

{***********************************************}

function TJvItemsSearchs.DeleteExactString(Items: TStrings; Value: string;
  All, CaseSensitive: Boolean): Integer;
var
  i: Integer;
begin
  Result := 0;
  i := SearchExactString(Items, Value, CaseSensitive);
  while i <> -1 do
  begin
    Inc(Result);
    Items.Delete(i);
    if All then
      i := SearchExactString(Items, Value, CaseSensitive)
    else
      Exit;
  end;
end;

{***********************************************}

function TJvItemsSearchs.SearchExactString(Items: TStrings; Value: string;
  CaseSensitive: Boolean): Integer;
var
  i: Integer;
begin
  Result := -1;
  if CaseSensitive then
  begin
    for i := 0 to Items.Count - 1 do
      if AnsiCompareStr(Value, Items[i]) = 0 then
      begin
        Result := i;
        Exit;
      end;
  end
  else
  begin
    for i := 0 to Items.Count - 1 do
      if AnsiCompareText(Value, Items[i]) = 1 then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

{***********************************************}

function TJvItemsSearchs.SearchPrefix(Items: TStrings; Value: string;
  CaseSensitive: Boolean): Integer;
var
  i: Integer;
begin
  Result := -1;
  if CaseSensitive then
  begin
    for i := 0 to Items.Count - 1 do
      if Pos(Value, Items[i]) = 1 then
      begin
        Result := i;
        Exit;
      end;
  end
  else
  begin
    Value := AnsiUpperCase(Value);
    for i := 0 to Items.Count - 1 do
      if Pos(Value, AnsiUpperCase(Items[i])) = 1 then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

{***********************************************}

function TJvItemsSearchs.SearchSubString(Items: TStrings; Value: string;
  CaseSensitive: Boolean): Integer;
var
  i: Integer;
begin
  Result := 0;
  if CaseSensitive then
  begin
    for i := 0 to Items.Count - 1 do
      if Pos(Value, Items[i]) <> 0 then
      begin
        Result := i;
        Exit;
      end;
  end
  else
  begin
    Value := AnsiUpperCase(Value);
    for i := 0 to Items.Count - 1 do
      if Pos(Value, AnsiUpperCase(Items[i])) <> 0 then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

end.

