{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgStringContainer.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgStringContainer;

interface
{$I glDEF.INC}
uses
  Windows, Messages, SysUtils, Classes;

type
  TOnReadItem = procedure(Sender: TObject; Index: integer) of object;

  TJvgStringContainer = class(TComponent)
  private
    FItems: TStringList;
    FReadOnly: boolean;
    FOnReadItem: TOnReadItem;
    function GetString(Index: integer): string;
    procedure SetString(Index: integer; const Value: string);
    procedure SetItems(Value: TStringList);
    function GetCount: integer;
  public
    property Strings[Index: Integer]: string read GetString write SetString; default;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TStringList read FItems write SetItems;
    property Count: integer read GetCount;
    property ReadOnly: boolean read FReadOnly write FReadOnly default false;
    property OnReadItem: TOnReadItem read FOnReadItem write FOnReadItem;
  end;

procedure Register;

implementation
uses JvgUtils, JvgTypes;

procedure Register;
begin
  RegisterComponents('Proba', [TJvgStringContainer]);
end;

constructor TJvgStringContainer.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor TJvgStringContainer.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TJvgStringContainer.GetString(Index: integer): string;
begin
  if Assigned(FOnReadItem) then FOnReadItem(self, Index);
  Result := FItems[Index];
end;

procedure TJvgStringContainer.SetString(Index: integer; const Value: string);
begin
  if not FReadOnly then FItems[Index] := Value;
end;

procedure TJvgStringContainer.SetItems(Value: TStringList);
begin
  FItems.Assign(Value);
end;

function TJvgStringContainer.GetCount: integer;
begin
  Result := FItems.Count;
end;

end.
