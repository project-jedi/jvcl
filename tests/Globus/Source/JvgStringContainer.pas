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

uses
  Windows, Messages, SysUtils, Classes,
  JvComponent;

type
  TOnReadItem = procedure(Sender: TObject; Index: Integer) of object;

  TJvgStringContainer = class(TJvComponent)
  private
    FItems: TStringList;
    FReadOnly: Boolean;
    FOnReadItem: TOnReadItem;
    function GetString(Index: Integer): string;
    function GetCount: Integer;
    procedure SetString(Index: Integer; const Value: string);
    procedure SetItems(Value: TStringList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Strings[Index: Integer]: string read GetString write SetString; default;
  published
    property Items: TStringList read FItems write SetItems;
    property Count: Integer read GetCount;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property OnReadItem: TOnReadItem read FOnReadItem write FOnReadItem;
  end;

implementation

uses
  JvgUtils, JvgTypes;

constructor TJvgStringContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TStringList.Create;
end;

destructor TJvgStringContainer.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJvgStringContainer.GetString(Index: Integer): string;
begin
  Result := FItems[Index];
  if Assigned(FOnReadItem) then
    FOnReadItem(Self, Index);
end;

procedure TJvgStringContainer.SetString(Index: Integer; const Value: string);
begin
  if not FReadOnly then
    FItems[Index] := Value;
end;

procedure TJvgStringContainer.SetItems(Value: TStringList);
begin
  FItems.Assign(Value);
end;

function TJvgStringContainer.GetCount: Integer;
begin
  Result := FItems.Count;
end;

end.
