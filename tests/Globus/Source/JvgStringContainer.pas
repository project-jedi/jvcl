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

UNIT JvgStringContainer;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   jvComponent,
   Classes;

TYPE
   TOnReadItem = PROCEDURE(Sender: TObject; Index: integer) OF OBJECT;

   TJvgStringContainer = CLASS(TJvComponent)
   PRIVATE
      FItems: TStringList;
      FReadOnly: boolean;
      FOnReadItem: TOnReadItem;
      FUNCTION GetString(Index: integer): STRING;
      PROCEDURE SetString(Index: integer; CONST Value: STRING);
      PROCEDURE SetItems(Value: TStringList);
      FUNCTION GetCount: integer;
   PUBLIC
      PROPERTY Strings[Index: Integer]: STRING READ GetString WRITE SetString;
         DEFAULT;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
   PUBLISHED
      PROPERTY Items: TStringList READ FItems WRITE SetItems;
      PROPERTY Count: integer READ GetCount;
      PROPERTY ReadOnly: boolean READ FReadOnly WRITE FReadOnly DEFAULT false;
      PROPERTY OnReadItem: TOnReadItem READ FOnReadItem WRITE FOnReadItem;
   END;

PROCEDURE Register;

IMPLEMENTATION
USES JvgUtils,
   JvgTypes;

PROCEDURE Register;
BEGIN
END;

CONSTRUCTOR TJvgStringContainer.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   FItems := TStringList.Create;
END;

DESTRUCTOR TJvgStringContainer.Destroy;
BEGIN
   FItems.Free;
   INHERITED;
END;

FUNCTION TJvgStringContainer.GetString(Index: integer): STRING;
BEGIN
   IF Assigned(FOnReadItem) THEN
      FOnReadItem(self, Index);
   Result := FItems[Index];
END;

PROCEDURE TJvgStringContainer.SetString(Index: integer; CONST Value: STRING);
BEGIN
   IF NOT FReadOnly THEN
      FItems[Index] := Value;
END;

PROCEDURE TJvgStringContainer.SetItems(Value: TStringList);
BEGIN
   FItems.Assign(Value);
END;

FUNCTION TJvgStringContainer.GetCount: integer;
BEGIN
   Result := FItems.Count;
END;

END.

