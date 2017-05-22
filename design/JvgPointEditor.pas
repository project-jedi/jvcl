{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgPointEditor.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgPointEditor;

{$I jvcl.inc}

interface

uses
  Windows, SysUtils, Classes,
  DesignIntf, DesignEditors, PropertyCategories,
  TypInfo;

type
  TJvgPointProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

procedure Register;

implementation

{ (rom) disabled seem obsolete
var
  PPointTypeInfo: PTypeInfo;
  PointTypeInfo: TTypeInfo;
}

function TJvgPointProperty.GetAttributes: TPropertyAttributes;
begin
  Result := []; // paSubProperties, paReadOnly ];
end;

function TJvgPointProperty.GetValue: string;
var
  pPT: PPoint;
begin
  //  pPT := PPoint(GetOrdValue);
  //  Result := Format('(%d,%d)', [ ppt^.x, ppt^.y ]);
  Result := '[,]';
end;

procedure Register;
begin
  PointTypeInfo.Name := 'TPoint';
  PointTypeInfo.Kind := tkFloat;
  PPointTypeInfo := @PointTypeInfo;
  RegisterPropertyEditor(TypeInfo(TPoint), nil, '', TJvgPointProperty);
end;

end.