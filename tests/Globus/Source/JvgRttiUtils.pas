{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgRttiUtils.PAS, released on 2003-01-15.

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

{ Процедуры для удобной работы со свойствами объектов через RTTI }

unit JvgRttiUtils;

interface

function GetValueFromPropertyName(Component: TObject; PropertyName: string): string;
procedure SetValueByPropertyName(Component: TObject; const PropertyName, PropertyValue: string);
procedure Assign(Source, Target: TObject; fRecurcive: boolean);

implementation
uses Classes, SysUtils, TypInfo;

function GetValueFromPropertyName(Component: TObject; PropertyName: string): string;
var
  PropInfo: PPropInfo;
  TypeInf, PropTypeInf: PTypeInfo;
  TypeData: PTypeData;
  i, j: integer;
  AName, PropName, sPropValue: string;
  PropList: PPropList;
  NumProps: word;
  PropObject: TObject;
begin
  { Playing with RTTI }
  TypeInf := Component.ClassInfo;
  AName := TypeInf^.Name;
  TypeData := GetTypeData(TypeInf);
  NumProps := TypeData^.PropCount;

  Result := '';
  GetMem(PropList, NumProps * sizeof(pointer));
  try
    { Получаем список свойств }
    GetPropInfos(TypeInf, PropList);

    for i := 0 to NumProps - 1 do
    begin
      PropName := PropList^[i]^.Name;
      PropTypeInf := PropList^[i]^.PropType^;
      PropInfo := PropList^[i];

      if PropTypeInf^.Kind = tkClass then
      begin
        PropObject := GetObjectProp(Component, PropInfo);
        Result := GetValueFromPropertyName(PropObject, PropertyName);
      end
      else if CompareText(PropName, PropertyName) = 0 then
      begin
        Result := GetPropValue(Component, PropName, true);
        break;
      end;

      if Result <> '' then exit;

    end;
  finally
    FreeMem(PropList, NumProps * sizeof(pointer));
  end;
end;

procedure SetValueByPropertyName(Component: TObject; const PropertyName, PropertyValue: string);
var
  PropInfo: PPropInfo;
  TypeInf, PropTypeInf: PTypeInfo;
  TypeData: PTypeData;
  i, j: integer;
  AName, PropName, sPropValue: string;
  PropList: PPropList;
  NumProps: word;
  PropObject: TObject;
begin
  { Playing with RTTI }
  TypeInf := Component.ClassInfo;
  AName := TypeInf^.Name;
  TypeData := GetTypeData(TypeInf);
  NumProps := TypeData^.PropCount;

  GetMem(PropList, NumProps * sizeof(pointer));
  try
    { Получаем список свойств }
    GetPropInfos(TypeInf, PropList);

    for i := 0 to NumProps - 1 do
    begin
      PropName := PropList^[i]^.Name;
      PropTypeInf := PropList^[i]^.PropType^;
      PropInfo := PropList^[i];

      if PropTypeInf^.Kind = tkClass then
      begin
        PropObject := GetObjectProp(Component, PropInfo);
        SetValueByPropertyName(PropObject, PropertyName, PropertyValue);
      end
      else if CompareText(PropName, PropertyName) = 0 then
      begin
        SetPropValue(Component, PropName, PropertyValue);
        exit;
      end;

    end;
  finally
    FreeMem(PropList, NumProps * sizeof(pointer));
  end;
end;

procedure Assign(Source, Target: TObject; fRecurcive: boolean);
var
  PropInfo: PPropInfo;
  {TypeInf, } PropTypeInf: PTypeInfo;
  TypeData: PTypeData;
  i, j, Index: integer;
  PropName, sPropValue: string;
  Source_PropList, Target_PropList: PPropList;
  Source_NumProps, Target_NumProps: word;
  Source_PropObject, Target_PropObject: TObject;

  { Поиск в списке свойства с заданным именем }

  function FindProperty(const PropName: string; PropList: PPropList; NumProps: word): integer;
  var
    i: integer;
  begin
    Result := -1;
    for i := 0 to NumProps - 1 do
      if CompareStr(PropList^[i]^.Name, PropName) = 0 then
      begin
        Result := i;
        break;
      end;
  end;

begin
  if not Assigned(Source) or not Assigned(Target) then exit;

  { Playing with RTTI }
  Source_NumProps := GetTypeData(Source.ClassInfo)^.PropCount;
  Target_NumProps := GetTypeData(Target.ClassInfo)^.PropCount;

  GetMem(Source_PropList, Source_NumProps * sizeof(pointer));
  GetMem(Target_PropList, Target_NumProps * sizeof(pointer));
  try
    { Получаем список свойств }
    GetPropInfos(Source.ClassInfo, Source_PropList);
    GetPropInfos(Target.ClassInfo, Target_PropList);

    for i := 0 to Source_NumProps - 1 do
    begin
      PropName := Source_PropList^[i]^.Name;

      Index := FindProperty(PropName, Target_PropList, Target_NumProps);
      if Index = -1 then continue; // не нашли

      { проверить совпадение типов }
      if Source_PropList^[i]^.PropType^.Kind <> Target_PropList^[i]^.PropType^.Kind then continue;

      PropTypeInf := Source_PropList^[i]^.PropType^;
      //      PropInfo := PropList^[i];
      if (PropTypeInf^.Kind = tkClass) then
      begin
        if fRecurcive then
        begin
          Source_PropObject := GetObjectProp(Source, Source.ClassInfo);
          Target_PropObject := GetObjectProp(Target, Target.ClassInfo);
          Assign(Source_PropObject, Target_PropObject, fRecurcive);
        end;
      end
      else
        SetPropValue(Target, PropName, GetPropValue(Source, PropName));

    end;
  finally
    FreeMem(Source_PropList, Source_NumProps * sizeof(pointer));
    FreeMem(Target_PropList, Target_NumProps * sizeof(pointer));
  end;
end;

end.
