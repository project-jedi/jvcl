{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgRttiUtils.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgRttiUtils;

{$I jvcl.inc}

interface

{ Procedures for comfort working with objects' properties via RTTI }

function GetValueFromPropertyName(Component: TObject; const PropertyName: string): string;
procedure SetValueByPropertyName(Component: TObject; const PropertyName, PropertyValue: string);
procedure Assign(Source, Target: TObject; Recursive: Boolean);

implementation

uses
  Classes, SysUtils, TypInfo;

function GetValueFromPropertyName(Component: TObject; const PropertyName: string): string;
var
  PropInfo: PPropInfo;
  TypeInf, PropTypeInf: PTypeInfo;
  TypeData: PTypeData;
  I: Integer;
  AName, PropName: string;
  PropList: PPropList;
  NumProps: Word;
  PropObject: TObject;
begin
  { Playing with RTTI }
  TypeInf := Component.ClassInfo;
  AName := TypeInf^.Name;
  TypeData := GetTypeData(TypeInf);
  NumProps := TypeData^.PropCount;

  Result := '';
  GetMem(PropList, NumProps * SizeOf(Pointer));
  try
    //{ ѕолучаем список свойств }
    { Retrieving list of properties [translated] }
    GetPropInfos(TypeInf, PropList);

    for I := 0 to NumProps - 1 do
    begin
      PropName := PropList^[I]^.Name;
      PropTypeInf := PropList^[I]^.PropType^;
      PropInfo := PropList^[I];

      if PropTypeInf^.Kind = tkClass then
      begin
        PropObject := GetObjectProp(Component, PropInfo);
        Result := GetValueFromPropertyName(PropObject, PropertyName);
      end
      else
      if CompareText(PropName, PropertyName) = 0 then
      begin
        Result := GetPropValue(Component, PropName, true);
        Break;
      end;

      if Result <> '' then
        Exit;
    end;
  finally
    FreeMem(PropList);
  end;
end;

procedure SetValueByPropertyName(Component: TObject; const PropertyName, PropertyValue: string);
var
  PropInfo: PPropInfo;
  TypeInf, PropTypeInf: PTypeInfo;
  TypeData: PTypeData;
  I: Integer;
  AName, PropName: string;
  PropList: PPropList;
  NumProps: Word;
  PropObject: TObject;
begin
  { Playing with RTTI }
  TypeInf := Component.ClassInfo;
  AName := TypeInf^.Name;
  TypeData := GetTypeData(TypeInf);
  NumProps := TypeData^.PropCount;

  GetMem(PropList, NumProps * SizeOf(Pointer));
  try
    //{ ѕолучаем список свойств }
    { Retrieving list of properties [translated] }
    GetPropInfos(TypeInf, PropList);

    for I := 0 to NumProps - 1 do
    begin
      PropName := PropList^[I]^.Name;
      PropTypeInf := PropList^[I]^.PropType^;
      PropInfo := PropList^[I];

      if PropTypeInf^.Kind = tkClass then
      begin
        PropObject := GetObjectProp(Component, PropInfo);
        SetValueByPropertyName(PropObject, PropertyName, PropertyValue);
      end
      else
      if CompareText(PropName, PropertyName) = 0 then
      begin
        SetPropValue(Component, PropName, PropertyValue);
        Break;
      end;
    end;
  finally
    FreeMem(PropList);
  end;
end;

procedure Assign(Source, Target: TObject; Recursive: Boolean);
var
  {TypeInf, } PropTypeInf: PTypeInfo;
  I, Index: Integer;
  PropName: string;
  Source_PropList, Target_PropList: PPropList;
  Source_NumProps, Target_NumProps: Word;
  Source_PropObject, Target_PropObject: TObject;

  //{ ѕоиск в списке свойства с заданным именем }
  { Searching for given name in the list of properties [translated] }

  function FindProperty(const PropName: string; PropList: PPropList; NumProps: Word): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to NumProps - 1 do
      if CompareStr(PropList^[I]^.Name, PropName) = 0 then
      begin
        Result := I;
        Break;
      end;
  end;

begin
  if not Assigned(Source) or not Assigned(Target) then
    Exit;

  { Playing with RTTI }
  Source_NumProps := GetTypeData(Source.ClassInfo)^.PropCount;
  Target_NumProps := GetTypeData(Target.ClassInfo)^.PropCount;

  GetMem(Source_PropList, Source_NumProps * SizeOf(Pointer));
  GetMem(Target_PropList, Target_NumProps * SizeOf(Pointer));
  try
    //{ ѕолучаем список свойств }
    { Retrieving list of properties [translated] }
    GetPropInfos(Source.ClassInfo, Source_PropList);
    GetPropInfos(Target.ClassInfo, Target_PropList);

    for I := 0 to Source_NumProps - 1 do
    begin
      PropName := Source_PropList^[I]^.Name;

      Index := FindProperty(PropName, Target_PropList, Target_NumProps);
      if Index = -1 then
        Continue; // не нашли, Not found [translated]

      //{ проверить совпадение типов }
      { check whether the types do match }
      if Source_PropList^[I]^.PropType^.Kind <> Target_PropList^[I]^.PropType^.Kind then
        Continue;

      PropTypeInf := Source_PropList^[I]^.PropType^;
      //      PropInfo := PropList^[I];
      if PropTypeInf^.Kind = tkClass then
      begin
        if Recursive then
        begin
          Source_PropObject := GetObjectProp(Source, Source.ClassInfo);
          Target_PropObject := GetObjectProp(Target, Target.ClassInfo);
          Assign(Source_PropObject, Target_PropObject, Recursive);
        end;
      end
      else
        SetPropValue(Target, PropName, GetPropValue(Source, PropName));
    end;
  finally
    FreeMem(Source_PropList);
    FreeMem(Target_PropList);
  end;
end;

end.

