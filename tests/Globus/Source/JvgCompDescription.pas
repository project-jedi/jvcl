{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgCompDescription.PAS, released on 2003-01-15.

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

UNIT JvgCompDescription;

INTERFACE
USES classes,
   {$IFDEF COMPILER6_UP}
   DesignIntf,
   DesignEditors,
   PropertyCategories,
   {$ELSE}
   DsgnIntf,
   {$ENDIF COMPILER6_UP}

   TypInfo;

TYPE
   TJvgPropInfos = CLASS;
   TJvgPropInform = CLASS;

   TJvgComponentDescription = CLASS(TComponent)
   PRIVATE
      FPropInfos: TJvgPropInfos;
      FNote: STRING;
      FClass_Name: STRING;

      PropList: PPropList;
      NumProps: word;
   PUBLIC
      CONSTRUCTOR Create(AOwner, Component: TComponent);
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE LoadProperties(Component: TComponent);
   PUBLISHED
      PROPERTY Class_Name: STRING READ FClass_Name WRITE FClass_Name;
      PROPERTY Note: STRING READ FNote WRITE FNote;
      PROPERTY PropInfos: TJvgPropInfos READ FPropInfos WRITE FPropInfos;
   END;

   TJvgPropInfos = CLASS(TCollection)
   PRIVATE
      PROCEDURE AddPropInfo(PropInfo: PPropInfo; Component: TComponent);
      FUNCTION GetItem(Index: Integer): TJvgPropInform;
      PROCEDURE SetItem(Index: Integer; CONST Value: TJvgPropInform);
   PUBLIC
      FUNCTION Add: TJvgPropInform;
      FUNCTION Insert(Index: Integer): TJvgPropInform;
      PROPERTY Items[Index: Integer]: TJvgPropInform READ GetItem WRITE SetItem;
         DEFAULT;
   END;

   TJvgPropInform = CLASS(TCollectionItem)
   PRIVATE
      FName: STRING;
      FTypeName: STRING;
      FTypeKind: TTypeKind;
      FChecked: boolean;
      FMakeHref: boolean;
      FNote: STRING;
   PUBLIC
      Info: PPropInfo;
   PUBLISHED
      PROPERTY Name: STRING READ FName WRITE FName;
      PROPERTY TypeName: STRING READ FTypeName WRITE FTypeName;
      PROPERTY TypeKind: TTypeKind READ FTypeKind WRITE FTypeKind;
      PROPERTY Checked: boolean READ FChecked WRITE FChecked;
      PROPERTY MakeHref: boolean READ FMakeHref WRITE FMakeHref;
      PROPERTY Note: STRING READ FNote WRITE FNote;
   END;

IMPLEMENTATION

CONSTRUCTOR TJvgComponentDescription.Create(AOwner, Component: TComponent);
BEGIN
   INHERITED Create(AOwner);
   PropInfos := TJvgPropInfos.Create(TJvgPropInform);
   LoadProperties(Component);
END;

DESTRUCTOR TJvgComponentDescription.Destroy;
BEGIN
   FreeMem(PropList, NumProps * sizeof(pointer));
   PropInfos.Free;
   INHERITED;
END;

PROCEDURE TJvgComponentDescription.LoadProperties(Component: TComponent);
VAR
   PropInfo                   : PPropInfo;
   TypeInf, PropTypeInf       : PTypeInfo;
   TypeData                   : PTypeData;
   i, j                       : integer;
   AName, PropName, sPropValue: STRING;
   PropObject                 : TObject;
BEGIN
   IF NumProps > 0 THEN
      FreeMem(PropList, NumProps * sizeof(pointer));

   { Playing with RTTI }
   TypeInf := Component.ClassInfo;
   AName := TypeInf^.Name;
   TypeData := GetTypeData(TypeInf);
   NumProps := TypeData^.PropCount;

   GetMem(PropList, NumProps * sizeof(pointer));
   TRY
      { Получаем список свойств }
      GetPropInfos(TypeInf, PropList);

      FOR i := 0 TO NumProps - 1 DO
      BEGIN
         PropInfos.AddPropInfo(PropList^[i], Component);

         PropName := PropList^[i]^.Name;

         PropTypeInf := PropList^[i]^.PropType^;
         PropInfo := PropList^[i];
      END;
   FINALLY

   END;
END;

PROCEDURE TJvgPropInfos.AddPropInfo(PropInfo: PPropInfo; Component: TComponent);
VAR
   TypeData                   : PTypeData;
   TypeInfo                   : PTypeInfo;
   j                          : integer;
   sNote                      : STRING;
BEGIN
   WITH TJvgPropInform(Add) DO
   BEGIN
      Name := PropInfo^.Name;
      TypeName := PropInfo^.PropType^.Name;
      TypeKind := PropInfo^.PropType^.Kind;
      Info := PropInfo;
      sNote := '';

      IF TypeKind IN [tkEnumeration, tkSet] THEN
      BEGIN
         IF TypeKind = tkSet THEN
            TypeInfo := GetTypeData(PropInfo^.PropType^)^.CompType^
         ELSE
            TypeInfo := PropInfo.PropType^;

         TypeData := GetTypeData(TypeInfo);

         FOR j := TypeData^.MinValue TO TypeData^.MaxValue DO
         BEGIN
            IF sNote <> '' THEN
               IF TypeKind = tkSet THEN
                  sNote := sNote + ' | '
               ELSE
                  sNote := sNote + ', ';
            sNote := sNote + GetEnumName(TypeInfo, j);
         END;
         sNote := '[' + sNote + ']';
      END;
      Note := sNote;

      Checked := NOT IsPublishedProp(Component.ClassParent, Name);
   END;
END;

FUNCTION TJvgPropInfos.Add: TJvgPropInform;
BEGIN
   Result := TJvgPropInform(INHERITED Add);
END;

FUNCTION TJvgPropInfos.Insert(Index: Integer): TJvgPropInform;
BEGIN
   Result := TJvgPropInform(INHERITED Insert(Index));
END;

FUNCTION TJvgPropInfos.GetItem(Index: Integer): TJvgPropInform;
BEGIN
   Result := TJvgPropInform(INHERITED Items[Index]);
END;

PROCEDURE TJvgPropInfos.SetItem(Index: Integer; CONST Value: TJvgPropInform);
BEGIN
   Items[Index].Assign(Value);
END;

END.

