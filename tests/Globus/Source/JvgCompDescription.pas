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

unit JvgCompDescription;

interface
uses classes,
  {$IFDEF COMPILER6_UP}
  DesignIntf,
  DesignEditors,
  PropertyCategories,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JVComponent,
  TypInfo;

type
  TJvgPropInfos = class;
  TJvgPropInform = class;

  TJvgComponentDescription = class(TJvComponent)
  private
    FPropInfos: TJvgPropInfos;
    FNote: string;
    FClass_Name: string;

    PropList: PPropList;
    NumProps: word;
  public
    constructor Create(AOwner, Component: TComponent);
    destructor Destroy; override;
    procedure LoadProperties(Component: TComponent);
  published
    property Class_Name: string read FClass_Name write FClass_Name;
    property Note: string read FNote write FNote;
    property PropInfos: TJvgPropInfos read FPropInfos write FPropInfos;
  end;

  TJvgPropInfos = class(TCollection)
  private
    procedure AddPropInfo(PropInfo: PPropInfo; Component: TComponent);
    function GetItem(Index: Integer): TJvgPropInform;
    procedure SetItem(Index: Integer; const Value: TJvgPropInform);
  public
    function Add: TJvgPropInform;
    function Insert(Index: Integer): TJvgPropInform;
    property Items[Index: Integer]: TJvgPropInform read GetItem write SetItem;
    default;
  end;

  TJvgPropInform = class(TCollectionItem)
  private
    FName: string;
    FTypeName: string;
    FTypeKind: TTypeKind;
    FChecked: boolean;
    FMakeHref: boolean;
    FNote: string;
  public
    Info: PPropInfo;
  published
    property Name: string read FName write FName;
    property TypeName: string read FTypeName write FTypeName;
    property TypeKind: TTypeKind read FTypeKind write FTypeKind;
    property Checked: boolean read FChecked write FChecked;
    property MakeHref: boolean read FMakeHref write FMakeHref;
    property Note: string read FNote write FNote;
  end;

implementation

constructor TJvgComponentDescription.Create(AOwner, Component: TComponent);
begin
  inherited Create(AOwner);
  PropInfos := TJvgPropInfos.Create(TJvgPropInform);
  LoadProperties(Component);
end;

destructor TJvgComponentDescription.Destroy;
begin
  FreeMem(PropList, NumProps * sizeof(pointer));
  PropInfos.Free;
  inherited;
end;

procedure TJvgComponentDescription.LoadProperties(Component: TComponent);
var
  PropInfo: PPropInfo;
  TypeInf, PropTypeInf: PTypeInfo;
  TypeData: PTypeData;
  i, j: integer;
  AName, PropName, sPropValue: string;
  PropObject: TObject;
begin
  if NumProps > 0 then
    FreeMem(PropList, NumProps * sizeof(pointer));

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
      PropInfos.AddPropInfo(PropList^[i], Component);

      PropName := PropList^[i]^.Name;

      PropTypeInf := PropList^[i]^.PropType^;
      PropInfo := PropList^[i];
    end;
  finally

  end;
end;

procedure TJvgPropInfos.AddPropInfo(PropInfo: PPropInfo; Component: TComponent);
var
  TypeData: PTypeData;
  TypeInfo: PTypeInfo;
  j: integer;
  sNote: string;
begin
  with TJvgPropInform(Add) do
  begin
    Name := PropInfo^.Name;
    TypeName := PropInfo^.PropType^.Name;
    TypeKind := PropInfo^.PropType^.Kind;
    Info := PropInfo;
    sNote := '';

    if TypeKind in [tkEnumeration, tkSet] then
    begin
      if TypeKind = tkSet then
        TypeInfo := GetTypeData(PropInfo^.PropType^)^.CompType^
      else
        TypeInfo := PropInfo.PropType^;

      TypeData := GetTypeData(TypeInfo);

      for j := TypeData^.MinValue to TypeData^.MaxValue do
      begin
        if sNote <> '' then
          if TypeKind = tkSet then
            sNote := sNote + ' | '
          else
            sNote := sNote + ', ';
        sNote := sNote + GetEnumName(TypeInfo, j);
      end;
      sNote := '[' + sNote + ']';
    end;
    Note := sNote;

    Checked := not IsPublishedProp(Component.ClassParent, Name);
  end;
end;

function TJvgPropInfos.Add: TJvgPropInform;
begin
  Result := TJvgPropInform(inherited Add);
end;

function TJvgPropInfos.Insert(Index: Integer): TJvgPropInform;
begin
  Result := TJvgPropInform(inherited Insert(Index));
end;

function TJvgPropInfos.GetItem(Index: Integer): TJvgPropInform;
begin
  Result := TJvgPropInform(inherited Items[Index]);
end;

procedure TJvgPropInfos.SetItem(Index: Integer; const Value: TJvgPropInform);
begin
  Items[Index].Assign(Value);
end;

end.
