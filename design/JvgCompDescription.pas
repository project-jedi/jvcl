{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgCompDescription.PAS, released on 2003-01-15.

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

unit JvgCompDescription;

{$I jvcl.inc}

interface

uses
  Classes, TypInfo,
  {$IFDEF USEJVCL}
  JvComponent,
  {$ENDIF USEJVCL}
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, PropertyCategories;
  {$ELSE}
  DsgnIntf;
  {$ENDIF COMPILER6_UP}

type
  TJvgPropInfos = class;
  TJvgPropInform = class;

  {$IFDEF USEJVCL}
  TJvgComponentDescription = class(TJvComponent)
  {$ELSE}
  TJvgComponentDescription = class(TComponent)
  {$ENDIF USEJVCL}
  private
    FPropInfos: TJvgPropInfos;
    FNote: string;
    FClass_Name: string;
    FPropList: PPropList;
    FNumProps: Word;
  public
    constructor Create(AOwner, Component: TComponent); reintroduce;
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
    FChecked: Boolean;
    FMakeHref: Boolean;
    FNote: string;
  public
    Info: PPropInfo;
  published
    property Name: string read FName write FName;
    property TypeName: string read FTypeName write FTypeName;
    property TypeKind: TTypeKind read FTypeKind write FTypeKind;
    property Checked: Boolean read FChecked write FChecked;
    property MakeHref: Boolean read FMakeHref write FMakeHref;
    property Note: string read FNote write FNote;
  end;

implementation

//=== { TJvgComponentDescription } ===========================================

constructor TJvgComponentDescription.Create(AOwner, Component: TComponent);
begin
  inherited Create(AOwner);
  PropInfos := TJvgPropInfos.Create(TJvgPropInform);
  LoadProperties(Component);
end;

destructor TJvgComponentDescription.Destroy;
begin
  FreeMem(FPropList, FNumProps * SizeOf(Pointer));
  PropInfos.Free;
  inherited Destroy;
end;

procedure TJvgComponentDescription.LoadProperties(Component: TComponent);
var
  TypeInf: PTypeInfo;
  TypeData: PTypeData;
  I: Integer;
  AName, PropName: string;
begin
  if FNumProps > 0 then
    FreeMem(FPropList, FNumProps * SizeOf(Pointer));

  { Playing with RTTI }
  TypeInf := Component.ClassInfo;
  AName := TypeInf^.Name;
  TypeData := GetTypeData(TypeInf);
  FNumProps := TypeData^.PropCount;

  GetMem(FPropList, FNumProps * SizeOf(Pointer));
  try
    //{ Получаем список свойств }
    { Retrieving list of properties [translated] }
    GetPropInfos(TypeInf, FPropList);

    for I := 0 to FNumProps - 1 do
    begin
      PropInfos.AddPropInfo(FPropList^[I], Component);

      PropName := FPropList^[I]^.Name;
      //      PropTypeInf := FPropList^[I]^.PropType^;
      //      PropInfo := FPropList^[I];
    end;
  finally
  end;
end;

//=== { TJvgPropInfos } ======================================================

procedure TJvgPropInfos.AddPropInfo(PropInfo: PPropInfo; Component: TComponent);
var
  TypeData: PTypeData;
  TypeInfo: PTypeInfo;
  J: Integer;
  NoteStr: string;
begin
  with TJvgPropInform(Add) do
  begin
    Name := PropInfo^.Name;
    TypeName := PropInfo^.PropType^.Name;
    TypeKind := PropInfo^.PropType^.Kind;
    Info := PropInfo;
    NoteStr := '';

    if TypeKind in [tkEnumeration, tkSet] then
    begin
      if TypeKind = tkSet then
        TypeInfo := GetTypeData(PropInfo^.PropType^)^.CompType^
      else
        TypeInfo := PropInfo.PropType^;

      TypeData := GetTypeData(TypeInfo);

      for J := TypeData^.MinValue to TypeData^.MaxValue do
      begin
        if NoteStr <> '' then
          if TypeKind = tkSet then
            NoteStr := NoteStr + ' | '
          else
            NoteStr := NoteStr + ', ';
        NoteStr := NoteStr + GetEnumName(TypeInfo, J);
      end;
      NoteStr := '[' + NoteStr + ']';
    end;
    Note := NoteStr;

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

