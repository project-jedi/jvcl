{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTranslator.PAS, released on 2002-06-03

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): _________________________________.

Last Modified: 2002-06-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvTranslator;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, TypInfo, ComCtrls, JvSimpleXml,
  Menus, JvComponent, IniFiles, Dialogs;

type
  TJvTranslator = class(TJvComponent)
  private
    FXml: TJvSimpleXml;
  protected
    procedure TranslateComponent(const Component: TComponent;
      const Elem: TJvSimpleXmlElem);virtual;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;

    procedure Translate(const FileName: string);overload;
    procedure Translate(const FileName: string;const Form: TForm);overload;
    procedure Translate(const Form: TForm);overload;
    function Translate(const Category, Item: string): string;overload;
  end;

  TJvTranslatorStrings = class(TJvComponent)
  private
    FList: THashedStringList;
    function GetString(const Index: Integer): string;
    procedure SetString(const Index: Integer; const Value: string);
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;

    function IndexOf(const Name: string):Integer;
    function Add(const Name: string;var Value: string):Integer;
    property Strings[const Index: Integer]:string read GetString write SetString;
  end;

implementation

{*******************************************************************}
procedure TJvTranslator.Translate(const FileName: string);
begin
  try
    FXml.LoadFromFile(FileName);
    TranslateComponent(Application,FXml.Root);
  except
  end;
end;
{*******************************************************************}
constructor TJvTranslator.Create(AOwner: TComponent);
begin
  inherited;
  FXml := TJvSimpleXml.Create(nil);
end;
{*******************************************************************}
destructor TJvTranslator.Destroy;
begin
  FXml.Free;
  inherited;
end;
{*******************************************************************}
procedure TJvTranslator.Translate(const FileName: string;
  const Form: TForm);
begin
  try
    FXml.LoadFromFile(FileName);
    Translate(Form);
  except
  end;
end;
{*******************************************************************}
procedure TJvTranslator.TranslateComponent(const Component: TComponent;
  const Elem: TJvSimpleXmlElem);
var
 i,j: Integer;
 prop: PPropInfo;
 obj: TObject;
 ok: Boolean;
 st: string;

  procedure TransObject(const Obj: TObject;const Elem: TJvSimpleXmlElem);forward;

  function AnalyseCRLF(Value: string):string;
  begin
    result := StringReplace(Value,'\n',#13#10,[rfReplaceAll]);
  end;

  function IsObject(const Obj: TClass; ClassName: string):Boolean;
  begin
    if Obj=nil then
      result := false
    else
      result := (Obj.ClassName=ClassName) or (IsObject(Obj.ClassParent,ClassName));
  end;

  procedure TransStrings(const Obj: TObject;const Elem: TJvSimpleXmlElem);
  var
   i,j: Integer;
  begin
    for i:=0 to Elem.Items.Count-1 do
    begin
      j := Elem.Items[i].Properties.IntValue('Index',MAXINT);
      if j<TStrings(obj).Count then
        TStrings(obj).Strings[j] := Elem.Items[i].Properties.Value('Value');
    end;
  end;

  procedure TransTreeNodes(const Obj: TObject;const Elem: TJvSimpleXmlElem);
  var
   i,j: Integer;
  begin
    for i:=0 to Elem.Items.Count-1 do
    begin
      j := Elem.Items[i].Properties.IntValue('Index',MAXINT);
      if j<TTreeNodes(obj).Count then
        TTreeNodes(obj).Item[j].Text := Elem.Items[i].Properties.Value('Value');
    end;
  end;

  procedure TransVars;
  var
   i,j: Integer;
  begin
    with TJvTranslatorStrings(Component) do
      for i:=0 to Elem.Items.Count-1 do
      begin
        j := TJvTranslatorStrings(Component).IndexOf(Elem.Items[i].Properties.Value('Name'));
        if j<>-1 then
          TJvTranslatorStrings(Component).Strings[j] := AnalyseCRLF(Elem.Items[i].Properties.Value('Value'));
      end;
  end;

  procedure TransListItems(const Obj: TObject;const Elem: TJvSimpleXmlElem);
  var
   i,j: Integer;
  begin
    for i:=0 to Elem.Items.Count-1 do
    begin
      j := Elem.Items[i].Properties.IntValue('Index',MAXINT);
      if j<TListItems(obj).Count then
        with TListItems(obj).Item[j] do
        begin

          j := Elem.Items[i].Properties.IntValue('Column',MAXINT);
          if j=0 then
            Caption := Elem.Items[i].Properties.Value('Value')
          else
          begin
            dec(j);
            if j<SubItems.Count then
              SubItems[j] := Elem.Items[i].Properties.Value('Value');
          end;
        end;
    end;
  end;

  procedure TransProperties(const Obj: TObject;const Elem: TJvSimpleXmlElem);
  var
   i,j: Integer;
   prop: PPropInfo;
   st: string;
  begin
    if Obj=nil then
      Exit;
    for i:=0 to Elem.Properties.Count-1 do
    try
      prop := GetPropInfo(Obj,Elem.Properties[i].Name,[tkInteger,
        tkEnumeration, tkSet, tkString, tkLString, tkWString]);
      if prop<>nil then
       case Prop^.PropType^.Kind of
         tkstring, tkLString, tkWString:
           SetStrProp(Obj, Prop, StringReplace(Elem.Properties[i].Value,'\n',#13#10,[]));
         tkSet:
           SetSetProp(Obj, Prop, Elem.Properties[i].Value);
         tkEnumeration:
           begin
             st := Elem.Properties[i].Value;
             if (StrToIntDef(st,0)=0) and (st<>'0') then
             begin
               try
                 j := GetEnumValue(Prop.PropType^,st);
               except
                 j := 0;
               end;
             end
             else
               j := StrToIntDef(st,0);
             SetOrdProp(Obj, Prop, j);
           end;
         tkInteger:
           if prop^.Name='ShortCut' then
             SetOrdProp(Obj, Prop, TextToShortcut(Elem.Properties[i].Value))
           else
             SetOrdProp(Obj, Prop, Elem.Properties[i].IntValue);
       end;
    except
    end;
  end;

  procedure TranslateCollection(const Collection: TCollection;const Elem: TJvSimpleXmlElem);
  var
   i,j: Integer;
  begin
    for i:=0 to Elem.Items.Count-1 do
    begin
      j := Elem.Items[i].Properties.IntValue('Index',-1);
      if j=-1 then
        Continue;
      TransProperties(Collection.Items[j],Elem.Items[i]);
      TransObject(Collection.Items[j],Elem.Items[i]);
    end;
  end;

  procedure TransObject(const Obj: TObject;const Elem: TJvSimpleXmlElem);
  var
   i,j: Integer;
   prop: PPropInfo;
   st: string;
   lObj: TObject;
  begin
    if Obj=nil then
      Exit;
    if IsObject(Obj.ClassType,'TCollection') then
      TranslateCollection(TCollection(Obj),Elem)
    else
      for i:=0 to Elem.Items.Count-1 do
      try
        prop := GetPropInfo(Obj,Elem.Items[i].Name,[tkInteger,
          tkEnumeration, tkSet, tkString, tkLString, tkClass]);
        if prop<>nil then
         case Prop^.PropType^.Kind of
           tkString, tkLString:
             SetStrProp(Obj, Prop, StringReplace(Elem.Items[i].Value,'\n',#13#10,[]));
           tkSet:
             SetSetProp(Obj, Prop, Elem.Items[i].Value);
           tkEnumeration:
             begin
               st := Elem.Items[i].Value;
               if (StrToIntDef(st,0)=0) and (st<>'0') then
               begin
                 try
                   j := GetEnumValue(Prop.PropType^,st);
                 except
                   j := 0;
                 end;
               end
               else
                 j := StrToIntDef(st,0);
               SetOrdProp(Obj, Prop, j);
             end;
           tkInteger:
             SetOrdProp(Obj, Prop, Elem.Items[i].IntValue);
           tkClass:
             begin
               lObj := GetObjectProp(Obj,Elem.Items[i].Name);
               TransProperties(lObj,Elem.Items[i]);
               TransObject(lObj,Elem.Items[i]);
             end;
         end;
      except
      end;
  end;

begin
  if IsObject(Component.ClassType,'TJvTranslatorStrings') then
  begin
    TransVars;
    Exit;
  end;

  try
    //Transform properties
    TransProperties(Component,Elem);

    //Transform childs
    with Component do
      for i:=0 to Elem.Items.Count-1 do
      begin
        ok := false;
        for j:=0 to ComponentCount-1 do
        begin
          st := LowerCase(Elem.Items[i].Name);
          if LowerCase(Components[j].Name) = st then
          begin
            TranslateComponent(Components[j],Elem.Items[i]);
            ok := true;
            break;
          end;
        end;
        if not ok then
        begin
          prop := GetPropInfo(Component,Elem.Items[i].Name,[tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
            tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
            tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray]);
          if prop<>nil then
          begin
            obj := GetObjectProp(Component,Elem.Items[i].Name);
            if IsObject(obj.ClassType,'TStrings') then
              TransStrings(obj,Elem.Items[i])
            else if IsObject(obj.ClassType,'TTreeNodes') then
              TransTreeNodes(obj,Elem.Items[i])
            else if IsObject(obj.ClassType,'TListItems') then
              TransListItems(obj,Elem.Items[i])
            else
            begin
              TransProperties(obj,Elem.Items[i]);
              TransObject(obj,Elem.Items[i]);
            end;
          end;
        end;
      end;
  except
  end;
end;
{*******************************************************************}
procedure TJvTranslator.Translate(const Form: TForm);
var
 j: Integer;
 st: string;
 lElem: TJvSimpleXmlElem;
begin
  j := pos('_',Form.Name);
  if j=0 then
    st := Form.Name
  else
    st := Copy(Form.Name,1,j-1);
  lElem := FXml.Root.Items.ItemNamed[st];
  if lElem<>nil then
    TranslateComponent(Form,lElem);
end;
{*******************************************************************}
function TJvTranslator.Translate(const Category, Item: string): string;
var
 lElem: TJvSimpleXmlElem;
begin
  result := '';
  lElem := FXml.Root.Items.ItemNamed[Category];
  if lElem<>nil then
  begin
    lElem := lElem.Items.ItemNamed[Item];
    if lElem<>nil then
    begin
      result := lElem.Value;
      if result='' then
        result := lElem.Properties.Value('Value');
    end;
  end;
end;
{*******************************************************************}

{ TJvTranslatorStrings }

{*******************************************************************}
function TJvTranslatorStrings.Add(const Name: string;var Value: string): Integer;
begin
  result := FList.AddObject(Name,@Value);
end;
{*******************************************************************}
constructor TJvTranslatorStrings.Create(AOwner: TComponent);
begin
  inherited;
  FList := THashedStringList.Create;
end;
{*******************************************************************}
destructor TJvTranslatorStrings.Destroy;
begin
  FList.Free;
  inherited;
end;
{*******************************************************************}
function TJvTranslatorStrings.GetString(const Index: Integer): string;
begin
  result := FList[Index];
end;
{*******************************************************************}
function TJvTranslatorStrings.IndexOf(const Name: string): Integer;
begin
  result := FList.IndexOf(Name);
end;
{*******************************************************************}
procedure TJvTranslatorStrings.SetString(const Index: Integer; const Value: string);
begin
  PString(FList.Objects[Index])^ := Value;
end;
{*******************************************************************}
end.
