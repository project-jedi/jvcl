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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvTranslator;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, TypInfo, ComCtrls, JvSimpleXml,
  JvComponent, IniFiles, Dialogs;

{.$DEFINE GX_OUTLOOK}

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

{$IFDEF GX_OUTLOOK}
 uses
 GX_Outlook; //Haven't found better :-/
{$ENDIF}

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

 function AnalyseCRLF(Value: string):string;
 begin
   result := StringReplace(Value,'\n',#13#10,[rfReplaceAll]);
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

 procedure TransColumns(const Obj: TObject;const Elem: TJvSimpleXmlElem);
 var
  i,j: Integer;
 begin
   for i:=0 to Elem.Items.Count-1 do
   begin
     j := Elem.Items[i].Properties.IntValue('Index',MAXINT);
     if j<TListColumns(obj).Count then
       TListColumns(obj).Items[j].Caption := Elem.Items[i].Properties.Value('Value');
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

 function IsObject(const Obj: TClass; ClassName: string):Boolean;
 begin
   if Obj=nil then
     result := false
   else
     result := (Obj.ClassName=ClassName) or (IsObject(Obj.ClassParent,ClassName));
 end;

 {$IFDEF GX_OUTLOOK}
 procedure TransOutlook;
 var
  i,j: Integer;
 begin
   with TFEGXOutlookBar(Component) do
     for i:=0 to Elem.Items.Count-1 do
     begin
       j := Elem.Items[i].Properties.IntValue('Index',MAXINT);
       if j<TFEGXOutlookBar(Component).Count then
         TFEGXOutlookBar(Component).Items[j].Caption := Elem.Items[i].Properties.Value('Value');
     end;
 end;
 {$ENDIF}

begin
  if IsObject(Component.ClassType,'TJvTranslatorStrings') then
  begin
    TransVars;
    Exit;
  end;
  {$IFDEF GX_OUTLOOK}
  if IsObject(Component.ClassType,'TFEGXOutlookBar') then
  begin
    TransOutlook;
    Exit;
  end;
  {$ENDIF}

  try
    //Transform properties
    for i:=0 to Elem.Properties.Count-1 do
    try
      prop := GetPropInfo(Component,Elem.Properties[i].Name,[tkInteger,
        tkEnumeration, tkString, tkLString]);
      if prop<>nil then
       case Prop^.PropType^.Kind of
         tkstring, tkLString:
           SetStrProp(Component, Prop, StringReplace(Elem.Properties[i].Value,'\n',#13#10,[]));
         tkEnumeration, tkInteger:
           SetOrdProp(Component, Prop, Elem.Properties[i].IntValue);
       end;
    except
    end;

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
            else if IsObject(obj.ClassType,'TListColumns') then
              TransColumns(obj,Elem.Items[i])
            else if IsObject(obj.ClassType,'TListItems') then
              TransListItems(obj,Elem.Items[i]);
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
