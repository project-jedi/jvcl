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

{$I jvcl.inc}

unit JvTranslator;

interface

uses
  SysUtils, Classes, Forms, ComCtrls, Menus, IniFiles, Dialogs,
  JvSimpleXml, JvComponent;

type
  TJvTranslator = class(TJvComponent)
  private
    FXml: TJvSimpleXml;
    FSkipClass: TList;

  protected
    procedure TranslateComponent(const Component: TComponent;
      const Elem: TJvSimpleXmlElem); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Call SkipClass to register classes that should not be added to the translation list
    // NOTE: only used when crating the XML string in ComponentToXML, ignored by TranslateXXX methods
    // (for the moment)
    procedure SkipClass(AClass: TClass);
    function InSkipList(AClass: TClass): boolean;overload;
    // ComponentToXML converts a TComponent and, optionally, it's owned components to an XML string
    // and returns it
    function ComponentToXML(const AComponent: TComponent; Recurse: boolean): string;
    // Translate the entire Application using the file Filename
    procedure Translate(const FileName: string); overload;
    // Translate the entire Application using a stream
    procedure Translate(const Stream: TStream); overload;
    // Translate a form using the file Filename
    procedure Translate(const FileName: string; const Form: TCustomForm); overload;
    // Translate a form using the currently loaded XML (wherever it came from)
    procedure Translate(const Form: TCustomForm); overload;
    // Translates all form instances owned by the global screen object using the file Filename
    procedure TranslateScreen(const FileName: string); overload;
    // Translates all form instances owned by the global screen object using a stream
    procedure TranslateScreen(const Stream: TStream); overload;
    // Returns the value of a node or a property value of a node based on certain search criteria.
    // To find the value, the method first searches the root for a subnode with the name in Category.
    // If found, Category is searched for a subnode with the name in Item. If found, either the value
    // of Item or the value of a property named "Value" in Item is returned.
    // Structurally it should look something like this:
    // <Root>
    //   <Category>
    //     <Item Value="PropValue">Value</Item>
    //   </Category>
    //   ....
    // This method returns either Value or, if not found, PropValue or, if not found, an empty string
    function Translate(const Category, Item: string): string; overload;
  end;

  TJvTranslatorStrings = class(TJvComponent)
  private
    FList: THashedStringList;
    function GetString(Index: Integer): string;
    procedure SetString(Index: Integer; const Value: string);
    function GetCount: integer;
    function GetValue(Index: integer): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IndexOf(const Name: string): Integer;
    function Add(const Name: string; var Value: string): Integer;
    // (p3) this is weird: GetString returns the *Name* but SetString sets the *Value*...
    property Strings[Index: Integer]: string read GetString write SetString; default;
    property Value[Index:integer]:string read GetValue;
    property Count: integer read GetCount;
  end;

implementation

uses
  TypInfo, JvConsts;

function InternalGetWideStrProp(Instance: TObject; const PropName: string): WideString; overload;
begin
  {$IFDEF COMPILER6_UP}
  Result := GetWideStrProp(Instance,PropName);
  {$ELSE}
  Result := GetStrProp(Instance, PropName);
  {$ENDIF}
end;

function InternalGetPropList(AObject: TObject; out PropList: PPropList): Integer;
begin
  {$IFDEF COMPILER6_UP}
  Result := GetPropList(AObject,PropList);
  {$ELSE}
  Result := GetTypeData(AObject.ClassInfo)^.PropCount;
  if Result > 0 then
  begin
    GetMem(PropList, Result * SizeOf(Pointer));
    GetPropInfos(AObject.ClassInfo, PropList);
  end;
  Result := GetPropList(AObject.ClassInfo,[tkUnknown..tkDynArray],PropList);
  {$ENDIF}
end;
//=== TJvTranslator ==========================================================

constructor TJvTranslator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXml := TJvSimpleXml.Create(nil);
end;

destructor TJvTranslator.Destroy;
begin
  FXml.Free;
  FSkipClass.Free;
  inherited Destroy;
end;

function TJvTranslator.ComponentToXML(const AComponent: TComponent; Recurse: boolean): string;
var
  AName: string;
  AXML: TJvSimpleXML;
  AElem: TJvSimpleXMLElem;

  function IsObject(const Obj: TClass; const ClassName: string): Boolean;
  begin
    if (Obj = nil) or InSkipList(Obj) then
      Result := False
    else
      Result := SameText(Obj.ClassName, ClassName) or (IsObject(Obj.ClassParent, ClassName));
  end;

  procedure CollectionToXML(Collection: TCollection; Elem: TJvSimpleXmlElem); forward;

  procedure TreeNodesToXML(Nodes: TTreeNodes; Elem: TJvSimpleXmlElem);
  var
    N: TTreeNode;
    AElem: TJvSimpleXmlElem;
  begin
    // format: <Items>
    //           <Item Index="" Value="" />
    // TODO
    N := Nodes.GetFirstNode;
    while Assigned(N) do
    begin
      AElem := Elem.Items.Add('Item');
      AElem.Properties.Add('Index', N.Index);
      AElem.Properties.Add('Value', N.Text);
      {
            AElem.Properties.Add('ImageIndex',N.ImageIndex);
            AElem.Properties.Add('SelectedIndex',N.SelectedIndex);
      }
      N := N.GetNext;
    end;
  end;

  procedure ListItemsToXML(Items: TListItems; Elem: TJvSimpleXmlElem);
  var
    i, j: integer;
    AElem: TJvSimpleXmlElem;
  begin
    // format: <Items>
    //           <Item Index="" Column="" Value="" />
    // TODO
    for i := 0 to Items.Count - 1 do
    begin
      AElem := Elem.Items.Add('Item');
      AElem.Properties.Add('Index', i);
      AElem.Properties.Add('Column', 0);
      AElem.Properties.Add('Value', Items[i].Caption);
      for j := 0 to Items[i].SubItems.Count - 1 do
      begin
        AElem := Elem.Items.Add('Item');
        AElem.Properties.Add('Index', i);
        AElem.Properties.Add('Column', j + 1);
        AElem.Properties.Add('Value', Items[i].SubItems[j]);
      end;
    end;
  end;

  procedure StringsToXML(Strings: TStrings; Elem: TJvSimpleXmlElem);
  var
    i: integer;
    AElem: TJvSimpleXmlElem;
  begin
    // format: <Items>
    //           <Item Index="" Value="" />
    for i := 0 to Strings.Count - 1 do
    begin
      AElem := Elem.Items.Add('Item');
      AElem.Properties.Add('Index', i);
      AElem.Properties.Add('Value', Strings[i]);
    end;
  end;

  procedure TranslatorStringsToXML(AStrings: TJvTranslatorStrings; Elem: TJvSimpleXMLElem);
  var i:integer;AElem:TJvSimpleXMLElem;
  begin
    // I'm not sure how to create a translation template for this component, so this is just a guess...
    Elem.Name := 'Variables';
    for i := 0 to AStrings.Count - 1 do
    begin
      AElem := Elem.Items.Add('Item');
      AElem.Properties.Add('Name',AStrings[i]);
      AElem.Properties.Add('Value',AStrings.Value[i]);
    end;
  end;

  procedure ObjectToXML(AnObject: TObject; Elem: TJvSimpleXmlElem);
  var
    j, Count: integer;
    PropList: PPropList;
    PropName: string;
    PropInfo: PPropInfo;
    AnObj: TObject;
  begin

    if (AnObject <> nil) and not InSkipList(AnObject.ClassType) then
    begin
      Count := InternalGetPropList(AnObject, PropList);
      for j := 0 to Count - 1 do
      begin
        PropInfo := PropList[j];
        PropName := PropInfo^.Name;
        try
          if SameText(PropName, 'Name') then Continue;
          case PropInfo^.PropType^.Kind of
            tkInteger:
              Elem.Properties.Add(PropName, GetOrdProp(AnObject, PropName));
            tkEnumeration:
              Elem.Properties.Add(PropName, GetEnumProp(AnObject, PropName));
            tkSet:
              Elem.Properties.Add(PropName, GetSetProp(AnObject, PropName));
            tkString, tkLString:
              Elem.Properties.Add(PropName, GetStrProp(AnObject, PropName));
            tkClass:
              begin
                AnObj := GetObjectProp(AnObject, PropName);
                if IsObject(AnObj.ClassType, 'TTreeNodes') then
                  TreeNodesToXML(TTreeNodes(AnObj), Elem.Items.Add(PropName))
                else if IsObject(AnObj.ClassType, 'TListItems') then
                  ListItemsToXML(TListItems(AnObj), Elem.Items.Add(PropName))
                else if IsObject(AnObj.ClassType, 'TStrings') then
                  StringsToXML(TStrings(AnObj), Elem.Items.Add(PropName))
                else if IsObject(AnObj.ClassType, 'TCollection') then
                  CollectionToXML(TCollection(AnObj), Elem.Items.Add(PropName))
                else if not IsObject(AnObj.ClassType, 'TComponent') then
                  ObjectToXML(AnObj, Elem.Items.Add(PropName));
              end;
          end; // case
        except
          //
        end;
      end;
    end;
  end;

  procedure CollectionToXML(Collection: TCollection; Elem: TJvSimpleXmlElem);
  var
    i: integer;
  begin
    for i := 0 to Collection.Count - 1 do
      ObjectToXML(Collection.Items[i], Elem.Items.Add(Collection.Items[i].DisplayName));
  end;

  procedure InnerComponentToXML(AComponent: TComponent; Elem: TJvSimpleXmlElem; Recurse: boolean);
  var
    i, Count: integer;
    PropList: PPropList;
    PropName: string;
    PropInfo: PPropInfo;
    AnObj: TObject;
  begin

    if AComponent = nil then Exit;
    if not InSkipList(AComponent.ClassType) then
    begin
      if IsObject(AComponent.ClassType, 'TJvTranslatorStrings') then
      begin
        TranslatorStringsToXML(TJvTranslatorStrings(AComponent), Elem);
        Exit;
      end;
      Count := InternalGetPropList(AComponent, PropList);
      for i := 0 to Count - 1 do
      begin
        PropInfo := PropList[i];
        PropName := PropInfo^.Name;
        try
          if SameText(PropName, 'Name') then Continue;
          case PropInfo^.PropType^.Kind of
            tkInteger:
              Elem.Properties.Add(PropName, GetOrdProp(AComponent, PropName));
            tkEnumeration:
              Elem.Properties.Add(PropName, GetEnumProp(AComponent, PropName));
            tkSet:
              Elem.Properties.Add(PropName, GetSetProp(AComponent, PropName));
            tkString, tkLString, tkWString:
              Elem.Properties.Add(PropName, InternalGetWideStrProp(AComponent, PropName));
            tkClass:
              begin
                AnObj := GetObjectProp(AComponent, PropName);
                if IsObject(AnObj.ClassType, 'TTreeNodes') then
                  TreeNodesToXML(TTreeNodes(AnObj), Elem.Items.Add(PropName))
                else if IsObject(AnObj.ClassType, 'TListItems') then
                  ListItemsToXML(TListItems(AnObj), Elem.Items.Add(PropName))
                else if IsObject(AnObj.ClassType, 'TStrings') then
                  StringsToXML(TStrings(AnObj), Elem.Items.Add(PropName))
                else if IsObject(AnObj.ClassType, 'TCollection') then
                  CollectionToXML(TCollection(AnObj), Elem.Items.Add(PropName))
                else if not IsObject(AnObj.ClassType, 'TComponent') and not InSkipList(AnObj.ClassType) then
                  // NB! TComponents are excluded because most of the time, a published TComponent
                  // property references antother component on the form. In some cases, however, a TComponent
                  // *can* be an internal component and this code won't list it. No known solution yet (no, HasParent/GetparentComponent doesn't work here)
                  ObjectToXML(AnObj, Elem.Items.Add(PropName));
              end;
          end; // case
        except
          //
        end;
      end;
    end;
    if Recurse then
      for i := 0 to AComponent.ComponentCount - 1 do
        if (AComponent.Components[i].Name <> '') then
          InnerComponentToXML(AComponent.Components[i], Elem.Items.Add(AComponent.Components[i].Name), true);
  end;
begin
  Result := '';
  if AComponent = nil then Exit;
  AXML := TJvSimpleXML.Create(nil);
  try
    AName := TComponent(AComponent).Name;
    //    AXML.Root.Name := 'Translation'; // DO NOT LOCALIZE
    if AName <> '' then
    begin
      AElem := AXML.Root.Items.Add(AComponent.Name);
      InnerComponentToXML(AComponent, AElem, Recurse);
      Result := AElem.SaveToString;
    end;
  finally
    AXML.Free;
  end;
end;

procedure TJvTranslator.Translate(const FileName: string);
begin
  try
    FXml.LoadFromFile(FileName);
    TranslateComponent(Application, FXml.Root);
  except
  end;
end;

procedure TJvTranslator.Translate(const Stream: TStream);
begin
  try
    FXml.LoadFromStream(Stream);
    TranslateComponent(Application, FXml.Root);
  except
  end;
end;

procedure TJvTranslator.TranslateScreen(const FileName: string);
var
  i: Integer;
begin
  try
    FXml.LoadFromFile(FileName);
    for i := 0 to Screen.FormCount - 1 do
      Translate(Screen.Forms[i]);
  except
  end;
end;

procedure TJvTranslator.TranslateScreen(const Stream: TStream);
var
  i: Integer;
begin
  try
    FXml.LoadFromStream(Stream);
    for i := 0 to Screen.FormCount - 1 do
      Translate(Screen.Forms[i]);
  except
  end;
end;

procedure TJvTranslator.Translate(const FileName: string; const Form: TCustomForm);
begin
  try
    FXml.LoadFromFile(FileName);
    Translate(Form);
  except
  end;
end;

procedure TJvTranslator.TranslateComponent(const Component: TComponent;
  const Elem: TJvSimpleXmlElem);
var
  I, J: Integer;
  Prop: PPropInfo;
  Obj: TObject;
  Ok: Boolean;
  S: string;

  procedure TransObject(const Obj: TObject; const Elem: TJvSimpleXmlElem); forward;

  function AnalyseCRLF(Value: string): string;
  begin
    Result := StringReplace(Value, '\n', sLineBreak, [rfReplaceAll]);
  end;

  function IsObject(const Obj: TClass; ClassName: string): Boolean;
  begin
    if Obj = nil then
      Result := False
    else
      Result := (Obj.ClassName = ClassName) or (IsObject(Obj.ClassParent, ClassName));
  end;

  procedure TransStrings(const Obj: TObject; const Elem: TJvSimpleXmlElem);
  var
    I, J: Integer;
  begin
    if (Elem.Items.Count > 0) and (Elem.Items[0] is TJvSimpleXmlElemCData) then
      TStrings(obj).Text := Elem.Items[0].Value
    else
      for I := 0 to Elem.Items.Count - 1 do
      begin
        J := Elem.Items[I].Properties.IntValue('Index', MaxInt);
        if J < TStrings(Obj).Count then
          TStrings(Obj).Strings[J] := Elem.Items[I].Properties.Value('Value');
      end;
  end;

  procedure TransTreeNodes(const Obj: TObject; const Elem: TJvSimpleXmlElem);
  var
    I, J: Integer;
  begin
    for I := 0 to Elem.Items.Count - 1 do
    begin
      J := Elem.Items[I].Properties.IntValue('Index', MaxInt);
      if J < TTreeNodes(Obj).Count then
        TTreeNodes(Obj).Item[J].Text := Elem.Items[I].Properties.Value('Value');
    end;
  end;

  procedure TransVars;
  var
    I, J: Integer;
  begin
    with TJvTranslatorStrings(Component) do
      for I := 0 to Elem.Items.Count - 1 do
      begin
        J := TJvTranslatorStrings(Component).IndexOf(Elem.Items[I].Properties.Value('Name'));
        if J <> -1 then
          TJvTranslatorStrings(Component).Strings[J] := AnalyseCRLF(Elem.Items[I].Properties.Value('Value'));
      end;
  end;

  procedure TransListItems(const Obj: TObject; const Elem: TJvSimpleXmlElem);
  var
    I, J: Integer;
  begin
    for I := 0 to Elem.Items.Count - 1 do
    begin
      J := Elem.Items[I].Properties.IntValue('Index', MaxInt);
      if J < TListItems(Obj).Count then
        with TListItems(Obj).Item[J] do
        begin
          J := Elem.Items[I].Properties.IntValue('Column', MaxInt);
          if J = 0 then
            Caption := Elem.Items[I].Properties.Value('Value')
          else
          begin
            Dec(J);
            if J < SubItems.Count then
              SubItems[J] := Elem.Items[I].Properties.Value('Value');
          end;
        end;
    end;
  end;

  procedure TransProperties(const Obj: TObject; const Elem: TJvSimpleXmlElem);
  var
    I, J: Integer;
    Prop: PPropInfo;
    S: string;
  begin
    if Obj = nil then
      Exit;
    for I := 0 to Elem.Properties.Count - 1 do
    try
      Prop := GetPropInfo(Obj, Elem.Properties[I].Name, [tkInteger,
        tkEnumeration, tkSet, tkString, tkLString, tkWString]);
      if Prop <> nil then
        case Prop^.PropType^.Kind of
          tkstring, tkLString, tkWString:
            SetStrProp(Obj, Prop, StringReplace(Elem.Properties[I].Value, '\n', sLineBreak, []));
          tkSet:
            SetSetProp(Obj, Prop, Elem.Properties[I].Value);
          tkEnumeration:
            begin
              S := Elem.Properties[I].Value;
              if (StrToIntDef(S, 0) = 0) and (S <> '0') then
              begin
                try
                  J := GetEnumValue(Prop.PropType^, S);
                except
                  J := 0;
                end;
              end
              else
                J := StrToIntDef(S, 0);
              SetOrdProp(Obj, Prop, J);
            end;
          tkInteger:
            if Prop^.Name = 'ShortCut' then
              SetOrdProp(Obj, Prop, TextToShortcut(Elem.Properties[I].Value))
            else
              SetOrdProp(Obj, Prop, Elem.Properties[I].IntValue);
        end;
    except
    end;
  end;

  procedure TranslateCollection(const Collection: TCollection; const Elem: TJvSimpleXmlElem);
  var
    I, J: Integer;
  begin
    for I := 0 to Elem.Items.Count - 1 do
    begin
      J := Elem.Items[I].Properties.IntValue('Index', -1);
      if J = -1 then
        Continue;
      if (j < Collection.Count) then
      begin
        TransProperties(Collection.Items[J], Elem.Items[I]);
        TransObject(Collection.Items[J], Elem.Items[I]);
      end;
    end;
  end;

  procedure TransObject(const Obj: TObject; const Elem: TJvSimpleXmlElem);
  var
    I, J: Integer;
    Prop: PPropInfo;
    S: string;
    lObj: TObject;
  begin
    if Obj = nil then
      Exit;
    if IsObject(Obj.ClassType, 'TCollection') then
      TranslateCollection(TCollection(Obj), Elem)
    else
      for I := 0 to Elem.Items.Count - 1 do
      try
        Prop := GetPropInfo(Obj, Elem.Items[I].Name, [tkInteger,
          tkEnumeration, tkSet, tkString, tkLString, tkClass]);
        if Prop <> nil then
          case Prop^.PropType^.Kind of
            tkString, tkLString:
              SetStrProp(Obj, Prop, StringReplace(Elem.Items[I].Value, '\n', sLineBreak, []));
            tkSet:
              SetSetProp(Obj, Prop, Elem.Items[I].Value);
            tkEnumeration:
              begin
                S := Elem.Items[I].Value;
                if (StrToIntDef(S, 0) = 0) and (S <> '0') then
                begin
                  try
                    J := GetEnumValue(Prop.PropType^, S);
                  except
                    J := 0;
                  end;
                end
                else
                  J := StrToIntDef(S, 0);
                SetOrdProp(Obj, Prop, J);
              end;
            tkInteger:
              SetOrdProp(Obj, Prop, Elem.Items[I].IntValue);
            tkClass:
              begin
                lObj := GetObjectProp(Obj, Elem.Items[I].Name);
                TransProperties(lObj, Elem.Items[I]);
                TransObject(lObj, Elem.Items[I]);
              end;
          end;
      except
      end;
  end;

begin
  if IsObject(Component.ClassType, 'TJvTranslatorStrings') then
  begin
    TransVars;
    Exit;
  end;

  try
    //Transform properties
    TransProperties(Component, Elem);

    //Transform childs
    with Component do
      for I := 0 to Elem.Items.Count - 1 do
      begin
        Ok := False;
        for J := 0 to ComponentCount - 1 do
        begin
          S := LowerCase(Elem.Items[I].Name);
          if LowerCase(Components[J].Name) = S then
          begin
            TranslateComponent(Components[J], Elem.Items[I]);
            Ok := True;
            Break;
          end;
        end;
        if not Ok then
        begin
          Prop := GetPropInfo(Component, Elem.Items[I].Name, [tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
            tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
              tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray]);
          if Prop <> nil then
          begin
            Obj := GetObjectProp(Component, Elem.Items[I].Name);
            if IsObject(Obj.ClassType, 'TStrings') then
              TransStrings(Obj, Elem.Items[I])
            else if IsObject(Obj.ClassType, 'TTreeNodes') then
              TransTreeNodes(Obj, Elem.Items[I])
            else if IsObject(Obj.ClassType, 'TListItems') then
              TransListItems(Obj, Elem.Items[I])
            else
            begin
              TransProperties(Obj, Elem.Items[I]);
              TransObject(Obj, Elem.Items[I]);
            end;
          end;
        end;
      end;
  except
  end;
end;

procedure TJvTranslator.Translate(const Form: TCustomForm);
var
  J: Integer;
  S: string;
  lElem: TJvSimpleXmlElem;
begin
  J := Pos('_', Form.Name);
  if J = 0 then
    S := Form.Name
  else
    S := Copy(Form.Name, 1, J - 1);
  lElem := FXml.Root.Items.ItemNamed[S];
  if lElem <> nil then
    TranslateComponent(Form, lElem);
end;

function TJvTranslator.Translate(const Category, Item: string): string;
var
  lElem: TJvSimpleXmlElem;
begin
  Result := '';
  lElem := FXml.Root.Items.ItemNamed[Category];
  if lElem <> nil then
  begin
    lElem := lElem.Items.ItemNamed[Item];
    if lElem <> nil then
    begin
      Result := lElem.Value;
      if Result = '' then
        Result := lElem.Properties.Value('Value');
    end;
  end;
end;

procedure TJvTranslator.SkipClass(AClass: TClass);
begin
  if FSkipClass = nil then
    FSkipClass := TList.Create;
  if FSkipClass.IndexOf(Pointer(AClass)) < 0 then
    FSkipClass.Add(Pointer(AClass));
end;

function TJvTranslator.InSkipList(AClass: TClass): boolean;
begin
  Result := (FSkipClass <> nil) and (FSkipClass.IndexOf(Pointer(AClass)) > -1);
end;

//=== TJvTranslatorStrings ===================================================

constructor TJvTranslatorStrings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := THashedStringList.Create;
end;

destructor TJvTranslatorStrings.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TJvTranslatorStrings.Add(const Name: string; var Value: string): Integer;
begin
  // (rom) AddObject? Strange.
  Result := FList.AddObject(Name, TObject(@Value));
end;

function TJvTranslatorStrings.GetString(Index: Integer): string;
begin
  Result := FList[Index];
end;

function TJvTranslatorStrings.IndexOf(const Name: string): Integer;
begin
  Result := FList.IndexOf(Name);
end;

procedure TJvTranslatorStrings.SetString(Index: Integer; const Value: string);
begin
  PString(FList.Objects[Index])^ := Value;
end;

function TJvTranslatorStrings.GetCount: integer;
begin
  Result := FList.Count;
end;

function TJvTranslatorStrings.GetValue(Index: integer): string;
begin
  if (Index >= 0) and (Index < Count) and (FList.Objects[Index] <> nil) then
    Result := PString(FList.Objects[Index])^
  else
    Result := '';
end;


end.

