{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTranslator.PAS, released on 2002-06-03

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): _________________________________.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQTranslator;

interface

uses
  SysUtils, Classes, IniFiles,
  
  
  QForms, QComCtrls, QMenus, QDialogs,
  
  JvQSimpleXml, JvQComponent;

type
  TJvTranslator = class(TJvComponent)
  private
    FXML: TJvSimpleXml;
    FSkipList: TList;
    function IsObject(const Obj: TClass; const ClassName: string): Boolean;
  protected
    function FindItemNamed(Root: TJvSimpleXMLElem; const AName: string;
      ARecurse: Boolean = False): TJvSimpleXMLElem; virtual;
    procedure TranslateComponent(const Component: TComponent; const Elem: TJvSimpleXmlElem); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Call SkipClass to register a class to skip when reading/writing
    procedure SkipClass(AClass: TClass);
    // Call UnskipClass to unregister a class so it won't be skip when reading/writing
    procedure UnskipClass(AClass: TClass);
    // Call SkipProperty to register a class property to skip when reading/writing
    // If UnskipClass has already been called for this class, does nothing
    procedure SkipProperty(AClass: TClass; const PropName: string);
    // Call UnskipProperty to unregister a class property so it won't be skipped when reading/writing
    // If SkipClass has already been called for this class, does nothing
    procedure UnskipProperty(AClass: TClass; const PropName: string);
    // Returns True if the specifed class/object/property is in the skip list
    function InSkipList(AClass: TClass): Boolean; overload;
    function InSkipList(Obj: TObject): Boolean; overload;
    function InSkipList(AClass: TClass; const PropName: string): Boolean; overload;
    function InSkipList(Obj: TObject; const PropName: string): Boolean; overload;
    procedure ClearSkipList;
    // ComponentToXML converts a TComponent and, optionally, it's owned components to an XML string
    // and returns it
    function ComponentToXML(const AComponent: TComponent; Recurse: Boolean): string;
    // Translate the entire Application using the file Filename
    procedure Translate(const FileName: string); overload;
    // Translate the entire Application using a stream
    procedure Translate(const Stream: TStream); overload;
    // Translate the entire Application using a string
    procedure TranslateString(const S: string); overload;
    // Translate a form using the file Filename
    procedure Translate(const FileName: string; const Form: TCustomForm); overload;
    // Translate a form using a stream
    procedure Translate(const Stream: TStream; const Form: TCustomForm); overload;
    // Translate a form using a string
    procedure TranslateString(const S: string; const Form: TCustomForm); overload;
    // Translate a form using the currently loaded XML (wherever it came from)
    procedure Translate(const Form: TCustomForm); overload;
    // Translates all form instances owned by the global screen object using the file Filename
    procedure TranslateScreen(const FileName: string); overload;
    // Translates all form instances owned by the global screen object using a stream
    procedure TranslateScreen(const Stream: TStream); overload;
    // Translates all form instances owned by the global screen object using a string
    procedure TranslateScreenString(const S: string);
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
    property XML: TJvSimpleXml read FXML;
  end;

  TJvTranslatorStrings = class(TJvComponent)
  private
    FList: THashedStringList;
    function GetString(Index: Integer): string;
    procedure SetString(Index: Integer; const Value: string);
    function GetCount: Integer;
    function GetValue(Index: Integer): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IndexOf(const Name: string): Integer;
    function Add(const Name: string; var Value: string): Integer;
    // (p3) this is weird: GetString returns the *Name* but SetString sets the *Value*...
    property Strings[Index: Integer]: string read GetString write SetString; default;
    property Value[Index: Integer]: string read GetValue;
    property Count: Integer read GetCount;
  end;

implementation

uses
  TypInfo,
  JvQConsts;

const
  cName = 'Name';
  cItem = 'Item';
  cIndex = 'Index';
  cColumn = 'Column';
  cValue = 'Value';
  cVariables = 'Variables';
  cTTreeNodes = 'TTreeNodes';
  cTListItems = 'TListItems';
  cTStrings = 'TStrings';
  cTCollection = 'TCollection';
  cTComponent = 'TComponent';
  cTJvTranslatorStrings = 'TJvTranslatorStrings';
  cNewline = '\n';

type
  PSkipPropRec = ^TSkipPropRec;
  TSkipPropRec = record
    AClass: TClass;
    AProps: TStringList;
  end;

function InternalGetWideStrProp(Instance: TObject; const PropName: string): WideString; overload;
begin
  
  Result := GetWideStrProp(Instance, PropName);
  
end;

function InternalGetPropList(AObject: TObject; out PropList: PPropList): Integer;
begin
  
  Result := GetPropList(AObject, PropList);
  
end;

//=== TJvTranslator ==========================================================

constructor TJvTranslator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXML := TJvSimpleXml.Create(nil);
  SkipProperty(TComponent, cName);
end;

destructor TJvTranslator.Destroy;
begin
  FXML.Free;
  ClearSkipList;
  FreeAndNil(FSkipList);
  inherited Destroy;
end;

function TJvTranslator.FindItemNamed(Root: TJvSimpleXMLElem; const AName: string; ARecurse: Boolean): TJvSimpleXMLElem;
var
  I: Integer;
begin
  Result := nil;
  if Root = nil then
    Root := FXML.Root;
  if AnsiSameText(Root.Name, AName) then
    Result := Root
  else
  if not ARecurse then
    Result := Root.Items.ItemNamed[AName]
  else
    for I := 0 to Root.Items.Count - 1 do
    begin
      Result := FindItemNamed(Root.Items[I], AName, True);
      if Result <> nil then
        Break;
    end;
end;

function TJvTranslator.IsObject(const Obj: TClass; const ClassName: string): Boolean;
begin
  if Obj = nil then
    Result := False
  else
    Result := SameText(Obj.ClassName, ClassName) or (IsObject(Obj.ClassParent, ClassName));
end;

function TJvTranslator.ComponentToXML(const AComponent: TComponent; Recurse: Boolean): string;
var
  AName: string;
  AElem: TJvSimpleXMLElem;

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
      AElem := Elem.Items.Add(cItem);
      AElem.Properties.Add(cIndex, N.Index);
      AElem.Properties.Add(cValue, N.Text);
      {
            AElem.Properties.Add('ImageIndex',N.ImageIndex);
            AElem.Properties.Add('SelectedIndex',N.SelectedIndex);
      }
      N := N.GetNext;
    end;
  end;

  procedure ListItemsToXML(Items: TListItems; Elem: TJvSimpleXmlElem);
  var
    I, J: Integer;
    AElem: TJvSimpleXmlElem;
  begin
    // format: <Items>
    //           <Item Index="" Column="" Value="" />
    // TODO
    for I := 0 to Items.Count - 1 do
    begin
      AElem := Elem.Items.Add(cItem);
      AElem.Properties.Add(cIndex, I);
      AElem.Properties.Add(cColumn, 0);
      AElem.Properties.Add(cValue, Items[I].Caption);
      for J := 0 to Items[I].SubItems.Count - 1 do
      begin
        AElem := Elem.Items.Add(cItem);
        AElem.Properties.Add(cIndex, I);
        AElem.Properties.Add(cColumn, J + 1);
        AElem.Properties.Add(cValue, Items[I].SubItems[J]);
      end;
    end;
  end;

  procedure StringsToXML(Strings: TStrings; Elem: TJvSimpleXmlElem);
  var
    I: Integer;
    AElem: TJvSimpleXmlElem;
  begin
    // format: <Items>
    //           <Item Index="" Value="" />
    for I := 0 to Strings.Count - 1 do
    begin
      AElem := Elem.Items.Add(cItem);
      AElem.Properties.Add(cIndex, I);
      AElem.Properties.Add(cValue, Strings[I]);
    end;
  end;

  procedure TranslatorStringsToXML(AStrings: TJvTranslatorStrings; Elem: TJvSimpleXMLElem);
  var
    I: Integer;
    AElem: TJvSimpleXMLElem;
  begin
    // I'm not sure how to create a translation template for this component, so this is just a guess...
    Elem.Name := cVariables;
    for I := 0 to AStrings.Count - 1 do
    begin
      AElem := Elem.Items.Add(cItem);
      AElem.Properties.Add(cName, AStrings[I]);
      AElem.Properties.Add(cValue, AStrings.Value[I]);
    end;
  end;

  procedure ObjectToXML(AnObject: TObject; Elem: TJvSimpleXmlElem);
  var
    J, Count: Integer;
    PropList: PPropList;
    PropName: string;
    PropInfo: PPropInfo;
    AnObj: TObject;
  begin

    if (AnObject <> nil) and not InSkipList(AnObject) then
    begin
      Count := InternalGetPropList(AnObject, PropList);
      for J := 0 to Count - 1 do
      begin
        PropInfo := PropList[J];
        PropName := PropInfo^.Name;
        try
          if (PropInfo^.SetProc = nil) or InSkipList(AnObject, PropName) then
            Continue;
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
                if IsObject(AnObj.ClassType, cTTreeNodes) then
                  TreeNodesToXML(TTreeNodes(AnObj), Elem.Items.Add(PropName))
                else
                if IsObject(AnObj.ClassType, cTListItems) then
                  ListItemsToXML(TListItems(AnObj), Elem.Items.Add(PropName))
                else
                if IsObject(AnObj.ClassType, cTStrings) then
                  StringsToXML(TStrings(AnObj), Elem.Items.Add(PropName))
                else
                if IsObject(AnObj.ClassType, cTCollection) then
                  CollectionToXML(TCollection(AnObj), Elem.Items.Add(PropName))
                else
                if not IsObject(AnObj.ClassType, cTComponent) then
                  // NB! TComponents are excluded because most of the time, a published TComponent
                  // property references another component on the form. In some cases, however, a TComponent
                  // *can* be an internal component and this code won't list it.
                  // No known solution yet (no, HasParent/GetParentComponent doesn't work here)
                  ObjectToXML(AnObj, Elem.Items.Add(PropName));
              end;
          end;
        except
          //
        end;
      end;
    end;
  end;

  procedure CollectionToXML(Collection: TCollection; Elem: TJvSimpleXmlElem);
  var
    I: Integer;
  begin
    for I := 0 to Collection.Count - 1 do
      ObjectToXML(Collection.Items[I], Elem.Items.Add(Collection.Items[I].DisplayName));
  end;

  procedure InnerComponentToXML(AComponent: TComponent; Elem: TJvSimpleXmlElem; Recurse: Boolean);
  var
    I, Count: Integer;
    PropList: PPropList;
    PropName: string;
    PropInfo: PPropInfo;
    AnObj: TObject;
  begin

    if AComponent = nil then
      Exit;
    if not InSkipList(AComponent) then
    begin
      if IsObject(AComponent.ClassType, cTJvTranslatorStrings) then
      begin
        TranslatorStringsToXML(TJvTranslatorStrings(AComponent), Elem);
        Exit;
      end;
      Count := InternalGetPropList(AComponent, PropList);
      for I := 0 to Count - 1 do
      begin
        PropInfo := PropList[I];
        PropName := PropInfo^.Name;
        try
          if InSkipList(AComponent, PropName) or (PropInfo^.SetProc = nil) then
            Continue;
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
                if IsObject(AnObj.ClassType, cTTreeNodes) then
                  TreeNodesToXML(TTreeNodes(AnObj), Elem.Items.Add(PropName))
                else
                if IsObject(AnObj.ClassType, cTListItems) then
                  ListItemsToXML(TListItems(AnObj), Elem.Items.Add(PropName))
                else
                if IsObject(AnObj.ClassType, cTStrings) then
                  StringsToXML(TStrings(AnObj), Elem.Items.Add(PropName))
                else
                if IsObject(AnObj.ClassType, cTCollection) then
                  CollectionToXML(TCollection(AnObj), Elem.Items.Add(PropName))
                else
                if not IsObject(AnObj.ClassType, cTComponent) then
                  // NB! TComponents are excluded because most of the time, a published TComponent
                  // property references another component on the form. In some cases, however, a TComponent
                  // *can* be an internal component and this code won't list it.
                  // No known solution yet (no, HasParent/GetparentComponent doesn't work here)
                  ObjectToXML(AnObj, Elem.Items.Add(PropName));
              end;
          end;
        except
          //
        end;
      end;
    end;
    if Recurse then
      for I := 0 to AComponent.ComponentCount - 1 do
        if AComponent.Components[I].Name <> '' then
          InnerComponentToXML(AComponent.Components[I], Elem.Items.Add(AComponent.Components[I].Name), True);
  end;

begin
  Result := '';
  FXML.Root.Clear;
  if AComponent = nil then
    Exit;
  if AComponent is TApplication then
  begin
    AName := TApplication(AComponent).Title;
    FXML.Root.Name := 'Translation'; // DO NOT LOCALIZE
    AElem := FXML.Root.Items.Add(AName);
  end
  else
  begin
    AName := TComponent(AComponent).Name;
    AElem := FXML.Root;
    FXML.Root.Name := AName;
  end;
  if AName <> '' then
  begin
    InnerComponentToXML(AComponent, AElem, Recurse);
    Result := FXML.Root.SaveToString;
  end;
end;

procedure TJvTranslator.Translate(const FileName: string);
begin
  try
    FXML.LoadFromFile(FileName);
    TranslateComponent(Application, FXML.Root);
  except
  end;
end;

procedure TJvTranslator.Translate(const Stream: TStream);
begin
  try
    FXML.LoadFromStream(Stream);
    TranslateComponent(Application, FXML.Root);
  except
  end;
end;

procedure TJvTranslator.TranslateScreen(const FileName: string);
var
  I: Integer;
begin
  try
    FXML.LoadFromFile(FileName);
    for I := 0 to Screen.FormCount - 1 do
      Translate(Screen.Forms[I]);
  except
  end;
end;

procedure TJvTranslator.TranslateScreen(const Stream: TStream);
var
  I: Integer;
begin
  try
    FXML.LoadFromStream(Stream);
    for I := 0 to Screen.FormCount - 1 do
      Translate(Screen.Forms[I]);
  except
  end;
end;

procedure TJvTranslator.Translate(const FileName: string; const Form: TCustomForm);
begin
  try
    FXML.LoadFromFile(FileName);
    Translate(Form);
  except
  end;
end;

procedure TJvTranslator.TranslateComponent(const Component: TComponent;
  const Elem: TJvSimpleXmlElem);
var
  I, J: Integer;
  PropInfo: PPropInfo;
  Obj: TObject;
  Ok: Boolean;
  S: string;

  procedure TransObject(const Obj: TObject; const Elem: TJvSimpleXmlElem); forward;

  function AnalyseCRLF(Value: string): string;
  begin
    Result := StringReplace(Value, cNewline, sLineBreak, [rfReplaceAll]);
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
        J := Elem.Items[I].Properties.IntValue(cIndex, MaxInt);
        if J < TStrings(Obj).Count then
          TStrings(Obj).Strings[J] := Elem.Items[I].Properties.Value(cValue);
      end;
  end;

  procedure TransTreeNodes(const Obj: TObject; const Elem: TJvSimpleXmlElem);
  var
    I, J: Integer;
  begin
    for I := 0 to Elem.Items.Count - 1 do
    begin
      J := Elem.Items[I].Properties.IntValue(cIndex, MaxInt);
      if J < TTreeNodes(Obj).Count then
        TTreeNodes(Obj).Item[J].Text := Elem.Items[I].Properties.Value(cValue);
    end;
  end;

  procedure TransVars;
  var
    I, J: Integer;
  begin
    with TJvTranslatorStrings(Component) do
      for I := 0 to Elem.Items.Count - 1 do
      begin
        J := TJvTranslatorStrings(Component).IndexOf(Elem.Items[I].Properties.Value(cName));
        if J <> -1 then
          TJvTranslatorStrings(Component).Strings[J] := AnalyseCRLF(Elem.Items[I].Properties.Value(cValue));
      end;
  end;

  procedure TransListItems(const Obj: TObject; const Elem: TJvSimpleXmlElem);
  var
    I, J: Integer;
  begin
    for I := 0 to Elem.Items.Count - 1 do
    begin
      J := Elem.Items[I].Properties.IntValue(cIndex, MaxInt);
      if J < TListItems(Obj).Count then
        with TListItems(Obj).Item[J] do
        begin
          J := Elem.Items[I].Properties.IntValue(cColumn, MaxInt);
          if J = 0 then
            Caption := Elem.Items[I].Properties.Value(cValue)
          else
          begin
            Dec(J);
            if J < SubItems.Count then
              SubItems[J] := Elem.Items[I].Properties.Value(cValue);
          end;
        end;
    end;
  end;

  procedure TransProperties(const Obj: TObject; const Elem: TJvSimpleXmlElem);
  var
    I, J: Integer;
    PropInfo: PPropInfo;
    S: string;
  begin
    if Obj = nil then
      Exit;
    for I := 0 to Elem.Properties.Count - 1 do
    try
      PropInfo := GetPropInfo(Obj, Elem.Properties[I].Name, [tkInteger,
        tkEnumeration, tkSet, tkString, tkLString, tkWString]);
      if (PropInfo <> nil) and (PropInfo^.SetProc <> nil) and not InSkipList(Obj, Elem.Properties[I].Name) then
        case PropInfo^.PropType^.Kind of
          tkstring, tkLString, tkWString:
            SetStrProp(Obj, PropInfo, StringReplace(Elem.Properties[I].Value, cNewline, sLineBreak, []));
          tkSet:
            SetSetProp(Obj, PropInfo, Elem.Properties[I].Value);
          tkEnumeration:
            begin
              S := Elem.Properties[I].Value;
              if (StrToIntDef(S, 0) = 0) and (S <> '0') then
              begin
                try
                  J := GetEnumValue(PropInfo.PropType^, S);
                except
                  J := 0;
                end;
              end
              else
                J := StrToIntDef(S, 0);
              SetOrdProp(Obj, PropInfo, J);
            end;
          tkInteger:
            if PropInfo^.Name = 'ShortCut' then
              SetOrdProp(Obj, PropInfo, TextToShortcut(Elem.Properties[I].Value))
            else
              SetOrdProp(Obj, PropInfo, Elem.Properties[I].IntValue);
        end;
    except
    end;
  end;

  procedure TranslateCollection(const Collection: TCollection; const Elem: TJvSimpleXmlElem);
  var
    I, J: Integer;
  begin
    if Obj = nil then
      Exit;
    for I := 0 to Elem.Items.Count - 1 do
    begin
      J := Elem.Items[I].Properties.IntValue(cIndex, -1);
      if J = -1 then
        Continue;
      if J < Collection.Count then
      begin
        TransProperties(Collection.Items[J], Elem.Items[I]);
        TransObject(Collection.Items[J], Elem.Items[I]);
      end;
    end;
  end;

  procedure TransObject(const Obj: TObject; const Elem: TJvSimpleXmlElem);
  var
    I, J: Integer;
    PropInfo: PPropInfo;
    S: string;
    lObj: TObject;
  begin
    if Obj = nil then
      Exit;
    if IsObject(Obj.ClassType, cTCollection) then
      TranslateCollection(TCollection(Obj), Elem)
    else
      for I := 0 to Elem.Items.Count - 1 do
      try
        PropInfo := GetPropInfo(Obj, Elem.Items[I].Name, [tkInteger,
          tkEnumeration, tkSet, tkString, tkLString, tkClass]);
        if (PropInfo <> nil) and (PropInfo^.SetProc <> nil) and not InSkipList(Obj, Elem.Items[I].Name) then
          case PropInfo^.PropType^.Kind of
            tkString, tkLString:
              SetStrProp(Obj, PropInfo, StringReplace(Elem.Items[I].Value, cNewline, sLineBreak, []));
            tkSet:
              SetSetProp(Obj, PropInfo, Elem.Items[I].Value);
            tkEnumeration:
              begin
                S := Elem.Items[I].Value;
                if (StrToIntDef(S, 0) = 0) and (S <> '0') then
                begin
                  try
                    J := GetEnumValue(PropInfo.PropType^, S);
                  except
                    J := 0;
                  end;
                end
                else
                  J := StrToIntDef(S, 0);
                SetOrdProp(Obj, PropInfo, J);
              end;
            tkInteger:
              SetOrdProp(Obj, PropInfo, Elem.Items[I].IntValue);
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
  if IsObject(Component.ClassType, cTJvTranslatorStrings) then
  begin
    TransVars;
    Exit;
  end;

  try
    //Transform properties
    if not InSkipList(Component) then
      TransProperties(Component, Elem);

    //Transform childs
    with Component do
      for I := 0 to Elem.Items.Count - 1 do
      begin
        Ok := False;
        for J := 0 to ComponentCount - 1 do
        begin
          S := LowerCase(Elem.Items[I].Name);
          if AnsiSameText(Components[J].Name, S) then
          begin
            TranslateComponent(Components[J], Elem.Items[I]);
            Ok := True;
            Break;
          end;
        end;
        if not Ok then
        begin
          PropInfo := GetPropInfo(Component, Elem.Items[I].Name, [tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
            tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
              tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray]);
          if (PropInfo <> nil) and (PropInfo^.SetProc <> nil) and not InSkipList(Component, Elem.Items[I].Name) then
          begin
            Obj := GetObjectProp(Component, Elem.Items[I].Name);
            if IsObject(Obj.ClassType, cTStrings) then
              TransStrings(Obj, Elem.Items[I])
            else
            if IsObject(Obj.ClassType, cTTreeNodes) then
              TransTreeNodes(Obj, Elem.Items[I])
            else
            if IsObject(Obj.ClassType, cTListItems) then
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
  lElem := FindItemNamed(nil, S, True);
  if lElem <> nil then
    TranslateComponent(Form, lElem)
end;

function TJvTranslator.Translate(const Category, Item: string): string;
var
  lElem: TJvSimpleXmlElem;
begin
  Result := '';
  lElem := FindItemNamed(nil, Category, True);
  if lElem <> nil then
  begin
    lElem := FindItemNamed(lElem, Item, True);
    if lElem <> nil then
    begin
      Result := lElem.Value;
      if Result = '' then
        Result := lElem.Properties.Value(cValue);
    end;
  end;
end;

procedure TJvTranslator.SkipClass(AClass: TClass);
begin
  SkipProperty(AClass, '');
end;

procedure TJvTranslator.UnskipClass(AClass: TClass);
begin
  UnskipProperty(AClass, '');
end;

function TJvTranslator.InSkipList(AClass: TClass): Boolean;
begin
  Result := InSkipList(AClass, '');
end;

function TJvTranslator.InSkipList(Obj: TObject): Boolean;
begin
  if Obj = nil then
    Result := InSkipList(nil)
  else
    Result := InSkipList(Obj.ClassType);
end;

function TJvTranslator.InSkipList(Obj: TObject; const PropName: string): Boolean;
begin
  if Obj = nil then
    Result := InSkipList(nil, PropName)
  else
    Result := InSkipList(Obj.ClassType, PropName);
end;

function TJvTranslator.InSkipList(AClass: TClass; const PropName: string): Boolean;
var
  I: Integer;
  P: PSkipPropRec;
begin
  Result := False;
  if FSkipList <> nil then
    for I := 0 to FSkipList.Count - 1 do
    begin
      P := PSkipPropRec(FSkipList[I]);
      if (P^.AClass = AClass) or AClass.InheritsFrom(P^.AClass) then
      begin
        if ((PropName = '') and (P^.AProps.Count = 0)) or (P^.AProps.IndexOf(PropName) > -1) then
        begin
          Result := True;
          if PropName = '' then
            // move item to beginning of list since it is very likely that we want to access this class very soon
            FSkipList.Move(I, 0);
          Break;
        end;
      end;
    end;
end;

procedure TJvTranslator.Translate(const Stream: TStream; const Form: TCustomForm);
begin
  FXML.LoadFromStream(Stream);
  Translate(Form);
end;

procedure TJvTranslator.TranslateString(const S: string);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(S);
  try
    Translate(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvTranslator.TranslateString(const S: string; const Form: TCustomForm);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(S);
  try
    Translate(Stream, Form);
  finally
    Stream.Free;
  end;
end;

procedure TJvTranslator.TranslateScreenString(const S: string);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(S);
  try
    TranslateScreen(Stream);
  finally
    Stream.Free;
  end;
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

function TJvTranslatorStrings.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJvTranslatorStrings.GetValue(Index: Integer): string;
begin
  if (Index >= 0) and (Index < Count) and (FList.Objects[Index] <> nil) then
    Result := PString(FList.Objects[Index])^
  else
    Result := '';
end;

procedure TJvTranslator.SkipProperty(AClass: TClass; const PropName: string);
var
  I: Integer;
  P: PSkipPropRec;
begin
  if FSkipList = nil then
    FSkipList := TList.Create;
  for I := 0 to FSkipList.Count - 1 do
    if PSkipPropRec(FSkipList[I])^.AClass = AClass then
    begin
      P := PSkipPropRec(FSkipList[I]);
      if PropName = '' then
        P^.AProps.Clear // skip entire class
      else
      if P^.AProps.Count > 0 then // only add if the class is not skipped as a whole
        P^.AProps.Add(PropName); // the list is sorted, so property name will only be added once
      Exit;
    end;
  // class not found, so add new class record to list
  New(P);
  P^.AClass := AClass;
  P^.AProps := TStringList.Create;
  P^.AProps.Sorted := True;
  if PropName <> '' then
    P^.AProps.Add(PropName); // skip this property only
  FSkipList.Add(P);
  if AClass.InheritsFrom(TPersistent) then
    RegisterClass(TPersistentClass(AClass));
end;

procedure TJvTranslator.UnskipProperty(AClass: TClass; const PropName: string);
var
  I, J: Integer;
  P: PSkipPropRec;
begin
  if FSkipList <> nil then
  begin
    for I := 0 to FSkipList.Count - 1 do
      if PSkipPropRec(FSkipList[I])^.AClass = AClass then
      begin
        P := PSkipPropRec(FSkipList[I]);
        if PropName <> '' then
          J := P^.AProps.IndexOf(PropName)
        else
        begin
          J := -1;
          P^.AProps.Clear;
        end;
        if J > -1 then
          P^.AProps.Delete(J);
        if P^.AProps.Count = 0 then
          // remove the entry when there are no properties skipped or if this is a UnskipClass call
        begin
          P^.AProps.Free;
          FSkipList.Delete(I);
          Dispose(P);
        end;
        if FSkipList.Count = 0 then
          FreeAndnil(FSkipList);
        Break;
      end;
  end;
end;

procedure TJvTranslator.ClearSkipList;
var
  I: Integer;
begin
  if FSkipList <> nil then
  begin
    for I := 0 to FSkipList.Count - 1 do
    begin
      PSkipPropRec(FSkipList[I]).AProps.Free;
      Dispose(PSkipPropRec(FSkipList[I]));
    end;
    FreeAndNil(FSkipList);
  end;
end;

end.

