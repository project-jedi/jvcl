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
  SysUtils, Classes, Forms, TypInfo, ComCtrls, Menus, IniFiles, Dialogs,
  JvSimpleXml, JvComponent;

type
  TJvTranslator = class(TJvComponent)
  private
    FXml: TJvSimpleXml;
  protected
    procedure TranslateComponent(const Component: TComponent;
      const Elem: TJvSimpleXmlElem); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Translate(const FileName: string); overload;
    procedure Translate(const Stream: TStream); overload;
    procedure Translate(const FileName: string; const Form: TCustomForm); overload;
    procedure Translate(const Form: TCustomForm); overload;
    procedure TranslateScreen(const FileName: string); overload;
    procedure TranslateScreen(const Stream: TStream); overload;
    function Translate(const Category, Item: string): string; overload;
  end;

  TJvTranslatorStrings = class(TJvComponent)
  private
    FList: THashedStringList;
    function GetString(const Index: Integer): string;
    procedure SetString(const Index: Integer; const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IndexOf(const Name: string): Integer;
    function Add(const Name: string; var Value: string): Integer;
    property Strings[const Index: Integer]: string read GetString write SetString;default;
  end;

implementation

uses
  JvTypes;

//=== TJvTranslator ==========================================================

constructor TJvTranslator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXml := TJvSimpleXml.Create(nil);
end;

destructor TJvTranslator.Destroy;
begin
  FXml.Free;
  inherited Destroy;
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
    for i:=0 to Screen.FormCount-1 do
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
    for i:=0 to Screen.FormCount-1 do
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
    Result := StringReplace(Value, '\n', CrLf, [rfReplaceAll]);
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
            SetStrProp(Obj, Prop, StringReplace(Elem.Properties[I].Value, '\n', CrLf, []));
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
              SetStrProp(Obj, Prop, StringReplace(Elem.Items[I].Value, '\n', CrLf, []));
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
            else
            if IsObject(Obj.ClassType, 'TTreeNodes') then
              TransTreeNodes(Obj, Elem.Items[I])
            else
            if IsObject(Obj.ClassType, 'TListItems') then
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

function TJvTranslatorStrings.GetString(const Index: Integer): string;
begin
  Result := FList[Index];
end;

function TJvTranslatorStrings.IndexOf(const Name: string): Integer;
begin
  Result := FList.IndexOf(Name);
end;

procedure TJvTranslatorStrings.SetString(const Index: Integer; const Value: string);
begin
  PString(FList.Objects[Index])^ := Value;
end;

end.

