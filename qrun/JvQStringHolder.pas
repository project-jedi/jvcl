{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStrHlder.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQStringHolder;

{$I jvcl.inc}

interface

uses
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  {$IFDEF HAS_UNIT_RTLCONSTS}
  RTLConsts,
  {$ENDIF HAS_UNIT_RTLCONSTS}
  SysUtils, Classes;

type
  TJvMacros = class;
  TMacroTextEvent = procedure(Sender: TObject; Data: Variant; var Text: string) of object;

  TJvMacro = class(TCollectionItem)
  private
    FName: string;
    FData: Variant;
    FOnGetText: TMacroTextEvent;
    function IsMacroStored: Boolean;
    function GetText: string;
    function GetMacros: TJvMacros;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    procedure GetMacroText(var AText: string);
    function GetAsVariant: Variant;
    procedure SetAsVariant(Value: Variant);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function IsEqual(Value: TJvMacro): Boolean;
    property Macros: TJvMacros read GetMacros;
    property Text: string read GetText;
  published
    property Name: string read FName write SetDisplayName;
    property Value: Variant read GetAsVariant write SetAsVariant stored IsMacroStored;
    property OnGetText: TMacroTextEvent read FOnGetText write FOnGetText;
  end;

  TJvMacros = class(TOwnedCollection)
  private
    function GetMacroValue(const MacroName: string): Variant;
    procedure SetMacroValue(const MacroName: string; const Value: Variant);
    function GetItem(Index: Integer): TJvMacro;
    procedure SetItem(Index: Integer; Value: TJvMacro);
  public
    constructor Create(AOwner: TPersistent);
    procedure AssignValues(Value: TJvMacros);
    procedure AddMacro(Value: TJvMacro);
    procedure RemoveMacro(Value: TJvMacro);
    function CreateMacro(const MacroName: string): TJvMacro;
    procedure GetMacroList(List: TList; const MacroNames: string);
    function IndexOf(const AName: string): Integer;
    function IsEqual(Value: TJvMacros): Boolean;
    function ParseString(const Value: string; DoCreate: Boolean; SpecialChar: Char): string;
    function MacroByName(const Value: string): TJvMacro;
    function FindMacro(const Value: string): TJvMacro;
    property Items[Index: Integer]: TJvMacro read GetItem write SetItem; default;
    property MacroValues[const MacroName: string]: Variant read GetMacroValue write SetMacroValue;
  end;

  TJvStrHolder = class(TComponent)
  private
    FStrings: TStringList;
    FXorKey: string;
    FReserved: Integer;
    FMacros: TJvMacros;
    FMacroChar: Char;
    FOnExpandMacros: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    function GetDuplicates: TDuplicates;
    procedure SetDuplicates(Value: TDuplicates);
    function GetSorted: Boolean;
    procedure SetSorted(Value: Boolean);
    function GetStrings: TStrings;
    procedure SetStrings(Value: TStrings);
    procedure StringsChanged(Sender: TObject);
    procedure StringsChanging(Sender: TObject);
    procedure ReadStrings(Reader: TReader);
    procedure WriteStrings(Writer: TWriter);
    procedure ReadVersion(Reader: TReader);
    procedure WriteVersion(Writer: TWriter);
    function GetCommaText: string;
    procedure SetCommaText(const Value: string);
    function GetCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetMacros(Value: TJvMacros);
    procedure RecreateMacros;
    procedure SetMacroChar(Value: Char);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Changed; dynamic;
    procedure Changing; dynamic;
    procedure BeforeExpandMacros; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function MacroCount: Integer;
    function MacroByName(const MacroName: string): TJvMacro;
    function ExpandMacros: string;
    property CommaText: string read GetCommaText write SetCommaText;
  published
    property Capacity: Integer read GetCapacity write SetCapacity default 0;
    property MacroChar: Char read FMacroChar write SetMacroChar default '%';
    property Macros: TJvMacros read FMacros write SetMacros;
    property OnExpandMacros: TNotifyEvent read FOnExpandMacros write FOnExpandMacros;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates default dupIgnore;
    property KeyString: string read FXorKey write FXorKey stored False;
    property Sorted: Boolean read GetSorted write SetSorted default False;
    property Strings: TStrings read GetStrings write SetStrings stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  { MultiStringHolder }

  EJvMultiStringHolderException = class(Exception);

  TJvMultiStringHolderCollectionItem = class(TCollectionItem)
  private
    FName: string;
    FStrings: TStrings;
    procedure SetName(Value: string);
    procedure SetStrings(const Value: TStrings);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Name: string read FName write SetName;
    property Strings: TStrings read FStrings write SetStrings;
  end;

  TJvMultiStringHolderCollection = class(TCollection)
  protected
    function GetItem(Index: Integer): TJvMultiStringHolderCollectionItem;
    procedure SetItem(Index: Integer; Value: TJvMultiStringHolderCollectionItem);
  public
    function DoesNameExist(const Name: string): Boolean;
    property Items[Index: Integer]: TJvMultiStringHolderCollectionItem read GetItem write SetItem;
    function Add: TJvMultiStringHolderCollectionItem;
    function Insert(Index: Integer): TJvMultiStringHolderCollectionItem;
  end;

  TJvMultiStringHolder = class(TComponent)
  private
    FMultipleStrings: TJvMultiStringHolderCollection;
    procedure SetMultipleStrings(Value: TJvMultiStringHolderCollection);
    function GetItemByName(const Name: string): TJvMultiStringHolderCollectionItem;
    function GetStringsByName(const Name: string): TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ItemByName[const Name: string]: TJvMultiStringHolderCollectionItem read GetItemByName;
    property StringsByName[const Name: string]: TStrings read GetStringsByName;
  published
    property MultipleStrings: TJvMultiStringHolderCollection read FMultipleStrings write SetMultipleStrings;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}  
  QConsts, 
  JvQJCLUtils, JvQResources, JvQConsts, JvQTypes;

const
  XorVersion = 1;

function ExtractName(const Items: string; var Pos: Integer): string;
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(Items)) and (Items[I] <> ';') do
    Inc(I);
  Result := Trim(Copy(Items, Pos, I - Pos));
  if (I <= Length(Items)) and (Items[I] = ';') then
    Inc(I);
  Pos := I;
end;

function NameDelimiter(C: Char; Delims: TCharSet): Boolean;
begin
  Result := (C in [' ', ',', ';', ')', Cr, Lf]) or (C in Delims);
end;

function IsLiteral(C: Char): Boolean;
begin
  Result := C in ['''', '"'];
end;

procedure CreateMacros(List: TJvMacros; const Value: PChar; SpecialChar: Char; Delims: TCharSet);
var
  CurPos, StartPos: PChar;
  CurChar: Char;
  Literal: Boolean;
  EmbeddedLiteral: Boolean;
  Name: string;

  function StripLiterals(Buffer: PChar): string;
  var
    Len: Word;
    TempBuf: PChar;

    procedure StripChar(Value: Char);
    begin
      if TempBuf^ = Value then
        StrMove(TempBuf, TempBuf + 1, Len - 1);
      if TempBuf[StrLen(TempBuf) - 1] = Value then
        TempBuf[StrLen(TempBuf) - 1] := #0;
    end;

  begin
    Len := StrLen(Buffer) + 1;
    TempBuf := AllocMem(Len);
    Result := '';
    try
      StrCopy(TempBuf, Buffer);
      StripChar('''');
      StripChar('"');
      Result := StrPas(TempBuf);
    finally
      FreeMem(TempBuf, Len);
    end;
  end;

begin
  if SpecialChar = #0 then
    Exit;
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
    CurChar := CurPos^;
    if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ <> SpecialChar) then
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter(CurChar, Delims)) do
      begin
        Inc(CurPos);
        CurChar := CurPos^;
        if IsLiteral(CurChar) then
        begin
          Literal := Literal xor True;
          if CurPos = StartPos + 1 then
            EmbeddedLiteral := True;
        end;
      end;
      CurPos^ := #0;
      if EmbeddedLiteral then
      begin
        Name := StripLiterals(StartPos + 1);
        EmbeddedLiteral := False;
      end
      else
        Name := StrPas(StartPos + 1);
      if Assigned(List) then
      begin
        if List.FindMacro(Name) = nil then
          List.CreateMacro(Name);
      end;
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end
    else
    if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ = SpecialChar) then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else
    if IsLiteral(CurChar) then
      Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;

//=== { TJvMacro } ===========================================================

constructor TJvMacro.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FData := Unassigned;
end;

procedure TJvMacro.Assign(Source: TPersistent);
begin
  if Source is TJvMacro then
  begin
    if VarIsEmpty(TJvMacro(Source).FData) then
      Clear
    else
      Value := TJvMacro(Source).FData;
    Name := TJvMacro(Source).Name;
  end
  else
    inherited Assign(Source);
end;

function TJvMacro.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := FName;
end;

procedure TJvMacro.SetDisplayName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, FName) <> 0) and
    (Collection is TJvMacros) and (TJvMacros(Collection).IndexOf(Value) >= 0) then
    raise EJVCLException.CreateRes(@SDuplicateString);
  FName := Value;
  inherited SetDisplayName(Value);
end;

procedure TJvMacro.GetMacroText(var AText: string);
begin
  if Assigned(FOnGetText) then
    FOnGetText(Self, FData, AText);
end;

function TJvMacro.GetText: string;
begin
  Result := FData;
  GetMacroText(Result);
end;

function TJvMacro.GetMacros: TJvMacros;
begin
  if Collection is TJvMacros then
    Result := TJvMacros(Collection)
  else
    Result := nil;
end;

procedure TJvMacro.Clear;
begin
  FData := Unassigned;
end;

function TJvMacro.IsMacroStored: Boolean;
begin
  Result := not VarIsEmpty(FData);
end;

function TJvMacro.GetAsVariant: Variant;
begin
  Result := FData;
end;

procedure TJvMacro.SetAsVariant(Value: Variant);
begin
  FData := Value;
end;

function TJvMacro.IsEqual(Value: TJvMacro): Boolean;
begin
  Result := (VarType(FData) = VarType(Value.FData)) and
    (VarIsEmpty(FData) or (FData = Value.FData)) and
    (Name = Value.Name);
end;

//=== { TJvMacros } ==========================================================

constructor TJvMacros.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvMacro);
end;

function TJvMacros.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiSameText(TJvMacro(Items[Result]).Name, AName) then
      Exit;
  Result := -1;
end;

function TJvMacros.GetItem(Index: Integer): TJvMacro;
begin
  Result := TJvMacro(inherited Items[Index]);
end;

procedure TJvMacros.SetItem(Index: Integer; Value: TJvMacro);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

procedure TJvMacros.AddMacro(Value: TJvMacro);
begin
  Value.Collection := Self;
end;

procedure TJvMacros.RemoveMacro(Value: TJvMacro);
begin
  if Value.Collection = Self then
    Value.Collection := nil;
end;

function TJvMacros.CreateMacro(const MacroName: string): TJvMacro;
begin
  Result := Add as TJvMacro;
  Result.Name := MacroName;
end;

function TJvMacros.IsEqual(Value: TJvMacros): Boolean;
var
  I: Integer;
begin
  Result := Count = Value.Count;
  if Result then
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].IsEqual(Value.Items[I]);
      if not Result then
        Break;
    end;
end;

function TJvMacros.MacroByName(const Value: string): TJvMacro;
begin
  Result := FindMacro(Value);
  if Result = nil then
    raise EJVCLException.CreateRes(@SInvalidPropertyValue);
end;

function TJvMacros.FindMacro(const Value: string): TJvMacro;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TJvMacro(inherited Items[I]);
    if AnsiSameText(Result.Name, Value) then
      Exit;
  end;
  Result := nil;
end;

procedure TJvMacros.AssignValues(Value: TJvMacros);
var
  I: Integer;
  P: TJvMacro;
begin
  BeginUpdate;
  try
    for I := 0 to Value.Count - 1 do
    begin
      P := FindMacro(Value[I].Name);
      if P <> nil then
        P.Assign(Value[I]);
    end;
  finally
    EndUpdate;
  end;
end;

function TJvMacros.ParseString(const Value: string; DoCreate: Boolean;
  SpecialChar: Char): string;
var
  Macros: TJvMacros;
begin
  Result := Value;
  Macros := TJvMacros.Create(Self.GetOwner);
  try
    CreateMacros(Macros, PChar(Result), SpecialChar, ['.']);
    if DoCreate then
    begin
      Macros.AssignValues(Self);
      Self.Assign(Macros);
    end;
  finally
    Macros.Free;
  end;
end;

function TJvMacros.GetMacroValue(const MacroName: string): Variant;
var
  I: Integer;
  Macros: TList;
begin
  if Pos(';', MacroName) <> 0 then
  begin
    Macros := TList.Create;
    try
      GetMacroList(Macros, MacroName);
      Result := VarArrayCreate([0, Macros.Count - 1], varVariant);
      for I := 0 to Macros.Count - 1 do
        Result[I] := TJvMacro(Macros[I]).Value;
    finally
      Macros.Free;
    end;
  end
  else
    Result := MacroByName(MacroName).Value;
end;

procedure TJvMacros.SetMacroValue(const MacroName: string;
  const Value: Variant);
var
  I: Integer;
  Macros: TList;
begin
  if Pos(';', MacroName) <> 0 then
  begin
    Macros := TList.Create;
    try
      GetMacroList(Macros, MacroName);
      for I := 0 to Macros.Count - 1 do
        TJvMacro(Macros[I]).Value := Value[I];
    finally
      Macros.Free;
    end;
  end
  else
    MacroByName(MacroName).Value := Value;
end;

procedure TJvMacros.GetMacroList(List: TList; const MacroNames: string);
var
  Pos: Integer;
begin
  Pos := 1;
  while Pos <= Length(MacroNames) do
    List.Add(MacroByName(ExtractName(MacroNames, Pos)));
end;

//=== { TJvStrHolder } =======================================================

constructor TJvStrHolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStrings := TStringList.Create;
  FMacros := TJvMacros.Create(Self);
  FMacroChar := '%';
  FStrings.OnChange := StringsChanged;
  FStrings.OnChanging := StringsChanging;
end;

destructor TJvStrHolder.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  FMacros.Free;
  FStrings.OnChange := nil;
  FStrings.OnChanging := nil;
  FStrings.Free;
  inherited Destroy;
end;

procedure TJvStrHolder.Assign(Source: TPersistent);
begin
  if Source is TStrings then
    FStrings.Assign(Source)
  else
  if Source is TJvStrHolder then
    FStrings.Assign(TJvStrHolder(Source).Strings)
  else
    inherited Assign(Source);
end;

procedure TJvStrHolder.AssignTo(Dest: TPersistent);
begin
  if Dest is TStrings then
    Dest.Assign(Strings)
  else
    inherited AssignTo(Dest);
end;

procedure TJvStrHolder.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvStrHolder.Changing;
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TJvStrHolder.Clear;
begin
  Strings.Clear;
end;

function TJvStrHolder.GetCommaText: string;
begin
  Result := Strings.CommaText;
end;

procedure TJvStrHolder.SetCommaText(const Value: string);
begin
  Strings.CommaText := Value;
end;

function TJvStrHolder.GetCapacity: Integer;
begin
  Result := Strings.Capacity;
end;

procedure TJvStrHolder.SetCapacity(NewCapacity: Integer);
begin
  Strings.Capacity := NewCapacity;
end;

procedure TJvStrHolder.BeforeExpandMacros;
begin
  if Assigned(FOnExpandMacros) then
    FOnExpandMacros(Self);
end;

procedure TJvStrHolder.SetMacros(Value: TJvMacros);
begin
  FMacros.AssignValues(Value);
end;

procedure TJvStrHolder.RecreateMacros;
begin
  if not (csReading in ComponentState) then
    Macros.ParseString(Strings.Text, True, MacroChar);
end;

procedure TJvStrHolder.SetMacroChar(Value: Char);
begin
  if Value <> FMacroChar then
  begin
    FMacroChar := Value;
    RecreateMacros;
  end;
end;

function TJvStrHolder.MacroCount: Integer;
begin
  Result := Macros.Count;
end;

function TJvStrHolder.MacroByName(const MacroName: string): TJvMacro;
begin
  Result := Macros.MacroByName(MacroName);
end;

function TJvStrHolder.ExpandMacros: string;
var
  I, J, P, LiteralChars: Integer;
  Macro: TJvMacro;
  Found: Boolean;
begin
  BeforeExpandMacros;
  Result := Strings.Text;
  for I := Macros.Count - 1 downto 0 do
  begin
    Macro := Macros[I];
    if VarIsEmpty(Macro.FData) then
      Continue;
    repeat
      P := Pos(MacroChar + Macro.Name, Result);
      Found := (P > 0) and ((Length(Result) = P + Length(Macro.Name)) or
        NameDelimiter(Result[P + Length(Macro.Name) + 1], ['.']));
      if Found then
      begin
        LiteralChars := 0;
        for J := 1 to P - 1 do
          if IsLiteral(Result[J]) then
            Inc(LiteralChars);
        Found := LiteralChars mod 2 = 0;
        if Found then
        begin
          Result := Copy(Result, 1, P - 1) + Macro.Text + Copy(Result,
            P + Length(Macro.Name) + 1, MaxInt);
        end;
      end;
    until not Found;
  end;
end;

procedure TJvStrHolder.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  var
    I: Integer;
    Ancestor: TJvStrHolder;
  begin
    Ancestor := TJvStrHolder(Filer.Ancestor);
    Result := False;
    if (Ancestor <> nil) and (Ancestor.Strings.Count = Strings.Count) and
      (KeyString = Ancestor.KeyString) and (Strings.Count > 0) then
      for I := 0 to Strings.Count - 1 do
      begin
        Result := CompareText(Strings[I], Ancestor.Strings[I]) <> 0;
        if Result then
          Break;
      end
    else
      Result := (Strings.Count > 0) or (Length(KeyString) > 0);
  end;

begin
  inherited DefineProperties(Filer);
  { for backward compatibility }
  Filer.DefineProperty('InternalVer', ReadVersion, WriteVersion, Filer.Ancestor = nil);
  Filer.DefineProperty('StrData', ReadStrings, WriteStrings, DoWrite);
end;

function TJvStrHolder.GetSorted: Boolean;
begin
  Result := FStrings.Sorted;
end;

function TJvStrHolder.GetDuplicates: TDuplicates;
begin
  Result := FStrings.Duplicates;
end;

procedure TJvStrHolder.ReadStrings(Reader: TReader);
begin
  Reader.ReadListBegin;
  if not Reader.EndOfList then
    KeyString := Reader.ReadString;
  Strings.Clear;
  while not Reader.EndOfList do
    if FReserved >= XorVersion then
      Strings.Add(XorDecode(KeyString, Reader.ReadString))
    else
      Strings.Add(XorString(KeyString, Reader.ReadString));
  Reader.ReadListEnd;
end;

procedure TJvStrHolder.SetDuplicates(Value: TDuplicates);
begin
  FStrings.Duplicates := Value;
end;

procedure TJvStrHolder.SetSorted(Value: Boolean);
begin
  FStrings.Sorted := Value;
end;

function TJvStrHolder.GetStrings: TStrings;
begin
  Result := FStrings;
end;

procedure TJvStrHolder.SetStrings(Value: TStrings);
begin
  FStrings.Assign(Value);
end;

procedure TJvStrHolder.StringsChanged(Sender: TObject);
begin
  RecreateMacros;
  if not (csReading in ComponentState) then
    Changed;
end;

procedure TJvStrHolder.StringsChanging(Sender: TObject);
begin
  if not (csReading in ComponentState) then
    Changing;
end;

procedure TJvStrHolder.WriteStrings(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  Writer.WriteString(KeyString);
  for I := 0 to Strings.Count - 1 do
    Writer.WriteString(XorEncode(KeyString, Strings[I]));
  Writer.WriteListEnd;
end;

procedure TJvStrHolder.ReadVersion(Reader: TReader);
begin
  FReserved := Reader.ReadInteger;
end;

procedure TJvStrHolder.WriteVersion(Writer: TWriter);
begin
  Writer.WriteInteger(XorVersion);
end;

//=== { TJvMultiStringHolderCollectionItem } =================================

procedure TJvMultiStringHolderCollectionItem.SetName(Value: string);
begin
  Value := Trim(Value);
  if Value = '' then
    FName := ''
  else
  begin
    if not TJvMultiStringHolderCollection(Collection).DoesNameExist(Value) then
      FName := Value
    else
      raise EJVCLException.CreateRes(@SDuplicateString);
  end;
end;

procedure TJvMultiStringHolderCollectionItem.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
end;

function TJvMultiStringHolderCollectionItem.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := RsNoName;
end;

constructor TJvMultiStringHolderCollectionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FStrings := TStringList.Create;
end;

destructor TJvMultiStringHolderCollectionItem.Destroy;
begin
  FStrings.Free;
  inherited Destroy;
end;

//=== { TJvMultiStringHolderCollection } =====================================

function TJvMultiStringHolderCollection.GetItem(Index: Integer): TJvMultiStringHolderCollectionItem;
begin
  Result := TJvMultiStringHolderCollectionItem(inherited GetItem(Index));
end;

procedure TJvMultiStringHolderCollection.SetItem(Index: Integer; Value: TJvMultiStringHolderCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TJvMultiStringHolderCollection.DoesNameExist(const Name: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
    if CompareText(Items[I].Name, Name) = 0 then
      Exit;
  Result := False;
end;

function TJvMultiStringHolderCollection.Add: TJvMultiStringHolderCollectionItem;
begin
  Result := TJvMultiStringHolderCollectionItem(inherited Add);
end;

function TJvMultiStringHolderCollection.Insert(Index: Integer): TJvMultiStringHolderCollectionItem;
begin
  Result := Add;
  Result.Index := Index;
end;

//=== { TJvMultiStringHolder } ===============================================

constructor TJvMultiStringHolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMultipleStrings := TJvMultiStringHolderCollection.Create(TJvMultiStringHolderCollectionItem);
end;

destructor TJvMultiStringHolder.Destroy;
begin
  FMultipleStrings.Free;
  inherited Destroy;
end;

procedure TJvMultiStringHolder.SetMultipleStrings(Value: TJvMultiStringHolderCollection);
begin
  FMultipleStrings.Assign(Value);
end;

function TJvMultiStringHolder.GetItemByName(const Name: string): TJvMultiStringHolderCollectionItem;
var
  I: Integer;
begin
  for I := 0 to MultipleStrings.Count - 1 do
    if CompareText(MultipleStrings.Items[I].Name, Name) = 0 then
    begin
      Result := MultipleStrings.Items[I];
      Exit;
    end;
  raise EJvMultiStringHolderException.CreateResFmt(@RsNoItemFoundWithName, [Name]);
end;

function TJvMultiStringHolder.GetStringsByName(const Name: string): TStrings;
begin
  Result := GetItemByName(Name).Strings;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

