{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxStrHlder.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}
unit JvxStrHlder;

interface

uses SysUtils, Classes, Variants, RTLConsts{, JvxComponent};

type

{$IFDEF Delphi3_Up}

{ TJvxMacro }

  TJvxMacros = class;
  TMacroTextEvent = procedure(Sender: TObject; Data: Variant; 
    var Text: string) of object;
  
  TJvxMacro = class(TCollectionItem)
  private
    FName: string;
    FData: Variant;
    FOnGetText: TMacroTextEvent;
    function IsMacroStored: Boolean;
    function GetText: string;
    function GetMacros: TJvxMacros;
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
    function IsEqual(Value: TJvxMacro): Boolean;
    property Macros: TJvxMacros read GetMacros;
    property Text: string read GetText;
  published
    property Name: string read FName write SetDisplayName;
    property Value: Variant read GetAsVariant write SetAsVariant stored IsMacroStored;
    property OnGetText: TMacroTextEvent read FOnGetText write FOnGetText;
  end;

{ TJvxMacros }

  TJvxMacros = class({$IFDEF Delphi4_Up}TOwnedCollection{$ELSE}TCollection{$ENDIF})
  private
    function GetMacroValue(const MacroName: string): Variant;
    procedure SetMacroValue(const MacroName: string;
      const Value: Variant);
    function GetItem(Index: Integer): TJvxMacro;
    procedure SetItem(Index: Integer; Value: TJvxMacro);
  public
{$IFDEF Delphi4_Up}
    constructor Create(AOwner: TPersistent);
{$ELSE}
    constructor Create;
{$ENDIF}
    procedure AssignValues(Value: TJvxMacros);
    procedure AddMacro(Value: TJvxMacro);
    procedure RemoveMacro(Value: TJvxMacro);
    function CreateMacro(const MacroName: string): TJvxMacro;
    procedure GetMacroList(List: TList; const MacroNames: string);
    function IndexOf(const AName: string): Integer;
    function IsEqual(Value: TJvxMacros): Boolean;
    function ParseString(const Value: string; DoCreate: Boolean; 
      SpecialChar: Char): string;
    function MacroByName(const Value: string): TJvxMacro;
    function FindMacro(const Value: string): TJvxMacro;
    property Items[Index: Integer]: TJvxMacro read GetItem write SetItem; default;
    property MacroValues[const MacroName: string]: Variant read GetMacroValue write SetMacroValue;
  end;

{$ENDIF Delphi3_Up}

{ TJvxStrHolder }

  TJvxStrHolder = class(TComponent)
  private
    FStrings: TStrings;
    FXorKey: string;
    FReserved: Integer;
{$IFDEF Delphi3_Up}
    FMacros: TJvxMacros;
    FMacroChar: Char;
    FOnExpandMacros: TNotifyEvent;
{$ENDIF}
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    function GetDuplicates: TDuplicates;
    procedure SetDuplicates(Value: TDuplicates);
    function GetSorted: Boolean;
    procedure SetSorted(Value: Boolean);
    procedure SetStrings(Value: TStrings);
    procedure StringsChanged(Sender: TObject);
    procedure StringsChanging(Sender: TObject);
    procedure ReadStrings(Reader: TReader);
    procedure WriteStrings(Writer: TWriter);
    procedure ReadVersion(Reader: TReader);
    procedure WriteVersion(Writer: TWriter);
{$IFDEF WIN32}
    function GetCommaText: string;
    procedure SetCommaText(const Value: string);
{$ENDIF}
{$IFDEF Delphi3_Up}
    function GetCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer);
{$ENDIF}
{$IFDEF Delphi3_Up}
    procedure SetMacros(Value: TJvxMacros);
    procedure RecreateMacros;
    procedure SetMacroChar(Value: Char);
{$ENDIF}
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Changed; dynamic;
    procedure Changing; dynamic;
{$IFDEF Delphi3_Up}
    procedure BeforeExpandMacros; dynamic;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
{$IFDEF Delphi3_Up}
    function MacroCount: Integer;
    function MacroByName(const MacroName: string): TJvxMacro;
    function ExpandMacros: string;
{$ENDIF}
{$IFDEF WIN32}
    property CommaText: string read GetCommaText write SetCommaText;
{$ENDIF}
  published
{$IFDEF Delphi3_Up}
    property Capacity: Integer read GetCapacity write SetCapacity default 0;
    property MacroChar: Char read FMacroChar write SetMacroChar default '%';
    property Macros: TJvxMacros read FMacros write SetMacros;
    property OnExpandMacros: TNotifyEvent read FOnExpandMacros write FOnExpandMacros;
{$ENDIF}
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates
      default dupIgnore;
    property KeyString: string read FXorKey write FXorKey stored False;
    property Sorted: Boolean read GetSorted write SetSorted default False;
    property Strings: TStrings read FStrings write SetStrings stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

implementation

uses
{$IFDEF Delphi3_Up}
  Consts,
{$ENDIF}
  JvxStrUtils;

const
  XorVersion = 1;

{$IFDEF Delphi3_Up}

function ExtractName(const Items: string; var Pos: Integer): string;
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(Items)) and (Items[I] <> ';') do Inc(I);
  Result := Trim(Copy(Items, Pos, I - Pos));
  if (I <= Length(Items)) and (Items[I] = ';') then Inc(I);
  Pos := I;
end;

function NameDelimiter(C: Char; Delims: TCharSet): Boolean;
begin
  Result := (C in [' ', ',', ';', ')', #13, #10]) or (C in Delims);
end;

function IsLiteral(C: Char): Boolean;
begin
  Result := C in ['''', '"'];
end;

procedure CreateMacros(List: TJvxMacros; const Value: PChar; SpecialChar: Char; Delims: TCharSet);
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
  if SpecialChar = #0 then Exit;
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
    CurChar := CurPos^;
    if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ <> SpecialChar) then
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter(CurChar, Delims)) do begin
        Inc(CurPos);
        CurChar := CurPos^;
        if IsLiteral(CurChar) then begin
          Literal := Literal xor True;
          if CurPos = StartPos + 1 then EmbeddedLiteral := True;
        end;
      end;
      CurPos^ := #0;
      if EmbeddedLiteral then begin
        Name := StripLiterals(StartPos + 1);
        EmbeddedLiteral := False;
      end
      else Name := StrPas(StartPos + 1);
      if Assigned(List) then begin
        if List.FindMacro(Name) = nil then
          List.CreateMacro(Name);
      end;
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end
    else if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ = SpecialChar) then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else if IsLiteral(CurChar) then Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;

{ TJvxMacro }

constructor TJvxMacro.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FData := Unassigned;
end;

procedure TJvxMacro.Assign(Source: TPersistent);
begin
  if (Source is TJvxMacro) and (Source <> nil) then begin
    if VarIsEmpty(TJvxMacro(Source).FData) then Clear
    else Value := TJvxMacro(Source).FData;
    Name := TJvxMacro(Source).Name;
  end;
end;

function TJvxMacro.GetDisplayName: string;
begin
  if FName = '' then 
    Result := inherited GetDisplayName 
  else
    Result := FName;
end;

procedure TJvxMacro.SetDisplayName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, FName) <> 0) and
    (Collection is TJvxMacros) and (TJvxMacros(Collection).IndexOf(Value) >= 0) then
    raise Exception.Create(SDuplicateString);    
  FName := Value;
  inherited;
end;

procedure TJvxMacro.GetMacroText(var AText: string);
begin
  if Assigned(FOnGetText) then FOnGetText(Self, FData, AText);
end;

function TJvxMacro.GetText: string;
begin
  Result := FData;
  GetMacroText(Result);
end;

function TJvxMacro.GetMacros: TJvxMacros;
begin
  if Collection is TJvxMacros then 
    Result := TJvxMacros(Collection)
  else 
    Result := nil;
end;

procedure TJvxMacro.Clear;
begin
  FData := Unassigned;
end;

function TJvxMacro.IsMacroStored: Boolean;
begin
  Result := not VarIsEmpty(FData);
end;

function TJvxMacro.GetAsVariant: Variant;
begin
  Result := FData;
end;

procedure TJvxMacro.SetAsVariant(Value: Variant);
begin
  FData := Value;
end;

function TJvxMacro.IsEqual(Value: TJvxMacro): Boolean;
begin
  Result := (VarType(FData) = VarType(Value.FData)) and
    (VarIsEmpty(FData) or (FData = Value.FData)) and
    (Name = Value.Name);
end;

{ TJvxMacros }

{$IFDEF Delphi4_Up}
constructor TJvxMacros.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvxMacro);
end;
{$ELSE}
constructor TJvxMacros.Create;
begin
  inherited Create(TJvxMacro);
end;
{$ENDIF}

function TJvxMacros.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(TJvxMacro(Items[Result]).Name, AName) = 0 then Exit;
  Result := -1;
end;

function TJvxMacros.GetItem(Index: Integer): TJvxMacro;
begin
  Result := TJvxMacro(inherited Items[Index]);
end;

procedure TJvxMacros.SetItem(Index: Integer; Value: TJvxMacro);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

procedure TJvxMacros.AddMacro(Value: TJvxMacro);
begin
  Value.Collection := Self;
end;

procedure TJvxMacros.RemoveMacro(Value: TJvxMacro);
begin
  if Value.Collection = Self then
    Value.Collection := nil;
end;

function TJvxMacros.CreateMacro(const MacroName: string): TJvxMacro;
begin
  Result := Add as TJvxMacro;
  Result.Name := MacroName;
end;

function TJvxMacros.IsEqual(Value: TJvxMacros): Boolean;
var
  I: Integer;
begin
  Result := Count = Value.Count;
  if Result then
    for I := 0 to Count - 1 do begin
      Result := Items[I].IsEqual(Value.Items[I]);
      if not Result then Break;
    end;
end;

function TJvxMacros.MacroByName(const Value: string): TJvxMacro;
begin
  Result := FindMacro(Value);
  if Result = nil then
    raise Exception.Create(SInvalidPropertyValue);
end;

function TJvxMacros.FindMacro(const Value: string): TJvxMacro;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do begin
    Result := TJvxMacro(inherited Items[I]);
    if AnsiCompareText(Result.Name, Value) = 0 then Exit;
  end;
  Result := nil;
end;

procedure TJvxMacros.AssignValues(Value: TJvxMacros);
var
  I: Integer;
  P: TJvxMacro;
begin
  BeginUpdate;
  try
    for I := 0 to Value.Count - 1 do begin
      P := FindMacro(Value[I].Name);
      if P <> nil then P.Assign(Value[I]);
    end;
  finally
    EndUpdate;
  end;
end;

function TJvxMacros.ParseString(const Value: string; DoCreate: Boolean; 
  SpecialChar: Char): string;
var
  Macros: TJvxMacros;
begin
  Result := Value;
  Macros := TJvxMacros.Create{$IFDEF Delphi4_Up}(Self.GetOwner){$ENDIF};
  try
    CreateMacros(Macros, PChar(Result), SpecialChar, ['.']);
    if DoCreate then begin
      Macros.AssignValues(Self);
      Self.Assign(Macros);
    end;
  finally
    Macros.Free;
  end;
end;

function TJvxMacros.GetMacroValue(const MacroName: string): Variant;
var
  I: Integer;
  Macros: TList;
begin
  if Pos(';', MacroName) <> 0 then begin
    Macros := TList.Create;
    try
      GetMacroList(Macros, MacroName);
      Result := VarArrayCreate([0, Macros.Count - 1], varVariant);
      for I := 0 to Macros.Count - 1 do
        Result[I] := TJvxMacro(Macros[I]).Value;
    finally
      Macros.Free;
    end;
  end 
  else Result := MacroByName(MacroName).Value;
end;

procedure TJvxMacros.SetMacroValue(const MacroName: string;
  const Value: Variant);
var
  I: Integer;
  Macros: TList;
begin
  if Pos(';', MacroName) <> 0 then begin
    Macros := TList.Create;
    try
      GetMacroList(Macros, MacroName);
      for I := 0 to Macros.Count - 1 do
        TJvxMacro(Macros[I]).Value := Value[I];
    finally
      Macros.Free;
    end;
  end 
  else MacroByName(MacroName).Value := Value;
end;

procedure TJvxMacros.GetMacroList(List: TList; const MacroNames: string);
var
  Pos: Integer;
begin
  Pos := 1;
  while Pos <= Length(MacroNames) do
    List.Add(MacroByName(ExtractName(MacroNames, Pos)));
end;

{$ENDIF Delphi3_Up}

{ TJvxStrHolder }

constructor TJvxStrHolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStrings := TStringList.Create;
{$IFDEF Delphi3_Up}
  FMacros := TJvxMacros.Create{$IFDEF Delphi4_Up}(Self){$ENDIF};
  FMacroChar := '%';
{$ENDIF}
  TStringList(FStrings).OnChange := StringsChanged;
  TStringList(FStrings).OnChanging := StringsChanging;
end;

destructor TJvxStrHolder.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
{$IFDEF Delphi3_Up}
  FMacros.Free;
{$ENDIF}
  FStrings.Free;
  inherited Destroy;
end;

procedure TJvxStrHolder.Assign(Source: TPersistent);
begin
  if Source is TStrings then
    FStrings.Assign(Source)
  else if Source is TJvxStrHolder then
    FStrings.Assign(TJvxStrHolder(Source).Strings)
  else
    inherited Assign(Source);
end;

procedure TJvxStrHolder.AssignTo(Dest: TPersistent);
begin
  if Dest is TStrings then
    Dest.Assign(Strings)
  else
    inherited AssignTo(Dest);
end;

procedure TJvxStrHolder.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJvxStrHolder.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TJvxStrHolder.Clear;
begin
  FStrings.Clear;
end;

{$IFDEF WIN32}
function TJvxStrHolder.GetCommaText: string;
begin
  Result := FStrings.CommaText;
end;

procedure TJvxStrHolder.SetCommaText(const Value: string);
begin
  FStrings.CommaText := Value;
end;
{$ENDIF WIN32}

{$IFDEF Delphi3_Up}
function TJvxStrHolder.GetCapacity: Integer;
begin
  Result := FStrings.Capacity;
end;

procedure TJvxStrHolder.SetCapacity(NewCapacity: Integer);
begin
  FStrings.Capacity := NewCapacity;
end;
{$ENDIF Delphi3_Up}

{$IFDEF Delphi3_Up}
procedure TJvxStrHolder.BeforeExpandMacros;
begin
  if Assigned(FOnExpandMacros) then FOnExpandMacros(Self);
end;

procedure TJvxStrHolder.SetMacros(Value: TJvxMacros);
begin
  FMacros.AssignValues(Value);
end;

procedure TJvxStrHolder.RecreateMacros;
begin
  if not (csReading in ComponentState) then
    Macros.ParseString(FStrings.Text, True, MacroChar);
end;

procedure TJvxStrHolder.SetMacroChar(Value: Char); 
begin
  if Value <> FMacroChar then begin
    FMacroChar := Value;
    RecreateMacros;
  end;
end;

function TJvxStrHolder.MacroCount: Integer;
begin
  Result := Macros.Count;
end;

function TJvxStrHolder.MacroByName(const MacroName: string): TJvxMacro;
begin
  Result := Macros.MacroByName(MacroName);
end;

function TJvxStrHolder.ExpandMacros: string;
var
  I, J, P, LiteralChars: Integer;
  Macro: TJvxMacro;
  Found: Boolean;
begin
  BeforeExpandMacros;
  Result := FStrings.Text;
  for I := Macros.Count - 1 downto 0 do begin
    Macro := Macros[I];
    if VarIsEmpty(Macro.FData) then Continue;
    repeat
      P := Pos(MacroChar + Macro.Name, Result);
      Found := (P > 0) and ((Length(Result) = P + Length(Macro.Name)) or
        NameDelimiter(Result[P + Length(Macro.Name) + 1], ['.']));
      if Found then begin
        LiteralChars := 0;
        for J := 1 to P - 1 do
          if IsLiteral(Result[J]) then Inc(LiteralChars);
        Found := LiteralChars mod 2 = 0;
        if Found then begin
          Result := Copy(Result, 1, P - 1) + Macro.Text + Copy(Result,
            P + Length(Macro.Name) + 1, MaxInt);
        end;
      end;
    until not Found;
  end;
end;
{$ENDIF Delphi3_Up}

procedure TJvxStrHolder.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
{$IFDEF WIN32}
  var
    I: Integer;
    Ancestor: TJvxStrHolder;
{$ENDIF}
  begin
{$IFDEF WIN32}
    Ancestor := TJvxStrHolder(Filer.Ancestor);
    Result := False;
    if (Ancestor <> nil) and (Ancestor.FStrings.Count = FStrings.Count) and
      (KeyString = Ancestor.KeyString) and (FStrings.Count > 0) then
      for I := 0 to FStrings.Count - 1 do begin
        Result := CompareText(FStrings[I], Ancestor.FStrings[I]) <> 0;
        if Result then Break;
      end
    else Result := (FStrings.Count > 0) or (Length(KeyString) > 0);
{$ELSE}
    Result := (FStrings.Count > 0) or (Length(KeyString) > 0);
{$ENDIF}
  end;

begin
  inherited DefineProperties(Filer);
  { for backward compatibility }
  Filer.DefineProperty('InternalVer', ReadVersion, WriteVersion,
    {$IFDEF WIN32} Filer.Ancestor = nil {$ELSE} False {$ENDIF});
  Filer.DefineProperty('StrData', ReadStrings, WriteStrings, DoWrite);
end;

function TJvxStrHolder.GetSorted: Boolean;
begin
  Result := TStringList(FStrings).Sorted;
end;

function TJvxStrHolder.GetDuplicates: TDuplicates;
begin
  Result := TStringList(FStrings).Duplicates;
end;

procedure TJvxStrHolder.ReadStrings(Reader: TReader);
begin
  Reader.ReadListBegin;
  if not Reader.EndOfList then KeyString := Reader.ReadString;
  FStrings.Clear;
  while not Reader.EndOfList do
    if FReserved >= XorVersion then
      FStrings.Add(XorDecode(KeyString, Reader.ReadString))
    else
      FStrings.Add(XorString(KeyString, Reader.ReadString));
  Reader.ReadListEnd;
end;

procedure TJvxStrHolder.SetDuplicates(Value: TDuplicates);
begin
  TStringList(FStrings).Duplicates := Value;
end;

procedure TJvxStrHolder.SetSorted(Value: Boolean);
begin
  TStringList(FStrings).Sorted := Value;
end;

procedure TJvxStrHolder.SetStrings(Value: TStrings);
begin
  FStrings.Assign(Value);
end;

procedure TJvxStrHolder.StringsChanged(Sender: TObject);
begin
{$IFDEF Delphi3_Up}
  RecreateMacros;
{$ENDIF}
  if not (csReading in ComponentState) then Changed;
end;

procedure TJvxStrHolder.StringsChanging(Sender: TObject);
begin
  if not (csReading in ComponentState) then Changing;
end;

procedure TJvxStrHolder.WriteStrings(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  Writer.WriteString(KeyString);
  for I := 0 to FStrings.Count - 1 do
{$IFDEF WIN32}
    Writer.WriteString(XorEncode(KeyString, FStrings[I]));
{$ELSE}
    Writer.WriteString(XorString(KeyString, FStrings[I]));
{$ENDIF}
  Writer.WriteListEnd;
end;

procedure TJvxStrHolder.ReadVersion(Reader: TReader);
begin
  FReserved := Reader.ReadInteger;
end;

procedure TJvxStrHolder.WriteVersion(Writer: TWriter);
begin
{$IFDEF WIN32}
  Writer.WriteInteger(XorVersion);
{$ENDIF}
end;

end.
