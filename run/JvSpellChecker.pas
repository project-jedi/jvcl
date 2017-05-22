{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSpellChecker.PAS, released on 2003-08-19.

The Initial Developer of the Original Code is Peter Th�rnqvist [peter3 at sourceforge dot net]
Portions created by Peter Th�rnqvist are Copyright (C) 2003 Peter Th�rnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
- Items in the UserDictionary are not added to the internal WordTable/SoundexTable when
  you add a new item (i.e call UserDictionary.Add). This is mostly for performance.
  UserDictionary entries are loaded into the dictionary table in BuildTables, so to get
  them added make sure UserDictionary is filled before setting the Dictionary property.
-----------------------------------------------------------------------------}
// $Id$

unit JvSpellChecker;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Windows, Controls, Messages,
  JvSpellIntf, JvComponentBase;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF RTL230_UP}
  TJvSpellChecker = class(TJvComponent)
  private
    FSpellChecker: IJvSpellChecker;
    procedure SetText(const Value: string);
    function GetText: string;
    function GetDictionary: TFileName;
    function GetUserDictionary: TStrings;
    procedure SetDictionary(const Value: TFileName);
    procedure SetUserDictionary(const Value: TStrings);
    function GetSpellChecker: IJvSpellChecker;
    function GetDelimiters: TSysCharSet;
    procedure SetDelimiters(const Value: TSysCharSet);
    function GetIgnores: TStrings;
    procedure SetIgnores(const Value: TStrings);
    function GetCanIgnore: TJvSpellCheckIgnoreEvent;
    procedure SetCanIgnore(const Value: TJvSpellCheckIgnoreEvent);
  public
    // reference to the actual spell check implementation
    property SpellChecker: IJvSpellChecker read GetSpellChecker;
    property Delimiters: TSysCharSet read GetDelimiters write SetDelimiters;
  published
    // Surface interface properties to make it a bit easier to work with this component
    property Text: string read GetText write SetText;
    property Dictionary: TFileName read GetDictionary write SetDictionary;
    property UserDictionary: TStrings read GetUserDictionary write SetUserDictionary;
    property Ignores: TStrings read GetIgnores write SetIgnores;
    property OnCanIgnore: TJvSpellCheckIgnoreEvent read GetCanIgnore write SetCanIgnore;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
//  JclStrings, // StrAddRef, StrDecRef
  JvTypes, JvResources;

// NOTE: hash table and soundex lookup code originally from Julian Bucknall's
// "Algorithms Alfresco" column in The Delphi Magazine, Issue 52, December 1999
// Used with permission
const
  WordTableSize = 10007; {a prime}
  SoundexTableSize = 26 * 7 * 7 * 7; {the exact number of Soundexes}
  cDelimiters: TSysCharSet = [#0..#32, '.', ',', '<', '>', '=', '!', '?', ':', ';', '"', '''', '(', ')', '[', ']', '{', '}', '+', '|'];

type
  TSoundex = string[4];
  // default implementation of the IJvSpellChecker interface. To provide a new implementation,
  // assign a function to the CreateSpellChecker function variable in JvSpellIntf that returns an
  // instance of your implementation. For more info, see InternalSpellChecker in this unit.
  TJvDefaultSpellChecker = class(TInterfacedObject, IJvSpellChecker)
  private
    FText: string;
    FCurrentWord: string;
    FPosition: Integer;
    FDictionary: string;
    FSuggestions: TStringList;
    FUserDictionary: TStringList;
    FIgnores: TStringList;
    FWordTable: TList;
    FSoundexTable: TList;
    FDelimiters: TSysCharSet;
    FOnCanIgnore: TJvSpellCheckIgnoreEvent;
    { IJvSpellChecker }
    procedure SetDictionary(const Value: string);
    function GetDictionary: string;
    function GetUserDictionary: TStrings;
    procedure SetUserDictionary(const Value: TStrings);
    function GetSuggestions: TStrings;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetIgnores: TStrings;
    procedure SetIgnores(const Value: TStrings);
    function GetDelimiters: TSysCharSet;
    procedure SetDelimiters(const Value: TSysCharSet);
    function GetCanIgnore: TJvSpellCheckIgnoreEvent;
    procedure SetCanIgnore(const Value: TJvSpellCheckIgnoreEvent);
  protected
    procedure BuildTables; virtual;
    procedure ClearTables; virtual;
    function GetCurrentWord: string; virtual;
    procedure GetWordSuggestions(const Value: string; AStrings: TStrings); virtual;
    procedure AddSoundex(ASoundex: TSoundex; const Value: string); virtual;
    procedure AddWord(const Value: string); virtual;
    function WordExists(const Value: string): Boolean; virtual;
    function CanIgnore(const Value: string): Boolean; virtual;
    { IJvSpellChecker }
    function Next(out StartIndex, WordLength: Integer): WordBool; virtual;
    procedure Seek(Position: Integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Delimiters: TSysCharSet read GetDelimiters write SetDelimiters;
    property Suggestions: TStrings read GetSuggestions;
    property Dictionary: string read GetDictionary write SetDictionary;
    property UserDictionary: TStrings read GetUserDictionary write SetUserDictionary;
    property Text: string read GetText write SetText;
    property Ignores: TStrings read GetIgnores write SetIgnores;
    property OnCanIgnore: TJvSpellCheckIgnoreEvent read GetCanIgnore write SetCanIgnore;
  end;

function InternalCreateSpellChecker: IJvSpellChecker;
begin
  // create our implementation of the spell checker interface
  Result := TJvDefaultSpellChecker.Create;
end;

function Soundex(const Value: string): TSoundex;
const
  Encode: array ['A'..'Z'] of AnsiChar =
   ('0', '1', '2', '3', '0', '1', '2', '/', '0', '2', '2',
    '4', '5', '5', '0', '1', '2', '6', '2', '3', '0', '1',
    '/', '2', '0', '2');
var
  Ch: Char;
  Code, OldCode: AnsiChar;
  SxInx: Integer;
  I: Integer;
  UpperValue: string;
begin
  Result := 'A000';
  if Value = '' then
    Exit;
//    raise Exception.Create('Soundex: input string is empty');
  UpperValue := AnsiUpperCase(Value);

  Ch := UpperValue[1];
  if (Ch < 'A') or (Ch > 'Z') then
    Ch := 'A';
//    raise Exception.Create('Soundex: unknown character in input string');
  Result[1] := AnsiChar(Ch);
  Code := Encode[Ch];
  OldCode := Code;
  SxInx := 2;
  for I := 2 to Length(UpperValue) do
  begin
    if (Code <> '/') then
      OldCode := Code;
    Ch := UpperValue[I];
    if not ('A' <= Ch) and (Ch <= 'Z') then
      Code := '0'
    else
      Code := Encode[Ch];
    if (Code <> OldCode) and (Code > '0') then
    begin
      Result[SxInx] := Code;
      Inc(SxInx);
      if SxInx > 4 then
        Break;
    end;
  end;
end;

function ELFHash(const S: string): Integer;
var
  G, I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
  begin
    Result := (Result shl 4) + Ord(S[I]);
    G := Result and Longint($F0000000);
    if G <> 0 then
      Result := Result xor (G shr 24);
    Result := Result and (not G);
  end;
end;

function SoundexHash(const S: TSoundex): Integer;
begin
  Result :=
    ((Ord(S[1]) - Ord('A')) * 343) +
    ((Ord(S[2]) - Ord('0')) * 49) +
    ((Ord(S[3]) - Ord('0')) * 7) +
     (Ord(S[4]) - Ord('0'));
end;

function GetNextWord(var S: PChar; out Word: string; Delimiters: TSysCharSet): Boolean;
var
  Start: PChar;
begin
  Word := '';
  Result := S = nil;
  if Result then
    Exit;
  Start := nil;
  while True do
  begin
    if S^ = #0 then
    begin
      Word := Start;
      Result := Start <> nil;
      Exit;
    end
    else
    if ((S^ <= #255) and (AnsiChar(S^) in Delimiters)) then
    begin
      if Start <> nil then
      begin
        SetString(Word, Start, S - Start);
        Exit;
      end
      else
        while ((S^ <= #255) and (AnsiChar(S^) in Delimiters)) and (S^ <> #0) do
          Inc(S);
    end
    else
    begin
      if Start = nil then
        Start := S;
      Inc(S);
    end;
  end;
end;

//=== { TJvDefaultSpellChecker } =============================================

constructor TJvDefaultSpellChecker.Create;
begin
  inherited Create;
  FDelimiters := cDelimiters;
  FSuggestions := TStringList.Create;
  FUserDictionary := TStringList.Create;
  FUserDictionary.Sorted := True;
  FIgnores := TStringList.Create;
  FIgnores.Sorted := True;

  FWordTable := TList.Create;
  FWordTable.Count := WordTableSize;
  FSoundexTable := TList.Create;
  FSoundexTable.Count := SoundexTableSize;
end;

destructor TJvDefaultSpellChecker.Destroy;
begin
  ClearTables;
  FreeAndNil(FSuggestions);
  FreeAndNil(FUserDictionary);
  FreeAndNil(FWordTable);
  FreeAndNil(FSoundexTable);
  FreeAndNil(FIgnores);
  inherited Destroy;
end;

procedure TJvDefaultSpellChecker.AddSoundex(ASoundex: TSoundex; const Value: string);
var
  Hash: Integer;
begin
  Hash := SoundexHash(ASoundex) mod SoundexTableSize;
  if FSoundexTable[Hash] = nil then
    FSoundexTable[Hash] := TStringList.Create;
  TStringList(FSoundexTable[Hash]).Add(Value);
end;

procedure TJvDefaultSpellChecker.AddWord(const Value: string);
var
  Hash: Integer;
begin
  Hash := ELFHash(Value) mod WordTableSize;
  if FWordTable[Hash] = nil then
    FWordTable[Hash] := TStringList.Create;
  TStringList(FWordTable[Hash]).Add(Value);
end;

procedure TJvDefaultSpellChecker.BuildTables;
var
  AFile: TextFile;
  Value: string;
  LastValue: string;
  SoundexVal: TSoundex;
  I: Integer;
  N: Integer;
begin
  ClearTables;
  if FileExists(Dictionary) then
  begin
    System.AssignFile(AFile, Dictionary);
    System.Reset(AFile);
    try
      repeat
        ReadLn(AFile, Value);
        if Value <> '' then
        begin
          // (rom) simple compession for dictionary
          N := Ord(Value[1]) - Ord('0');
          Value := Copy(Value, 2, Length(Value) - 1);
          if N > 0 then
            Value := Copy(LastValue, 1, N) + Value;
          LastValue := Value;

          Value := AnsiLowerCase(Value);
          AddWord(Value);
          SoundexVal := Soundex(Value);
          AddSoundex(SoundexVal, Value);
        end;
      until Eof(AFile);
    finally
      System.Close(AFile);
    end;
    for I := 0 to UserDictionary.Count - 1 do
      if UserDictionary[I] <> '' then
      begin
        Value := AnsiLowerCase(UserDictionary[I]);
        AddWord(Value);
        SoundexVal := Soundex(Value);
        AddSoundex(SoundexVal, Value);
      end;
  end;
end;

procedure TJvDefaultSpellChecker.ClearTables;
var
  I: Integer;
begin
  if FSoundexTable <> nil then
    for I := 0 to FSoundexTable.Count - 1 do
    begin
      TObject(FSoundexTable[I]).Free;
      FSoundexTable[I] := nil;
    end;

  if FWordTable <> nil then
    for I := 0 to FWordTable.Count - 1 do
    begin
      TObject(FWordTable[I]).Free;
      FWordTable[I] := nil;
    end;
end;

function TJvDefaultSpellChecker.GetSuggestions: TStrings;
begin
  Result := FSuggestions;
end;

function TJvDefaultSpellChecker.GetText: string;
begin
  Result := FText;
end;

procedure TJvDefaultSpellChecker.GetWordSuggestions(const Value: string; AStrings: TStrings);
var
  SoundexVal: TSoundex;
  Hash: Integer;
begin
  if AStrings <> nil then
  begin
    AStrings.BeginUpdate;
    try
      AStrings.Clear;
      SoundexVal := Soundex(Value);
      Hash := SoundexHash(SoundexVal) mod SoundexTableSize;
      if FSoundexTable[Hash] <> nil then
        AStrings.AddStrings(TStringList(FSoundexTable[Hash]));
    finally
      AStrings.EndUpdate;
    end;
  end;
end;

function TJvDefaultSpellChecker.Next(out StartIndex, WordLength: Integer): WordBool;
var
  S: PChar;
begin
  StartIndex := 0;
  WordLength := 0;
  Result := False;
  if FPosition <= 0 then
    FPosition := 1;
  if FPosition >= Length(FText) then
    Exit;
  S := PChar(Text) + FPosition - 1;
  if (S = nil) or (S^ = #0) or (Trim(S) = '') then
    Exit;
  while True do
  begin
    FCurrentWord := '';
    GetNextWord(S, FCurrentWord, Delimiters);
    WordLength := Length(FCurrentWord);
    StartIndex := S - PChar(Text) - WordLength + 1;
    FPosition := StartIndex + WordLength;
    if (FCurrentWord <> '') and not CanIgnore(FCurrentWord) then
    begin
      FSuggestions.Clear;
      Result := not WordExists(FCurrentWord);
      if Result then
      begin
        GetWordSuggestions(FCurrentWord, FSuggestions);
        Break;
      end;
    end;
    if (S = nil) or (S^ = #0) or (Trim(S) = '') then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TJvDefaultSpellChecker.Seek(Position: Integer);
begin
  FPosition := Position;
end;

procedure TJvDefaultSpellChecker.SetText(const Value: string);
begin
  FText := Value;
  FPosition := 1;
end;

function TJvDefaultSpellChecker.WordExists(const Value: string): Boolean;
var
  I: Integer;
  Hash: Integer;
  List: TStringList;
  FWord: string;
begin
  FWord := AnsiLowerCase(Value);
  Hash := ELFHash(FWord) mod WordTableSize;
  if FWordTable[Hash] <> nil then
  begin
    List := TStringList(FWordTable[Hash]);
    for I := 0 to List.Count - 1 do
      if AnsiSameText(PChar(List[I]), FWord) then
      begin
        Result := True;
        Exit;
      end;
  end;
  // ignore or user word?
  Result := (UserDictionary.IndexOf(FWord) > -1) or (Ignores.IndexOf(FWord) > -1);
end;

function TJvDefaultSpellChecker.GetDictionary: string;
begin
  Result := FDictionary;
end;

function TJvDefaultSpellChecker.GetUserDictionary: TStrings;
begin
  Result := FUserDictionary;
end;

procedure TJvDefaultSpellChecker.SetDictionary(const Value: string);
begin
  if FDictionary <> Value then
  begin
    FDictionary := Value;
    BuildTables;
  end;
end;

procedure TJvDefaultSpellChecker.SetUserDictionary(const Value: TStrings);
begin
  FUserDictionary.Assign(Value);
end;

function TJvDefaultSpellChecker.GetIgnores: TStrings;
begin
  Result := FIgnores;
end;

procedure TJvDefaultSpellChecker.SetIgnores(const Value: TStrings);
begin
  FIgnores.Assign(Value);
end;

function TJvDefaultSpellChecker.GetDelimiters: TSysCharSet;
begin
  Result := FDelimiters;
end;

procedure TJvDefaultSpellChecker.SetDelimiters(const Value: TSysCharSet);
begin
  FDelimiters := Value;
end;

function TJvDefaultSpellChecker.CanIgnore(const Value: string): Boolean;
begin
  Result := False;
  if Assigned(FOnCanIgnore) then
    FOnCanIgnore(Self, Value, Result);
end;

function TJvDefaultSpellChecker.GetCanIgnore: TJvSpellCheckIgnoreEvent;
begin
  Result := FOnCanIgnore;
end;

procedure TJvDefaultSpellChecker.SetCanIgnore(const Value: TJvSpellCheckIgnoreEvent);
begin
  FOnCanIgnore := Value;
end;

function TJvDefaultSpellChecker.GetCurrentWord: string;
begin
  Result := FCurrentWord;
end;

//=== { TJvSpellChecker } ====================================================

function TJvSpellChecker.GetCanIgnore: TJvSpellCheckIgnoreEvent;
begin
  Result := SpellChecker.OnCanIgnore;
end;

function TJvSpellChecker.GetDelimiters: TSysCharSet;
begin
  Result := SpellChecker.Delimiters;
end;

function TJvSpellChecker.GetDictionary: TFileName;
begin
  Result := SpellChecker.Dictionary;
end;

function TJvSpellChecker.GetIgnores: TStrings;
begin
  Result := SpellChecker.Ignores;
end;

function TJvSpellChecker.GetSpellChecker: IJvSpellChecker;
begin
  if FSpellChecker = nil then
  begin
    if Assigned(CreateSpellChecker) then
      FSpellChecker := CreateSpellChecker
    else
      FSpellChecker := InternalCreateSpellChecker;
  end;
  Result := FSpellChecker;
end;

function TJvSpellChecker.GetText: string;
begin
  Result := SpellChecker.GetText;
end;

function TJvSpellChecker.GetUserDictionary: TStrings;
begin
  Result := SpellChecker.UserDictionary;
end;

procedure TJvSpellChecker.SetCanIgnore(const Value: TJvSpellCheckIgnoreEvent);
begin
  SpellChecker.OnCanIgnore := Value;
end;

procedure TJvSpellChecker.SetDelimiters(const Value: TSysCharSet);
begin
  SpellChecker.Delimiters := Value;
end;

procedure TJvSpellChecker.SetDictionary(const Value: TFileName);
begin
  SpellChecker.Dictionary := Value;
end;

procedure TJvSpellChecker.SetIgnores(const Value: TStrings);
begin
  SpellChecker.Ignores := Value;
end;

procedure TJvSpellChecker.SetText(const Value: string);
begin
  SpellChecker.SetText(Value);
end;

procedure TJvSpellChecker.SetUserDictionary(const Value: TStrings);
begin
  SpellChecker.UserDictionary := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);
{$ENDIF UNITVERSIONING}

{$IFDEF UNITVERSIONING}
finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
