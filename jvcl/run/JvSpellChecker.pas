{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSpellChecker.PAS, released on 2003-08-19.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-08-19

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
- Items in the UserDictionary are not added to the internal WordTable/SoundexTable when
  you add a new item (i.e cal USerDictionary.Add). This is mostly for performance. UserDictionary
  entries are loaded into the dictionary table in BuildTables, so to get them added
  make sure UserDictionary is filled before setting the Dictionary property. 
-----------------------------------------------------------------------------}

{$I JVCL.INC}
unit JvSpellChecker;

interface
uses
  Windows, SysUtils, Classes, Controls, Messages, JvSpellIntf, JvComponent;

type
  TJvSpellChecker = class(TJvComponent)
  private
    FSpellChecker: IJvSpellChecker;
    procedure SetText(const Value: string);
    function GetText: string;
    function GetDictionary: TFilename;
    function GetUserDictionary: TStrings;
    procedure SetDictionary(const Value: TFilename);
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
    property Dictionary: TFilename read GetDictionary write SetDictionary;
    property UserDictionary: TStrings read GetUserDictionary write SetUserDictionary;
    property Ignores: TStrings read GetIgnores write SetIgnores;
    property OnCanIgnore: TJvSpellCheckIgnoreEvent read GetCanIgnore write SetCanIgnore;
  end;

resourcestring
  SNoSpellCheckerAvailable = 'No IJvSpellChecker implementation available!';

implementation
uses
  JclStrings; // StrAddRef, StrDecRef 

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
  TJvDefaultSpellChecker = class(TInterfacedObject, IInterface, IJvSpellChecker)
  private
    FText, FCurrentWord: string;
    FPosition: integer;
    FDictionary: string;
    FSuggestions, FUserDictionary, FIgnores: TStringlist;
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
    function GetCurrentWord:string;virtual;
    procedure GetWordSuggestions(const Value: string; Strings: TStrings); virtual;
    procedure AddSoundex(ASoundex: TSoundex; Value: string); virtual;
    procedure AddWord(Value: string); virtual;
    function WordExists(const Value: string): boolean; virtual;
    function CanIgnore(const Value: string): boolean; virtual;
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
  Encode: array['A'..'Z'] of char =
  ('0', '1', '2', '3', '0', '1', '2', '/', '0', '2', '2',
    '4', '5', '5', '0', '1', '2', '6', '2', '3', '0', '1',
    '/', '2', '0', '2');
var
  Ch: char;
  Code, OldCode: char;
  SxInx: integer;
  i: integer;
begin
  Result := 'A000';
  if (Value = '') then Exit;
//    raise Exception.Create('Soundex: input string is empty');
  Ch := UpCase(Value[1]);
  if not ('A' <= Ch) and (Ch <= 'Z') then
    Ch := 'A';
//    raise Exception.Create('Soundex: unknown character in input string');
  Result[1] := Ch;
  Code := Encode[Ch];
  OldCode := Code;
  SxInx := 2;
  for i := 2 to Length(Value) do
  begin
    if (Code <> '/') then
      OldCode := Code;
    Ch := UpCase(Value[i]);
    if not ('A' <= Ch) and (Ch <= 'Z') then
      Code := '0'
    else
      Code := Encode[Ch];
    if (Code <> OldCode) and (Code > '0') then
    begin
      Result[SxInx] := Code;
      inc(SxInx);
      if (SxInx > 4) then
        Break;
    end;
  end;
end;

function ELFHash(const S: string): integer;
var
  G, i: integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
  begin
    Result := (Result shl 4) + ord(S[i]);
    G := Result and longint($F0000000);
    if (G <> 0) then
      Result := Result xor (G shr 24);
    Result := Result and (not G);
  end;
end;

function SoundexHash(const S: TSoundex): integer;
begin
  Result := ((Ord(S[1]) - ord('A')) * 343) +
    ((Ord(S[2]) - ord('0')) * 49) +
    ((Ord(S[3]) - ord('0')) * 7) +
    (Ord(S[4]) - ord('0'));
end;

function GetNextWord(var S: PAnsiChar; out Word: AnsiString; Delimiters: TSysCharSet): Boolean;
var
  Start: PAnsiChar;
begin
  Word := '';
  if S = nil then
  begin
    Result := True;
    Exit;
  end;
  Start := nil;
  Result := false;
  while True do
  begin
    if S^ = #0 then
    begin
      if Start <> nil then
      begin
        SetString(Word, Start, S - Start);
        Result := True;
      end;
      Exit;
    end
    else if S^ in Delimiters then
    begin
      if Start <> nil then
      begin
        SetString(Word, Start, S - Start);
        Exit;
      end
      else
        while (S^ in Delimiters) do
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

{ TJvDefaultSpellChecker }

procedure TJvDefaultSpellChecker.AddSoundex(ASoundex: TSoundex; Value: string);
var
  Hash: integer;
begin
  Hash := SoundexHash(ASoundex) mod SoundexTableSize;
  if FSoundexTable[Hash] = nil then
    FSoundexTable[Hash] := TList.Create;
  TList(FSoundexTable[Hash]).Add(Pointer(Value));
end;

procedure TJvDefaultSpellChecker.AddWord(Value: string);
var
  Hash: integer;
begin
  Hash := ELFHash(Value) mod WordTableSize;
  if FWordTable[Hash] = nil then
    FWordTable[Hash] := TList.Create;
  TList(FWordTable[Hash]).Add(Pointer(Value));
end;

procedure TJvDefaultSpellChecker.BuildTables;
var
  AFile: TextFile;
  Value: string;
  ASoundex: TSoundex;
  i: integer;
begin
  ClearTables;
  if FileExists(Dictionary) then
  begin
    System.AssignFile(AFile, Dictionary);
    System.Reset(AFile);
    try
      repeat
        readln(AFile, Value);
        if Value <> '' then
        begin
          Value := AnsiLowerCase(Value);
          StrAddRef(Value);
          AddWord(Value);
          ASoundex := Soundex(Value);
          AddSoundex(ASoundex, Value);
        end;
      until EOF(AFile);
    finally
      System.Close(AFile);
    end;
    for i := 0 to UserDictionary.Count - 1 do
      if UserDictionary[i] <> '' then
      begin
        Value := AnsiLowerCase(UserDictionary[i]);
        StrAddRef(Value);
        AddWord(Value);
        ASoundex := Soundex(Value);
        AddSoundex(ASoundex, Value);
      end;
  end;
end;

constructor TJvDefaultSpellChecker.Create;
begin
  inherited Create;
  FDelimiters := cDelimiters;
  FSuggestions := TStringlist.Create;
  FUserDictionary := TStringlist.Create;
  FUserDictionary.Sorted := true;
  FIgnores := TStringlist.Create;
  FIgnores.Sorted := true;

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
  inherited;
end;

procedure TJvDefaultSpellChecker.ClearTables;
var
  i, j: integer;
  aList: TList;
  S: string;
begin
  if FSoundexTable <> nil then
  begin
    for i := 0 to FSoundexTable.Count - 1 do
    begin
      TList(FSoundexTable[i]).Free;
      FSoundexTable[i] := nil;
    end;
//    FSoundexTable.Clear;
  end;

  if FWordTable <> nil then
  begin
    for i := 0 to FWordTable.Count - 1 do
    begin
      aList := TList(FWordTable[i]);
      if aList <> nil then
      begin
        for j := 0 to aList.Count - 1 do
        begin
          S := PChar(aList[j]);
          StrDecRef(S);
        end;
        TList(FWordTable[i]).Free;
        FWordTable[i] := nil;
      end;
    end;
//    FWordTable.Clear;
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

procedure TJvDefaultSpellChecker.GetWordSuggestions(const Value: string; Strings: TStrings);
var
  ASoundex: TSoundex;
  i, Hash: integer;
  aList: TList;
begin
  if Strings = nil then Exit;
  Strings.Clear;
  ASoundex := Soundex(Value);
  Hash := SoundexHash(ASoundex) mod SoundexTableSize;
  if FSoundexTable[Hash] <> nil then
  begin
    aList := TList(FSoundexTable[Hash]);
    for i := 0 to aList.Count - 1 do
      Strings.Add(string(aList[i]));
  end;
end;

function TJvDefaultSpellChecker.Next(out StartIndex, WordLength: Integer): WordBool;
var
  S: PChar;
begin
  StartIndex := 0;
  WordLength := 0;
  Result := false;
  if FPosition <= 0 then FPosition := 1;
  if FPosition >= Length(FText) then Exit;
  S := PChar(Text) + FPosition - 1;
  if (S = nil) or (S^ = #0) then Exit;
  while true do
  begin
    FCurrentWord := '';
    GetNextWord(S, FCurrentWord, Delimiters);
    WordLength := Length(FCurrentWord);
    StartIndex := S - PChar(Text) - WordLength + 1;
    FPosition := StartIndex + WordLength;
    if (S = nil) or (S^ = #0) then Exit;
    if (FCurrentWord <> '') and not CanIgnore(FCurrentWord) then
    begin
      FSuggestions.Clear;
      Result := not WordExists(FCurrentWord);
      if Result then
      begin
        GetWordSuggestions(FCurrentWord, FSuggestions);
        Exit;
      end;
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

function TJvDefaultSpellChecker.WordExists(const Value: string): boolean;
var
  i: integer;
  Hash: integer;
  aList: TList;
  FWord: string;
begin
  FWord := AnsiLowerCase(Value);
  Hash := ELFHash(FWord) mod WordTableSize;
  if FWordTable[Hash] <> nil then
  begin
    aList := TList(FWordTable[Hash]);
    for i := 0 to aList.Count - 1 do
      if AnsiCompareText(PChar(aList[i]), FWord) = 0 then
      begin
        Result := true;
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

function TJvDefaultSpellChecker.CanIgnore(const Value: string): boolean;
begin
  Result := false;
  if Assigned(FOnCanIgnore) then FOnCanIgnore(self, Value, Result);
end;

function TJvDefaultSpellChecker.GetCanIgnore: TJvSpellCheckIgnoreEvent;
begin
  Result := FOnCanIgnore;
end;

procedure TJvDefaultSpellChecker.SetCanIgnore(
  const Value: TJvSpellCheckIgnoreEvent);
begin
  FOnCanIgnore := Value;
end;

function TJvDefaultSpellChecker.GetCurrentWord: string;
begin
  Result := FCurrentWord;
end;

{ TJvSpellChecker }

function TJvSpellChecker.GetCanIgnore: TJvSpellCheckIgnoreEvent;
begin
  Result := SpellChecker.OnCanIgnore;
end;

function TJvSpellChecker.GetDelimiters: TSysCharSet;
begin
  Result := SpellChecker.Delimiters;
end;

function TJvSpellChecker.GetDictionary: TFilename;
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
    if not Assigned(CreateSpellChecker) then
      raise Exception.Create(SNoSpellCheckerAvailable);
    FSpellChecker := CreateSpellChecker;
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

procedure TJvSpellChecker.SetCanIgnore(
  const Value: TJvSpellCheckIgnoreEvent);
begin
  SpellChecker.OnCanIgnore := Value;
end;

procedure TJvSpellChecker.SetDelimiters(const Value: TSysCharSet);
begin
  SpellChecker.Delimiters := Value;
end;

procedure TJvSpellChecker.SetDictionary(const Value: TFilename);
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

initialization
  @CreateSpellChecker := @InternalCreateSpellChecker;

end.

