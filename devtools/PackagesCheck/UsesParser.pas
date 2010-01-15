{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: UsesParser.pas, released on 2006-02-20.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2006 Florent Ouchet.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit UsesParser;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes;

type
  TUsesParser = class (TObject)
  private
    FFileContent: string;
    FDefines: TStrings;
    FIncludeDirs: TStrings;
    FUsesList: TStrings;
    procedure SetDefines(const Value: TStrings);
    procedure SetFileContent(const Value: string);
    procedure SetIncludeDirs(const Value: TStrings);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function ParseUses: Boolean;
    function LoadFromFile(const FileName: string): Boolean;

    property FileContent: string read FFileContent write SetFileContent;
    property Defines: TStrings read FDefines write SetDefines;
    property IncludeDirs: TStrings read FIncludeDirs write SetIncludeDirs;
    property UsesList: TStrings read FUsesList;
  end;

implementation

uses
  JclStrings, JclFileUtils, JclStreams;

function LoadFile(const FileName: string): string;
var
  AFileStream: TStream;
  AStringStream: TJclAutoStream;
  Start, Count: Integer;
begin
  Result := '';
  AFileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  if AFileStream.Size > MaxInt then
    raise ERangeError.CreateFmt('File %s too big', [FileName]);
  SetLength(Result, AFileStream.Size);
  try
    AStringStream := TJclAutoStream.Create(AFileStream);
    try
      Start := 0;
      repeat
        Count := AFileStream.Size;
        SetLength(Result, Length(Result) + Count);
        Count := AStringStream.ReadString(Result, Start + 1, Count);
        Inc(Start, Count);
      until Count = 0;
      SetLength(Result, Start);
    finally
      AStringStream.Free;
    end;
  finally
    AFileStream.Free;
  end;
end;

function ParseFile(const AFileContent: string;
  ADefines, AIncludeDirs, UsesList: TStrings; var IfDefCount: Integer;
  var InUsesSection: Boolean): Boolean;
  function GetNextWord(Ptr: PChar): string;
  var
    PtrStart: PChar;
  begin
    while (Ptr^ <> NativeNull) and not CharIsValidIdentifierLetter(Ptr^) do
      Inc(Ptr);
    PtrStart := Ptr;
    while (Ptr^ = '.') or CharIsValidIdentifierLetter(Ptr^) do
      Inc(Ptr);
    SetString(Result, PtrStart, Ptr - PtrStart);
  end;
  procedure Define(const Symbol: string);
  var
    Index: Integer;
  begin
    for Index := 0 to ADefines.Count - 1 do
      if SameText(Symbol, ADefines.Strings[Index]) then
        Exit;
    ADefines.Add(Symbol);
  end;
  procedure Undef(const Symbol: string);
  var
    Index: Integer;
  begin
    for Index := ADefines.Count - 1 downto 0 do
      if SameText(Symbol, ADefines.Strings[Index]) then
        ADefines.Delete(Index);
  end;
  function IfDef(const Symbol: string): Boolean;
  var
    Index: Integer;
  begin
    Result := True;
    for Index := 0 to ADefines.Count - 1 do
      if SameText(Symbol, ADefines.Strings[Index]) then
        Exit;
    Result := False;
  end;
  function ParseInclude(const Symbol: string): Boolean;
  var
    BFileContent: string;
    Index: Integer;
    AFileName, BFileName: string;
  begin
    Result := False;
    AFileName := StrTrimQuotes(string(Symbol));
    for Index := 0 to AIncludeDirs.Count - 1 do
    begin
      BFileName := PathAddSeparator(AIncludeDirs.Strings[Index]) + AFileName;
      if FileExists(BFileName) then
      begin
        BFileContent := LoadFile(BFileName);
        Result := ParseFile(BFileContent, ADefines, AIncludeDirs, UsesList, IfDefCount, InUsesSection);
        Exit;
      end;
    end;
  end;
var
  Ptr, PtrStartWord: PChar;
  WordAtPtr: string;
begin
  Result := True;
  InUsesSection := False;

  Ptr := PChar(AFileContent);
  while True do
  begin
    case Ptr^ of
      NativeNull :
        Exit;
      NativeForwardSlash :
        if Ptr[1] = '/' then
          while (Ptr^ <> NativeNull) and (Ptr^ <> NativeCarriageReturn) and (Ptr^ <> NativeLineFeed) do
            Inc(Ptr);
      '{' :
        begin
          if Ptr[1] = '$' then
          begin
            WordAtPtr := GetNextWord(@Ptr[1]);
            if SameText(WordAtPtr, 'IFDEF') then
            begin
              // inside $IFDEF
              if (IfDefCount > 0) or not IfDef(GetNextWord(@Ptr[7])) then
                Inc(IfDefCount);
            end
            else
            if SameText(WordAtPtr, 'IFNDEF') then
            begin
              // inside $IFNDEF
              if (IfDefCount > 0) or IfDef(GetNextWord(@Ptr[8])) then
                Inc(IfDefCount);
            end
            else
            if SameText(WordAtPtr, 'ELSE') then
            begin
              // inside $ELSE
              if IfDefCount = 1 then
                IfDefCount := 0
              else if IfDefCount = 0 then
                IfDefCount := 1;
            end
            else
            if SameText(WordAtPtr, 'ENDIF') then
            begin
              if IfDefCount > 0 then
                Dec(IfDefCount);
              // inside $ENDIF
            end
            else
            if SameText(WordAtPtr, 'DEFINE') then
            begin
              // inside $DEFINE
              if IfDefCount = 0 then
                Define(GetNextWord(@Ptr[8]));
            end
            else
            if SameText(WordAtPtr, 'UNDEF') then
            begin
              // inside $UNDEF
              if IfDefCount = 0 then
                Undef(GetNextWord(@Ptr[7]));
            end
            else
            if SameText(WordAtPtr, 'INCLUDE') then
            begin
              // inside $INCLUDE
              if IfDefCount = 0 then
              begin
                Result := ParseInclude(GetNextWord(@Ptr[9]));
                if not Result then
                  Exit;
              end;
            end
            else
            if SameText(WordAtPtr, 'I') then
            begin
              // inside $I
              if IfDefCount = 0 then
              begin
                Result := ParseInclude(GetNextWord(@Ptr[3]));
                if not Result then
                  Exit;
              end;
            end; // TODO: $IF + condition
          end;

          while (Ptr^ <> NativeNull) and (Ptr^ <>  '}') do
            Inc(Ptr);
        end;
      NativeSingleQuote :
        begin
          Inc(Ptr);
          while (Ptr^ <> NativeNull) and (Ptr^ <> NativeCarriageReturn) and
                (Ptr^ <> NativeLineFeed) and (Ptr^ <> NativeSingleQuote) do
            Inc(Ptr);
        end;
      '(' :
        if Ptr[1] = '*' then
        begin
          Inc(Ptr, 2);
          while Ptr^ <> NativeNull do
            if (Ptr^ = '*') and (Ptr[1] = ')') then
          begin
            Inc(Ptr, 2);
            Break;
          end
          else
            Inc(Ptr);
        end;
      'a'..'z',
      'A'..'Z',
      '_' :
        begin
          PtrStartWord := Ptr;
          while (Ptr^ = '.') or CharIsValidIdentifierLetter(Ptr^) do
            Inc(Ptr);
          SetString(WordAtPtr, PtrStartWord, Ptr - PtrStartWord);
          if SameText(WordAtPtr, 'uses') and (IfDefCount = 0) then
            InUsesSection := True
          else
          if InUsesSection and (UsesList.IndexOf(WordAtPtr) = -1)
            and (IfDefCount = 0) then
            UsesList.Add(WordAtPtr);
        end;
      ';' :
        if IfDefCount = 0 then
          InUsesSection := False;
    end;
    case Ptr^ of
      NativeNull :
        Exit;
      ';' :
        if IfDefCount = 0 then
          InUsesSection := False;
    end;
    Inc(Ptr);
  end;
end;

//=== TUsesParser ============================================================

constructor TUsesParser.Create;
begin
  inherited Create;
  FDefines := TStringList.Create;
  FIncludeDirs := TStringList.Create;
  FUsesList := TStringList.Create;
end;

destructor TUsesParser.Destroy;
begin
  FDefines.Free;
  FIncludeDirs.Free;
  FUsesList.Free;
  inherited Destroy;
end;

function TUsesParser.LoadFromFile(const FileName: string): Boolean;
begin
  SetLength(FFileContent, 0);
  if FileExists(FileName) then
    FFileContent := LoadFile(FileName);
  Result := Length(FileContent) > 0;
end;

function TUsesParser.ParseUses: Boolean;
var
  IfDefCount: Integer;
  InUsesSection: Boolean;
begin
  IfDefCount := 0;
  InUsesSection := False;
  Result := ParseFile(FileContent, Defines, IncludeDirs, UsesList, IfDefCount, InUsesSection);
end;

procedure TUsesParser.SetDefines(const Value: TStrings);
begin
  FDefines.Assign(Value);
end;

procedure TUsesParser.SetFileContent(const Value: string);
begin
  FFileContent := Value;
end;

procedure TUsesParser.SetIncludeDirs(const Value: TStrings);
begin
  FIncludeDirs.Assign(Value);
end;

end.
