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
  SysUtils, Classes, JclAnsiStrings;

type
  TUsesParser = class (TObject)
  private
    FFileContent: AnsiString;
    FDefines: TJclAnsiStrings;
    FIncludeDirs: TStrings;
    FUsesList: TJclAnsiStrings;
    procedure SetDefines(const Value: TJclAnsiStrings);
    procedure SetFileContent(const Value: AnsiString);
    procedure SetIncludeDirs(const Value: TStrings);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function ParseUses: Boolean;
    function LoadFromFile(const FileName: string): Boolean;

    property FileContent: AnsiString read FFileContent write SetFileContent;
    property Defines: TJclAnsiStrings read FDefines write SetDefines;
    property IncludeDirs: TStrings read FIncludeDirs write SetIncludeDirs;
    property UsesList: TJclAnsiStrings read FUsesList;
  end;

implementation

uses
  {$IFDEF SUPPORTS_UNICODE}
  AnsiStrings,
  {$ENDIF SUPPORTS_UNICODE}
  Windows,
  JclStrings, JclFileUtils;

function ParseFile(const AFileContent: AnsiString;
  ADefines: TJclAnsiStrings; AIncludeDirs: TStrings; UsesList: TJclAnsiStrings; var IfDefCount: Integer;
  var InUsesSection: Boolean): Boolean;
  function GetNextWord(Ptr: PAnsiChar): AnsiString;
  var
    PtrStart: PAnsiChar;
  begin
    while not (Ptr^ in [AnsiNull, 'a'..'z', 'A'..'Z', '_', '0'..'9']) do
      Inc(Ptr);
    PtrStart := Ptr;
    while Ptr^ in ['a'..'z', 'A'..'Z', '_', '.', '0'..'9'] do
      Inc(Ptr);
    SetString(Result, PtrStart, Ptr - PtrStart);
  end;
  procedure Define(const Symbol: AnsiString);
  var
    Index: Integer;
  begin
    for Index := 0 to ADefines.Count - 1 do
      if AnsiSameText(Symbol, ADefines.Strings[Index]) then
        Exit;
    ADefines.Add(Symbol);
  end;
  procedure Undef(const Symbol: AnsiString);
  var
    Index: Integer;
  begin
    for Index := ADefines.Count - 1 downto 0 do
      if AnsiSameText(Symbol, ADefines.Strings[Index]) then
        ADefines.Delete(Index);
  end;
  function IfDef(const Symbol: AnsiString): Boolean;
  var
    Index: Integer;
  begin
    Result := True;
    for Index := 0 to ADefines.Count - 1 do
      if AnsiSameText(Symbol, ADefines.Strings[Index]) then
        Exit;
    Result := False;
  end;
  function ParseInclude(const Symbol: AnsiString): Boolean;
  var
    AFileStream: TFileStream;
    BFileContent: AnsiString;
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
        AFileStream := TFileStream.Create(BFileName, fmOpenRead or fmShareDenyWrite);
        try
          if AFileStream.Size > MaxInt then
            raise ERangeError.CreateFmt('File %s too big', [BFileName]);
          SetLength(BFileContent, AFileStream.Size);
          AFileStream.Read(BFileContent[1], AFileStream.Size);
          Result := ParseFile(BFileContent, ADefines, AIncludeDirs, UsesList, IfDefCount, InUsesSection);
        finally
          AFileStream.Free;
        end;
        Exit;
      end;
    end;
  end;
var
  Ptr, PtrStartWord: PAnsiChar;
  WordAtPtr: AnsiString;
begin
  Result := True;
  InUsesSection := False;

  Ptr := PAnsiChar(AFileContent);
  while True do
  begin
    case Ptr^ of
      AnsiNull :
        Exit;
      AnsiForwardSlash :
        if Ptr[1] = '/' then
          while not (Ptr^ in [AnsiNull, AnsiCarriageReturn, AnsiLineFeed]) do
            Inc(Ptr);
      '{' :
        begin
          if Ptr[1] = '$' then
          begin
            WordAtPtr := GetNextWord(@Ptr[1]);
            if AnsiSameText(WordAtPtr, AnsiString('IFDEF')) then
            begin
              // inside $IFDEF
              if (IfDefCount > 0) or not IfDef(GetNextWord(@Ptr[7])) then
                Inc(IfDefCount);
            end
            else
            if AnsiSameText(WordAtPtr, AnsiString('IFNDEF')) then
            begin
              // inside $IFNDEF
              if (IfDefCount > 0) or IfDef(GetNextWord(@Ptr[8])) then
                Inc(IfDefCount);
            end
            else
            if AnsiSameText(WordAtPtr, AnsiString('ELSE')) then
            begin
              // inside $ELSE
              if IfDefCount = 1 then
                IfDefCount := 0
              else if IfDefCount = 0 then
                IfDefCount := 1;
            end
            else
            if AnsiSameText(WordAtPtr, AnsiString('ENDIF')) then
            begin
              if IfDefCount > 0 then
                Dec(IfDefCount);
              // inside $ENDIF
            end
            else
            if AnsiSameText(WordAtPtr, AnsiString('DEFINE')) then
            begin
              // inside $DEFINE
              if IfDefCount = 0 then
                Define(GetNextWord(@Ptr[8]));
            end
            else
            if AnsiSameText(WordAtPtr, AnsiString('UNDEF')) then
            begin
              // inside $UNDEF
              if IfDefCount = 0 then
                Undef(GetNextWord(@Ptr[7]));
            end
            else
            if AnsiSameText(WordAtPtr, AnsiString('INCLUDE')) then
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
            if AnsiSameText(WordAtPtr, AnsiString('I')) then
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

          while not (Ptr^ in [AnsiNull, '}']) do
            Inc(Ptr);
        end;
      AnsiSingleQuote :
        begin
          Inc(Ptr);
          while not (Ptr^ in [AnsiNull, AnsiCarriageReturn, AnsiLineFeed, AnsiSingleQuote]) do
            Inc(Ptr);
        end;
      '(' :
        if Ptr[1] = '*' then
        begin
          Inc(Ptr, 2);
          while Ptr^ <> AnsiNull do
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
          while Ptr^ in ['a'..'z', 'A'..'Z', '_', '.', '0'..'9'] do
            Inc(Ptr);
          SetString(WordAtPtr, PtrStartWord, Ptr - PtrStartWord);
          if SameText(WordAtPtr, AnsiString('uses')) and (IfDefCount = 0) then
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
      AnsiNull :
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
  FDefines := TJclAnsiStringList.Create;
  FIncludeDirs := TStringList.Create;
  FUsesList := TJclAnsiStringList.Create;
end;

destructor TUsesParser.Destroy;
begin
  FDefines.Free;
  FIncludeDirs.Free;
  FUsesList.Free;
  inherited Destroy;
end;

function TUsesParser.LoadFromFile(const FileName: string): Boolean;
var
  AFileStream: TFileStream;
begin
  SetLength(FFileContent, 0);
  if FileExists(FileName) then
  begin
    AFileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      if AFileStream.Size > MaxInt then
        raise ERangeError.CreateFmt('File %s is too big', [FileName]);
      SetLength(FFileContent, AFileStream.Size);
      AFileStream.Read(FFileContent[1], AFileStream.Size);
    finally
      AFileStream.Free;
    end;
  end;

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

procedure TUsesParser.SetDefines(const Value: TJclAnsiStrings);
begin
  FDefines.Assign(Value);
end;

procedure TUsesParser.SetFileContent(const Value: AnsiString);
begin
  FFileContent := Value;
end;

procedure TUsesParser.SetIncludeDirs(const Value: TStrings);
begin
  FIncludeDirs.Assign(Value);
end;

end.
