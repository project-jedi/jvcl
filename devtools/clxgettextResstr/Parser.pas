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

The Original Code is: Parser.pas, released on 2003-10-04.

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-10-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit Parser;
interface
uses
  SysUtils, Classes, Contnrs, dpp_PascalParser, QDialogs;

type
  TProgressEvent = procedure(Sender: TObject; const Text: string; Percentage: Integer) of object;
  TGetResNameEvent = procedure(Sender: TObject; var ResName: string; const Value: string) of object;

  TResStrings = class;

  TParser = class(TObject)
  private
    FOnProgress: TProgressEvent;
    FOnGetResName: TGetResNameEvent;
    FSubDirs: Boolean;
    FSingleResFile: Boolean;
    FResStrings: TResStrings;

    procedure DoProgress(const Text: string; Percentage: Integer);
    function GetResName(const Value: string): string;
  private
    function ReadString(Parser: TPascalParser): string;
    procedure ParseUnderscore(Parser: TPascalParser);
    function Parse(const FileName: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function ParseFiles(const Dir: string): Integer;
     // returns the number of parsed files

    property SubDirs: Boolean read FSubDirs write FSubDirs;
    property SingleResFile: Boolean read FSingleResFile write FSingleResFile;

    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnGetResName: TGetResNameEvent read FOnGetResName write FOnGetResName;
  end;

  TResItem = class(TObject)
  public
    Name: string;
    Value: string;
    RefCount: Integer;

    procedure AddRef;
  end;

  TResStrings = class(TObject)
  private
    FItems: TObjectList;
    function GetItems(Index: Integer): TResItem;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function HasResValue(const Value: string): TResItem;
    function NewResString(const Name, Value: string): TResItem;
    procedure Clear;

    function GetAsString: string;

    procedure SaveToFile(const Filename: string);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TResItem read GetItems;
  end;

implementation

uses
  JvQConsts; // (rom) for sLineBreak  no dependencies to packages

function SubStr(const Value: string; StartIndex, EndIndex: Integer): string;
begin
  Result := Copy(Value, StartIndex, EndIndex - StartIndex + 1);
end;

function GetCleanValue(const Value: string): string;
// removes all white chars from the string for better comparision
var
  Token: PTokenInfo;
  Parser: TPascalParser;
begin
  Result := '';
  Parser := TPascalParser.Create('', Value);
  while Parser.GetToken(Token) do
    Result := Result + Token.Value;
end;

function RemoveWhiteChars(const Value: string): string;
var i, j: Integer;
begin
  SetLength(Result, Length(Value));
  j := 0;
  for i := 1 to Length(Value) do
  begin
    if (Value[i] in ['a'..'z', 'A'..'Z']) then
    begin
      Inc(j);
      Result[j] := Value[i];
    end;
  end;
  SetLength(Result, j);
end;

function ProperOut(const Indent, Value: string; MaxLen: Integer): string;
begin
  Result := Value;
end;

function ReadFileToString(const FileName: string): AnsiString;
var Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, Stream.Size);
    if Result <> '' then
      Stream.Read(Result[1], Length(Result));
  finally
    Stream.Free;
  end;
end;

procedure WriteStringToFile(const FileName: string; const Data: AnsiString);
var Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Data <> '' then
      Stream.Write(Data[1], Length(Data));
  finally
    Stream.Free;
  end;
end;

function IsInList(const S: string; const A: array of string): Boolean;
var i: Integer;
begin
  Result := True;
  for i := 0 to High(A) do
    if CompareText(A[i], S) = 0 then
      Exit;
  Result := False;
end;

procedure FindFiles(const Dir: string; Files: TStrings; SubDirs: Boolean);
var sr: TSearchRec;
begin
  if FindFirst(Dir + '\*.*', faAnyFile or faDirectory, sr) = 0 then
  try
    repeat
      if sr.Attr and faDirectory <> 0 then
      begin
        if (SubDirs) and (sr.Name <> '.') and (sr.Name <> '..') then
          FindFiles(Dir, Files, SubDirs);
      end
      else
        Files.Add(Dir + '\' + sr.Name);
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
end;

{ TParser }

constructor TParser.Create;
begin
  inherited Create;
  FResStrings := TResStrings.Create;
end;

destructor TParser.Destroy;
begin
  FResStrings.Free;
  inherited Destroy;
end;

procedure TParser.DoProgress(const Text: string; Percentage: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Text, Percentage);
end;

function TParser.GetResName(const Value: string): string;
var
  Parser: TPascalParser;
  Token: PTokenInfo;
  S: string;
  P: PChar;
begin
  Result := '';
  Parser := TPascalParser.Create('', Value);
  try
    if Parser.GetToken(Token) then
    begin
      if Token.Kind = tkString then
      begin
        S := Token.Value;
        Delete(S, 1, 1);
        Delete(S, Length(S), 1);

        P := Pointer(S);
        if P <> nil then
        begin
          P[0] := UpCase(P[0]);
          Inc(P);
          while P[0] <> #0 do
          begin
            if (P[0] = ' ') and (P[1] in ['a'..'z']) then
              P[1] := UpCase(P[1]);
            Inc(P);
          end;
          Result := 's' + Copy(RemoveWhiteChars(S), 1, 35);
        end;
      end;
    end;
  finally
    Parser.Free;
  end;

  if Assigned(FOnGetResName) then
    FOnGetResName(Self, Result, Value);
end;

function TParser.ParseFiles(const Dir: string): Integer;
var
  i: Integer;
  Files: TStrings;
begin
  FResStrings.Clear;
  Result := 0;
  Files := TStringList.Create;
  try
    DoProgress('Starting...', 0);
    FindFiles(ExcludeTrailingPathDelimiter(Dir), Files, FSubDirs);

    for i := Files.Count - 1 downto 0 do
      if not IsInList(ExtractFileExt(Files[i]), ['.pas']) then
        Files.Delete(i);

    for i := 0 to Files.Count - 1 do
    begin
      DoProgress(Files[i], i * 100 div Files.Count);
      if Parse(Files[i]) then
        Inc(Result);
    end;
    DoProgress('Finished.', 100);
  finally
    Files.Free;
  end;

  if FSingleResFile and (FResStrings.Count > 0) then
    FResStrings.SaveToFile(ExcludeTrailingPathDelimiter(Dir) + 'Strings.res.pas');
end;

function TParser.Parse(const FileName: string): Boolean;
var
  Parser: TPascalParser;
  Token: PTokenInfo;
  InsertPos: Integer;
  S: string;
begin
  if not FSingleResFile then
    FResStrings.Clear;

  InsertPos := 0;

  Result := False;
  Parser := TPascalParser.Create('', ReadFileToString(FileName));
  try
    while Parser.GetToken(Token) do
    begin
      if Token.Kind = tkIdent then
      begin
        if Token.Value = '_' then
        begin
          ParseUnderscore(Parser);
          Result := True;
        end
        else if CompareText(Token.Value, 'implementation') = 0 then
        begin
          InsertPos := Token.StartIndex;
        end;
      end;
    end;

    if Result and (FResStrings.Count > 0) then
    begin
      S := Parser.Text;
      if not FSingleResFile then
      begin
//        FResStrings.SaveToFile(ChangeFileExt(FileName, '.res.inc'));
        if InsertPos > 0 then
          Insert(sLineBreak + FResStrings.GetAsString + sLineBreak,
            S, InsertPos)
        else
        begin
          ShowMessage('no "implementation" in unit ' + ExtractFileName(FileName));
          Exit;
        end;
      end;
//      WriteStringToFile(ChangeFileExt(FileName, '.pas.txt'), S);
      WriteStringToFile(FileName, S);
      S := '';
    end;
  finally
    Parser.Free;
  end;
end;

procedure TParser.ParseUnderscore(Parser: TPascalParser);
var
  Token: PTokenInfo;
  Item: TResItem;
  Value: string;
  StartIndex, EndIndex: Integer;
begin // CurToken='_'
  StartIndex := Parser.CurToken.StartIndex;
  if (not Parser.GetToken(Token)) or (Token.Value <> '(') then // -> '('
    Exit;
  if not Parser.GetToken(Token) then // -> string
    Exit;
  if Token.Kind = tkString then
  begin
    Value := ReadString(Parser);
    if Value <> '' then
    begin
      Item := FResStrings.HasResValue(Value);
      if Item = nil then
        Item := FResStrings.NewResString(GetResName(Value), Value);
    end
    else
      Exit;
    Token := Parser.CurToken;
    if Token = nil then
      Exit;
    if Token.Value = ',' then
    begin
     // needs Format()
      EndIndex := Parser.PreToken.EndIndex;
      Parser.Replace(StartIndex, EndIndex - StartIndex + 1, 'Format(' + Item.Name);
    end
    else if Token.Value = ')' then
    begin
      EndIndex := Token.EndIndex;
      Parser.Replace(StartIndex, EndIndex - StartIndex + 1, Item.Name);
      Parser.Index := StartIndex; // rescan
    end;
  end;
// CurToken=',' or ')'
end;

function TParser.ReadString(Parser: TPascalParser): string;
var
  Token: PTokenInfo;
  Bracket1, // ()
  Bracket2: Integer; // []
  StartIndex, EndIndex: Integer;
begin // CurToken='string'
  Token := Parser.CurToken;

  StartIndex := Token.StartIndex;
  EndIndex := Token.EndIndex;
  Result := Token.Value;

  Bracket1 := 1;
  Bracket2 := 0;
  while Parser.GetToken(Token) do
  begin
    if (Token.Kind = tkSymbol) then
    begin
      if Token.Value = '(' then Inc(Bracket1)
      else if Token.Value = ')' then Dec(Bracket1)
      else if Token.Value = '[' then Inc(Bracket2)
      else if Token.Value = ']' then Inc(Bracket2);

      if Bracket2 = 0 then
      begin
        if ((Bracket1 = 1) and (Token.Value = ',')) or
           ((Bracket1 = 0) and (Token.Value = ')')) then
        begin
          Result := SubStr(Parser.Text, StartIndex, EndIndex);
          Break;
        end;
      end;
    end;
    EndIndex := Token.EndIndex;
  end;
// CurToken=',' oder ')'
end;

{ TResStrings }

constructor TResStrings.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TResStrings.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TResStrings.Clear;
begin
  FItems.Clear;
end;

function TResStrings.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TResStrings.GetItems(Index: Integer): TResItem;
begin
  Result := TResItem(FItems[Index]);
end;

function TResStrings.HasResValue(const Value: string): TResItem;
var
  i: Integer;
  V: string;
begin
  V := GetCleanValue(Value);
  for i := 0 to Count - 1 do
  begin
    Result := Items[i];
    if GetCleanValue(Result.Value) = V then
      Exit;
  end;
  Result := nil;
end;

function TResStrings.NewResString(const Name, Value: string): TResItem;
var i: Integer;
begin
  if Trim(Name) = '' then
    raise Exception.CreateFmt('Invalid resourcestring name "%s"', [Name]);

  Result := TResItem.Create;
  Result.Name := Name;
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, Name) then
    begin
      Result.Name := Result.Name + '_';
      Break;
    end;
  Result.Value := Value;
  Result.RefCount := 1;
  FItems.Add(Result);
end;

procedure TResStrings.SaveToFile(const Filename: string);
var
  Lines: TStrings;
  i: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('resourcestring');
    for i := 0 to Count - 1 do
      Lines.Add('  ' + Items[i].Name + ' = ' + ProperOut('    ', Items[i].Value + ';', 70));
    Lines.SaveToFile(Filename);
  finally
    Lines.Free;
  end;
end;

function TResStrings.GetAsString: string;
var
  Lines: TStrings;
  i: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('resourcestring');
    for i := 0 to Count - 1 do
      Lines.Add('  ' + Items[i].Name + ' = ' + ProperOut('    ', Items[i].Value + ';', 70));
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

{ TResItem }

procedure TResItem.AddRef;
begin
  Inc(RefCount);
end;

end.
