{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Utils.pas, released on 2004-05-19.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit Utils;

interface

uses
  SysUtils, Classes;

type
  IStringBuilder = interface
    procedure Append(const Text: string); overload;
    procedure Append(const Args: array of string); overload;
    function GetValue: string; overload;
    procedure GetValue(var Value: string); overload;
    function Length: Integer;
    function Capacity: Integer;
  end;

function StringBuilder(const StartValue: string; StartCapacity: Integer = 1024;
  ADelta: Integer = 1024): IStringBuilder;

function AnsiStartsFilename(const SubStr, S: string): Boolean;
  // AnsiStartsText/Str depends on the used OS
function ParsePath(const Path: string): string;

function RepeatStr(const S: string; Count: Integer): string;
function IsEmptyStr(const S: string): Boolean;
function RemoveCommentChars(const S: string): string;
function TrimCopy(const S: string; Index, Count: Integer): string;

procedure ReadFileToString(const Filename: string; var S: string);
procedure WriteFileFromString(const Filename, S: string);

function FollowRelativeFilename(const RootDir: string; RelFilename: string): string;
function CutFirstDirectory(var Dir: string): string;

procedure ConvertBinDfmToText(const Filename: string);

implementation

uses
  StrUtils;

type
  TStringBuilder = class(TInterfacedObject, IStringBuilder)
  private
    FBuffer: string;
    FLength: Integer;
    FDelta: Integer;
    procedure Append(const Text: string); overload;
    procedure Append(const Args: array of string); overload;
    function GetValue: string; overload;
    procedure GetValue(var Value: string); overload;
    function Length: Integer;
    function Capacity: Integer;
  protected
    procedure Grow(AddLen: Integer);
  public
    constructor Create(const StartValue: string; StartCapacity: Integer = 1024;
      ADelta: Integer = 1024);
  end;

{ TStringBuilder }

constructor TStringBuilder.Create(const StartValue: string; StartCapacity,
  ADelta: Integer);
begin
  inherited Create;
  FDelta := ADelta;
  if FDelta < 16 then
    FDelta := 16;
  FLength := System.Length(StartValue);
  if StartCapacity < FLength then
    FBuffer := StartValue
  else
  begin
    SetLength(FBuffer, StartCapacity);
    if FLength > 0 then
      Move(StartValue[1], FBuffer[1], FLength);
  end;
end;

procedure TStringBuilder.Append(const Args: array of string);
var
  AddLen: Integer;
  i: Integer;
  Len: Integer;
begin
  AddLen := 0;
  for i := 0 to High(Args) do
    Inc(AddLen, System.Length(Args[i]));
  if AddLen = 0 then
    Exit; // nothing to do
  if FLength + AddLen > System.Length(FBuffer) then
    Grow(AddLen);
  for i := 0 to High(Args) do
  begin
    Len := System.Length(Args[i]);
    Move(Args[i][1], FBuffer[FLength + 1], Len);
    Inc(FLength, Len);
  end;
end;

procedure TStringBuilder.Append(const Text: string);
type
  PRec3 = ^TRec3;
  TRec3 = packed record
    res1: Word;
    res2: Byte;
  end;

  PRec5 = ^TRec5;
  TRec5 = packed record
    res1: LongWord;
    res2: Byte;
  end;

  PRec6 = ^TRec6;
  TRec6 = packed record
    res1: LongWord;
    res2: Word;
  end;

  PRec7 = ^TRec7;
  TRec7 = packed record
    res1: LongWord;
    res2: Word;
    res3: Byte;
  end;

var
  AddLen: Integer;
  P: PChar;
begin
  AddLen := System.Length(Text);
  if AddLen > 0 then
  begin
    if FLength + AddLen > System.Length(FBuffer) then
      Grow(AddLen);
    P := Pointer(FBuffer);
    Inc(P, FLength);
    case AddLen of
      1: P^ := Text[1];
      2: PWord(P)^ := PWord(Text)^;
      3: PRec3(P)^ := PRec3(Text)^;
      4: PLongWord(P)^ := PLongWord(Text)^;
      5: PRec5(P)^ := PRec5(Text)^;
      6: PRec6(P)^ := PRec6(Text)^;
      7: PRec7(P)^ := PRec7(Text)^;
      8: PInt64(P)^ := PInt64(Text)^;
    else
      Move(Text[1], P^, AddLen);
    end;
    Inc(FLength, AddLen);
  end;
end;

function TStringBuilder.Capacity: Integer;
begin
  Result := System.Length(FBuffer);
end;

function TStringBuilder.Length: Integer;
begin
  Result := FLength;
end;

function TStringBuilder.GetValue: string;
begin
  GetValue(Result);
end;

procedure TStringBuilder.GetValue(var Value: string);
begin
  if FLength > 0 then
    SetString(Value, PChar(Pointer(FBuffer)), FLength)
  else
    Value := '';
end;

procedure TStringBuilder.Grow(AddLen: Integer);
var
  NewLen: Integer;
begin
  NewLen := FLength + AddLen + FDelta;
  SetLength(FBuffer, NewLen);
end;


function StringBuilder(const StartValue: string; StartCapacity: Integer = 1024;
  ADelta: Integer = 1024): IStringBuilder;
begin
  Result := TStringBuilder.Create(StartValue, StartCapacity, ADelta);
end;

function AnsiStartsFilename(const SubStr, S: string): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := AnsiStartsText(SubStr, S);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := AnsiStartsStr(SubStr, S);
  {$ENDIF LINUX}
end;

{$IFDEF MSWINDOWS}
function ParsePath(const Path: string): string;
var
  i: Integer;
begin
  Result := Path;
  for i := 1 to Length(Result) do
    if Result[i] = '/' then
      Result[i] := '\';
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
function ParsePath(const Path: string): string;
var
  i: Integer;
begin
  Result := Path;
  for i := 1 to Length(Result) do
    if Result[i] = '\' then
      Result[i] := '/';
end;
{$ENDIF LINUX}

function RepeatStr(const S: string; Count: Integer): string;
var
  sb: IStringBuilder;
begin
  sb := StringBuilder('', Length(S) * Count);
  while Count > 0 do
  begin
    sb.Append(S);
    Dec(Count);
  end;
  sb.GetValue(Result);
end;

function IsEmptyStr(const S: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  if S <> '' then
  begin
    for i := 1 to Length(S) do
      if S[i] >= #32 then
      begin
        Result := False;
        Exit;
      end;
  end;
end;

function RemoveCommentChars(const S: string): string;
begin
  if S <> '' then
  begin
    if S[1] = '{' then
      Result := Copy(S, 2, Length(S) - 2)
    else // '(*'
      Result := Copy(S, 3, Length(S) - 3);
  end
  else
    Result := '';
end;

function TrimCopy(const S: string; Index, Count: Integer): string;
var
  Len, StartIndex, EndIndex: Integer;
begin
  Result := '';

  Len := Length(S);
  if Index <= 0 then
    Index := 1;
  if Count > Len then
    Count := Len;

  if (Count > 0) and (Len > 0) then
  begin
    StartIndex := Index;
    while (StartIndex <= Len) and (S[StartIndex] <= #32) do
      Inc(StartIndex);
    Dec(Count, StartIndex - Index);

    EndIndex := StartIndex + Count - 1;
    if EndIndex > Len then
    begin
      Dec(Count, EndIndex - Len);
      EndIndex := Len;
    end;

    while (EndIndex > 0) and (S[EndIndex] <= #32) do
    begin
      Dec(EndIndex);
      Dec(Count);
    end;

    if EndIndex >= StartIndex then
      SetString(Result, PChar(Pointer(S)) + StartIndex - 1, Count);
  end;
end;


procedure ReadFileToString(const Filename: string; var S: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(S, fs.Size);
    if Length(S) > 0 then
      fs.Read(S[1], Length(S));
  finally
    fs.Free;
  end;
end;

procedure WriteFileFromString(const Filename, S: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(Filename, fmCreate);
  try
    if Length(S) > 0 then
      fs.Write(S[1], Length(S));
  finally
    fs.Free;
  end;
end;

function CutFirstDirectory(var Dir: string): string;
var
  ps: Integer;
begin
  ps := Pos(PathDelim, Dir);
  if ps > 0 then
  begin
    Result := Copy(Dir, 1, ps - 1);
    Delete(Dir, 1, ps);
  end
  else
  begin
    Result := Dir;
    Dir := '';
  end;
end;

function FollowRelativeFilename(const RootDir: string; RelFilename: string): string;
var
  Dir: string;
begin
  Result := RootDir;
  while RelFilename <> '' do
  begin
    Dir := CutFirstDirectory(RelFilename);
    if Dir = '..' then
      Result := ExtractFileDir(Result)
    else if Dir = '.' then
      Continue
    else
      Result := Result + PathDelim + Dir;
  end;
end;

procedure ConvertBinDfmToText(const Filename: string);
var
  InStream, OutStream: TStream;
begin
  OutStream := TMemoryStream.Create;
  try
    InStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
    try
      ObjectResourceToText(InStream, OutStream);
    finally
      InStream.Free;
    end;
    TMemoryStream(OutStream).SaveToFile(Filename); // overwrite file
  finally
    OutStream.Free;
  end;
end;


end.
