{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Utils.pas, released on 2004-03-29.

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

{$I jvcl.inc}

unit Utils;
                                               
interface

uses
  Windows, ShellAPI, SysUtils, Classes, JvConsts;

function WordWrapString(const S: string; Width: Integer = 75): string;
  
function CompareFileAge(const Filename1Fmt: string; const Args1: array of const;
  const Filename2Fmt: string; const Args2: array of const): Integer;
function GetReturnPath(const Dir: string): string;
function FileExists(const Filename: string): Boolean;
function DirectoryExists(const Dir: string): Boolean;

function FollowRelativeFilename(const RootDir: string; RelFilename: string): string;
function CutFirstDirectory(var Dir: string): string;

function StartsWith(const Text, StartText: string; CaseInsensitive: Boolean = False): Boolean;
function EndsWith(const Text, EndText: string; CaseInsensitive: Boolean): Boolean;
function SubStr(const Text: string; StartIndex, EndIndex: Integer): string;

function HasText(Text: string; const Values: array of string): Boolean; // case insensitive
procedure AddPaths(List: TStrings; Add: Boolean; const Dir: string;
  const Paths: array of string);
function OpenAtAnchor(const FileName, Anchor: string): Boolean;

procedure FindFiles(const Dir, Mask: string; SubDirs: Boolean; List: TStrings;
  const FileExtensions: array of string);

function FindFilename(const Paths: string; const Filename: string): string;

{$IFDEF COMPILER5}
type
  IInterface = IUnknown;

function Supports(const Intf: IInterface; const IID: TGUID): Boolean; overload;

function FileSetReadOnly(const FileName: string; ReadOnly: Boolean): Boolean;
{$ENDIF COMPILER5}

implementation

uses
  DelphiData;

function WordWrapString(const S: string; Width: Integer = 75): string;
var
  i, cnt, Len, LastWordStart, BreakStrLen: Integer;
begin
  Result := S;
  BreakStrLen := Length(sLineBreak);
  if (Width <= 0) or (S = '') then
    Exit;

  Len := Length(Result);
  i := 1;
  while i <= Len do
  begin
    cnt := 0;
    LastWordStart := 0;
    while (i <= Len) and ((LastWordStart = 0) or (cnt <= Width)) do
    begin
      if Result[i] = ' ' then
        LastWordStart := i;
      Inc(cnt);
      Inc(i);
    end;
    if i <= Len then
    begin
      if LastWordStart > 0 then
      begin
        Delete(Result, LastWordStart, 1);
        Dec(Len, 1);
        i := LastWordStart;
      end;
      Insert(sLineBreak, Result, i);
      Inc(Len, BreakStrLen);
      Inc(i, BreakStrLen);
    end;
  end;
end;

function CompareFileAge(const Filename1Fmt: string; const Args1: array of const;
  const Filename2Fmt: string; const Args2: array of const): Integer;
begin
  Result := FileAge(Format(Filename1Fmt, Args1))
            -
            FileAge(Format(Filename2Fmt, Args2));
end;

function GetReturnPath(const Dir: string): string;
var
  i: Integer;
begin
  Result := '';
  if Dir <> '' then
  begin
    Result := '..';
    for i := 1 to Length(Dir) do
      if Dir[i] = PathDelim then
        Result := Result + PathDelim + '..';
  end;
end;

function FileExists(const Filename: string): Boolean;
var
  Attr: Cardinal;
begin
  Attr := GetFileAttributes(PChar(Filename));
  Result := (Attr <> $FFFFFFFF) and (Attr and FILE_ATTRIBUTE_DIRECTORY = 0);
end;

function DirectoryExists(const Dir: string): Boolean;
var
  Attr: Cardinal;
begin
  Attr := GetFileAttributes(PChar(Dir));
  Result := (Attr <> $FFFFFFFF) and (Attr and FILE_ATTRIBUTE_DIRECTORY <> 0);
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

function FindFilename(const Paths: string; const Filename: string): string;
var
  List: TStrings;
  i: Integer;
begin
  if Filename <> '' then
  begin
    List := TStringList.Create;
    try
      ConvertPathList(Paths, List);
      for i := 0 to List.Count - 1 do
      begin
        Result := List[i];
        if (Result <> '') and (Result[1] = '"') then
        begin
          Delete(Result, 1, 1);
          Delete(Result, Length(Result), 1);
        end;
        if Result <> '' then
        begin
          if Result[Length(Result)] <> PathDelim then
            Result := Result + PathDelim;
          if Filename[1] = PathDelim then
            Result := Result + Copy(Filename, 2, MaxInt)
          else
            Result := Result + Filename;
          if FileExists(Result) then
            Exit; // found
        end;
      end;
    finally
      List.Free;
    end;
  end;
  Result := Filename;
end;

function StartsWith(const Text, StartText: string; CaseInsensitive: Boolean = False): Boolean;
var
  Len, i: Integer;
begin
  Result := False;
  Len := Length(StartText);
  if Len > Length(Text) then
    Exit;
  if CaseInsensitive then
  begin
    for i := 1 to Len do
      if UpCase(Text[i]) <> UpCase(StartText[i]) then
        Exit;
  end
  else
  begin
    for i := 1 to Len do
      if Text[i] <> StartText[i] then
        Exit;
  end;
  Result := True;
end;

function EndsWith(const Text, EndText: string; CaseInsensitive: Boolean): Boolean;
var
  Len, i, x: Integer;
begin
  Result := False;
  Len := Length(EndText);
  x := Length(Text);
  if Len > x then
    Exit;
  if CaseInsensitive then
  begin
    for i := Len downto 1 do
      if UpCase(Text[x]) <> UpCase(EndText[i]) then
        Exit
      else
        Dec(x);
  end
  else
  begin
    for i := Len downto 1 do
      if Text[x] <> EndText[i] then
        Exit
      else
        Dec(x);
  end;
  Result := True;
end;

function SubStr(const Text: string; StartIndex, EndIndex: Integer): string;
begin
  Result := Copy(Text, StartIndex, EndIndex - StartIndex + 1);
end;

function HasText(Text: string; const Values: array of string): Boolean;
var
  i: Integer;
begin
  Result := True;
  Text := AnsiLowerCase(Text);
  for i := 0 to High(Values) do
    if Pos(Values[i], Text) > 0 then
      Exit;
  Result := False;
end;

procedure AddPaths(List: TStrings; Add: Boolean; const Dir: string;
  const Paths: array of string);
var
  i, j: Integer;
  Path: string;
begin
 // remove old paths
  for j := 0 to High(Paths) do
    for i := List.Count - 1 downto 0 do
      if Paths[j] <> '' then
      begin
        Path := Paths[j];
        if (Pos(':', Path) = 0) and (Path[1] <> '$') then
          Path := PathDelim + ExtractFileName(Dir) + PathDelim + Paths[j];
        if EndsWith(List[i], Path, True) then
          List.Delete(i)
        else if EndsWith(List[i], Path + '\', True) then
          List.Delete(i);
      end;

  if Add then
   // add new paths
    for j := 0 to High(Paths) do
      if Paths[j] <> '' then
      begin
        Path := Paths[j];
        if (Pos(':', Path) = 0) and (Path[1] <> '$') then
          List.Add(Dir + PathDelim + Path)
        else
          List.Add(Path);
      end;
end;

function OpenAtAnchor(const FileName, Anchor: string): Boolean;
var
  Cmd: string;
begin
  SetLength(Cmd, MAX_PATH);
  Result := FindExecutable(PChar(FileName), nil, PChar(Cmd)) > 32;
  SetLength(Cmd, StrLen(PChar(Cmd)));
  if Result then
    Result := ShellExecute(0, 'open', PChar(Cmd), PChar(FileName + '#' + Anchor), nil,
      SW_SHOWNORMAL) > 32;
end;

{$IFDEF COMPILER5}
function Supports(const Intf: IInterface; const IID: TGUID): Boolean; overload;
var
  TempIntf: IInterface;
begin
  Result := Supports(Intf, IID, TempIntf);
end;

function FileSetReadOnly(const FileName: string; ReadOnly: Boolean): Boolean;
var
  Attr: Cardinal;
begin
  Result := False;
  Attr := GetFileAttributes(PChar(FileName));
  if Attr = -1 then
    Exit;
  if ReadOnly then
    Attr := Attr or FILE_ATTRIBUTE_READONLY
  else
    Attr := Attr and not FILE_ATTRIBUTE_READONLY;
  SetFileAttributes(PChar(FileName), Attr);
end;
{$ENDIF COMPILER5}

function IsInArray(const Value: string; const Args: array of string): Integer;
begin
  for Result := 0 to High(Args) do
    if CompareText(Value, Args[Result]) = 0 then
      Exit;
  Result := -1;
end;

procedure FindFiles(const Dir, Mask: string; SubDirs: Boolean; List: TStrings;
  const FileExtensions: array of string);
var
  sr: TSearchRec;
begin
  if FindFirst(Dir + '\' + Mask, faAnyFile or faDirectory, sr) = 0 then
  try
    repeat
      if sr.Attr and faDirectory <> 0 then
      begin
        if (sr.Name <> '.') and (sr.Name <> '..') then
          if SubDirs then
            FindFiles(Dir + '\' + sr.Name, Mask, SubDirs, List, FileExtensions);
      end
      else
      begin
        if IsInArray(ExtractFileExt(sr.Name), FileExtensions) <> -1 then
          List.Add(Dir + '\' + sr.Name);
      end;
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
end;

end.
