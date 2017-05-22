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
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit Utils;

{$I jvcl.inc}

interface

uses
  Windows, ShellAPI, SysUtils, Classes, JvConsts;

function WordWrapString(const S: string; Width: Integer = 75): string;

function CompareFileAge(const Filename1Fmt: string; const Args1: array of const;
  const Filename2Fmt: string; const Args2: array of const): Integer;
function GetReturnPath(const Dir: string): string;
function FileExists(const Filename: string): Boolean;
function DirectoryExists(const Dir: string): Boolean;
function FileAgeEx(const Filename: string): Integer;
function Path(const APath: string): string; // converts each '\' to PathDelim
function HaveFilesChanged(Files: TStrings; StartIndex: Integer = 0): Boolean;

function FollowRelativeFilename(const RootDir: string; RelFilename: string): string;
function CutFirstDirectory(var Dir: string): string;

function StartsWith(const Text, StartText: string; CaseInsensitive: Boolean = False): Boolean;
function EndsWith(const Text, EndText: string; CaseInsensitive: Boolean): Boolean;
function SubStr(const Text: string; StartIndex, EndIndex: Integer): string;

function HasText(Text: string; const Values: array of string): Boolean; // case insensitive
procedure AddPaths(List: TStrings; Add: Boolean; const Dir: string; const Paths: array of string);
function OpenAtAnchor(const FileName, Anchor: string): Boolean;

procedure FindFiles(const Dir, Mask: string; SubDirs: Boolean; List: TStrings;
  const FileExtensions: array of string);

function FindFilename(const Paths: string; const Filename: string): string;
function ForceDirectoriesEx(const Directory: string): Boolean;

 { DirContainsFiles returns True if the directory Dir contains at least one file
   that matches Mask. }
function DirContainsFiles(const Dir, Mask: string): Boolean;

function PathListToStr(List: TStrings): string;
procedure StrToPathList(Paths: string; List: TStrings);

function RemoveInvalidPaths(const Paths: string): string;
function ConcatPaths(List: TStrings; const Separator: string): string;
function AppendPath(const APath, ASubDir: string): string;

function FixBackslashBackslash(const Dir: string): string;

procedure ClearEnvironment;
{ ClearEnvironment deletes almost all environment variables }

implementation

uses
  DelphiData;

procedure ClearEnvironment;
var
  EnvP, P, StartP: PChar;
  S: string;
begin
  EnvP := GetEnvironmentStrings;
  if EnvP <> nil then
  begin
    try
      P := EnvP;
      StartP := P;
      repeat
        while P^ <> #0 do
          Inc(P);
        if P^ = #0 then
        begin
          SetString(S, StartP, P - StartP);
          S := Copy(S, 1, Pos('=', S) - 1);
          if S <> '' then
          begin
            { Delete the environment variable }
            if not (
              SameText(S, 'TEMP') or  SameText(S, 'ComSpec') or SameText(S ,'OS') or
              SameText(S, 'PATHEXT') or SameText(S, 'windir') or SameText(S, 'SystemRoot') or
              SameText(S, 'SystemDrive') or
              SameText(S, 'INSTALLOPTIONS') or SameText(S, 'LANG')
              ) then
             SetEnvironmentVariable(PChar(S), nil);
          end;

          Inc(P);
          if P^ = #0 then
            Break; // finished

          StartP := P;
        end;
      until False;
    finally
      FreeEnvironmentStrings(EnvP);
    end;
  end;
end;

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
  Result := FileAgeEx(Format(Filename1Fmt, Args1))
            -
            FileAgeEx(Format(Filename2Fmt, Args2));
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

{$IFDEF COMPILER11_UP}
function FileAgeEx(const FileName: string): Integer;
begin
  {$WARNINGS OFF} // FileAge; deprecated
  Result := SysUtils.FileAge(Filename);
  {$WARNINGS ON}
end;

{$ELSE}
function GetFileAttributesExPreload(lpFileName: PChar; fInfoLevelId: TGetFileExInfoLevels;
  lpFileInformation: Pointer): BOOL; stdcall;
  forward;

var
  GetFileAttributesExFunc: function(lpFileName: PChar; fInfoLevelId: TGetFileExInfoLevels;
    lpFileInformation: Pointer): BOOL; stdcall = GetFileAttributesExPreload;

function GetFileAttributesExEmulated(lpFileName: PChar; fInfoLevelId: TGetFileExInfoLevels;
  lpFileInformation: Pointer): BOOL; stdcall;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Handle := FindFirstFile(lpFileName, FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if lpFileInformation <> nil then
    begin
      Move(FindData, lpFileInformation^, SizeOf(TWin32FileAttributeData));
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function GetFileAttributesExPreload(lpFileName: PChar; fInfoLevelId: TGetFileExInfoLevels;
  lpFileInformation: Pointer): BOOL; stdcall;
begin
  {$IFDEF UNICODE}
  GetFileAttributesExFunc := GetProcAddress(GetModuleHandle(kernel32), 'GetFileAttributesExW');
  {$ELSE}
  GetFileAttributesExFunc := GetProcAddress(GetModuleHandle(kernel32), 'GetFileAttributesExA');
  {$ENDIF UNICODE}
  if not Assigned(GetFileAttributesExFunc) then
    GetFileAttributesExFunc := GetFileAttributesExEmulated;
  Result := GetFileAttributesExFunc(lpFileName, fInfoLevelId, lpFileInformation);
end;

{ Faster replacement for the SysUtils.FileAge }
function FileAgeEx(const FileName: string): Integer;
var
  FindData: TWin32FileAttributeData;
  LocalFileTime: TFileTime;
begin
  if GetFileAttributesExFunc(Pointer(Filename), GetFileExInfoStandard, @FindData) then
  begin
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi, LongRec(Result).Lo) then
        Exit;
    end;
  end;
  Result := -1;
end;
{$ENDIF COMPILER11_UP}

function Path(const APath: string): string;
var
  i: Integer;
begin
  Result := APath;
  for i := 1 to Length(Result) do
    if Result[i] = '\' then
      Result[i] := PathDelim;
end;

function HaveFilesChanged(Files: TStrings; StartIndex: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := StartIndex to Files.Count - 1 do
    if FileAgeEx(Files[i]) <> Integer(Files.Objects[i]) then
      Exit;
  Result := False;
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

function ForceDirectoriesEx(const Directory: string): Boolean;
begin
  if Directory <> '' then
    Result := ForceDirectories(Directory)
  else
    Result := True;
end;

function DirContainsFiles(const Dir, Mask: string): Boolean;
var
  sr: TSearchRec;
begin
  Result := FindFirst(Dir + PathDelim + Mask, faAnyFile and not faDirectory, sr) = 0;
  if Result then
    FindClose(sr);
end;

function PathListToStr(List: TStrings): string;
var
  i: Integer;
begin
  Result := '';
  if List.Count > 0 then
    Result := List[0];
  for i := 1 to List.Count - 1 do
    Result := Result + ';' + List[i];
end;

procedure StrToPathList(Paths: string; List: TStrings);
var
  ps: Integer;
  S: string;
begin
  Assert(List <> nil);

  ps := Pos(';', Paths);
  while ps > 0 do
  begin
    S := Trim(Copy(Paths, 1, ps - 1));
    if (Length(S) > 1) and (S[1] = '"') and (S[Length(S)] = '"') then
      S := Copy(S, 2, Length(S) - 2);
    if S <> '' then
      List.Add(S);
    Delete(Paths, 1, ps);
    ps := Pos(';', Paths);
  end;
end;

function RemoveInvalidPaths(const Paths: string): string;
var
  List: TStrings;
  I: Integer;
begin
  List := TStringList.Create;
  try
    StrToPathList(Paths, List);
    for I := List.Count - 1 downto 0 do
      if not DirectoryExists(List[I]) or (List.IndexOf(List[I]) <> I) then
        List.Delete(I);
    Result := PathListToStr(List);
  finally
    List.Free;
  end;
end;

function ConcatPaths(List: TStrings; const Separator: string): string;
var
  i: Integer;
  S: string;
begin
  Result := '';
  for i := 0 to List.Count - 1 do
  begin
    S := List[i];
    if Pos(' ', S) > 0 then
      S := '"' + S + '"';
    if i = 0 then
      Result := S
    else
      Result := Result + Separator + S;
  end;
end;

function AppendPath(const APath, ASubDir: string): string;
begin
  if ASubDir <> '' then
    Result := IncludeTrailingPathDelimiter(APath) + ASubDir
  else
    Result := ExcludeTrailingPathDelimiter(APath);
end;

function FixBackslashBackslash(const Dir: string): string;
var
  I: Integer;
begin
  Result := Dir;
  I := Length(Result) - 1;
  while I > 1 do
  begin
    if (Result[I] = PathDelim) and (Result[I + 1] = PathDelim) then
      Delete(Result, I, 1);
    Dec(I);
  end;
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
  if Dir = '' then
  begin
    FindFiles('.', Mask, SubDirs, List, FileExtensions);
    Exit;
  end;

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
        if (Length(FileExtensions) = 0) or (IsInArray(ExtractFileExt(sr.Name), FileExtensions) <> -1) then
          List.AddObject(Dir + '\' + sr.Name, TObject(sr.Size));
      end;
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
end;

end.