(****************************************************************************
 * WANT - A build management tool.                                          *
 * Copyright (c) 2001-2003 Juancarlo Anez, Caracas, Venezuela.              *
 * All rights reserved.                                                     *
 *                                                                          *
 * This library is free software; you can redistribute it and/or            *
 * modify it under the terms of the GNU Lesser General Public               *
 * License as published by the Free Software Foundation; either             *
 * version 2.1 of the License, or (at your option) any later version.       *
 *                                                                          *
 * This library is distributed in the hope that it will be useful,          *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        *
 * Lesser General Public License for more details.                          *
 *                                                                          *
 * You should have received a copy of the GNU Lesser General Public         *
 * License along with this library; if not, write to the Free Software      *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA *
 ****************************************************************************)
{
    @brief 

    @author Juanco Añez
}

unit WildPaths;
{:
@BUG:  SplitPath is not consisten with PathConcat. The output of the first
       can not be put back together with the second. This is a problem for
       UNC paths.

@TODO: File-by-file matching currently relies too much on the OS, so it may
       not work consistently in Windows and Linux.

       To make it consistent:
         * Allways retreive lists of files with an '*' pattern
         * Filter the resulting list using IsMatch and our own pattern.

       Besides consistency, our own matching would allow us to use extended,
       Unix-like file matching ([-] ranges, etc.) on Windows.

       Do not make these changes until specific user stories prove them
       necessary.

@TODO: Factor out stuff that JCL already handles well.
}

interface

uses
  Windows,
  SysUtils,
  Math,
  Classes,

  JclFileUtils,

  WantUtils;

const
  WildChars        = '?*';
  InvalidPathChars = string(';' {$IFDEF LINUX} + ':' {$ENDIF});

  {$IFDEF LINUX}
  SystemPathDelimiter: string = '/';
  {$ELSE}
  SystemPathDelimiter: string = '\';
  {$ENDIF}

type
  TPath  = type string;
  TPattern  = TPath;
  TPaths = array of TPath;
  TPatterns = TPaths;

  TSystemPath  = string;
  TSystemPaths = array of TSystemPath;

  EPathException = class(Exception);
  EFileOpException    = class(EPathException);

  TFileAttribute = (
    ReadOnly,  {= $00000001 }
    Hidden,    {= $00000002 }
    SysFile,   {= $00000004 }
    VolumeID,  {= $00000008 }
    Directory, {= $00000010 }
    Archive,   {= $00000020 }
    NoFile     {= -1}
  );
  TFileAttributes = set of TFileAttribute;

const
  AnyFileAttribute = [];

function  IsLocalPath(const Path: TPath): boolean;
procedure AssertIsLocalPath(const Path: TPath);

function IsWindowsPath(const Path: TPath):boolean;

function PathDrive(const Path: TPath): string;
function PathServer(Path: TPath): string;
function PathFile(const Path: TPath): TPath;
function PathDir(const Path: TPath): TPath;

function RemovePathDrive(Path: TPath):TPath;
function RemovePathServer(Path: TPath):TPath;

function PathConcat(Path1, Path2: TPath): TPath;

function ToSystemPath(const Path: TPath; const BasePath: TPath = ''):TSystemPath;
function ToPath(SystemPath: TSystemPath;  const BasePath: TPath = ''):TPath;

function ToSystemPaths(const Paths: TPaths; const BasePath: TPath = ''): TSystemPaths; overload;
function ToPaths(OSPaths: TSystemPaths; const BasePath: TPath = ''): TPaths;

procedure ToSystemPaths(Paths: TStrings; const BasePath: TPath = ''); overload;


function StringsToPaths(S: TStrings):TPaths;
function SplitPath(Path: TPath): TPaths;
function JoinPaths(Paths: TPaths): TPath;

function  MovePath(Path, FromBase: TPath; ToBase: TPath = ''): TPath;
function  MovePaths(const Paths: TPaths; const FromBase: TPath; const ToBase: TPath = ''): TPaths;

function  ToRelativePath(Path, BasePath: TPath):TPath;
function  ToRelativePaths(const Paths: TPaths; const BasePath: TPath):TPaths; overload;
function  ToRelativePaths(Paths: TStrings; const BasePath: TPath):TPaths; overload;
function  NormalizePath(Path :TPath) :TPath;

function  PathIsAbsolute(Path: TPath): boolean;
procedure ForceRelativePath(var Path, BasePath: TPath);

function FindPaths(Path: TPath; BasePath: TPath = '';
                   IncludeAttr: TFileAttributes = AnyFileAttribute;
                   ExcludeAttr: TFileAttributes = []
                   ): TPaths;
function FindFiles(const Path: TPath; const BasePath: TPath = ''): TPaths;
function FindDirs(const  Path: TPath; const BasePath: TPath  = ''): TPaths;

function  Wild(    Pattern: TPath; BasePath: TPath = '';
                   IncludeAttr: TFileAttributes = AnyFileAttribute;
                   ExcludeAttr: TFileAttributes = []
                ):TPaths; overload;
procedure Wild(Files: TStrings; Pattern: TPath; BasePath: TPath = '';
                   IncludeAttr: TFileAttributes = AnyFileAttribute;
                   ExcludeAttr: TFileAttributes = []
          ); overload;


function IsMatch(Pattern, Path: TPath):boolean; overload;
function IsMatch(const Patterns: TPatterns; const Paths: TPaths; s: Integer = 0; p: Integer = 0):boolean; overload;

function  PathExists(Path: TPath):boolean;
function  PathIsDir(Path: TPath):boolean;
function  PathIsFile(Path: TPath):boolean;
function  SuperPath(Path: TPath): TPath;


// file operations

procedure MakeDir(const Path: TPath);
function  ChangeDir(const Path: TPath; Verify :boolean = true) :boolean;

function  CurrentDir: TPath;
function  CurrentDrive :TPath;

procedure CopyFile(const Src, Dst: TPath);
procedure CopyFiles(const Sources, Dests: TPaths);  overload;
procedure CopyFiles(const Files: TPaths; FromPath, ToPath: TPath);  overload;
procedure CopyFiles(const Pattern: TPattern; const FromPath, ToPath: TPath); overload;

procedure MoveFile(const Src, Dst: TPath);
procedure MoveFiles(const Sources, Dests: TPaths);  overload;
procedure MoveFiles(const Files: TPaths; const FromPath, ToPath: TPath);  overload;
procedure MoveFiles(const Pattern: TPattern; const FromPath, ToPath: TPath); overload;

procedure DeleteFile(const Path: TPath; DeleteReadOnly: boolean = false);
procedure DeleteFiles(const Files: TPaths; DeleteReadOnly: boolean = false);  overload;
procedure DeleteFiles(const Pattern: TPath; const BasePath: TPath= ''; DeleteReadOnly: boolean = false);  overload;

procedure TouchFile(const Path: TPath; When: TDateTime = 0); overload;
procedure TouchFile(const Path: TPath; When: string); overload;

function  FileAttributes(const Path: TPath):TFileAttributes;
procedure SetFileAttributes(const Path: TPath; const Attr: TFileAttributes);

function  FileTime(const Path: TPath): TDateTime;

function  SystemFileAttributes(const Path: TPath): Integer;
function  SystemFileTime(const Path: TPath): Longint;

function  TimeToSystemFileTime(const Time: TDateTime):Integer;
function  FileAttributesToSystemAttributes(const Attr: TFileAttributes):Byte;
function  SystemAttributesToFileAttributes(Attr: Integer) :TFileAttributes;
function  ChangeExtension( Path: TPath; extension : String ): TPath;

implementation


procedure Wild( Files: TStrings; const Patterns: TPatterns; BasePath: TPath = '';
                Index: Integer = 0;
                IncludeAttr: TFileAttributes = AnyFileAttribute;
                ExcludeAttr: TFileAttributes = []
                );
  overload; forward;


function IsLocalPath(const Path: TPath): boolean;
begin
  Result := ( Pos(SystemPathDelimiter, Path) = 0 );
end;

function IsWindowsPath(const Path: TPath):boolean;
begin
  Result := Pos(':', Path) <> 0;
end;

function CurrentDrive :TPath;
begin
  Result := ToPath(ExtractFileDrive(GetCurrentDir));
end;

function PathDrive(const Path: TPath): string;
var
  p: Integer;
begin
  if not IsWindowsPath(Path) then
    Result := ''
  else
  begin
    p := Pos(':', Path);
    Result := StrLeft(Path, p);
  end;
end;

function PathServer(Path: TPath): string;
var
  P: string;
begin
  Path := ToPath(Path);
  if StrLeft(Path, 2) <> '//' then
    Result := PathDrive(Path)
  else
  begin
    P := Copy(Path, 3, Length(Path));
    Result := '//'+ StrToken(P, '/');
  end;
end;

function PathFile(const Path: TPath): TPath;
var
  splits :TPaths;
begin
  Result := '';
  splits := SplitPath(Path);
  if Length(splits) > 0 then
    Result := splits[High(splits)];
end;

function PathDir(const Path: TPath): TPath;
var
  splits :TPaths;
begin
  Result := '';
  splits := SplitPath(Path);
  if Length(splits) > 0 then
  begin
    SetLength(splits, Length(splits)-1);
    Result := JoinPaths(splits); 
  end;
end;

function RemovePathDrive(Path: TPath):TPath;
var
  p: Integer;
begin
  Result := Path;
  p := Pos(':', Result);
  Delete(Result, 1, p);
end;

function RemovePathServer(Path: TPath):TPath;
var
  Server: string;
begin
  Result := Path;
  Server := PathServer(Path);
  if Server <> '' then
    Delete(Result, 1, 3 + Length(Server));
end;


procedure AssertIsLocalPath(const Path: TPath);
begin
  if Pos(SystemPathDelimiter, Path) <> 0 then
    raise EPathException.Create( Format( '"%s" looks like a system path. Expected a system independent one.',
                                  [Path])
                            );
end;

function PathConcat(Path1, Path2: TPath): TPath;
var
  Parts: TPaths;
  i    : Integer;
  P1   : TPath;
  P2   : TPath;
begin
  Path1 := ToPath(Path1);
  Path2 := ToPath(Path2);

  Parts := nil;
  if (Length(Path1) = 0)
  //or (P1 = '.')
  or PathIsAbsolute(Path2) then
    Result := Path2
  else if Length(Path2) = 0 then
    Result := Path1
  else begin
    P1 := Path1;
    P2 := Path2;
    if P1[Length(P1)] = '/' then
      Delete(P1, Length(P1), 1);
    Result := P1;
    Parts := SplitPath(P2);
    for i := Low(Parts) to High(Parts) do
    begin
      if Parts[i] = '..' then
        Result := SuperPath(Result)
      else if Parts[i] = '.' then
        // do nothing
      else
        Result := Result + '/' + Parts[i];
    end;
  end;
  Assert(Pos('//', Result) <= 1);
end;

procedure CheckPath(Path :TPath);
var
  i :Integer;
begin
  for i := 1 to Length(InvalidPathChars) do
  begin
    if Pos(InvalidPathChars[i], Path) <> 0 then
      raise EPathException.Create('invalid path chars in ' + Path);
  end;
end;

function ToPath(SystemPath: TSystemPath; const BasePath: TPath): TPath;
begin
  CheckPath(SystemPath);
  CheckPath(BasePath);

  Result := SystemPath;
  if (Length(Result) >= 2)
  and (Result[2] = ':')
  and (Result[1] in ['a'..'z', 'A'..'Z'])
  then
  begin
    Result[1] := LowerCase(''+Result[1])[1];
    Result := SystemPathDelimiter + Result;
  end;
  Result := StringReplace(Result, SystemPathDelimiter, '/', [rfReplaceAll]);
  if (BasePath <> '') then
  begin
    if (Length(Result) = 0) then
      Result := ToPath(BasePath)
    else if Result[1] <> '/'  then
    begin
      Result := ToPath(BasePath) + '/' + Result;
    end;
  end;
end;

function ToSystemPath(const Path: TPath; const BasePath: TPath): string;
begin
   CheckPath(Path);
   CheckPath(BasePath);

   Result := MovePath(Path, '', BasePath);
   if (Length(Result) >= 3) and (Result[3] = ':') and (Result[1] = '/') then
     Delete(Result,1, 1);
   if (Length(Result) >= 1) and (Result[Length(Result)] = '/') then
     Delete(Result,Length(Result), 1);
   Result := StringReplace(Result, '/', SystemPathDelimiter, [rfReplaceAll]);
   if PathIsAbsolute(Path) then
     Result := ExpandFileName(Result);
end;

function ToSystemPaths(const Paths: TPaths; const BasePath: TPath = ''): TSystemPaths;
var
  i: Integer;
begin
  SetLength(Result, Length(Paths));
  for i := 0 to High(Result) do
    Result[i] := ToSystemPath(Paths[i], BasePath);
end;

procedure ToSystemPaths(Paths: TStrings; const BasePath: TPath = '');
var
  i: Integer;
begin
  for i := 0 to Paths.Count-1 do
    Paths[i] := ToSystemPath(Paths[i], BasePath);
end;



function ToPaths(OSPaths: TSystemPaths; const BasePath: TPath = ''): TPaths;
var
  i: Integer;
begin
  SetLength(Result, Length(OSPaths));
  for i := 0 to High(Result) do
    Result[i] := ToPath(OSPaths[i], BasePath);
end;


function FindPaths( Path: TPath; BasePath: TPath;
                    IncludeAttr: TFileAttributes;
                    ExcludeAttr: TFileAttributes
                    ):TPaths;
var
  S: TStringList;
  Search: TSearchRec;
  SearchResult: Integer;
  Pattern :TPath;
  Dir     :TPath;
  Lookup  :TPath;
begin
  Path    := ToPath(Path);
  Dir     := PathDir(Path);
  Pattern := PathFile(Path);

  if (Pos('*', Pattern) > 0) or (Pos('?', Pattern) > 0)then
    Lookup := ToSystemPath(PathConcat(Dir, '*'), BasePath)
  else
    Lookup := ToSystemPath(Path, BasePath);

  S := TStringList.Create;
  S.Sorted := True;
  try
    SearchResult := FindFirst(Lookup, faAnyFile, Search);
    try
      while SearchResult = 0 do
      begin
        if  (Search.Name <> '.' )
        and (Search.Name <> '..' )
        and ((IncludeAttr = AnyFileAttribute) or ((SystemAttributesToFileAttributes(Search.Attr) * IncludeAttr) <> []))
        and ((SystemAttributesToFileAttributes(Search.Attr) * ExcludeAttr) =  [])
        and IsMatch(Pattern, Search.Name)
        then
          S.Add(ToPath(Search.Name, BasePath));
        SearchResult := FindNext(Search);
      end;
    finally
      FindClose(Search);
    end;
    Result := StringsToPaths(S);
  finally
    FreeAndNil(S);
  end;
end;

function FindDirs(const Path: TPath; const BasePath: TPath): TPaths;
begin
   Result := FindPaths(Path, BasePath, [Directory]);
end;

function FindFiles(const Path: TPath; const BasePath: TPath): TPaths;
begin
   Result := FindPaths(Path, BasePath, AnyFileAttribute, [Directory]);
end;

function SplitPath(Path: TPath): TPaths;
var
  S   : TStrings;
  n,
  i   : Integer;
  Base: TPath;
begin
  Path := ToPath(Path);

  S := TStringList.Create;
  try
    n := 0;
    if PathIsAbsolute(Path) then
    begin
      ForceRelativePath(Path, Base);
      SetLength(Result, 1);
      Result[0] := Base;
      n := 1;
    end;

    StrToStrings(Path, '/', S);

    SetLength(Result, n+S.Count);
    for i := 0 to S.Count-1 do
      Result[i+n] := S[i];
  finally
    FreeAndNil(S);
  end;
end;

function JoinPaths(Paths: TPaths): TPath;
var
 i :Integer;
begin
  if Length(Paths) = 0 then
    Result := ''
  else
  begin
    Result := Paths[0];
    for i := 1 to High(Paths) do
      Result := PathConcat(Result, Paths[i]);
  end;
end;


function StringsToPaths(S: TStrings): TPaths;
var
  i: Integer;
begin
  SetLength(Result, S.Count);
  for i := 0 to S.Count-1 do
    Result[i] := S[i];
end;

function MovePath(Path, FromBase, ToBase: TPath): TPath;
begin
  Path     := ToPath(Path);
  FromBase := ToPath(FromBase);
  ToBase   := ToPath(ToBase);

  if (FromBase <> '') and (Pos(FromBase+'/', Path) = 1) then
    Result := PathConcat(ToBase, Copy(Path, 2+Length(FromBase), Length(Path)))
  else if PathIsAbsolute(Path) then
    Result :=  Path
  else
    Result := PathConcat(ToBase, Path);
end;


function MovePaths(const Paths: TPaths; const FromBase: TPath; const ToBase: TPath): TPaths;
var
  i: Integer;
begin
   SetLength(Result, Length(Paths));
   for i := Low(Paths) to High(Paths) do
     Result[i] := MovePath(Paths[i], FromBase, ToBase);
end;

function NormalizePath(Path :TPath) :TPath;
begin
  Result := ToPath(Path);
  if not PathIsAbsolute(Path) then
    Result := PathConcat(CurrentDir, Result);
  if PathDrive(Result) = '' then
    Result := CurrentDrive + Result;
end;


function  ToRelativePath(Path, BasePath: TPath):TPath;
var
  P, B  : TPaths;
  i, j: Integer;
begin
  P := nil;
  B := nil;
  Path     := PathConcat('.', ToPath(Path));
  if not PathIsAbsolute(Path)
  and PathIsAbsolute(BasePath) then
    Result := Path
  else
  begin
    BasePath := ToPath(BasePath);

    if (PathDrive(Path) <> PathDrive(BasePath)) then
    begin
      Path     := NormalizePath(Path);
      BasePath := NormalizePath(BasePath);
    end;

    if (PathDrive(Path) <> PathDrive(BasePath)) then
      Result := Path
    else
    begin
      Result := '';

      P := SplitPath(Path);
      B := SplitPath(BasePath);
      i := 0;
      j := 0;
      while (i <= High(P))
      and   (j <= High(B))
      and   (P[i] = B[j])
      do begin
        Inc(i);
        Inc(j);
      end;

      if j > High(B) then
        Result := '.'
      else
      begin
        while  j <= High(B) do
        begin
          if Result = '' then
            Result := '..'
          else
            Result := Result + '/..';
          Inc(j);
        end;
      end;
      while i <= High(P) do
      begin
        Result := PathConcat(Result, P[i]);
        Inc(i);
      end;
    end;

    if Result = '' then
      Result := '.';
  end;
end;

function  ToRelativePaths(const Paths: TPaths; const BasePath: TPath):TPaths;
begin
  Result := MovePaths(Paths, BasePath, '');
end;

function  ToRelativePaths(Paths: TStrings; const BasePath: TPath):TPaths;
var
  i :Integer;
begin
  for i := 0 to Paths.Count-1 do
    Paths[i] := ToRelativePath(Paths[i], BasePath);
end;

function  PathIsAbsolute(Path: TPath): boolean;
begin
  Path := ToPath(Path);

  Result :=    (Length(Path) > 0) and (Path[1] = '/')
             or (Length(Path) >= 3) and (Path[2] = ':') and (Path[3] = '/');
end;

procedure ForceRelativePath(var Path, BasePath: TPath);
var
  p: Integer;
begin
  Path := ToPath(Path);
  BasePath := ToPath(BasePath);

  if PathIsAbsolute(Path) then
  begin
    BasePath := '';
    p := Pos('/', Path);
    BasePath := BasePath + Copy(Path, 1, p);
    Delete(Path, 1, p);
    if PathIsAbsolute(Path) then
    begin // must be UNC, URI, or C:/ style
      p := 1+Pos('/', Copy(Path, 2, Length(Path)));
      if p = 1 then
      begin
        BasePath := BasePath + Path;
        Path := '';
      end
      else
      begin
        BasePath := BasePath + Copy(Path, 1, p-1);
        Delete(Path, 1, p);
      end;
    end;
  end;
end;

function Wild(      Pattern: TPath; BasePath: TPath;
                    IncludeAttr: TFileAttributes;
                    ExcludeAttr: TFileAttributes
                    ):TPaths;
var
  Files: TStringList;
begin
  Pattern  := ToPath(Pattern);
  BasePath := ToPath(BasePath);

  Files := TStringList.Create;
  try
    Files.Sorted := True;
    Wild(Files, Pattern, BasePath, IncludeAttr, ExcludeAttr);
    Result := StringsToPaths(Files);
  finally
    FreeAndNil(Files);
  end;
end;

procedure Wild(Files: TStrings; Pattern: TPath; BasePath: TPath;
               IncludeAttr: TFileAttributes;
               ExcludeAttr: TFileAttributes
               );
var
  Pats: string;
  Pat : TPath;
begin
  Pattern  := ToPath(Pattern);
  BasePath := ToPath(BasePath);

  Pats := Pattern;
  Pat  := StrToken(Pats, ',');
  while Pat <> '' do
  begin
    //ForceRelativePath(Pattern, BasePath);
    if PathIsAbsolute(Pattern) then
      Wild(Files, SplitPath(Pat), '',       0, IncludeAttr, ExcludeAttr)
    else
      Wild(Files, SplitPath(Pat), BasePath, 0, IncludeAttr, ExcludeAttr);
    Pat  := StrToken(Pats, ',');
  end;
end;

procedure Wild( Files: TStrings; const Patterns: TPatterns; BasePath: TPath;
                Index: Integer;
                IncludeAttr: TFileAttributes;
                ExcludeAttr: TFileAttributes
                );
var
  i      : Integer;
  Matches: TPaths;
  NewBase: TPath;
begin
  BasePath := ToPath(BasePath);
  Matches := nil;

  if Index > High(Patterns) then
    EXIT;

  NewBase := BasePath;
  // absorb all non-wildcard patterns
  while (Index < High(Patterns))
  and ((LastDelimiter(WildChars, Patterns[Index]) = 0)
       or (Patterns[Index] = '.')
      )
  do
  begin
    NewBase := PathConcat(NewBase, Patterns[Index]);
    Inc(Index);
  end;
  Assert(Index <= High(Patterns));
  if Index = High(Patterns) then
  begin // add files (works for '**' too)
    Matches := FindPaths(Patterns[Index], NewBase, IncludeAttr, ExcludeAttr);
    for i := Low(Matches) to High(Matches) do
      Files.Add(PathConcat(NewBase, Matches[i]))
  end;
  // handle wildcards
  if Patterns[Index] = '**' then
  begin // match anything and recurse
    Wild(Files, Patterns, NewBase, Index+1, IncludeAttr, ExcludeAttr);
    Matches := FindDirs('*', NewBase);
    // use same Index ('**') to recurse
    for i := Low(Matches) to High(Matches) do
      Wild(Files, Patterns, PathConcat(NewBase, Matches[i]), Index, IncludeAttr, ExcludeAttr);
  end
  else if Index < High(Patterns) then
  begin // match directories
    Matches := FindDirs(Patterns[Index], NewBase);
    for i := Low(Matches) to High(Matches) do
      Wild(Files, Patterns, PathConcat(NewBase, Matches[i]), Index+1, IncludeAttr, ExcludeAttr);
  end;
end;

function Matches(A, B: TPath; i: Integer = 1; j: Integer = 1):boolean;
begin
  while (i <= Length(A))
  and   (j <= Length(B))
  and   (UpCase(A[i]) = UpCase(B[j])) do
  begin
    Inc(i);
    Inc(j);
  end;
  if B = '.' then
    Result := True
  else if j > Length(B) then
    Result := i > Length(A)
  else if i > Length(A) then
    Result := False
  else if B[j] = '?' then
    Result :=    Matches(A, B, i+1, j+1)
              or Matches(A, B, i,   j+1)
  else if B[j] = '*' then
    Result :=    Matches(A, B, i+1, j+1)
              or Matches(A, B, i+1, j)
              or Matches(A, B, i  , j+1)
  else
    Result := False;
end;

function IsMatch(const Patterns: TPatterns; const Paths: TPaths; s: Integer = 0; p: Integer = 0):boolean;
begin
  while (p <= High(Paths))
  and   (s <= High(Patterns))
  and   (Patterns[s] <> '**')
  and   Matches(Paths[p], Patterns[s]) do
  begin
    Inc(p);
    Inc(s);
  end;
  if s > High(Patterns) then
    Result := p > High(Paths)
  else if Patterns[s] = '**' then
  begin
    if p > High(Paths) then
      Result := s = High(Patterns)
    else
      Result :=    IsMatch(Patterns, Paths, s+1,   p)
                or IsMatch(Patterns, Paths, s+1, p+1)
                or IsMatch(Patterns, Paths, s,   p+1)
  end
  else
    Result := False;
end;

function IsMatch(Pattern, Path : TPath):boolean;
begin
  Pattern  := ToPath(Pattern);
  Path     := ToPath(Path);

  Result := IsMatch(SplitPath(Pattern), SplitPath(Path));
end;

function  PathExists(Path: TPath):boolean;
begin
  Path     := ToPath(Path);

  Result := Length(FindPaths(Path)) >= 1;
end;

function  PathIsDir(Path: TPath):boolean;
begin
  Path     := ToPath(Path);

  Result := (FileAttributes(Path) * [NoFile, Directory]) = [Directory];
end;

function  PathIsFile(Path: TPath):boolean;
begin
  Path     := ToPath(Path);
  Result := (FileAttributes(Path) * [NoFile, Directory]) = [];
end;

function  SuperPath(Path: TPath): TPath;
var
  p   : Integer;
  f   : string;
begin
  Path     := ToPath(Path);

  if (Path = '.') or (Path = '') then
    Result := '..'
  else if Path = '..' then
    Result := '../..'
  else
  begin
    Result := Path;
    p := LastDelimiter('/', Result);
    f := Copy(Result, p+1, Length(Result));
    if (p = Length(Result)) or (f = '.') then
    begin
      Result := Copy(Result, 1, p-1);
      p := LastDelimiter('/', Result);
      f := Copy(Result, p+1, Length(Result))
    end;
    if  f = '..' then
      Result := Result + '/..'
    else
      Result := Copy(Result, 1, p-1);
  end;
end;



// file operations


procedure MakeDir(const Path: TPath);
begin
  if (Length(Path) > 0)
  and (Path[Length(Path)] <> ':')  // Oops! Windows specific!
  and not PathIsDir(Path) then
  begin
    MakeDir(SuperPath(Path));
    SysUtils.CreateDir(ToSystemPath(Path));
    if not PathIsDir(Path) then
      raise EFileOpException.Create(Format('Could not create directory "%s"', [Path]));
  end;
end;

function ChangeDir(const Path: TPath; Verify :boolean) :boolean;
begin
  Result := True;
  if (Path <> '') and (Path <> CurrentDir) then
    Result := SetCurrentDir(ToSystemPath(Path));
    if not Result and Verify then
      raise EFileOpException.CreateFmt('Could not change to directory "%s"',[Path]);
end;

function  CurrentDir: TPath;
begin
  Result := ToPath(SysUtils.GetCurrentDir);
end;


procedure CopyFile(const Src, Dst: TPath);
begin
   MakeDir(SuperPath(Dst));
   if PathIsDir(Src) then
     MakeDir(Dst)
   else if not Windows.CopyFile( PChar(ToSystemPath(Src)),
                                 PChar(ToSystemPath(Dst)),
                                 False)
     then
       raise EFileOpException.Create(SysErrorMessage(GetLastError));
end;

procedure CopyFiles(const Pattern: TPattern; const FromPath, ToPath: TPath);
begin
  CopyFiles(Wild(Pattern, FromPath), FromPath, ToPath);
end;

procedure CopyFiles(const Files: TPaths; FromPath, ToPath: TPath);
begin
  CopyFiles(Files, MovePaths(Files, FromPath, ToPath));
end;

procedure CopyFiles(const Sources, Dests: TPaths);
var
  f  : Integer;
begin
  for f := Max(Low(Sources), Low(Dests)) to Min(High(Sources), High(Dests)) do
  begin
    CopyFile(Sources[f], Dests[f]);
  end;
end;

procedure MoveFile(const Src, Dst: TPath);
begin
  if PathIsDir(Src) then
  begin
    raise EFileOpException.Create(Format('Don''t know how to move dir "%s" to "%s"', [Src, Dst]));
  end;

  (* MoveFileEx not implemented on Win9x. Some help files say it is, but see
     this link for current doc:
       http://msdn.microsoft.com/library/psdk/winbase/filesio_9oe0.htm

     In addition, MOVEFILE_REPLACE_EXISTING does not work with read only files
  if not Windows.MoveFileEx(PChar(ToSystemPath(Src)), PChar(ToSystemPath(Dst)),
                            MOVEFILE_COPY_ALLOWED or
                            MOVEFILE_REPLACE_EXISTING or
                            MOVEFILE_WRITE_THROUGH)  -- Chrismo *)

  WildPaths.CopyFile(Src, Dst);
  WildPaths.DeleteFile(Src);
end;

procedure MoveFiles(const Pattern: TPattern; const FromPath, ToPath: TPath);
begin
  MoveFiles(Wild(Pattern, FromPath), FromPath, ToPath);
end;

procedure MoveFiles(const Files: TPaths; const FromPath, ToPath: TPath);
begin
  MoveFiles(Files, MovePaths(Files, FromPath, ToPath));
end;

procedure MoveFiles(const Sources, Dests: TPaths);
var
  f  : Integer;
begin
  for f := Max(Low(Sources), Low(Dests)) to Min(High(Sources), High(Dests)) do
  begin
    MoveFile(Sources[f], Dests[f]);
  end;
end;

procedure DeleteFile(const Path: TPath; DeleteReadOnly: boolean = false);
var
  SysPath: string;
  FileAttr: Integer;
begin
  SysPath := ToSystemPath(Path);
  if not PathIsDir(Path) then
  begin
    if DeleteReadOnly then
    begin
      { take off read only attribute if it exists }
      FileAttr := SysUtils.FileGetAttr(SysPath);
      FileAttr := FileAttr and (not $00000001); // faReadOnly. Avoid warning.
      SysUtils.FileSetAttr(SysPath, FileAttr);
    end;
    SysUtils.DeleteFile(SysPath)
  end
  else
    SysUtils.RemoveDir(SysPath);
end;

procedure DeleteFiles(const Pattern: TPath; const BasePath: TPath; DeleteReadOnly :boolean);
begin
  DeleteFiles(Wild(Pattern, BasePath), DeleteReadOnly);
end;

procedure DeleteFiles(const Files: TPaths; DeleteReadOnly: boolean = false);
var
  f: Integer;
begin
  for f := Low(Files) to High(Files) do
    if not PathIsDir(Files[f]) then
      DeleteFile(Files[f], DeleteReadOnly);
  for f := Low(Files) to High(Files) do
    if PathIsDir(Files[f]) then
      DeleteFile(Files[f], DeleteReadOnly);
end;

procedure TouchFile(const Path: TPath; When: string);
begin
  //!!! StrToDateTime changes with locale and platform!!
  TouchFile(Path, StrToDateTime(When));
end;


procedure TouchFile(const Path: TPath; When: TDateTime);
var
  Handle: Integer;
begin
   if When = 0 then
     When := Now;

   MakeDir(SuperPath(Path));
   
   Handle := FileOpen(ToSystemPath(Path), fmOpenWrite or fmShareDenyNone);
   if Handle < 0 then
     Handle := FileCreate(ToSystemPath(Path));
   try
     FileSetDate(Handle, DateTimeToFileDate(When))
   finally
     FileClose(Handle);
   end;
end;

function FileAttributes(const Path: TPath):TFileAttributes;
var
  Attr :Integer;
begin
  Attr := SystemFileAttributes(Path);
  if Attr < 0 then
    Result := [NoFile]
  else
    Result := TFileAttributes(Byte(Attr));
end;

procedure SetFileAttributes(const Path: TPath; const Attr: TFileAttributes);
begin
  SysUtils.FileSetAttr(ToSystemPath(Path), FileAttributesToSystemAttributes(Attr));
end;

function  FileTime(const Path: TPath): TDateTime;
var
  SystemTime: Longint;
begin
  SystemTime := SystemFileTime(Path);
  if SystemTime <= 0 then
    Result := 0
  else
    Result := FileDateToDateTime(SystemTime);
end;

function  SystemFileAttributes(const Path: TPath): Integer;
begin
  Result := Byte(SysUtils.FileGetAttr(ToSystemPath(Path)));
end;

function  SystemFileTime(const Path: TPath)    : Longint;
begin
  Result := SysUtils.FileAge(ToSystemPath(Path));
  if Result < 0 then
    Result := 0;
end;

function  TimeToSystemFileTime(const Time: TDateTime):Integer;
begin
  Result := DateTimeToFileDate(Time);
end;

function  FileAttributesToSystemAttributes(const Attr: TFileAttributes):Byte;
begin
  Result := Byte(Attr);
end;

function  SystemAttributesToFileAttributes(Attr: Integer) :TFileAttributes;
begin
  Result := TFileAttributes(Byte(Attr));
end;

function ChangeExtension( Path: TPath; extension : String ): TPath;
var
  p : integer ;
begin
  p:=LastDelimiter('.',Path);
  if (p < Length(Path)) then
  begin
    Result:=copy(Path,1,p)+extension;
  end
  else Result:=Path
end;

end.
