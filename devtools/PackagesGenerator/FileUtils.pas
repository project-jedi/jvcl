unit FileUtils;

{$I jvcl.inc}

interface

uses
  Windows;

{$IFNDEF COMPILER6_UP}
 {$DEFINE MSWINDOWS}
{$ENDIF COMPILER6_UP}

// returns the relative path between Origin and Destination
// This is the string you would have to type after the 'cd'
// command is you were located in Origin and willing to change
// to Destination
// Both Origin and Destination should be absolute path but this
// is not verified by the function.
// If Origin and Destination are on different partitions,
// the function returns Destination
function GetRelativePath(const Origin, Destination : string) : string;

// returns the given path without any relative instructions inside
// for instance, if you pass c:\a\b\..\c\.\d, it returns c:\a\c\d
// Note that if you pass ..\..\a\b it doesn't remove the heading
// relative paths instructions.
// If there are more ..\ than paths in front of it, the result
// is undefined (most likely an exception will be triggered)
function PathNoInsideRelative(const Path : string) : string;

{$IFDEF COMPILER5}
// Sets the date and time of the indicated file
// This function with this signature doesn't exists in D5 so
// we had to declare it here
function FileSetDate(const FileName: string; Age: Integer): Integer;
{$ENDIF COMPILER5}

implementation

uses
  Classes,
  {$IFDEF COMPILER6_UP}
  StrUtils,
  {$ENDIF COMPILER6_UP}
  {$IFDEF NO_JCL}
  UtilsJcl,
  {$ELSE}
  JclStrings, JclFileUtils,
  {$ENDIF NO_JCL}
  SysUtils;

{$IFDEF COMPILER5}
function AnsiStartsStr(const SubStr, S: string): Boolean;
begin
  Result := StrLComp(PChar(S), PChar(SubStr), Length(SubStr)) = 0;
end;

function AnsiEndsStr(const SubStr, S: string): Boolean;
begin
  Result := StrLComp(PChar(S) + Length(S) - Length(SubStr), PChar(SubStr), Length(SubStr)) = 0;
end;
{$ENDIF COMPILER5}

function StrEnsureNoPrefix(const prefix, str : string) : string;
begin
  if AnsiStartsStr(prefix, str) then
    Result := Copy(str, Length(prefix)+1, MaxInt)
  else
    Result := str;
end;

function StrEnsureNoSuffix(const suffix, str : string) : string;
begin
  if AnsiEndsStr(suffix, str) then
    Result := Copy(str, 1, Length(str)-Length(suffix))
  else
    Result := str;
end;

function GetRelativePath(const Origin, Destination : string) : string;
var
  OrigList : TStringList;
  DestList : TStringList;
  DiffIndex : Integer;
  i : Integer;
begin
  // create a list of paths as separate strings
  OrigList := TStringList.Create;
  DestList := TStringList.Create;
  try
    // NOTE: DO NOT USE DELIMITER AND DELIMITEDTEXT FROM
    // TSTRINGS, THEY WILL SPLIT PATHS WITH SPACES !!!!
    StrToStrings(Origin, PathSeparator, OrigList);
    StrToStrings(Destination, PathSeparator, DestList);
{$IFDEF MSWINDOWS}
    // Let's do some tests when the paths indicate drive letters
    // This, of course, only happens under a Windows platform

    // If the destination indicates a drive and the drive
    // letter is different from the one from the one in
    // origin, then simply return it as the result
    // Else, if the origin indicates a drive and destination
    // doesn't, then return the concatenation of origin and
    // destination, ensuring a pathseparator between them.
    // Else, try to find the relative path between the two.
    if (DestList[0][2] = ':') and
       (DestList[0][1] <> OrigList[0][1]) then
    begin
      Result := Destination;
    end
    else if (OrigList[0][2] = ':') and
            (DestList[0][2] <> ':') then
    begin
      Result := StrEnsureSuffix(PathSeparator, Origin)+StrEnsureNoPrefix(PathSeparator, Destination);
    end
    else
{$ENDIF}
    begin
      // find the first directory that is not the same
      DiffIndex := 0;
{$IFDEF MSWINDOWS} // case insensitive
      while CompareStr(OrigList[DiffIndex], DestList[DiffIndex]) = 0 do
{$ELSE}            // case sensitive
      while OrigList[DiffIndex] = DestList[DiffIndex] do
{$ENDIF}
        Inc(DiffIndex);

      Result := StrRepeat('..'+PathSeparator, OrigList.Count - DiffIndex);
      if DiffIndex < DestList.Count then
      begin
        for i := DiffIndex to DestList.Count - 2 do
          Result := Result + DestList[i] + PathSeparator;
        Result := Result + DestList[DestList.Count-1];
      end;
    end;
  finally
    DestList.Free;
    OrigList.Free;
  end;
end;

function PathNoInsideRelative(const Path : string) : string;
var
  PathList : TStringList;
  i : Integer;
begin
  PathList := TStringList.Create;
  try
    StrToStrings(Path, PathSeparator, PathList);
    i := 0;
    while i < PathList.Count do
    begin
      // three cases:
      // - a single point is found, we simply remove it
      // - a double point is found, not being the first and not being
      //   preceded by a double point, then we remove it and the
      //   directory before it
      // - everything else is left in
      if PathList[i] = '.' then
        PathList.Delete(i)
      else if (PathList[i] = '..') and
              (i>0) and
              (PathList[i-1] <> '..') then
      begin
        Dec(i);
        PathList.Delete(i);
        PathList.Delete(i);
      end
      else
        Inc(i);
    end;
    Result := StringsToStr(PathList, PathSeparator);
  finally
    PathList.Free;
  end;
end;

{$IFDEF COMPILER5}
function FileSetDate(const FileName: string; Age: Integer): Integer;
var
  f: THandle;
begin
  f := FileOpen(FileName, fmOpenWrite);
  if f = THandle(-1) then
    Result := GetLastError
  else
  begin
    Result := SysUtils.FileSetDate(f, Age);
    FileClose(f);
  end;
end;
{$ENDIF COMPILER5}


end.
