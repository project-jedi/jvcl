unit FileUtils;

{$I jvcl.inc}

interface

uses
  Windows;

// returns the relative path between Origin and Destination
// This is the string you would have to type after the 'cd'
// command is you were located in Origin and willing to change
// to Destination
// Both Origin and Destination should be absolute path but this
// is not verified by the function.
// If Origin and Destination are on different partitions,
// the function returns Destination
function GetRelativePath(const Origin, Destination: string): string;

// returns the given path without any relative instructions inside
// for instance, if you pass c:\a\b\..\c\.\d, it returns c:\a\c\d
// Note that if you pass ..\..\a\b it doesn't remove the heading
// relative paths instructions.
// If there are more ..\ than paths in front of it, the result
// is undefined (most likely an exception will be triggered)
function PathNoInsideRelative(const Path: string): string;

implementation

uses
  Classes, Math,
  StrUtils,
  JclStrings, JclFileUtils,
  SysUtils;

function StrEnsureNoPrefix(const prefix, str: string): string;
begin
  if AnsiStartsStr(prefix, str) then
    Result := Copy(str, Length(prefix)+1, MaxInt)
  else
    Result := str;
end;

function StrEnsureNoSuffix(const suffix, str: string): string;
begin
  if AnsiEndsStr(suffix, str) then
    Result := Copy(str, 1, Length(str)-Length(suffix))
  else
    Result := str;
end;

function GetRelativePath(const Origin, Destination: string): string;
var
  OrigList: TStringList;
  DestList: TStringList;
  DiffIndex, MinSize: Integer;
  i: Integer;
begin
  // create a list of paths as separate strings
  OrigList := TStringList.Create;
  DestList := TStringList.Create;
  try
    // NOTE: DO NOT USE DELIMITER AND DELIMITEDTEXT FROM
    // TSTRINGS, THEY WILL SPLIT PATHS WITH SPACES !!!!
    StrToStrings(Origin, DirDelimiter, OrigList);
    StrToStrings(Destination, DirDelimiter, DestList);
{$IFDEF MSWINDOWS}
    // Let's do some tests when the paths indicate drive letters
    // This, of course, only happens under a Windows platform

    // If the destination indicates a drive and the drive
    // letter is different from the one from the one in
    // origin, then simply return it as the result
    // Else, if the origin indicates a drive and destination
    // doesn't, then return the concatenation of origin and
    // destination, ensuring a DirDelimiter between them.
    // Else, try to find the relative path between the two.
    if (DestList[0][2] = ':') and
       (DestList[0][1] <> OrigList[0][1]) then
    begin
      Result := Destination;
    end
    else if (OrigList[0][2] = ':') and
            (DestList[0][2] <> ':') then
    begin
      Result := StrEnsureSuffix(DirDelimiter, Origin)+StrEnsureNoPrefix(DirDelimiter, Destination);
    end
    else
{$ENDIF}
    begin
      // find the first directory that is not the same
      DiffIndex := 0;
      MinSize := Min(OrigList.Count, DestList.Count);
{$IFDEF MSWINDOWS} // case insensitive
      while (DiffIndex < MinSize) and SameText (OrigList[DiffIndex], DestList[DiffIndex]) do
{$ELSE}            // case sensitive
      while (DiffIndex < MinSize) and (OrigList[DiffIndex] = DestList[DiffIndex]) do
{$ENDIF}
        Inc(DiffIndex);

      Result := StrRepeat('..' + DirDelimiter, OrigList.Count - DiffIndex);
      if DiffIndex < DestList.Count then
      begin
        for i := DiffIndex to DestList.Count - 2 do
          Result := Result + DestList[i] + DirDelimiter;
        Result := Result + DestList[DestList.Count-1];
      end;
    end;
  finally
    DestList.Free;
    OrigList.Free;
  end;
end;

function PathNoInsideRelative(const Path: string): string;
var
  PathList: TStringList;
  i: Integer;
begin
  PathList := TStringList.Create;
  try
    StrToStrings(Path, DirDelimiter, PathList);
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
    Result := StringsToStr(PathList, DirDelimiter);
  finally
    PathList.Free;
  end;
end;

end.
