unit FileUtils;

interface

// returns the relative path between Origin and Destination
// This is the string you would have to type after the 'cd'
// command is you were located in Origin and willing to change
// to Destination
// Both Origin and Destination should be absolute path but this
// is not verified by the function.
function GetRelativePath(Origin, Destination : string) : string;

// returns the given path without any relative instructions inside
// for instance, if you pass c:\a\b\..\c\.\d, it returns c:\a\c\d
// Note that if you pass ..\..\a\b it doesn't remove the heading
// relative paths instructions.
// If there are more ..\ than paths in front of it, the result
// is undefined (most likely an exception will be triggered)
function PathNoInsideRelative(Path : string) : string;

implementation

uses Classes, JclStrings, JclFileUtils, SysUtils;

function GetRelativePath(Origin, Destination : string) : string;
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

    // find the first directory that is not the same
    DiffIndex := 0;
    while (OrigList[DiffIndex] = DestList[DiffIndex]) do
      Inc(DiffIndex);

    Result := StrRepeat('..'+PathSeparator, OrigList.Count - DiffIndex);
    if DiffIndex < DestList.Count then
    begin
      for i := DiffIndex to DestList.Count - 2 do
        Result := Result + DestList[i] + PathSeparator;
      Result := Result + DestList[DestList.Count-1];
    end;
  finally
    DestList.Free;
    OrigList.Free;
  end;
end;

function PathNoInsideRelative(Path : string) : string;
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

end.
