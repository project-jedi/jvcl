unit MakeDOFUtils;

interface

procedure Run;

implementation
uses
  Windows, Classes, SysUtils, JFileSearch;

procedure ShowHelp;
begin
  writeln('');
  writeln('');
  writeln('MakeDOF: Creates DOF files for projects based on a template file');
  writeln('');
  writeln('Usage:');
  writeln(ExtractFilename(ParamStr(0)),' <filemask> <template> [-S]');
  writeln('');
  writeln(#9'<filemask> - the files to find');
  writeln(#9'<template> - a template DOF file to create for the found file(s)');
  writeln(#9'-S         - alter the DOF file to match the sub directory');
  writeln('');
  writeln('The created DOF will have the same base name as the found file,');
  writeln('but the extension DOF.');
  writeln('');
  writeln('MakeDOF searches subfolders by default and overwrites any existing files');
end;

function GetReturnPath(const Dir: string): string;
var
  i: Integer;
begin
  Result := '';
  if Dir <> '' then
  begin
    Result := '';
    for i := 1 to Length(Dir) do
      if Dir[i] = PathDelim then
        Result := Result + '..\';
    while (Length(Result) > 0) and (Result[Length(Result)] = PathDelim) do
      SetLength(Result, Length(Result) - 1);
  end;
end;

procedure AlterData(List: TStrings; RootDir, Dir: string);
const
  ParentPath = '..' + PathDelim;
var
  S, NewLine, A: string;
  i: Integer;
  NextPs, ps: Integer;
begin
  if (RootDir <> '') and (RootDir[Length(RootDir)] = PathDelim) then
    Delete(RootDir, Length(RootDir), 1);
  if (Dir <> '') and (Dir[Length(Dir)] = PathDelim) then
    Delete(Dir, Length(Dir), 1);

  if Length(RootDir) = Length(Dir) then
    Exit; // nothing to do
  S := Copy(Dir, Length(RootDir) + 1, MaxInt);
  if S[1] = PathDelim then
    Delete(S, 1, 1);

  Dir := GetReturnPath(S);
  if Dir <> '' then
  begin
    Dir := Dir + PathDelim;
    for i := 0 to List.Count - 1 do
    begin
      S := List[i];
      if Trim(S) = '' then Continue;

      NewLine := '';
      S := S + ';';
      NextPs := Pos(';', S);
      while S <> '' do
      begin
        A := Copy(S, 1, NextPs - 1);
        ps := Pos(ParentPath, A);
        if ps > 0 then
          Insert(Dir, A, ps);
        NewLine := NewLine + ';' + A;
        Delete(S, 1, NextPs);
        NextPs := Pos(';', S);
      end;
      Delete(NewLine, 1, 1);
      List[i] := NewLine;
    end;
  end;
end;

function CreateDOF(const FileMask, TemplateFile: string; AlterForSubDirs: Boolean): integer;
var F: TJvSearchFiles;
  i: integer;
  S, S2: TStringlist;
  T: string;
begin
  Result := 0;
  if not FileExists(TemplateFile) then
  begin
    writeln('ERROR: ', TemplateFile, ' not found!');
    Exit;
  end;

  S := TStringList.Create;
  S2 := TStringList.Create;
  F := TJvSearchFiles.Create(nil);
  try
    S.LoadFromFile(TemplateFile);
    F.FileParams.FileMask := ExtractFileName(FileMask);
    F.FileParams.SearchTypes := [stFileMask];
    F.RootDirectory := ExpandUNCFileName(ExtractFilePath(FileMask));
    
    F.Options := F.Options - [soStripDirs];
    F.DirOption := doIncludeSubDirs;
    F.Search;
    for i := 0 to F.Files.Count - 1 do
    begin
      T := ChangeFileExt(F.Files[i], '.dof');
      if FileExists(T) and (GetFileAttributes(PChar(T)) and FILE_ATTRIBUTE_READONLY > 0) then
        writeln('WARNING: ', T, ' exists and is read-only')
      else
      begin
        S2.Assign(S);
        if AlterForSubDirs then
          AlterData(S2, F.RootDirectory, ExtractFilePath(T));
        S2.SaveToFile(T);
        Inc(Result);
      end;
    end;
  finally
    F.Free;
    S2.Free;
    S.Free;
  end;
end;

procedure Run;
var i: integer;
begin
  {        
    command-line:
      <filemask> <doftemplate> [-S]
  }
  if (ParamCount < 2) or (ParamCount > 3) then
    ShowHelp
  else
  try
    i := CreateDOF(ExpandUNCFileName(ParamStr(1)), ExpandUNCFileName(ParamStr(2)), 
      CompareText(ParamStr(3), '-S') = 0);
    writeln(i, ' file(s) created');
  except
    on E: Exception do
      writeln('ERROR: ', E.Message);
  end;
end;

end.

