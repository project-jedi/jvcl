unit MakeDOFUtils;

interface

procedure Run;

implementation
uses
  Windows, Classes, SysUtils, JvSearchFiles;

procedure ShowHelp;
begin
  writeln('');
  writeln('');
  writeln('MakeDOF: Creates DOF files for projects based on a template file');
  writeln('');
  writeln('Usage:');
  writeln(ExtractFilename(ParamStr(0)),' <filemask> <template>');
  writeln('');
  writeln(#9'<filemask> - the files to find');
  writeln(#9'<template> - a template DOF file to create for the found file(s)');
  writeln('');
  writeln('The created DOF will have the same base name as the found file,');
  writeln('but the extension DOF.');
  writeln('');
  writeln('MakeDOF searches subfolders by default and overwrites any existing files');
end;

function CreateDOF(const FileMask, TemplateFile: string): integer;
var F: TJvSearchFiles;
  i: integer;
  S: TStringlist;
  T: string;
begin
  Result := 0;
  if not FileExists(TemplateFile) then
  begin
    writeln('ERROR: ', TemplateFile, ' not found!');
    Exit;
  end;

  S := TStringlist.Create;
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
        S.SaveToFile(T);
        Inc(Result);
      end;
    end;
  finally
    F.Free;
    S.Free;
  end;
end;

procedure Run;
var i: integer;
begin
  {        
    command-line:
      <filemask> <doftemplate>
  }
  if ParamCount <> 2 then
    ShowHelp
  else
  try
    i := CreateDOF(ExpandUNCFileName(ParamStr(1)), ExpandUNCFileName(ParamStr(2)));
    writeln(i, ' file(s) created');
  except
    on E: Exception do
      writeln('ERROR: ', E.Message);
  end;
end;

end.

