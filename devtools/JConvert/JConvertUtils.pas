unit JConvertUtils;

interface

procedure Run;

implementation
uses
  Windows, Classes, SysUtils, JvSearchFiles, JvJCLUtils, JvJVCLUtils, JTools;

procedure ShowHelp;
begin
  writeln('Usage: ', ExtractFileName(ParamStr(0)),
    ' [-i] [-s] [-t | -b] <filespec(s) | @filelist>');
  writeln(#9'-i', #9, 'Convert files in-place (output overwrites input)');
  writeln(#9'-s', #9, 'Recurse subdirectories');
  writeln(#9'-t', #9, 'Convert to text');
  writeln(#9'-b', #9, 'Convert to binary');
  writeln(#9'-c', #9, 'Check without converting');
  writeln(#9'-v', #9, 'Verbose output');
  writeln('');
  writeln('');
  writeln('This program differs from Borland''s Convert utility in two ways:');
  writeln('');
  writeln('1. Only if a file is changed, is it written back to disk.');
  writeln(#9'This is essential when working with VCS systems.');
  writeln('');
  writeln('2. You can use the -c option to check how many files would be converted,');
  writeln(#9, 'without actually making the conversion.');
end;

function InternalConvertDFM(InPlace, BinToTxt, Simulate, Verbose: boolean; const Filename: string): integer;
var
  AFormat: TStreamOriginalFormat;
  InFile, OutFile: TFileStream;
  TmpStream: TMemoryStream;
  ShortFilename: string;
  function GetOutName(const AName: string; InPlace: boolean): string;
  begin
    if InPlace then
      Result := AName
    else
      Result := ChangeFileExt(AName, '.txt');
  end;
  function IsTextResource(Stream: TStream): boolean;
  var
    Signature: integer;
  begin
    Stream.Read(Signature, sizeof(Signature));
    Result := (Char(Signature) in [#9, #11, #13, ' ', 'o', 'O', 'i', 'I']);
    Stream.Seek(-sizeof(Signature), soFromCurrent);
  end;
begin
  Result := 0;
  ShortFilename := ExtractRelativePath(IncludeTrailingPathDelimiter(GetCurrentDir), Filename);
  if (GetFileAttributes(PChar(Filename)) and FILE_ATTRIBUTE_READONLY <> 0) and not Simulate then
  begin
    writeln('WARNING: ', ShortFilename, ' is read-only (NOT converted).');
    Exit;
  end;
  if Verbose then
    writeln('Reading ', ShortFilename);
  AFormat := sofUnknown;
  try
    InFile := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
    TmpStream := TMemoryStream.Create;
    try
      if BinToTxt then
      begin
        if IsTextResource(InFile) then
          Exit;
        ObjectResourceToText(InFile, TmpStream, AFormat);
        if AFormat = sofBinary then
        begin
          FreeAndNil(InFile);
          TmpStream.Seek(0, soFromBeginning);
          if Verbose then
          begin
            if not Simulate then
              writeln('Writing ', GetOutName(ShortFileName, InPlace))
            else
              writeln('Is Binary: ', ShortFileName)
          end;
          if not Simulate then
          begin
            OutFile := TFileStream.Create(getOutName(Filename, InPlace), fmCreate);
            try
              OutFile.CopyFrom(TmpStream, 0);
            finally
              FreeAndNil(OutFile);
            end;
          end;
          Result := 1;
        end;
      end
      else
      begin
        if not IsTextResource(InFile) then
          Exit;
        ObjectTextToResource(InFile, TmpStream, AFormat);
        if AFormat = sofText then
        begin
          FreeAndNil(InFile);
          TmpStream.Seek(0, soFromBeginning);
          if Verbose and not Simulate then
          begin
            if not Simulate then
              writeln('Writing ', GetOutName(ShortFileName, InPlace))
            else
              writeln('Is Text: ', ShortFileName)
          end;
          if not Simulate then
          begin
            OutFile := TFileStream.Create(GetOutName(Filename, InPlace), fmCreate);
            try
              OutFile.CopyFrom(TmpStream, 0);
            finally
              FreeAndNil(OutFile);
            end;
          end;
          Result := 1;
        end;
      end;
    finally
      InFile.Free;
      TmpStream.Free;
    end;
  except
    on E: Exception do
      writeln('ERROR: (', E.Message, ') in ', ShortFilename);
  end;
end;

function ConvertDFM(SubDirs, Inplace, BinToTxt, Simulate, Verbose: boolean; FileSpecs:
  TStringlist; var TotalCount:integer): integer;
var F: TJvSearchFiles;
  i, j: integer;
begin
  Result := 0;
  F := TJvSearchFiles.Create(nil);
  try
    if SubDirs then
      F.DirOption := doIncludeSubDirs
    else
      F.DirOption := doExcludeSubDirs;
    F.Options := F.Options - [soStripDirs];
    if Verbose then
      writeln('Building file list, please wait...');
    for i := 0 to FileSpecs.Count - 1 do
    begin
      F.RootDirectory := ExtractFilePath(FileSpecs[i]);
      if F.RootDirectory = '' then
        F.RootDirectory := GetCurrentDir
      else
        F.RootDirectory := ExpandUNCFileName(F.RootDirectory);
      F.DirParams.FileMask := '*.*';
      F.FileParams.FileMask := ExtractFilename(FileSpecs[i]);
      F.FileParams.SearchTypes := [stFileMask];
      if F.FileParams.FileMask = '' then
        F.FileParams.FileMask := '*.dfm';
      F.Search;
      TotalCount := F.Files.Count;
      for j := 0 to F.Files.Count - 1 do
        Inc(Result, InternalConvertDFM(Inplace, BinToTxt, Simulate, Verbose, F.Files[j]));
    end;
  finally
    F.Free;
  end;
end;


procedure Run;
var
  DoSubDirs, DoInplace, DoBinToTxt, DoSimulate, DoVerbose: boolean;
  Path: string;
  i,TotalCount: integer;
  FileSpecs: TStringlist;


begin
  { Command switches:
    -s do subdirs
    -i write inpace
    -t bin -> txt
    -b txt -> bin
    -c simulate
    -v verbose output
    }
  // always tell them who we are:
  try
    writeln('JEDI Form Conversion Utility Version 1.0');

    if (ParamCount < 1) or GetCmdSwitchValue('?', ['-', '/'], Path, true) or
      GetCmdSwitchValue('h', ['-', '/'], Path, true) then
      ShowHelp
    else
    begin
      FileSpecs := TStringlist.Create;
      try
      // Path is used as dummy here
        DoSubDirs := GetCmdSwitchValue('s', ['-', '/'], Path, true);
        DoInplace := GetCmdSwitchValue('i', ['-', '/'], Path, true);
        DoBinToTxt := GetCmdSwitchValue('t', ['-', '/'], Path, true);
        DoSimulate := GetCmdSwitchValue('c', ['-', '/'], Path, true);
        DoVerbose := GetCmdSwitchValue('v', ['-', '/'], Path, true);
      // Path is actually used here
        if not GetCmdSwitchValue('@', [], Path, true) then
        begin
          for i := 1 to ParamCount do   // we must still read entire command line to find any filespecs
            if not (ParamStr(i)[1] in ['-', '/', '@']) then
              FileSpecs.Add(ParamStr(i));
        end
        else if FileExists(Path) then   // load file specs from a file
          FileSpecs.LoadFromFile(Path);

        if FileSpecs.Count = 0 then
          writeln('ERROR: no files specified')
        else
        begin
          i := ConvertDFM(DoSubDirs, DoInplace, DoBinToTxt, DoSimulate, DoVerbose, FileSpecs,TotalCount);
          if DoSimulate then
            writeln(TotalCount,' file(s) found, ',i, ' file(s) should have been converted')
          else
            writeln(TotalCount,' file(s) found, ',i, ' file(s) converted');
        end;
      finally
        FileSpecs.Free;
      end;
    end;
  except
    on E: Exception do
      writeln('ERROR: ', E.Message);
  end;
end;

end.

