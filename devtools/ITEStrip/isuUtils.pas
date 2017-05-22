unit isuUtils;

interface

procedure Run;

implementation
uses
  Forms, Classes, SysUtils;

function FindFiles(const Path, Filemask: string; Recursive: boolean; Files: TStrings): integer;
var
  F: TSearchRec;
  H: integer;
  tmp: string;
begin
  Result := 0;
  tmp := IncludeTrailingPathDelimiter(Path);
  H := FindFirst(tmp + Filemask, faAnyFile, F);
  if H = 0 then
  begin
    repeat
      if F.Attr and faDirectory = 0 then
      begin
        Files.Add(tmp + F.Name);
        Inc(Result);
      end;
    until FindNext(F) <> 0;
    FindClose(F);
  end;
  if Recursive then
  begin
    H := FindFirst(tmp + '*.*', faDirectory, F);
    if H = 0 then
    begin
      repeat
        if (F.Attr and faDirectory = faDirectory) and (F.Name[1] <> '.') then
          Inc(Result, FindFiles(tmp + F.Name, Filemask, true, Files));
      until FindNext(F) <> 0;
      FindClose(F);
    end;
  end;
end;

function AnsiContainsText(const S, SubString: string): boolean;
begin
  Result := Pos(AnsiLowerCase(SubString), AnsiLowerCase(S)) > 0;
end;

procedure StripUnused(const Filename: string);
var
  S: TStringlist;
  i: integer;
begin
  S := TStringlist.Create;
  try
    S.LoadFromFile(Filename);
    for i := S.Count - 1 downto 0 do
      if AnsiContainsText(S[i], 'Status:3') then
        S.Delete(i);
    try
      S.SaveToFile(Filename);
    except
      writeln('Unable to save ', Filename);
    end;
  finally
    S.Free;
  end;
end;

procedure ShowHelp;
begin
  writeln('isu: ITE StripUnused');
  writeln('Removes all unused translations from dfn files (those with Status = 3)');
  writeln('Processes the current folder and any subfolder.');
  writeln('Now working in ', GetCurrentDir, ':');
end;

procedure Run;
var
  FFiles: TStringlist;
  i: integer;
begin
  try
    ShowHelp;
    FFiles := TStringlist.Create;
    try
      FindFiles(GetCurrentDir, '*.dfn', true, FFiles);
      for i := 0 to FFiles.Count - 1 do
      begin
        writeln('Processing ', FFiles[i], '...');
        StripUnused(FFiles[i]);
      end;
      writeln('Done: Found and processed ', FFiles.Count, ' files');
    finally
      FFiles.Free;
    end;
  except
    on E: Exception do
      writeln(ErrOutput, 'ERROR:', E.Message);
  end;
end;
end.

