unit isuUtils;

interface

procedure Run;

implementation
uses
  Forms, Classes, StrUtils, SysUtils, JvSearchFiles;

procedure StripUnused(const Filename: string);
var S: TStringlist;
  i: integer;
begin
  S := TStringlist.Create;
  try
    S.LoadFromFile(Filename);
    for i := S.Count - 1 downto 0 do
      if AnsiContainsText(S[i], 'Status:3') then
        S.Delete(i);
    S.SaveToFile(Filename);
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
var i: integer;
begin
  try
    ShowHelp;
    with TJvSearchFiles.Create(nil) do
    try
      RootDirectory := GetCurrentDir;
      FileParams.FileMask := '*.dfn';
      Search;
      for i := 0 to Files.Count - 1 do
      begin
        writeln('Processing ',ExtractRelativePath(GetCurrentDir,ExtractFilePath(Files[i])),ExtractFileName(Files[i]),'...');
        StripUnused(Files[i]);
      end;
      writeln('Done: Found and processed ',Files.Count, ' files');
    finally
      Free;
    end;
  except
    on E: Exception do
      writeln(ERROUTPUT, 'ERROR:', E.Message);
  end;
end;
end.

