unit MakeRCUtils;

interface

procedure Run;

implementation
uses
  Windows, SysUtils, Classes, JTools;

procedure ShowHelp;
begin
  writeln('');
  writeln('');
  writeln('MakeRC: creates an RC file from the BMP files in a specific folder');
  writeln('');
  writeln('Usage:');
  writeln(ExtractFileName(ParamStr(0)), ' <filemask(s)> <filename>');
  writeln(#9'<filemask>'#9'files to search for. Defaults to <curdir>\*.bmp');
  writeln(#9'<filename>'#9'name of rc to create. Defaults to <curdir>\new.rc');
  writeln('or:');
  writeln('/a <infile> <infile> <infile>');
  writeln('Output file will be saved as new.rc');
  writeln('');
  readln;
end;

procedure MakeRCFile(const FileMask, Filename: string; Append: boolean);
var
  fd: TWin32FindData;
  H: THandle;
  S: TStringlist;
begin
  writeln('File mask is ', FileMask, ':');
  H := FindFirstFile(PChar(FileMask), fd);
  if H <> INVALID_HANDLE_VALUE then
  begin
    S := TStringlist.Create;
    try
      if Append and FileExists(Filename) then
        S.LoadFromFile(Filename);
      repeat
        S.Add(Format('%s'#9'BITMAP'#9'"%s"', [UpperCase(ChangeFileExt(fd.cFileName, '')), UpperCase(fd.cFileName)]));
        writeln('Adding ', string(fd.cFileName), '...');
      until not FindNextFile(H, fd);
      if S.Count > 0 then
      begin
        writeln('Writing RC file as ', ExtractFilePath(FileMask), 'new.rc...');
        S.SaveToFile(Filename);
      end
      else
        writeln('No bitmaps found! RC *not* created');
    finally
      S.Free;
    end;
    if H <> INVALID_HANDLE_VALUE then
      Windows.FindClose(H);
  end;
end;

procedure Run;
var
  FileMask, Filename: string; i: integer; Append: boolean;
begin
  try
    writeln('MakeRC: creates an RC file from the bitmap files in a specific folder');
    Append := GetCmdSwitchValue('a', ['-', '/'], FileMask, true);
    if GetCmdSwitchValue('h', ['-', '/'], FileMask, true) or
      GetCmdSwitchValue('?', ['-', '/'], FileMask, true) then
    begin
      ShowHelp;
      Exit;
    end;
    if Append then
    begin
      for i := 2 to ParamCount do
        MakeRCFile(ParamStr(i), ExpandUNCFilename('new.rc'), true);
    end
    else
      case ParamCount of
        0:
          begin
            FileMask := ExpandUNCFilename('*.bmp');
            Filename := ExpandUNCFilename('new.rc');
          end;
        1:
          begin
            FileMask := ExpandUNCFilename(ParamStr(1));
            Filename := ExpandUNCFilename('new.rc');
          end;
      else
        begin
          FileMask := ExpandUNCFilename(ParamStr(1));
          Filename := ExpandUNCFilename('new.rc');
        end;
      end;
    MakeRCFile(FileMask, Filename, false);
  except
    on E: Exception do
      writeln('ERROR: ', E.Message);
  end;
end;
end.

