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
  writeln(ExtractFileName(ParamStr(0)), ' <filemask> <filename>');
  writeln('');
  writeln(#9'<filemask>'#9'files to search for. Defaults to <curdir>\*.bmp');
  writeln(#9'<filename>'#9'name of rc to create. Defaults to <curdir>\new.rc');
  readln;
end;

procedure MakeRCFile(const FileMask, Filename: string);
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
  FileMask, Filename: string;
begin
  try
    writeln('MakeRC: creates an RC file from the bitmap files in a specific folder');
    if GetCmdSwitchValue('h', ['-', '/'], FileMask, true) or
      GetCmdSwitchValue('?', ['-', '/'], FileMask, true) then
    begin
      ShowHelp;
      Exit;
    end;
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
        FileMask := ParamStr(1);
        Filename := ExpandUNCFilename(ParamStr(2));
      end;
    end;
    MakeRCFile(FileMask, Filename);
  except
    on E: Exception do
      writeln('ERROR: ', E.Message);
  end;
end;
end.

