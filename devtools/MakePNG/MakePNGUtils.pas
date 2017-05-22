unit MakePNGUtils;

interface

procedure Run;

implementation
uses
  Windows, SysUtils, Classes, Graphics, JTools,
  pngImage; // download from http://pngdelphi.sourceforge.net/ 

procedure ShowHelp;
begin
  writeln('');
  writeln('');
  writeln('MakePNG: BMP to PNG converter');
  writeln('Copyright (c) 2002 by Peter Thornqvist and JEDI');
  writeln('');
  writeln('Usage:');
  writeln(#9,ExtractFileName(ParamStr(0)),' <filemask> <outpath>');
  writeln('');
  writeln(#9'<filemask>'#9'sets the files to search for. Default is <curdir>\*.bmp');
  writeln(#9'<outpath>'#9'folder to save the PNG files to. Default is <curdir>');
end;

procedure MakePNGFiles(const FileMask, OutputFolder: string);
var
  fd: TWin32FindData;
  H: THandle;
  P: TPNGObject;
  B: TBitmap;
begin
  H := FindFirstFile(PChar(FileMask), fd);
  if H <> INVALID_HANDLE_VALUE then
  begin
    P := TPNGObject.Create;
    B := TBitmap.Create;
    try
      repeat
        try
          writeln('Processing ', string(fd.cFileName), '...');
          B.LoadFromFile(fd.cFileName);
          P.Assign(B);
          P.SaveToFile(OutputFolder + UpperCase(ChangeFileExt(fd.cFileName, '.PNG')));
        except
          writeln('Error processing ', string(fd.cFileName), '!');
          // probably not a valid BMP
        end;
      until not FindNextFile(H, fd);
    finally
      B.Free;
      P.Free;
    end;
    if H <> INVALID_HANDLE_VALUE then
      Windows.FindClose(H);
  end;
end;

procedure Run;
var
  FileMask, OutputFolder: string;
begin
  try
    if GetCmdSwitchValue('h', ['-', '/'], FileMask, true) or
      GetCmdSwitchValue('?', ['-', '/'], FileMask, true) then
    begin
      ShowHelp;
      Exit;
    end;
    case ParamCount of
      0:
        begin
          FileMask := ExtractFilePath(ParamStr(0)) + '*.bmp';
          OutputFolder := ExtractFilePath(ParamStr(0));
        end;
      1:
        begin
          FileMask := ParamStr(1);
          OutputFolder := ExtractFilePath(ParamStr(0));
        end;
    else
      begin
        FileMask := ParamStr(1);
        OutputFolder := ParamStr(2);
      end;
    end;
    ForceDirectories(OutputFolder);
    MakePNGFiles(FileMask, IncludeTrailingPathDelimiter(OutputFolder));
  except
    on E: Exception do
      writeln('ERROR: ', E.Message);
  end;
end;

end.

