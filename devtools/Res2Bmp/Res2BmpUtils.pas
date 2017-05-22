unit Res2BmpUtils;

interface

procedure Run;

implementation
uses
  Windows, SysUtils, Classes, Graphics,
  unitResFile,  unitResourceGraphics, // get these from http://www.wilsonc.demon.co.uk/d7resourceutils.htm
  JTools;

procedure ShowHelp;
begin
  writeln('');
  writeln('');
  writeln('Res2Bmp: extracts bitmaps from resource files');
  writeln('');
  writeln('Usage:');
  writeln(ExtractFilename(ParamStr(0)), ' <filemask>');
  writeln('<filemask>'#9'the files to extract bitmaps from. Defaults to <curdir>\*.dcr');
  writeln('');
  writeln('The extracted bitmap files are written to the same folder as the resource file(s)');
end;

procedure ExtractBmpResources(const FileMask: string);
var
  fd: TWin32FindData;
  H: THandle;
  R: TResModule;
  i: integer;
  P: TPicture;
  Folder: string;
begin
  H := FindFirstFile(PChar(FileMask), fd);

  if H <> INVALID_HANDLE_VALUE then
  begin
    Folder := ExtractFilePath(FileMask);
    R := TResModule.Create;
    P := TPicture.Create;
    try
      repeat
        writeln('Reading resources from ', string(fd.cFileName), '...');
        R.LoadFromFile(Folder + string(fd.cFileName));
        for i := 0 to R.ResourceCount - 1 do
          if R.ResourceDetails[i] is TBitmapResourceDetails then
          begin
            TBitmapResourceDetails(R.ResourceDetails[i]).GetImage(P);
            if Assigned(P.Graphic) and not P.Graphic.Empty then
            begin
              writeln('Writing ', UpperCase(R.ResourceDetails[i].ResourceName), '.BMP...');
              P.SaveToFile(Folder + UpperCase(R.ResourceDetails[i].ResourceName) + '.BMP');
            end
            else
              writeln('No bitmaps found in ', string(fd.cFileName));
          end;
      until not FindNextFile(H, fd);
      writeln('Done!');
    finally
      R.Free;
      P.Free;
    end;
    if H <> INVALID_HANDLE_VALUE then
      Windows.FindClose(H);
  end;
end;


procedure Run;
var
  FileMask: string;
begin
  try
    if GetCmdSwitchValue('h', ['-', '/'], FileMask, true) or
      GetCmdSwitchValue('?', ['-', '/'], FileMask, true) then
    begin
      ShowHelp;
      Exit;
    end;
    if ParamCount = 0 then
      FileMask := ExtractFilePath(ParamStr(0)) + '*.dcr'
    else
      FileMask := ParamStr(1);
    ExtractBmpResources(FileMask);
  except
    on E: Exception do
      writeln('ERROR: ', E.Message);
  end;
end;
end.

