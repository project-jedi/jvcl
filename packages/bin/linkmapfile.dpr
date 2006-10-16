program linkmapfile;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JclDebug;

var
  ExecutableFilename, MapFilename: string;
  LinkerBugUnit: string;
  MapFileSize, JclDebugDataSize: Integer;
begin
  WriteLn('Link MAP file - command line tool');
  WriteLn;
  ExecutableFilename := ParamStr(1);
  MapFilename := ParamStr(2);
  if MapFilename = '' then
    MapFilename := ChangeFileExt(ExecutableFilename, '.map');

  if FileExists(ExecutableFilename) and FileExists(MapFilename) then
  begin
    ExitCode := Ord(
      not InsertDebugDataIntoExecutableFile(ExecutableFilename, MapFilename,
                                        LinkerBugUnit, MapFileSize, JclDebugDataSize)
    );
    if (ExitCode = 0) and (MapFileSize > 0) then
      WriteLn('MAP data inserted into ', ExtractFileName(ExecutableFilename), ' (', JclDebugDataSize div MapFileSize * 100, '%)');
  end
  else
  begin
    WriteLn('Usage:');
    WriteLn('  linkmapfile filename.bpl filename.map');
    WriteLn;
    ExitCode := 1;
  end;
end.

