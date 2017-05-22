program UpCase;

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils;
procedure ShowTitle;
begin
  writeln('JEDI UPCASE 1.0 - converts filenames to uppercase');
end;

procedure ShowHelp;
begin
  ShowTitle;
  writeln('');
  writeln('Usage:');
  writeln('upcase.exe [filemask]');
  writeln('');
  writeln('where:');
  writeln('[filemask] is the files to uppercase (supports wildcards). If [filemask] is missing, all filenames are uppercased');
end;

var
  FD:TWin32FindData;
  Handle:Cardinal;
  FileMask:String;
begin
  ShowHelp;
  if ParamCount > 0 then
    FileMask := ParamStr(1)
  else
    FileMask := '*.*';
  Handle := FindFirstFile(PChar(FileMask),FD);
  if Handle <> 0 then
  begin
    repeat
      RenameFile(fd.cFileName,UpperCase(fd.cFileName));
      writeln(fd.cFileName);
    until not FindNextFile(Handle,fd);
    Windows.FindClose(Handle);
  end;
end.
 