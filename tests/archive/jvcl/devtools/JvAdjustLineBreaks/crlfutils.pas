{$I JVCL.INC}
unit crlfutils;

interface

procedure Run;
implementation
uses
  Windows, SysUtils, Classes;

procedure ShowHelp;
begin
  writeln('JEDI CR(LF) utility: converts LF to CRLF or CRLF to LF');
  writeln('');
  writeln('Usage:');
  writeln('crlf [/l|/w] <filemask> [/l|/w] <filemask> (etc)');
  writeln('');
  writeln('where');
  writeln('  /l - convert to Linux line breaks (LF)');
  writeln('  /w - convert to Windows line breaks (CRLF)');
  writeln('');
  writeln('Example:');
  writeln('crlf /l *.pas /w *.dfm');
  writeln('Converts the pas files to LF and the dfm files to CRLF');
  writeln('');
  writeln('Example:');
  writeln('crlf *.pas *.dfm');
  writeln('Converts the pas and dfm files to LF on Linux and CRLF on Windows (system default)');
  writeln('');
  writeln('');
  writeln('NOTE: if you compile in Delphi 5 or earlier,');
  writeln('      CRLF->LF is NOT supported!');
end;

procedure ConvertFile(const Filename:string;ToWindows:boolean);
{$IFDEF COMPILER6_UP}
const
  cStyle:array[boolean] of TTextLineBreakStyle = (tlbsLF,tlbsCRLF);
{$ENDIF COMPILER6_UP}
var S:TFileStream;tmp:string;
begin
  S := TFileStream.Create(Filename,fmOpenReadWrite or fmShareExclusive );
  try
    SetLength(tmp,S.Size);
    if S.Size > 0 then
    begin
      S.Read(tmp[1],S.Size);
      tmp := AdjustLineBreaks(tmp{$IFDEF COMPILER6_UP},cStyle[ToWindows]{$ENDIF});
      S.Size := 0; 
      S.Write(tmp[1],Length(tmp));
    end;
  finally
    S.Free;
  end;
end;

function ConvertFiles(const FileMask:string;ToWindows:boolean):integer;
var
  SearchHandle:DWORD;
  FindData:TWin32FindData;
  APath:string;
begin
  Result := 0;
  APath := ExtractFilePath(Filemask);
  SearchHandle := FindFirstFile(PChar(Filemask),FindData);
  if SearchHandle <> INVALID_HANDLE_VALUE then
  try
    repeat
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0) then
      begin
        if (FindData.dwFileAttributes and FILE_ATTRIBUTE_READONLY = FILE_ATTRIBUTE_READONLY) then
          writeln('ERROR: ',FindData.cFileName,' is read-only!')
        else
        begin
          writeln('Converting ',FindData.cFileName,'...');
          ConvertFile(APath + FindData.cFileName,ToWindows);
          Inc(Result);
        end;
      end;
    until not FindNextFile(SearchHandle,FindData);
  finally
    Windows.FindClose(SearchHandle);
  end;
end;

procedure Run;
var
  ToWindows:boolean;
  i,Count:integer;
begin
  // cmd line: -l *.pas *.dfm *.txt -w *.xfm
  // where
  // -l - convert CRLF to LF (to linux)
  // -w - convert LF to CRLF (to windows)
  Count := 0;
  if ParamCount = 0 then
  begin
    ShowHelp;
    Exit;
  end;
  // set depending on target
  ToWindows := true;
  {$IFDEF LINUX}
  ToWindows := false;
  {$ENDIF}
  for i := 1 to ParamCount do
  begin
    if AnsiSameText(ParamStr(i),'/l') or AnsiSameText(ParamStr(i),'-l') then
    begin
      ToWindows := false;
      Continue;
    end;
    if AnsiSameText(ParamStr(i),'/w') or AnsiSameText(ParamStr(i),'-w') then
    begin
      ToWindows := true;
      Continue;
    end
    else if AnsiSameText(ParamStr(i),'/?') or AnsiSameText(ParamStr(i),'-?') or
       AnsiSameText(ParamStr(i),'/h') or AnsiSameText(ParamStr(i),'-h')then
    begin
      ShowHelp;
      Exit;
    end;
    Inc(Count,ConvertFiles(ExpandUNCFilename(ParamStr(i)),ToWindows));
  end;
  writeln(Count,' files converted.')
end;

end.
