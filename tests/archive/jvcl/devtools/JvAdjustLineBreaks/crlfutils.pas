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
  writeln('Converts all pas files to LF and all dfm files to CRLF');
  writeln('');
  writeln('NOTE: if you compile in Delphi 5 or earlier,');
  writeln('      only LF->CRLF (/w option) is supported!');
end;

procedure ConvertFile(const Filename:string;ToWindows:boolean);
{$IFDEF COMPILER6_UP}
const
  cStyle:array[boolean] of TTextLineBreakStyle = (tlbsLF,tlbsCRLF);
{$ENDIF COMPILER6_UP}
var S:TStringlist;
begin
  S := TStringlist.Create;
  try
    S.LoadFromFile(Filename);
    S.Text := AdjustLineBreaks(S.Text{$IFDEF COMPILER6_UP},cStyle[ToWindows]{$ENDIF});
    S.SaveToFile(Filename);
  finally
    S.Free;
  end;
end;

procedure ConvertFiles(const FileMask:string;ToWindows:boolean);
var
  SearchHandle:DWORD;
  FindData:TWin32FindData;
  APath:string;
begin
  APath := ExtractFilePath(Filemask);
  SearchHandle := FindFirstFile(PChar(Filemask),FindData);
  if SearchHandle <> INVALID_HANDLE_VALUE then
  try
    repeat
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0) then
         ConvertFile(APath + FindData.cFileName,ToWindows);
    until not FindNextFile(SearchHandle,FindData);
  finally
    Windows.FindClose(SearchHandle);
  end;

end;

procedure Run;
var
  ToWindows:boolean;
  i:integer;
begin
  // cmd line: -l *.pas *.dfm *.txt -w *.xfm
  // where
  // -l - convert CRLF to LF (to linux)
  // -w - convert LF to CRLF (to windows)
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
    ConvertFiles(ExpandUNCFilename(ParamStr(i)),ToWindows);
  end;
end;

end.
