{$I jvcl.inc}
unit crlfutils;

interface

procedure Run;
implementation
uses
  Windows, SysUtils, {$IFNDEF COMPILER6_UP} JvFunctions, {$ENDIF} Classes;

function IsTextStream(Stream:TStream):Boolean;
const
  MaxLineLength = 255;
var
  LineBuffer:array[0..MaxLineLength] of Char;
  CharFlags:array of Word;
  I, Count:Integer;
  P, S:PChar;
begin
  Result := False;

  FillChar(LineBuffer, SizeOf(LineBuffer), 0);

  Stream.Position := 0;
  if Stream.Size > MaxLineLength then
    Count := MaxLineLength
  else
    Count := Stream.Size;
  Stream.Read(LineBuffer, Count);

  // see if we can come up with an EOL (unless we read the whole file)
  if Count < Stream.Size then
  begin
    P := StrPos(LineBuffer, #13);
    // try the LF variant too
    if P = nil then
    begin
      P := StrPos(LineBuffer,#10);
      if P = nil then
        Exit;
    end;

    // terminate the string here
    P^ := #0;

    // check if there are any terminators prior to where we expect the EOL
    S := @LineBuffer;
    while S < P do
    begin
      if S^ = #0 then
        Exit;
      Inc(S);
    end;

  end;

  Count := StrLen(LineBuffer);

  // some editors place a $1A (26) as an EOF marker
  if LineBuffer[Count - 1] = Char($1A) then
  begin
    LineBuffer[Count - 1] := #0;
    Dec(Count);
  end;

  // if first character is $FF, then it's likely not text
  if LineBuffer[0] = Char($FF) then
    Exit;

  // get the char flags
  SetLength(CharFlags, Count);
  GetStringTypeEx(LOCALE_USER_DEFAULT, CT_CTYPE1, LineBuffer, Count,
    CharFlags[0]);

  // check the CharFlags array to see if anything looks fishy
  for I := Low(CharFlags) to High(CharFlags) do
    if ((CharFlags[I] and C1_CNTRL) <> 0) and ((CharFlags[I] and $0F) = 0) then
      Exit;

  // best guess is that it looks reasonable
  Result := True;
end;

function IsTextFile(const FileName:string):Boolean;
var
  S:TStream;
begin
  Result := False;
  if not FileExists(FileName) then
    Exit;

  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := IsTextStream(S);
  finally
    S.Free;
  end;
end;

procedure ShowHelp;
begin
  writeln('===================================================');
  writeln('Usage:');
  writeln('crlf [options] <filemask> [options] <filemask> (etc)');
  writeln('');
  writeln('where [options] can be any combination of:');
  writeln('  /s - recurse into sub-folders from this point on');
  writeln('  /c - compare before write: only write if file has changed (default)');
  writeln('  /u - do NOT compare before write: always write');
  writeln('  /l - convert to Linux line breaks (LF) (default on Linux)');
  writeln('  /w - convert to Windows line breaks (CRLF) (default on Windows)');
  writeln('');
  writeln('<filemask> accepts wildcards (* and ?).');
  writeln('');
  writeln('Example:');
  writeln('========');
  writeln('crlf /u /l *.pas /c /w /s *.dfm');
  writeln('Converts the pas files to LF (always writes) and the dfm files to CRLF (writes if modified). Recurses into sub-folders when searching for dfm''s.');
  writeln('');
  writeln('Example:');
  writeln('========');
  writeln('crlf *.pas *.dfm');
  writeln('Converts the pas and dfm files to LF on Linux and CRLF on Windows (system default). Always checks before write (no /u option).');
  writeln('');
  writeln('');
  writeln('NOTE: if you compiled using Delphi 5 or earlier, CRLF->LF is NOT supported!');
end;

function ConvertFile(const Filename:string;ToWindows,CompareBeforeWrite,Quiet:boolean):boolean;
{$IFDEF COMPILER6_UP}
const
  cStyle:array[boolean] of TTextLineBreakStyle = (tlbsLF,tlbsCRLF);
{$ENDIF COMPILER6_UP}
var
  F:TFileStream;
  tmp,tmp2:string;
begin
  // don't convert binary files
  Result := false;
  if not IsTextFile(Filename) then Exit;
  F := TFileStream.Create(Filename,fmOpenReadWrite or fmShareExclusive );
  try
    SetLength(tmp,F.Size);
    if F.Size > 0 then
    begin
      F.Read(tmp[1],F.Size);
      if CompareBeforeWrite then
        tmp2 := tmp;
      tmp := AdjustLineBreaks(tmp{$IFDEF COMPILER6_UP},cStyle[ToWindows]{$ENDIF});
      if CompareBeforeWrite and (tmp = tmp2) then
      begin
        if not Quiet then
          writeln(ExtractFilename(Filename), ' not converted');
        Exit;
      end;
      F.Size := 0;
      F.Write(tmp[1],Length(tmp));
      if not Quiet then
        writeln(ExtractFilename(Filename), ' converted');
    end;
  finally
    F.Free;
  end;
  Result := true;
end;

function ConvertFiles(const FileMask:string;ToWindows,CompareBeforeWrite,Recurse,Quiet:boolean):integer;
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
        else if ConvertFile(APath + FindData.cFileName,ToWindows,CompareBeforeWrite,Quiet) then
            Inc(Result);
      end;
    until not FindNextFile(SearchHandle,FindData);
  finally
    Windows.FindClose(SearchHandle);
  end;
  // do sub-folders
  if Recurse then
  begin
    SearchHandle := FindFirstFile(PChar(APath + '*.*'),FindData);
    if SearchHandle <> INVALID_HANDLE_VALUE then
    try
      repeat
        if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY) and (FindData.cFileName[0] <> '.') then
          Inc(Result,ConvertFiles(IncludeTrailingPathdelimiter(APath + FindData.cFileName) + ExtractFilename(Filemask),ToWindows,CompareBeforeWrite,true,Quiet));
      until not FindNextFile(SearchHandle,FindData);
    finally
      Windows.FindClose(SearchHandle);
    end;
  end;
end;

procedure Run;
const
  cCurrentOS:array[boolean] of PChar = ('(CRLF->LF)','(LF->CRLF)');
var
  ToWindows,CompareBeforeWrite,Recurse,Quiet:boolean;
  i,Count:integer;
begin
  // cmd line: -l *.pas *.dfm *.txt -c -w *.xfm
  // where
  // -l - convert CRLF to LF (to linux)
  // -w - convert LF to CRLF (to windows)
  // -c - check content: only write if file has changed (default)
  // -u - never check content: always write
  writeln('');
  writeln('JEDI CR(LF) version 0.1: LF->CRLF and CRLF->LF converter.');

  Count := 0;
  if ParamCount = 0 then
  begin
    ShowHelp;
    Exit;
  end;
  CompareBeforeWrite := true;
  Recurse := false;
  // set depending on target
  ToWindows := true;
  {$IFDEF LINUX}
  ToWindows := false;
  {$ENDIF}
  Quiet := false;
  for i := 1 to ParamCount do
  begin
    if SameText(ParamStr(i),'/l') or SameText(ParamStr(i),'-l') then
    begin
      ToWindows := false;
      if not Quiet then
        writeln('Converting ', cCurrentOS[ToWindows],':');
      Continue;
    end
    else if SameText(ParamStr(i),'/w') or SameText(ParamStr(i),'-w') then
    begin
      ToWindows := true;
      if not Quiet then
        writeln('Converting ', cCurrentOS[ToWindows],':');
      Continue;
    end
    else if SameText(ParamStr(i),'/?') or SameText(ParamStr(i),'-?') or
       SameText(ParamStr(i),'/h') or SameText(ParamStr(i),'-h')then
    begin
      ShowHelp;
      Exit;
    end
    else if SameText(ParamStr(i),'/c') or SameText(ParamStr(i),'-c') then
    begin
      CompareBeforeWrite := true;
      Continue;
    end
    else if SameText(ParamStr(i),'/u') or SameText(ParamStr(i),'-u') then
    begin
      CompareBeforeWrite := false;
      Continue;
    end
    else if SameText(ParamStr(i),'/s') or SameText(ParamStr(i),'-s') then
    begin
      Recurse := true;
      Continue;
    end
    else if SameText(ParamStr(i),'/q') or SameText(ParamStr(i),'-q') then
    begin
      Quiet := true;
      Continue;
    end
    else
      Inc(Count,ConvertFiles(ExpandUNCFilename(ParamStr(i)),ToWindows,CompareBeforeWrite,Recurse,Quiet));
  end;
  writeln('');
  writeln('Done: ', Count, ' files converted.');
end;

end.
