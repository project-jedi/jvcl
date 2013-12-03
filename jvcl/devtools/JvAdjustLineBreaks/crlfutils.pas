unit crlfutils;

{$I jvcl.inc}

interface

procedure Run;

implementation

uses
  Windows, SysUtils, AnsiStrings, Classes;

function FindLineEnd(const Buffer: TBytes; Offset: Integer): PByte;
begin
  if (Buffer <> nil) and (Offset >= 0) and (Offset < Length(Buffer)) then
  begin
    Result := PByte(Buffer) + Offset;
    Offset := Length(Buffer) - Offset;
    while Offset > 0 do
    begin
      case Result^ of
        0: Break;
        10, 13: Exit;
      end;
      Inc(Result);
      Dec(Offset);
    end;
  end;
  Result := nil;
end;

function IsTextStream(Stream: TStream): Boolean;
const
  MaxLineLength = 1024;
var
  LineBuffer: TBytes;
  CharFlags: array of Word;
  I, Count: Integer;
  P: PByte;
  Offset: Integer;
  Encoding: TEncoding;
  Text: string;
begin
  Result := False;

  SetLength(LineBuffer, MaxLineLength);

  Stream.Position := 0;
  if Stream.Size > MaxLineLength then
    Count := MaxLineLength
  else
    Count := Stream.Size;

  if Count = 0 then
    Exit;

  Stream.Read(LineBuffer[0], Count);
  Offset := TEncoding.GetBufferEncoding(LineBuffer, Encoding);

  // if first character is $FF, then it's likely not text
  if LineBuffer[Offset] = $FF then
    Exit;

  // see if we can come up with an EOL (unless we read the whole file)
  if Count < Stream.Size then
  begin
    P := FindLineEnd(LineBuffer, Offset);
    if P = nil then
      Exit;

    // terminate the string here
    P^ := 0;
    Count := P - PByte(LineBuffer);
  end
  else
  begin
    Count := Length(LineBuffer);
    // some editors place a $1A (26) as an EOF marker
    if LineBuffer[Count - 1] = $1A then
    begin
      LineBuffer[Count - 1] := 0;
      Dec(Count);
    end;
  end;
  Count := Count - Offset;

  Text := Encoding.GetString(LineBuffer, Offset, Count);
  Count := Length(Text);

  // get the char flags
  SetLength(CharFlags, Count);
  GetStringTypeEx(LOCALE_USER_DEFAULT, CT_CTYPE1, PChar(Text), Count, CharFlags[0]);

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

function AdjustByteBufferLineBreaks(Buffer: TBytes; Offset: Integer; out AdjustedBuffer: TBytes; Style: TTextLineBreakStyle): Boolean;
var
  Source, SourceEnd, Dest: PByte;
  SourceLen, DestLen: Integer;
begin
  if Buffer = nil then
  begin
    AdjustedBuffer := nil;
    Result := False;
    Exit;
  end;

  Source := PByte(Buffer);
  SourceEnd := Source + Length(Buffer);
  Inc(Source, Offset);
  SourceLen := SourceEnd - Source;

  DestLen := SourceLen;
  while Source < SourceEnd do
  begin
    case Source^ of
      10:
        if Style = tlbsCRLF then
          Inc(DestLen);
      13:
        if Style = tlbsCRLF then
          if (Source + 1 < SourceEnd) and (Source[1] = 10) then
            Inc(Source)
          else
            Inc(DestLen)
        else
          if (Source + 1 < SourceEnd) and (Source[1] = 10) then
            Dec(DestLen);
    end;
    Inc(Source);
  end;
  if DestLen = SourceLen then
  begin
    if Offset = 0 then
      AdjustedBuffer := Buffer
    else
      AdjustedBuffer := Copy(Buffer, Offset, MaxInt);
    Result := False;
  end
  else
  begin
    SetLength(AdjustedBuffer, DestLen);
    Source := PByte(Source) + Offset;
    Dest := PByte(AdjustedBuffer);
    while Source < SourceEnd do
    begin
      case Source^ of
        10:
          begin
            if Style = tlbsCRLF then
            begin
              Dest^ := 13;
              Inc(Dest);
            end;
            Dest^ := 10;
            Inc(Dest);
            Inc(Source);
          end;
        13:
          begin
            if Style = tlbsCRLF then
            begin
              Dest^ := 13;
              Inc(Dest);
            end;
            Dest^ := 10;
            Inc(Dest);
            Inc(Source);
            if Source^ = 10 then
              Inc(Source);
          end;
      else
        Dest^ := Source^;
        Inc(Dest);
        Inc(Source);
      end;
    end;
    Result := True;
  end;
end;

function ConvertFile(const Filename: string; ToWindows, CompareBeforeWrite, Quiet: Boolean): Boolean;
const
  cStyle: array[Boolean] of TTextLineBreakStyle = (tlbsLF,tlbsCRLF);
var
  F: TFileStream;
  Buffer, WorkBuffer, Preamble: TBytes;
  Encoding: TEncoding;
  Offset: Integer;
  WorkBufferConverted: Boolean;
  Modified: Boolean;
begin
  // don't convert binary files
  Result := False;
  if not IsTextFile(Filename) then
    Exit;

  F := TFileStream.Create(Filename, fmOpenReadWrite or fmShareExclusive);
  try
    SetLength(Buffer, F.Size);
    if F.Size > 0 then
    begin
      F.Read(Buffer[0], F.Size);
      Offset := TEncoding.GetBufferEncoding(Buffer, Encoding);
      Preamble := nil;
      if Offset > 0 then
        Preamble := Copy(Buffer, 0, Offset);
      WorkBufferConverted := False;
      if (Encoding <> TEncoding.ASCII) and (Encoding <> TEncoding.Default) and (Encoding <> TEncoding.UTF8) then
      begin
        WorkBufferConverted := True;
        WorkBuffer := TEncoding.Convert(Encoding, TEncoding.UTF8, Buffer, Offset, Length(Buffer) - Offset);
        Offset := 0;
      end
      else
        WorkBuffer := Buffer;

      Buffer := nil;
      Modified := AdjustByteBufferLineBreaks(WorkBuffer, Offset, Buffer, cStyle[ToWindows]);
      if not Modified then
      begin
        if CompareBeforeWrite and not Quiet then
          writeln(ExtractFilename(Filename), ' not converted');
      end
      else
      begin
        WorkBuffer := nil; // release unused memory
        F.Size := 0;
        if WorkBufferConverted then
        begin
          WorkBuffer := Buffer;
          Buffer := TEncoding.Convert(TEncoding.UTF8, Encoding, WorkBuffer, 0, Length(WorkBuffer));
          WorkBuffer := nil; // release unused memory
        end;

        if Preamble <> nil then
          F.Write(Preamble[0], Length(Preamble));
        if Buffer <> nil then
          F.Write(Buffer[0], Length(Buffer));

        if not Quiet then
          writeln(ExtractFilename(Filename), ' converted');
      end;
    end;
  finally
    F.Free;
  end;
  Result := True;
end;

function ConvertFiles(const FileMask: string; ToWindows, Recurse, CompareBeforeWrite, Quiet: Boolean): Integer;
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
        else if ConvertFile(APath + FindData.cFileName, ToWindows, CompareBeforeWrite, Quiet) then
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
          Inc(Result,ConvertFiles(IncludeTrailingPathdelimiter(APath + FindData.cFileName) + ExtractFilename(Filemask), ToWindows, True, CompareBeforeWrite, Quiet));
      until not FindNextFile(SearchHandle,FindData);
    finally
      Windows.FindClose(SearchHandle);
    end;
  end;
end;

procedure Run;
const
  cCurrentOS:array[Boolean] of string = (
    '(CRLF->LF)',
    '(LF->CRLF)'
  );
var
  ToWindows, Recurse, CompareBeforeWrite, Quiet: Boolean;
  i, Count: Integer;
  Param: string;
begin
  // cmd line: -l *.pas *.dfm *.txt -c -w *.xfm
  // where
  // -l - convert CRLF to LF (to linux)
  // -w - convert LF to CRLF (to windows)
  // -c - check content: only write if file has changed (default)
  // -u - never check content: always write
  writeln('');
  writeln('JEDI CR(LF) version 0.2: LF->CRLF and CRLF->LF converter.');

  Count := 0;
  if ParamCount = 0 then
  begin
    ShowHelp;
    Exit;
  end;
  Recurse := False;
  // set depending on target
  ToWindows := True;
  {$IFDEF LINUX}
  ToWindows := False;
  {$ENDIF}
  CompareBeforeWrite := False;
  Quiet := False;
  for i := 1 to ParamCount do
  begin
    Param := ParamStr(i);
    if SameText(Param, '/l') or SameText(Param, '-l') then
    begin
      ToWindows := False;
      if not Quiet then
        writeln('Converting ', cCurrentOS[ToWindows],':');
      Continue;
    end
    else if SameText(Param,'/w') or SameText(Param, '-w') then
    begin
      ToWindows := True;
      if not Quiet then
        writeln('Converting ', cCurrentOS[ToWindows], ':');
      Continue;
    end
    else if SameText(Param, '/?') or SameText(Param, '-?') or
            SameText(Param, '/h') or SameText(Param, '-h')then
    begin
      ShowHelp;
      Exit;
    end
    else if SameText(Param, '/c') or SameText(Param, '-c') then
    begin
      CompareBeforeWrite := True;
      Continue;
    end
    else if SameText(Param, '/u') or SameText(Param, '-u') then
    begin
      CompareBeforeWrite := False;
      Continue;
    end
    else if SameText(Param, '/s') or SameText(Param, '-s') then
    begin
      Recurse := True;
      Continue;
    end
    else if SameText(Param, '/q') or SameText(Param, '-q') then
    begin
      Quiet := True;
      Continue;
    end
    else
      Inc(Count, ConvertFiles(ExpandUNCFilename(Param), ToWindows, Recurse, CompareBeforeWrite, Quiet));
  end;
  writeln('');
  writeln('Done: ', Count, ' files converted.');
end;

end.
