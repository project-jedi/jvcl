{$I jvcl.inc}

unit dcUtils;

interface

procedure Run;

implementation

uses
  SysUtils, Classes, JTools, TypInfo,
  {$IFNDEF DELPHI6_UP}
  Consts,
  {$ELSE}
  RTLConsts,
  {$ENDIF}
  JvConsts; // (rom) for sLineBreak  no dependencies to packages

{ format of skiplist file:
<classname>.<property>

example:
*.DesignSize // applies to DesignSize in all classes
TPageControl.TabIndex, // TabIndex for TPageControl only
}

function IsBinDFM(Stream: TStream): boolean;
var
  ASignature: byte;
begin
  Stream.Read(ASignature, sizeof(ASignature));
  Result := ASignature = $FF;
  Stream.Seek(-sizeof(ASignature), soFromCurrent);
end;

function CleanDFM(Input, Output: TStream; SkipList: TStrings; SkipUnicode: boolean): boolean;
var
  NestingLevel: Integer;
  SaveSeparator: Char;
  Reader: TReader;
  Writer: TWriter;
  ClassName, ObjectName, PropName: string;
  tmpStream: TMemoryStream;

  procedure WriteIndent;
  const
    Blanks: array[0..1] of Char = '  ';
  var
    I: Integer;
  begin
    for I := 1 to NestingLevel do
      Writer.Write(Blanks, SizeOf(Blanks));
  end;

  procedure WriteStr(const S: string);
  begin
    Writer.Write(S[1], Length(S));
  end;

  procedure NewLine;
  begin
    WriteStr(sLineBreak);
    WriteIndent;
  end;

  procedure ConvertValue; forward;

  procedure ConvertHeader;
  var
    Flags: TFilerFlags;
    Position: Integer;
  begin
    Reader.ReadPrefix(Flags, Position);
    ClassName := Reader.ReadStr;
    ObjectName := Reader.ReadStr;
    WriteIndent;
    if ffInherited in Flags then
      WriteStr('inherited ')
    else if ffInline in Flags then
      WriteStr('inline ')
    else
      WriteStr('object ');
    if ObjectName <> '' then
    begin
      WriteStr(ObjectName);
      WriteStr(': ');
    end;
    WriteStr(ClassName);
    if ffChildPos in Flags then
    begin
      WriteStr(' [');
      WriteStr(IntToStr(Position));
      WriteStr(']');
    end;

    if ObjectName = '' then
      ObjectName := ClassName; // save for error reporting

    WriteStr(sLineBreak);
  end;

  procedure ConvertBinary;
  const
    BytesPerLine = 32;
  var
    MultiLine: Boolean;
    I: Integer;
    Count: Longint;
    Buffer: array[0..BytesPerLine - 1] of Char;
    Text: array[0..BytesPerLine * 2 - 1] of Char;
  begin
    Reader.ReadValue;
    WriteStr('{');
    Inc(NestingLevel);
    Reader.Read(Count, SizeOf(Count));
    MultiLine := Count >= BytesPerLine;
    while Count > 0 do
    begin
      if MultiLine then
        NewLine;
      if Count >= 32 then
        I := 32
      else
        I := Count;
      Reader.Read(Buffer, I);
      BinToHex(Buffer, Text, I);
      Writer.Write(Text, I * 2);
      Dec(Count, I);
    end;
    Dec(NestingLevel);
    WriteStr('}');
  end;

  procedure ConvertProperty; forward;

  procedure ConvertValue;
  const
    LineLength = 64;
  var
    I, J, K, L: Integer;
    S: string;
    W: WideString;
    LineBreak: Boolean;
  begin
    case Reader.NextValue of
      vaList:
        begin
          Reader.ReadValue;
          WriteStr('(');
          Inc(NestingLevel);
          while not Reader.EndOfList do
          begin
            NewLine;
            ConvertValue;
          end;
          Reader.ReadListEnd;
          Dec(NestingLevel);
          WriteStr(')');
        end;
      vaInt8, vaInt16, vaInt32:
        WriteStr(IntToStr(Reader.ReadInteger));
      vaExtended:
        WriteStr(FloatToStr(Reader.ReadFloat));
      vaSingle:
        WriteStr(FloatToStr(Reader.ReadSingle) + 's');
      vaCurrency:
        WriteStr(FloatToStr(Reader.ReadCurrency * 10000) + 'c');
      vaDate:
        WriteStr(FloatToStr(Reader.ReadDate) + 'd');
      vaWString{$IFDEF DELPHI6_UP}, vaUTF8String{$ENDIF}:
        begin
          W := Reader.ReadWideString;
          L := Length(W);
          if L = 0 then
            WriteStr('''''')
          else
          begin
            I := 1;
            Inc(NestingLevel);
            try
              if L > LineLength then
                NewLine;
              K := I;
              repeat
                LineBreak := False;
                if (W[I] >= ' ') and (W[I] <> '''') and (Ord(W[i]) <= 127) then
                begin
                  J := I;
                  repeat
                    Inc(I)
                  until (I > L) or (W[I] < ' ') or (W[I] = '''') or
                    ((I - K) >= LineLength) or (Ord(W[i]) > 127);
                  if ((I - K) >= LineLength) then
                    LineBreak := True;
                  WriteStr('''');
                  while J < I do
                  begin
                    WriteStr(Char(W[J]));
                    Inc(J);
                  end;
                  WriteStr('''');
                end
                else
                begin
                  WriteStr('#');
                  if (Ord(W[I]) > 255) and SkipUnicode then
                  begin
                    Result := True;
                    WriteStr('32');
                  end
                  else
                    WriteStr(IntToStr(Ord(W[I])));
                  Inc(I);
                  if ((I - K) >= LineLength) then
                    LineBreak := True;
                end;
                if LineBreak and (I <= L) then
                begin
                  WriteStr(' +');
                  NewLine;
                  K := I;
                end;
              until I > L;
            finally
              Dec(NestingLevel);
            end;
          end;
        end;
      vaString, vaLString:
        begin
          S := Reader.ReadString;
          L := Length(S);
          if L = 0 then
            WriteStr('''''')
          else
          begin
            I := 1;
            Inc(NestingLevel);
            try
              if L > LineLength then
                NewLine;
              K := I;
              repeat
                LineBreak := False;
                if (S[I] >= ' ') and (S[I] <> '''') then
                begin
                  J := I;
                  repeat
                    Inc(I)
                  until (I > L) or (S[I] < ' ') or (S[I] = '''') or
                    ((I - K) >= LineLength);
                  if ((I - K) >= LineLength) then
                  begin
                    LIneBreak := True;
                    if ByteType(S, I) = mbTrailByte then
                      Dec(I);
                  end;
                  WriteStr('''');
                  Writer.Write(S[J], I - J);
                  WriteStr('''');
                end
                else
                begin
                  WriteStr('#');
                  WriteStr(IntToStr(Ord(S[I])));
                  Inc(I);
                  if ((I - K) >= LineLength) then
                    LineBreak := True;
                end;
                if LineBreak and (I <= L) then
                begin
                  WriteStr(' +');
                  NewLine;
                  K := I;
                end;
              until I > L;
            finally
              Dec(NestingLevel);
            end;
          end;
        end;
      vaIdent, vaFalse, vaTrue, vaNil, vaNull:
        WriteStr(Reader.ReadIdent);
      vaBinary:
        ConvertBinary;
      vaSet:
        begin
          Reader.ReadValue;
          WriteStr('[');
          I := 0;
          while True do
          begin
            S := Reader.ReadStr;
            if S = '' then
              Break;
            if I > 0 then
              WriteStr(', ');
            WriteStr(S);
            Inc(I);
          end;
          WriteStr(']');
        end;
      vaCollection:
        begin
          Reader.ReadValue;
          WriteStr('<');
          Inc(NestingLevel);
          while not Reader.EndOfList do
          begin
            NewLine;
            WriteStr('item');
            if Reader.NextValue in [vaInt8, vaInt16, vaInt32] then
            begin
              WriteStr(' [');
              ConvertValue;
              WriteStr(']');
            end;
            WriteStr(sLineBreak);
            Reader.CheckValue(vaList);
            Inc(NestingLevel);
            while not Reader.EndOfList do
              ConvertProperty;
            Reader.ReadListEnd;
            Dec(NestingLevel);
            WriteIndent;
            WriteStr('end');
          end;
          Reader.ReadListEnd;
          Dec(NestingLevel);
          WriteStr('>');
        end;
      vaInt64:
        WriteStr(IntToStr(Reader.ReadInt64));
    else
      raise EReadError.CreateResFmt(@sPropertyException,
        [ObjectName, DotSep, PropName, IntToStr(Ord(Reader.NextValue))]);
    end;
  end;

  procedure ConvertProperty;
  var
    APos: integer;
  begin
    // sve current position
    APos := Writer.Position;
    WriteIndent;
    PropName := Reader.ReadStr; // save for error reporting
    WriteStr(PropName);
    WriteStr(' = ');
    ConvertValue;
    WriteStr(sLineBreak);
    // Check if the current property should be removed
    if (SkipList <> nil) and ((SkipList.IndexOf(ClassName + '.' + PropName) >= 0) or
      (SkipList.IndexOf('*.' + PropName) >= 0)) then
    begin
      Result := true;
      Writer.Position := APos; // go back to previous position
    end;
  end;

  procedure ConvertObject;
  begin
    ConvertHeader;
    Inc(NestingLevel);
    while not Reader.EndOfList do
      ConvertProperty;
    Reader.ReadListEnd;
    while not Reader.EndOfList do
      ConvertObject;
    Reader.ReadListEnd;
    Dec(NestingLevel);
    WriteIndent;
    WriteStr('end' + sLineBreak);
  end;
begin
  Result := false; // result is set to true in ConvertProperty if we skip anything
  tmpStream := TMemoryStream.Create;
  try
    // we don't want to rewrite everything in CleanDFM,
    // so convert text-> binary if necessary
    // (the other option is to convert Output to text after writing it...)
    if not IsBinDFM(Input) then
    begin
      ObjectTextToResource(Input, tmpStream);
      tmpStream.Seek(0, soFromBeginning);
      Input := tmpStream;
    end;

    Input.ReadResHeader;
    NestingLevel := 0;
    Reader := TReader.Create(Input, 4096);
    SaveSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    try
      Writer := TWriter.Create(Output, 4096);
      try
        Reader.ReadSignature;
        ConvertObject;
      finally
        Writer.Free;
      end;
    finally
      DecimalSeparator := SaveSeparator;
      Reader.Free;
    end;
  finally
    tmpStream.Free;
  end;
end;

procedure ShowHeader;
begin
  writeln('');
  writeln('JEDI DFMCleaner 0.1: DFM property cleaner.');
  writeln('=========================================');
end;

procedure ShowHelp;
begin
  writeln('');
  writeln('Usage:');
  writeln('dc <options> <filemask> <filemask>...');
  writeln('');
  writeln('where <options> are');
  writeln('-i - replaces in-line (output overwrites input)');
  writeln('-s - recurse into subfolders');
  writeln('-u - skip unicode chars');  
  writeln('-f<filename> - where <filename> is a text file that contains the');
  writeln('names of properties to remove.');
  writeln('');
  writeln('<filemask> - a space-delimited list of filenames with optional wildcards.');
  writeln('');
  writeln('');
end;

function ParseDFM(const Filename: string; ASkipList: TStrings; ReplaceInline,
  SkipUnicode: boolean): boolean;
var
  F: TFileStream;
  F2:TMemoryStream;
begin
  Result := false;
  try
    F := TFileStream.Create(Filename, fmOpenReadWrite or fmShareExclusive);
    F2 := TMemoryStream.Create;
    try
      if CleanDFM(F, F2, ASkipList, SkipUnicode) then // only write if something changed
      begin
        Result := true;
        if ReplaceInline then
        begin
          F.Size := 0;
          F.CopyFrom(F2,0);
          writeln('Writing ',Filename,'...');
        end
        else
        begin
          F2.SaveToFile(ChangeFileExt(Filename, '.txt'));
          writeln('Writing ',ChangeFileExt(Filename, '.txt'),'...');
        end;
      end;
    finally
      F.Free;
      F2.Free;
    end;
  except
    on E: Exception do
      writeln('ERROR: ', E.Message, ' (', Filename, ')');
  end;
end;

function ParseFiles(const Filemask: string; ASkipList: TStrings; ReplaceInline,
  Recurse, SkipUnicode: boolean; var FilesFound: integer): integer;
var
  F: TSearchRec;
begin
  Result := 0;
  if FindFirst(Filemask, faAnyFile, F) = 0 then
  begin
    repeat
      Inc(FilesFound);
      if ParseDFM(ExtractFilePath(Filemask) + F.Name, ASkipList, ReplaceInline, SkipUnicode) then
        Inc(Result)
    until FindNext(F) <> 0;
    FindClose(F);
  end;
  if Recurse then
  begin
    if FindFirst(ExtractFilePath(Filemask) + '*.*', faDirectory, F) = 0 then
    begin
      repeat
        if (F.Attr and faDirectory = faDirectory) and (F.Name <> '.') and (F.Name <> '..') then
          Result := Result + ParseFiles(ExtractFilePath(Filemask) + F.Name + '\' + ExtractFileName(Filemask), ASkipList, ReplaceInline, Recurse, SkipUnicode, FilesFound);
      until FindNext(F) <> 0;
      FindClose(F);
    end;
  end;
end;

procedure Run;
var
  i,ACount,FilesFound: integer;
  CmdSwitch: string;
  ReplaceInline, Recurse, SkipUnicode: boolean;
  SkipList: TStringlist;
begin
  ShowHeader;
  if (ParamCount = 0) or GetCmdSwitchValue('h', ['-', '/'], CmdSwitch, true) or GetCmdSwitchValue('?', ['-', '/'], CmdSwitch, true) then
  begin
    ShowHelp;
    Exit;
  end;
  try
    ACount := 0;
    FilesFound := 0;
    Recurse := GetCmdSwitchValue('s', ['-', '/'], CmdSwitch, true);
    ReplaceInline := GetCmdSwitchValue('i', ['-', '/'], CmdSwitch, true);
    SkipUnicode := GetCmdSwitchValue('u', ['-', '/'], CmdSwitch, true);
    if not GetCmdSwitchValue('f', ['-', '/'], CmdSwitch, true) or not FileExists(ExpandUNCFileName(CmdSwitch)) then
      raise Exception.Create('Config file not found!');
    // done: add handling of skiplist and subfolders
    SkipList := TStringlist.Create;
    try
      SkipList.LoadFromFile(ExpandUNCFileName(CmdSwitch));
      SkipList.Sorted := true; // faster lookup
      for i := 1 to ParamCount do
        if not (ParamStr(i)[1] in ['-', '/']) then
          ACount := ACount + ParseFiles(ExpandUNCFileName(ParamStr(i)), SkipList, ReplaceInline, Recurse, SkipUnicode, FilesFound);
    finally
      SkipList.Free;
    end;
    writeln('Done: ',FilesFound,' files found, ', ACount,' files cleaned');
  except
    on E: Exception do
    begin
      writeln('ERROR: ',E.Message);
    end;
  end;
end;

end.

