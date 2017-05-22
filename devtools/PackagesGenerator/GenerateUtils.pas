unit GenerateUtils;

{$I jvcl.inc}

interface

uses
  Contnrs, Classes,
  JclSimpleXml,
  PackageInformation, GenerateTargets, GenerateAlias, GenerateReplacements, DefinesConditionParser;

function IsTrimmedStartsWith(const SubStr, TrimStr: string): Boolean;
function IsTrimmedString(const TrimStr, S: string): Boolean;
function StartsWith(const SubStr, S: string): Boolean;
procedure StrReplaceLines(Lines: TStrings; const Search, Replace: string);
function MacroReplace(var Text: string; MacroChar: Char;
  const Macros: array of string; CaseSensitive: Boolean = True): Boolean;
procedure MacroReplaceLines(Lines: TStrings; MacroChar: Char;
  const Macros: array of string; CaseSensitive: Boolean = True);
function VerifyModelNode(Node : TJclSimpleXmlElem; var ErrMsg : string) : Boolean;
function GetUnitName(const FileName : string) : string;
function NowUTC : TDateTime;
function HasFileChanged(const OutFileName, TemplateFileName: string;
  OutLines: TStrings; TimeStampLine: Integer): Boolean;
procedure AdjustEndingSemicolon(Lines: TStrings);
procedure EnumeratePackages(const Path : string; packages : TStrings);



implementation

uses
  Windows, SysUtils,
  JclStrings, JclFileUtils, JclDateTime;

function IsTrimmedStartsWith(const SubStr, TrimStr: string): Boolean;
var
  l, r, Len, SLen, i: Integer;
begin
  Result := False;

  l := 1;
  r := Length(TrimStr);
  while (l < r) and (TrimStr[l] <= #32) do
    Inc(l);
  while (r > l) and (TrimStr[r] <= #32) do
    Dec(r);
  if r > l then
  begin
    Len := r - l + 1;
    SLen := Length(SubStr);
    if Len >= SLen then
    begin
      Dec(l);
      for i := 1 to SLen do
        if SubStr[i] <> TrimStr[l + i] then
          Exit;
      Result := True;
    end;
  end;
end;

function IsTrimmedString(const TrimStr, S: string): Boolean;
var
  l, r, Len, SLen, i: Integer;
begin
  Result := False;

  l := 1;
  r := Length(TrimStr);
  while (l < r) and (TrimStr[l] <= #32) do
    Inc(l);
  while (r > l) and (TrimStr[r] <= #32) do
    Dec(r);
  if r > l then
  begin
    Len := r - l + 1;
    SLen := Length(S);
    if Len = SLen then
    begin
      Dec(l);
      for i := 1 to SLen do
        if S[i] <> TrimStr[l + i] then
          Exit;
      Result := True;
    end;
  end;
end;

function StartsWith(const SubStr, S: string): Boolean;
var
  i, Len: Integer;
begin
  Result := False;
  len := Length(SubStr);
  if Len <= Length(S) then
  begin
    for i := 1 to Len do
      if SubStr[i] <> S[i] then
        Exit;
    Result := True;
  end;
end;

procedure StrReplaceLines(Lines: TStrings; const Search, Replace: string);
var
  i: Integer;
  S: string;
begin
  for i := 0 to Lines.Count - 1 do
  begin
    S := Lines[i];
    if Pos(Search, S) > 0 then
    begin
      StrReplace(S, Search, Replace, [rfReplaceAll]);
      Lines[i] := S;
    end;
  end;
end;

function MacroReplace(var Text: string; MacroChar: Char;
  const Macros: array of string; CaseSensitive: Boolean = True): Boolean;
const
  Delta = 1024;
var
  Index, i, Count, Len, SLen, MacroHigh: Integer;
  S: string;
  Found: Boolean;
  Cmp: function(const Str1, Str2: PChar; MaxLen: Cardinal): Integer;
begin
  Result := False;
  if CaseSensitive then
    Cmp := StrLComp
  else
    Cmp := StrLIComp;

  MacroHigh := Length(Macros) div 2 - 1;
  Len := Length(Text);
  i := 1;
  SetLength(S, Delta);
  SLen := 0;
  while i <= Len do
  begin
    Count := 0;
   // add normal chars in one step
    while (i <= Len) and (Text[i] <> MacroChar) do
    begin
      Inc(Count);
      Inc(i);
    end;
    if Count > 0 then
    begin
      if SLen + Count > Length(S) then
        SetLength(S, SLen + Count + Delta);
      Move(Text[i - Count], S[SLen + 1], Count * SizeOf(Char));
      Inc(SLen, Count);
    end;

    if i <= Len then
    begin
     // replace macros
      Found := False;
      for Index := 0 to MacroHigh do
      begin
        Count := Length(Macros[Index * 2]);
        if Cmp(PChar(Pointer(Text)) + i, PChar(Macros[Index * 2]), Count) = 0 then
        begin
          Inc(i, Count);
          Count := Length(Macros[Index * 2 + 1]);
          if Count > 0 then
          begin
            if SLen + Count > Length(S) then
              SetLength(S, SLen + Count + Delta);
            Move(Macros[Index * 2 + 1][1], S[SLen + 1], Count * SizeOf(Char));
            Inc(SLen, Count);
          end;
          Result := True;
          Found := True;
          Break;
        end;
      end;
      if not Found then
      begin
        // copy macro-text
        if Macros[0][Length(Macros[0])] = MacroChar then
        begin
          Count := 1;
          while (i + Count <= Len) and (Text[i + Count] <> MacroChar) do
            Inc(Count);
          Inc(Count);
          if SLen + Count > Length(S) then
            SetLength(S, SLen + Count + Delta);
          Move(Text[i], S[SLen + 1], Count * SizeOf(Char));
          Inc(SLen, Count);
          Inc(i, Count - 1);
        end;
      end;
    end;
    Inc(i);
  end;
  SetLength(S, SLen);
  Text := S;
end;

procedure MacroReplaceLines(Lines: TStrings; MacroChar: Char;
  const Macros: array of string; CaseSensitive: Boolean = True);
var
  i: Integer;
  S: string;
begin
  for i := 0 to Lines.Count - 1 do
  begin
    S := Lines[i];
    if MacroReplace(S, MacroChar, Macros, CaseSensitive) then
      Lines[i] := S;
  end;
end;

function VerifyModelNode(Node : TJclSimpleXmlElem; var ErrMsg : string) : Boolean;
begin
  // a valid model node must exist
  if not Assigned(Node) then
  begin
    Result := False;
    ErrMsg := 'No ''model'' node found in the ''models'' node.';
    Exit;
  end;

  // it must have a Name property
  if not Assigned(Node.Properties.ItemNamed['name']) then
  begin
    Result := False;
    ErrMsg := 'A ''model'' node must have a ''name'' property.';
    Exit;
  end;

  // it must have a prefix property
  if not Assigned(Node.Properties.ItemNamed['prefix']) then
  begin
    Result := False;
    ErrMsg := 'A ''model'' node must have a ''prefix'' property.';
    Exit;
  end;

  // it must have a format property
  if not Assigned(Node.Properties.ItemNamed['format']) then
  begin
    Result := False;
    ErrMsg := 'A ''model'' node must have a ''format'' property.';
    Exit;
  end;

  // it must have a packages property
  if not Assigned(Node.Properties.ItemNamed['packages']) then
  begin
    Result := False;
    ErrMsg := 'A ''model'' node must have a ''packages'' property.';
    Exit;
  end;

  // it must have a incfile property
  if not Assigned(Node.Properties.ItemNamed['incfile']) then
  begin
    Result := False;
    ErrMsg := 'A ''model'' node must have a ''incfile'' property.';
    Exit;
  end;

  // it must contain Targets
  if not Assigned(Node.Items.ItemNamed['targets']) then
  begin
    Result := False;
    ErrMsg := 'A ''model'' node must contain a ''targets'' node.';
    Exit;
  end;

  // it must contain Aliases
  if not Assigned(Node.Items.ItemNamed['aliases']) then
  begin
    Result := False;
    ErrMsg := 'A ''model'' node must contain a ''aliases'' node.';
    Exit;
  end;

  // if all went ok, then the node is deemed to be valid
  Result := True;
end;

function GetUnitName(const FileName : string) : string;
begin
  Result := PathExtractFileNameNoExt(FileName);
end;

function NowUTC : TDateTime;
var
  sysTime : TSystemTime;
  fileTime : TFileTime;
begin
  Windows.GetSystemTime(sysTime);
  Windows.SystemTimeToFileTime(sysTime, fileTime);
  Result := FileTimeToDateTime(fileTime);
end;

function FilesEqual(const FileName1, FileName2: string): Boolean;
const
  MaxBufSize = 65535;
var
  Stream1, Stream2: TFileStream;
  Buffer1, Buffer2: array[0..MaxBufSize - 1] of Byte;
  BufSize: Integer;
  Size: Integer;
begin
  Result := True;

  Stream1 := nil;
  Stream2 := nil;
  try
    Stream1 := TFileStream.Create(FileName1, fmOpenRead or fmShareDenyWrite);
    Stream2 := TFileStream.Create(FileName2, fmOpenRead or fmShareDenyWrite);

    Size := Stream1.Size;
    if Size <> Stream2.Size then
    begin
      Result := False;
      Exit;     // Note: the finally clause WILL be executed
    end;

    BufSize := MaxBufSize;
    while Size > 0 do
    begin
      if BufSize > Size then
        BufSize := Size;
      Dec(Size, BufSize);

      Stream1.Read(Buffer1[0], BufSize);
      Stream2.Read(Buffer2[0], BufSize);

      Result := CompareMem(@Buffer1[0], @Buffer2[0], BufSize);
      if not Result then
        Exit;    // Note: the finally clause WILL be executed
    end;
  finally
    Stream1.Free;
    Stream2.Free;
  end;
end;

function HasFileChanged(const OutFileName, TemplateFileName: string;
  OutLines: TStrings; TimeStampLine: Integer): Boolean;
var
  CurLines: TStrings;
begin
  Result := True;
  if not FileExists(OutFileName) then
    Exit;

  if OutLines.Count = 0 then
  begin
    // binary file -> compare files
    Result := not FilesEqual(OutFileName, TemplateFileName);
  end
  else
  begin
    // text file -> compare lines
    CurLines := TStringList.Create;
    try
      CurLines.LoadFromFile(OutFileName);

      if CurLines.Count <> OutLines.Count then
      begin
        Result := True;
        Exit;
      end;

      // Replace the time stamp line by the new one to ensure that this
      // won't break the comparison.
      if TimeStampLine > -1 then
        CurLines[TimeStampLine] := OutLines[TimeStampLine];

      Result := not CurLines.Equals(OutLines);
    finally
      CurLines.Free;
    end;
  end;
end;

procedure AdjustEndingSemicolon(Lines: TStrings);
var
  S: string;
  Len, Index: Integer;
begin
  if Lines.Count > 0 then
  begin
    Index := Lines.Count - 1;
    S := Lines[Index];
    Len := Length(S);

    { If the last line is a comment then we have a problem. Here we allow the
      last comment to have no comma }
    if (Len > 2) and (S[1] = '{') and (S[2] = '$') and (Index > 0) then
    begin
      Dec(Index);
      S := Lines[Index];
      Len := Length(S);
    end;
    if Len > 0 then
    begin
      if S[Len] = ',' then
      begin
        Delete(S, Len, 1);
        Lines[Index] := S;
      end;
    end;
  end;
end;

procedure EnumeratePackages(const Path : string; packages : TStrings);
var
  rec : TSearchRec;
begin
  packages.Clear;
  if FindFirst(StrEnsureSuffix(DirDelimiter, path) + 'xml' + DirDelimiter + '*.xml', faAnyFile, rec) = 0 then
  begin
    repeat
      packages.Add(PathExtractFileNameNoExt(rec.Name));
    until FindNext(rec) <> 0;
  end;
  FindClose(rec);
end;

end.
