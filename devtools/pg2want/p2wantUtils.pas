unit p2wantUtils;

interface
procedure Run;

implementation
uses
  Windows, SysUtils, Classes, JvSimpleXML;
var
  GlobalDir: string = ''; 

function ConvertBackSlash(const S: string): string;
begin
  Result := StringReplace(S, '\', '/', [rfReplaceAll]);
end;

function ConvertFrontSlash(const S: string): string;
begin
  Result := StringReplace(S, '/', '\', [rfReplaceAll]);
end;

function RemoveRelativePath(const S:string):string;
begin
  Result := StringReplace(ConvertBackSlash(S),'../','',[rfReplaceAll]);
  Result := StringReplace(Result,'..','',[rfReplaceAll]);
  Result := StringReplace(Result,'//','/',[rfReplaceAll]);
end;

function ExtractRelativePathPart(const S:string):string;
var i:integer;
begin
  i := 1;
  while (i <= Length(S)) and (S[i] in ['.','/','\']) do
    Inc(i);
  Result := Copy(S,1,i-1);
  if Result = '' then
    Result := S;
end;

function PrefixPath(const S: string): string;
begin
  if GlobalDir <> '' then
    Result := GlobalDir
  else
    Result := ConvertBackSlash(ExtractRelativePathPart(ConvertFrontSlash(S)));
end;

procedure ShowHelp;
begin
  writeln('p2want: converts package generator xml files to want xml files');
  writeln('USAGE: pg2want -is src dest [fixed] [dir]');
  writeln('where');
  writeln('src  - the pg xml file(s) to read from. Accepts wildcards.');
  writeln('dest - the file to write to. Defaults to want.xml in the current directory.');
  writeln('If dest exists, it will be overwritten.');
  writeln('fixed - an xml fragment file with include/exclude items that should be added to each fileset.');
  writeln('dir - the root directory to replace relative paths with in the pg xml file(s).');
end;

procedure AddRow(Strings:TStrings; Indent:integer;const S:string);
begin
  if Indent < 0 then Indent := 0;
  Strings.Add(Format('%s%s',[StringOfChar(' ', Indent),S]));
end;

function ParseItem(const ANode: TJvSimpleXMLElem; const Dest: TStrings;Indent:integer): integer;
var
  AFile: string;
  procedure FindAdditional(const AFile: string; Dest: TStrings);
  var
    i, j: integer;
    S: TStringlist;
    tmp: string;
  begin
    if not FileExists(ExpandUNCFilename(AFile)) then
    begin
//      writeln(Afile, ' not found!');
      Exit;
    end;
    writeln('Parsing ', AFile, '...');
    S := TStringlist.Create;
    try
      S.LoadFromFile(ExpandUNCFilename(AFile));
      for i := 0 to S.Count - 1 do
      begin
        j := Pos('{$R ', S[i]);
        if j = 0 then
          j := Pos('{$RESOURCE ', S[i]);
        if j = 0 then
          j := Pos('{$L ', S[i]);
        if j = 0 then
          j := Pos('{$LINK ', S[i]);
//        if j = 0 then
//          j := Pos('{$I ',S[i]);
//        if j = 0 then
//          j := Pos('{$INCLUDE ',S[i]);
        if j > 0 then
        begin
          tmp := trim(S[i]);
          tmp := Copy(tmp, Pos(' ', tmp) + 1, MaxInt);
          tmp := trim(Copy(tmp, 1, Pos('}', tmp) - 1));
          tmp := ConvertFrontSlash(StringReplace(tmp, '*', ChangeFileExt(ExtractFileName(AFile), ''), []));
          if tmp <> '' then
          begin // tmp path could be in unix format
            if ExtractFilePath(tmp) = '' then
              tmp := ExtractFilePath(AFile) + ExtractFilename(tmp);
            tmp := Format('<include name="%s%s" />', [PrefixPath(tmp), RemoveRelativePath(tmp)]);
            if Dest.IndexOf(tmp) <> Dest.Count - 1 then // check for Linux/Windows double inclusion
              AddRow(Dest,Indent,tmp);
          end;
        end
      end;
    finally
      S.Free;
    end;
  end;
begin
  Result := 0;
  AFile := ANode.Properties.Value('Name', '');
  if AFile <> '' then
  begin
    AddRow(Dest, Indent, Format('<include name="%s%s" />', [PrefixPath(AFile), RemoveRelativePath(AFile)]));
    Result := 1;
    FindAdditional(AFile, Dest);
  end;
end;

function ParseFile(const SrcFile: string; const XML: TJvSimpleXML; const Dest, Fixed: TStrings; Indent:integer): integer;
var
  i, j: integer;
  Node: TJvSimpleXMLElem;
  S, PackName, ZipName: string;
begin
  Result := 0;
  XML.LoadFromFile(SrcFile);
  Node := XML.Root;
  PackName := Node.Properties.Value('Name', '');
  ZipName := AnsiLowerCase(Copy(PackName, 3, MaxInt)) + 'zip';
  if Node <> nil then
    for i := 0 to Node.Items.Count - 1 do
      if AnsiSameText(Node.Items[i].Name, 'Contains') then
      begin
        Node := Node.Items[i];
        Break;
      end;
  if Node <> nil then
  begin
    Inc(Indent,2);
    S := Format('%s<property name="%s" value="%s${shortversion}.zip" />', [StringOfChar(' ',Indent),ZipName, PackName]);
    if Dest.IndexOf(S) < 0 then
      AddRow(Dest, 0, S);
    S := Format('%s<delete file="${%s}" />', [StringOfChar(' ',Indent),ZipName]);
    if Dest.IndexOf(S) < 0 then
      AddRow(Dest, 0, S);
    S := Format('%s<zip zipfile="${%s}">', [StringOfChar(' ',Indent),ZipName]);
    if Dest.IndexOf(S) < 0 then
    begin
      AddRow(Dest, 0, S);
      S := '';
      Inc(Indent, 2);
      AddRow(Dest, Indent, '<fileset>');
      Inc(Indent, 2);
      AddRow(Dest, Indent, Format('<exclude name="%s${%s}" />', [PrefixPath(ZipName), ZipName]));
      AddRow(Dest, Indent, Format('<include name="%s**/%s*" />', [PrefixPath(PackName), PackName]));
      for j := 0 to Fixed.Count - 1 do
        AddRow(Dest, Indent, Fixed[j]);
      Dec(Indent, 2);
      Dec(Indent, 2);
    end;
    Inc(Indent, 4);
    for i := 0 to Node.Items.Count - 1 do
      if AnsiSameText(Node.Items[i].Name, 'File') then
        Result := ParseItem(Node.Items[i], Dest, Indent);
    Dec(Indent, 4);
    if S <> '' then // this relies on the fact that the src xml files are sorted and come in pairs
    begin
      Inc(Indent, 2);
      AddRow(Dest, Indent,'</fileset>');
      Dec(Indent, 2);
      AddRow(Dest, Indent,'</zip>');
    end;
  end;
end;

function ConvertToWant(const Src, Dest, Fixed: string): integer;
var
  F: TSearchRec;
  APath: string;
  XML: TJvSimpleXML;
  Dst, AFiles, AFixed: TStringlist;
  i, Indent: integer;
begin
  Result := 0;
  APath := ExtractFilePath(Src);
  XML := TJvSimpleXML.Create(nil);
  Dst := TStringlist.Create;
  AFiles := TStringlist.Create;
  AFixed := TStringlist.Create;
  try
    if FileExists(Fixed) then
      AFixed.LoadFromFile(Fixed);
    if FindFirst(Src, faAnyFile and not faDirectory and not faVolumeID, F) = 0 then
    begin
      repeat
        if F.Attr and faDirectory = 0 then
          AFiles.Add(APath + F.Name);
      until FindNext(F) <> 0;
      FindClose(F);
    end;
    AFiles.Sort;
    Indent := 0;
    AddRow(Dst, Indent,'<project name="JVCL Separate Zips" default="separate">');
    Inc(Indent, 2);
    AddRow(Dst, Indent,'<target name="separate">');
    for i := 0 to AFiles.Count - 1 do
      Inc(Result, ParseFile(AFiles[i], XML, Dst, AFixed, Indent));
    AddRow(Dst, Indent,'</target>');
    Dec(Indent,2);
    AddRow(Dst, Indent,'</project>');
    if (Result > 0) then
      Dst.SaveToFile(Dest);
  finally
    XML.Free;
    Dst.Free;
    AFiles.Free;
  end;
end;

procedure Run;
var
  i: integer;
  Src, Dest, Fixed: string;
begin
  if ParamCount < 2 then
  begin
    ShowHelp;
    Halt(1);
    Exit;
  end;
  Src := ExpandUNCFilename(ParamStr(1));
  if ParamStr(2) = '' then
    Dest := ExpandUNCFilename('want.xml')
  else
    Dest := ExpandUNCFilename(ParamStr(2));
  Fixed := ExpandUNCFilename(ParamStr(3));
  if ParamCount >= 4 then
    GlobalDir := ParamStr(4);
//    GlobalDir := ExtractFilePath(Src); // assume source folder is "root"
  if FileExists(Dest) then
    RenameFile(Dest, Dest + '.bak');
  i := ConvertToWant(Src, Dest, Fixed);
  writeln(i, ' xml file(s) parsed.');
  if FileExists(Dest + '.bak') then
    DeleteFile(Dest + '.bak');
end;

end.

