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
  writeln('USAGE: pg2want src dest [fixed] [dir]');
  writeln('where');
  writeln('src  - the pg xml file(s) to read from. Accepts wildcards.');
  writeln('dest - the file to write to. Defaults to want.xml in the current directory.');
  writeln('If dest exists, it will be overwritten.');
  writeln('fixed - an xml fragment file with include/exclude items that should be added to each fileset.');
  writeln('dir - the root directory to replace relative paths with in the pg xml file(s).');
end;

function ParseItem(const ANode: TJvSimpleXMLElem; const Dest: TStrings): integer;
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
            tmp := Format('    <include name="%s%s" />', [PrefixPath(tmp), RemoveRelativePath(tmp)]);
            if Dest.IndexOf(tmp) <> Dest.Count - 1 then // check for Linux/Windows double inclusion
              Dest.Add(tmp);
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
    Dest.Add(Format('    <include name="%s%s" />', [PrefixPath(AFile), RemoveRelativePath(AFile)]));
    Result := 1;
    FindAdditional(AFile, Dest);
  end;
end;

function ParseFile(const SrcFile: string; const XML: TJvSimpleXML; const Dest, Fixed: TStrings): integer;
var
  i: integer;
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
    S := Format('<property name="%s" value="%s${shortversion}.zip" />', [ZipName, PackName]);
    if Dest.IndexOf(S) < 0 then
      Dest.Add(S);
    S := Format('<delete file="${%s}" />', [ZipName]);
    if Dest.IndexOf(S) < 0 then
      Dest.Add(S);
    S := Format('<zip zipfile="${%s}">', [ZipName]);
    if Dest.IndexOf(S) < 0 then
    begin
      Dest.Add(S);
      S := '';
      Dest.Add('  <fileset>');
      Dest.Add(Format('    <exclude name="%s${%s}" />', [PrefixPath(ZipName), ZipName]));
      Dest.Add(Format('    <include name="%s**/%s*" />', [PrefixPath(PackName), PackName]));
      if Fixed.Count > 0 then
        Dest.AddStrings(Fixed);
    end;
    for i := 0 to Node.Items.Count - 1 do
      if AnsiSameText(Node.Items[i].Name, 'File') then
        Result := ParseItem(Node.Items[i], Dest);
    if S <> '' then // this relies on the fact that the src xml files are sorted and come in pairs
    begin
      Dest.Add('  </fileset>');
      Dest.Add('</zip>');
    end;
  end;
end;

function ConvertToWant(const Src, Dest, Fixed: string): integer;
var
  F: TSearchRec;
  APath: string;
  XML: TJvSimpleXML;
  Dst, AFiles, AFixed: TStringlist;
  i: integer;
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
    Dst.Add('<project name="JVCL Separate Zips" default="separate">');
    Dst.Add('<target name="separate">');
    for i := 0 to AFiles.Count - 1 do
      Inc(Result, ParseFile(AFiles[i], XML, Dst, AFixed));
    Dst.Add('</target>');
    Dst.Add('</project>');
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
  GlobalDir := ParamStr(4);
//  if GlobalDir = '' then
//    GlobalDir := ExtractFilePath(Src); // assume source folder is "root"
  if FileExists(Dest) then
    RenameFile(Dest, Dest + '.bak');
  i := ConvertToWant(Src, Dest, Fixed);
  writeln(i, ' xml file(s) parsed.');
  if FileExists(Dest + '.bak') then
    DeleteFile(Dest + '.bak');
end;

end.

