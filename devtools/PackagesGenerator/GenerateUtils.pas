unit GenerateUtils;

interface

uses Classes;

procedure Generate(packages : TStrings; targets : TStrings; path : string);

procedure EnumerateTargets(Path : string; targets : TStrings);

procedure EnumeratePackages(Path : string; packages : TStrings);

implementation

uses SysUtils, JclStrings, JclFileUtils, JvSimpleXml;

function targetToSuffix(target : string) : string;
begin
  target := StrUpper(target);
  if target = 'BCB6' then Result := 'C6'
  else if target = 'BCB6PER' then Result := 'C6'
  else if target = 'BCB5' then Result := 'C5'
  else if target = 'D5' then Result := 'D5'
  else if target = 'D5STD' then Result := 'D5'
  else if target = 'D6' then Result := 'D6'
  else if target = 'D6PER' then Result := 'D6'
  else if target = 'D7' then Result := 'D7'
  else if target = 'D7PER' then Result := 'D7'
  else if target = 'K2' then Result := 'K2'
  else if target = 'K3' then Result := 'K3';
end;

function ShortTarget(target : string) : string;
begin
  target := StrUpper(target);
  if target = 'BCB6' then Result := 'C6'
  else if target = 'BCB6PER' then Result := 'C6p'
  else if target = 'BCB5' then Result := 'C5'
  else if target = 'D5' then Result := 'D5'
  else if target = 'D5STD' then Result := 'D5s'
  else if target = 'D6' then Result := 'D6'
  else if target = 'D6PER' then Result := 'D6p'
  else if target = 'D7' then Result := 'D7'
  else if target = 'D7PER' then Result := 'D7p'
  else if target = 'K2' then Result := 'K2'
  else if target = 'K3' then Result := 'K3';
end;

procedure ApplyTemplateAndSave(path, target, package, extension : string; template : TStrings; xml : TJvSimpleXml);
var
  OutFileName : string;
  packSuffix : string;
  NameSuffix : string;
  rootNode : TJvSimpleXmlElemClassic;
  requiredNode : TJvSimpleXmlElem;
  packageNode : TJvSimpleXmlElem;
  filesNode : TJvSimpleXmlElem;
  fileNode : TJvSimpleXmlElem;
  outFile : TStringList;
  curLine : string;
  repeatLines : string;
  I : Integer;
  j : Integer;
begin
  packSuffix := targetToSuffix(target);
  outFile := TStringList.Create;

  try

    // read the xml file
    rootNode := xml.Root;
    if rootNode.Properties.ItemNamed['Design'].BoolValue then
      NameSuffix := 'D'
    else
      NameSuffix := 'R';

    OutFileName := path + target + '\' +
                   rootNode.Properties.ItemNamed['Name'].Value +
                   PackSuffix + NameSuffix + Extension;
    // get the nodes
    requiredNode := rootNode.Items.ItemNamed['requires'];
    filesNode := rootNode.Items.ItemNamed['files'];

    // read the lines of the templates and do some replacements
    i := 0;
    while i < template.Count do
    begin
      curLine := template[i];
      if Trim(curLine) = '<%%% START REQUIRES %%%>' then
      begin
        Inc(i);
        repeatLines := '';
        while(Trim(template[i]) <> '<%%% END REQUIRES %%%>') do
        begin
          repeatLines := repeatLines + template[i] + #13#10;
          Inc(i);
        end;
        for j := 0 to requiredNode.Items.Count -1 do
        begin
          packageNode := requiredNode.Items[j];
          // if this required package is to be included for this target
          if packageNode.Properties.ItemNamed[ShortTarget(target)].Value <> '' then
          begin
            curLine := repeatLines;
            StrReplace(curLine, '%NAME%', packageNode.Properties.ItemNamed['Name'].Value, [rfReplaceAll]);
            outFile.Text := outFile.Text + #13#10 + curLine;            
          end;
        end;
      end
      else if Trim(curLine) = '<%%% START FILES %%%>' then
      begin
      end
      else
      begin
        StrReplace(curLine, '%NAME%',
                   rootNode.Properties.ItemNamed['Name'].Value+
                   PackSuffix + NameSuffix,
                   [rfReplaceAll]);
        StrReplace(curLine, '%DESCRIPTION%',
                   rootNode.Items.ItemNamed['Description'].Value,
                   [rfReplaceAll]);
        StrReplace(curLine, '%C5PFLAGS%',
                   rootNode.Items.ItemNamed['C5PFlags'].Value,
                   [rfReplaceAll]);
        StrReplace(curLine, '%C6PFLAGS%',
                   rootNode.Items.ItemNamed['C6PFlags'].Value,
                   [rfReplaceAll]);
        outFile.Add(curLine);
      end;
      Inc(i);
    end;

    // Save to the file
    outFile.SaveToFile(OutFileName);
  finally
    outFile.Free;
  end;
end;

procedure Generate(packages : TStrings; targets : TStrings; path : string);
var
  rec : TSearchRec;
  i : Integer;
  j : Integer;
  xml : TJvSimpleXml;
  template : TStringList;
begin
  path := StrEnsureSuffix('\', path);
  // for all targets
  for i := 0 to targets.Count - 1 do
  begin
    // find all template files for that target
    if FindFirst(path+targets[i]+'\template.*', 0, rec) = 0 then
    begin
      repeat
        template := TStringList.Create;
        try
          // apply the template for all packages
          for j := 0 to packages.Count -1 do
          begin
            template.LoadFromFile(path+targets[i]+'\'+rec.Name);
            xml := TJvSimpleXml.Create(nil);
            try
              xml.LoadFromFile(path+'xml\'+packages[j]+'.xml');
              ApplyTemplateAndSave(path,
                                   targets[i],
                                   packages[j],
                                   ExtractFileExt(rec.Name),
                                   template,
                                   xml); 
            finally
              xml.Free;
            end;
          end;
        finally
          template.Free;
        end;
      until FindNext(rec) <> 0;
    end;
    FindClose(rec);
  end;
end;

procedure EnumerateTargets(Path : string; targets : TStrings);
var
  rec : TSearchRec;
begin
  targets.Clear;
  if FindFirst(StrEnsureSuffix('\', Path)+'*.*', faDirectory, rec) = 0 then
  begin
    repeat
      if ((rec.Attr and faDirectory) <> 0) and
        (rec.Name[1] <> '.') and
        (rec.Name <> 'xml') and
        (rec.Name <> 'CVS') then
      begin
        targets.Add(ExtractFileName(rec.Name));
      end;
    until FindNext(rec) <> 0;
  end;
  FindClose(rec);
end;

procedure EnumeratePackages(Path : string; packages : TStrings);
var
  rec : TSearchRec;
begin
  packages.Clear;
  if FindFirst(StrEnsureSuffix('\', path) +'xml\*.xml', 0, rec) = 0 then
  begin
    repeat
      packages.Add(PathExtractFileNameNoExt(rec.Name));
    until FindNext(rec) <> 0;
  end;
  FindClose(rec);
end;

end.
