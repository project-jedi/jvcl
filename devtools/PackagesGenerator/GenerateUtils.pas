unit GenerateUtils;

interface

uses Classes;

type
  TGenerateCallback = procedure (msg : string);

procedure Generate(packages : TStrings; targets : TStrings; path : string; callback : TGenerateCallback);

procedure EnumerateTargets(Path : string; targets : TStrings);

procedure EnumeratePackages(Path : string; packages : TStrings);

var
  StartupDir : string;

implementation

uses SysUtils, JclSysUtils, JclStrings, JclFileUtils, JvSimpleXml;

var GCallBack : TGenerateCallBack;

procedure SendMsg(Msg : string);
begin
  if Assigned(GCallBack) then
    GCallBack(Msg);
end;

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

function GetPersoTarget(target : string) : string;
begin
  target := StrUpper(target);
  if target = 'BCB6' then Result := 'bcb6per'
  else if target = 'BCB6PER' then Result := ''
  else if target = 'BCB5' then Result := ''
  else if target = 'D5' then Result := 'd5std'
  else if target = 'D5STD' then Result := ''
  else if target = 'D6' then Result := 'd6per'
  else if target = 'D6PER' then Result := ''
  else if target = 'D7' then Result := 'd7per'
  else if target = 'D7PER' then Result := ''
  else if target = 'K2' then Result := ''
  else if target = 'K3' then Result := '';
end;

function BuildPackageName(packageNode : TJvSimpleXmlElem; target : string) : string;
var
  Name : string;
  Suffix : string;
begin
  Name := packageNode.Properties.ItemNamed['Name'].Value;
  if Copy(Name, 1, 2) = 'Jv' then
  begin
    Suffix := TargetToSuffix(target);
    Result := Name+Suffix;
    if packageNode.Properties.ItemNamed['Design'].Value <> '' then
      Result := Result+'D'
    else
      Result := Result+'R';
  end
  else
  begin
    Result := Name;
  end;
end;

function IsNotInPerso(Node : TJvSimpleXmlElem; target : string) : Boolean;
var
  persoTarget : string;
begin
  persoTarget := GetPersoTarget(target);
  if persoTarget = '' then
    Result := False
  else
  begin
    Result := (Node.Properties.ItemNamed[ShortTarget(persoTarget)].Value = '') and
              (Node.Properties.ItemNamed[ShortTarget(target)].Value <> '');
  end;
  if Result then
    Beep;
end;

function IsOnlyInPerso(Node : TJvSimpleXmlElem; target : string) : Boolean;
var
  persoTarget : string;
begin
  persoTarget := GetPersoTarget(target);
  if persoTarget = '' then
    Result := False
  else
  begin
    Result := (Node.Properties.ItemNamed[ShortTarget(persoTarget)].Value <> '') and
              (Node.Properties.ItemNamed[ShortTarget(target)].Value = '');
  end;
  if Result then
    Beep;
end;

function EnsureCondition(line : string; Node : TJvSimpleXmlElem; target : string) : string;
begin
  // if there is a condition and target supports it
  if (Node.Properties.ItemNamed['Condition'].Value <> '') and
     (StrUpper(target)[1] = 'D') then
  begin
    Result := '{$IFDEF ' + Node.Properties.ItemNamed['Condition'].Value + '}'#13#10 +
            line + '{$ENDIF}';
  end
  else
    Result := line;
end;

function GetUnitName(FileName : string) : string;
begin
  Result := PathExtractFileNameNoExt(FileName);
end;

procedure ApplyFormName(fileNode : TJvSimpleXmlElem; var Lines : string);
var
  formName : string;
  formType : string;
  formNameAndType : string;
  incFileName : string;
begin
  formNameAndType := fileNode.Properties.ItemNamed['FormName'].Value;
  incFileName := fileNode.Properties.ItemNamed['Name'].Value;

  if Pos(':', formNameAndType) = 0 then
  begin
    formName := formNameAndType;
    formType := '';
  end
  else
  begin
    formName := Copy(formNameAndType, 1, Pos(':', formNameAndType)-1);
    formType := Copy(formNameAndType, Pos(':', formNameAndType)+2, Length(formNameAndType));
  end;

  if formName = '' then
  begin
    StrReplace(Lines, '{', '', [rfReplaceAll]);
    StrReplace(Lines, '}', '', [rfReplaceAll]);
    StrReplace(Lines, '/*', '', [rfReplaceAll]);
    StrReplace(Lines, '*/', '', [rfReplaceAll]);
    StrReplace(Lines, '%FORMNAME%', '', [rfReplaceAll]);
    StrReplace(Lines, '%FORMTYPE%', '', [rfReplaceAll]);
    StrReplace(Lines, '%FORMNAMEANDTYPE%', '', [rfReplaceAll]);
    StrReplace(Lines, '%FORMPATHNAME%', '', [rfReplaceAll]);
    StrReplace(Lines, '%Unitname%',
               StrProper(GetUnitName(incFileName)),
               [rfReplaceAll]);
  end
  else
  begin
    StrReplace(Lines, '%FORMNAME%', formName, [rfReplaceAll]);
    StrReplace(Lines, '%FORMTYPE%', formType, [rfReplaceAll]);
    StrReplace(Lines, '%FORMNAMEANDTYPE%', formNameAndType, [rfReplaceAll]);
    StrReplace(Lines, '%FORMPATHNAME%',
               StrEnsureSuffix('\', ExtractFilePath(incFileName))+formName,
               [rfReplaceAll]);
    StrReplace(Lines, '%Unitname%',
               StrProper(GetUnitName(incFileName)),
               [rfReplaceAll]);
  end;
end;

procedure ApplyTemplateAndSave(path, target, package, extension : string; template : TStrings; xml : TJvSimpleXml; templateDate, xmlDate : TDateTime);
var
  OutFileName : string;
  packSuffix : string;
  NameSuffix : string;
  reqPackName : string;
  incFileName : string;
  rootNode : TJvSimpleXmlElemClassic;
  requiredNode : TJvSimpleXmlElem;
  packageNode : TJvSimpleXmlElem;
  containsNode : TJvSimpleXmlElem;
  fileNode : TJvSimpleXmlElem;
  outFile : TStringList;
  curLine : string;
  repeatLines : string;
  I : Integer;
  j : Integer;
  tmpStr : string;
begin
  packSuffix := targetToSuffix(target);
  outFile := TStringList.Create;

  SendMsg(#9#9'Applying to ' + package);

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
    containsNode := rootNode.Items.ItemNamed['contains'];

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
            tmpStr := repeatLines;
            reqPackName := BuildPackageName(packageNode, target);
            StrReplace(tmpStr, '%NAME%', reqPackName, [rfReplaceAll]);
            outFile.Text := outFile.Text +
                            EnsureCondition(tmpStr, packageNode, target);
          end;

          // if this required package is not in the associated 'perso'
          // target or only in the 'perso' target then build again
          // for this 'perso' target. 'perso' either means 'per' or
          // 'std'
          if IsNotInPerso(packageNode, target) or
             IsOnlyInPerso(packageNode, target) then
            ApplyTemplateAndSave(path, GetPersoTarget(target), package, extension, template, xml, templateDate, xmlDate);
        end;
        // if the last character in the output file is
        // a comma, then remove it. This possible comma will
        // be followed by a carriage return so we look
        // at the third character starting from the end
        tmpStr := outFile.Text;
        if tmpStr[Length(tmpStr)-2] = ',' then
          outFile.Text := Copy(tmpStr, 1, Length(tmpStr) - 3) + #13#10;
      end
      else if Trim(curLine) = '<%%% START FILES %%%>' then
      begin
        Inc(i);
        repeatLines := '';
        while(Trim(template[i]) <> '<%%% END FILES %%%>') do
        begin
          repeatLines := repeatLines + template[i] + #13#10;
          Inc(i);
        end;

        for j := 0 to containsNode.Items.Count -1 do
        begin
          fileNode := containsNode.Items[j];
          // if this included file is to be included for this target
          if fileNode.Properties.ItemNamed[ShortTarget(target)].Value <> '' then
          begin
            tmpStr := repeatLines;
            incFileName := fileNode.Properties.ItemNamed['Name'].Value;
            StrReplace(tmpStr, '%FILENAME%', incFileName, [rfReplaceAll]);
            StrReplace(tmpStr, '%UNITNAME%', GetUnitName(incFileName), [rfReplaceAll]);
            ApplyFormName(fileNode, tmpStr);
            outFile.Text := outFile.Text +
                            EnsureCondition(tmpStr, fileNode, target);
          end;

          // if this included file is not in the associated 'perso'
          // target or only in the 'perso' target then build again
          // for this 'perso' target. 'perso' either means 'per' or
          // 'std'
          if IsNotInPerso(fileNode, target) or
             IsOnlyInPerso(fileNode, target) then
            ApplyTemplateAndSave(path, GetPersoTarget(target), package, extension, template, xml, templateDate, xmlDate); 
        end;
        // if the last character in the output file is
        // a comma, then remove it. This possible comma will
        // be followed by a carriage return so we look
        // at the third character starting from the end
        tmpStr := outFile.Text;
        if tmpStr[Length(tmpStr)-2] = ',' then
          outFile.Text := Copy(tmpStr, 1, Length(tmpStr) - 3) + #13#10;
      end
      else if Trim(curLine) = '<%%% START FORMS %%%>' then
      begin
        Inc(i);
        repeatLines := '';
        while(Trim(template[i]) <> '<%%% END FORMS %%%>') do
        begin
          repeatLines := repeatLines + template[i] + #13#10;
          Inc(i);
        end;

        for j := 0 to containsNode.Items.Count -1 do
        begin
          fileNode := containsNode.Items[j];
          // if this included file is to be included for this target
          if (fileNode.Properties.ItemNamed[ShortTarget(target)].Value <> '') and
            (fileNode.Properties.ItemNamed['FormName'].Value <> '') then
          begin
            tmpStr := repeatLines;
            ApplyFormName(fileNode, tmpStr);
            outFile.Text := outFile.Text +
                            EnsureCondition(tmpStr, fileNode, target);
          end;
        end;
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
        StrReplace(curLine, '%TYPE%',
                   Iff(rootNode.Properties.ItemNamed['Design'].BoolValue,
                      'DESIGN', 'RUN'),
                   [rfReplaceAll]);
        StrReplace(curLine, '%type%', StrLower(NameSuffix), [rfReplaceAll]);
        outFile.Add(curLine);
      end;
      Inc(i);
    end;

    // Save the file, only if the template or the xml are newer
    // than the output file. If that output file doesn't exist,
    // create it too
    if not FileExists(OutFileName) or
      (FileDateToDateTime(FileAge(OutFileName)) < templateDate) or
      (FileDateToDateTime(FileAge(OutFileName)) < xmlDate) then
    outFile.SaveToFile(OutFileName);
  finally
    outFile.Free;
  end;
end;

procedure Generate(packages : TStrings; targets : TStrings; path : string; callback : TGenerateCallback);
var
  rec : TSearchRec;
  i : Integer;
  j : Integer;
  templateFileName : string;
  xml : TJvSimpleXml;
  template : TStringList;
begin
  GCallBack := CallBack;
  path := StrEnsureSuffix('\', path);
  // for all targets
  for i := 0 to targets.Count - 1 do
  begin
    SendMsg('Generating packages for ' + targets[i]);
    // find all template files for that target
    if FindFirst(path+targets[i]+'\template.*', 0, rec) = 0 then
    begin
      repeat
        template := TStringList.Create;
        try
          SendMsg(#9'Loaded '+rec.Name);
          // apply the template for all packages
          for j := 0 to packages.Count -1 do
          begin
            templateFileName := path+targets[i]+'\'+rec.Name;
            template.LoadFromFile(templateFileName);
            xml := TJvSimpleXml.Create(nil);
            try
              xml.LoadFromFile(path+'xml\'+packages[j]+'.xml');
              ApplyTemplateAndSave(path,
                                   targets[i],
                                   packages[j],
                                   ExtractFileExt(rec.Name),
                                   template,
                                   xml,
                                   FileDateToDateTime(rec.Time),
                                   FileDateToDateTime(FileAge(path+'xml\'+packages[j]+'.xml'))); 
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
        (rec.Name <> 'CVS') and
        StrIsDigit(rec.Name[Length(Rec.Name)]) then
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

initialization
  StartupDir := GetCurrentDir;

end.
