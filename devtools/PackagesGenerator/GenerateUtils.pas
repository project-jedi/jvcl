unit GenerateUtils;

interface

uses Classes;

type
  TGenerateCallback = procedure (msg : string);

procedure Generate(packages : TStrings; targets : TStrings; path, prefix, format : string; callback : TGenerateCallback; makeDof : Boolean = False);

procedure EnumerateTargets(Path : string; targets : TStrings);

procedure EnumeratePackages(Path : string; packages : TStrings);

procedure ExpandTargets(targets : TStrings);

var
  StartupDir : string;

implementation

uses Windows, SysUtils, JclSysUtils, JclStrings, JclFileUtils, JvSimpleXml,
    ShellApi, JclDateTime;

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

function suffixToTarget(suffix : string) : string;
var
  tmpSuffix : string;
begin
  tmpSuffix := StrUpper(suffix);
  if tmpSuffix = 'C6' then Result := 'bcb6'
  else if tmpSuffix = 'C6P' then Result := 'bcb6'
  else if tmpSuffix = 'C5' then Result := 'bcb5'
  else if tmpSuffix = 'D5S' then Result := 'd5'
  else if tmpSuffix = 'D6P' then Result := 'd6'
  else if tmpSuffix = 'D7P' then Result := 'd7'
  else Result := suffix;
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

function GetNonPersoTarget(target : string) : string;
begin
  target := StrUpper(target);
  if target = 'BCB6' then Result := 'bcb6'
  else if target = 'BCB6PER' then Result := 'bcb6'
  else if target = 'BCB5' then Result := 'bcb5'
  else if target = 'D5' then Result := 'd5'
  else if target = 'D5STD' then Result := 'd5'
  else if target = 'D6' then Result := 'd6'
  else if target = 'D6PER' then Result := 'd6'
  else if target = 'D7' then Result := 'd7'
  else if target = 'D7PER' then Result := 'd7'
  else if target = 'K2' then Result := 'k2'
  else if target = 'K3' then Result := 'k3';
end;

procedure EnsureTargets(targets : TStrings);
var
  validTargets : TStringList;
  i : Integer;
begin
  validTargets := TStringList.Create;
  try
    // ensure uniqueness in expanded list
    validTargets.Sorted := True;
    validTargets.CaseSensitive := False;
    validTargets.Duplicates := dupIgnore;

    for i := 0 to targets.Count - 1 do
    begin
      validTargets.Add(suffixToTarget(targets[i]));
    end;

    // assign the values back into the caller
    targets.Clear;
    targets.Assign(validTargets);
  finally
    validTargets.Free;
  end;
end;

function ExpandPackageName(Name, target, prefix, format : string) : string;
var
  Suffix : string;
  Env : string;
  Ver : string;
  Typ : string;
begin
  Suffix := TargetToSuffix(target);
  Env := Suffix[1];
  Ver := Suffix[2];
  Typ := Copy(Name, Length(Name), 1);
  Name := Copy(Name, Length(Prefix)+1, Pos('-', Name)-Length(Prefix)-1);

  StrReplace(Format, '%p', Prefix, [rfReplaceAll]);
  StrReplace(Format, '%n', Name, [rfReplaceAll]);
  StrReplace(Format, '%e', Env, [rfReplaceAll]);
  StrReplace(Format, '%v', Ver, [rfReplaceAll]);
  StrReplace(Format, '%t', Typ, [rfReplaceAll]);
  Result := Format;
end;

function BuildPackageName(packageNode : TJvSimpleXmlElem; target : string; prefix : string; Format : string) : string;
var
  Name : string;
begin
  Name := packageNode.Properties.ItemNamed['Name'].Value;
  if (Copy(Name, 1, Length(Prefix)) = Prefix) and
     (Pos('-', Name) <> 0)  then
  begin
    Result := ExpandPackageName(Name, target, prefix, Format);
  end
  else
  begin
    Result := Name;
  end;
end;

function IsNotInPerso(Node : TJvSimpleXmlElem; target : string) : Boolean;
var
  persoTarget : string;
  targets : TStringList;
begin
  persoTarget := GetPersoTarget(target);
  if persoTarget = '' then
    Result := False
  else
  begin
    targets := TStringList.Create;
    try
      StrToStrings(Node.Properties.ItemNamed['Targets'].Value,
                   ',',
                   targets);
      Result := (targets.IndexOf(ShortTarget(persoTarget)) = -1) and
                (targets.IndexOf(ShortTarget(target)) > -1);
    finally
      targets.Free;
    end;
  end;
end;

function IsOnlyInPerso(Node : TJvSimpleXmlElem; target : string) : Boolean;
var
  persoTarget : string;
  targets : TStringList;
begin
  persoTarget := GetPersoTarget(target);
  if persoTarget = '' then
    Result := False
  else
  begin
    targets := TStringList.Create;
    try
      StrToStrings(Node.Properties.ItemNamed['Targets'].Value,
                   ',',
                   targets);
      Result := (targets.IndexOf(ShortTarget(persoTarget)) > -1) and
                (targets.IndexOf(ShortTarget(target)) = -1);
    finally
      targets.Free;
    end;
  end;
end;

function EnsureCondition(line : string; Node : TJvSimpleXmlElem; target : string) : string;
begin
  // if there is a condition and the target supports it
  // Currently, only Delphi targets support conditions
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

procedure EnsureProperSeparator(var Name : string; target : string);
begin
  // ensure that the path separator stored in the xml file is
  // replaced by the one for the system we are targeting

  target := StrLower(target);

  // first ensure we only have backslashes
  StrReplace(Name, '/', '\', [rfReplaceAll]);

  // and if the target is kylix, replace all them by forward slashes
  if target[1] = 'k' then
    StrReplace(Name, '\', '/', [rfReplaceAll]);
end;

procedure ApplyFormName(fileNode : TJvSimpleXmlElem; var Lines : string; target : string);
var
  formName : string;
  formType : string;
  formNameAndType : string;
  incFileName : string;
  openPos : Integer;
  closePos : Integer;
  unitname : string;
  punitname : string;
  formpathname : string;
begin
  formNameAndType := fileNode.Properties.ItemNamed['FormName'].Value;
  incFileName := fileNode.Properties.ItemNamed['Name'].Value;

  unitname := GetUnitName(incFileName);
  punitname := StrLower(unitname);
  punitname[1] := CharUpper(punitname[1]);
  formpathname := StrEnsureSuffix(PathSeparator, ExtractFilePath(incFileName))+GetUnitName(incFileName);

  EnsureProperSeparator(formpathname, target);
  EnsureProperSeparator(incfilename, target);

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

  StrReplace(Lines, '%FILENAME%', incFileName, [rfReplaceAll]);
  StrReplace(Lines, '%UNITNAME%', unitname, [rfReplaceAll]);
  StrReplace(Lines, '%Unitname%', punitname, [rfReplaceAll]);

  if (formType = '') or (formName = '') then
  begin
    openPos := Pos('/*', Lines);
    if openPos > 0 then
    begin
      closePos := Pos('*/', Lines);
      Lines := Copy(Lines, 1, openPos-1)+Copy(Lines,closePos+2,Length(Lines));
    end;
  end;

  if formName = '' then
  begin
    openPos := Pos('{', Lines);
    if openPos > 0 then
    begin
      closePos := Pos('}', Lines);
      Lines := Copy(Lines, 1, openPos-1)+Copy(Lines,closePos+1,Length(Lines));
    end;
    StrReplace(Lines, '%FORMNAME%', '', [rfReplaceAll]);
    StrReplace(Lines, '%FORMTYPE%', '', [rfReplaceAll]);
    StrReplace(Lines, '%FORMNAMEANDTYPE%', '', [rfReplaceAll]);
    StrReplace(Lines, '%FORMPATHNAME%', '', [rfReplaceAll]);
  end
  else
  begin
    StrReplace(Lines, '%FORMNAME%', formName, [rfReplaceAll]);
    StrReplace(Lines, '%FORMTYPE%', formType, [rfReplaceAll]);
    StrReplace(Lines, '%FORMNAMEANDTYPE%', formNameAndType, [rfReplaceAll]);
    StrReplace(Lines, '%FORMPATHNAME%', formpathname, [rfReplaceAll]);
  end;
end;

procedure ExpandTargets(targets : TStrings);
var
  expandedTargets : TStringList;
  i : Integer;
begin
  expandedTargets := TStringList.Create;
  try
    // ensure uniqueness in expanded list
    expandedTargets.Sorted := True;
    expandedTargets.CaseSensitive := False;
    expandedTargets.Duplicates := dupIgnore;

    for i := 0 to targets.Count - 1 do
    begin
      if SameText(targets[i], 'all') then
        StrToStrings('c5,c6,d5,d5s,d6,d6p,d7,d7p,k2,k3', ',', expandedTargets)
      else if SameText(targets[i], 'windows') then
      begin
        expandedTargets.Add('c5');
        expandedTargets.Add('c6');
        expandedTargets.Add('c6p');
        expandedTargets.Add('d5');
        expandedTargets.Add('d5s');
        expandedTargets.Add('d6');
        expandedTargets.Add('d6p');
        expandedTargets.Add('d7');
        expandedTargets.Add('d7p');
      end
      else if SameText(targets[i], 'linux') or
              SameText(targets[i], 'kylix') then
      begin
        expandedTargets.Add('k2');
        expandedTargets.Add('k3');
      end
      else if SameText(targets[i], 'delphi') then
      begin
        expandedTargets.Add('d5');
        expandedTargets.Add('d5s');
        expandedTargets.Add('d6');
        expandedTargets.Add('d6p');
        expandedTargets.Add('d7');
        expandedTargets.Add('d7p');
      end
      else if SameText(targets[i], 'bcb') then
      begin
        expandedTargets.Add('c5');
        expandedTargets.Add('c6');
        expandedTargets.Add('c6p');
      end
      else
        expandedTargets.Add(targets[i]);
    end;

    // assign the values back into the caller
    targets.Clear;
    targets.Assign(expandedTargets);
  finally
    expandedTargets.Free;
  end;
end;

function IsIncluded(Node : TJvSimpleXmlElem; target : string) : Boolean;
var
  targets : TStringList;
begin
  targets := TStringList.Create;
  try
    StrToStrings(Node.Properties.ItemNamed['Targets'].Value,
                 ',', targets);
    ExpandTargets(targets);
    Result := (targets.IndexOf(ShortTarget(target)) > -1);
  finally
    targets.Free;
  end;
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

function ApplyTemplateAndSave(path, target, package, extension, prefix, format : string; template : TStrings; xml : TJvSimpleXml; templateDate, xmlDate : TDateTime; xmlName : string) : string;
var
  OutFileName : string;
  packSuffix : string;
  oneLetterType : string;
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
  bcblibs : string;
  bcblibsList : TStringList;
  containsSomething : Boolean; // true if package will contain something
  repeatSectionUsed : Boolean; // true if at least one repeat section was used
begin
  packSuffix := targetToSuffix(target);
  outFile := TStringList.Create;
  bcblibsList := TStringList.Create;
  Result := '';
  containsSomething := False;
  repeatSectionUsed := False;

  try
    // read the xml file
    rootNode := xml.Root;
    OutFileName := rootNode.Properties.ItemNamed['Name'].Value;
    if rootNode.Properties.ItemNamed['Design'].BoolValue then
    begin
      OutFileName := OutFileName + '-D';
      oneLetterType := 'd';
    end
    else
    begin
      OutFileName := OutFileName + '-R';
      oneLetterType := 'r';
    end;

    OutFileName := path + target + PathSeparator +
                   ExpandPackageName(OutFileName, target, prefix, format)+
                   Extension;

    // Process the file, only if the template or the xml are newer
    // than the output file. If that output file doesn't exist,
    // create it too
    if not FileExists(OutFileName) or
      (FileDateToDateTime(FileAge(OutFileName)) < templateDate) or
      (FileDateToDateTime(FileAge(OutFileName)) < xmlDate) then
    begin
      SendMsg(#9#9'Applying to ' + package);

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
          repeatSectionUsed := True;
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
            if IsIncluded(packageNode, target) then
            begin
              tmpStr := repeatLines;
              reqPackName := BuildPackageName(packageNode, target, prefix, format);
              StrReplace(tmpStr, '%NAME%', reqPackName, [rfReplaceAll]);
              containsSomething := True;
              outFile.Text := outFile.Text +
                              EnsureCondition(tmpStr, packageNode, target);
            end;

            // if this required package is not in the associated 'perso'
            // target or only in the 'perso' target then return the
            // 'perso' target name. 'perso' either means 'per' or
            // 'std'
            if IsNotInPerso(packageNode, target) or
               IsOnlyInPerso(packageNode, target) then
              Result := GetPersoTarget(target);
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
          repeatSectionUsed := True;
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
            if IsIncluded(fileNode, target) then
            begin
              tmpStr := repeatLines;
              incFileName := fileNode.Properties.ItemNamed['Name'].Value;
              ApplyFormName(fileNode, tmpStr, target);
              containsSomething := True;
              outFile.Text := outFile.Text +
                              EnsureCondition(tmpStr, fileNode, target);
            end;

            // if this include file is not in the associated 'perso'
            // target or only in the 'perso' target then return the
            // 'perso' target name. 'perso' either means 'per' or
            // 'std'
            if IsNotInPerso(fileNode, target) or
               IsOnlyInPerso(fileNode, target) then
              Result := GetPersoTarget(target);
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
          repeatSectionUsed := True;
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
            // and there is a form associated to the file
            if IsIncluded(fileNode, target) then
            begin
              containsSomething := True;
              if (fileNode.Properties.ItemNamed['FormName'].Value <> '') then
              begin
                tmpStr := repeatLines;
                ApplyFormName(fileNode, tmpStr, target);
                outFile.Text := outFile.Text + tmpStr;
              end;
            end;
          end;
        end
        else if Trim(curLine) = '<%%% START LIBS %%%>' then
        begin
          Inc(i);
          repeatLines := '';
          while(Trim(template[i]) <> '<%%% END LIBS %%%>') do
          begin
            repeatLines := repeatLines + template[i] + #13#10;
            Inc(i);
          end;

          // read libs as a string of comma separated value
          bcblibs :=  rootNode.Items.ItemNamed[packSuffix+'Libs'].Value;
          if bcblibs <> '' then
          begin
            StrToStrings(bcblibs, ',', bcblibsList);
            for j := 0 to bcbLibsList.Count - 1 do
            begin
              tmpStr := repeatLines;
              StrReplace(tmpStr, '%FILENAME%', bcblibsList[j], [rfReplaceAll]);
              StrReplace(tmpStr, '%UNITNAME%', GetUnitName(bcblibsList[j]), [rfReplaceAll]);
              outFile.Text := outFile.Text + tmpStr;
            end;
          end;
        end
        else
        begin
          StrReplace(curLine, '%NAME%',
                     PathExtractFileNameNoExt(OutFileName),
                     [rfReplaceAll]);
          StrReplace(curLine, '%XMLNAME%',
                     ExtractFileName(xmlName),
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
          StrReplace(curLine, '%DATETIME%',
                      FormatDateTime('dd-mm-yyyy  hh:nn:ss', NowUTC) + ' UTC',
                      [rfReplaceAll]);
          StrReplace(curLine, '%type%', OneLetterType, [rfReplaceAll]);
          outFile.Add(curLine);
        end;
        Inc(i);
      end;

      if not repeatSectionUsed then
      begin
        // if no repeat section was used, we must check manually
        // that at least one file or package is to be used by
        // the given target. This will then force the generation
        // of the output file (Useful for cfg templates for instance).

        for j := 0 to requiredNode.Items.Count -1 do
          if IsIncluded(requiredNode.Items[j], target) then
            containsSomething := True;

        for j := 0 to containsNode.Items.Count -1 do
          if IsIncluded(containsNode.Items[j], target) then
            containsSomething := True;
      end;

      if containsSomething then
        outFile.SaveToFile(OutFileName);
    end;
  finally
    bcblibsList.Free;
    outFile.Free;
  end;
end;

procedure Generate(packages : TStrings; targets : TStrings; path, prefix, format : string; callback : TGenerateCallback; makeDof : Boolean);
var
  rec : TSearchRec;
  i : Integer;
  j : Integer;
  templateFileName : string;
  xml : TJvSimpleXml;
  xmlName : string;
  template : TStringList;
  persoTarget : string;
  target : string;
begin
  GCallBack := CallBack;
  path := StrEnsureSuffix(PathSeparator, path);
  // for all targets
  EnsureTargets(targets);
  i := 0;
  while i < targets.Count do
  begin
    target := targets[i];
    SendMsg('Generating packages for ' + target);
    // find all template files for that target
    if FindFirst(path+target+PathSeparator+'template.*', 0, rec) = 0 then
    begin
      repeat
        template := TStringList.Create;
        try
          SendMsg(#9'Loaded '+rec.Name);
          // apply the template for all packages
          for j := 0 to packages.Count-1 do
          begin
            templateFileName := path+target+PathSeparator+rec.Name;
            template.LoadFromFile(templateFileName);
            xml := TJvSimpleXml.Create(nil);
            try
              xml.Options := [sxoAutoCreate];
              xmlName := path+'xml'+PathSeparator+packages[j]+'.xml';
              xml.LoadFromFile(xmlName);
              persoTarget := ApplyTemplateAndSave(
                                   path,
                                   target,
                                   packages[j],
                                   ExtractFileExt(rec.Name),
                                   prefix,
                                   Format,
                                   template,
                                   xml,
                                   FileDateToDateTime(rec.Time),
                                   FileDateToDateTime(FileAge(xmlName)),
                                   xmlName);

              // if the generation requested a perso target to be done
              // then generate it now. If we find a template file
              // named the same as the current one in the perso
              // directory then use it instead
              if persoTarget <> '' then
              begin
                SendMsg(#9'Regenerating for '+persoTarget);
                if FileExists(path+persoTarget+PathSeparator+rec.Name) then
                begin
                  SendMsg(#9+persoTarget+ ' template used instead');
                  template.LoadFromFile(path+persoTarget+PathSeparator+rec.Name);
                end;

                ApplyTemplateAndSave(
                   path,
                   persoTarget,
                   packages[j],
                   ExtractFileExt(rec.Name),
                   prefix,
                   Format,
                   template,
                   xml,
                   FileDateToDateTime(rec.Time),
                   FileDateToDateTime(FileAge(xmlName)),
                   xmlName);
              end;
            finally
              xml.Free;
            end;
          end;
        finally
          template.Free;
        end;
      until FindNext(rec) <> 0;
    end
    else
      SendMsg(#9'No template found for '+target);
    FindClose(rec);
    Inc(i);
  end;

  if makeDof then
  begin
    SendMsg('Calling MakeDofs.bat');
    ShellExecute(0,
                '',
                PChar(StrEnsureSuffix(PathSeparator, ExtractFilePath(ParamStr(0))) + 'MakeDofs.bat'),
                '',
                PChar(ExtractFilePath(ParamStr(0))),
                SW_SHOW);
  end;
end;

procedure EnumerateTargets(Path : string; targets : TStrings);
var
  rec : TSearchRec;
begin
  targets.Clear;
  if FindFirst(StrEnsureSuffix(PathSeparator, Path)+'*.*', faDirectory, rec) = 0 then
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
  if FindFirst(StrEnsureSuffix(PathSeparator, path) +'xml'+PathSeparator+'*.xml', 0, rec) = 0 then
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
