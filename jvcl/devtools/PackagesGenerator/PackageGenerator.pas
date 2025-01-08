unit PackageGenerator;

{$I jvcl.inc}

interface

uses
  Contnrs, Classes,
  JclSimpleXml,
  PackageInformation, GenerateDefines, GenerateTargets, GenerateAlias, GenerateReplacements, DefinesConditionParser;

type
  TProjectProperties = class(TStringList)
  public
    constructor Create(Node: TJclSimpleXmlElem);
  end;

  TGenerateCallback = procedure (const msg : string);

  TPackageGenerator = class(TObject)
  private
    FCallBack: TGenerateCallback;
    FStartupDir        : string;
    FPackagesLocation  : string;
    FIncDefFileName    : string;
    FIncFileName       : string;
    FPrefix            : string;
    FNoLibSuffixPrefix : string;
    FClxPrefix         : string;
    FDotNetPrefix      : string;
    FFormat            : string;
    FNoLibSuffixFormat : string;
    FClxFormat         : string;
    FDotNetFormat      : string;
    FTargetList: TTargetList;
    FAliasList: TAliasList;
    FClxReplacementList: TClxReplacementList;
    FIsBinaryCache: TStringList;
    FProjectProperties: TProjectProperties;
    FDefinesConditionParser: TDefinesConditionParser;
    procedure SendMsg(const Msg : string);
    function GetPersoTarget(const Target : string) : string;
    function GetNonPersoTarget(const PersoTarget : string) : string;
    function TargetToDir(const target : string) : string;
    function ExpandPackageName(Name: string; const target : string) : string;
    function HasModelPrefix(Name : string; const target:string): Boolean;
    function BuildPackageName(xml: TRequiredPackage; const target : string) : string;
    function IsNotInPerso(Item: TPackageXmlInfoItem; const target : string) : Boolean;
    function IsOnlyInPerso(Item: TPackageXmlInfoItem; const target : string) : Boolean;
    procedure EnsureProperSeparator(var Name : string; const target : string);
    procedure ApplyFormName(ContainedFile: TContainedFile; index: Integer; Lines : TStrings;
      const target : string);
    function GetDescription(xml: TPackageXmlInfo; const target: string): string;
    function ApplyTemplateAndSave(const path, target, package, extension: string;
      template : TStrings; xml : TPackageXmlInfo;
      const templateName, xmlName : string): string;
    function LoadDefines(const Target: string; Filename: string): Boolean;
    function IsBinaryFile(const Filename: string): Boolean;

  public
    constructor Create;
    destructor Destroy; override;
    function LoadConfig(const XmlFileName : string; const ModelName : string; var ErrMsg : string) : Boolean;
    function Generate(packages : TStrings;
                      targets : TStrings;
                      callback : TGenerateCallback;
                      const XmlFileName : string;
                      const ModelName : string;
                      var ErrMsg : string;
                      path : string = '';
                      prefix : string = '';
                      format : string = '';
                      incFileName : string = ''
                     ) : Boolean;
    procedure ExpandTargets(targets : TStrings);
    procedure ExpandTargetsNoPerso(targets : TStrings);
    property StartupDir: string read FStartupDir write FStartupDir;
    property PackagesLocation: string read FPackagesLocation write FPackagesLocation;
    property TargetList: TTargetList read FTargetList write FTargetList;
//    property IncDefFileName    : string read FIncDefFileName    write FIncDefFileName   ;
//    property IncFileName       : string read FIncFileName       write FIncFileName      ;
//    property Prefix            : string read FPrefix            write FPrefix           ;
//    property NoLibSuffixPrefix : string read FNoLibSuffixPrefix write FNoLibSuffixPrefix;
//    property ClxPrefix         : string read FClxPrefix         write FClxPrefix        ;
//    property DotNetPrefix      : string read FDotNetPrefix      write FDotNetPrefix     ;
//    property NoLibSuffixFormat : string read FNoLibSuffixFormat write FNoLibSuffixFormat;
//    property ClxFormat         : string read FClxFormat         write FClxFormat        ;
//    property DotNetFormat      : string read FDotNetFormat      write FDotNetFormat     ;
  end;

implementation

uses
  Windows, SysUtils, FileUtils,
  {$IFDEF UNICODE}
  Character, // needed for JclStrings inlined functions
  {$ENDIF UNICODE}
  JclStrings, JclFileUtils, JclDateTime, JclSysUtils,
  GenerateUtils;

{ TPackageGenerator }

constructor TPackageGenerator.Create;
begin
  inherited Create;
  FStartupDir := GetCurrentDir;
  PackageInformation.ExpandPackageTargetsObj := ExpandTargets;
  FIsBinaryCache := TStringList.Create;
  FIsBinaryCache.Sorted := True;
  FIsBinaryCache.Duplicates := dupIgnore;
end;

destructor TPackageGenerator.Destroy;
begin
  PackageInformation.ExpandPackageTargetsObj := nil;
  FTargetList.Free;
  FAliasList.Free;
  FClxReplacementList.Free;
  FIsBinaryCache.Free;
  FProjectProperties.Free;
  FDefinesConditionParser.Free;
  inherited Destroy;
end;

procedure TPackageGenerator.SendMsg(const Msg : string);
begin
  if Assigned(FCallBack) then
    FCallBack(Msg);
end;

function TPackageGenerator.LoadConfig(const XmlFileName : string; const ModelName : string;
  var ErrMsg : string) : Boolean;
var
  xml : TJclSimpleXml;
  Node : TJclSimpleXmlElem;
  i : integer;
  all : string;
  target : TTarget;
begin
  Result := True;
  FreeAndNil(FTargetList);
  FreeAndNil(FAliasList);
  FreeAndNil(FClxReplacementList);
  FreeAndNil(FProjectProperties);

  // Ensure the xml file exists
  if not FileExists(XmlFileName) then
  begin
    ErrMsg := Format('%s does not exist.', [XmlFileName]);
    Result := False;
    Exit;
  end;

  try
    // read the xml config file
    xml := TJclSimpleXml.Create;
    try
      xml.LoadFromFile(XmlFileName);

      // The xml file must contain the models node
      if not Assigned(xml.Root.Items.itemNamed['models']) then
      begin
        Result := False;
        ErrMsg := 'The root node of the xml file must contain '+
                  'a node called ''models''.';
        Exit;
      end;

      Node := xml.root.Items.itemNamed['models'].items[0];
      if not VerifyModelNode(Node, ErrMsg) then
      begin
        Result := False;
        Exit;
      end;

      for i := 0 to xml.root.Items.itemNamed['models'].items.count - 1 do
        if xml.root.Items.itemNamed['models'].items[i].Properties.ItemNamed['Name'].value = ModelName then
          Node := xml.root.Items.itemNamed['models'].items[i];

      if not VerifyModelNode(Node, ErrMsg) then
      begin
        Result := False;
        Exit;
      end;

      FTargetList := TTargetList.Create(Node.Items.ItemNamed['targets']);
      FAliasList := TAliasList.Create(Node.Items.ItemNamed['aliases']);
      FClxReplacementList := TClxReplacementList.Create(Node.Items.ItemNamed['ClxReplacements']);
      FProjectProperties := TProjectProperties.Create(Node.Items.ItemNamed['ProjectProperties']);

      if Assigned(Node.Properties.ItemNamed['incdeffile']) then
        FIncDefFileName   := Node.Properties.ItemNamed['incdeffile'].Value;
      FIncFileName      := Node.Properties.ItemNamed['IncFile'].Value;
      FPackagesLocation := Node.Properties.ItemNamed['packages'].Value;
      FFormat           := Node.Properties.ItemNamed['format'].Value;
      FPrefix           := Node.Properties.ItemNamed['prefix'].Value;

      FNoLibSuffixPrefix  := FPrefix;
      FClxPrefix          := FPrefix;
      FDotNetPrefix       := FPrefix;
      FNoLibSuffixFormat  := FFormat;
      FClxFormat          := FFormat;
      FDotNetFormat       := FFormat;

      if Assigned(Node.Properties.ItemNamed['NoLibSuffixprefix']) then
        FNoLibSuffixPrefix := Node.Properties.ItemNamed['NoLibSuffixprefix'].Value;
      if Assigned(Node.Properties.ItemNamed['clxprefix']) then
        FClxPrefix         := Node.Properties.ItemNamed['clxprefix'].Value;
      if Assigned(Node.Properties.ItemNamed['dotnetprefix']) then
        FDotNetPrefix      := Node.Properties.ItemNamed['dotnetprefix'].Value;
      if Assigned(Node.Properties.ItemNamed['NoLibSuffixformat']) then
        FNoLibSuffixFormat := Node.Properties.ItemNamed['NoLibSuffixformat'].Value;
      if Assigned(Node.Properties.ItemNamed['clxformat']) then
        FClxFormat         := Node.Properties.ItemNamed['clxformat'].Value;
      if Assigned(Node.Properties.ItemNamed['dotnetformat']) then
        FDotNetFormat      := Node.Properties.ItemNamed['dotnetformat'].Value;

      // create the 'all' alias
      all := '';
      for i := 0 to FTargetList.Count-1 do
      begin
        Target := FTargetList.Items[i];
        all := all + Target.Name + ',';
        if Target.PName <> '' then
          all := all + Target.PName + ',';
      end;
      SetLength(all, Length(all) - 1);

      Node := TJclSimpleXmlElemClassic.Create(nil);
      try
        Node.Properties.Add('name', 'all');
        Node.Properties.Add('value', all);
        FAliasList.Add(TAlias.Create(Node));
      finally
        Node.Free;
      end;
    finally
      xml.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ErrMsg := E.Message;
    end;
  end;
end;

function TPackageGenerator.GetPersoTarget(const Target : string) : string;
begin
  if FTargetList[Target] <> nil then
    Result := FTargetList[Target].PName
  else
    Result := Target;
end;

function TPackageGenerator.GetNonPersoTarget(const PersoTarget : string) : string;
var
  i : integer;
  Target : TTarget;
begin
  Result := PersoTarget;
  for i := 0 to FTargetList.Count - 1 do
  begin
    Target := FTargetList.Items[i];
    if SameText(Target.PName, PersoTarget) then
    begin
      Result := Target.Name;
      Break;
    end;
  end;
end;

function TPackageGenerator.TargetToDir(const target : string) : string;
begin
  if Assigned(FTargetList[target]) then
    Result := FTargetList[target].Dir
  else if Assigned(FTargetList[GetNonPersoTarget(target)]) then
    Result := FTargetList[GetNonPersoTarget(target)].PDir
  else
    raise Exception.CreateFmt('Target "%s" not found.', [target]);
end;

function TPackageGenerator.ExpandPackageName(Name: string; const target : string) : string;
var
  Env   : string;
  Ver   : string;
  Typ   : string;
  Prefix: string;
  ATarget: TTarget;
begin
  ATarget := FTargetList[GetNonPersoTarget(target)];
  Env := ATarget.Env;
  Ver := ATarget.Ver;
  Typ := Copy(Name, Length(Name), 1);

  if ((AnsiLowerCase(Env) = 'd') or (AnsiLowerCase(Env) = 'c')) and (StrToInt(Ver) < 6) then
  begin
    Result := FNoLibSuffixFormat;
    Prefix := FNoLibSuffixPrefix;
  end
  else if (FTargetList[GetNonPersoTarget(target)].IsCLX) then
  begin
    Result := FClxFormat;
    Prefix := FClxPrefix;
  end
  else if (FTargetList[GetNonPersoTarget(target)].IsDotNet) then
  begin
    Result := FDotNetFormat;
    Prefix := FDotNetPrefix;
  end
  else
  begin
    Result := FFormat;
    Prefix := FPrefix;
  end;

  // If we find Prefix in the Name, then use it first, else, fall back
  // to GPrefix.
  if (Pos(Prefix, Name) > 0) then
    Name := Copy(Name, Length(Prefix)+1, Pos('-', Name)-Length(Prefix)-1)
  else
    Name := Copy(Name, Length(FPrefix)+1, Pos('-', Name)-Length(FPrefix)-1);

  // Always use Prefix as the replacement string for %p
  MacroReplace(Result, '%',
    ['p', Prefix,
     'n', Name,
     'e', Env,
     'v', Ver,
     't', Typ]);
end;

function TPackageGenerator.HasModelPrefix(Name : string; const target:string): Boolean;
var
  Env: string;
  Ver: string;
  ATarget: TTarget;
begin
  ATarget := FTargetList[GetNonPersoTarget(target)];
  Env := ATarget.Env;
  Ver := ATarget.Ver;
  Result := False;

  // We first try a CLX prefix
  // If this failed, then we try a NoLibSuffix prefix
  // If this failed too, then we go back to the standard prefix.
  // This methods is employed mostly for CLX targets as this allows
  // to have a single xml source file for both CLX and non CLX
  // targets. For instance, in the JVCL, we would have a source file
  // called JvSystem-R.xml which requires JvCore-R. Using this method
  // when generating a CLX package which has a JvQ prefix, we still can
  // recognize JvCore-R has being one of the package names that needs
  // to be modified and thus will end up being JvQCoreD7R in the case
  // of the Delphi 7 CLX target while still being JvCoreD7R for a
  // regular Delphi 7 target (non CLX)

  if (FTargetList[GetNonPersoTarget(target)].IsCLX) then
    Result := StartsWith(FClxPrefix, Name);

  if (FTargetList[GetNonPersoTarget(target)].IsDotNet) then
    Result := StartsWith(FDotNetPrefix, Name);

  if not Result and ((AnsiLowerCase(Env) = 'd') or (AnsiLowerCase(Env) = 'c')) and (StrToInt(Ver) < 6) then
    Result := StartsWith(FNoLibSuffixPrefix, Name);

  if not Result then
    Result := StartsWith(FPrefix, Name);
end;

function TPackageGenerator.BuildPackageName(xml: TRequiredPackage; const target : string) : string;
var
  Name : string;
begin
  Name := xml.Name;
  {TODO : CrossPlatform packages}
  if HasModelPrefix(Name, target) then
  begin
    Result := ExpandPackageName(Name, target);
  end
  else
  begin
    Result := Name;
  end;
end;

function TPackageGenerator.IsNotInPerso(Item: TPackageXmlInfoItem; const target : string) : Boolean;
var
  persoTarget : string;
begin
  persoTarget := GetPersoTarget(target);
  if persoTarget = '' then
    Result := False
  else
  begin
    Result := not Item.IsIncluded(persoTarget) and
              Item.IsIncluded(target);
  end;
end;

function TPackageGenerator.IsOnlyInPerso(Item: TPackageXmlInfoItem; const target : string) : Boolean;
var
  persoTarget : string;
begin
  persoTarget := GetPersoTarget(target);
  if persoTarget = '' then
    Result := False
  else
  begin
    Result := Item.IsIncluded(persoTarget) and
              not Item.IsIncluded(target);
  end;
end;

procedure TPackageGenerator.EnsureProperSeparator(var Name : string; const target : string);
var
  TmpName: string;
begin
  // ensure that the path separator stored in the xml file is
  // replaced by the one for the system we are targeting

  TmpName := Name;

  // first ensure we only have backslashes
  StrReplace(TmpName, '/', '\', [rfReplaceAll]);

  // and replace all them by the path separator for the target
  StrReplace(TmpName, '\', FTargetList[GetNonPersoTarget(target)].PathSep, [rfReplaceAll]);

  Name := TmpName;
end;

procedure TPackageGenerator.ApplyFormName(ContainedFile: TContainedFile; index: Integer; Lines : TStrings;
  const target : string);
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
  S: string;
  ps: Integer;
begin
  formNameAndType := ContainedFile.FormName;
  incFileName := ContainedFile.Name;

  // Do the CLX filename replacements if the target is marked as
  // being a CLX target
  if FTargetList[GetNonPersoTarget(target)].IsCLX then
    incFileName := FClxReplacementList.DoReplacement(incFileName);

  unitname := GetUnitName(incFileName);
  punitname := AnsiLowerCase(unitname);
  punitname[1] := CharUpper(punitname[1]);
  formpathname := StrEnsureSuffix(DirDelimiter, ExtractFilePath(incFileName))+GetUnitName(incFileName);

  EnsureProperSeparator(formpathname, target);
  EnsureProperSeparator(incfilename, target);

  ps := Pos(':', formNameAndType);
  if ps = 0 then
  begin
    formName := formNameAndType;
    formType := '';
  end
  else
  begin
    formName := Copy(formNameAndType, 1, ps-1);
    formType := Copy(formNameAndType, ps+2, MaxInt);
  end;

  if (formType = '') or (formName = '') then
  begin
    S := Lines.Text;
    openPos := Pos('/*', S);
    if openPos > 0 then
    begin
      closePos := Pos('*/', S);
      Delete(S, openPos, closepos + 2 - openPos);
      Lines.Text := S;
    end;
  end;

  if formName = '' then
  begin
    S := Lines.Text;
    openPos := Pos('{', S);
    if openPos > 0 then
    begin
      closePos := Pos('}', S);
      Delete(S, openPos, closePos + 1 - openPos);
      Lines.Text := S;
    end;
    formName := '';
    formType := '';
    formNameAndType := '';
    formpathname := '';
  end;

  MacroReplaceLines(Lines, '%',
    ['FILENAME%', incFileName,
     'UNITNAME%', unitname,
     'Unitname%', punitname,
     'INDEX%', IntToStr(Index),
     'INDEX0%', IntToStr(Index - 1),
     'FORMNAME%', formName,
     'FORMTYPE%', formType,
     'FORMNAMEANDTYPE%', formNameAndType,
     'FORMPATHNAME%', formpathname]);
end;

procedure TPackageGenerator.ExpandTargets(targets : TStrings);
var
  expandedTargets : TStringList;
  i : Integer;
  Alias : TAlias;
  currentTarget : string;
begin
  expandedTargets := TStringList.Create;
  try
    // ensure uniqueness in expanded list
    expandedTargets.Sorted := True;
// CaseSensitive doesn't exist in D5 and the default is False anyway
//    expandedTargets.CaseSensitive := False;
    expandedTargets.Duplicates := dupIgnore;

    for i := 0 to targets.Count - 1 do
    begin
      currentTarget := targets[i];
      Alias := FAliasList[currentTarget];
      if Assigned(Alias) then
      begin
        expandedTargets.AddStrings(Alias.ValueAsTStrings);
      end
      else
      begin
        expandedTargets.Add(Trim(currentTarget));
        if not Assigned(FTargetList.ItemsByName[currentTarget]) and (GetNonPersoTarget(currentTarget) = currentTarget) then
          SendMsg(Format('Unknown target: %s', [currentTarget]));
      end;
    end;

    // assign the values back into the caller
    targets.Clear;
    targets.Assign(expandedTargets);
  finally
    expandedTargets.Free;
  end;
end;

procedure TPackageGenerator.ExpandTargetsNoPerso(targets : TStrings);
var
  i : integer;
begin
  ExpandTargets(targets);
  // now remove "perso" targets
  for i := targets.Count - 1 downto 0 do
    if not Assigned(FTargetList.ItemsByName[targets[i]]) then
      targets.Delete(i);
end;

function TPackageGenerator.GetDescription(xml: TPackageXmlInfo; const target: string): string;
begin
  if FTargetList[GetNonPersoTarget(target)].IsCLX then
    Result := xml.ClxDescription
  else
    Result := xml.Description;
end;

{$WARNINGS OFF} // hide wrong warning: "Function return value could be undefined."
function TPackageGenerator.ApplyTemplateAndSave(const path, target, package, extension: string;
  template : TStrings; xml : TPackageXmlInfo;
  const templateName, xmlName : string): string;
  procedure AddProperty(Properties: TStrings; const Name, Value: string);
  begin
    Properties.Values[Name] := Value;
    if Value = '' then
      Properties.Add(Name + '=');
  end;
type
  TProjectConditional = record
    StartLine: string;
    EndLine: string;
    ProjectType: TProjectType;
  end;
const
  ProjectConditionals: array [0..4] of TProjectConditional =
    ( ( StartLine:'<%%% BEGIN PROGRAMONLY %%%>'; EndLine:'<%%% END PROGRAMONLY %%%>'; ProjectType:ptProgram),
      ( StartLine:'<%%% BEGIN PACKAGEONLY %%%>'; EndLine:'<%%% END PACKAGEONLY %%%>'; ProjectType:ptPackage),
      ( StartLine:'<%%% BEGIN LIBRARYONLY %%%>'; EndLine:'<%%% END LIBRARYONLY %%%>'; ProjectType:ptLibrary),
      ( StartLine:'<%%% BEGIN DESIGNONLY %%%>'; EndLine:'<%%% END DESIGNONLY %%%>'; ProjectType:ptPackageDesign),
      ( StartLine:'<%%% BEGIN RUNONLY %%%>'; EndLine:'<%%% END RUNONLY %%%>'; ProjectType:ptPackageRun) );
type
  TPlatformConditional = record
    StartLine: string;
    EndLine: string;
    PlatformType: TPlatformType;
  end;
const
  ProjectPlatforms: array [0..2] of TPlatformConditional =
    ( ( StartLine:'<%%% START PLATFORM WIN32 %%%>'; EndLine:'<%%% END PLATFORM WIN32 %%%>'; PlatformType: pftWin32),
      ( StartLine:'<%%% START PLATFORM WIN64 %%%>'; EndLine:'<%%% END PLATFORM WIN64 %%%>'; PlatformType: pftWin64),
      ( StartLine:'<%%% START PLATFORM WIN64X %%%>'; EndLine:'<%%% END PLATFORM WIN64X %%%>'; PlatformType: pftWin64x) );
var
  OutFileName : string;
  oneLetterType : string;
  reqPackName : string;
  incFileName : string;
  outFile : TStringList;
  curLine, curLineTrim : string;
  tmpLines, repeatLines : TStrings;
  I : Integer;
  j : Integer;
  tmpStr : string;
  bcbId : string;
  bcblibsList : TStrings;
  TimeStampLine : Integer;
  Count, RequireCount, ContainCount, FormCount, LibCount, DefineCount: Integer;
  containsSomething : Boolean; // true if package will contain something
  repeatSectionUsed : Boolean; // true if at least one repeat section was used
  AddedLines: Integer;
  IgnoreNextSemicolon: Boolean;
  UnitFileName, UnitFilePath, UnitFileExtension, NoLinkPackageList: string;
  PathPAS, PathCPP, PathRC, PathASM, PathLIB: string;
  ItemIndex: Integer;
  CompilerDefines, Properties: TStringList;
  Replacements: array of string;
  TmpName: string;
  ProjectPlatformIdx: Integer;
begin
  Result := '';

  outFile := TStringList.Create;
  containsSomething := False;
  repeatSectionUsed := False;

  repeatLines := TStringList.Create;
  tmpLines := TStringList.Create;
  Properties := TStringList.Create;
  CompilerDefines := TStringList.Create;
  try
    // generate list of pathes
    PathPAS := '.;';
    PathCPP := '.;';
    PathRC := '.;';
    PathASM := '.;';
    PATHLIB := '';
    ContainCount := 0;
    FormCount := 0;
    for I := 0 to xml.ContainCount-1 do
      if xml.Contains[I].IsIncluded(Target) then
    begin
      Inc(ContainCount);
      if xml.Contains[I].FormName <> '' then
        Inc(FormCount);

      containsSomething := True;
      UnitFileName := xml.Contains[I].Name;
      UnitFilePath := ExtractFilePath(UnitFileName);
      if (UnitFilePath <> '') and (UnitFilePath[Length(UnitFilePath)] = DirDelimiter) then
        UnitFilePath := Copy(UnitFilePath, 1, Length(UnitFilePath)-1);
      UnitFilePath := UnitFilePath + ';';
      UnitFileExtension := ExtractFileExt(UnitFileName);
      if Pos(';'+UnitFilePath,PathLIB) = 0 then
        PathLIB := Format('%s%s',[PathLIB,UnitFilePath]);
      if CompareText(UnitFileExtension,'.pas') = 0 then
      begin
        if Pos(';'+UnitFilePath,PathPAS) = 0 then
          PathPAS := Format('%s%s',[PathPAS,UnitFilePath]);
      end
      else if CompareText(UnitFileExtension,'.asm') = 0 then
      begin
        if Pos(';'+UnitFilePath,PathASM) = 0 then
          PathASM := Format('%s%s',[PathASM,UnitFilePath]);
      end
      else if CompareText(UnitFileExtension,'.cpp') = 0 then
      begin
        if Pos(';'+UnitFilePath,PathCPP) = 0 then
          PathCPP := Format('%s%s',[PathCPP,UnitFilePath]);
      end
      else if CompareText(UnitFileExtension,'.rc') = 0 then
        if Pos(';'+UnitFilePath,PathRC) = 0 then
          PathRC := Format('%s%s',[PathRC,UnitFilePath]);
    end;
    // read the xml file
    OutFileName := xml.Name;
    OneLetterType := ProjectTypeToChar(xml.ProjectType);
    OutFileName := OutFileName + '-' + OneLetterType[1];
    if ProjectTypeIsDesign(xml.ProjectType) then
      OneLetterType := 'd'
    else
      OneLetterType := 'r';

    NoLinkPackageList := '';
    RequireCount := 0;
    for i := 0 to xml.RequireCount - 1 do
      if xml.Requires[i].IsIncluded(Target) then
      begin
        Inc(RequireCount);
        reqPackName := BuildPackageName(xml.Requires[i], target);
        if NoLinkPackageList = '' then
          NoLinkPackageList := reqPackName
        else
          NoLinkPackageList := Format('%s;%s', [NoLinkPackageList, reqPackName]);
      end;

    OutFileName := path + TargetToDir(target) + DirDelimiter +
                   ExpandPackageName(OutFileName, target)+
                   Extension;

    // project-wide properties if not redefined in xml
    CompilerDefines.Assign(FTargetList[GetNonPersoTarget(Target)].Defines);
    CompilerDefines.AddStrings(xml.CompilerDefines);
    DefineCount := CompilerDefines.Count;

    // read libs as a string of space separated value
    bcbId := FTargetList[GetNonPersoTarget(target)].Env+FTargetList[GetNonPersoTarget(target)].Ver;
    bcblibsList := nil;
    if CompareText(bcbId, 'c6') = 0 then
      bcblibsList := xml.C6Libs
    else
    if bcblibsList <> nil then
      LibCount := bcblibsList.Count
    else
      LibCount := 0;

    // prefetch replacements
    Properties.CaseSensitive := True;
    for i := 0 to FProjectProperties.Count - 1 do
    begin
      TmpName := FProjectProperties.Names[i]; 
      AddProperty(Properties, UpperCase(TmpName), FProjectProperties.Values[TmpName]);
    end;
    for i := 0 to xml.Properties.Count - 1 do
    begin 
      TmpName := xml.Properties.Names[i];
      AddProperty(Properties, UpperCase(TmpName), xml.Properties.Values[TmpName]);
    end;
    // add custom properties (with backward compatibility)
    AddProperty(Properties, 'NAME', PathExtractFileNameNoExt(OutFileName));
    AddProperty(Properties, 'XMLNAME', ExtractFileName(xmlName));
    AddProperty(Properties, 'DESCRIPTION', GetDescription(xml, target));
    AddProperty(Properties, 'C6PFLAGS', FDefinesConditionParser.EnsurePFlagsCondition(Properties.Values['C6PFLAGS']));
    AddProperty(Properties, 'GUID', xml.GUID);
    AddProperty(Properties, 'IMAGE_BASE', Properties.Values[UpperCase(ImageBaseKnownPackageProperty)]);
    AddProperty(Properties, 'IMAGE_BASE_INT', IntToStr(StrToInt('$' + Properties.Values[UpperCase(ImageBaseKnownPackageProperty)])));
    AddProperty(Properties, 'VERSION_MAJOR_NUMBER', Properties.Values[UpperCase(VersionMajorNumberKnownPackageProperty)]);
    AddProperty(Properties, 'VERSION_MINOR_NUMBER', Properties.Values[UpperCase(VersionMinorNumberKnownPackageProperty)]);
    AddProperty(Properties, 'RELEASE_NUMBER', Properties.Values[UpperCase(ReleaseNumberKnownPackageProperty)]);
    AddProperty(Properties, 'BUILD_NUMBER', Properties.Values[UpperCase(BuildNumberKnownPackageProperty)]);
    AddProperty(Properties, 'TYPE', Iff(ProjectTypeIsDesign(xml.ProjectType), 'DESIGN', 'RUN'));
    AddProperty(Properties, 'DATETIME', FormatDateTime('dd-mm-yyyy  hh:nn:ss', NowUTC) + ' UTC');
    AddProperty(Properties, 'type', OneLetterType);
    AddProperty(Properties, 'PATHPAS', PathPAS);
    AddProperty(Properties, 'PATHCPP', PathCPP);
    AddProperty(Properties, 'PATHASM', PathASM);
    AddProperty(Properties, 'PATHRC', PathRC);
    AddProperty(Properties, 'PATHLIB', PathLIB);
    AddProperty(Properties, 'PROJECT', ProjectTypeToProjectName(xml.ProjectType));
    AddProperty(Properties, 'BINEXTENSION', ProjectTypeToBinaryExtension(xml.ProjectType));
    AddProperty(Properties, 'ISDLL', Iff(ProjectTypeIsDLL(xml.ProjectType), 'True', 'False'));
    AddProperty(Properties, 'ISPACKAGE', Iff(ProjectTypeIsPackage(xml.ProjectType), 'True', 'False'));
    AddProperty(Properties, 'SOURCEEXTENSION', ProjectTypeToSourceExtension(xml.ProjectType));
    AddProperty(Properties, 'NOLINKPACKAGELIST', NoLinkPackageList);
    AddProperty(Properties, 'DEFINES', StringsToStr(CompilerDefines, ';', False));
    AddProperty(Properties, 'COMPILERDEFINES', Iff(DefineCount > 0, '-D' + StringsToStr(CompilerDefines, ';', False), ''));
    AddProperty(Properties, 'REQUIRECOUNT', IntToStr(RequireCount));
    AddProperty(Properties, 'CONTAINCOUNT', IntToStr(ContainCount));
    AddProperty(Properties, 'FORMCOUNT', IntToStr(FormCount));
    AddProperty(Properties, 'LIBCOUNT', IntToStr(LibCount));
    AddProperty(Properties, 'DEFINECOUNT', IntToStr(DefineCount));
    AddProperty(Properties, 'WIN32ENABLED', Iff(pftWin32 in XML.PlatformTypes, 'True', 'False'));
    AddProperty(Properties, 'WIN64ENABLED', Iff(pftWin64 in XML.PlatformTypes, 'True', 'False'));
    AddProperty(Properties, 'WIN64XENABLED', Iff(pftWin64x in XML.PlatformTypes, 'True', 'False'));
    SetLength(Replacements, Properties.Count * 2);
    for i := 0 to Properties.Count - 1 do
    begin
      TmpName := Properties.Names[i]; 
      Replacements[2*i] := TmpName + '%';
      Replacements[2*i+1] := Properties.Values[TmpName];
    end;

    // The time stamp hasn't been found yet
    TimeStampLine := -1;

    // read the lines of the templates and do some replacements
    i := 0;
    Count := template.Count;
    IgnoreNextSemicolon := False;
    while i < Count do
    begin
      curLine := template[i];
      if IsTrimmedStartsWith('<%%% ', curLine) then
      begin
        curLineTrim := Trim(curLine);
        if curLine = '<%%% START REQUIRES %%%>' then
        begin
          Inc(i);
          repeatSectionUsed := True;
          repeatLines.Clear;
          while (i < Count) and
                not IsTrimmedString(template[i], '<%%% END REQUIRES %%%>') do
          begin
            repeatLines.Add(template[i]);
            Inc(i);
          end;

          AddedLines := 0;
          ItemIndex := 0;
          for j := 0 to xml.RequireCount - 1 do
          begin
            // if this required package is to be included for this target
            if xml.Requires[j].IsIncluded(target) then
            begin
              Inc(ItemIndex);
              tmpLines.Assign(repeatLines);
              reqPackName := BuildPackageName(xml.Requires[j], target);
              MacroReplaceLines(tmpLines, '%',
                ['NAME%', reqPackName,
                 'INDEX%', IntToStr(ItemIndex),
                 'INDEX0%', IntToStr(ItemIndex - 1)]);
              // We do not say that the package contains something because
              // a package is only interesting if it contains files for
              // the given target
              // containsSomething := True;
              FDefinesConditionParser.EnsureCondition(tmpLines, xml.Requires[j].Condition);
              outFile.AddStrings(tmpLines);
              Inc(AddedLines);
            end;
          end;

          if (outFile.Count > 0) and (AddedLines = 0) then
          begin
            // delete "requires" clause.
            j := outFile.Count - 1;
            while (j > 0) and (Trim(outFile[j]) = '') do
              Dec(j);
            if CompareText(Trim(outFile[j]), 'requires') = 0 then
            begin
              outFile.Delete(j);
              IgnoreNextSemicolon := True;
            end;
          end
          else
            // if the last character in the output file is
            // a comma, then remove it. This possible comma will
            // be followed by a carriage return so we look
            // at the third character starting from the end
            AdjustEndingSemicolon(outFile);
        end
        else if curLineTrim = '<%%% START FILES %%%>' then
        begin
          Inc(i);
          repeatSectionUsed := True;
          repeatLines.Clear;
          while (i < Count) and
                not IsTrimmedString(template[i], '<%%% END FILES %%%>') do
          begin
            repeatLines.Add(template[i]);
            Inc(i);
          end;

          AddedLines := 0;
          ItemIndex := 0;
          for j := 0 to xml.ContainCount - 1 do
          begin
            // if this included file is to be included for this target
            if xml.Contains[j].IsIncluded(target) then
            begin
              Inc(ItemIndex);
              tmpLines.Assign(repeatLines);
              incFileName := xml.Contains[j].Name;
              ApplyFormName(xml.Contains[j], ItemIndex, tmpLines, target);
              FDefinesConditionParser.EnsureCondition(tmpLines, xml.Contains[j].Condition);
              outFile.AddStrings(tmpLines);
              Inc(AddedLines);

              // if this included file is not in the associated 'perso'
              // target or only in the 'perso' target then return the
              // 'perso' target name.
              if IsNotInPerso(xml.Contains[j], target) or
                 IsOnlyInPerso(xml.Contains[j], target) then
                Result := GetPersoTarget(target);
            end;
          end;

          if (outFile.Count > 0) and (AddedLines = 0) then
          begin
            // delete "contains" clause.
            j := outFile.Count - 1;
            while (j > 0) and (Trim(outFile[j]) = '') do
              Dec(j);
            if CompareText(Trim(outFile[j]), 'contains') = 0 then
            begin
              outFile.Delete(j);
              IgnoreNextSemicolon := True;
            end;
          end
          else
            // if the last character in the output file is
            // a comma, then remove it. This possible comma will
            // be followed by a carriage return so we look
            // at the third character starting from the end
            AdjustEndingSemicolon(outFile);
        end
        else if curLine = '<%%% START FORMS %%%>' then
        begin
          Inc(i);
          repeatSectionUsed := True;
          repeatLines.Clear;
          while (i < Count) and
                not IsTrimmedString(template[i], '<%%% END FORMS %%%>') do
          begin
            repeatLines.Add(template[i]);
            Inc(i);
          end;

          ItemIndex := 0;
          for j := 0 to xml.ContainCount - 1 do
          begin
            // if this included file is to be included for this target
            // and there is a form associated to the file
            if xml.Contains[j].IsIncluded(target) then
            begin
              containsSomething := True;
              if (xml.Contains[j].FormName <> '') then
              begin
                Inc(ItemIndex);
                tmpLines.Assign(repeatLines);
                ApplyFormName(xml.Contains[j], ItemIndex, tmpLines, target);
                FDefinesConditionParser.EnsureCondition(tmpLines, xml.Contains[j].Condition);
                outFile.AddStrings(tmpLines);
              end;

              // if this included file is not in the associated 'perso'
              // target or only in the 'perso' target then return the
              // 'perso' target name.
              if IsNotInPerso(xml.Contains[j], target) or
                 IsOnlyInPerso(xml.Contains[j], target) then
                Result := GetPersoTarget(target);
            end;

          end;
        end
        else if curLine = '<%%% START LIBS %%%>' then
        begin
          Inc(i);
          repeatLines.Clear;
          while (i < Count) and
                not IsTrimmedString(template[i], '<%%% END LIBS %%%>') do
          begin
            repeatLines.Add(template[i]);
            Inc(i);
          end;

          if bcblibsList <> nil then
          begin
            ItemIndex := 0;
            for j := 0 to bcbLibsList.Count - 1 do
            begin
              Inc(ItemIndex);
              tmpLines.Assign(repeatLines);
              MacroReplaceLines(tmpLines, '%',
                ['FILENAME%', bcblibsList[j],
                 'INDEX%', IntToStr(ItemIndex),
                 'INDEX0%', IntToStr(ItemIndex - 1),
                 'UNITNAME%', GetUnitName(bcblibsList[j])]);
              outFile.AddStrings(tmpLines);
            end;
          end;
        end
        else if curLine = '<%%% START COMPILER DEFINES %%%>' then
        begin
          Inc(i);
          repeatLines.Clear;
          while (i < Count) and
                not IsTrimmedString(template[i], '<%%% END COMPILER DEFINES %%%>') do
          begin
            repeatLines.Add(template[i]);
            Inc(i);
          end;
          ItemIndex := 0;
          for j := 0 to CompilerDefines.Count - 1 do
          begin
            Inc(ItemIndex);
            tmpLines.Assign(repeatLines);
            MacroReplaceLines(tmpLines, '%',
              ['COMPILERDEFINE%', CompilerDefines[j],
               'INDEX%', IntToStr(ItemIndex),
               'INDEX0%', IntToStr(ItemIndex - 1)]);
            outFile.AddStrings(tmpLines);
          end;
        end
        else if curLine = '<%%% DO NOT GENERATE %%%>' then
          Exit
        else if Pos('<%%% START PLATFORM ', curLine) = 1 then
        begin
          ProjectPlatformIdx := -1;
          for J := Low(ProjectPlatforms) to High(ProjectPlatforms) do
            if curLine = ProjectPlatforms[J].StartLine then
            begin
              ProjectPlatformIdx := J;
              Break;
            end;
          if ProjectPlatformIdx <> -1 then
          begin
            Inc(i);
            repeatLines.Clear;
            while (i < Count) and
                  not IsTrimmedString(template[i], ProjectPlatforms[ProjectPlatformIdx].EndLine) do
            begin
              repeatLines.Add(template[i]);
              Inc(i);
            end;

            if ProjectPlatforms[ProjectPlatformIdx].PlatformType in xml.PlatformTypes then
              outFile.AddStrings(repeatLines);
          end;
        end
        else for j := Low(ProjectConditionals) to High(ProjectConditionals) do
        begin
          if curLine = ProjectConditionals[j].StartLine then
          begin
            if xml.ProjectType <> ProjectConditionals[j].ProjectType then
              while (i < Count) and not IsTrimmedString(template[i], ProjectConditionals[j].EndLine) do
                Inc(i);
            Break;
          end;
        end
      end
      else
      begin
        if Pos('%', curLine) > 0 then
        begin
          tmpStr := curLine;
          if MacroReplace(curLine, '%', Replacements) then
          begin
            if Pos('%DATETIME%', tmpStr) > 0 then
              TimeStampLine := I;
          end;
        end;
        if IgnoreNextSemicolon then
        begin
          if (Trim(curLine) <> '') and (Trim(curLine) = ';') then
            IgnoreNextSemicolon := False
          else
            outFile.Add(curLine);
        end
        else
          outFile.Add(curLine);
      end;
      Inc(i);
    end;

    // test if there are required packages and/or contained files
    // that make the package require a different version for a
    // perso target. This is determined like that:
    // if a file is not in the associated 'perso'
    // target or only in the 'perso' target then return the
    // 'perso' target name.
    for j := 0 to xml.RequireCount - 1 do
    begin
      if IsNotInPerso(xml.Requires[j], target) or
         IsOnlyInPerso(xml.Requires[j], target) then
        Result := GetPersoTarget(target);
    end;
    for j := 0 to xml.ContainCount - 1 do
    begin
      if IsNotInPerso(xml.Contains[j], target) or
         IsOnlyInPerso(xml.Contains[j], target) then
        Result := GetPersoTarget(target);
    end;

    // if no repeat section was used, we must check manually
    // that at least one file is to be used by the given target.
    // This will then force the generation of the output file
    // (Useful for cfg templates for instance).
    // We do not check for the use of "required" packages because
    // a package is only interesting if it contains files for
    // the given target
    if not repeatSectionUsed then
    begin
      for j := 0 to xml.ContainCount - 1 do
        if xml.Contains[j].IsIncluded(target) then
        begin
          containsSomething := True;
          Break;
        end;
    end;

    // Save the file, if it contains something, and it
    // has changed when compared with the existing one
    if containsSomething and
       (HasFileChanged(OutFileName, templateName, outFile, TimeStampLine)) then
    begin
      tmpStr := ExtractFilePath(templateName);
      if tmpStr[length(tmpStr)] = DirDelimiter then
        SetLength(tmpStr, length(tmpStr)-1);
      if ExtractFileName(tmpStr) = FTargetList[GetNonPersoTarget(target)].PDir then
        SendMsg(SysUtils.Format(#9#9'Writing %s for %s (%s template used)', [ExtractFileName(OutFileName), target, target]))
      else
        SendMsg(SysUtils.Format(#9#9'Writing %s for %s', [ExtractFileName(OutFileName), target]));

      // if outfile contains line, save it.
      // else, it's because the template file was a binary file, so simply
      // copy it to the destination name
      SetFileAttributes(PChar(OutFileName), 0); // do not fail on read only files
      if outFile.count > 0 then
        outFile.SaveToFile(OutFileName)
      else
      begin
        CopyFile(PChar(templateName), PChar(OutFileName), False);
        FileSetDate(OutFileName, DateTimeToFileDate(Now)); // adjust file time
      end;
    end
    else
    if FileExists(OutFileName) and not containsSomething then
      SendMsg(SysUtils.Format(#9#9'File %s should be removed', [ExtractFileName(OutFileName)]));
  finally
    tmpLines.Free;
    repeatLines.Free;
    CompilerDefines.Free;
    outFile.Free;
  end;
end;
{$WARNINGS ON}

function TPackageGenerator.IsBinaryFile(const Filename: string): Boolean;
const
  BufferSize = 50;
  BinaryPercent = 10;
var
  F : TFileStream;
  Buffer : array[0..BufferSize] of AnsiChar;
  I, Index : Integer;
  BinaryCount : Integer;
begin
  Result := False;
  // If the cache contains information on that file, get the result
  // from it and skip the real test
  if FIsBinaryCache.Find(FileName, Index) then
  begin
    Result := Boolean(FIsBinaryCache.Objects[Index]);
    Exit;
  end;

  // Read the first characters of the file and if enough of them
  // are not text characters, then consider the file to be binary
  if FileExists(FileName) then
  begin
    F := TFileStream.Create(FileName, fmOpenRead);
    try
      F.Read(Buffer, (BufferSize+1) * SizeOf(AnsiChar));
      BinaryCount := 0;
      for I := 0 to BufferSize do
        if not (Buffer[I] in [#9, #13, #10, #32..#127]) then
          Inc(BinaryCount);

      Result := BinaryCount > BufferSize * BinaryPercent div 100;
    finally
      F.Free;
    end;
  end;

  // save the result in the cache
  FIsBinaryCache.AddObject(FileName, TObject(Result));
end;

// loads the .inc file into Defines and returns True if the Filename contains
// a "%t"
function TPackageGenerator.LoadDefines(const Target: string; Filename: string): Boolean;
var
  incfile : TStringList;
  ps: Integer;
  TargetDefines: TStrings;
begin
  Result := False;

  // read the include file for this target or the default file if jvclxx.inc does not exist
  ps := Pos('%t', Filename);
  if ps > 0 then
  begin
    Delete(Filename, ps, 2);
    Insert(LowerCase(Target), Filename, ps);
    if not FileExists(Filename) then
      Filename := FIncDefFileName;
    Result := True;
  end;

  if Length(Target) <> 0 then
    TargetDefines := FTargetList[GetNonPersoTarget(Target)].Defines
  else
    TargetDefines := nil;

  incfile := TStringList.Create;
  try
    if FileExists(Filename) then
      incfile.LoadFromFile(Filename);
    if not Assigned(FDefinesConditionParser) then
      FDefinesConditionParser := TDefinesConditionParser.Create(incfile, TargetDefines)
    else
      FDefinesConditionParser.Append(incfile, TargetDefines);
  finally
    incfile.free;
  end;
end;

function TPackageGenerator.Generate(packages : TStrings;
                                    targets : TStrings;
                                    callback : TGenerateCallback;
                                    const XmlFileName : string;
                                    const ModelName : string;
                                    var ErrMsg : string;
                                    path : string = '';
                                    prefix : string = '';
                                    format : string = '';
                                    incfileName : string = ''
                                   ) : Boolean;
var
  rec : TSearchRec;
  i : Integer;
  j : Integer;
  templateName, templateExtension, templateNamePers : string;
  xml : TPackageXmlInfo;
  xmlName : string;
  template, templatePers : TStringList;
  persoTarget : string;
  target : string;
  GenericIncFile: Boolean;

begin
  Result := True;

  if packages.Count = 0 then
  begin
    ErrMsg := '[Error] No package to generate, no xml file found';
    Result := False;
    Exit;
  end;

  if not LoadConfig(XmlFileName, ModelName, ErrMsg) then
  begin
    Result := False;
    Exit;
  end;

  // Empty the binary file cache
  FIsBinaryCache.Clear;

  if incFileName = '' then
    incFileName := FIncFileName;

  FCallBack := CallBack;

  if path = '' then
  begin
    if PathIsAbsolute(FPackagesLocation) then
      path := FPackagesLocation
    else
      path := PathNoInsideRelative(StrEnsureSuffix(DirDelimiter, FStartupDir) + FPackagesLocation);
  end;

  path := StrEnsureSuffix(DirDelimiter, path);

  if prefix <> '' then
    FPrefix := Prefix;
  if format <> '' then
    FFormat := Format;

  // for all targets
  for i := 0 to targets.Count - 1 do
  begin
    if Assigned(FDefinesConditionParser) then
      FDefinesConditionParser.Defines.Clear;

    GenericIncFile := LoadDefines('', ExtractFilePath(incFileName) + 'jedi%t.inc') and
                      LoadDefines('', incFileName);
    FDefinesConditionParser.Defines.RemoveDefine('JEDI_INC');
    FDefinesConditionParser.Defines.RemoveDefine('UNKNOWN_COMPILER_VERSION');

    target := targets[i];
    if GenericIncFile then
      LoadDefines(target, incFileName);

    SendMsg(SysUtils.Format('Generating packages for %s', [target]));
    // find all template files for that target
    if FindFirst(path + TargetToDir(target) + DirDelimiter + 'template.*',
                 faAnyFile, rec) = 0 then
    begin
      repeat
        template := TStringList.Create;
        templatePers := TStringList.Create;
        try
          SendMsg(SysUtils.Format(#9'Loaded %s', [rec.Name]));

          templateName := path + TargetToDir(target) + DirDelimiter + rec.Name;
          if IsBinaryFile(templateName) then
            template.Clear
          else
            template.LoadFromFile(templateName);

          // Try to find a template file named the same as the
          // current one in the perso directory so it can
          // be used instead
          templateNamePers := templateName;
          templatePers.Assign(template);
          persoTarget := GetPersoTarget(target);
          if (persoTarget <> '') and
             DirectoryExists(path+TargetToDir(persoTarget)) then
          begin
            templateNamePers := path + TargetToDir(persoTarget) + DirDelimiter + rec.Name;
            if FileExists(templateNamePers) then
            begin
              if IsBinaryFile(templateNamePers) then
                templatePers.Clear
              else
                templatePers.LoadFromFile(templateNamePers);
            end
            else
            begin
              templateNamePers := templateName;
            end
          end;

          // apply the template for all packages
          for j := 0 to packages.Count - 1 do
          begin
           // load (buffered) xml file
            xmlName := path + 'xml' + DirDelimiter + packages[j] + '.xml';
            xml := GetPackageXmlInfo(xmlName);

            TemplateExtension := ExtractFileExt(templateName);

            persoTarget := ApplyTemplateAndSave(
                                 path,
                                 target,
                                 packages[j],
                                 ExtractFileExt(rec.Name),
                                 template,
                                 xml,
                                 templateName,
                                 xmlName);

            // if the generation requested a perso target to be done
            // then generate it now, using the perso template
            if persoTarget <> '' then
            begin
              ApplyTemplateAndSave(
                 path,
                 persoTarget,
                 packages[j],
                 ExtractFileExt(rec.Name),
                 templatePers,
                 xml,
                 templateNamePers,
                 xmlName);
            end;
          end;
        finally
          template.Free;
          templatePers.Free;
        end;
      until FindNext(rec) <> 0;
    end
    else
      SendMsg(SysUtils.Format(#9'No template found for %s' , [target]));
    FindClose(rec);
  end;
{  if makeDof then
  begin
    SendMsg('Calling MakeDofs.bat');
    ShellExecute(0,
                '',
                PChar(StrEnsureSuffix(DirDelimiter, ExtractFilePath(ParamStr(0))) + 'MakeDofs.bat'),
                '',
                PChar(ExtractFilePath(ParamStr(0))),
                SW_SHOW);
  end;}
end;

{ TProjectProperties }

constructor TProjectProperties.Create(Node: TJclSimpleXmlElem);
var
  i: Integer;
  NameProp, ValueProp: TJclSimpleXMLProp;
begin
  inherited Create;

  if Assigned(Node) then
    for i := 0 to Node.Items.Count - 1 do
    begin
      NameProp := Node.Items.Item[i].Properties.ItemNamed['name'];
      ValueProp := Node.Items.Item[i].Properties.ItemNamed['value'];
      if Assigned(NameProp) and Assigned(ValueProp) then
        Values[NameProp.Value] := ValueProp.Value;
    end;
end;

end.
