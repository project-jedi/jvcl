const
  DelphiIDEFileName          = 'Bin\delphi32.exe';
  DelphiRepositoryFileName   = 'Bin\delphi32.dro';
  DCC32FileName              = 'Bin\dcc32.exe';
  BRCC32FileName             = 'Bin\brcc32.exe';
  DelphiKeyName              = 'SOFTWARE\Borland\Delphi';
  BCBKeyName                 = 'SOFTWARE\Borland\C++Builder';

  LibraryKeyName             = 'Library';
  LibraryRootDir             = 'RootDir';
  LibrarySearchPathValueName = 'Search Path';
  LibraryBPLOutputValueName  = 'Package DPL Output';
  LibraryDCPOutputValueName  = 'Package DCP Output';
  KnownPackagesKeyName       = 'Known Packages';
  DCC32CFGFileName           = 'DCC32.CFG';

  DelphiHelpContentFileName  = 'Help\%s.ohc';
  DelphiHelpIndexFileName    = 'Help\%s.ohi';
  DelphiHelpLinkFileName     = 'Help\%s.ohl';
  DelphiHelpProjectFileName  = 'Help\%s.ohp';
  DelphiHelpGidFileName      = 'Help\%s.gid';
  MSHelpSystemKeyName        = 'Software\Microsoft\Windows\Help';
  
#define DEBUGOUTPUT
procedure Log(Msg:String);
begin
#if defined DEBUGOUTPUT
  MsgBox(Msg,mbInformation, MB_OK);
#endif  
end;

// utility function for testing the Check: attribute
function ResultFalse:boolean;
begin
  Result := false;
end;

// utility function for testing the Check: attribute
function ResultTrue:boolean;
begin
  Result := true;
end;


function ExtractDOFDirectories(DOFFilename:string;UnitOutputDir:boolean):string;
var
  S:TStringlist;
  i,j:integer;
  AFind:string;
begin
  Result := '';
  DOFFilename := ExpandConstant(DOFFilename);
  if not FileExists(DOFFilename) then Exit;
  if UnitOutputDir then
    AFind := LowerCase('UnitOutputDir=')
  else
    AFind := LowerCase('SearchPath=');

  S := TStringlist.Create;
  try
    S.LoadFromFile(DOFFilename);
    for i := 0 to S.Count - 1 do
    begin
      j := Pos(AFind,LowerCase(S.Strings[i]));
      if j = 1 then
      begin
        j := Pos('=',S.Strings[i]);
        if j > 0 then
          Result := Copy(S.Strings[i],j + 1,Length(S.Strings[I]));
        Exit;
      end;
    end;
  finally
    S.Free;
  end;
end;

function IsVersionEqualOrAbove(S:string;Major, Minor, Release, Build:integer):boolean;
var
  Version:array of integer;i,j:integer;
begin
  SetArrayLength(Version,4);
  for i := 0 to 3 do
  begin
    Version[i] := 0;
    j := Pos('.',S);
    if j > 0 then
    begin
      Version[i] := StrToIntDef(Copy(S,1,j-1),0);
      S := Copy(S,j+1, Length(S));
    end
    else
    begin
      Version[i] := StrToIntDef(S,0);
      S := '';
    end;
  end;
  Result := false;
  if Version[0] < Major then Exit;
  if Version[1] < Minor then Exit;
  if Version[2] < Release then Exit;
  if Version[3] < Build then Exit;
  Result := true;
end;


// folder where DCP files are output. DelphiVersion should be on the form '5.0', '6.0' etc
function DelphiDCPFolder(DelphiVersion:string):string;
var S:string;
begin
  S := AddBackSlash(DelphiKeyName) + AddBackSlash(DelphiVersion) + LibraryKeyName;
  if not RegQueryStringValue(HKCU, S, LibraryDCPOutputValueName,Result) then
    Result := '';
end;

// folder where BPL files are output. DelphiVersion should be on the form '5.0', '6.0' etc
function DelphiBPLFolder(DelphiVersion:string):string;
var S:string;
begin
  S := AddBackSlash(DelphiKeyName) + AddBackSlash(DelphiVersion) + LibraryKeyName;
  if not RegQueryStringValue(HKCU, S, LibraryBPLOutputValueName,Result) then
    Result := '';
end;

// root folder where Delphi is installed (without trailing backslash). DelphiVersion should be on the form '5.0', '6.0' etc
function DelphiRootDir(DelphiVersion:string):string;
begin
  if not RegQueryStringValue(HKLM, AddBackSlash(DelphiKeyName) + DelphiVersion, LibraryRootDir, Result) then
    Result := '';
end;

// path and filename of dcc.exe
function DelphiDCC(DelphiVersion:string):string;
begin
  Result := DelphiRootDir(DelphiVersion);
  if Result <> '' then
  begin
    Result := AddBackSlash(Result) + DCC32FileName;
    if not FileExists(Result) then
      Result := '';
  end;
end;

// path and filename of brcc32.exe
function DelphiBRCC(DelphiVersion:string):string;
begin
  Result := DelphiRootDir(DelphiVersion);
  if Result <> '' then
  begin
    Result := AddBackSlash(Result) + BRCC32FileName;
    if not FileExists(Result) then
      Result := '';
  end;
end;

function IsDelphiInstalled(DelphiVersion:string):boolean;
begin
  Result := FileExists(AddBackSlash(DelphiRootDir(DelphiVersion)) + DelphiIDEFileName);
//  Result := RegKeyExists(HKLM, 'Software\Borland\Delphi\' + DelphiVersion);
end;

function IsBCBInstalled(BCBVersion:string):boolean;
begin
  Result := RegKeyExists(HKLM, AddBackSlash(BCBKeyName) + BCBVersion);
end;

function DCCCompile(DelphiVersion, CommandLine, WorkDir:string):boolean;
var S,OldDir:string;ACode:integer;
begin
  Result := false;
  S := DelphiDCC(DelphiVersion);
  if FileExists(S) then
  begin
    OldDir := GetCurrentDir;
    try
      SetCurrentDir(ExpandConstant(WorkDir));
      if FileExists(DCC32CFGFileName) then DeleteFile(DCC32CFGFileName);
//      Log('DCCCompile: ' + #13#10 + S + ' ' + CommandLine + ' (' + WorkDir + ')');
      Result := InstExec(S, CommandLine, WorkDir, true, false, SW_HIDE, ACode);
    finally
      SetCurrentDir(OldDir);
    end;
    if not Result then SysErrorMessage(ACode);
  end;
end;

function InstallDelphiPackage(DelphiVersion, PackageFileName, PackageDescription:string):boolean;
begin
  Result := RegWriteStringValue(HKCU,AddBackSlash(DelphiKeyName) + DelphiVersion + KnownPackagesKeyName,
    ExpandConstant(PackageFileName), PackageDescription);
end;

function BuildDelphiPackage(DelphiVersion, PackageName, PackageDescription:String; Install:boolean):boolean;
var DCC:string;
begin
  Result := false;
  DCC := DelphiDCC(DelphiVersion);
  if DCC = '' then Exit;
  // extract paths from dof, build command-line, call dcc.exe
  if Result and Install then
    Result := InstallDelphiPackage(AddBackSlash(DelphiBPLFolder(DelphiVersion)) + ChangeFileExt(ExtractFileName(PackageName),'.bpl'),
      PackageDescription, DelphiVersion);
end;

// installs the specified help file in the OpenHelp system
// DelphiVersion should be on the form '5.0', '6.0' etc
function InstallDelphiOpenHelpFile(DelphiVersion, HelpFile:string):boolean;
begin
  HelpFile := ExpandConstant(HelpFile);

end;

