{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: BuildHelpers.pas, released on 2003-11-28.

The Initial Developer of the Original Code is Andreas Hausladen [Andreas.Hausladen@gmx.de]
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-12-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}

unit BuildHelpers;
interface
uses
  Windows, SysUtils, Classes, Contnrs, CoreData;

type
  TMakeTarget = class(TObject)
  private
    FName: string;
    FSources: string; // should be a list but the BPG-make files have only one source
    FActions: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    
    property Name: string read FName;
    property Sources: string read FSources;
    property Actions: TStrings read FActions;
  end;

  TMakeFile = class(TObject)
  private
    FProjects: TStrings;
    FTargets: TObjectList;
    function GetTargetCount: Integer;
    function GetTargets(Index: Integer): TMakeTarget;
  protected
    procedure LoadFromFile(const Filename: string);
  public
    constructor Create(const Filename: string);
    destructor Destroy; override;

    property Projects: TStrings read FProjects;
    property TargetCount: Integer read GetTargetCount;
    property Targets[Index: Integer]: TMakeTarget read GetTargets;
  end;

  TPrepareBpgData = class(TObject)
  private
    FMake: TMakeFile;
    FCreatedFiles: TStrings;
    FCleaning: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property Make: TMakeFile read FMake;
    property Cleaning: Boolean read FCleaning write FCleaning;
    property CreatedFiles: TStrings read FCreatedFiles;
  end;

function GetReturnPath(const Dir: string): string;
function FastStringReplace(const Text, SearchText, ReplaceText: string;
  ReplaceAll, IgnoreCase: Boolean): string;
procedure CreateCfgFile(const Filename, SearchPaths, LibDir: string);
function PrepareBpg(const Filename: string; Target: TTargetInfo): TPrepareBpgData;
 // PrepareBpg creates Filename{.bpg->.mak) and the needed cfg files.
 // returns the number of Projects to compile (PROJECTS=xxx)"
function PrepareDcpBpg(const MakeFilename: string; Files: TStrings;
  Target: TTargetInfo; IsJcl: Boolean): TPrepareBpgData;
procedure CreateDelphiPackageForBCB(Package: TPackageInfo; Files: TStrings; IsJcl: Boolean);
procedure MoveBCBFiles(const Dir: string; Target: TTargetInfo);
procedure MoveHPPFiles(const Paths, StartDir: string; Target: TTargetInfo);
procedure DeleteDcuFiles(const Directory, StartDir: string);

const
  JclIncludePaths = '..\..\source\common';
  JclSourcePaths = '..\..\source\common;..\..\source\windows;..\..\source\vcl;..\..\source\visclx';
  JclLibDir = '..\..\lib';
  JVCLIncludePaths = '..\..\common';
  JVCLSourcePaths = '..\..\common;..\..\run;..\..\design';


implementation                          

const
  DefaultMakeFile =
    ''#10 +
    '!ifndef ROOT'#10 +
    'ROOT = $(MAKEDIR)\..'#10 +
    '!endif'#10 +
    '#------------------------------------------------------------------------------#'#10 +
    'MAKE = "$(ROOT)\bin\make.exe" -$(MAKEFLAGS) -f$**'#10 +
    'DCC  = "$(ROOT)\bin\dcc32.exe" -U"$(DCPDIR)" -LE"$(BPLDIR)" -LN"$(DCPDIR)" -Q -W -H -M $(DCCOPT)'#10 +// "$&.dpk"'#10 +
    'BRCC = "$(ROOT)\bin\brcc32.exe" $**'#10 +
    '#------------------------------------------------------------------------------#'#10;

//--------------------------------------------------------------------------------------------------

function GetReturnPath(const Dir: string): string;
var
  i: Integer;
begin
  Result := '';
  if Dir <> '' then
  begin
    Result := '..';
    for i := 1 to Length(Dir) do
      if Dir[i] = '\' then
        Result := Result + '\..';
  end;
end;

function CutFirstDir(var Dir: string): string;
var ps: Integer;
begin
  ps := Pos('\', Dir);
  if ps = 0 then
  begin
    Result := Dir;
    Dir := '';
  end
  else
  begin
    Result := Copy(Dir, 1, ps - 1);
    Delete(Dir, 1, ps);
  end;
end;

function GetAbsoluteDir(StartDir, RelDir: string): string;
var FirstDir: string;
begin
  while RelDir <> '' do
  begin
    FirstDir := CutFirstDir(RelDir);
    if FirstDir = '..' then
      StartDir := ExtractFileDir(StartDir)
    else if FirstDir = '.' then
    begin
      // do nothing
    end
    else
      StartDir := StartDir + '\' + FirstDir;
  end;
  Result := StartDir;
end;

function StrEqualText(Text: PChar; SearchText: PChar; MaxLen: Integer;
  IgnoreCase: Boolean): Boolean;
var
  i: Integer;
begin
  if IgnoreCase then
    Result := StrLIComp(Text, SearchText, MaxLen) = 0
  else
  begin
    Result := False;
    for i := 0 to MaxLen - 1 do
      if (Text[i] = #0) or {(SearchText[i] = #0) or}
         (Text[i] <> SearchText[i]) then Exit;
    Result := True;
  end;
end;

function FastStringReplace(const Text, SearchText, ReplaceText: string;
  ReplaceAll, IgnoreCase: Boolean): string;
var
  LenSearchText, LenReplaceText, LenText: Integer;
  Index, Len, StartIndex: Integer;
begin
  LenSearchText := Length(SearchText);
  LenReplaceText := Length(ReplaceText);
  LenText := Length(Text);
  if LenSearchText = 0 then
  begin
    Result := Text;
    Exit;
  end;

  if ReplaceAll then
  begin
    if LenReplaceText - LenSearchText > 0 then
      SetLength(Result, LenText +
        (LenReplaceText - LenSearchText) * (LenText div LenSearchText))
    else
      SetLength(Result, LenText);
  end
  else
    SetLength(Result, LenText + (LenReplaceText - LenSearchText));


  Len := 0;
  StartIndex := 1;
  for Index := 1 to LenText do
  begin
    if StrEqualText(PChar(Pointer(Text)) + Index - 1, Pointer(SearchText),
                   LenSearchText, IgnoreCase) then
    begin
      if Index > StartIndex then
      begin 
        Move(Text[StartIndex], Result[Len + 1], Index - StartIndex); 
        Inc(Len, Index - StartIndex); 
      end; 
      StartIndex := Index + LenSearchText; 

     // Ersetzungstext einfügen 
      if LenReplaceText > 0 then
      begin
        Move(ReplaceText[1], Result[Len + 1], LenReplaceText); 
        Inc(Len, LenReplaceText); 
      end; 

      if not ReplaceAll then Break; 
    end;
  end; 

  Index := LenText + 1;
  if Index > StartIndex then
  begin
    Move(Text[StartIndex], Result[Len + 1], Index - StartIndex);
    Inc(Len, Index - StartIndex);
  end;

  SetLength(Result, Len);
end;

procedure CreateCfgFile(const Filename, SearchPaths, LibDir: string);
var
  f: TextFile;
  List: TStrings;
  i: Integer;
begin
  AssignFile(f, Filename);
  Rewrite(f);
  try
    WriteLn(f, '-Q');
    WriteLn(f, '-N"..\..\' + LibDir + '"');
    WriteLn(f, '-LE"..\..\' + LibDir + '"');
    WriteLn(f, '-LN"..\..\' + LibDir + '"');
    WriteLn(f, '-U"..\..\' + LibDir + ';..\..\run;..\..\common"');
    WriteLn(f, '-O"..\..\run;..\..\common;..\..\dcu"');
    WriteLn(f, '-I"..\..\common');
    WriteLn(f, '-R"..\..\Resources"');

    List := TStringList.Create;
    try
      SplitPaths(SearchPaths, List);
      for i := 0 to List.Count - 1 do
        if List[i] <> '' then
        begin
          WriteLn(f, '-U"' + List[i] + '"');
          WriteLn(f, '-I"' + List[i] + '"');
        end;
    finally
      List.Free;
    end;
  finally
    CloseFile(f);
  end;
end;

procedure CreateMakeFile(Make: TMakeFile; const Path, MakeFileName,
  SearchPaths, LibDir, BplDir, DcpDir: string;
  CreatedFiles: TStrings; BCBTarget: Boolean; MajorVersion: Integer);
var
  Lines: TStrings;
  i, ActIndex: Integer;
  S, Dir, SourceFile: string;
  CfgFile: string;
begin
  Lines := TStringList.Create;
  try
    for i := 0 to Make.TargetCount - 1 do
      if Pos('..\', Make.Targets[i].Sources) > 0 then
      begin
        ActIndex := Make.Projects.IndexOf(Make.Targets[i].Name);
        if ActIndex <> -1 then
          Make.Projects.Delete(ActIndex);
      end;

    Lines.Add('#------------------------------------------------------------------------------#');
    Lines.Add('# JVCL 3 Package Installer - autogenerated makefile                            #');
    Lines.Add('#------------------------------------------------------------------------------#');
    Lines.Add('!ifndef DCPDIR');
    Lines.Add('DCPDIR = ' + DcpDir);
    Lines.Add('!endif');
    Lines.Add('!ifndef BPLDIR');
    Lines.Add('BPLDIR = ' + BplDir);
    Lines.Add('!endif');
    Lines.Text := Lines.Text + DefaultMakeFile;
    Lines.Add('PROJECTS = \');
    for i := 0 to Make.Projects.Count div 2 - 1 do
      Lines.Add(#9 + Make.Projects[i * 2] + ' ' + Make.Projects[i * 2 + 1] + ' \');
    if Make.Projects.Count mod 2 = 1 then
      Lines.Add(#9 + Make.Projects[Make.Projects.Count - 1] + ' \');
    Lines.Add('');
    Lines.Add('#------------------------------------------------------------------------------#');
    Lines.Add('default: $(PROJECTS)');
    Lines.Add('#------------------------------------------------------------------------------#');
    Lines.Add('');

    for i := 0 to Make.TargetCount - 1 do
    begin
      if Pos('..\', Make.Targets[i].Sources) > 0 then
        Continue;
      
      Lines.Add(Make.Targets[i].Name + ': ' + Make.Targets[i].Sources);
      Lines.Add(#9'@echo.');
      Lines.Add(#9'@echo Compiling package: ' + Make.Targets[i].Name);
      Dir := ExtractFileDir(Make.Targets[i].Sources);
      if Dir <> '' then
        Lines.Add(#9'@cd ' + Dir);

      SourceFile := ExtractFileName(Make.Targets[i].Sources);
      for ActIndex := 0 to Make.Targets[i].Actions.Count - 1 do
      begin
        S := Make.Targets[i].Actions[ActIndex];
        if CompareText(S, '$(DCC)') = 0 then S := S + Format(' "%s"', [SourceFile]);  
        S := FastStringReplace(S, '$**', SourceFile, True, False);
        S := FastStringReplace(S, '$*', Copy(SourceFile, 1, Length(SourceFile) - Length(ExtractFileExt(SourceFile))), True, False);
        if S[1] <> '@' then S := '@' + S;
        Lines.Add(#9 + S);
      end;

      if Dir <> '' then
        Lines.Add(#9'@cd ' + GetReturnPath(Dir));
      Lines.Add('');

      CfgFile := ChangeFileExt(Path + Make.Targets[i].Sources, '.cfg');
      if not BCBTarget then
      begin
        CreateCfgFile(CfgFile, SearchPaths, LibDir);
        CreatedFiles.Add(CfgFile);
      end
      else
      begin
        CreatedFiles.Add(ChangeFileExt(CfgFile, '.mak')); // created by $(BCB)\bin\bpr2mak.exe
        CreatedFiles.Add(ChangeFileExt(CfgFile, '') + IntToStr(MajorVersion) + '0.tds'); // created by $(BCB)\bin\bcc32
      end;
    end;

    Lines.SaveToFile(MakeFileName);
    CreatedFiles.Add(MakeFileName);
  finally
    Lines.Free;
  end;
end;

function PrepareBpg(const Filename: string; Target: TTargetInfo): TPrepareBpgData;
var
  Make: TMakeFile;
  i: Integer;
  Pkg: TPackageInfo;
begin
  Result := TPrepareBpgData.Create;
  try
    Make := TMakeFile.Create(Filename);
    try
      Result.FMake := Make;
      Result.FCleaning := True;
      for i := Make.Projects.Count - 1 downto 0 do
      begin
        Pkg := Target.Packages.FindPackage(ChangeFileExt(ExtractFileName(Make.Projects[i]), ''));
        if (Pkg <> nil) and (not Pkg.Install) then
        begin
          OutputDebugString(PChar('Removed: ' + Make.Projects[i]));
          Make.Projects.Delete(i);
        end;
      end;

      CreateMakeFile(Make, ExtractFilePath(FileName), ChangeFileExt(FileName, '.mak'),
        Target.SearchPaths, Target.LibDir, Target.BplDir, Target.DcpDir,
        Result.FCreatedFiles, Target.IsBCB, Target.MajorVersion);
    finally
      // Make.Free; done in Result.Destroy by the caller
    end;
  except
    Result.Free;
    raise;
  end;
end;

function PrepareDcpBpg(const MakeFilename: string; Files: TStrings;
  Target: TTargetInfo; IsJcl: Boolean): TPrepareBpgData;
var
  Make: TMakeFile;
  i: Integer;
  Item: TMakeTarget;
  Name: string;
begin
  Result := TPrepareBpgData.Create;
  try
    Make := TMakeFile.Create('');
    try
      Result.FMake := Make;
      Result.FCleaning := True;
      for i := 0 to Files.Count - 1 do
      begin
        Name := ChangeFileExt(ExtractFileName(Files[i]), '.bpl');
        Make.Projects.Add(Name);
        Item := TMakeTarget.Create;
        Make.FTargets.Add(Item);
        Item.FName := Name;
        if IsJcl then
          Item.FSources := Target.JclDirName + '\'
        else
          Item.FSources := Target.JVCLDirName + '\';
        Item.FSources := Item.FSources + ChangeFileExt(Name, '.dpk');
        Item.Actions.Add('$(DCC)');
      end;

      CreateMakeFile(Make, ExtractFilePath(MakeFileName), MakeFileName,
        Target.SearchPaths, Target.LibDir, Target.BplDir, Target.DcpDir,
        Result.FCreatedFiles, Target.IsBCB, Target.MajorVersion);
    finally
      // Make.Free; done in Result.Destroy by the caller
    end;
  except
    Result.Free;
    raise;
  end;
end;

const
  DelphiPackageFile =
    'package %s;'#10 + // <- package name
    ''#10 +
    '{$R *.res}'#10 +
    '{$ALIGN 8}'#10 +
    '{$ASSERTIONS OFF}'#10 +
    '{$BOOLEVAL OFF}'#10 +
    '{$DEBUGINFO OFF}'#10 +
    '{$EXTENDEDSYNTAX ON}'#10 +
    '{$IMPORTEDDATA ON}'#10 +
    '{$IOCHECKS ON}'#10 +
    '{$LOCALSYMBOLS OFF}'#10 +
    '{$LONGSTRINGS ON}'#10 +
    '{$OPENSTRINGS ON}'#10 +
    '{$OPTIMIZATION ON}'#10 +
    '{$OVERFLOWCHECKS OFF}'#10 +
    '{$RANGECHECKS ON}'#10 +
    '{$REFERENCEINFO OFF}'#10 +
    '{$SAFEDIVIDE OFF}'#10 +
    '{$STACKFRAMES OFF}'#10 +
    '{$TYPEDADDRESS OFF}'#10 +
    '{$VARSTRINGCHECKS ON}'#10 +
    '{$WRITEABLECONST ON}'#10 +
    '{$MINENUMSIZE 1}'#10 +
    '{$IMAGEBASE $4100000}'#10 +
    '{$DESCRIPTION ''%s''}'#10 +  // <- Description
    '{$RUNONLY}'#10 +
    '{$IMPLICITBUILD OFF}';

procedure CreateDelphiPackageForBCB(Package: TPackageInfo; Files: TStrings; IsJcl: Boolean);
var
  Filename, Prefix: string;
  S, FormName, Name: string;
  Lines: TStrings;
  i, ps: Integer;
begin
 // LibVer is added in CoreData.CreateJclPackageList for IsJcl
  if IsJcl then
  begin
    if Package.Target.IsDelphi then Prefix := 'D' else Prefix := 'C';
  end
  else
    Prefix := '';

  if IsJcl then
    Filename := Package.Target.JCLPackageDir + '\' + Package.Target.JclDirName + '\'
  else
    Filename := JVCLPackageDir + '\' + Package.Target.JVCLDirName + '\';
  Filename := Filename + Prefix + Package.DisplayName + '.dpk';

  Lines := TStringList.Create;
  try
    Lines.Text := Format(DelphiPackageFile, [Prefix + Package.DisplayName, Package.Description]);
    if (Package.Target.MajorVersion >= 6) and (IsJcl) then
      Lines.Add('{$LIBSUFFIX ''' + IntToStr(Package.Target.MajorVersion) + '0''}');
    if Package.Target.IsPersonal then
    begin
      if Package.Target.MajorVersion = 6 then
        Lines.Add('{$DEFINE D6PersonalEdition}');
      Lines.Add('{$DEFINE DelphiPersonalEdition}');
    end;

    Lines.Add('');
    Lines.Add('requires');
    for i := 0 to Package.RequireCount - 1 do
    begin
      if Package.Target.IsTargetFor(Package.Requires[i].Targets) then
      begin
        Name := Package.Requires[i].Name;
        if StartsWith(AnsiLowerCase(Name), 'jv') then
        begin
          ps := Pos('-r', AnsiLowerCase(Name));
          if ps > 0 then
          begin
            Delete(Name, ps, MaxInt);
            Name := Name + 'C' + IntToStr(Package.Target.MajorVersion) + 'R';
          end
          else
          begin
            ps := Pos('-d', AnsiLowerCase(Name));
            if ps > 0 then
            begin
              Delete(Name, ps, MaxInt);
              Name := Name + 'C' + IntToStr(Package.Target.MajorVersion) + 'D';
            end
          end;
        end;
        if Package.Requires[i].Condition <> '' then
          Lines.Add('{$IFDEF ' + Package.Requires[i].Condition + '}');
        Lines.Add('  ' + Name + ',');
        if Package.Requires[i].Condition <> '' then
          Lines.Add('{$ENDIF}');
      end;
    end;
    S := Lines[Lines.Count - 1];
    S[Length(S)] := ';';
    Lines[Lines.Count - 1] := S;

    Lines.Add('');
    Lines.Add('contains');
    for i := 0 to Package.ContaionCount - 1 do
    begin
      if Package.Target.IsTargetFor(Package.Contains[i].Targets) then
      begin
        if Package.Contains[i].Condition <> '' then
          Lines.Add('{$IFDEF ' + Package.Contains[i].Condition + '}');

        if Package.Contains[i].FormName <> '' then
          FormName := ' {' + Package.Contains[i].FormName + '},'
        else
          FormName := ',';

        Lines.Add('  ' + ChangeFileExt(ExtractFileName(Package.Contains[i].Name), '') +
           ' in ''' + Package.Contains[i].Name + '''' + FormName);
        if Package.Contains[i].Condition <> '' then
          Lines.Add('{$ENDIF}');
      end;
    end;
    S := Lines[Lines.Count - 1];
    S[Length(S)] := ';';
    Lines[Lines.Count - 1] := S;

    Lines.Add('');
    Lines.Add('end.');

    Lines.SaveToFile(FileName);
    Files.Add(FileName);
  finally
    Lines.Free;
  end;
end;

procedure MoveBCBFiles(const Dir: string; Target: TTargetInfo);
var
  sr: TSearchRec;
  Ext: string;
begin
  if not DirectoryExists(Target.DcpDir) then
    Exit;

  if FindFirst(Dir + '\*.*', faAnyFile and not faDirectory, sr) = 0 then
  try
    repeat
      Ext := AnsiLowerCase(ExtractFileExt(sr.Name));
      if (Ext = '.bpl') or (Ext = '.dcp') then
        MoveFile(Dir + '\' + sr.Name, Target.BplDir + '\' + sr.Name)
      else if (Ext = '.bpi') or (Ext = '.lib') then
        MoveFile(Dir + '\' + sr.Name, Target.DcpDir + '\' + sr.Name)
      else if (Ext = '.tds') then
        DeleteFile(Dir + '\' + sr.Name);
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
end;

procedure MoveHPPFiles(const Paths, StartDir: string; Target: TTargetInfo);
var
  sr: TSearchRec;
  DestDir, Dir: string;
  List: TStrings;
  i: Integer;
begin
  DestDir := Target.HppFilesDir;
  if not DirectoryExists(DestDir) then
    Exit;

  List := TStringList.Create;
  try
    SplitPaths(Paths, List);

    for i := 0 to List.Count - 1 do
    begin
      Dir := GetAbsoluteDir(StartDir, List[i]);
      if DirectoryExists(Dir) then
      begin
        if FindFirst(Dir + '\*.hpp', faAnyFile and not faDirectory, sr) = 0 then
        try
          repeat
            MoveFile(Dir + '\' + sr.Name, DestDir + '\' + sr.Name);
          until FindNext(sr) <> 0;
        finally
          FindClose(sr);
        end;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure DeleteDcuFiles(const Directory, StartDir: string);
var
  sr: TSearchRec;
  Dir: string;
begin
  Dir := GetAbsoluteDir(StartDir, Directory);
  if not DirectoryExists(Dir) then
    Exit;

  if FindFirst(Dir + '\Jcl*.dcu', faAnyFile and not faDirectory, sr) = 0 then
  try        
    repeat
      DeleteFile(Dir + '\' + sr.Name);
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
end;

{ TMakeTarget }

constructor TMakeTarget.Create;
begin
  inherited Create;
  FActions := TStringList.Create;
end;

destructor TMakeTarget.Destroy;
begin
  FActions.Free;
  inherited Destroy;
end;

{ TMakeFile }

constructor TMakeFile.Create(const Filename: string);
begin
  inherited Create;
  FProjects := TStringList.Create;
  FTargets := TObjectList.Create;
  if Filename <> '' then
    LoadFromFile(Filename);
end;

destructor TMakeFile.Destroy;
begin
  FProjects.Free;
  FTargets.Free;
  inherited Destroy;
end;

function TMakeFile.GetTargetCount: Integer;
begin
  Result := FTargets.Count;
end;

function TMakeFile.GetTargets(Index: Integer): TMakeTarget;
begin
  Result := TMakeTarget(FTargets[Index]);
end;

procedure TMakeFile.LoadFromFile(const Filename: string);
var
  Lines: TStrings;
  i, ps: Integer;
  S: string;
  PrjS: string;
  F, P: PChar;
  Item: TMakeTarget;
begin
  FProjects.Clear;
  FTargets.Clear;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(Filename);
    i := 0;
    while i < Lines.Count do
    begin
      S := Lines[i];
      if StartsWith(S, 'PROJECTS =') then
      begin
        PrjS := '';
        Delete(S, 1, 10);
        S := Trim(S);
        while (i < Lines.Count) and (S[Length(S)] = '\') do
        begin
          Delete(S, Length(S), 1);
          PrjS := PrjS + ' ' + S;
          Inc(i);
          S := Trim(Lines[i]);
        end;
        PrjS := PrjS + ' ' + S;
      end
      else
      begin
        ps := Pos('.bpl:', S);
        if ps > 0 then
        begin
          Item := TMakeTarget.Create;
          FTargets.Add(Item);
          Item.FName := Trim(Copy(S, 1, ps + 3));
          Item.FSources := Trim(Copy(S, ps + 5, MaxInt));
          Inc(i);
          while (i < Lines.Count) and (Trim(Lines[i]) <> '') do
          begin
            Item.FActions.Add(Trim(Lines[i]));
            Inc(i);
          end;
        end;
      end;

      Inc(i);
    end;
  finally
    Lines.Free;
  end;

 // parse "PrjS"
  P := PChar(PrjS);
  while P[0] <> #0 do
  begin
    while P[0] in [#1..#32] do Inc(P);
    F := P;
    while not (P[0] in [#0..#32]) do Inc(P);
    if P - F > 0 then
    begin
      SetString(S, F, P - F);
      FProjects.Add(S);
    end;
  end;
end;

{ TPrepareBpgData }

constructor TPrepareBpgData.Create;
begin
  inherited Create;
  FCreatedFiles := TStringList.Create;
end;

destructor TPrepareBpgData.Destroy;
var i: Integer;
begin
  if FCleaning then
    for i := 0 to FCreatedFiles.Count - 1 do
      DeleteFile(FCreatedFiles[i]);
  FCreatedFiles.Free;
  FMake.Free;
  inherited Destroy;
end;

end.
