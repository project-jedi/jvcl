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

Last Modified: 2003-11-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{.$I JVCL.INC}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$IFDEF VER150}
 // Delphi 7 .NET preview warnings
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

unit BuildHelpers;
interface
uses
  SysUtils, Classes, Contnrs, CoreData;

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
  end;

function GetReturnPath(const Dir: string): string;
function FastStringReplace(const Text, SearchText, ReplaceText: string;
  ReplaceAll, IgnoreCase: Boolean): string;
procedure CreateCfgFile(const Filename, SearchPaths, LibDir: string);
function PrepareBpg(const Filename, SearchPaths, LibDir, BplDir, DcpDir: string): TPrepareBpgData;
 // PrepareBpg creates Filename{.bpg->.mak) and the needed cfg files.
 // returns the number of Projects to compile (PROJECTS=xxx)"

implementation

const
  DefaultMakeFile =
    ''#10 +
    '!ifndef ROOT'#10 +
    'ROOT = $(MAKEDIR)\..'#10 +
    '!endif'#10 +
    '#------------------------------------------------------------------------------#'#10 +
    'MAKE = "$(ROOT)\bin\make.exe" -$(MAKEFLAGS) -f$**'#10 +
    'DCC  = "$(ROOT)\bin\dcc32.exe" -U"$(DCPDIR)" -LE"$(BPLDIR)" -LN"$(DCPDIR)" -Q -W -H -M $(DCCOPT) "$&.dpk"'#10 +
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
    WriteLn(f, '-N"..\..\' + LibDir + '"');
    WriteLn(f, '-LE"..\..\' + LibDir + '"');
    WriteLn(f, '-LN"..\..\' + LibDir + '"');
    WriteLn(f, '-U"..\..\' + LibDir + ';..\..\run;..\..\common;..\..\Resources"');
    WriteLn(f, '-O"..\..\run;..\..\common;..\..\Resources"');
    WriteLn(f, '-I"..\..\run;..\..\common;..\..\Resources"');
    WriteLn(f, '-R"..\..\run;..\..\common;..\..\Resources"');

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

function PrepareBpg(const Filename, SearchPaths, LibDir, BplDir, DcpDir: string): TPrepareBpgData;
var
  Make: TMakeFile;
  Lines: TStrings;
  i, ActIndex: Integer;
  S, Dir: string;
  CfgFile, SourceFile: string;
begin
  Result := TPrepareBpgData.Create;

  Make := TMakeFile.Create(Filename);
  try
    Result.FMake := Make;
    Result.FCleaning := True;

    Lines := TStringList.Create;

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
        S := FastStringReplace(S, '$**', SourceFile, True, False);
        S := FastStringReplace(S, '$*', Copy(SourceFile, 1, Length(SourceFile) - Length(ExtractFileExt(SourceFile))), True, False);
        if S[1] <> '@' then S := '@' + S;
        Lines.Add(#9 + S);
      end;

      if Dir <> '' then
        Lines.Add(#9'@cd ' + GetReturnPath(Dir));
      Lines.Add('');

      CfgFile := ChangeFileExt(ExtractFilePath(Filename) + Make.Targets[i].Sources, '.cfg');
      CreateCfgFile(CfgFile, SearchPaths, LibDir);
      Result.FCreatedFiles.Add(CfgFile);
    end;

    Lines.SaveToFile(ChangeFileExt(Filename, '.mak'));
    Result.FCreatedFiles.Add(ChangeFileExt(Filename, '.mak'));
  finally
    // Make.Free; done in Result.Destroy
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
