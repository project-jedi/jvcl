(****************************************************************************
 * WANT - A build management tool.                                          *
 * Copyright (c) 2001-2003 Juancarlo Anez, Caracas, Venezuela.              *
 * All rights reserved.                                                     *
 *                                                                          *
 * This library is free software; you can redistribute it and/or            *
 * modify it under the terms of the GNU Lesser General Public               *
 * License as published by the Free Software Foundation; either             *
 * version 2.1 of the License, or (at your option) any later version.       *
 *                                                                          *
 * This library is distributed in the hope that it will be useful,          *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        *
 * Lesser General Public License for more details.                          *
 *                                                                          *
 * You should have received a copy of the GNU Lesser General Public         *
 * License along with this library; if not, write to the Free Software      *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA *
 ****************************************************************************)
{
    @brief 

    @author Juancarlo Añez
}

unit PatternSets;

interface
uses
  Classes,
  JALOwnedTrees,
  WildPaths,
  WantUtils,
  WantClasses;

type
  TPatternPart = class(TScriptElement)
  protected
    procedure SetValue(Value: string); virtual; abstract;
  published
    property name: string write SetValue;
  end;

  TIncludeElement = class(TPatternPart)
    procedure SetValue(Value: string); override;
  end;

  TExcludeElement = class(TPatternPart)
    procedure SetValue(Value: string); override;
  end;

  TPatternSet = class(TScriptElement)
  protected
    FIncludes: TStrings;
    FExcludes: TStrings;

    FPatternSets: array of TPatternSet;

    procedure InsertNotification(Child :TTree); override;

    procedure AddPatternSet(APatternSet: TPatternSet);
    procedure SetRefId(Id :string);

    procedure SetIncludes(Value: TStrings);
    procedure SetExcludes(Value: TStrings);


    procedure DoInclude( Files: TStrings; Pattern: TPath; Base: string;
                         IncAtt: TFileAttributes = AnyFileAttribute;
                         ExcAtt: TFileAttributes = []
                         ); virtual;
    procedure DoExclude(Files: TStrings; Pattern: TPath; Base: string); virtual;

    procedure DoIncludes(Files: TStrings; Base: string;
                         IncAtt: TFileAttributes = AnyFileAttribute;
                         ExcAtt: TFileAttributes = []
                         ); virtual;
    procedure DoExcludes(Files: TStrings; Base: string); virtual;

  public
    constructor Create(Owner: TScriptElement); override;
    destructor  Destroy; override;

    procedure Include(Pattern: TPath);  overload;
    procedure Exclude(Pattern: TPath);  overload;


    function  Paths  : TPaths;
    procedure GetPaths(Files: TStrings);
    procedure AddPaths(Paths: TPaths);

    function SystemPaths: TSystemPaths;
    function RelativePaths: TPaths;
    function MovePaths(ToBase: TPath): TPaths;

    property Includes: TStrings read FIncludes;
    property Excludes: TStrings read FExcludes;
  published
    function createInclude: TIncludeElement;
    function createExclude: TExcludeElement;
    property id;
    property refid :string write SetRefId;
  end;     


  TCustomFileSet  = class(TPatternSet)
  public
    procedure AddDefaultPatterns; virtual;
    property dir: TPath read GetBaseDir write SetBaseDir;
  end;

  TCustomDirSet  = class(TCustomFileSet)
  protected
    procedure DoIncludes(Files: TStrings; Base: string;
                         IncAtt: TFileAttributes = AnyFileAttribute;
                         ExcAtt: TFileAttributes = []
                         ); override;
    procedure DoExcludes(Files: TStrings; Base: string); override;
  end;

  TFileSet  = class(TCustomFileSet)
  published
    property dir;
  end;

  TDirSet = class(TCustomDirSet)
  published
    property dir;
  end;

implementation

{ TIncludeElement }

procedure TIncludeElement.SetValue(Value: string);
begin
 (Owner as TPatternSet).Include(Value);
end;

{ TExcludeElement }

procedure TExcludeElement.SetValue(Value: string);
begin
 (Owner as TPatternSet).Exclude(Value);
end;

{ TPatternSet }

constructor TPatternSet.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  FIncludes := TStringList.Create;
  FExcludes := TStringList.Create;
end;

destructor TPatternSet.Destroy;
begin
  FreeAndNil(FIncludes);
  FreeAndNil(FExcludes);
  inherited Destroy;
end;

procedure TPatternSet.SetIncludes(Value: TStrings);
begin
  FIncludes.Assign(Value);
end;

procedure TPatternSet.SetExcludes(Value: TStrings);
begin
  FExcludes.Assign(Value);
end;

procedure TPatternSet.Include(Pattern: TPath);
begin
  Assert(Pattern <> '');
  FIncludes.Add(Pattern);
end;

procedure TPatternSet.Exclude(Pattern: TPath);
begin
  FExcludes.Add(Pattern);
end;

procedure TPatternSet.DoInclude(Files: TStrings; Pattern: TPath; Base: string; IncAtt, ExcAtt: TFileAttributes);
begin
  Wild(Files, Pattern, Base, IncAtt, ExcAtt);
end;

procedure TPatternSet.DoExclude(Files: TStrings; Pattern: TPath; Base: string);
var
  Excluded: TPaths;
  Path    : TPath;
  f       : Integer;
begin
  Excluded := SplitPath(ToRelativePath(Pattern));
  for f := Files.Count-1 downto 0 do
  begin
    Path := ToRelativePath(Files[f]);
    if IsMatch(Excluded, SplitPath(Path)) then
      Files.Delete(f);
  end;
end;

function TPatternSet.createInclude: TIncludeElement;
begin
  Result := TIncludeElement.Create(Self);
end;

function TPatternSet.createExclude: TExcludeElement;
begin
  Result := TExcludeElement.Create(Self);
end;

procedure TPatternSet.DoIncludes(Files: TStrings; Base: string; IncAtt, ExcAtt: TFileAttributes);
var
  i: Integer;
begin
  if (Base <> '')
  and (FIncludes.Count = 0)
  and (Length(FPatternSets) = 0)
  then
    DoInclude(Files, '**', Base, IncAtt, ExcAtt)
  else
    for i := 0 to FIncludes.Count-1 do
      DoInclude(Files, FIncludes[i], Base, IncAtt, ExcAtt);

  for i := Low(FPatternSets) to High(FPatternSets) do
    FPatternSets[i].DoIncludes(Files, Base, IncAtt, ExcAtt);
end;


procedure TPatternSet.DoExcludes(Files: TStrings; Base: string);
var
  i: Integer;
begin
  for i := 0 to FExcludes.Count-1 do
    DoExclude(Files, FExcludes[i], Base);

  for i := Low(FPatternSets) to High(FPatternSets) do
    FPatternSets[i].DoExcludes(Files, Base);
end;

procedure TPatternSet.AddPatternSet(APatternSet: TPatternSet);
begin
  SetLength(FPatternSets, 1+Length(FPatternSets));
  FPatternSets[High(FPatternSets)] := APatternSet;
end;

procedure TPatternSet.SetRefId(Id: string);
begin
  AddPatternSet(Project.FindChild(Id, TPatternSet) as TPatternSet);
end;

function TPatternSet.Paths: TPaths;
var
  Files   : TStringList;
begin
  Files := TStringList.Create;
  try
    Files.Sorted := True;

    Log(vlDebug, 'fileset basedir="%s"', [basedir]);
    GetPaths(Files);

    Result := StringsToPaths(Files);
  finally
    FreeAndNil(Files);
  end;
end;


procedure TPatternSet.AddPaths(Paths: TPaths);
var
  Files   : TStringList;
  i, n    : Integer;
begin
  Files := TStringList.Create;
  try
    Files.Sorted := True;

    GetPaths(Files);

    n := Length(Paths);
    SetLength(Paths, n + Files.Count);
    for i := 0 to Files.Count-1 do
      Paths[i+n] := Files[i];
  finally
    FreeAndNil(Files);
  end;
end;


procedure TPatternSet.GetPaths(Files: TStrings);
begin
  DoIncludes(Files, BasePath);
  DoExcludes(Files, BasePath);
end;

function TPatternSet.MovePaths(ToBase: TPath): TPaths;
begin
  Result := WildPaths.MovePaths(Paths, BasePath, ToBase);
end;

function TPatternSet.RelativePaths: TPaths;
begin
  Result := ToRelativePaths(Paths, BasePath);
end;

function TPatternSet.SystemPaths: TSystemPaths;
begin
   Result := ToSystemPaths(Paths);
end;


procedure TPatternSet.InsertNotification(Child: TTree);
begin
  if Child is TPatternSet then
    AddPatternSet(TPatternSet(Child));
end;

{ TCustomFileSet }

procedure TCustomFileSet.AddDefaultPatterns;
begin
  // add the default Ant excludes
  Exclude('**/CVS/**');
  Exclude('**/.cvsignore');

  Exclude('**/*~');
  Exclude('**/#*#');
  Exclude('**/%*%');

  // Some additional excludes
  Exclude('**/*.*~*');
  Exclude('**/*.bak');
  Exclude('**/dunit.ini');
end;

{ TCustomDirSet }

procedure TCustomDirSet.DoIncludes(Files: TStrings; Base: string; IncAtt, ExcAtt: TFileAttributes);
begin
  inherited DoIncludes(Files, Base, [Directory], ExcAtt);
  //inherited DoIncludes(Files, Base, IncAtt, ExcAtt);
end;

procedure TCustomDirSet.DoExcludes(Files: TStrings; Base: string);
var
  f       : Integer;
begin
  for f := Files.Count-1 downto 0 do
    if not PathIsdir(PathConcat(Base, Files[f])) then
      Files.Delete(f);
  inherited DoExcludes(Files, Base);
end;


initialization
  RegisterElement(TPatternSet);
end.
