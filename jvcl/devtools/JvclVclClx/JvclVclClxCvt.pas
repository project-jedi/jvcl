{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvclVclClxCvt.pas, released on 2004-05-19.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvclVclClxCvt;

interface

uses
  SysUtils, Classes, VclClxCvt, PackageInformation, PackageModels;

type
  TJVCLConverter = class(TVCLConverter)
  protected
    procedure InitUnitReplaceList; override;
    function ChangeFileName(const Name: String): String; override;
    procedure TranslateUnit(var AName: String); override;
    procedure TranslateInc(var AName: String); override;
    procedure TranslateResource(var AName: String); override;
    procedure BeforeSave(const Filename: string; Lines: TStrings); override;
  public
    constructor Create(const AIniDirectory: string);
    destructor Destroy; override;
  end;

  TProgressEvent = procedure(Sender: TObject; const Text: string;
    Position, Max: Integer) of object;

  TConverter = class(TObject)
  private
    FPackageModels: TPackageModelList;
    FModel: TPackageModel; 
    FJVCLDir: string;
    FCvt: TJVCLConverter;
    FOnProgress: TProgressEvent;
  protected
    function GetDestFilename(const Filename: string): string;
    function IgnoredFile(const Filename: string): Boolean;
  public
    constructor Create(const AJVCLDir: string);
    destructor Destroy; override;

    function CreateClxFiles: Integer;
    property PackageModels: TPackageModelList read FPackageModels;
    property JVCLDir: string read FJVCLDir;
    property Cvt: TJVCLConverter read FCvt;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

uses
  Utils, StrUtils;

function JvclBplNameToGenericNameHook(const BplName: string): string;
begin
   // obtain package name used in the xml file
  Result := ChangeFileExt(BplName, '');
  Delete(Result, Length(Result) - 2, 2);
  if Length(Result) > 2 then
  begin
    if Result[3] = 'Q' then
      Delete(Result, 3, 1);
    Insert('-', Result, Length(Result)); // do not localize
  end;
end;

{ TJVCLConverter }

constructor TJVCLConverter.Create(const AIniDirectory: string);
begin
  inherited Create(AIniDirectory);
  //ReduceConditions := False;
  KeepLines := False;
  ConditionVCL := 'VCL';
  ConditionCLX := 'VisualCLX';
end;

destructor TJVCLConverter.Destroy;
begin

  inherited Destroy;
end;

procedure TJVCLConverter.InitUnitReplaceList;
var
  Lines: TStrings;
  i: Integer;
begin
  inherited InitUnitReplaceList; // load VCL conversions
  UnitReplaceList.AddFromIni(IniDirectory + PathDelim + 'convertqvcl.ini');

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(IniDirectory + PathDelim + 'jvclremconditions.ini');
    for i := 0 to Lines.Count - 1 do
      if not IsEmptyStr(Lines[i]) then
        RemoveConditions.Add(Trim(Lines[i]));
  finally
    Lines.Free;
  end;
end;

function TJVCLConverter.ChangeFileName(const Name: String): String;
begin
  if AnsiStartsText('Jv', Name) then
  begin
    Result := Name;
    Insert('Q', Result, 3);
  end;
end;

procedure TJVCLConverter.TranslateUnit(var AName: String);
begin
  if AnsiStartsText('Jv', AName) then
    Insert('Q', AName, 3)
  else
    inherited TranslateUnit(AName);
end;

procedure TJVCLConverter.TranslateInc(var AName: String);
begin
  if not ReduceConditions then
  begin
    if SameText(AName, 'jvcl.inc') then
      AName := 'qjvcl.inc'
    else
      AName := 'jvcl.inc'; // force lowercase
  end;
end;

procedure TJVCLConverter.TranslateResource(var AName: String);
begin
  inherited TranslateResource(AName); // replaces *.DFM -> .xfm
end;

procedure TJVCLConverter.BeforeSave(const Filename: string; Lines: TStrings);
begin
  inherited BeforeSave(Filename, Lines);
  Lines.Insert(0, '{******************************************************************************}');
  Lines.Insert(1, '{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}');
  Lines.Insert(2, '{*           Manual modifications will be lost on next release.               *}');
  Lines.Insert(3, '{******************************************************************************}');
  Lines.Insert(4, '');
end;



{ TConverter }

constructor TConverter.Create(const AJVCLDir: string);
begin
  inherited Create;
  FJVCLDir := AJVCLDir;
  FPackageModels := TPackageModelList.Create(FJVCLDir + ParsePath('/devtools/bin/pgEdit.xml'));
  FModel := FPackageModels.FindModel('JVCL');
  if FModel = nil then
    Fail;
  ExpandPackageTargetsObj := FModel.ExpandTargets;

  FCvt := TJVCLConverter.Create(ExtractFilePath(ParamStr(0)) + 'VclClxData');
end;

destructor TConverter.Destroy;
begin
  FModel := nil;
  FPackageModels.Free;
  FCvt.Free;
  inherited Destroy;
end;

function TConverter.CreateClxFiles: Integer;
var
  FileList, TargetList: TStringList;
  TargetIndex: Integer;
  Target: TModelTarget;
  Packages: TPackageGroup;
  Pkg: TBpgPackageTarget;
  PkgIndex, i: Integer;
  Filename: string;
begin
  FileList := TStringList.Create;
  TargetList := TStringList.Create;
  try
    FileList.Sorted := True;
    FileList.Duplicates := dupIgnore;

    TargetList.Add('allclx');
    FModel.ExpandTargets(TargetList);

   // get JCLX compatble all files
    for TargetIndex := 0 to TargetList.Count - 1 do
    begin
      Target := FModel.FindTarget(TargetList[TargetIndex]);
      Packages := TPackageGroup.Create(JVCLDir + ParsePath('/packages/') + Target.Name + ' Packages.bpg',
        JVCLDir + ParsePath('/packages/xml'), Target.Name);
      try
        for PkgIndex := 0 to Packages.Count - 1 do
        begin
          Pkg := Packages.Packages[PkgIndex];
          for i := 0 to Pkg.ContainCount - 1 do
          begin
            Filename := FollowRelativeFilename(JVCLDir + ParsePath('/packages/xml'), ParsePath(Pkg.Contains[i].Name));
            if not SameFileName(GetDestFilename(Filename), Filename) and
               not IgnoredFile(Filename) then
            begin
              FileList.Add(Filename); // .pas file
              {Filename := ChangeFileExt(Filename, '.dfm');
              if FileExists(Filename) then
                FileList.Add(Filename); // .dfm file}
            end;
          end;
        end;
      finally
        Packages.Free;
      end;
    end;
    Result := FileList.Count;

   // parse the files
    for i := 0 to FileList.Count - 1 do
    begin
      if Assigned(FOnProgress) then
        FOnProgress(Self, ExtractFileName(ExtractFilePath(Filename)) + ExtractFileName(Filename),
          i, FileList.Count);
      Filename := FileList[i];
      if (ExtractFileExt(Filename) = '.dfm') then
      begin
        if not FileExists(ChangeFileExt(GetDestFilename(Filename), '.xfm')) then
          Cvt.ParseDfmFile(Filename);
      end
      else
      begin
        Cvt.OutDirectory := ExtractFileDir(GetDestFilename(Filename));
        Cvt.ParsePasFile(Filename);
      end;
    end;
    if Assigned(FOnProgress) then
      FOnProgress(Self, 'Finished.', FileList.Count, FileList.Count);
  finally
    TargetList.Free;
    FileList.Free;
  end;
end;

function TConverter.GetDestFilename(const Filename: string): string;
begin
  Result := FModel.ReplacePath(Filename);
end;

function TConverter.IgnoredFile(const Filename: string): Boolean;
var
  fn: string;
  Index: Integer;
begin
  fn := ExtractFileName(ExtractFileDir(Filename));
  if fn = 'qdesign' then
    fn := 'qdesign';
  Result := (fn <> '') and
    (SameText(fn, 'qrun') or SameText(fn, 'qdesign') or SameText(fn, 'qcommon'));
  if not Result then
    Result := Cvt.IgnoreUnits.Find(ChangeFileExt(ExtractFileName(Filename), ''), Index);
end;

initialization
  BplNameToGenericNameHook := JvclBplNameToGenericNameHook;

end.
