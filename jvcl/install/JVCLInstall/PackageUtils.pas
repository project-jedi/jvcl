{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: PackageUtils.pas, released on 2004-03-29.

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

{$I jvcl.inc}

unit PackageUtils;

interface

uses
  SysUtils, Classes, Contnrs,
  JvSimpleXml,
  Utils, DelphiData, Intf, GenerateUtils, PackageInformation;

type
  TPackageTarget = class;
  TProjectGroup = class;

  TPackageGroupArray = array[{Personal:}Boolean, {Kind:}TPackageGroupKind] of TProjectGroup;

  /// <summary>
  /// TJVCLFrameworks contains all possible package lists for the target. If
  /// Items[x] is nil then there is no .bpg file for this target kind.
  /// </summary>
  TJVCLFrameworks = class(TObject)
  private
    FItems: TPackageGroupArray;
    FTargetConfig: ITargetConfig;
    function GetCount: Integer;
  public
    constructor Create(ATargetConfig: ITargetConfig);
    destructor Destroy; override;

    property Items: TPackageGroupArray read FItems write FItems;
    property Count: Integer read GetCount;
    property TargetConfig: ITargetConfig read FTargetConfig;
  end;


  /// <summary>
  /// TPackageTarget contains a .bpl target and the .xml file in the
  /// Info property. This class is used to specify if the package should be
  /// compiled and/or installed. But it does not perform these actions itself.
  /// </summary>
  TPackageTarget = class(TBpgPackageTarget)
  private
    FLockInstallChange: Integer;

    FJvDependencies: TStringList;  // Strings[]: "JvXxxx-"[D|R] | "JvQXxxx-"[D|R]
                                   // Objects[]: TRequiredPackage
    FJclDependencies: TStringList; // Strings[]: "JvXxxx-"[D|R] | "JvQXxxx-"[D|R]
                                   // Objects[]: TRequiredPackage
    FCompile: Boolean;
    FInstall: Boolean;

    procedure SetCompile(Value: Boolean);
    procedure SetInstall(const Value: Boolean);
    function GetJclDependenciesReqPkg(Index: Integer): TRequiredPackage;
    function GetJvDependenciesReqPkg(Index: Integer): TRequiredPackage;
    function GetOwner: TProjectGroup;
  protected
    procedure GetDependencies; override; // is called after alle package targets are created
  public
    constructor Create(AOwner: TPackageGroup; const ATargetName, ASourceName: string); override;
    destructor Destroy; override;

    function FindRuntimePackage: TPackageTarget;

    property JvDependencies: TStringList read FJvDependencies;
    property JvDependenciesReqPkg[Index: Integer]: TRequiredPackage read GetJvDependenciesReqPkg;
    property JclDependencies: TStringList read FJclDependencies;
    property JclDependenciesReqPkg[Index: Integer]: TRequiredPackage read GetJclDependenciesReqPkg;

    property Compile: Boolean read FCompile write SetCompile;
    property Install: Boolean read FInstall write SetInstall;

    property Owner: TProjectGroup read GetOwner;
  end;

  /// <summary>
  /// TProjectGroup contains the data from a .bpg (Borland Package Group) file.
  /// </summary>
  TProjectGroup = class(TPackageGroup)
  private
    FTargetConfig: ITargetConfig;
    FOnCompileChange: TNotifyEvent;

    function GetPackages(Index: Integer): TPackageTarget;
    function GetTarget: TCompileTarget;
  protected
    function GetIsVCLX: Boolean; override;
    procedure DoInstallChange; virtual;
    function GetPackageTargetClass: TBpgPackageTargetClass; override;
  public
    constructor Create(ATargetConfig: ITargetConfig; const AFilename: string);

    function GetBplNameOf(Package: TRequiredPackage): string; override;
    function FindPackageByXmlName(const XmlName: string): TPackageTarget;
      { FindPackageByXmlName returns the TPackageTarget object that contains
        the specified .xml file. }

    property Packages[Index: Integer]: TPackageTarget read GetPackages; default;
    property TargetConfig: ITargetConfig read FTargetConfig;
    property Target: TCompileTarget read GetTarget;
    property OnCompileChange: TNotifyEvent read FOnCompileChange;
  end;

implementation

function BplNameToGenericName(const BplName: string): string;
begin
   // obtain package name used in the xml file
  Result := ChangeFileExt(BplName, '');
  Delete(Result, Length(Result) - 2, 1);
  Result[Length(Result) - 1] := '-';
  if Result[3] = 'Q' then
    Delete(Result, 3, 1);
end;

{ TJVCLFrameworks }

constructor TJVCLFrameworks.Create(ATargetConfig: ITargetConfig);
var
  Kind: TPackageGroupKind;
begin
  inherited Create;
  FTargetConfig := ATargetConfig;
  for Kind := pkFirst to pkLast do
  begin
    if FileExists(TargetConfig.GetBpgFilename(False, Kind)) then
      FItems[False, Kind] := TProjectGroup.Create(TargetConfig, TargetConfig.GetBpgFilename(False, Kind));
    if FileExists(TargetConfig.GetBpgFilename(True, Kind)) then
      FItems[True, Kind] := TProjectGroup.Create(TargetConfig, TargetConfig.GetBpgFilename(True, Kind));
  end;
end;

destructor TJVCLFrameworks.Destroy;
var
  Kind: TPackageGroupKind;
begin
  for Kind := pkFirst to pkLast do
  begin
    FItems[False, Kind].Free;
    FItems[True, Kind].Free;
  end;
  inherited Destroy;
end;

function TJVCLFrameworks.GetCount: Integer;
begin
  Result := Length(FItems);
end;

{ TProjectGroup }

constructor TProjectGroup.Create(ATargetConfig: ITargetConfig; const AFilename: string);
begin
  FTargetConfig := ATargetConfig;
  inherited Create(AFilename, ATargetConfig.JVCLPackagesXmlDir, ATargetConfig.TargetSymbol);
end;

procedure TProjectGroup.DoInstallChange;
begin
  if Assigned(FOnCompileChange) then
    FOnCompileChange(Self);
end;

function TProjectGroup.FindPackageByXmlName(const XmlName: string): TPackageTarget;
begin
  Result := TPackageTarget(inherited FindPackageByXmlName(XmlName));
end;

function TProjectGroup.GetBplNameOf(Package: TRequiredPackage): string;
begin
  if StartsWith(Package.Name, 'Jv', True) then
    Result := inherited GetBplNameOf(Package)
  else
    Result := Package.Name;
end;

function TProjectGroup.GetIsVCLX: Boolean;
begin
  Result := Pos('clx', LowerCase(BpgName)) > 0; 
end;

function TProjectGroup.GetPackages(Index: Integer): TPackageTarget;
begin
  Result := TPackageTarget(inherited Packages[Index]);
end;

function TProjectGroup.GetPackageTargetClass: TBpgPackageTargetClass;
begin
  Result := TPackageTarget;
end;

function TProjectGroup.GetTarget: TCompileTarget;
begin
  Result := TargetConfig.Target;
end;

function SortProc_PackageTarget(Item1, Item2: Pointer): Integer;
var
  p1, p2: TPackageTarget;
begin
  p1 := Item1;
  p2 := Item2;
  Result := CompareText(p1.Info.DisplayName, p2.Info.DisplayName);
  if Result = 0 then
  begin
    if p1.Info.IsDesign and not p2.Info.IsDesign then
      Result := 1
    else if not p1.Info.IsDesign and p2.Info.IsDesign then
      Result := -1;
  end;
end;

{ TPackageTarget }

constructor TPackageTarget.Create(AOwner: TPackageGroup; const ATargetName,
  ASourceName: string);
begin
  inherited Create(AOwner, ATargetName, ASourceName);
  FJvDependencies := TStringList.Create;
  FJvDependencies.Sorted := True;
  FJclDependencies := TStringList.Create;
  FJclDependencies.Sorted := True;
  FCompile := True;
end;

destructor TPackageTarget.Destroy;
begin
  FJvDependencies.Free;
  FJclDependencies.Free;
  inherited Destroy;
end;

/// <summary>
/// GetDependencies obtains the JVCL (JvXxx) and JCL ([D|C]JCLxx) dependencies
/// from the PackageInfo data. Only the JvXxx packages that are for this target
/// are added to the JvDependencies list. And only the [D|C]JCLxxx packages are
/// added to the JclDependencies list that are for this target. All items in
/// JvDependencies are physical files and are a valid JVCL target. All items in
/// JclDependencies must not be physical files.
/// </summary>
function TPackageTarget.FindRuntimePackage: TPackageTarget;
begin
  Result := TPackageTarget(inherited FindRuntimePackage);
end;

procedure TPackageTarget.GetDependencies;
var
  i: Integer;
begin
  FJvDependencies.Clear;
  for i := 0 to Info.RequireCount - 1 do
  begin
    // JVCL dependencies
    if StartsWith(Info.Requires[i].Name, 'Jv', True) then // do not localize
    begin
      if FileExists(Info.XmlDir + '\' + Info.Requires[i].Name + '.xml') and // do not localize
         (Owner.FindPackagebyXmlName(Info.Requires[i].Name) <> nil) and
         Info.Requires[i].IsRequiredByTarget(Owner.TargetSymbol) then
      begin
        FJvDependencies.AddObject(Info.Requires[i].Name, Info.Requires[i]);
      end;
    end
    else
    // is it a JCL dependency
    if StartsWith(Info.Requires[i].Name, 'DJcl', True) or // do not localize
       StartsWith(Info.Requires[i].Name, 'CJcl', True) then // do not localize
    begin
      if Info.Requires[i].IsRequiredByTarget(Owner.TargetSymbol) then
        FJclDependencies.AddObject(Info.Requires[i].Name, Info.Requires[i]);
    end;
  end;
end;

function TPackageTarget.GetJclDependenciesReqPkg(Index: Integer): TRequiredPackage;
begin
  Result := TRequiredPackage(JclDependencies.Objects[Index]);
end;

function TPackageTarget.GetJvDependenciesReqPkg(Index: Integer): TRequiredPackage;
begin
  Result := TRequiredPackage(JvDependencies.Objects[Index]);
end;

function TPackageTarget.GetOwner: TProjectGroup;
begin
  Result := TProjectGroup(inherited Owner);
end;

procedure TPackageTarget.SetCompile(Value: Boolean);
var
  i: Integer;
  Pkg: TPackageTarget;
begin
  if Value <> FCompile then
  begin
    FCompile := Value;
    if not FCompile then
      FInstall := False;

    Inc(FLockInstallChange);
    try
      if FCompile then
      begin
       // activate packages on which this package depend on
        for i := 0 to JvDependencies.Count - 1 do
        begin
          Pkg := Owner.FindPackagebyXmlName(JvDependencies[i]);
          if Pkg <> nil then
            Pkg.SetCompile(True);
        end;
      end
      else
      begin
       // deactivate all packages which depend on this package
        for i := 0 to Owner.Count - 1 do
        begin
          Pkg := Owner.Packages[i];
          if Pkg <> Self then
          begin
            if Pkg.JvDependencies.IndexOf(Info.Name) <> -1 then
              Pkg.Compile := False;
          end;
        end;
      end;
    finally
      Dec(FLockInstallChange);
    end;
    if FLockInstallChange = 0 then
      Owner.DoInstallChange;
  end;
end;

procedure TPackageTarget.SetInstall(const Value: Boolean);
begin
  if Info.IsDesign then
    FInstall := Value
  else
    FInstall := False; // runtime packages are not installable.
  if Value then
    Compile := True;
end;

initialization
  BplNameToGenericNameHook := BplNameToGenericName;
  ExpandPackageTargets := ExpandTargets;

end.
