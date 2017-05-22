{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: TargetInfo.pas, released on 2006-02-20.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2006 Florent Ouchet.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit TargetInfo;

{$I jvcl.inc}

interface

uses
  Classes,
  JvSimpleXML;

type
  TTargetPackage = class
  private
    FUnits: TStringList;
    function GetUnitCount: Integer;
    function GetUnit(Index: Integer): string;
    procedure SetUnit(Index: Integer; const Value: string);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure LoadFromXMLElem(const XMLElem: TJvSimpleXMLElem);
    procedure SaveToXMLElem(const XMLElem: TJvSimpleXMLElem);
    function AddUnit(const UnitName: string): Integer;
    procedure DeleteUnit(const Index: Integer);
    procedure RemoveUnit(const UnitName: string);
    function IndexOf(const UnitName: string): Integer;
    procedure Clear;
    property UnitCount: Integer read GetUnitCount;
    property Units[Index: Integer]: string read GetUnit write SetUnit; default;
  end;

  TTargetInfo = class
  private
    FIncludeDirs: TStrings;
    FDefines: TStrings;
    FPackages: TStringList;
    function GetPackageCount: Integer;
    function GetPackage(Index: Integer): TTargetPackage;
    procedure SetPackage(Index: Integer; const Value: TTargetPackage);
    function GetName(Index: Integer): string;
    procedure SetDefines(const Value: TStrings);
    procedure SetIncludeDirs(const Value: TStrings);
    procedure SetName(Index: Integer; const Value: string);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure LoadFromXMLElem(const XMLElem: TJvSimpleXMLElem);
    procedure SaveToXMLElem(const XMLElem: TJvSimpleXMLElem);
    function AddPackage(const PackageName: string): TTargetPackage;
    procedure DeletePackage(const Index: Integer);
    procedure RemovePackage(const Package: TTargetPackage);
    function IndexOf(const Package: TTargetPackage): Integer; overload;
    function IndexOf(const PackageName: string): Integer; overload;
    procedure Clear;
    property PackageCount: Integer read GetPackageCount;
    property Packages[Index: Integer]: TTargetPackage read GetPackage write SetPackage; default;
    property Names[Index: Integer]: string read GetName write SetName;
    property IncludeDirs: TStrings read FIncludeDirs write SetIncludeDirs;
    property Defines: TStrings read FDefines write SetDefines;
  end;

  TTargetsInfo = class
  private
    FInfos: TStringList;
    function GetInfo(Index: Integer): TTargetInfo;
    function GetTargetCount: Integer;
    procedure SetInfo(Index: Integer; const Value: TTargetInfo);
    function GetName(Index: Integer): string;
    procedure SetName(Index: Integer; const Value: string);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure LoadFromXMLElem(const XMLElem: TJvSimpleXMLElem);
    procedure SaveToXMLElem(const XMLElem: TJvSimpleXMLElem);
    function AddInfo(const TargetName: string): TTargetInfo;
    procedure DeleteInfo(const Index: Integer);
    procedure RemoveInfo(const Info: TTargetInfo);
    function IndexOf(const TargetName: string): Integer; overload;
    function IndexOf(const Info: TTargetInfo): Integer; overload;
    procedure Clear;
    property TargetCount: Integer read GetTargetCount;
    property Infos[Index: Integer]: TTargetInfo read GetInfo write SetInfo; default;
    property Names[Index: Integer]: string read GetName write SetName;
  end;

implementation

uses
  SysUtils;

//=== TTargetsInfo ===========================================================

function TTargetsInfo.AddInfo(const TargetName: string): TTargetInfo;
begin
  Result := TTargetInfo.Create;
  try
    FInfos.AddObject(TargetName, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TTargetsInfo.Clear;
var
  Index: Integer;
begin
  for Index := FInfos.Count - 1 downto 0 do
    TTargetInfo(FInfos.Objects[Index]).Free;
  FInfos.Clear;
end;

constructor TTargetsInfo.Create;
begin
  inherited Create;
  FInfos := TStringList.Create;
  FInfos.Duplicates := dupError;
  FInfos.CaseSensitive := False;
end;

procedure TTargetsInfo.DeleteInfo(const Index: Integer);
begin
  FInfos.Objects[Index].Free;
  FInfos.Delete(Index);
end;

destructor TTargetsInfo.Destroy;
begin
  Clear;
  FInfos.Free;
  inherited Destroy;
end;

function TTargetsInfo.GetInfo(Index: Integer): TTargetInfo;
begin
  Result := TTargetInfo(FInfos.Objects[Index]);
end;

function TTargetsInfo.GetName(Index: Integer): string;
begin
  Result := FInfos.Strings[Index];
end;

function TTargetsInfo.GetTargetCount: Integer;
begin
  Result := FInfos.Count;
end;

function TTargetsInfo.IndexOf(const TargetName: string): Integer;
begin
  Result := FInfos.IndexOf(TargetName);
end;

function TTargetsInfo.IndexOf(const Info: TTargetInfo): Integer;
begin
  Result := FInfos.IndexOfObject(Info);
end;

procedure TTargetsInfo.LoadFromXMLElem(const XMLElem: TJvSimpleXMLElem);
var
  IndexTarget: Integer;
  TargetNode: TJvSimpleXMLElem;
  TargetInfo: TTargetInfo;
begin
  Clear;
  for IndexTarget := 0 to XMLElem.Items.Count - 1 do
  begin
    TargetNode := XMLElem.Items.Item[IndexTarget];
    TargetInfo := AddInfo(TargetNode.Properties.ItemNamed['Name'].Value);
    TargetInfo.LoadFromXMLElem(TargetNode);
  end;
end;

procedure TTargetsInfo.RemoveInfo(const Info: TTargetInfo);
begin
  DeleteInfo(IndexOf(Info));
end;

procedure TTargetsInfo.SaveToXMLElem(const XMLElem: TJvSimpleXMLElem);
var
  IndexTarget: Integer;
  TargetNode: TJvSimpleXMLElem;
begin
  for IndexTarget := 0 to FInfos.Count - 1 do
  begin
    TargetNode := XMLElem.Items.Add('TARGET');
    TargetNode.Properties.Add('NAME', FInfos.Strings[IndexTarget]);
    Infos[IndexTarget].SaveToXMLElem(TargetNode);
  end;
end;

procedure TTargetsInfo.SetInfo(Index: Integer; const Value: TTargetInfo);
begin
  FInfos.Objects[Index] := Value;
end;

procedure TTargetsInfo.SetName(Index: Integer; const Value: string);
begin
  FInfos.Strings[Index] := Value;
end;

//=== TTargetInfo ============================================================

function TTargetInfo.AddPackage(const PackageName: string): TTargetPackage;
begin
  Result := TTargetPackage.Create;
  try
    FPackages.AddObject(PackageName, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TTargetInfo.Clear;
var
  IndexPackage: Integer;
begin
  for IndexPackage := 0 to FPackages.Count - 1 do
    TTargetPackage(FPackages.Objects[IndexPackage]).Free;
  FPackages.Clear;
  FIncludeDirs.Clear;
  FDefines.Clear;
end;

constructor TTargetInfo.Create;
begin
  inherited Create;
  FPackages := TStringList.Create;
  FPackages.Duplicates := dupIgnore;
  FPackages.CaseSensitive := False;

  FIncludeDirs := TStringList.Create;
  TStringList(FIncludeDirs).Duplicates := dupIgnore;
  TStringList(FIncludeDirs).CaseSensitive := False;

  FDefines := TStringList.Create;
  TStringList(FDefines).Duplicates := dupIgnore;
  TStringList(FDefines).CaseSensitive := False;
end;

procedure TTargetInfo.DeletePackage(const Index: Integer);
begin
  FPackages.Objects[Index].Free;
  FPackages.Delete(Index);
end;

destructor TTargetInfo.Destroy;
begin
  FPackages.Free;
  FIncludeDirs.Free;
  FDefines.Free;
  inherited Destroy;
end;

function TTargetInfo.GetName(Index: Integer): string;
begin
  Result := FPackages.Strings[Index];
end;

function TTargetInfo.GetPackage(Index: Integer): TTargetPackage;
begin
  Result := TTargetPackage(FPackages.Objects[Index]);
end;

function TTargetInfo.GetPackageCount: Integer;
begin
  Result := FPackages.Count;
end;

function TTargetInfo.IndexOf(const PackageName: string): Integer;
begin
  Result := FPackages.IndexOf(PackageName);
end;

function TTargetInfo.IndexOf(const Package: TTargetPackage): Integer;
begin
  Result := FPackages.IndexOfObject(Package);
end;

procedure TTargetInfo.LoadFromXMLElem(const XMLElem: TJvSimpleXMLElem);
var
  Index: Integer;
  PackagesNode, PackageNode,
  DefinesNode, IncludesNode: TJvSimpleXMLElem;
begin
  Clear;
  PackagesNode := XMLElem.Items.ItemNamed['PACKAGES'];
  if Assigned(PackagesNode) then
    for Index := 0 to PackagesNode.Items.Count - 1 do
  begin
    PackageNode := PackagesNode.Items.Item[Index];
    AddPackage(PackageNode.Properties.ItemNamed['NAME'].Value).LoadFromXMLElem(PackageNode);
  end;

  DefinesNode := XMLElem.Items.ItemNamed['DEFINES'];
  if Assigned(DefinesNode) then
    for Index := 0 to DefinesNode.Items.Count - 1 do
      FDefines.Add(DefinesNode.Items.Item[Index].Properties.ItemNamed['NAME'].Value);

  IncludesNode := XMLElem.Items.ItemNamed['INCLUDES'];
  if Assigned(IncludesNode) then
    for Index := 0 to IncludesNode.Items.Count - 1 do
      FIncludeDirs.Add(IncludesNode.Items.Item[Index].Properties.ItemNamed['NAME'].Value);
end;

procedure TTargetInfo.RemovePackage(const Package: TTargetPackage);
begin
  DeletePackage(IndexOf(Package));
end;

procedure TTargetInfo.SaveToXMLElem(const XMLElem: TJvSimpleXMLElem);
var
  Index: Integer;
  PackagesNode, DefinesNode, IncludesNode, PackageNode: TJvSimpleXMLElem;
begin
  PackagesNode := XMLElem.Items.Add('PACKAGES');
  for Index := 0 to FPackages.Count - 1 do
  begin
    PackageNode := PackagesNode.Items.Add('PACKAGE');
    PackageNode.Properties.Add('NAME', Names[Index]);
    Packages[Index].SaveToXMLElem(PackageNode);
  end;

  DefinesNode := XMLElem.Items.Add('DEFINES');
  for Index := 0 to FDefines.Count - 1 do
    DefinesNode.Items.Add('DEFINE').Properties.Add('NAME', FDefines.Strings[Index]);

  IncludesNode := XMLElem.Items.Add('INCLUDES');
  for Index := 0 to FIncludeDirs.Count - 1 do
    IncludesNode.Items.Add('INCLUDE').Properties.Add('NAME', FIncludeDirs.Strings[Index]);
end;

procedure TTargetInfo.SetDefines(const Value: TStrings);
begin
  FDefines := Value;
end;

procedure TTargetInfo.SetIncludeDirs(const Value: TStrings);
begin
  FIncludeDirs := Value;
end;

procedure TTargetInfo.SetName(Index: Integer; const Value: string);
begin
  FPackages.Strings[Index] := Value;
end;

procedure TTargetInfo.SetPackage(Index: Integer; const Value: TTargetPackage);
begin
  FPackages.Objects[Index] := Value;
end;

//=== TTargetPackage =========================================================

function TTargetPackage.AddUnit(const UnitName: string): Integer;
begin
  Result := FUnits.IndexOf(UnitName);
  if Result = -1 then
    Result := FUnits.Add(UnitName);
end;

procedure TTargetPackage.Clear;
begin
  FUnits.Clear;
end;

constructor TTargetPackage.Create;
begin
  inherited Create;
  FUnits := TStringList.Create;
  FUnits.Duplicates := dupIgnore;
  FUnits.CaseSensitive := False;
end;

procedure TTargetPackage.DeleteUnit(const Index: Integer);
begin
  FUnits.Delete(Index);
end;

destructor TTargetPackage.Destroy;
begin
  FUnits.Free;
  inherited Destroy;
end;

function TTargetPackage.GetUnit(Index: Integer): string;
begin
  Result := FUnits.Strings[Index];
end;

function TTargetPackage.GetUnitCount: Integer;
begin
  Result := FUnits.Count;
end;

function TTargetPackage.IndexOf(const UnitName: string): Integer;
begin
  Result := FUnits.IndexOf(UnitName);
end;

procedure TTargetPackage.LoadFromXMLElem(const XMLElem: TJvSimpleXMLElem);
var
  Index: Integer;
begin
  Clear;

  for Index := 0 to XMLElem.Items.Count - 1 do
    FUnits.Add(XMLElem.Items.Item[Index].Properties.ItemNamed['NAME'].Value);
end;

procedure TTargetPackage.RemoveUnit(const UnitName: string);
var
  StrIndex: Integer;
begin
  StrIndex := FUnits.IndexOf(UnitName);
  if StrIndex >= 0 then
    FUnits.Delete(StrIndex);
end;

procedure TTargetPackage.SaveToXMLElem(const XMLElem: TJvSimpleXMLElem);
var
  Index: Integer;
begin
  for Index := 0 to FUnits.Count - 1 do
    XMLElem.Items.Add('UNIT').Properties.Add('NAME', FUnits.Strings[Index]);
end;

procedure TTargetPackage.SetUnit(Index: Integer; const Value: string);
begin
  FUnits.Strings[Index] := Value;
end;

end.
