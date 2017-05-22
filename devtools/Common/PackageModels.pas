{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: PackageModels.pas, released on 2004-05-19.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit PackageModels;

interface

uses
  SysUtils, Classes, Contnrs,
  JvSimpleXml;

type
  TPackageModelList = class;
  TPackageModel = class;

  TModelTarget = class(TObject)
  private
    FOwner: TPackageModel;
    FName: string;
    FPersonalName: string;
    FPersonalDir: string;
    FPathDelimiter: string;
    FIsClx: Boolean;
    FIsBDS: Boolean;
    FIsDotNet: Boolean;
    FDefines: TStringList;
  public
    constructor Create(AOwner: TPackageModel; XmlNode: TJvSimpleXMLElem);
    destructor Destroy; override;

    property Name: string read FName;
    property PersonalName: string read FPersonalName;
    property PersonalDir: string read FPersonalDir;
    property PathDelimiter: string read FPathDelimiter;
    property Defines: TStringList read FDefines;
    property IsClx: Boolean read FIsClx;
    property IsBDS: Boolean read FIsBDS;
    property IsDotNet: Boolean read FIsDotNet;

    property Owner: TPackageModel read FOwner;
  end;

  TModelAlias = class(TObject)
  private
    FOwner: TPackageModel;
    FName: string;
    FTargetNames: TStringList;
    function GetValue: string;
    function GetTargetCount: Integer;
    function GetTargets(Index: Integer): TModelTarget;
  public
    constructor Create(AOwner: TPackageModel; XmlNode: TJvSimpleXMLElem);
    destructor Destroy; override;

    function IsTargetIn(const Name: string): Boolean;

    property Name: string read FName;
    property TargetNames: TStringList read FTargetNames;
    property TargetCount: Integer read GetTargetCount;
    property Targets[Index: Integer]: TModelTarget read GetTargets; default;
    property Value: string read GetValue; // comma seperated list

    property Owner: TPackageModel read FOwner;
  end;

  TClxReplacement = class(TObject)
  private
    FOwner: TPackageModel;
    FOriginal: string;
    FReplacement: string;
  public
    constructor Create(AOwner: TPackageModel; XmlNode: TJvSimpleXMLElem);

    property Original: string read FOriginal;
    property Replacement: string read FReplacement;

    property Owner: TPackageModel read FOwner;
  end;

  TPackageModel = class(TObject)
  private
    FOwner: TPackageModelList;
    FName: string;
    FPrefix: string;
    FFormat: string;
    FClxPrefix: string;
    FDotNetPrefix: string;
    FPackagesDir: string;
    FIncFile: string;

    FTargets: TObjectList;
    FAliases: TObjectList;
    FClxReplacements: TObjectList;
    FIgnoredClxReplacements: TStringList;

    function GeClxReplacementCount: Integer;
    function GetAliasCount: Integer;
    function GetAliases(Index: Integer): TModelAlias;
    function GetClxReplacements(Index: Integer): TClxReplacement;
    function GetTargetCount: Integer;
    function GetTargets(Index: Integer): TModelTarget;
  public
    constructor Create(AOwner: TPackageModelList; XmlNode: TJvSimpleXMLElem); virtual;
    destructor Destroy; override;

    function FindTarget(const Name: string): TModelTarget;
    function FindAlias(const Name: string): TModelAlias;
    procedure ExpandTargets(Targets: TStrings);
    function ReplacePath(const Path: string): string;

    property Name: string read FName;
    property Prefix: string read FPrefix;
    property Format: string read FFormat;
    property ClxPrefix: string read FClxPrefix;
    property DotNetPrefix: string read FDotNetPrefix;
    property PackagesDir: string read FPackagesDir;
    property IncFile: string read FIncFile;

    property TargetCount: Integer read GetTargetCount;
    property Targets[Index: Integer]: TModelTarget read GetTargets; default;
    property AliasCount: Integer read GetAliasCount;
    property Aliases[Index: Integer]: TModelAlias read GetAliases;
    property ClxReplacementCount: Integer read GeClxReplacementCount;
    property ClxReplacements[Index: Integer]: TClxReplacement read GetClxReplacements;
    property IgnoredClxReplacements: TStringList read FIgnoredClxReplacements;

    property Owner: TPackageModelList read FOwner;
  end;

  TPackageModelClass = class of TPackageModel;

  TPackageModelList = class(TObject)
  private
    FModels: TObjectList;
    FFilename: string;
    function GetModelCount: Integer;
    function GetModels(Index: Integer): TPackageModel;
  protected
    procedure LoadFile; virtual;
    function GetPackageModelClass: TPackageModelClass; virtual;
  public
    constructor Create(const AFilename: string);
    destructor Destroy; override;

    function FindModel(const Name: string): TPackageModel; 

    property ModelCount: Integer read GetModelCount;
    property Models[Index: Integer]: TPackageModel read GetModels;

    property Filename: string read FFilename;
  end;

implementation

{ TPackageModelList }

constructor TPackageModelList.Create(const AFilename: string);
begin
  inherited Create;
  FModels := TObjectList.Create;
  FFilename := AFilename;
  LoadFile;
end;

destructor TPackageModelList.Destroy;
begin
  FModels.Free;
  inherited Destroy;
end;

function TPackageModelList.FindModel(const Name: string): TPackageModel;
var
  i: Integer;
begin
  for i := 0 to ModelCount - 1 do
    if CompareText(Models[i].Name, Name) = 0 then
    begin
      Result := Models[i];
      Exit;
    end;
  Result := nil;
end;

function TPackageModelList.GetModelCount: Integer;
begin
  Result := FModels.Count;
end;

function TPackageModelList.GetModels(Index: Integer): TPackageModel;
begin
  Result := TPackageModel(FModels[Index]);
end;

function TPackageModelList.GetPackageModelClass: TPackageModelClass;
begin
  Result := TPackageModel;
end;

procedure TPackageModelList.LoadFile;
var
  Xml: TJvSimpleXML;
  i: Integer;
  ModelsNode: TJvSimpleXMLElem;
begin
  FModels.Clear;

  Xml := TJvSimpleXML.Create(nil);
  try
    Xml.LoadFromFile(FFilename);
    ModelsNode := Xml.Root.Items.ItemNamed['models'];
    if ModelsNode = nil then
      Exit; // no valid file, ignore it

    for i := 0 to ModelsNode.Items.Count - 1 do
      if ModelsNode.Items[i].Properties.ItemNamed['name'] <> nil then
        FModels.Add(GetPackageModelClass.Create(Self, ModelsNode.Items[i]));
  finally
    Xml.Free;
  end;
end;

{ TPackageModel }

constructor TPackageModel.Create(AOwner: TPackageModelList;
  XmlNode: TJvSimpleXMLElem);
var
  Node: TJvSimpleXMLElem;
  i: Integer;
  AllAlias: TModelAlias;
begin
  inherited Create;
  FOwner := AOwner;
  FTargets := TObjectList.Create;
  FAliases := TObjectList.Create;
  FClxReplacements := TObjectList.Create;
  FIgnoredClxReplacements := TStringList.Create;
  FIgnoredClxReplacements.Sorted := True;
  FIgnoredClxReplacements.Duplicates := dupIgnore;

  FName := AnsiLowerCase(XmlNode.Properties.ItemNamed['name'].Value);
  FPrefix := XmlNode.Properties.Value('prefix');
  FFormat := XmlNode.Properties.Value('format');
  FClxPrefix := XmlNode.Properties.Value('clxPrefix');
  FDotNetPrefix := XmlNode.Properties.Value('dotnetPrefix');
  FPackagesDir := XmlNode.Properties.Value('packages');
  FIncFile := XmlNode.Properties.Value('incfile');

  Node := XmlNode.Items.ItemNamed['targets'];
  if Node <> nil then
    for i := 0 to Node.Items.Count - 1 do
      if Node.Items[i].Properties.ItemNamed['name'] <> nil then
        FTargets.Add(TModelTarget.Create(Self, Node.Items[i]));

  Node := XmlNode.Items.ItemNamed['aliases'];
  if Node <> nil then
    for i := 0 to Node.Items.Count - 1 do
      if Node.Items[i].Properties.ItemNamed['name'] <> nil then
        FAliases.Add(TModelAlias.Create(Self, Node.Items[i]));

  Node := XmlNode.Items.ItemNamed['ClxReplacements'];
  if Node <> nil then
    for i := 0 to Node.Items.Count - 1 do
    begin
      if CompareText(Node.Items[i].Name, 'replacement') = 0 then
      begin
        if Node.Items[i].Properties.ItemNamed['original'] <> nil then
          FClxReplacements.Add(TClxReplacement.Create(Self, Node.Items[i]));
      end
      else
      if CompareText(Node.Items[i].Name, 'ignoredFile') = 0 then
        FIgnoredClxReplacements.Add(Node.Items[i].Properties.Value('filename'));
    end;

  if FindTarget('all') = nil then
  begin
    AllAlias := TModelAlias.Create(Self, nil);
    FAliases.Add(AllAlias);
    AllAlias.FName := 'all';
    for i := 0 to TargetCount - 1 do
      AllAlias.FTargetNames.Add(Targets[i].Name);
  end;
end;

destructor TPackageModel.Destroy;
begin
  FTargets.Free;
  FAliases.Free;
  FClxReplacements.Free;
  FIgnoredClxReplacements.Free;
  inherited Destroy;
end;

procedure TPackageModel.ExpandTargets(Targets: TStrings);
var
  i: Integer;
  List: TStrings;
  Alias: TModelAlias;
  currentTarget: string;
begin
  List := TStringList.Create;
  try
    for i := 0 to Targets.Count - 1 do
    begin
      currentTarget := Targets[i];
      Alias := FindAlias(currentTarget);
      if Alias <> nil then
      begin
        List.AddStrings(Alias.TargetNames);
      end
      else
      begin
        List.Add(currentTarget);

        // (obones): Need to find a way to inform the caller that we don't
        // know the target it asked for. Using WriteLn is not a good idea
        // as the caller may be a GUI application.
        //if not Assigned(FindTarget(currentTarget)) then
          //WriteLn('Unknown target: ' + currentTarget);
      end;
    end;
    Targets.Assign(List);
  finally
    List.Free;
  end;
end;

function TPackageModel.FindAlias(const Name: string): TModelAlias;
var
  i: Integer;
begin
  for i := 0 to AliasCount - 1 do
    if CompareText(Aliases[i].Name, Name) = 0 then
    begin
      Result := Aliases[i];
      Exit;
    end;
  Result := nil;
end;

function TPackageModel.FindTarget(const Name: string): TModelTarget;
var
  i: Integer;
begin
  for i := 0 to TargetCount - 1 do
    if (CompareText(Targets[i].Name, Name) = 0) or
       ((Targets[i].PersonalName <> '') and (CompareText(Targets[i].PersonalName, Name) = 0)) then
    begin
      Result := Targets[i];
      Exit;
    end;
  Result := nil;
end;

function TPackageModel.GeClxReplacementCount: Integer;
begin
  Result := FClxReplacements.Count;
end;

function TPackageModel.GetAliasCount: Integer;
begin
  Result := FAliases.Count;
end;

function TPackageModel.GetAliases(Index: Integer): TModelAlias;
begin
  Result := TModelAlias(FAliases[Index]);
end;

function TPackageModel.GetClxReplacements(Index: Integer): TClxReplacement;
begin
  Result := TClxReplacement(FClxReplacements[Index]);
end;

function TPackageModel.GetTargetCount: Integer;
begin
  Result := FTargets.Count;
end;

function TPackageModel.GetTargets(Index: Integer): TModelTarget;
begin
  Result := TModelTarget(FTargets[Index]);
end;

function TPackageModel.ReplacePath(const Path: string): string;
var
  i: Integer;
  Ps: Integer;
  CmpPath: string;
begin
  if Path <> '' then
  begin
    if not IgnoredClxReplacements.Find(ExtractFileName(Path), i) then
    begin
      {$IFDEF MSWINDOWS}
      CmpPath := LowerCase(Path);
      {$ENDIF MSWINDOWS}
      {$IFDEF LINUX}
      CmpPath := Path;
      {$ENDIF LINUX}
      for i := 0 to ClxReplacementCount - 1 do
      begin
        Ps := Pos(ClxReplacements[i].Original, CmpPath);
        if Ps > 0 then
        begin
          Result := Copy(Path, 1, Ps - 1) +
                    ClxReplacements[i].Replacement +
                    Copy(Path, Ps + Length(ClxReplacements[i].Original), MaxInt);
          Exit;
        end;
      end;
    end;
  end;
  Result := Path;
end;

{ TModelTarget }

constructor TModelTarget.Create(AOwner: TPackageModel; XmlNode: TJvSimpleXMLElem);
begin
  inherited Create;
  FOwner := AOwner;
  FName := XmlNode.Properties.ItemNamed['name'].Value;
  FPersonalName := XmlNode.Properties.Value('pname');
  FPersonalDir := XmlNode.Properties.Value('pdir');
  FPathDelimiter := XmlNode.Properties.Value('pathsep', '\');
  FIsClx := XmlNode.Properties.BoolValue('IsClx', False);
  FIsBDS := XmlNode.Properties.BoolValue('IsBDS', False);
  FIsDotNet := XmlNode.Properties.BoolValue('IsDotNet', False);

  FDefines := TStringList.Create;
  FDefines.Sorted := True;
  FDefines.Duplicates := dupIgnore;
  FDefines.CommaText := XmlNode.Properties.Value('defines');
end;

destructor TModelTarget.Destroy;
begin
  FDefines.Free;
  inherited Destroy;
end;

{ TModelAlias }

constructor TModelAlias.Create(AOwner: TPackageModel; XmlNode: TJvSimpleXMLElem);
begin
  inherited Create;
  FOwner := AOwner;
  FTargetNames := TStringList.Create;
  FTargetNames.Sorted := True;
  FTargetNames.Duplicates := dupIgnore;
  if Assigned(XmlNode) then
  begin
    FName := XmlNode.Properties.ItemNamed['name'].Value;
    FTargetNames.CommaText := XmlNode.Properties.Value('value');
  end;
end;

destructor TModelAlias.Destroy;
begin
  FTargetNames.Free;
end;

function TModelAlias.GetTargetCount: Integer;
begin
  Result := FTargetNames.Count;
end;

function TModelAlias.GetTargets(Index: Integer): TModelTarget;
begin
  Result := Owner.FindTarget(TargetNames[Index]);
end;

function TModelAlias.GetValue: string;
begin
  Result := FTargetNames.CommaText;
end;

function TModelAlias.IsTargetIn(const Name: string): Boolean;
var
  Index: Integer;
begin
  Result := FTargetNames.Find(Name, Index);
end;

{ TClxReplacement }

constructor TClxReplacement.Create(AOwner: TPackageModel; XmlNode: TJvSimpleXMLElem);
begin
  inherited Create;
  FOwner := AOwner;
  FOriginal := XmlNode.Properties.ItemNamed['original'].Value;
  FReplacement := XmlNode.Properties.Value('replacement');
  {$IFDEF MSWINDOWS}
  FOriginal := LowerCase(FOriginal);
  {$ENDIF MSWINDOWS}
end;

end.