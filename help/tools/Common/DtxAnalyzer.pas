unit DtxAnalyzer;

interface

uses
  Windows, DtxParser, JVCLHelpUtils, Classes, ContNrs, DtxDiagnoser;

type
  TGroupInfo = class;
  TDtxPackage = class;
  TGroupInfoProc = procedure(AGroupInfo: TGroupInfo) of object;

  TGroupInfos = class(TObject)
  private
    FList: TObjectList;
    FOwner: TGroupInfo;
  protected
    function GetCount: Integer;
    function GetItems(I: Integer): TGroupInfo;

    property Owner: TGroupInfo read FOwner;
  public
    constructor Create(AOwner: TGroupInfo);
    destructor Destroy; override;

    procedure FillGroupIDs(Strings: TStrings);
    function Add(const AInfoString: string): TGroupInfo;
    function Locate(AGroupID: string): TGroupInfo;

    procedure Iterate(Proc: TGroupInfoProc; const GroupsFirst: Boolean);

    property Count: Integer read GetCount;
    property Items[I: Integer]: TGroupInfo read GetItems; default;
  end;

  TComponentInfo = class(TObject)
  private
    FName: string;
    FImage: string;
    FSummary: TSymbolList;
    FFlags: TComponentInfoFlags;
    FPackage: TDtxPackage;
    procedure SetSummary(ASymbolList: TSymbolList);
    procedure SetPackage(APackage: TDtxPackage);
  protected
    function GetValid: Boolean;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    property Name: string read FName;
    property Image: string read FImage write FImage;
    property Summary: TSymbolList read FSummary write SetSummary;
    property Flags: TComponentInfoFlags read FFlags write FFlags;
    property Package: TDtxPackage read FPackage write SetPackage;

    property Valid: Boolean read GetValid;
  end;

  TComponentInfos = class(TObject)
  private
    FList: TStrings;
  protected
    function GetCount: Integer;
    function GetItems(I: Integer): TComponentInfo;
    function GetItemByName(Name: string): TComponentInfo;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AName: string): TComponentInfo;
    procedure Delete(Index: Integer);
    function IndexOf(const AName: string): Integer;
    procedure Remove(Comp: TComponentInfo);
    procedure RemoveInvalid;

    property Count: Integer read GetCount;
    property Items[I: Integer]: TComponentInfo read GetItems; default;
    property ItemByName[Name: string]: TComponentInfo read GetItemByName;
  end;

  TGroupInfo = class(TObject)
  private
    FGroupID: string;
    FGroupTitle: string;
    FGroupText: string;
    FParent: TGroupInfo;
    FSubGroups: TGroupInfos;
    FComponents: TStrings;
  protected
    function CountComp(AComponentList: TComponentInfos): Integer;
    function CountClass(AComponentList: TComponentInfos): Integer;
    function CountRoutines(AComponentList: TComponentInfos): Integer;
  public
    constructor Create(const AParent: TGroupInfo; const AInfoString: string);
    destructor Destroy; override;

    //    procedure AddComponent(AComp: TComponentInfo);
    function ParentGroupID(const WantFuncRef: Boolean = False): string;
    procedure FillGroupIDs(Strings: TStrings);
    procedure Iterate(Proc: TGroupInfoProc; const GroupsFirst: Boolean);
    procedure Sort;

    property Components: TStrings read FComponents;
    property GroupID: string read FGroupID;
    property GroupTitle: string read FGroupTitle;
    property GroupText: string read FGroupText;
    property Parent: TGroupInfo read FParent;
    property SubGroups: TGroupInfos read FSubGroups;
  end;

  TGroupsTreeParser = class
  private
    FLines: TStrings;
    FCurList: TStack;
    FGroupInfos: TGroupInfos;
    FInternalGroupInfos: TGroupInfos;
    function GetGroupInfos: TGroupInfos;
  protected
    function ParseAsTreeItem(const Item: Integer): Boolean;
    function ParseLines: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Parse(const AFileName: string): Boolean;
    procedure FillGroupIDs(Strings: TStrings);
    property GroupInfos: TGroupInfos read GetGroupInfos write FGroupInfos;
  end;

  TDtxInfo = class
  private
    FName: string;
    FPackage: TDtxPackage;
    FPercComplete: Integer;
    function GetUnitName: string;
    function GetPercComplete: Integer;
    function GetPackage: TDtxPackage;
  protected
    procedure SetPercComplete(const Value: Integer);
    procedure SetPackage(Value: TDtxPackage);
  public
    constructor Create(AName: string);
    destructor Destroy; override;
    property UnitName: string read GetUnitName;
    property PercComplete: Integer read GetPercComplete;
    property Package: TDtxPackage read GetPackage;
  end;

  TDtxInfos = class(TList)
  private
    function GetItems(Index: Integer): TDtxInfo;
  protected
    function AddUnit(Name: string): TDtxInfo;
  public
    destructor Destroy; override;
    procedure RemoveUnit(Index: Integer);
//    function CountCompleted: Integer;
    function IndexOfUnit(Name: string): Integer;
    procedure Sort;
    property Items[Index: Integer]: TDtxInfo read GetItems; default;
  end;

  TDtxPackages = class(TList)
  private
    function GetItems(Index: Integer): TDtxPackage;
  protected
    function AddPackage(Name: string): TDtxPackage;
  public
    destructor Destroy; override;
    function CountEmpty: Integer;
    function IndexOfPackage(PackageName: string): Integer;
    procedure RemoveEmptyPackages;
    procedure RemoveInvalidPackages;
    procedure Sort;
    procedure SaveToDtx(ADtxWriter: TDtxWriter);
    property Items[Index: Integer]: TDtxPackage read GetItems; default;
  end;

  TDtxPackage = class
  private
    FDtxList: TList;
    FComponentList: TList;
    FName: string;
    FPackageText: string;
    function GetComponentCount: Integer;
    function GetComponentInfos(Index: Integer): TComponentInfo;
    function GetDtxCount: Integer;
    function GetDtxInfos(Index: Integer): TDtxInfo;
    function GetPackageName: string;
  protected
    procedure AddDtx(DtxInfo: TDtxInfo);
    procedure RemoveDtx(DtxInfo: TDtxInfo);
    procedure AddComponent(AComponentInfo: TComponentInfo);
    procedure RemoveComponent(AComponentInfo: TComponentInfo);
  public
    constructor Create(AName: string);
    destructor Destroy; override;
    procedure SaveToDtx(ADtxWriter: TDtxWriter);
    property DtxCount: Integer read GetDtxCount;
    property DtxInfos[Index: Integer]: TDtxInfo read GetDtxInfos;
    property ComponentCount: Integer read GetComponentCount;
    property ComponentInfos[Index: Integer]: TComponentInfo read GetComponentInfos;
    property PackageName: string read GetPackageName;
    property PackageText: string read FPackageText write FPackageText;
  end;

  TDtxAnalyzer = class(TTask)
  private
    FGroupInfos: TGroupInfos;
    FComponentInfos: TComponentInfos;
    FPackages: TDtxPackages;
    FDtxInfos: TDtxInfos;
//    FJVCLGroupsTreeFileName: string;
    FSourceDtxDir: string;
    FDestDtxDir: string;
    FSourcePasDir: string;
    FRegisteredComponents: TRegisteredComponents;
    FGenerateForBuild: Boolean;
    FGenerateFormattedDtxFiles: Boolean;
    FOriginalDtxDir: string;
    FCopySeeAlsoFromOriginal: Boolean;
    FCopyHeaderFromOriginal: Boolean;
    FUpdateOriginalEmpties: Boolean;
    FPackageListFileName: string;
    FDiagnoser: TDtxDiagnoser;
    FSpecificDtxFiles: TStringList;
    procedure RemoveUnknownComponents(AGroupInfo: TGroupInfo);
    procedure MergeComps(AGroupInfo: TGroupInfo);
    function GetSpecificDtxFiles: TStrings;
    procedure SetSpecificDtxFiles(Value: TStrings);
  protected
    procedure ParseGroupString(AComp: TComponentInfo; GroupString: string);

//    function ParseGroupsTree: Boolean;
    function CleanupComponents: Boolean;
    function CleanupDtx: Boolean;
    function CleanupPackages: Boolean;
    function MergeGroupCompLists: Boolean;
    function CheckFileExists(AFiles: TStrings): Boolean;

//    function Init: Boolean;
    procedure CollectFiles(AFiles: TStrings);
    function ProcessFiles: Boolean;
    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
    function Final: Boolean;
  public
    constructor Create(ATaskManager: ITaskManager); override;
    destructor Destroy; override;

    function AddDtxFile(ADtxList: TDtxList; const APercComplete: Integer): Boolean;
    function AddComponent(ADtxInfo: TDtxInfo; ADtxTopic: TDtxTopic): Boolean;

//    property JVCLGroupsTreeFileName: string read FJVCLGroupsTreeFileName write FJVCLGroupsTreeFileName;
    property PackageListFileName: string read FPackageListFileName write FPackageListFileName;

    property GroupInfos: TGroupInfos read FGroupInfos;
    property ComponentInfos: TComponentInfos read FComponentInfos;
    property Packages: TDtxPackages read FPackages;
    property DtxInfos: TDtxInfos read FDtxInfos;

    property OriginalDtxDir: string read FOriginalDtxDir write FOriginalDtxDir;
    property SourceDtxDir: string read FSourceDtxDir write FSourceDtxDir;
    property SourcePasDir: string read FSourcePasDir write FSourcePasDir;
    property DestDtxDir: string read FDestDtxDir write FDestDtxDir;
    property RegisteredComponents: TRegisteredComponents read FRegisteredComponents write FRegisteredComponents;
    property GenerateFormattedDtxFiles: Boolean read FGenerateFormattedDtxFiles write FGenerateFormattedDtxFiles;
    property GenerateForBuild: Boolean read FGenerateForBuild write FGenerateForBuild;
    property CopyHeaderFromOriginal: Boolean read FCopyHeaderFromOriginal write FCopyHeaderFromOriginal;
    property UpdateOriginalEmpties: Boolean read FUpdateOriginalEmpties write FUpdateOriginalEmpties;
    property CopySeeAlsoFromOriginal: Boolean read FCopySeeAlsoFromOriginal write FCopySeeAlsoFromOriginal;
    property Diagnoser: TDtxDiagnoser read FDiagnoser write FDiagnoser;
    property SpecificDtxFiles: TStrings read GetSpecificDtxFiles write SetSpecificDtxFiles;
  end;

implementation

uses
  DtxPasCombiner,
  SysUtils;

const
  cifSymbolType = [cifComponent, cifClass, cifRoutine];

  //=== Local procedures =======================================================

procedure ParseGroupInfo(AInfoString: string; out FGroupID, FGroupTitle, FGroupText: string);
var
  I: Integer;
begin
  I := Pos('=', AInfoString);
  if I = 0 then
    raise Exception.Create('invalid group string: no title');
  FGroupID := Trim(Copy(AInfoString, 1, I - 1));
  Delete(AInfoString, 1, I);
  I := Pos('=', AInfoString);
  if I = 0 then
    raise Exception.Create('invalid group string: no description');
  FGroupTitle := Trim(Copy(AInfoString, 1, I - 1));
  Delete(AInfoString, 1, I);
  FGroupText := TrimLeft(AInfoString);
  I := Length(FGroupTitle);
  while (I > 0) do
  begin
    if (FGroupTitle[I] = ',') and ((I = 1) or (FGroupTitle[I - 1] <> '\')) then
      Insert('\', FGroupTitle, I);
    Dec(I);
  end;
  I := Length(FGroupText);
  while I > 0 do
  begin
    if (FGroupText[I] = '$') and ((I = 1) or (FGroupText[I - 1] <> '\')) then
    begin
      Delete(FGroupText, I, 1);
      Insert(#13#10, FGroupText, I);
    end
    else
      if FGroupText[I] = '$' then
      Delete(FGroupText, I - 1, 1);
    Dec(I);
  end;
end;

//=== { TComponentInfo } =====================================================

constructor TComponentInfo.Create(const AName: string);
begin
  inherited Create;
  FName := Trim(AName);
  FImage := '';
  FSummary := TSymbolList.Create;
end;

destructor TComponentInfo.Destroy;
begin
  Package := nil;
  inherited Destroy;
end;

function TComponentInfo.GetValid: Boolean;
begin
  Result :=
    (cifAssignedToGroup in Flags) and not Summary.IsEmpty and (
    (
    ((cifComponent in Flags) and not IsNullStr(Image)) or
    (cifClass in Flags) or
    (cifRoutine in Flags)
    )
    );
end;

procedure TComponentInfo.SetPackage(APackage: TDtxPackage);
begin
  if FPackage <> nil then
    FPackage.RemoveComponent(Self);
  FPackage := APackage;
  if FPackage <> nil then
    FPackage.AddComponent(Self);
end;

procedure TComponentInfo.SetSummary(ASymbolList: TSymbolList);
var
  I: Integer;
begin
  FSummary.Clear;
  for I := 0 to ASymbolList.Count - 1 do
    FSummary.Add(ASymbolList[i].ConstructCopy);
end;

//=== { TComponentInfos } ====================================================

constructor TComponentInfos.Create;
begin
  inherited Create;
  FList := TOwnedStringList.Create;
end;

destructor TComponentInfos.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TComponentInfos.Add(const AName: string): TComponentInfo;
begin
  if FList.IndexOf(AName) < 0 then
    FList.AddObject(AName, TComponentInfo.Create(AName));
  Result := TComponentInfo(FList.Objects[FList.IndexOf(AName)])
end;

procedure TComponentInfos.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

function TComponentInfos.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TComponentInfos.GetItemByName(Name: string): TComponentInfo;
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I > -1 then
    Result := Items[I]
  else
    Result := nil;
end;

function TComponentInfos.GetItems(I: Integer): TComponentInfo;
begin
  Result := TComponentInfo(FList.Objects[I]);
end;

function TComponentInfos.IndexOf(const AName: string): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and not AnsiSameText(Items[Result].Name, AName) do
    Dec(Result);
end;

procedure TComponentInfos.Remove(Comp: TComponentInfo);
begin
  if FList.IndexOfObject(Comp) > -1 then
    Delete(FList.IndexOfObject(Comp));
end;

procedure TComponentInfos.RemoveInvalid;
var
  I: Integer;
  Prefix: string;
  InvComps: string;
begin
  I := Count - 1;
  Prefix := '';
  InvComps := '';
  while (I >= 0) do
  begin
    if not Items[I].Valid then
    begin
      InvComps := InvComps + Prefix + Items[I].Name;
      Prefix := ', ';
      Delete(I);
    end;
    Dec(I);
  end;
  if InvComps > '' then
    HintMsgFmt('Removed %s', [InvComps]);
end;

//=== { TDtxAnalyzer } =======================================================

constructor TDtxAnalyzer.Create(ATaskManager: ITaskManager);
begin
  inherited Create(ATaskManager);
  FGroupInfos := TGroupInfos.Create(nil);
  FComponentInfos := TComponentInfos.Create;
  FPackages := TDtxPackages.Create;
  FDtxInfos := TDtxInfos.Create;
  FSpecificDtxFiles := TStringList.Create;
  FSpecificDtxFiles.Sorted := True;
  FSpecificDtxFiles.Duplicates := dupIgnore;
end;

destructor TDtxAnalyzer.Destroy;
begin
  FSpecificDtxFiles.Free;
  FGroupInfos.Free;
  FComponentInfos.Free;
  FPackages.Free;
  FDtxInfos.Free;
  inherited Destroy;
end;

function TDtxAnalyzer.AddComponent(ADtxInfo: TDtxInfo; ADtxTopic: TDtxTopic): Boolean;
var
  CompName: string;
  Comp: TComponentInfo;
begin
  CompName := ADtxTopic.Name;
  Result := not IsNullStr(CompName) and not HasDot(CompName);
  if not Result then
    Exit;

  Comp := FComponentInfos.Add(CompName);
  ParseGroupString(Comp, ADtxTopic.Group);
  if IsNullStr(ADtxTopic.TitleImg) then
    Comp.Image := 'NO_ICON'
  else
    Comp.Image := RegisteredClassNameToImageFileName(ADtxTopic.TitleImg);
  Comp.Flags := Comp.Flags + ADtxTopic.Flags;
  Comp.Summary := ADtxTopic.Summary;

  if (cifIgnore in Comp.Flags) or (Comp.Flags = []) then
  begin
    FComponentInfos.Remove(Comp);
    Exit;
  end;
  Comp.Package := ADtxInfo.Package;
  if (cifSymbolType * Comp.Flags = []) and (cifAssignedToGroup in Comp.Flags) then
    Comp.Flags := Comp.Flags + [cifComponent];
end;

function TDtxAnalyzer.AddDtxFile(ADtxList: TDtxList; const APercComplete: Integer): Boolean;
var
  I: Integer;
  Dtx: TDtxInfo;
  Pck: TDtxPackage;

  PckName: string;
  PckIdx: Integer;
begin
  Dtx := FDtxInfos.AddUnit(ChangeFileExt(ADtxList.PasFileName, ''));
  Dtx.SetPercComplete(APercComplete);

  PckName := Trim(ADtxList.Package);
  PckIdx := FPackages.IndexOfPackage(PckName);
  if PckIdx > -1 then
    Pck := FPackages[PckIdx]
  else
    Pck := FPackages.AddPackage(PckName);
  Dtx.SetPackage(Pck);

  Result := True;
  for I := 0 to ADtxList.Count - 1 do
    if ADtxList[i].HasJVCLInfo then
      AddComponent(Dtx, ADtxList[i]);
end;

function TDtxAnalyzer.CanStart: Boolean;
begin
  Result := {CheckFile(JVCLGroupsTreeFileName) and}
    ((PackageListFileName = '') or CheckFile(PackageListFileName));
end;

//procedure TDtxAnalyzer.Analyze;
//var
//  I: Integer;
//begin
//  StatusMsg('Removing undocumented units');
//  for I := FDtxInfos.Count - 1 downto 0 do
//    if (FDtxInfos[I].PercComplete < MinPercComplete) and not IsInOkList(I) then
//      FDtxInfos.RemoveUnit(I);
//
//  I := 0;
//  while (I < FDtxInfos.Count - 1) and (FDtxInfos.Count > MaxFilesInDox) do
//    if IsInOkList(I) then
//      Inc(I)
//    else
//      FDtxInfos.RemoveUnit(I);
//
//  FDtxInfos.Sort;
//
//  StatusMsg('Removing empty packages');
//  FPackages.RemoveEmptyPackages;
//  FPackages.Sort;
//end;

function TDtxAnalyzer.CleanupComponents: Boolean;
begin
  Result := True;
  StatusMsg('Removing invalid components...');
  GroupInfos.Iterate(RemoveUnknownComponents, False);
end;

function TDtxAnalyzer.CleanupDtx: Boolean;
begin
  Result := True;

  FDtxInfos.Sort;
end;

function TDtxAnalyzer.CleanupPackages: Boolean;
begin
  StatusMsg('Removing empty packages');

  Result := True;

  FPackages.RemoveEmptyPackages;
  FPackages.RemoveInvalidPackages;
  FPackages.Sort;
end;

//procedure TDtxAnalyzer.SetMinPercComplete(const Value: Integer);
//begin
//  FMinPercComplete := Value;
//  if FMinPercComplete < 0 then
//    FMinPercComplete := 0
//  else
//  if FMinPercComplete > 100 then
//    FMinPercComplete := 100;
//end;

function TDtxAnalyzer.DoExecute: Boolean;
begin
//  Result := Init;
//  if not Result then
//    Exit;

  Result := ProcessFiles;
  if not Result then
    Exit;

  Result := Final;
  if not Result then
    Exit;
end;

function TDtxAnalyzer.Final: Boolean;
begin
  Result := CleanupComponents;
  if not Result then
    Exit;

  Result := MergeGroupCompLists;
  if not Result then
    Exit;

  Result := CleanupDtx;
  if not Result then
    Exit;

  Result := CleanupPackages;
  if not Result then
    Exit;
end;

function TDtxAnalyzer.GetTaskDescription: string;
begin
  Result := 'Analyzing dtx files';
end;

//function TDtxAnalyzer.Init: Boolean;
//begin
//  Result := ParseGroupsTree;
//end;

procedure TDtxAnalyzer.MergeComps(AGroupInfo: TGroupInfo);
var
  SL: TStrings;
  I: Integer;
  J: Integer;
begin
  // Add components from sub groups
  SL := TStringList.Create;
  try
    for I := 0 to AGroupInfo.SubGroups.Count - 1 do
    begin
      SL.Assign(AGroupInfo.SubGroups[I].Components);
      // Remove items we don't want in the parent list
      for J := SL.Count - 1 downto 0 do
        if cifNotInParentList in ComponentInfos.ItemByName[SL[J]].Flags then
          SL.Delete(J);
      AGroupInfo.Components.AddStrings(SL);
    end
  finally
    SL.Free;
  end;
  // Sort the component list
  AGroupInfo.Sort;
end;

//function TDtxAnalyzer.IsInOkList(const I: Integer): Boolean;
//begin
//  Result := Assigned(FOkList) and
//    (FOkList.IndexOf(FDtxInfos[i].UnitName) >= 0);
//end;

function TDtxAnalyzer.MergeGroupCompLists: Boolean;
begin
  Result := True;
  StatusMsg('Merge component lists with parents...');
  { TODO : Check }
  GroupInfos.Iterate(MergeComps, True);
  StatusMsg('Done.');
end;

//function TDtxAnalyzer.ParseGroupsTree: Boolean;
//var
//  Parser: TGroupsTreeParser;
//begin
//  Parser := TGroupsTreeParser.Create;
//  try
//    Parser.GroupInfos := Self.GroupInfos;
//    Result := Parser.Parse(JVCLGroupsTreeFileName);
//  finally
//    Parser.Free;
//  end;
//end;

procedure TDtxAnalyzer.ParseGroupString(AComp: TComponentInfo;
  GroupString: string);
var
  I: Integer;
  ThisName: string;
  Grp: TGroupInfo;
begin
  GroupString := Trim(GroupString);
  //  Delete(GroupString, Length(GroupString), 1);
  while GroupString <> '' do
  begin
    I := Pos(',', GroupString);
    if I = 0 then
      I := Length(GroupString) + 1;
    ThisName := Trim(Copy(GroupString, 1, I - 1));
    Delete(GroupString, 1, I);
    if Copy(ThisName, 1, 1) = '$' then
      Delete(ThisName, 1, 1);
    GroupString := Trim(GroupString);
    if AnsiSameText(Copy(ThisName, 1, 5), 'JVCL.') then
      Grp := GroupInfos.Locate(Copy(ThisName, 6, Length(ThisName) - 5))
    else
      Grp := nil;
    if Grp <> nil then
    begin
      if Grp.Components.IndexOf(AComp.Name) < 0 then
      begin
        AComp.Flags := AComp.Flags + [cifAssignedToGroup];
        Grp.Components.Add(AComp.Name);
      end
      else
        HintMsgFmt('%s already added to group %s', [AComp.Name, ThisName]);
    end
    else
      if AnsiSameText(Copy(ThisName, 1, 5), 'JVCL.') and not AnsiSameText(Copy(ThisName, 1, 10), 'JVCL.Info.') then
      WarningMsgFmt('Group %s not found', [ThisName]);
  end;
end;

function TDtxAnalyzer.ProcessFiles: Boolean;
var
  DtxFiles: TStringList;
  PackageList: TStringList;
  DtxPasCombiner: TDtxPasCombiner;
  DtxList: TDtxList;
  I: Integer;
  AFileName: string;
begin
  DtxFiles := TStringList.Create;
  try
    StatusMsg('Collecting dtx files');
    CollectFiles(DtxFiles);

    Result := CheckFileExists(DtxFiles);
    if not Result then
      Exit;

    PackageList := TStringList.Create;
    try
      if PackageListFileName <> ''  then
        PackageList.LoadFromFile(PackageListFileName);

      StatusMsg('Processing dtx files');

      for I := 0 to DtxFiles.Count - 1 do
      begin
        Progress(I, DtxFiles.Count);
        ErrorHeaderFmt('Processing %s', [ChangeFileExt(ExtractFileName(DtxFiles[i]), '')]);

        DtxList := TDtxList.Create;
        try
          Result := DtxList.LoadFromFile(DtxFiles[i]);
          if not Result then
          begin
            ErrorMsgFmt('Error parsing %s', [DtxFiles[i]]);
            Exit;
          end;

          if Assigned(Diagnoser) then
          begin
            Result := Diagnoser.Collect(DtxList);
            if not Result then
              Exit;
          end;

          DtxPasCombiner := TDtxPasCombiner.Create;
          try
            AFileName := ExtractFileName(DtxFiles[i]);
            DtxPasCombiner.DtxList := DtxList;
            DtxPasCombiner.SourcePasFileName := SourcePasDir + ChangeFileExt(AFileName, '.pas');
            //          DtxPasCombiner.SourceDtxFileName := DtxFiles[i];
            DtxPasCombiner.OriginalDtxFileName := OriginalDtxDir + ChangeFileExt(AFileName, '.dtx');

            //            DtxPasCombiner.DestXMLFileName := XMLDir + ChangeFileExt(AFileName, '.xml');
            DtxPasCombiner.DestDtxFileName := DestDtxDir + ChangeFileExt(AFileName, '.dtx');
            DtxPasCombiner.RegisteredComponents := RegisteredComponents;
            DtxPasCombiner.CopyHeaderFromOriginal := Self.CopyHeaderFromOriginal;
            DtxPasCombiner.UpdateOriginalEmpties := Self.UpdateOriginalEmpties;
            DtxPasCombiner.CopySeeAlsoFromOriginal := Self.CopySeeAlsoFromOriginal;

            DtxPasCombiner.OnlyCheck := not GenerateFormattedDtxFiles;
            DtxPasCombiner.GenerateForBuild := GenerateForBuild;

            DtxPasCombiner.PackageList := PackageList;

            Result := DtxPasCombiner.Execute;
            if not Result then
              Exit;

            AddDtxFile(DtxPasCombiner.DtxList, DtxPasCombiner.PercComplete);
            //            GroupProducer.AddPasFile(DtxPasCombiner.DtxList);
            //
            //            DoxProducer.AddDtxFile(DtxPasCombiner.DtxList, DtxPasCombiner.PercComplete);
          finally
            DtxPasCombiner.Free;
          end;
        finally
          DtxList.Free;
        end;
      end;
    finally
      ErrorHeader('');
      PackageList.Free;
    end;
  finally
    DtxFiles.Free;
  end;
end;

procedure TDtxAnalyzer.RemoveUnknownComponents(AGroupInfo: TGroupInfo);
var
  CompList: TStrings;
  I: Integer;
begin
  CompList := AGroupInfo.Components;
  I := CompList.Count - 1;
  while I >= 0 do
  begin
    if ComponentInfos.IndexOf(CompList[I]) < 0 then
    begin
      WarningMsgFmt('Removed %s', [CompList[I]]);
      CompList.Delete(I);
    end;
    Dec(I);
  end;
end;

//=== { TDtxInfo } ===========================================================

//procedure TGroupInfo.AddComponent(AComp: TComponentInfo);
//begin
//
//end;

constructor TDtxInfo.Create(AName: string);
begin
  inherited Create;
  FName := AName;
end;

destructor TDtxInfo.Destroy;
begin
  SetPackage(nil);
  inherited Destroy;
end;

function TDtxInfo.GetPackage: TDtxPackage;
begin
  Result := FPackage;
end;

function TDtxInfo.GetPercComplete: Integer;
begin
  Result := FPercComplete;
end;

function TDtxInfo.GetUnitName: string;
begin
  Result := FName;
end;

procedure TDtxInfo.SetPackage(Value: TDtxPackage);
begin
  if Value <> Package then
  begin
    if Package <> nil then
      Package.RemoveDtx(Self);
    FPackage := Value;
    if Package <> nil then
      Package.AddDtx(Self);
  end;
end;

procedure TDtxInfo.SetPercComplete(const Value: Integer);
begin
  FPercComplete := Value;
end;

//=== { TDtxInfos } ==========================================================

destructor TDtxInfos.Destroy;
var
  I: Integer;
begin
  I := Count - 1;
  while (I >= 0) do
  begin
    Items[I].Free;
    Dec(I);
  end;
  Clear;
  inherited Destroy;
end;

function TDtxInfos.AddUnit(Name: string): TDtxInfo;
begin
  if IndexOfUnit(Name) < 0 then
  begin
    Result := TDtxInfo.Create(Name);
    try
      Add(Result);
    except
      Result.Free;
      raise;
    end;
  end
  else
    Result := Items[IndexOfUnit(Name)];
end;

//function TDtxInfos.CountCompleted: Integer;
//var
//  I: Integer;
//begin
//  Result := 0;
//  for I := 0 to Count - 1 do
//    if Items[I].PercComplete >= 10 then
//      Inc(Result);
//end;

function TDtxInfos.GetItems(Index: Integer): TDtxInfo;
begin
  Result := TDtxInfo(inherited Items[Index]);
end;

function TDtxInfos.IndexOfUnit(Name: string): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and not AnsiSameText(Items[Result].UnitName, Name) do
    Dec(Result);
end;

procedure TDtxInfos.RemoveUnit(Index: Integer);
begin
  Items[Index].Free;
  Delete(Index);
end;

procedure TDtxInfos.Sort;
var
  SL: TStrings;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    for I := 0 to Count - 1 do
      SL.AddObject(Items[I].UnitName, Items[I]);
    Clear;
    TStringList(SL).Sort;
    for I := 0 to SL.Count - 1 do
      Add(SL.Objects[I]);
  finally
    SL.Free;
  end;
end;

//=== { TDtxPackage } ========================================================

constructor TDtxPackage.Create(AName: string);
begin
  inherited Create;
  FName := AName;
  FDtxList := TList.Create;
  FComponentList := TList.Create;
end;

destructor TDtxPackage.Destroy;
begin
  while FDtxList.Count > 0 do
    TDtxInfo(FDtxList[0]).SetPackage(nil);
  FreeAndNil(FDtxList);
  while FComponentList.Count > 0 do
    RemoveComponent(TComponentInfo(FComponentList[0]));
  FreeAndNil(FComponentList);
  inherited Destroy;
end;

procedure TDtxPackage.AddComponent(AComponentInfo: TComponentInfo);
begin
  FComponentList.Add(AComponentInfo);
  AComponentInfo.FPackage := Self;
end;

procedure TDtxPackage.AddDtx(DtxInfo: TDtxInfo);
begin
  if FDtxList.IndexOf(DtxInfo) < 0 then
    FDtxList.Add(DtxInfo);
end;

function TDtxPackage.GetComponentCount: Integer;
begin
  Result := FComponentList.Count;
end;

function TDtxPackage.GetComponentInfos(Index: Integer): TComponentInfo;
begin
  Result := TComponentInfo(FComponentList[Index]);
end;

function TDtxPackage.GetDtxCount: Integer;
begin
  Result := FDtxList.Count;
end;

function TDtxPackage.GetDtxInfos(Index: Integer): TDtxInfo;
begin
  Result := TDtxInfo(FDtxList[Index]);
end;

function TDtxPackage.GetPackageName: string;
begin
  Result := FName;
end;

procedure TDtxPackage.RemoveComponent(AComponentInfo: TComponentInfo);
begin
  AComponentInfo.FPackage := nil;
  FComponentList.Remove(AComponentInfo);
end;

procedure TDtxPackage.RemoveDtx(DtxInfo: TDtxInfo);
begin
  FDtxList.Remove(DtxInfo);
end;

procedure TDtxPackage.SaveToDtx(ADtxWriter: TDtxWriter);
begin
  ADtxWriter.WriteSepLine;
  ADtxWriter.WriteTopicName(PackageToTopicName(PackageName));
  ADtxWriter.WriteLn('<GROUP $JVCL.Packages>');
  ADtxWriter.WriteTitle(PackageName + ' package');
  ADtxWriter.BeginSection('Description');
  ADtxWriter.WriteLnFmt('This is package %s', [PackageName]);
  ADtxWriter.EndSection;
  ADtxWriter.WriteLn;
end;

//=== { TDtxPackages } =======================================================

destructor TDtxPackages.Destroy;
var
  I: Integer;
begin
  I := Count - 1;
  while (I >= 0) do
  begin
    Items[I].Free;
    Dec(I);
  end;
  Clear;
  inherited Destroy;
end;

function TDtxPackages.AddPackage(Name: string): TDtxPackage;
begin
  if IndexOfPackage(Name) < 0 then
  begin
    Result := TDtxPackage.Create(Name);
    try
      Add(Result);
    except
      Result.Free;
      raise;
    end;
  end
  else
    Result := Items[IndexOfPackage(Name)];
end;

function TDtxPackages.CountEmpty: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if Items[I].DtxCount = 0 then
      Inc(Result);
end;

function TDtxPackages.GetItems(Index: Integer): TDtxPackage;
begin
  Result := TDtxPackage(inherited Items[Index]);
end;

function TDtxPackages.IndexOfPackage(PackageName: string): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and not AnsiSameText(Items[Result].PackageName, PackageName) do
    Dec(Result);
end;

procedure TDtxPackages.RemoveEmptyPackages;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I].DtxCount = 0 then
    begin
      HintMsgFmt('Deleted package %s', [Items[i].PackageName]);
      Delete(I);
    end;
end;

procedure TDtxPackages.RemoveInvalidPackages;
var
  I: Integer;
  J: Integer;
begin
  for I := Count - 1 downto 0 do
    if Pos('?', Items[I].PackageName) >= 1 then
    begin
      HintMsgFmt('Deleted package %s', [Items[i].PackageName]);
      for J := 0 to Items[i].ComponentCount - 1 do
        WarningMsgFmt(' Package %s contains component %s', [Items[i].PackageName, Items[i].ComponentInfos[j].Name]);
      Delete(I);
    end;
end;

procedure TDtxPackages.SaveToDtx(ADtxWriter: TDtxWriter);
var
  I: Integer;
begin
  // header
  ADtxWriter.WriteSepLine;
  ADtxWriter.WriteTopicName('$JVCL.Packages');
  ADtxWriter.WriteLn('<GROUP $JVCL>');
  ADtxWriter.WriteTitle('Packages');
  ADtxWriter.BeginSection('Description');
  ADtxWriter.WriteLn('JEDI-VCL consists of many packages');
  ADtxWriter.EndSection;
  ADtxWriter.WriteLn;

  for I := 0 to Count - 1 do
    Items[i].SaveToDtx(ADtxWriter);
end;

procedure TDtxPackages.Sort;
var
  SL: TStrings;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    for I := 0 to Count - 1 do
      SL.AddObject(Items[I].PackageName, Items[I]);
    Clear;
    TStringList(SL).Sort;
    for I := 0 to SL.Count - 1 do
      Add(SL.Objects[I]);
  finally
    SL.Free;
  end;
end;

//=== { TGroupInfo } =========================================================

constructor TGroupInfo.Create(const AParent: TGroupInfo; const AInfoString: string);
begin
  inherited Create;
  FComponents := TStringList.Create;
  FSubGroups := TGroupInfos.Create(Self);
  FParent := AParent;
  ParseGroupInfo(AInfoString, FGroupID, FGroupTitle, FGroupText);
end;

destructor TGroupInfo.Destroy;
begin
  FSubGroups.Free;
  FComponents.Free;
  inherited Destroy;
end;

function TGroupInfo.CountClass(AComponentList: TComponentInfos): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Components.Count - 1 downto 0 do
    if cifClass in AComponentList.ItemByName[Components[I]].Flags then
      Inc(Result);
end;

function TGroupInfo.CountComp(AComponentList: TComponentInfos): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Components.Count - 1 downto 0 do
    if cifComponent in AComponentList.ItemByName[Components[I]].Flags then
      Inc(Result);
end;

function TGroupInfo.CountRoutines(AComponentList: TComponentInfos): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Components.Count - 1 downto 0 do
    if cifRoutine in AComponentList.ItemByName[Components[I]].Flags then
      Inc(Result);
end;

procedure TGroupInfo.FillGroupIDs(Strings: TStrings);
begin
  Strings.Add(ParentGroupID + '.' + GroupID);
  SubGroups.FillGroupIDs(Strings);
end;

procedure TGroupInfo.Iterate(Proc: TGroupInfoProc;
  const GroupsFirst: Boolean);
begin
  if GroupsFirst then
  begin
    SubGroups.Iterate(Proc, GroupsFirst);
    Proc(Self);
  end
  else
  begin
    Proc(Self);
    SubGroups.Iterate(Proc, GroupsFirst);
  end;
end;

function TGroupInfo.ParentGroupID(const WantFuncRef: Boolean): string;
begin
  if FParent = nil then
  begin
    Result := 'JVCL';
    if WantFuncRef then
      Result := Result + '.FuncRef';
  end
  else
    Result := Parent.ParentGroupID(WantFuncRef) + '.' + Parent.GroupID;
end;

procedure TGroupInfo.Sort;
begin
  TStringList(Components).Sort;
end;

//=== { TGroupInfos } ========================================================

constructor TGroupInfos.Create(AOwner: TGroupInfo);
begin
  inherited Create;
  FList := TObjectList.Create(True);
  FOwner := AOwner;
end;

destructor TGroupInfos.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TGroupInfos.Add(const AInfoString: string): TGroupInfo;
begin
  Result := TGroupInfo.Create(Owner, AInfoString);
  if Locate(Result.GroupID) <> nil then
  begin
    ErrorMsg(Format('duplicate group ID "%s"', [Result.GroupID]));
    FreeAndNil(Result);
  end
  else
    FList.Add(Result);
end;

procedure TGroupInfos.FillGroupIDs(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[i].FillGroupIDs(Strings);
end;

function TGroupInfos.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TGroupInfos.GetItems(I: Integer): TGroupInfo;
begin
  Result := TGroupInfo(FList[I]);
end;

procedure TGroupInfos.Iterate(Proc: TGroupInfoProc; const GroupsFirst: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Iterate(Proc, GroupsFirst);
end;

function TGroupInfos.Locate(AGroupID: string): TGroupInfo;
var
  IDot: Integer;
  I: Integer;
begin
  IDot := Pos('.', AGroupID);
  if IDot = 0 then
    IDot := Length(AGroupID) + 1;
  I := Count - 1;
  while (I >= 0) and not AnsiSameText(Items[I].GroupID, Copy(AGroupID, 1, IDot - 1)) do
    Dec(I);
  if I >= 0 then
    Result := Items[I]
  else
    Result := nil;
  if (IDot < Length(AGroupID)) and (Result <> nil) then
    Result := Result.SubGroups.Locate(Copy(AGroupID, IDot + 1, Length(AGroupID) - IDot));
end;

//=== { TGroupsTreeParser } ==================================================

constructor TGroupsTreeParser.Create;
begin
  inherited Create;
  FCurList := TStack.Create;
  //  FGroupInfo := TGroupInfos.Create(nil);
end;

destructor TGroupsTreeParser.Destroy;
begin
  FCurList.Free;
  FInternalGroupInfos.Free;
  inherited Destroy;
end;

procedure TGroupsTreeParser.FillGroupIDs(Strings: TStrings);
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    GroupInfos.FillGroupIDs(Strings);
  finally
    Strings.EndUpdate;
  end;
end;

function TGroupsTreeParser.GetGroupInfos: TGroupInfos;
begin
  Result := FGroupInfos;
  if Result = nil then
  begin
    if FInternalGroupInfos = nil then
      FInternalGroupInfos := TGroupInfos.Create(nil);
    Result := FInternalGroupInfos;
  end;
end;

function TGroupsTreeParser.Parse(const AFileName: string): Boolean;
begin
  FLines := TStringList.Create;
  try
    FLines.LoadFromFile(AFileName);

    Result := ParseLines;
  finally
    FLines.Free;
  end;
end;

function TGroupsTreeParser.ParseAsTreeItem(const Item: Integer): Boolean;
var
  S: string;
  I: Integer;
  ThisLevel: Integer;
  FoundBar: Boolean;
begin
  Result := True;
  S := FLines[Item];
  Delete(S, 1, 3); // Ignore first three characters
  I := 2;
  ThisLevel := 0;
  FoundBar := False;
  while (I <= Length(S)) and ((S[I] = '|') or ((S[I] = ' ') and not FoundBar)) do
  begin
    FoundBar := S[I] = '|';
    Inc(ThisLevel);
    Inc(I, 4);
  end;
  Delete(S, 1, I - 2);
  if ThisLevel = FCurList.Count - 1 then
    // Add an object to the last added item.
    FCurList.Push(TGroupInfos(FCurList.Peek).Add(Trim(S)).SubGroups)
  else
    if ThisLevel > FCurList.Count - 1 then
  begin
    ErrorMsg('error during parsing; level error');
    Result := False;
    Exit;
  end
  else
    if ThisLevel < FCurList.Count - 1 then
  begin
    while ThisLevel < FCurList.Count - 1 do
      FCurList.Pop;
    FCurList.Push(TGroupInfos(FCurList.Peek).Add(Trim(S)).SubGroups);
  end;
end;

function TGroupsTreeParser.ParseLines: Boolean;
var
  I: Integer;
begin
  StatusMsg('Parsing the tree... ');

  FCurList.Push(GroupInfos);
  Result := True;
  for I := 0 to FLines.Count - 1 do
  begin
    //    ProgressMsg(I, FLines.Count);
    if not ParseAsTreeItem(I) then
    begin
      Result := False;
      Break;
    end;
  end;

  //  ProgressMsg(1, 1);
  StatusMsg('Done.');
end;

function TDtxAnalyzer.GetSpecificDtxFiles: TStrings;
begin
  Result := FSpecificDtxFiles;
end;

procedure TDtxAnalyzer.SetSpecificDtxFiles(Value: TStrings);
begin
  FSpecificDtxFiles.Assign(Value);
end;

procedure TDtxAnalyzer.CollectFiles(AFiles: TStrings);
var
  I: Integer;
  AFileName: string;
begin
  GetAllFilesFrom(SourceDtxDir, '*.dtx', AFiles);

  if SpecificDtxFiles.Count = 0 then
    Exit;

  for I := AFiles.Count - 1 downto 0 do
  begin
    AFileName := ExtractFileName(ChangeFileExt(AFiles[i], ''));
    if SpecificDtxFiles.IndexOf(AFileName) < 0 then
      AFiles.Delete(I);
  end;
end;

function TDtxAnalyzer.CheckFileExists(AFiles: TStrings): Boolean;
var
  AFileName: string;
  SourcePasFileName: string;
  OriginalDtxFileName: string;
//  DestDtxFileName: string;
  I: Integer;
begin
  Result := True;
  for I := 0 to AFiles.Count - 1 do
  begin
    AFileName := ExtractFileName(AFiles[i]);

    SourcePasFileName := SourcePasDir + ChangeFileExt(AFileName, '.pas');
    if not FileExists(SourcePasFileName) then
    begin
      ErrorMsgFmt('File <%s> does not exist', [SourcePasFileName]);
      Result := False;
    end;

    if CopyHeaderFromOriginal or UpdateOriginalEmpties or CopySeeAlsoFromOriginal then
    begin
      OriginalDtxFileName := OriginalDtxDir + ChangeFileExt(AFileName, '.dtx');
      if not FileExists(OriginalDtxFileName) then
      begin
        ErrorMsgFmt('File <%s> does not exist', [OriginalDtxFileName]);
        Result := False;
      end;
    end;

//    if GenerateFormattedDtxFiles then
//    begin
//      DestDtxFileName := DestDtxDir + ChangeFileExt(AFileName, '.dtx');
//      if not FileExists(DestDtxFileName) then
//      begin
//        ErrorMsgFmt('File <%s> does not exist', [DestDtxFileName]);
//        Result := False;
//      end;
//    end;
  end;
end;

end.


