unit GenDoxEngine;

interface

uses
  SysUtils, Classes;

type
  TDtxInfos = class;
  TDtxInfo = class;

  TDtxPackages = class;
  TDtxPackage = class;

  TDtxUnitState = (dusUnknown, dusLocked, dusCompleted, dusOther);
  TInformEvent = procedure(Action, SubActionCaption, SubAction: string) of object;

  TDtxInfos = class(TList)
  private
    function GetItems(Index: Integer): TDtxInfo;
  protected
    function AddUnit(Name: string): TDtxInfo;
    procedure RemoveUnit(Index: Integer);
  public
    destructor Destroy; override;
    function CountCompleted: Integer;
    function CountLocked: Integer;
    function IndexOfUnit(Name: string): Integer;
    procedure RemoveUnknownStates;
    procedure Sort;
    property Items[Index: Integer]: TDtxInfo read GetItems; default;
  end;

  TDtxInfo = class
  private
    FName: string;
    FState: TDtxUnitState;
    FPackage: TDtxPackage;
    function GetUnitName: string;
    function GetState: TDtxUnitState;
    function GetPackage: TDtxPackage;
  protected
    procedure SetState(Value: TDtxUnitState);
    procedure SetPackage(Value: TDtxPackage);
  public
    constructor Create(AName: string);
    destructor Destroy; override;
    property UnitName: string read GetUnitName;
    property State: TDtxUnitState read GetState;
    property Package: TDtxPackage read GetPackage;
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
    procedure Sort;
    property Items[Index: Integer]: TDtxPackage read GetItems; default;
  end;

  TDtxPackage = class
  private
    FDtxList: TList;
    FName: string;
    function GetDtxCount: Integer;
    function GetDtxInfos(Index: Integer): TDtxInfo;
    function GetPackageName: string;
  protected
    procedure AddDtx(DtxInfo: TDtxInfo);
    procedure RemoveDtx(DtxInfo: TDtxInfo);
  public
    constructor Create(AName: string);
    destructor Destroy; override;
    property DtxCount: Integer read GetDtxCount;
    property DtxInfos[Index: Integer]: TDtxInfo read GetDtxInfos; default;
    property PackageName: string read GetPackageName;
  end;

procedure GatherInfo(const RunPath: string; const OnInfo: TInformEvent; out Packages: TDtxPackages;
  out DtxInfos: TDtxInfos);
procedure Analyze(const OnInfo: TInformEvent; Packages: TDtxPackages; DtxInfos: TDtxInfos; WantIncomplete: Boolean);
procedure BuildMainHelpDOX(const OnInfo: TInformEvent; DtxList: TDtxInfos);

implementation

uses
  JclFileUtils, JclStrings;

resourcestring
  SGatherInfoAction = 'Gathering information.';
  SGatherInfoSubActCaption = 'Scanning:';
  SAnalyzeInfoAction = 'Analyzing data.';
  SAnalyzeInfoSubActCaption = 'Task:';
  SBuildMainAction = 'Building main help file project';
  SBuildMainSubActCaption = 'File:';

var
  FullDtxPath: string;
  FullRunPath: string;

function MakeFull(APath: string): string;
begin
  if Pos(PathSeparator, APath) = 1 then
    Result := PathCanonicalize(ExtractFileDrive(ParamStr(0)) + APath)
  else if Pos(':', APath) = 2 then
    Result := PathCanonicalize(APath)
  else
    Result := PathCanonicalize(ExtractFilePath(ParamStr(0)) + APath);
  if (Result <> '') and (AnsiLastChar(Result) <> '\') then
    Result := Result + '\';
end;

function MakeRelative(APath, RelativeFrom: string): string;
var
  BasePath: TStrings;
  ThisPath: TStrings;
  CommonDepth: Integer;
  I: Integer;
begin
  BasePath := nil;
  ThisPath := nil;
  try
    BasePath := TStringList.Create;
    ThisPath := TStringList.Create;
    StrIToStrings(RelativeFrom, PathSeparator, BasePath, False);
    StrIToStrings(APath, PathSeparator, ThisPath, False);
    if not PathIsAbsolute(APath) then
    begin
      CommonDepth := PathCommonPrefix(APath, RelativeFrom);
      if CommonDepth > 0 then
      begin
        for I := CommonDepth - 1 downto 0 do
          ThisPath.Delete(I);
        for I := 0 to BasePath.Count - CommonDepth - 1 do
          ThisPath.Insert(0, '..');
      end;
    end;
    Result := StringsToStr(ThisPath, PathSeparator, False);
    if (Result <> '') and (AnsiLastChar(Result) <> '\') then
      Result := Result + '\';
  finally
    BasePath.Free;
    ThisPath.Free;
  end;
end;

procedure Inform(Event: TInformEvent; Action, SubActionCaption, SubAction: string);
begin
  if @Event <> nil then
    Event(Action, SubActionCaption, SubAction);
end;

procedure ParseDtx(FileName: string; OnInfo: TInformEvent; Packages: TDtxPackages;
  DtxInfos: TDtxInfos);
var
  SL: TStrings;
  Dtx: TDtxInfo;
  I: Integer;
  HasStatus: Boolean;
  HasPackage: Boolean;
  S: string;
  PckName: string;
  PckIdx: Integer;
  Pck: TDtxPackage;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);
    Dtx := DtxInfos.AddUnit(ChangeFileExt(ExtractFileName(FileName), ''));
    I := 0;
    HasStatus := False;
    HasPackage := False;
    while (I < SL.Count) and (not HasStatus or not HasPackage) do
    begin
      if not HasStatus and AnsiSameText(Copy(SL[I], 1, 9), '##Status:') then
      begin
        HasStatus := True;
        S := Trim(Copy(SL[I], 10, Length(SL[I]) - 9));
        // dusUnknown, dusLocked, dusCompleted, dusOther
        if AnsiSameText(Copy(S, 1, 9), 'Completed') then
          Dtx.SetState(dusCompleted)
        else if AnsiSameText(Copy(S, 1, 6), 'Locked') then
          Dtx.SetState(dusLocked)
        else
          Dtx.SetState(dusOther);
      end
      else if not HasPackage and AnsiSameText(Copy(SL[I], 1, 10), 'Package:') then
      begin
        HasPackage := True;
        PckName := Trim(Copy(SL[I], 11, Length(SL[I]) - 10));
        PckIdx := Packages.IndexOfPackage(PckName);
        if PckIdx > -1 then
          Pck := Packages[PckIdx]
        else
          Pck := Packages.AddPackage(PckName);
        Dtx.SetPackage(Pck);
      end;
      Inc(I);
    end;
  finally
    SL.Free;
  end;
end;

procedure ScanDtx(OnInfo: TInformEvent; Packages: TDtxPackages; DtxInfos: TDtxInfos);
var
  Res: Integer;
  SR: TSearchRec;
begin
  Res := FindFirst(FullDtxPath + '*.dtx', faAnyFile, SR);
  try
    while Res = 0 do
    begin
      Inform(OnInfo, SGatherInfoAction, SGatherInfoSubActCaption, '..\' + SR.Name);
      ParseDtx(FullDtxPath + SR.Name, OnInfo, Packages, DtxInfos);
      Res := FindNext(SR);
    end;
  finally
    FindClose(SR);
  end;
end;

function GetValue(Strings: TStrings; Index: Integer): Integer;
var
  Name: string;
begin
  Name := Strings.Names[Index];
  Result := StrToIntDef(Copy(Strings[Index], Length(Name) + 2, Length(Strings[Index]) - Length(Name) - 1), 0);
end;

procedure IncValue(Strings: TStrings; Index: Integer; N: Integer = 1);
var
  Name: string;
  Value: Integer;
begin
  Name := Strings.Names[Index];
  Value := StrToIntDef(Copy(Strings[Index], Length(Name) + 2, Length(Strings[Index]) - Length(Name) - 1), 0);
  Inc(Value, N);
  Strings[Index] := Name + '=' + IntToStr(Value);
end;

procedure GatherInfo(const RunPath: string; const OnInfo: TInformEvent; out Packages: TDtxPackages;
  out DtxInfos: TDtxInfos);
begin
  Inform(OnInfo, SGatherInfoAction, SGatherInfoSubActCaption, '');
  Packages := nil;
  DtxInfos := nil;
  try
    Packages := TDtxPackages.Create;
    DtxInfos := TDtxInfos.Create;
    FullRunPath := MakeFull(RunPath);
    FullDtxPath := MakeFull('..');
    ScanDtx(OnInfo, Packages, DtxInfos);
  except
    FreeAndNil(DtxInfos);
    FreeAndNil(Packages);
    raise;
  end;
end;

procedure Analyze(const OnInfo: TInformEvent; Packages: TDtxPackages; DtxInfos: TDtxInfos; WantIncomplete: Boolean);
var
  I: Integer;
begin
  Inform(OnInfo, SAnalyzeInfoAction, SAnalyzeInfoSubActCaption, 'Removing undocumented units');
  DtxInfos.RemoveUnknownStates;
  for I := DtxInfos.Count - 1 downto 0 do
    if DtxInfos[I].State <> dusCompleted then
      DtxInfos.RemoveUnit(I);
  DtxInfos.Sort;
  Inform(OnInfo, SAnalyzeInfoAction, SAnalyzeInfoSubActCaption, 'Removing empty packages');
  Packages.RemoveEmptyPackages;
  Packages.Sort;
end;

procedure BuildMainHelpDOX(const OnInfo: TInformEvent; DtxList: TDtxInfos);
var
  Template: TStrings;
  FileSectionCntIndex: Integer;
  FileNum: Integer;
  I: Integer;
begin
  Inform(OnInfo, SBuildMainAction, SBuildMainSubActCaption, '');
  Template := TStringList.Create;
  try
    Template.LoadFromFile(FullDtxPath + 'JVCL3_FullHelpTemplate.dox');
    FileSectionCntIndex := Template.IndexOf('[Source Files]');
    if FileSectionCntIndex < 0 then
      raise Exception.Create('Couldn''t locate [Source Files] section in template file.');
    Inc(FileSectionCntIndex);
    FileNum := GetValue(Template, FileSectionCntIndex);
    for I := 0 to DtxList.Count - 1 do
    begin
      Inform(OnInfo, SBuildMainAction, SBuildMainSubActCaption, DtxList[I].UnitName);
      Template.Insert(FileSectionCntIndex + FileNum + (2 * I),
        'File' + IntToStr(FileNum + (2 * I)) + '=' + DtxList[I].UnitName + '.dtx');
      Template.Insert(FileSectionCntIndex + FileNum + (2 * I) + 1,
        'File' + IntToStr(FileNum + (2 * I) + 1) + '=' + MakeRelative(FullRunPath, FullDtxPath) +
          DtxList[I].UnitName + '.pas');
      IncValue(Template, FileSectionCntIndex, 2);
    end;
    Template.SaveToFile(FullDtxPath + 'JVCL3.dox');
  finally
    Template.Free;
  end;
end;

//===TDtxInfos======================================================================================

function TDtxInfos.GetItems(Index: Integer): TDtxInfo;
begin
  Result := TDtxInfo(inherited Items[Index]);
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

procedure TDtxInfos.RemoveUnit(Index: Integer);
begin
  Items[Index].Free;
  Delete(Index);
end;

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

function TDtxInfos.CountCompleted: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].State = dusCompleted then
      Inc(Result);
end;

function TDtxInfos.CountLocked: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].State = dusLocked then
      Inc(Result);
end;

function TDtxInfos.IndexOfUnit(Name: string): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and not AnsiSameText(Items[Result].UnitName, Name) do
    Dec(Result);
end;

procedure TDtxInfos.RemoveUnknownStates;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if Items[I].State = dusUnknown then
      Delete(I);
  end;
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

//===TDtxInfo=======================================================================================

function TDtxInfo.GetUnitName: string;
begin
  Result := FName;
end;

function TDtxInfo.GetState: TDtxUnitState;
begin
  Result := FState;
end;

function TDtxInfo.GetPackage: TDtxPackage;
begin
  Result := FPackage;
end;

procedure TDtxInfo.SetState(Value: TDtxUnitState);
begin
  FState := Value;
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

//===TDtxPackages===================================================================================

function TDtxPackages.GetItems(Index: Integer): TDtxPackage;
begin
  Result := TDtxPackage(inherited Items[Index]);
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

function TDtxPackages.CountEmpty: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if Items[I].DtxCount = 0 then
      Inc(Result);
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
      Delete(I);
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

//===TDtxPackage====================================================================================

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

procedure TDtxPackage.AddDtx(DtxInfo: TDtxInfo);
begin
  if FDtxList.IndexOf(DtxInfo) < 0 then
    FDtxList.Add(DtxInfo);
end;

procedure TDtxPackage.RemoveDtx(DtxInfo: TDtxInfo);
begin
  FDtxList.Remove(DtxInfo);
end;

constructor TDtxPackage.Create(AName: string);
begin
  inherited Create;
  FName := AName;
  FDtxList := TList.Create;
end;

destructor TDtxPackage.Destroy; 
begin
  FreeAndNil(FDtxList);
  inherited Destroy;
end;

end.
