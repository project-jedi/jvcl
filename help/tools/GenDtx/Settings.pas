unit Settings;

interface

uses
  Classes;

type
  TOutputType = (otClass, otClassHeader, otConst, otDispInterface,
    otField, otFunction, otFunctionType,
    otHeader, otInterface, otMetaClass, otProcedure, otProcedureType, otProperty, otRecord,
    otResourceString, otSet, otType, otVar);

const
  COutputTypeSection: array[TOutputType] of string = (
    'Class', {otClass}
    'ClassHeader', {otClassHeader}
    'Const', {otConst}
    'DispInterface', {otDispInterface}
    'Field', {otField}
    'Function', {otFunction}
    'FunctionType', {otFunctionType}
    'Header', {otHeader}
    'Interface', {otInterface}
    'Metaclass', {otMetaClass}
    'Procedure', {otProcedure}
    'ProcedureType', {otProcedureType}
    'Property', {otProperty}
    'Record', {otRecord}
    'ResourceString', {otResourceString}
    'Set', {otSet}
    'Type', {otType}
    'Var' {otVar}
    );

type
  TOutputTypeBool = array[TOutputType] of Boolean;
  TOutputTypeStrs = array[TOutputType] of string;
  TOutputTypeStrings = array[TOutputType] of TStringList;

  TSettingsChangeType = (ctDirectory);
  TSettingsChangeTypes = set of TSettingsChangeType;
  TSettingsChangeEvent = procedure(Sender: TObject;
    ChangeType: TSettingsChangeType) of object;
  TUnitStatus = (usCompleted, usIgnored, usGenerated, usOther);

const
  CRegisteredClassesFileName = 'RegisteredClasses.txt';
  CFilesInPackagesFileName = 'Files in packages.txt';
  CIgnoredTokensFileName = 'Ignored tokens.txt';
  CUnitStatusFileName: array[TUnitStatus] of string = (
    'Completed units.txt', 'Ignored units.txt', 'Generated units.txt',
    'Other units.txt');

type
  TSettings = class(TPersistent)
  private
    FGeneratedDtxDir: string;
    FOutputTypeDefaults: TOutputTypeStrs;
    FOutputTypeDesc: TOutputTypeStrings;
    FOutputTypeStrings: TOutputTypeStrings;
    FNiceNameClass: TStringList;
    FNiceNameDesc: TStringList;
    FOverwriteExisting: Boolean;
    FOutputTypeEnabled: TOutputTypeBool;
    FRegisteredClasses: TStrings;

    { Observer }
    FObservers: TList;
    FChangeEvents: TList;
    FUpdateCount: Integer;
    FChanges: TSettingsChangeTypes;

    FDefaultNiceName: string;
    FAcceptCompilerDirectives: TStrings;
    FUnitsStatus: array[TUnitStatus] of TStrings;
    FFilesInPackages: TStrings;
    FIgnoredTokens: TStrings;
    FRealDtxDir: string;
    FPackageDir: string;
    FDesignTimePasDir: string;
    FRunTimePasDir: string;
    FRootDir: string;
    FUseRootDir: Boolean;

    function GetFileName: string;
    function GetNiceName(const AClassName: string): string;
    function GetOutputTypeDefaults(const OutputType: TOutputType): string;
    function GetOutputTypeDesc(const OutputType: TOutputType): TStringList;
    function GetOutputTypeEnabled(const OutputTYpe: TOutputType): Boolean;
    function GetOutputTypeStrings(const OutputType: TOutputType): TStringList;
    function GetUnitsStatus(const AUnitStatus: TUnitStatus): TStrings;
    procedure SetAcceptCompilerDirectives(const Value: TStrings);
    procedure SetDesignTimePasDir(const Value: string);
    procedure SetFilesInPackages(const Value: TStrings);
    procedure SetGeneratedDtxDir(const Value: string);
    procedure SetOutputTypeDefaults(const OutputType: TOutputType; const Value: string);
    procedure SetOutputTypeEnabled(const OutputTYpe: TOutputType; const Value: Boolean);
    procedure SetOverwriteExisting(const Value: Boolean);
    procedure SetPackageDir(const Value: string);
    procedure SetRealDtxDir(const Value: string);
    procedure SetRegisteredClasses(const Value: TStrings);
    procedure SetRootDir(const Value: string);
    procedure SetRunTimePasDir(const Value: string);
    procedure SetUnitsStatus(const AUnitStatus: TUnitStatus; const Value: TStrings);
    procedure SetUseRootDir(const Value: Boolean);
  protected
    procedure DoEvent(ChangeType: TSettingsChangeType);
    procedure Changed;

    procedure DoLoad;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    class function Instance: TSettings;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure LoadAll;
    procedure LoadSettings;
    procedure LoadRegisteredClasses;
    procedure LoadUnitStatus(const AUnitStatus: TUnitStatus);
    procedure LoadUnitStatusAll;
    procedure LoadFilesInPackages;
    procedure LoadIgnoredTokens;
    procedure SaveAll;
    procedure SaveSettings;
    procedure SaveRegisteredClasses;
    procedure SaveUnitStatus(const AUnitStatus: TUnitStatus);
    procedure SaveUnitStatusAll;
    procedure SaveFilesInPackages;
    procedure SaveIgnoredTokens;

    procedure RegisterObserver(Observer: TObject; Event: TSettingsChangeEvent = nil); virtual;
    procedure UnRegisterObserver(Observer: TObject); virtual;

    procedure Reset;
    procedure Assign(Source: TPersistent); override;

    function FileNameToPackage(const AFileName: string): string;
    function IsRegisteredClass(const S: string): Boolean;
    procedure AddToUnitStatus(const AUnitStatus: TUnitStatus; const AFileName: string);
    procedure AddToIgnoreTokenList(const AUnit, AToken: string);
    function IsUnitFrom(const AUnitStatus: TUnitStatus; const AFileName: string): Boolean;
    function OnIgnoreTokenList(const AUnit, AToken: string): Boolean;

    property RegisteredClasses: TStrings read FRegisteredClasses write SetRegisteredClasses;
    property FilesInPackages: TStrings read FFilesInPackages write SetFilesInPackages;

    property RunTimePasDir: string read FRunTimePasDir write SetRunTimePasDir;
    property DesignTimePasDir: string read FDesignTimePasDir write SetDesignTimePasDir;
    property PackageDir: string read FPackageDir write SetPackageDir;
    property GeneratedDtxDir: string read FGeneratedDtxDir write SetGeneratedDtxDir;
    property RealDtxDir: string read FRealDtxDir write SetRealDtxDir;
    property RootDir: string read FRootDir write SetRootDir;

    property OverwriteExisting: Boolean read FOverwriteExisting write SetOverwriteExisting;
    property UseRootDir: Boolean read FUseRootDir write SetUseRootDir;
    property OutputTypeDefaults[const OutputType: TOutputType]: string read
    GetOutputTypeDefaults write SetOutputTypeDefaults;
    property OutputTypeDesc[const OutputType: TOutputType]: TStringList read
    GetOutputTypeDesc;
    property OutputTypeStrings[const OutputType: TOutputType]: TStringList read
    GetOutputTypeStrings;
    property OutputTypeEnabled[const OutputTYpe: TOutputType]: Boolean read
    GetOutputTypeEnabled write SetOutputTypeEnabled;

    property NiceNameClass: TStringList read FNiceNameClass;
    property NiceNameDesc: TStringList read FNiceNameDesc;
    property DefaultNiceName: string read FDefaultNiceName write
      FDefaultNiceName;
    property AcceptCompilerDirectives: TStrings read FAcceptCompilerDirectives write
      SetAcceptCompilerDirectives;
    property UnitsStatus[const AUnitStatus: TUnitStatus]: TStrings read GetUnitsStatus write SetUnitsStatus;

    property NiceName[const AClassName: string]: string read GetNiceName;

    property FileName: string read GetFileName;
  end;

implementation

uses
  SysUtils, Forms, IniFiles;

{ TSettings }

procedure TSettings.AddToIgnoreTokenList(const AUnit, AToken: string);
const
  COk = ['a'..'z', 'A'..'Z', '_', '0'..'9', '.', '@'];
var
  DotPos: Integer;
  S: string;
  I: Integer;
  LToken: string;
begin
  if (Length(AToken) < 3) or (AToken[1] <> '@') or (AToken[2] <> '@') then
    Exit;

  if AUnit = '' then
    Exit;

  I := 1;
  DotPos := -1;
  while (I < Length(AToken)) and (AToken[I] in COk) do
  begin
    if (AToken[I] = '.') and (DotPos < 0) then
      DotPos := I;
    Inc(I);
  end;

  LToken := Copy(AToken, 1, I);

  if DotPos < 0 then
  begin
    S := Format('%s=%s', [AUnit, LToken]);
    I := FIgnoredTokens.Add(S) + 1;
    while (I < FIgnoredTokens.Count - 1) and
      (StrLIComp(PChar(S), PChar(FIgnoredTokens[I]), Length(S)) = 0) do

      FIgnoredTokens.Delete(I);
  end
  else
  begin
    S := Copy(LToken, 1, DotPos - 1);
    if FIgnoredTokens.IndexOf(Format('%s=%s', [AUnit, S])) >= 0 then
      Exit;

    FIgnoredTokens.Add(Format('%s=%s', [AUnit, LToken]));
  end;

  SaveIgnoredTokens;
end;

procedure TSettings.AddToUnitStatus(const AUnitStatus: TUnitStatus; const AFileName: string);
var
  LUnitStatus: TUnitStatus;
  I: Integer;
  LFileName: string;
begin
  LFileName := ChangeFileExt(AFileName, '');

  for LUnitStatus := Low(TUnitStatus) to High(TUnitStatus) do
    if LUnitStatus = AUnitStatus then
      UnitsStatus[LUnitStatus].Add(LFileName)
    else
    begin
      I := UnitsStatus[LUnitStatus].IndexOf(LFileName);
      if I >= 0 then
        UnitsStatus[LUnitStatus].Delete(I);
    end;
end;

procedure TSettings.Assign(Source: TPersistent);
var
  OutputType: TOutputType;
  UnitStatus: TUnitStatus;
begin
  if Source is TSettings then
  begin
    BeginUpdate;
    try
      RunTimePasDir := TSettings(Source).RunTimePasDir;
      DesignTimePasDir := TSettings(Source).DesignTimePasDir;
      GeneratedDtxDir := TSettings(Source).GeneratedDtxDir;
      RealDtxDir := TSettings(Source).RealDtxDir;
      PackageDir := TSettings(Source).PackageDir;
      RootDir := TSettings(Source).RootDir;

      OverwriteExisting := TSettings(Source).OverwriteExisting;
      UseRootDir := TSEttings(Source).UseRootDir;
      for OutputType := Low(TOutputType) to High(TOutputType) do
      begin
        OutputTypeDefaults[OutputType] :=
          TSettings(Source).OutputTypeDefaults[OutputType];
        OutputTypeDesc[OutputType].Assign(TSettings(Source).OutputTypeDesc[OutputType]);
        OutputTypeStrings[OutputType].Assign(TSettings(Source).OutputTypeStrings[OutputType]);
        OutputTypeEnabled[OutputType] :=
          TSettings(Source).OutputTypeEnabled[OutputType];
      end;
      FNiceNameClass.Assign(TSettings(Source).FNiceNameClass);
      FNiceNameDesc.Assign(TSettings(Source).FNiceNameDesc);
      FRegisteredClasses.Assign(TSettings(Source).FRegisteredClasses);
      for UnitStatus := Low(TUnitStatus) to High(TUnitStatus) do
        FUnitsStatus[UnitStatus].Assign(TSettings(Source).FUnitsStatus[UnitStatus]);

      FDefaultNiceName := TSettings(Source).FDefaultNiceName;
      FAcceptCompilerDirectives.Assign(TSettings(Source).FAcceptCompilerDirectives);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TSettings.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSettings.Changed;
var
  ChangeType: TSettingsChangeType;
begin
  for ChangeType := Low(TSettingsChangeType) to High(TSettingsChangeType) do
    if ChangeType in FChanges then
      DoEvent(ChangeType);
  FChanges := [];
end;

constructor TSettings.Create;
var
  OutputType: TOutputType;
  UnitStatus: TUnitStatus;
begin
  inherited Create;

  FObservers := TList.Create;
  FChangeEvents := TList.Create;
  for OutputType := Low(TOutputType) to High(TOutputType) do
  begin
    FOutputTypeDesc[OutputType] := TStringList.Create;
    FOutputTypeStrings[OutputType] := TStringList.Create;
  end;
  FNiceNameClass := TStringList.Create;
  FNiceNameDesc := TStringList.Create;
  for UnitStatus := Low(TUnitStatus) to High(TUnitStatus) do
  begin
    FUnitsStatus[UnitStatus] := TStringList.Create;
    with FUnitsStatus[UnitStatus] as TStringList do
    begin
      Sorted := True;
      Duplicates := dupIgnore;
      CaseSensitive := False;
    end;
  end;

  FRegisteredClasses := TStringList.Create;
  with FRegisteredClasses as TStringList do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
    CaseSensitive := False;
  end;

  FAcceptCompilerDirectives := TStringList.Create;
  with FAcceptCompilerDirectives as TStringList do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
  end;

  FFilesInPackages := TStringList.Create;
  with FFilesInPackages as TStringList do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
  end;

  FIgnoredTokens := TStringList.Create;
  with FIgnoredTokens as TStringList do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
  end;
end;

destructor TSettings.Destroy;
var
  OutputType: TOutputType;
  UnitStatus: TUnitStatus;
begin
  FreeAndNil(FChangeEvents);
  FreeAndNil(FObservers);
  for OutputType := Low(TOutputType) to High(TOutputType) do
  begin
    FOutputTypeDesc[OutputType].Free;
    FOutputTypeStrings[OutputType].Free;
  end;
  FNiceNameClass.Free;
  FNiceNameDesc.Free;
  for UnitStatus := Low(TUnitStatus) to High(TUnitStatus) do
    FUnitsStatus[UnitStatus].Free;
  FRegisteredClasses.Free;
  FAcceptCompilerDirectives.Free;
  FFilesInPackages.Free;
  FIgnoredTokens.Free;
  inherited Destroy;
end;

procedure TSettings.DoEvent(ChangeType: TSettingsChangeType);
var
  I: Integer;
  ChangeEvent: TSettingsChangeEvent;
begin
  if FUpdateCount > 0 then
    Include(FChanges, ChangeType)
  else
    for I := 0 to FObservers.Count - 1 do
      if FChangeEvents[I] <> nil then
      begin
        TMethod(ChangeEvent).Code := FChangeEvents[I];
        TMethod(ChangeEvent).Data := FObservers[I];
        ChangeEvent(Self, ChangeType);
      end;
end;

procedure TSettings.DoLoad;
var
  IniFile: TMemIniFile;
  Stream: TStringStream;

  function ReadBinaryString(const Section, Ident: string): string;
  begin
    Stream.Size := 0;
    IniFile.ReadBinaryStream(Section, Ident, Stream);
    Stream.Position := 0;

    Result := Stream.ReadString(Stream.Size);
  end;

  procedure ReadList(const Section, CountIdent, Prefix: string; List: TStrings);
  var
    I, Count: Integer;
    S: string;
  begin
    Count := IniFile.ReadInteger(Section, CountIdent, 0);
    for I := 0 to Count - 1 do
    begin
      S := IniFile.ReadString(Section, Prefix + IntToStr(I), '');
      if S > '' then
        List.Add(S);
    end;
  end;

  procedure ReadBinaryList(const Section, CountIdent, Prefix: string; List: TStrings);
  var
    I, Count: Integer;
    S: string;
  begin
    Count := IniFile.ReadInteger(Section, CountIdent, 0);
    for I := 0 to Count - 1 do
    begin
      S := ReadBinaryString(Section, Prefix + IntToStr(I));
      if S > '' then
        List.Add(S);
    end;
  end;
var
  OutputType: TOutputType;
  Section: string;
begin
  IniFile := TMemIniFile.Create(FileName);
  Stream := TStringStream.Create('');
  try
    RunTimePasDir := IniFile.ReadString('Directories', 'PasDir', '');
    DesignTimePasDir := IniFile.ReadString('Directories', 'DesignTimePasDir', '');
    GeneratedDtxDir := IniFile.ReadString('Directories', 'GeneratedDtxDir', '');
    RealDtxDir := IniFile.ReadString('Directories', 'RealDtxDir', '');
    PackageDir := IniFile.ReadString('Directories', 'PackageDir', '');
    RootDir := IniFile.ReadString('Directories', 'RootDir', '');

    UseRootDir := IniFile.ReadBool('Directories', 'UseRootDir', UseRootDir);

    OverwriteExisting := IniFile.ReadBool('Options', 'OverwriteExisting', OverwriteExisting);
    DefaultNiceName := IniFile.ReadString('Options', 'DefaultNiceName', DefaultNiceName);

    for OutputType := Low(TOutputType) to High(TOutputType) do
    begin
      Section := COutputTypeSection[OutputType];

      FOutputTypeDefaults[OutputType] := ReadBinaryString(Section, 'Default');
      ReadList(Section, 'DescCount', 'Desc', FOutputTypeDesc[OutputType]);
      ReadBinaryList(Section, 'StringCount', 'String', FOutputTypeStrings[OutputType]);
      FOutputTypeEnabled[OutputType] := IniFile.ReadBool(Section, 'Enabled', FOutputTypeEnabled[OutputType]);
    end;

    {ReadList('RegisteredClasses', 'Count', 'Item', FRegisteredClasses);}
    {ReadList('IgnoredUnits', 'Count', 'Item', FIgnoredUnits);}
    {ReadList('DocumentedUnits', 'Count', 'Item', FDocumentedUnits);}
    ReadList('NiceNames', 'Count', 'Item', FNiceNameClass);
    ReadList('NiceNameDescs', 'Count', 'Item', FNiceNameDesc);
    ReadList('AcceptCompilerDirectives', 'Count', 'Item', FAcceptCompilerDirectives);
  finally
    IniFile.Free;
    Stream.Free;
  end;
end;

procedure TSettings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Changed;
end;

function TSettings.FileNameToPackage(const AFileName: string): string;
begin
  Result := FFilesInPackages.Values[ChangeFileExt(AFileName, '')];
  if Result = '' then
    Result := '??';
end;

function TSettings.GetFileName: string;
begin
  Result := ChangeFileExt(Application.ExeName, '.INI')
end;

function TSettings.GetNiceName(const AClassName: string): string;
var
  Index: Integer;
begin
  Index := FNiceNameClass.IndexOf(UpperCase(AClassName));
  if Index < 0 then
    Result := DefaultNiceName
  else
    Result := FNiceNameDesc[Index];
end;

function TSettings.GetOutputTypeDefaults(
  const OutputType: TOutputType): string;
begin
  Result := FOutputTypeDefaults[OutputType];
end;

function TSettings.GetOutputTypeDesc(
  const OutputType: TOutputType): TStringList;
begin
  Result := FOutputTypeDesc[OutputType];
end;

function TSettings.GetOutputTypeEnabled(
  const OutputTYpe: TOutputType): Boolean;
begin
  Result := FOutputTypeEnabled[OutputType];
end;

function TSettings.GetOutputTypeStrings(
  const OutputType: TOutputType): TStringList;
begin
  Result := FOutputTypeStrings[OutputType];
end;

function TSettings.GetUnitsStatus(
  const AUnitStatus: TUnitStatus): TStrings;
begin
  Result := FUnitsStatus[AUnitStatus];
end;

var
  GInstance: TSettings = nil;

class function TSettings.Instance: TSettings;
begin
  if not Assigned(GInstance) then
  begin
    GInstance := TSettings.Create;
    GInstance.LoadAll;
  end;
  Result := GInstance;
end;

function TSettings.IsRegisteredClass(const S: string): Boolean;
begin
  Result := FRegisteredClasses.IndexOf(S) >= 0;
end;

function TSettings.IsUnitFrom(const AUnitStatus: TUnitStatus;
  const AFileName: string): Boolean;
begin
  Result := UnitsStatus[AUnitStatus].IndexOf(ChangeFileExt(AFileName, '')) >= 0
end;

procedure TSettings.LoadAll;
begin
  LoadSettings;
  LoadRegisteredClasses;
  LoadUnitStatusAll;
  LoadFilesInPackages;
  LoadIgnoredTokens;
end;

procedure TSettings.LoadFilesInPackages;
var
  LFileName: string;
begin
  LFileName := ExtractFilePath(Application.ExeName) + CFilesInPackagesFileName;
  if FileExists(LFileName) then
    FFilesInPackages.LoadFromFile(LFileName);
end;

procedure TSettings.LoadIgnoredTokens;
var
  LFileName: string;
begin
  LFileName := ExtractFilePath(Application.ExeName) + CIgnoredTokensFileName;
  if FileExists(LFileName) then
    FIgnoredTokens.LoadFromFile(LFileName);
end;

procedure TSettings.LoadRegisteredClasses;
begin
  FRegisteredClasses.LoadFromFile(CRegisteredClassesFileName);
end;

procedure TSettings.LoadSettings;
begin
  Reset;
  if FileExists(FileName) then
    DoLoad;
end;

procedure TSettings.LoadUnitStatus(const AUnitStatus: TUnitStatus);
var
  LFileName: string;
begin
  LFileName := ExtractFilePath(Application.ExeName) + CUnitStatusFileName[AUnitStatus];
  if FileExists(LFileName) then
    FUnitsStatus[AUnitStatus].LoadFromFile(LFileName);
end;

procedure TSettings.LoadUnitStatusAll;
var
  UnitStatus: TUnitStatus;
begin
  for UnitStatus := Low(TUnitStatus) to High(TUnitStatus) do
    LoadUnitStatus(UnitStatus);
end;

function TSettings.OnIgnoreTokenList(const AUnit, AToken: string): Boolean;
var
  P: Integer;
begin
  Result := FIgnoredTokens.IndexOf(Format('%s=%s', [AUnit, AToken])) >= 0;
  if Result then
    Exit;

  P := Pos('.', AToken);
  if P < 0 then
    Exit;

  Result := FIgnoredTokens.IndexOf(Format('%s=%s', [AUnit, Copy(AToken, 1, P - 1)])) >= 0;
end;

procedure TSettings.RegisterObserver(Observer: TObject;
  Event: TSettingsChangeEvent);
begin
  FObservers.Add(Observer);
  FChangeEvents.Add(TMethod(Event).Code);
end;

procedure TSettings.Reset;
var
  OutputType: TOutputType;
  UnitStatus: TUnitStatus;
begin
  BeginUpdate;
  try
    FRunTimePasDir := '';
    FDesignTimePasDir := '';
    FGeneratedDtxDir := '';
    FRealDtxDir := '';
    FPackageDir := '';
    FUseRootDir := True;
    FRootDir := '';

    FOverwriteExisting := False;
    for OutputType := Low(TOutputType) to High(TOutputType) do
    begin
      FOutputTypeDefaults[OutputType] := '';
      FOutputTypeDesc[OutputType].Clear;
      FOutputTypeStrings[OutputType].Clear;
      FOutputTypeEnabled[OutputType] := True;
    end;
    FNiceNameClass.Clear;
    FNiceNameDesc.Clear;
    {FIgnoredUnits.Clear;}
    {FDocumentedUnits.Clear;}
    FRegisteredClasses.Clear;
    for UnitStatus := Low(TUnitStatus) to High(TUnitStatus) do
      FUnitsStatus[UnitStatus].Clear;
    FAcceptCompilerDirectives.Clear;
    FDefaultNiceName := '<xxx>';
    FFilesInPackages.Clear;
    FIgnoredTokens.Clear;
  finally
    EndUpdate;
  end;
end;

procedure TSettings.SaveAll;
begin
  SaveSettings;
  SaveUnitStatusAll;
  SaveRegisteredClasses;
end;

procedure TSettings.SaveFilesInPackages;
begin
  FFilesInPackages.SaveToFile(
    ExtractFilePath(Application.ExeName) + CFilesInPackagesFileName);
end;

procedure TSettings.SaveIgnoredTokens;
begin
  FIgnoredTokens.SaveToFile(
    ExtractFilePath(Application.ExeName) + CIgnoredTokensFileName);
end;

procedure TSettings.SaveRegisteredClasses;
begin
  FRegisteredClasses.SaveToFile(
    ExtractFilePath(Application.ExeName) + CRegisteredClassesFileName);
end;

procedure TSettings.SaveSettings;
var
  IniFile: TMemIniFile;
  Stream: TStringStream;

  procedure WriteBinaryString(const Section, Ident, Value: string);
  begin
    Stream.Size := 0;
    Stream.WriteString(Value);
    Stream.Position := 0;

    IniFile.WriteBinaryStream(Section, Ident, Stream);
  end;

  procedure WriteList(const Section, CountIdent, Prefix: string; List: TStrings);
  var
    I: Integer;
  begin
    IniFile.WriteInteger(Section, CountIdent, List.Count);
    for I := 0 to List.Count - 1 do
      IniFile.WriteString(Section, Prefix + IntToStr(I), List[I]);
  end;

  procedure WriteBinaryList(const Section, CountIdent, Prefix: string; List: TStrings);
  var
    I: Integer;
  begin
    IniFile.WriteInteger(Section, CountIdent, List.Count);
    for I := 0 to List.Count - 1 do
      WriteBinaryString(Section, Prefix + IntToStr(I), List[I]);
  end;
var
  OutputType: TOutputType;
  Section: string;
begin
  IniFile := TMemIniFile.Create(ChangeFileExt(FileName, '.INI'));
  Stream := TStringStream.Create('');
  try
    IniFile.WriteString('Directories', 'PasDir', RunTimePasDir);
    IniFile.WriteString('Directories', 'DesignTimePasDir', DesignTimePasDir);
    IniFile.WriteString('Directories', 'GeneratedDtxDir', GeneratedDtxDir);
    IniFile.WriteString('Directories', 'RealDtxDir', RealDtxDir);
    IniFile.WriteString('Directories', 'PackageDir', PackageDir);
    IniFile.WriteString('Directories', 'RootDir', RootDir);
    IniFile.WriteBool('Directories', 'UseRootDir', UseRootDir);

    IniFile.WriteBool('Options', 'OverwriteExisting', OverwriteExisting);
    IniFile.WriteString('Options', 'DefaultNiceName', DefaultNiceName);

    for OutputType := Low(TOutputType) to High(TOutputType) do
    begin
      Section := COutputTypeSection[OutputType];

      WriteBinaryString(Section, 'Default', FOutputTypeDefaults[OutputType]);
      WriteList(Section, 'DescCount', 'Desc', FOutputTypeDesc[OutputType]);
      WriteBinaryList(Section, 'StringCount', 'String', FOutputTypeStrings[OutputType]);
      IniFile.WriteBool(Section, 'Enabled', FOutputTypeEnabled[OutputType]);
    end;

    WriteList('NiceNames', 'Count', 'Item', FNiceNameClass);
    WriteList('NiceNameDescs', 'Count', 'Item', FNiceNameDesc);
    WriteList('AcceptCompilerDirectives', 'Count', 'Item', FAcceptCompilerDirectives);

    IniFile.UpdateFile;
  finally
    IniFile.Free;
    Stream.Free;
  end;
end;

procedure TSettings.SaveUnitStatus(const AUnitStatus: TUnitStatus);
begin
  FUnitsStatus[AUnitStatus].SaveToFile(
    ExtractFilePath(Application.ExeName) + CUnitStatusFileName[AUnitStatus]);
end;

procedure TSettings.SaveUnitStatusAll;
var
  UnitStatus: TUnitStatus;
begin
  for UnitStatus := Low(TUnitStatus) to High(TUnitStatus) do
    SaveUnitStatus(UnitStatus);
end;

procedure TSettings.SetAcceptCompilerDirectives(const Value: TStrings);
begin
  FAcceptCompilerDirectives.Assign(Value);
end;

procedure TSettings.SetDesignTimePasDir(const Value: string);
begin
  if Value <> FDesignTimePasDir then
  begin
    FDesignTimePasDir := Value;
    DoEvent(ctDirectory);
  end;
end;

procedure TSettings.SetFilesInPackages(const Value: TStrings);
begin
  FFilesInPackages.Assign(Value);
end;

procedure TSettings.SetGeneratedDtxDir(const Value: string);
begin
  if Value <> FGeneratedDtxDir then
  begin
    FGeneratedDtxDir := Value;
    DoEvent(ctDirectory);
  end;
end;

procedure TSettings.SetOutputTypeDefaults(const OutputType: TOutputType;
  const Value: string);
begin
  FOutputTypeDefaults[OutputType] := Value;
end;

procedure TSettings.SetOutputTypeEnabled(const OutputTYpe: TOutputType;
  const Value: Boolean);
begin
  FOutputTypeEnabled[OutputType] := Value;
end;

procedure TSettings.SetOverwriteExisting(const Value: Boolean);
begin
  FOverwriteExisting := Value;
end;

procedure TSettings.SetPackageDir(const Value: string);
begin
  if Value <> FPackageDir then
  begin
    FPackageDir := Value;
    DoEvent(ctDirectory);
  end;
end;

procedure TSettings.SetRealDtxDir(const Value: string);
begin
  if Value <> FRealDtxDir then
  begin
    FRealDtxDir := Value;
    DoEvent(ctDirectory);
  end;
end;

procedure TSettings.SetRegisteredClasses(const Value: TStrings);
begin
  FRegisteredClasses.Assign(Value);
end;

procedure TSettings.SetRootDir(const Value: string);
begin
  if Value <> FRootDir then
  begin
    FRootDir := Value;
    DoEvent(ctDirectory);
  end;
end;

procedure TSettings.SetRunTimePasDir(const Value: string);
begin
  if Value <> FRunTimePasDir then
  begin
    FRunTimePasDir := Value;
    DoEvent(ctDirectory);
  end;
end;

procedure TSettings.SetUnitsStatus(const AUnitStatus: TUnitStatus;
  const Value: TStrings);
begin
  FUnitsStatus[AUnitStatus].Assign(Value);
end;

procedure TSettings.SetUseRootDir(const Value: Boolean);
begin
  FUseRootDir := Value;
end;

procedure TSettings.UnRegisterObserver(Observer: TObject);
var
  Index: Integer;
begin
  Index := FObservers.IndexOf(Observer);
  if Index <> -1 then
  begin
    FObservers.Delete(Index);
    FChangeEvents.Delete(Index);
  end;
end;

initialization
finalization
  FreeAndNil(GInstance);
end.

