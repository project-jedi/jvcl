unit Settings;

interface

uses
  Classes;

type
  TOutputType = (otClass, otClassHeader, otConst, otDispInterface, otFunction,
    otFunctionType,
    otHeader, otInterface, otProcedure, otProcedureType, otProperty, otRecord,
    otResourceString, otSet, otType, otVar);

  TOutputTypeBool = array[TOutputType] of Boolean;
  TOutputTypeStrs = array[TOutputType] of string;
  TOutputTypeStrings = array[TOutputType] of TStringList;

  TSettingsChangeType = (ctInDirectory, ctOutDirectory);
  TSettingsChangeEvent = procedure(Sender: TObject;
    ChangeType: TSettingsChangeType) of object;

  TSettings = class(TPersistent)
  private
    FInDir: string;
    FOutDir: string;
    FOutputTypeDefaults: TOutputTypeStrs;
    FOutputTypeDesc: TOutputTypeStrings;
    FOutputTypeStrings: TOutputTypeStrings;
    FNiceNameClass: TStringList;
    FNiceNameDesc: TStringList;
    FOverwriteExisting: Boolean;
    FOutputTypeEnabled: TOutputTypeBool;
    FIgnoredUnits: TStringList;
    FRegisteredClasses: TStringList;

    { Observer }
    FObservers: TList;
    FChangeEvents: TList;
    FDefaultNiceName: string;
    FAcceptCompilerDirectives: TStrings;

    procedure SetInDir(const Value: string);
    procedure SetOutDir(const Value: string);
    procedure SetOverwriteExisting(const Value: Boolean);
    function GetOutputTypeDefaults(const OutputType: TOutputType): string;
    procedure SetOutputTypeDefaults(const OutputType: TOutputType; const Value: string);
    function GetOutputTypeDesc(const OutputType: TOutputType): TStringList;
    function GetOutputTypeStrings(const OutputType: TOutputType): TStringList;
    function GetNiceName(const AClassName: string): string;
    function GetOutputTypeEnabled(const OutputTYpe: TOutputType): Boolean;
    procedure SetOutputTypeEnabled(const OutputTYpe: TOutputType; const Value: Boolean);
    procedure SetAcceptCompilerDirectives(const Value: TStrings);
    function GetFileName: string;
  protected
    procedure DoEvent(ChangeType: TSettingsChangeType);

    procedure DoLoad;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    class function Instance: TSettings;

    procedure Load;
    procedure Save;

    procedure RegisterObserver(Observer: TObject; Event: TSettingsChangeEvent = nil); virtual;
    procedure UnRegisterObserver(Observer: TObject); virtual;

    procedure Reset;
    procedure Assign(Source: TPersistent); override;

    function IsRegisteredClass(const S: string): Boolean;
    function IsIgnoredUnit(const S: string): Boolean;

    property IgnoredUnits: TStringList read FIgnoredUnits;
    property RegisteredClasses: TStringList read FRegisteredClasses;
    property InDir: string read FInDir write SetInDir;
    property OutDir: string read FOutDir write SetOutDir;
    property OverwriteExisting: Boolean read FOverwriteExisting write
      SetOverwriteExisting;
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

    property NiceName[const AClassName: string]: string read GetNiceName;

    property FileName: string read GetFileName;
  end;

implementation

uses
  SysUtils, Forms, IniFiles;

const
  COutputTypeSection: array[TOutputType] of string = (
    'Class', {otClass}
    'ClassHeader', {otClassHeader}
    'Const', {otConst}
    'DispInterface', {otDispInterface}
    'Function', {otFunction}
    'FunctionType', {otFunctionType}
    'Header', {otHeader}
    'Interface', {otInterface}
    'Procedure', {otProcedure}
    'ProcedureType', {otProcedureType}
    'Property', {otProperty}
    'Record', {otRecord}
    'ResourceString', {otResourceString}
    'Set', {otSet}
    'Type', {otType}
    'Var' {otVar}
    );

  { TSettings }

procedure TSettings.Assign(Source: TPersistent);
var
  OutputType: TOutputType;
begin
  if Source is TSettings then
  begin
    InDir := TSettings(Source).InDir;
    OutDir := TSettings(Source).OutDir;
    OverwriteExisting := TSettings(Source).OverwriteExisting;
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
    FIgnoredUnits.Assign(TSettings(Source).FIgnoredUnits);
    FDefaultNiceName := TSettings(Source).FDefaultNiceName;
    FAcceptCompilerDirectives.Assign(TSettings(Source).FAcceptCompilerDirectives);
  end
  else
    inherited Assign(Source);
end;

constructor TSettings.Create;
var
  OutputType: TOutputType;
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
  FIgnoredUnits := TStringList.Create;
  with FIgnoredUnits do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
    CaseSensitive := False;
  end;

  FRegisteredClasses := TStringList.Create;
  with FRegisteredClasses do
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
end;

destructor TSettings.Destroy;
var
  OutputType: TOutputType;
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
  FIgnoredUnits.Free;
  FRegisteredClasses.Free;
  FAcceptCompilerDirectives.Free;
  inherited Destroy;
end;

procedure TSettings.DoEvent(ChangeType: TSettingsChangeType);
var
  I: Integer;
  ChangeEvent: TSettingsChangeEvent;
begin
  for I := 0 to FObservers.Count - 1 do
    if FChangeEvents[I] <> nil then
    begin
      TMethod(ChangeEvent).Code := FChangeEvents[I];
      TMethod(ChangeEvent).Data := FObservers[I];
      ChangeEvent(Self, ChangeType);
    end;
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

var
  GInstance: TSettings = nil;

class function TSettings.Instance: TSettings;
begin
  if not Assigned(GInstance) then
  begin
    GInstance := TSettings.Create;
    GInstance.Load;
  end;
  Result := GInstance;
end;

function TSettings.IsIgnoredUnit(const S: string): Boolean;
begin
  Result := FIgnoredUnits.IndexOf(S) >= 0;
end;

function TSettings.IsRegisteredClass(const S: string): Boolean;
begin
  Result := FRegisteredClasses.IndexOf(S) >= 0;
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
    InDir := IniFile.ReadString('Directories', 'InDir', '');
    OutDir := IniFile.ReadString('Directories', 'OutDir', '');
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

    ReadList('RegisteredClasses', 'Count', 'Item', FRegisteredClasses);
    ReadList('IgnoredUnits', 'Count', 'Item', FIgnoredUnits);
    ReadList('NiceNames', 'Count', 'Item', FNiceNameClass);
    ReadList('NiceNameDescs', 'Count', 'Item', FNiceNameDesc);
    ReadList('AcceptCompilerDirectives', 'Count', 'Item', FAcceptCompilerDirectives);
  finally
    IniFile.Free;
    Stream.Free;
  end;
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
begin
  FInDir := '';
  FOutDir := '';
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
  FIgnoredUnits.Clear;
  FRegisteredClasses.Clear;
  FAcceptCompilerDirectives.Clear;
  FDefaultNiceName := '<xxx>';
end;

procedure TSettings.Save;
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
    IniFile.WriteString('Directories', 'InDir', InDir);
    IniFile.WriteString('Directories', 'OutDir', OutDir);
    IniFile.WriteBool('Options', 'OverwriteExisting', OverwriteExisting);
    IniFile.WriteString('Options', 'DefaultNiceName', DefaultNiceName);

    for OutputType := Low(TOutputType) to High(TOutputType) do
    begin
      Section := COutputTypeSection[OutputType];

      WriteBinaryString(Section, 'Default', FOutputTypeDefaults[OutputType]);
      WriteList(Section, 'DescCount', 'Desc', FOutputTypeDesc[OutputType]);
      WriteBinaryList(Section, 'StringCount', 'String', FOutputTypeDesc[OutputType]);
      IniFile.WriteBool(Section, 'Enabled', FOutputTypeEnabled[OutputType]);
    end;

    WriteList('RegisteredClasses', 'Count', 'Item', FRegisteredClasses);
    WriteList('IgnoredUnits', 'Count', 'Item', FIgnoredUnits);
    WriteList('NiceNames', 'Count', 'Item', FNiceNameClass);
    WriteList('NiceNameDescs', 'Count', 'Item', FNiceNameDesc);
    WriteList('AcceptCompilerDirectives', 'Count', 'Item', FAcceptCompilerDirectives);

    IniFile.UpdateFile;
  finally
    IniFile.Free;
    Stream.Free;
  end;
end;

procedure TSettings.SetAcceptCompilerDirectives(const Value: TStrings);
begin
  FAcceptCompilerDirectives.Assign(Value);
end;

procedure TSettings.SetInDir(const Value: string);
begin
  if Value <> FInDir then
  begin
    FInDir := Value;
    DoEvent(ctInDirectory);
  end;
end;

procedure TSettings.SetOutDir(const Value: string);
begin
  if Value <> FOutDir then
  begin
    FOutDir := Value;
    DoEvent(ctOutDirectory);
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

procedure TSettings.Load;
begin
  Reset;
  if FileExists(FileName) then
    DoLoad;
end;

initialization
finalization
  FreeAndNil(GInstance);
end.

