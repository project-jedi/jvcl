unit PasFilesStripper;

interface

uses
  Classes, JVCLHelpUtils;

type
  TPasFilesStripper = class(TTask)
  private
    FSourcePasDir: string;
    FDestPasDir: string;
    FSpecificFiles: TStringList;
    FIncludeSourceSubDirs: Boolean;
    function ProcessFile(const AFileName: string): Boolean;
    function GetSpecificFiles: TStrings;
    procedure SetSpecificFiles(Value: TStrings);
    procedure CollectFiles(AFiles: TStrings);
  protected
    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    constructor Create(ATaskManager: ITaskManager); override;
    destructor Destroy; override;
    property SourcePasDir: string read FSourcePasDir write FSourcePasDir;
    property IncludeSourceSubDirs: Boolean read FIncludeSourceSubDirs write FIncludeSourceSubDirs;
    property DestPasDir: string read FDestPasDir write FDestPasDir;
    property SpecificFiles: TStrings read GetSpecificFiles write SetSpecificFiles;
  end;

implementation

uses
  AdjVclClxCvt, SysUtils;

type
  THelpConverter = class(TVCLConverter)
  protected
    procedure InitUnitReplaceList; override;
    function ChangeFileName(const Name: string): string; override;
    procedure TranslateUnit(var AName: string); override;
    procedure TranslateInc(var AName: string); override;
    procedure TranslateResource(var AName: string); override;
    procedure BeforeSave(const Filename: string; Lines: TStrings); override;
  end;

const
  cList: array[0..67] of string = (
    {0}'!VCL',
    '!JVCL_REGISTER_GLOBAL_DESIGNEDITORS',
    'VisualCLX',
    '!USEJVCL',
    '!JVCLThemesEnabled',
    {5}'!JVCLThemesEnabledD56',
    'BCB',
    'BCB1',
    'BCB2',
    'BCB3',
    {10}'BCB4',
    'BCB5',
    'BCB6',
    'BCB7',
    '!BCB1_UP',
    {15}'!BCB2_UP',
    '!BCB3_UP',
    '!BCB4_UP',
    '!BCB5_UP',
    '!BCB6_UP',
    {20}'!BCB7_UP',
    'COMPILER1',
    'COMPILER2',
    'COMPILER3',
    'COMPILER4',
    {25}'COMPILER5',
    'COMPILER6',
    '!COMPILER7',
    'COMPILER8',
    '!COMPILER1_UP',
    {30}'!COMPILER2_UP',
    '!COMPILER3_UP',
    '!COMPILER4_UP',
    '!COMPILER5_UP',
    '!COMPILER6_UP',
    {35}'~COMPILER7_UP',
    'COMPILER8_UP',
    'DELPHI1',
    'DELPHI2',
    'DELPHI3',
    {40}'DELPHI4',
    'DELPHI5',
    'DELPHI6',
    '!DELPHI7',
    'DELPHI8',
    {45}'!DELPHI1_UP',
    '!DELPHI2_UP',
    '!DELPHI3_UP',
    '!DELPHI4_UP',
    '!DELPHI5_UP',
    {50}'!DELPHI6_UP',
    '!DELPHI7_UP',
    'DELPHI8_UP',
    '~CLR',
    'WIN16',
    {55}'~WIN32',
    '~Win32API',
    'UNITVERSIONING',
    '~KEEP_DEPRECATED',
    '~UNIX',
    {60}'~MSWINDOWS',
    '~MATH_EXTENDED_PRECISION',
    '~MATH_DOUBLE_PRECISION',
    '~MATH_SINGLE_PRECISION',
    '~RTL140_UP',
    {65}'~FPC',
    '~CPU386',
    '~CRCINIT'
    );

//=== { THelpConverter } =====================================================

procedure THelpConverter.BeforeSave(const Filename: string;
  Lines: TStrings);
begin
  inherited;

end;

function THelpConverter.ChangeFileName(const Name: string): string;
begin
  Result := Name;
end;

procedure THelpConverter.InitUnitReplaceList;
var
  Lines: TStrings;
  i: Integer;
begin
  //  inherited InitUnitReplaceList; // load VCL conversions
  //  UnitReplaceList.AddFromIni(IniDirectory + PathDelim + 'convertqvcl.ini');

  Lines := TStringList.Create;
  try
    for I := 0 to High(cList) do
      Lines.Add(cList[i]);
    //    Lines.LoadFromFile(IniDirectory + PathDelim + 'jvclremconditions.ini');
    for i := 0 to Lines.Count - 1 do
      //      if not IsEmptyStr(Lines[i]) then
      RemoveConditions.Add(Lines[i]);
    //    for i := 0 to FModel.IgnoredClxReplacements.Count - 1 do
    //      IgnoreUnits.Add(ChangeFileExt(FModel.IgnoredClxReplacements[i], ''));
  finally
    Lines.Free;
  end;
end;

procedure THelpConverter.TranslateInc(var AName: string);
begin
  { Nothing }
end;

procedure THelpConverter.TranslateResource(var AName: string);
begin
  { Nothing }
end;

procedure THelpConverter.TranslateUnit(var AName: string);
begin
  { Nothing }
end;

//=== { TPasFilesStripper } ==================================================

function TPasFilesStripper.CanStart: Boolean;
begin
  Result := CheckDir(SourcePasDir) and CheckDir(DestPasDir);
end;

procedure TPasFilesStripper.CollectFiles(AFiles: TStrings);
var
  I: Integer;
  AFileName: string;
begin
  GetAllFilesFrom(SourcePasDir, '*.pas', AFiles, IncludeSourceSubDirs);

  if SpecificFiles.Count = 0 then
    Exit;

  for I := AFiles.Count - 1 downto 0 do
  begin
    AFileName := ExtractFileName(ChangeFileExt(AFiles[i], ''));
    if SpecificFiles.IndexOf(AFileName) < 0 then
      AFiles.Delete(I);
  end;
end;

constructor TPasFilesStripper.Create(ATaskManager: ITaskManager);
begin
  inherited Create(ATaskManager);
  FSpecificFiles := TStringList.Create;
  FSpecificFiles.Sorted := True;
  FSpecificFiles.Duplicates := dupIgnore;
  IncludeSourceSubDirs := False;
end;

destructor TPasFilesStripper.Destroy;
begin
  FSpecificFiles.Free;
  inherited Destroy;
end;

function TPasFilesStripper.DoExecute: Boolean;
var
  JVCL3RunPasFiles: TStringList;
  I: Integer;
begin
  Result := True;

  JVCL3RunPasFiles := TStringList.Create;
  try
    JVCL3RunPasFiles.Sorted := True;

    CollectFiles(JVCL3RunPasFiles);

    StatusMsg('Processing run time files');
    for I := 0 to JVCL3RunPasFiles.Count - 1 do
    begin
      Progress(I, JVCL3RunPasFiles.Count);
      ProcessFile(JVCL3RunPasFiles[I]);
    end;
  finally
    JVCL3RunPasFiles.Free;
  end;
end;

function TPasFilesStripper.GetSpecificFiles: TStrings;
begin
  Result := FSpecificFiles;
end;

function TPasFilesStripper.GetTaskDescription: string;
begin
  Result := 'Generating stripped pas files..';
end;

function TPasFilesStripper.ProcessFile(const AFileName: string): Boolean;
var
  JVCLConverter: THelpConverter;
begin
  Result := True;

  JVCLConverter := THelpConverter.Create('');
  try
    JVCLConverter.OutDirectory := DestPasDir;
    JVCLConverter.ReduceConditions := True;
    JVCLConverter.KeepLines := False;
    JVCLConverter.UnixLineBreak := False;
    JVCLConverter.ForceOverwrite := True;
    JVCLConverter.ParsePasFile(AFileName);
  finally
    JVCLConverter.Free;
  end;
end;

procedure TPasFilesStripper.SetSpecificFiles(Value: TStrings);
begin
  FSpecificFiles.Assign(Value);
end;

end.
