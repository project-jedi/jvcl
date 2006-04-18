unit ConfigurationBase;

interface

uses
  SysUtils, Classes, DelphiData, ConfigOptions;

type
  TPersonalityOptions = class;

  TConfiguration = class(TPersistent)
  protected
    function GetName: string; virtual; abstract;
    function GetVersionStr: string; virtual; abstract;
  public
    function SupportsPersonality(Personality: TIDEPersonality): Boolean; virtual; abstract;
    function IsUpToDate(IDE: TDelphiIDE): Boolean; virtual; abstract;
    function CreateOptionsFor(Personality: TIDEPersonality): TPersonalityOptions; virtual; abstract;

    property Name: string read GetName;
    property VersionStr: string read GetVersionStr;
  end;

  TCompileOptions = class(TPersistent)
  private
    FDebug: Boolean;
    FBuild: Boolean;
    FDeveloperInstall: Boolean;
    FMapFiles: Boolean;
    FRegisterPackages: Boolean;
    FContinueOnError: Boolean;
    FDetailedCompilation: Boolean;
  published
    property Build: Boolean read FBuild write FBuild; // -B
    property Debug: Boolean read FDebug write FDebug; // compile with -$D and -$D-
    property DeveloperInstall: Boolean read FDeveloperInstall write FDeveloperInstall; // source paths => search paths
    property MapFiles: Boolean read FMapFiles write FMapFiles;
    property RegisterPackages: Boolean read FRegisterPackages write FRegisterPackages;
    property ContinueOnError: Boolean read FContinueOnError write FContinueOnError; // do not stop on errors
    property DetailedCompilation: Boolean read FDetailedCompilation write FDetailedCompilation;
  end;

  TPersonalityOptions = class(TPersistent)
  private
    FBplDirectory: string;
    FDcpDirectory: string;
    FPersonality: TIDEPersonality;
    FInstall: Boolean;
    FWasInstalled: Boolean;
    FTabNames: TStrings;
    FHppDirectory: string;
    FCompileOptions: TCompileOptions;
  protected
    procedure SetWasInstalled(Value: Boolean);
    procedure SetInstall(const Value: Boolean); virtual;
  public
    constructor Create(APersonality: TIDEPersonality);
    destructor Destroy; override;

    function GetTabOptions(Index: Integer): TOptionList; virtual;
    property TabNames: TStrings read FTabNames;

    property Personality: TIDEPersonality read FPersonality;
    property Install: Boolean read FInstall write SetInstall;
    property WasInstalled: Boolean read FWasInstalled;
  published
    property BplDirectory: string read FBplDirectory write FBplDirectory;
    property DcpDirectory: string read FDcpDirectory write FDcpDirectory;
    property HppDirectory: string read FHppDirectory write FHppDirectory;

    property CompileOptions: TCompileOptions read FCompileOptions;
  end;

var
  Configuration: TConfiguration = nil;

implementation

uses
  InstallerConsts;

{ TPersonalityOptions }

constructor TPersonalityOptions.Create(APersonality: TIDEPersonality);
begin
  inherited Create;
  FTabNames := TStringList.Create;
  TStringList(FTabNames).Duplicates := dupError;
  FPersonality := APersonality;
  FBplDirectory := Personality.BplDir;
  FDcpDirectory := Personality.DcpDir;
  FCompileOptions := TCompileOptions.Create;
  FInstall := False;

  TabNames.Add(RsDirectories);
  TabNames.Add(RsCompileOptions);
end;

destructor TPersonalityOptions.Destroy;
begin
  FCompileOptions.Free;
  FTabNames.Free;
  inherited Destroy;
end;

function TPersonalityOptions.GetTabOptions(Index: Integer): TOptionList;
begin
  case Index of
    0: // Directories
      begin
        Result := TOptionList.Create;
        Result.Add(TOptionDirectoryEdit.Create(Self, 'BplDirectory', RsBplDirectoryCaption, RsBplDirectoryHint)); // do not localize
        Result.Add(TOptionDirectoryEdit.Create(Self, 'DcpDirectory', RsDcpDirectoryCaption, RsDcpDirectoryHint)); // do not localize
        {if (Personality is TCppPersonality) or (Personality is TDelphiCppPersonality) then
          Result.Add(TOptionDirectoryEdit.Create(Self, 'HppDirectory', RsHppDirectoryCaption, RsHppDirectoryHint)); // do not localize}
      end;
    1: // Compile Options
      begin
        Result := TOptionList.Create;
        Result.Add(TOptionCheckBox.Create(CompileOptions, 'Build', RsBuildCaption, RsBuildHint)); // do not localize
        Result.Add(TOptionCheckBox.Create(CompileOptions, 'Debug', RsDebugCaption, RsDebugHint)); // do not localize
        Result.Add(TOptionCheckBox.Create(CompileOptions, 'MapFiles', RsMapFilesCaption, RsMapFilesHint)); // do not localize
        Result.Add(TOptionSpacer.Create);
        Result.Add(TOptionCheckBox.Create(CompileOptions, 'DeveloperInstall', RsDeveloperInstallCaption, RsDeveloperInstallHint)); // do not localize
        Result.Add(TOptionCheckBox.Create(CompileOptions, 'RegisterPackages', RsRegisterPackagesCaption, RsRegisterPackagesHint)); // do not localize
        Result.Add(TOptionSpacer.Create);
        Result.Add(TOptionCheckBox.Create(CompileOptions, 'ContinueOnError', RsContinueOnErrorCaption, RsContinueOnErrorHint)); // do not localize
        Result.Add(TOptionCheckBox.Create(CompileOptions, 'DetailedCompilation', RsDetailedCompilationCaption, RsDetailedCompilationHint)); // do not localize
      end;
  else
    Result := nil;
  end;
end;

procedure TPersonalityOptions.SetInstall(const Value: Boolean);
begin
  FInstall := Value;
end;

procedure TPersonalityOptions.SetWasInstalled(Value: Boolean);
begin
  FWasInstalled := Value;
  Install := Value;
end;

initialization

finalization
  FreeAndNil(Configuration);

end.
