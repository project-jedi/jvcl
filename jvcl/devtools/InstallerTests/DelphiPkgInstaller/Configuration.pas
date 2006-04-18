unit Configuration;

interface

uses
  SysUtils, Classes, Contnrs, IniFiles, Registry, Dialogs, DelphiData;

type
  TVersion = (d0, d5, d6, d7, d9, d9net);

const
  VersionNames: array[TVersion] of string = (
    '', 'Delphi 5.0', 'Delphi 6.0', 'Delphi 7.0', 'Delphi 2005', 'Delphi 2005'
  );

type
  TConfig = class(TObject)
  private
    FTitle: string;
    FDefaultInstallDir: string;
    FDefaultBplDir: string;
    FLibraryPaths: TStrings;
    FVersion: TVersion;
    FVersionStr: string;
    FInstallDir: string;
    FBplDir: string;
    FTarget: TCompileTarget;
    FWizardPicture: string;
    FWelcomePicture: string;
    FStartupPicture: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const Filename: string);

    property Title: string read FTitle;
    property DefaultInstallDir: string read FDefaultInstallDir;
    property DefaultBplDir: string read FDefaultBplDir;

    property LibraryPaths: TStrings read FLibraryPaths;
    property Version: TVersion read FVersion;
    property VersionStr: string read FVersionStr; // 'd5', 'd6', ...

    property InstallDir: string read FInstallDir write FInstallDir;
    property BplDir: string read FBplDir write FBplDir;

    property Target: TCompileTarget read FTarget;

    property WizardPicture: string read FWizardPicture;
    property WelcomePicture: string read FWelcomePicture;
    property StartupPicture: string read FStartupPicture;
  end;

var
  Config: TConfig;

function CheckValidDelphiInstallation: Boolean;
function ResolveDirectory(const Dir: string; ResolveInternals: Boolean = True): string;

var
  CompilerList: TCompileTargetList;

implementation

function CheckValidDelphiInstallation: Boolean;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Result := Reg.OpenKeyReadOnly(Config.Target.RegistryKey + '\Library');
  finally
    Reg.Free;
  end;
end;

function ResolveDirectory(const Dir: string; ResolveInternals: Boolean = True): string;
begin
  Result := Dir;
  if ResolveInternals then
  begin
    Result := StringReplace(Result, '$(INSTALLDIR)', Config.InstallDir, [rfIgnoreCase]);
    Result := StringReplace(Result, '$(INSTALLBPLDIR)', Config.BplDir, [rfIgnoreCase]);
  end;

  Result := StringReplace(Result, '$(ROOT)', Config.Target.RootDir, [rfIgnoreCase]);
  Result := StringReplace(Result, '$(ROOTDIR)', Config.Target.RootDir, [rfIgnoreCase]);
  Result := StringReplace(Result, '$(BDSPROJECTDIR)', Config.Target.BDSProjectsDir, [rfIgnoreCase]);
  Result := StringReplace(Result, '$(PROJECTDIR)', Config.Target.ProjectDir, [rfIgnoreCase]);
  Result := StringReplace(Result, '$(BPLDIR)', Config.Target.BplDir, [rfIgnoreCase]);

  Result := Config.Target.ExpandDirMacros(Result);
end;

{ TConfig }

constructor TConfig.Create;
begin
  inherited Create;
  FLibraryPaths := TStringList.Create;
end;

destructor TConfig.Destroy;
begin
  FLibraryPaths.Free;
  inherited Destroy;
end;

procedure TConfig.LoadFromFile(const Filename: string);
var
  Ini: TMemIniFile;
  i: Integer;
begin
  FLibraryPaths.Clear;
  Ini := TMemIniFile.Create(Filename);
  try
    FTitle := Trim(Ini.ReadString('Installer', 'Title', ''));
    FWizardPicture := Trim(Ini.ReadString('Installer', 'WizardPicture', ''));
    FWelcomePicture := Trim(Ini.ReadString('Installer', 'WelcomePicture', ''));
    FStartupPicture := Trim(Ini.ReadString('Installer', 'StartupPicture', ''));

    FDefaultInstallDir := Trim(Ini.ReadString('Installer', 'DefaultInstallDir', '$(ROOTDIR)\Packages\JVCL'));
    FDefaultBplDir := Trim(Ini.ReadString('Installer', 'DefaultBplDir', '$(Package DPL Output)'));

    FVersion := d0;
    FVersionStr := Trim(Ini.ReadString('Installer', 'Version', ''));
    if SameText(FVersionStr, 'd5') then FVersion := d5
    else if SameText(FVersionStr, 'd6') then FVersion := d6
    else if SameText(FVersionStr, 'd7') then FVersion := d7
    else if SameText(FVersionStr, 'd9') then FVersion := d9
    else if SameText(FVersionStr, 'd9net') then FVersion := d9net;

    Ini.ReadSectionValues('LibraryPaths', FLibraryPaths);
  finally
    Ini.Free;
  end;

  for i := 0 to CompilerList.Count - 1 do
    if CompilerList[i].Name + ' ' + CompilerList[i].VersionStr = VersionNames[Config.Version] then
    begin
      FTarget := CompilerList[i];
      Break;
    end;

  if FTarget <> nil then
  begin
    FBplDir := ResolveDirectory(FDefaultBplDir, False);
    FInstallDir := ResolveDirectory(FDefaultInstallDir, False);
  end;
end;

initialization
  Config := TConfig.Create;
  CompilerList := TCompileTargetList.Create;
  Config.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Config\config.ini');

finalization
  Config.Free;

end.
