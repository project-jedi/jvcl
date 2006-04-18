unit JVCLConfiguration;

interface

uses
  SysUtils, Classes, DelphiData, ConfigurationBase, ConfigOptions;

type
  TJVCLConfiguration = class(TConfiguration)
  private
    FJCLDirectory: string;
  protected
    function GetName: string; override;
    function GetVersionStr: string; override;
  public
    constructor Create;
    function SupportsPersonality(Personality: TIDEPersonality): Boolean; override;
    function IsUpToDate(IDE: TDelphiIDE): Boolean; override;
    function CreateOptionsFor(Personality: TIDEPersonality): TPersonalityOptions; override;
  published
    property JCLDirectory: string read FJCLDirectory write FJCLDirectory;
  end;

  TJVCLPersonalityOptions = class(TPersonalityOptions)
  public
    constructor Create(APersonality: TIDEPersonality);
    function GetTabOptions(Index: Integer): TOptionList; override;
  end;

const
  Product = 'JVCL'; // do not localize
  ProductVersion = '3.3'; // do not localize

resourcestring
  RsJCLDirectoryCaption = '&JCL Directory:';
  RsJCLDirectoryHint = 'The JVCL Installer requires a JCL source directory.';
  RsJvclIncOptions = 'jvcl.inc Options';

implementation

{ TJVCLConfiguration }

constructor TJVCLConfiguration.Create;
begin

end;

function TJVCLConfiguration.GetVersionStr: string;
begin
  Result := ProductVersion;
end;

function TJVCLConfiguration.GetName: string;
begin
  Result := Product;
end;

function TJVCLConfiguration.SupportsPersonality(Personality: TIDEPersonality): Boolean;
begin
  case Personality.IDE.Version of
    5:
      Result := Personality.ClassType = TDelphiPersonality;
    6, 7:
      Result := ((Personality.ClassType = TDelphiPersonality) or
                 (Personality.ClassType = TCppPersonality)) and
                (Pos('VisualCLX', Personality.Name) = 0); // VisualCLX is not supported at the moment // do not localize
    9:
      Result := (Personality.ClassType = TDelphiPersonality) or
                (Personality.ClassType = TDotNetPersonality);
    10:
      Result := (Personality.ClassType = TDelphiCppPersonality) or
                (Personality.ClassType = TDotNetPersonality);
  else
    Result := False;
  end;
end;

function TJVCLConfiguration.IsUpToDate(IDE: TDelphiIDE): Boolean;
begin
  if IDE.Version = 6 then
    Result := IDE.LatestUpdate >= 2
  else
    Result := True;
end;

function TJVCLConfiguration.CreateOptionsFor(Personality: TIDEPersonality): TPersonalityOptions;
begin
  Result := TJVCLPersonalityOptions.Create(Personality);
end;

{ TJVCLPersonalityOptions }

constructor TJVCLPersonalityOptions.Create(APersonality: TIDEPersonality);
begin
  inherited Create(APersonality);
  SetWasInstalled(Personality.FindPackageEx('Jv') <> nil);
  TabNames.Add(RsJvclIncOptions);
end;

function TJVCLPersonalityOptions.GetTabOptions(Index: Integer): TOptionList;
begin
  Result := inherited GetTabOptions(Index);
  case Index of
    0: // Directories
      begin
        Result.Add(TOptionSpacer.Create);
        // add global JCLDirectory option
        Result.Add(TOptionDirectoryEdit.Create(Configuration, 'JCLDirectory',
          RsJCLDirectoryCaption, RsJCLDirectoryHint));
      end;

    1: ;// Compiler Options

    2: ;// jvcl.inc Options
  end;
end;

initialization
  Configuration := TJVCLConfiguration.Create;

end.
