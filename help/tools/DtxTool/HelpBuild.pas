unit HelpBuild;

interface

uses
  Windows, ShellAPI, SysUtils, Classes, Core, Graphics, Controls, Forms,
  StdCtrls, Dialogs,
  JvConsts, HelpBuildData;

type
  THelpBuilder = class(TInterfacedObject, IInstaller)
  private
    FWelcomePage: ISingleChoosePage;
    FData: THelpBuildData;
    FPackageInstaller: IPackageInstaller;
  protected
    { IInstaller }
    procedure Init(APackageInstaller: IPackageInstaller);
    function InstallerName: WideString;
    function FirstPage: IInstallerPage;
    function GetJCLDir: WideString;
    function CanInstall: Boolean;
    procedure Finish;
    function AutoInstall: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property PackageInstaller: IPackageInstaller read FPackageInstaller;
    property Data: THelpBuildData read FData;
  end;

  { Base class for all HelpBuilder pages }
  THelpBuildPage = class(TInterfacedObject, IInstallerPage)
  private
    FInstaller: THelpBuilder;
    FNext: IInstallerPage;
  public
    constructor Create(AInstaller: THelpBuilder);
    procedure Init; virtual;
    property HelpBuilder: THelpBuilder read FInstaller;
  public
    { IInstallerPage }
    procedure Title(var Title, SubTitle: WideString); virtual;
    function CanNext: Boolean; virtual;
    function CanPrev: Boolean; virtual;
    function NextPage: IInstallerPage; virtual; abstract;
    procedure Action; virtual;
  end;

  { Welcome page that shows a text field and the install types }
  TWelcomePage = class(THelpBuildPage, ISingleChoosePage)
  public
    { IInstallerPage }
    function NextPage: IInstallerPage; override;
    procedure Title(var Title, SubTitle: WideString); override;
  public
    { IWelcomePage }
    function Text: WideString;
    procedure Options(Options: TStrings; var HorzOrientation: THorzOrientation);
    procedure SetSelectedOption(Index: Integer);
    function GetSelectedOption: Integer;
    procedure SetupRadioButton(Index: Integer; Control: TRadioButton);
  end;

implementation

uses
  PgConfig;

resourcestring
  RsInstallerName = 'JVCL 3  Help Builder';
  RsInstallerTitle = 'JVCL 3 Help Builder';
  RsWelcomePageSubTitle = 'Welcome to the JVCL 3 help build application';

//=== { THelpBuilder } =======================================================

constructor THelpBuilder.Create;
begin
  inherited Create;
  FData := THelpBuildData.Create;
end;

destructor THelpBuilder.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

function THelpBuilder.AutoInstall: Boolean;
begin
  Result := False;
end;

function THelpBuilder.CanInstall: Boolean;
begin
  Result := True;
end;

procedure THelpBuilder.Finish;
begin
  // do nothing
end;

function THelpBuilder.FirstPage: IInstallerPage;
begin
  if not Assigned(FWelcomePage) then
    FWelcomePage := TWelcomePage.Create(Self);
  Result := FWelcomePage;
end;

function THelpBuilder.GetJclDir: WideString;
begin
end;

procedure THelpBuilder.Init(APackageInstaller: IPackageInstaller);
begin
  FPackageInstaller := APackageInstaller;
end;

function THelpBuilder.InstallerName: WideString;
begin
  Result := RsInstallerName;
end;

//=== { THelpBuildPage } =====================================================

constructor THelpBuildPage.Create(AInstaller: THelpBuilder);
begin
  inherited Create;
  FInstaller := AInstaller;
  Init;
end;

procedure THelpBuildPage.Action;
begin
  // do nothing
end;

function THelpBuildPage.CanNext: Boolean;
begin
  Result := True;
end;

function THelpBuildPage.CanPrev: Boolean;
begin
  Result := True;
end;

procedure THelpBuildPage.Init;
begin
  // do nothing
end;

procedure THelpBuildPage.Title(var Title, SubTitle: WideString);
begin
  Title := RsInstallerTitle;
  SubTitle := '';
end;

//=== { TWelcomePage } =======================================================

function TWelcomePage.GetSelectedOption: Integer;
begin
  Result := AllHelpBuildTypes.IndexOf(HelpBuilder.Data.HelpBuildType);
end;

function TWelcomePage.NextPage: IInstallerPage;
begin
  { TODO }
  if FNext = nil then
    FNext := TConfigPage.Create(HelpBuilder);
  Result := FNext;
end;

procedure TWelcomePage.Options(Options: TStrings; var HorzOrientation: THorzOrientation);
var
//  HelpBuildType: THelpBuildType;
  I: Integer;
begin
  for I := 0 to AllHelpBuildTypes.Count-1 do
    Options.Add(HelpBuildNiceStr(AllHelpBuildTypes[i]));

//  for HelpBuildType:= Low(THelpBuildType) to High(THelpBuildType) do
//    Options.Add(cHelpBuildTypeNiceStr[HelpBuildType]);
end;

procedure TWelcomePage.SetSelectedOption(Index: Integer);
begin
//  if (Index >= Ord(Low(THelpBuildType))) and (Index <= Ord(High(THelpBuildType))) then
    HelpBuilder.Data.HelpBuildType := AllHelpBuildTypes[Index];
  PackageInstaller.UpdatePages;
end;

procedure TWelcomePage.SetupRadioButton(Index: Integer; Control: TRadioButton);
begin
end;

function TWelcomePage.Text: WideString;
begin
  Result := 'Hello';
end;

procedure TWelcomePage.Title(var Title, SubTitle: WideString);
begin
  inherited Title(Title, SubTitle);
  SubTitle := RsWelcomePageSubTitle;
end;

initialization
  try
    PackageInstaller := TPackageInstaller.Create(THelpBuilder.Create);
  except
    on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), PChar(string(E.ClassName)), MB_OK or MB_ICONERROR);
      Halt(1);
    end;
  end;
end.
