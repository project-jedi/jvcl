unit PgDoxCompileOptions;

interface

uses
  SysUtils, Classes, Controls,
  Core, HelpBuild;

type
  TDoxCompileOptionsPage = class(THelpBuildPage, IUserDefinedPage)
  public
    procedure Title(var Title, SubTitle: WideString); override;
    function SetupPage(Client: TWinControl): TWinControl; virtual;
    function NextPage: IInstallerPage; override;
  end;

implementation

uses
  FrmDoxCompileOptions, HelpBuildHelper;

resourcestring
  RsConfigPageSubTitle = 'Choose the build options';
  RsConfigPageTitle = 'Configuration';

//=== { TDoxCompileOptionsPage } ==============================================

function TDoxCompileOptionsPage.NextPage: IInstallerPage;
begin
//  Result := TSummaryPage.Create(HelpBuilder);
  Result := GetNextPage(HelpBuilder, ClassType).Create(HelpBuilder);
end;

function TDoxCompileOptionsPage.SetupPage(Client: TWinControl): TWinControl;
begin
  Result := TFrameDoxCompileOptions.Build(HelpBuilder, Client);
end;

procedure TDoxCompileOptionsPage.Title(var Title, SubTitle: WideString);
begin
  Title := RsConfigPageTitle;
  SubTitle := RsConfigPageSubTitle;
end;

end.
