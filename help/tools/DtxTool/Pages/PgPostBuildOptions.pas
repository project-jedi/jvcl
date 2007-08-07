unit PgPostBuildOptions;

interface

uses
  SysUtils, Classes, Controls,
  Core, HelpBuild;

type
  { A page where the user can configure the compile process and the jvcl.inc
    file. }
  TPostBuildOptionsPage = class(THelpBuildPage, IUserDefinedPage)
  public
    procedure Title(var Title, SubTitle: WideString); override;
    function SetupPage(Client: TWinControl): TWinControl; virtual;
    function NextPage: IInstallerPage; override;
  end;

implementation

uses
  FrmPostBuildOptions, HelpBuildHelper;

resourcestring
  RsConfigPageSubTitle = 'Choose the build options';
  RsConfigPageTitle = 'Configuration';

//=== { TPostBuildOptionsPage } ==============================================

function TPostBuildOptionsPage.NextPage: IInstallerPage;
begin
  Result := GetNextPage(HelpBuilder, ClassType).Create(HelpBuilder);
//  Result := TSummaryPage.Create(HelpBuilder);
end;

function TPostBuildOptionsPage.SetupPage(Client: TWinControl): TWinControl;
begin
  Result := TFramePostBuildOptions.Build(HelpBuilder, Client);
end;

procedure TPostBuildOptionsPage.Title(var Title, SubTitle: WideString);
begin
  Title := RsConfigPageTitle;
  SubTitle := RsConfigPageSubTitle;
end;

end.
