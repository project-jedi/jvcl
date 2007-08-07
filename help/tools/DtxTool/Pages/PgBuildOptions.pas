unit PgBuildOptions;

interface

uses
  SysUtils, Classes, Controls,
  Core, HelpBuild;

type
  { A page where the user can configure the compile process and the jvcl.inc
    file. }
  TBuildOptionsPage = class(THelpBuildPage, IUserDefinedPage)
  public
    procedure Title(var Title, SubTitle: WideString); override;
    function SetupPage(Client: TWinControl): TWinControl; virtual;
    function NextPage: IInstallerPage; override;
  end;

implementation

uses
  FrmBuildOptions, PgSummary, PgDoxCompileOptions, HelpBuildData, HelpBuildHelper;

resourcestring
  RsConfigPageSubTitle = 'Choose the dox compile options';
  RsConfigPageTitle = 'Configuration';

//=== { TBuildOptionsPage } ==================================================

function TBuildOptionsPage.NextPage: IInstallerPage;
begin
  Result := GetNextPage(HelpBuilder, ClassType).Create(HelpBuilder);
//  if HelpBuilder.Data.Task[HelpBuildTask_BuildDoxFile_HTML] or
//     HelpBuilder.Data.Task[HelpBuildTask_BuildDoxFile_WinHelp] then
//    Result := TDoxCompileOptionsPage.Create(HelpBuilder)
//  else
//    Result := TSummaryPage.Create(HelpBuilder);
end;

function TBuildOptionsPage.SetupPage(Client: TWinControl): TWinControl;
begin
  Result := TFrameBuildOptions.Build(HelpBuilder, Client);
end;

procedure TBuildOptionsPage.Title(var Title, SubTitle: WideString);
begin
  Title := RsConfigPageTitle;
  SubTitle := RsConfigPageSubTitle;
end;

end.

