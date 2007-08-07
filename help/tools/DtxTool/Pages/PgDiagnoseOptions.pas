unit PgDiagnoseOptions;

interface

uses
  SysUtils, Classes, Controls,
  Core, HelpBuild;

type
  TDiagnoseOptionsPage = class(THelpBuildPage, IUserDefinedPage)
  public
    procedure Title(var Title, SubTitle: WideString); override;
    function SetupPage(Client: TWinControl): TWinControl; virtual;
    function NextPage: IInstallerPage; override;
  end;

implementation

uses
  FrmDiagnoseOptions, HelpBuildHelper, HelpBuildData;

//=== { TDiagnoseOptionsPage } ==================================================

function TDiagnoseOptionsPage.NextPage: IInstallerPage;
begin
  Result := GetNextPage(HelpBuilder, ClassType).Create(HelpBuilder);
//  Result := TSummaryPage.Create(HelpBuilder);
end;

function TDiagnoseOptionsPage.SetupPage(Client: TWinControl): TWinControl;
begin
  Result := TFrameDiagnoseOptions.Build(HelpBuilder, Client);
end;

procedure TDiagnoseOptionsPage.Title(var Title, SubTitle: WideString);
begin
  Title := 'Choose the diagnose options';
  SubTitle := 'Configuration';
end;

end.

