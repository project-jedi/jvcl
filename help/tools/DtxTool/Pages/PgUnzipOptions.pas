unit PgUnzipOptions;

interface

uses
  SysUtils, Classes, Controls,
  Core, HelpBuild;

type
  TUnzipOptionsPage = class(THelpBuildPage, IUserDefinedPage)
  public
    procedure Title(var Title, SubTitle: WideString); override;
    function SetupPage(Client: TWinControl): TWinControl; virtual;
    function NextPage: IInstallerPage; override;
  end;

implementation

uses
  FrmUnzipOptions, HelpBuildHelper;

//=== { TUnzipOptionsPage } ==================================================

function TUnzipOptionsPage.NextPage: IInstallerPage;
begin
  Result := GetNextPage(HelpBuilder, ClassType).Create(HelpBuilder);
//  Result := TSummaryPage.Create(HelpBuilder);
end;

function TUnzipOptionsPage.SetupPage(Client: TWinControl): TWinControl;
begin
  Result := TFrameUnzipOptions.Build(HelpBuilder, Client);
end;

procedure TUnzipOptionsPage.Title(var Title, SubTitle: WideString);
begin
  Title := 'Choose the unzip options';
  SubTitle := 'Configuration';
end;

end. 
