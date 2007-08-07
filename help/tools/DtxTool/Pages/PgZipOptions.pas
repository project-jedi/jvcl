unit PgZipOptions;

interface

uses
  SysUtils, Classes, Controls,
  Core, HelpBuild;

type
  TZipOptionsPage = class(THelpBuildPage, IUserDefinedPage)
  public
    procedure Title(var Title, SubTitle: WideString); override;
    function SetupPage(Client: TWinControl): TWinControl; virtual;
    function NextPage: IInstallerPage; override;
  end;

implementation

uses
  FrmZipOptions, HelpBuildHelper;

//=== { TZipOptionsPage } ==================================================

function TZipOptionsPage.NextPage: IInstallerPage;
begin
  Result := GetNextPage(HelpBuilder, ClassType).Create(HelpBuilder);
//  Result := TSummaryPage.Create(HelpBuilder);
end;

function TZipOptionsPage.SetupPage(Client: TWinControl): TWinControl;
begin
  Result := TFrameZipOptions.Build(HelpBuilder, Client);
end;

procedure TZipOptionsPage.Title(var Title, SubTitle: WideString);
begin
  Title := 'Choose the zip options';
  SubTitle := 'Configuration';
end;

end. 
