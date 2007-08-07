unit PgBuild;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Dialogs,
  Core, HelpBuild, HelpBuildData, FrmBuild;

type
  { A page where the compile progress is displayed. }
  TBuildPage = class(THelpBuildPage, IUserDefinedPage, IInstallPage)
  private
    FFrame: TFrameBuild;
  public
    { IInstallerPage }
    function NextPage: IInstallerPage; override;
    procedure Title(var Title: WideString; var SubTitle: WideString); override;
    function CanNext: Boolean; override;
    function CanPrev: Boolean; override;
    procedure Action; override;
    procedure Abort;
  public
    { IUserDefinedPage }
    function SetupPage(Client: TWinControl): TWinControl; virtual;
  end;

implementation

resourcestring
  RsInstallPageSubTitle = 'Building the JVCL Help';
  RsInstallPageTitle = 'Building Help';

//=== { TBuildPage } =========================================================

procedure TBuildPage.Abort;
begin
  if Assigned(FFrame) then
    FFrame.Aborted := True;
end;

procedure TBuildPage.Action;
begin
  if Assigned(FFrame) then
    FFrame.Execute;
end;

function TBuildPage.CanNext: Boolean;
begin
  Result := True;
end;

function TBuildPage.CanPrev: Boolean;
begin
  Result := True;
end;

function TBuildPage.NextPage: IInstallerPage;
begin
  Result := nil;
end;

function TBuildPage.SetupPage(Client: TWinControl): TWinControl;
begin
  FFrame.Free;
  FFrame := TFrameBuild.Build(HelpBuilder, Client);
  Result := FFrame;
end;

procedure TBuildPage.Title(var Title, SubTitle: WideString);
begin
  Title := RsInstallPageTitle;
  SubTitle := RsInstallPageSubTitle;
end;

end.
