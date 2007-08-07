{$I jvcl.inc}

unit PgConfig;

interface

uses
  SysUtils, Classes, Controls,
  Core, HelpBuild;

type
  TConfigPage = class(THelpBuildPage, IUserDefinedPage)
  public
    procedure Title(var Title, SubTitle: WideString); override;
    function SetupPage(Client: TWinControl): TWinControl; virtual;
    function NextPage: IInstallerPage; override;
    function CanNext: Boolean; override;
  end;

implementation

uses
  FrmConfigPage, HelpBuildData, HelpBuildHelper;

//=== { TConfigPage } ========================================================

function TConfigPage.CanNext: Boolean;
begin
  Result :=
    DirectoryExists(HelpBuilder.Data.HelpDir) and
    DirectoryExists(HelpBuilder.Data.JVCLxxDir);
end;

function TConfigPage.NextPage: IInstallerPage;
begin
  Result := GetNextPage(HelpBuilder, ClassType).Create(HelpBuilder);
//  if IsEqualGUID(HelpBuilder.Data.HelpBuildType, HelpBuildType_BuildHelp) then
//    Result := TBuildOptionsPage.Create(HelpBuilder)
//  else
//    if IsEqualGUID(HelpBuilder.Data.HelpBuildType, HelpBuildType_PostProcessHelpOutput) then
//    Result := TPostBuildOptionsPage.Create(HelpBuilder)
//  else
//    if IsEqualGUID(HelpBuilder.Data.HelpBuildType, HelpBuildType_SyncOnlineHelp) then
//  begin
//    if HelpBuilder.Data.Task[HelpBuildTask_UnzipOnlineHelpZipFile] then
//      Result := TUnzipOptionsPage.Create(HelpBuilder)
//    else
//      Result := TSummaryPage.Create(HelpBuilder)
//  end
//  else
//    if IsEqualGUID(HelpBuilder.Data.HelpBuildType, HelpBuildType_GenerateZipForUpdatingOnlineHelp) then
//    Result := TZipOptionsPage.Create(HelpBuilder)
//  else
//    if IsEqualGUID(HelpBuilder.Data.HelpBuildType, HelpBuildType_Diagnose) then
//    Result := TDiagnoseOptionsPage.Create(HelpBuilder)
//  else
//    Result := TSummaryPage.Create(HelpBuilder);
end;

function TConfigPage.SetupPage(Client: TWinControl): TWinControl;
begin
  Result := TFrameConfigPage.Build(HelpBuilder, Client);
end;

procedure TConfigPage.Title(var Title, SubTitle: WideString);
begin
  Title := 'Configuration';
  SubTitle := 'Choose the global help build options';
end;

end.

