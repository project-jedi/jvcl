unit PgSummary;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Dialogs,
  Core, HelpBuild, HelpBuildData;

type
  { TSummaryPage displays a summary of the JVCL un-/installation. }
  TSummaryPage = class(THelpBuildPage, ISummaryPage)
  public
    { IInstallerPage }
    function NextPage: IInstallerPage; override;
    procedure Title(var Title: WideString; var SubTitle: WideString); override;
    function CanNext: Boolean; override;
    procedure Action; override;
  public
    { ISummaryPage }
    procedure GetSummary(Actions: TStrings; Comments: TStrings); virtual;
  end;

implementation

uses
  HelpBuildHelper;

resourcestring
  RsSummaryPageSubTitle = 'The following actions will be done during the build';
//  RsSummaryPageSubTitleUninstall = 'The following actions will be done through uninstallation';
//  RsSummaryPageSubTitleUpdate = 'The following actions will be done through the update';
  RsSummaryPageTitle = 'Summary';

//=== { TSummaryPage } =======================================================

procedure TSummaryPage.Action;
begin
  // without this the install page can only be used once.
  PackageInstaller.UpdatePages;
end;

function TSummaryPage.CanNext: Boolean;
begin
  Result := True;
end;

procedure TSummaryPage.GetSummary(Actions, Comments: TStrings);

  procedure Add(const Action, Comment: string);
  begin
    Actions.Add(Action);
    Comments.Add(Comment);
  end;

  //var
  //  i: Integer;
  //  First: Boolean;
  //  S: string;
begin
  with HelpBuilder.Data do
  begin
    if MustCreateGenDtxDir then
      Add('Create directory:', GenDtxDir);
    if MustCreateGenPasDir then
      Add('Create directory:', GenPasDir);

    Add('Clean directories', '');
    if Task[HelpBuildTask_UnzipOnlineHelpZipFile] then
      Add('Unzip online help zip file', OnlineHelpZipFileName);
    if Task[HelpBuildTask_GenerateOnlineHelpZipFiles] then
      Add('Generate zip files for updating online help', OnlineHelpDtxDir);
    if Task[HelpBuildTask_GenerateImages] then
      Add('Generate images (bmp,png)', HelpImagesCompDir);
    if Task[HelpBuildTask_GenerateRegisteredClassesFile] then
      Add('Generate reg. classes file', RegisteredClassesFileName);
    if Task[HelpBuildTask_GenerateGroupsFile] then
      Add('Generate groups file', GroupListFileName);
    if Task[HelpBuildTask_GenerateStrippedPasFiles] then
      Add('Generate stripped pas files', GenPasDir);
    if Task[HelpBuildTask_GeneratePackageList] then
      Add('Generate package file', PackageListFileName);
    if Task[HelpBuildTask_GenerateDonatorsDtxFile] then
      Add('Generate donators dtx', DonatorsDtxFileName);
    if Task[HelpBuildTask_GenerateFormattedDtxFiles] then
      Add('Generate formatted dtx files', GenDtxDir);
    if Task[HelpBuildTask_GenerateFuncRefDtxFile] then
      Add('Generate func. ref.', FuncRefDtxFileName);
    if Task[HelpBuildTask_GenerateHelpDoxFile] then
      Add('Generate dox file', HelpBuilder.Data.DoxFileName);
    if Task[HelpBuildTask_GenerateReferenceDoxFile] then
      Add('Generate dox file', HelpBuilder.Data.ReferenceDoxFileName);
    if Task[HelpBuildTask_GeneratePackagesDoxFile] then
      Add('Generate dox file', HelpBuilder.Data.PackagesDoxFileName);
    if Task[HelpBuildTask_BuildHelpDoxFile_HTML] then
      Add('Generate HTML help', '');
    if Task[HelpBuildTask_BuildHelpDoxFile_WinHelp] then
      Add('Generate Windows help', '');
    if Task[HelpBuildTask_PostProcessWinHelpOutput] then
      Add('Process Windows Help files', WinHelpOutputDir);
    if Task[HelpBuildTask_PostProcessHtmlHelpOutput] then
      Add('Process HTML Help files', HtmlHelpOutputDir);
    if Task[HelpBuildTask_GenerateDtxWordsFile] then
      Add('Generate words file', DtxWordsFileName);
    if Task[HelpBuildTask_GenerateDtxIdentifiersFile] then
      Add('Generate identifiers file', DtxIdentifiersFileName);
    if Task[HelpBuildTask_GenerateDtxInvalidWordsFile] then
      Add('Generate invalid words file', DtxInvalidWordsFileName);
    if Task[HelpBuildTask_GenerateDtxDuplicateIdentifiersFile] then
      Add('Generate duplicate identifiers file', DtxDuplicateIdentifiersFileName);
  end;

  //  First := True;
  //  for i := 0 to Installer.SelTargetCount - 1 do
  //  begin
  //    with Installer.SelTargets[i] do
  //    begin
  //      if InstallJVCL then
  //      begin
  //        if not First then
  //          Add('', '')
  //        else
  //          First := False;
  //
  //        case Installer.InstallType of
  //          itFreshInstall,
  //          itUpdate:
  //            begin
  //              Add(RsInstallForTarget, Target.DisplayName);
  //              if FrameworkCount > 1 then
  //              begin
  //                S := '';
  //                for Kind := pkFirst to pkLast do
  //                  if Kind in InstallMode then
  //                    S := S + ', ' + PackageGroupKindToStr[Kind];
  //                Delete(S, 1, 2);
  //
  //                Add(RsInstallForFrameworks, S);
  //              end;
  //
  //             // directories:
  //              Add(RsBplOutputDirectory, BplDir);
  //              if Target.IsBCB then
  //              begin
  //                Add(RsLibOutputDirectory, DcpDir);
  //                Add(RsHppOutputDirectory, HppDir);
  //              end
  //              else
  //                Add(RsDcpOutputDirectory, DcpDir);
  //
  //             // options
  //              if Build then
  //                Add(RsBuildPackages, '')
  //              else
  //                Add(RsCompilePackages, '');
  //
  //              if not CompileOnly then
  //              begin
  //                if CleanPalettes then
  //                  Add(RsCleanComponentPalettes, '');
  //
  //               // search directories
  //                S := sJVCLMacroCommonDir;
  //                if pkVCL in InstallMode then
  //                  S := S + ';' + sJVCLMacroRunDir;
  //                if pkClx in InstallMode then
  //                  S := S + ';' + sJVCLMacroClxDirs;
  //                Add(RsAddToBrowsePath, S);
  //                if not DeveloperInstall then
  //                  S := sJVCLMacroCommonDir;
  //                Add(RsAddToSearchPath, S);
  //
  //                Add(RsAddToLibraryPath, UnitOutDir);
  //                if Target.IsBCB then
  //                  Add(RsAddToIncludePath, HppDir);
  //              end;
  //            end;
  //          itUninstall:
  //            begin
  //              Add(RsUninstallFromTarget, Target.DisplayName);
  //              Add(RsRemove, RsJVCLPalettes);
  //              Add(RsRemove, RsJVCLDirsFromPathLists);
  //              Add(RsUnregister, RsJVCLPackages);
  //              if Installer.Data.DeleteFilesOnUninstall then
  //                Add(RsRemove, RsJVCLFiles);
  //            end;
  //        end;
  //      end;
  //    end;
  //  end;
end;

function TSummaryPage.NextPage: IInstallerPage;
begin
  Result := GetNextPage(HelpBuilder, ClassType).Create(HelpBuilder);
  //  case Installer.InstallType of
  //    itFreshInstall,
  //    itUpdate:
//  Result := TBuildPage.Create(HelpBuilder);
  //    itUninstall:
  //      Result := TUninstallPage.Create(Installer);
  //  end;
//  Result := nil;
end;

procedure TSummaryPage.Title(var Title, SubTitle: WideString);
begin
  Title := RsSummaryPageTitle;
  SubTitle := RsSummaryPageSubTitle;
end;

end.
