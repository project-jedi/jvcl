{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: PgSummary.pas, released on 2004-03-29.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit PgSummary;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Dialogs,
  Core, JVCL3Install, DelphiData, JVCLData;

type
  { TSummaryPage displays a summary of the JVCL un-/installation. }
  TSummaryPage = class(TInstallerPage, ISummaryPage)
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
  InstallerConsts, PackageUtils, Intf, PgInstall, PgUninstall;

{ TSummaryPage }

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

var
  i: Integer;
  First: Boolean;
  S: string;
  Kind: TPackageGroupKind;
begin
  First := True;
  for i := 0 to Installer.SelTargetCount - 1 do
  begin
    with Installer.SelTargets[i] do
    begin
      if InstallJVCL then
      begin
        if not First then
          Add('', '')
        else
          First := False;

        case Installer.InstallType of
          itFreshInstall,
          itUpdate:
            begin
              Add(RsInstallForTarget, Target.DisplayName);
              if FrameworkCount > 1 then
              begin
                S := '';
                for Kind := pkFirst to pkLast do
                  if Kind in InstallMode then
                    S := S + ', ' + PackageGroupKindToStr[Kind];
                Delete(S, 1, 2);

                Add(RsInstallForFrameworks, S);
              end;

             // directories:
              Add(RsBplOutputDirectory, BplDir);
              if Target.IsBCB then
              begin
                Add(RsLibOutputDirectory, DcpDir);
                Add(RsHppOutputDirectory, HppDir);
              end
              else
                Add(RsDcpOutputDirectory, DcpDir);

             // options
              if Build then
                Add(RsBuildPackages, '')
              else
                Add(RsCompilePackages, '');

              if not CompileOnly then
              begin
                if CleanPalettes then
                  Add(RsCleanComponentPalettes, '');

               // search directories
                S := sJVCLMacroCommonDir;
                if pkVCL in InstallMode then
                  S := S + ';' + sJVCLMacroRunDir;
                if pkClx in InstallMode then
                  S := S + ';' + sJVCLMacroClxDirs;
                Add(RsAddToBrowsePath, S);
                if not DeveloperInstall then
                  S := sJVCLMacroCommonDir;
                Add(RsAddToSearchPath, S);

                Add(RsAddToLibraryPath, UnitOutDir);
                if Target.IsBCB then
                  Add(RsAddToIncludePath, HppDir);
              end;
            end;
          itUninstall:
            begin
              Add(RsUninstallFromTarget, Target.DisplayName);
              Add(RsRemove, RsJVCLPalettes);
              Add(RsRemove, RsJVCLDirsFromPathLists);
              Add(RsUnregister, RsJVCLPackages);
              if Installer.Data.DeleteFilesOnUninstall then
                Add(RsRemove, RsJVCLFiles);
            end;
        end;
      end;
    end;
  end;
end;

function TSummaryPage.NextPage: IInstallerPage;
begin
  case Installer.InstallType of
    itFreshInstall,
    itUpdate:
      Result := TInstallPage.Create(Installer);
    itUninstall:
      Result := TUninstallPage.Create(Installer);
  end;
end;

procedure TSummaryPage.Title(var Title, SubTitle: WideString);
begin
  Title := RsSummaryPageTitle;
  case Installer.InstallType of
    itFreshInstall:
      SubTitle := RsSummaryPageSubTitleInstall;
    itUpdate:
      SubTitle := RsSummaryPageSubTitleUpdate;
    itUninstall:
      SubTitle := RsSummaryPageSubTitleUninstall;
  end;
end;

end.
