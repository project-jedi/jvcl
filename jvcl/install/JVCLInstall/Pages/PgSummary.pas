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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

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
  PackageUtils, Intf, PgInstall, PgUninstall;

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
              Add('Install JVCL 3 for', Target.DisplayName);
              if FrameworkCount > 1 then
              begin
                S := '';
                for Kind := pkFirst to pkLast do
                  if Kind in InstallMode then
                    S := ', ' + PackageGroupKindToStr[Kind];
                Delete(S, 1, 2);

                Add('Install for frameworks:', S);
              end;

             // directories:
              Add('BPL output directory:', BplDir);
              if Target.IsBCB then
              begin
                Add('Lib output directory:', DcpDir);
                Add('HPP output directory:', HppDir);
              end
              else
                Add('DCP output directory:', DcpDir);

             // options
              if Build then
                Add('Build packages', '')
              else
                Add('Compile packages', '');

              if CleanPalettes then
                Add('Clean component palettes', '');

             // search directories
              if DeveloperInstall then
                Add('Add to search path:', '$(JVCL)\common;$(JVCL)\run;$(JVCL)\qcommon;$(JVCL)\qrun')
              else
                Add('Add to search path:', '$(JVCL)\common');

              Add('Add to browse path:', '$(JVCL)\common;$(JVCL)\run;$(JVCL)\qcommon;$(JVCL)\qrun');
              Add('Add to library path:', UnitOutDir);
              if Target.IsBCB then
                Add('Add to include path:', HppDir);
            end;
          itUninstall:
            begin
              Add('Uninstall from', Target.DisplayName);
              Add('Remove', 'JVCL palettes');
              Add('Unregister', 'JVCL 3 packages');
              Add('Remove', 'JVCL 3 files');
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
  Title := 'Summary';
  case Installer.InstallType of
    itFreshInstall:
      SubTitle := 'The following actions will be done through installation.';
    itUpdate:
      SubTitle := 'The following actions will be done through the update.';
    itUninstall:
      SubTitle := 'The following actions will be done through uninstallation.';
  end;
end;

end.
