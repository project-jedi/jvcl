{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: PgIDESelection.pas, released on 2004-03-29.

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

unit PgIDESelection;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Dialogs, ExtCtrls,
  Core, JVCL3Install, DelphiData, JVCLData,
  Windows;

type
  { A page where the user can choose for which IDE he wants to install the JVCL.
    Furthermore he can specifiy the directory where the JCL is located.
    For every IDE checkbox a label is displayed that contains informations like
    the installed JVCL version or a link where the prerequests could be
    downloaded. }
  TIDESelectionPage = class(TInstallerPage, IMultiChoosePage, IUserDefinedPage)
  private
    procedure JCLDirChanged(Sender: TObject; UserData: TObject; var Dir: string);
    procedure DoInstallJcl(Sender: TObject);
    procedure DoClickUninstallDeleteFiles(Sender: TObject);
  public
    { IInstallerPage }
    function NextPage: IInstallerPage; override;
    procedure Title(var Title: WideString; var SubTitle: WideString); override;
    function CanNext: Boolean; override;
  public
    { IMultiChoosePage }
    procedure CheckBoxes(CheckBoxes: TStrings; var HorzOrientation: THorzOrientation);
    procedure SetCheckBox(Index: Integer; Value: Boolean);
    function GetCheckBox(Index: Integer): Boolean;
    procedure SetupCheckBox(Index: Integer; Control: TCheckBox);

    { IUserDefinedPage }
    function SetupPage(Client: TWinControl): TWinControl; virtual;
  end;

implementation

uses
  InstallerConsts, FrmDirEditBrowse, PgConfig, PgSummary;

{ TIDESelectionPage }

procedure TIDESelectionPage.CheckBoxes(CheckBoxes: TStrings;
  var HorzOrientation: THorzOrientation);
var
  i: Integer;
begin
 // Here we build the Installer.SelTargets[] list that is used in all following
 // installer pages.
  Installer.SelTargetsClear;

  for i := 0 to Installer.Data.Targets.Count - 1 do
  begin
    with Installer.Data do
    begin
      if Installer.InstallType = itUninstall then
        if TargetConfig[i].InstalledJVCLVersion = 0 then
        begin
          TargetConfig[i].InstallJVCL := False;
          Continue;
        end;
      if Installer.InstallType = itUpdate then
        if TargetConfig[i].InstalledJVCLVersion <> 3 then
        begin
          TargetConfig[i].InstallJVCL := False;
          Continue;
        end;

      Installer.SelTargetsAdd(TargetConfig[i]);
      CheckBoxes.Add(Targets[i].DisplayName + '|');

     // reset to default
      TargetConfig[i].InstallJVCL := TargetConfig[i].CanInstallJVCL;
    end;
  end;

 // fill rest so the checkboxes are not spread over the whole form 
  while CheckBoxes.Count < 10 do
    CheckBoxes.Add('');
end;

function TIDESelectionPage.GetCheckBox(Index: Integer): Boolean;
begin
  Result := Installer.SelTargets[Index].InstallJVCL;
end;

function TIDESelectionPage.NextPage: IInstallerPage;
begin
  if Installer.InstallType <> itUninstall then
    Result := TConfigPage.Create(Installer)
  else
    Result := TSummaryPage.Create(Installer);
end;

function TIDESelectionPage.CanNext: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Installer.SelTargetCount - 1 do
    if Installer.SelTargets[i].InstallJVCL then
    begin
      Result := True;
      Break;
    end;
end;

procedure TIDESelectionPage.SetCheckBox(Index: Integer; Value: Boolean);
begin
  Installer.SelTargets[Index].InstallJVCL := Value;
  Installer.PackageInstaller.UpdatePages;
end;

procedure TIDESelectionPage.SetupCheckBox(Index: Integer; Control: TCheckBox);
var
  Lbl: TLabel;
  Config: TTargetConfig;
begin
  Config := Installer.SelTargets[Index];

  Lbl := TLabel.Create(Control);
  Lbl.Left := Control.Parent.BoundsRect.Right - 10 - Lbl.Width;
  Lbl.Alignment := taRightJustify;
  Lbl.Top := Control.Top;
  Lbl.ShowHint := True;
  Lbl.Parent := Control.Parent;

  if Installer.InstallType <> itUninstall then
  begin
    if not Config.IsUpToDate then
    begin
      Control.Enabled := False;
      Control.Checked := False;

      Lbl.Font.Color := clBlue;
      Lbl.Font.Style := [fsUnderline];
      Lbl.Caption := RsDelphiBCBUpdateRequired;
      Lbl.Hint := Format('%s <c:blue>%s<c:black>|%s', [RsDownloadUpdatesFrom,
        Config.Target.Homepage, Config.Target.Homepage]);
      Lbl.Cursor := crHandPoint;
      Lbl.OnClick := Installer.DoHomepageClick;
    end
    else if Config.MissingJCL and (Installer.JCLDir = '') then
    begin
      Control.Enabled := False;
      Control.Checked := False;

      Lbl.Font.Color := clBlue;
      Lbl.Font.Style := [fsUnderline];
      Lbl.Caption := RsJCLVersionRequired;
      Lbl.Hint := RsDownloadOrSelectJclDir;
      Lbl.Cursor := crHandPoint;
      Lbl.OnClick := Installer.DoHomepageClick;
    end
    else
    begin
      Control.Checked := True;
      if Config.InstalledJVCLVersion > 0 then
        Lbl.Caption := Format(RsInstalledJVCLVersion, [Config.InstalledJVCLVersion]);
      if Config.MissingJCL and (Installer.JCLDir <> '') then
      begin
        Control.Enabled := False;
        Control.Checked := False;
        Lbl := TLabel.Create(Control);
        Lbl.Font.Name := 'Arial';
        Lbl.Font.Style := [fsUnderline];
        Lbl.Font.Color := clBlue;
        Lbl.Caption := RsInstallJCL;
        Lbl.Left := Control.BoundsRect.Right + 10;
        Lbl.Top := Control.Top;
        Lbl.Parent := Control.Parent;
        Lbl.Cursor := crHandPoint;
        Lbl.OnClick := DoInstallJCL;
      end;
    end;
  end
  else // Uninstall
  begin
    Control.Checked := True;
    if Config.InstalledJVCLVersion > 0 then
      Lbl.Caption := Format(RsInstalledJVCLVersion, [Config.InstalledJVCLVersion]);
  end;
end;

function TIDESelectionPage.SetupPage(Client: TWinControl): TWinControl;
var
  Panel: TPanel;
begin
  if Installer.InstallType <> itUninstall then
    with TFrameDirEditBrowse.Build(RsJCLDirectoryCaption, Installer.JCLDir,
                                   JCLDirChanged, nil, Client, RsJCLDirectorySelectionBtnHint) do
      Align := alBottom
  else
  begin
    Panel := TPanel.Create(Client);
    with Panel do
    begin
      Height := 40;
      BevelOuter := bvNone;
      Parent := Client;
      Align := alBottom;
    end;
    with TCheckBox.Create(Panel) do
    begin
      Parent := Panel;
      Left := 8;
      Top := 10;
      Caption := RsDeleteJVCLFilesCaption;
      Width := Panel.ClientWidth - Left * 2;
      Checked := Installer.Data.DeleteFilesOnUninstall;
      OnClick := DoClickUninstallDeleteFiles;
    end;
  end;
  Result := nil;
end;

procedure TIDESelectionPage.Title(var Title, SubTitle: WideString);
begin
  Title := RsSelectionPageTitle;
  case Installer.InstallType of
    itFreshInstall:
      SubTitle := RsSelectionPageSubTitleInstall;
    itUpdate:
      SubTitle := RsSelectionPageSubTitleUpdate;
    itUninstall:
      SubTitle := RsSelectionPageSubTitleUninstall;
  end;
end;

procedure TIDESelectionPage.JCLDirChanged(Sender: TObject; UserData: TObject;
  var Dir: string);
begin
{  if (FileExists(Dir + '\CJcl.dcp') and FileExists(Dir + '\CJclVcl.dcp')) or
     (FileExists(Dir + '\DJcl.dcp') and FileExists(Dir + '\DJclVcl.dcp')) then
  begin
   // are D/CJcl.dcp and D/CJclVcl.dcp are available that we could use
   // Delphi 5 / BCB 5 are not supported here
  end
  else}
  begin
    Dir := Dir + '\';
    while (Dir <> '') and (not FileExists(Dir + 'source\common\JclBase.pas')) do
    begin
      Delete(Dir, Length(Dir), 1); // remove '\'
      Dir := ExtractFilePath(Dir);
    end;
  end;
  if (Dir <> '') and (Dir[Length(Dir)] = '\') then
    Delete(Dir, Length(Dir), 1);

  if Dir <> '' then
  begin
    if Installer.JCLDir <> Dir then
    begin
      Installer.JCLDir := Dir;
      Installer.PackageInstaller.RebuildPage;
    end;
  end
  else
    MessageDlg(RsNoJclVersionFound, mtError, [mbOk], 0);
end;

procedure TIDESelectionPage.DoInstallJcl(Sender: TObject);
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
  i: Integer;
  Version: Integer;
  Tg: TTargetConfig;
  Cmd, Dir: string;
begin
  Version := 5; // find the newest Delphi version
  Tg := nil;
 // is there a Delphi?
  for i := 0 to Installer.Data.Targets.Count - 1 do
  begin
    if (not Installer.Data.Targets[i].IsBCB) and
       (Version <= Installer.Data.Targets[i].Version) then
      Tg := Installer.Data.TargetConfig[i];
  end;

  if Tg = nil then
  begin
   // is there a BCB?
    for i := 0 to Installer.Data.Targets.Count - 1 do
    begin
      if (Installer.Data.Targets[i].IsBCB) and
         (Version <= Installer.Data.Targets[i].Version) then
        Tg := Installer.Data.TargetConfig[i];
    end;
  end;

  Assert(Tg <> nil, 'No Delphi/BCB installed'); // do not localize

  Dir := ExtractShortPathName(Installer.JCLDir) + '\install';
  Cmd := ExtractShortPathName(Tg.Target.Make);

  StartupInfo.cb := SizeOf(StartupInfo);
  GetStartupInfo(StartupInfo);
  SetEnvironmentVariable('ROOT', PChar(ExtractShortPathName(Tg.Target.RootDir)));
  if CreateProcess(nil, PChar(Cmd), nil, nil, True, 0, nil, PChar(Dir),
    StartupInfo, ProcessInfo) then
  begin
    CloseHandle(ProcessInfo.hThread);
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    PackageInstaller.RebuildPage;
  end
  else
    raise Exception.Create(RsErrorInstallingJCL);
end;

procedure TIDESelectionPage.DoClickUninstallDeleteFiles(Sender: TObject);
begin
  Installer.Data.DeleteFilesOnUninstall := TCheckBox(Sender).Checked;
end;

end.
