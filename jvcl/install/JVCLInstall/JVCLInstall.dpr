{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCLInstall.dpr, released on 2004-03-29.

The Initial Developer of the Original Code is Andreas Hausladen
[Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

program JVCLInstall;

{$I jvcl.inc}

uses
  JvGnugettext,
  Forms,
  Main in 'Main.pas' {FormMain},
  Core in 'Core.pas',
  JVCL3Install in 'JVCL3Install.pas',
  PageBuilder in 'PageBuilder.pas',
  DelphiData in 'DelphiData.pas',
  JCLData in 'JCLData.pas',
  JVCLData in 'JVCLData.pas',
  AHCompBrowseFolderDlg in 'Helpers\AHCompBrowseFolderDlg.pas',
  PgIDESelection in 'Pages\PgIDESelection.pas',
  PgConfig in 'Pages\PgConfig.pas',
  FrmConfigPage in 'Frames\FrmConfigPage.pas' {FrameConfigPage: TFrame},
  dpp_PascalParser in 'Helpers\dpp_PascalParser.pas',
  JVCLConfiguration in 'Helpers\JVCLConfiguration.pas',
  FrmDirEditBrowse in 'Frames\FrmDirEditBrowse.pas' {FrameDirEditBrowse: TFrame},
  FrmUninstall in 'Frames\FrmUninstall.pas' {FrameUninstall: TFrame},
  PgSummary in 'Pages\PgSummary.pas',
  FrmPackageSelection in 'Frames\FrmPackageSelection.pas' {FramePackageSelection: TFrame},
  FileUtils in '..\..\devtools\PackagesGenerator\FileUtils.pas',
  GenerateUtils in '..\..\devtools\PackagesGenerator\GenerateUtils.pas',
  ConditionParser in '..\..\devtools\PackagesGenerator\ConditionParser.pas',
  PackageInformation in '..\..\devtools\common\PackageInformation.pas',
  Utils in 'Utils.pas',
  Compile in 'Compile.pas',
  Intf in 'Intf.pas',
  PackageUtils in 'PackageUtils.pas',
  CapExec in 'Helpers\CapExec.pas',
  PgPackageSelection in 'Pages\PgPackageSelection.pas',
  HtHint in 'Helpers\HtHint.pas',
  MissingPropertyFix in 'MissingPropertyFix.pas',
  FrmInstall in 'Frames\FrmInstall.pas' {FrameInstall: TFrame},
  PgInstall in 'Pages\PgInstall.pas',
  PgUninstall in 'Pages\PgUninstall.pas',
  MainConfig in 'Helpers\MainConfig.pas' {FormJvclIncConfig},
  CmdLineUtils in 'CmdLineUtils.pas',
  InstallerConsts in 'InstallerConsts.pas',
  FrmCompile in 'FrmCompile.pas' {FormCompile},
  FrmCompileMessages in 'FrmCompileMessages.pas' {FormCompileMessages},
  JediRegInfo in 'JediRegInfo.pas',
  RegConfig in 'RegConfig.pas',
  Dcc32FileAgePatch in 'Dcc32FileAgePatch.pas',
  GenerateTargets in '..\..\devtools\PackagesGenerator\GenerateTargets.pas',
  GenerateAlias in '..\..\devtools\PackagesGenerator\GenerateAlias.pas',
  GenerateReplacements in '..\..\devtools\PackagesGenerator\GenerateReplacements.pas',
  DefinesConditionParser in '..\..\devtools\PackagesGenerator\DefinesConditionParser.pas',
  GenerateDefines in '..\..\devtools\PackagesGenerator\GenerateDefines.pas',
  PackageGenerator in '..\..\devtools\PackagesGenerator\PackageGenerator.pas';

{$R *.res}
{$R CommCtrlAsInvoker.res}

begin
  // By default, indicate an error.
  // If (un)installation goes succesfully to completion, it will be set to 0, indicating success
  ExitCode := 1;

  {$IFDEF USE_DXGETTEXT}
  if CmdOptions.Lang <> '' then
     UseLanguage(CmdOptions.Lang);
  {$ENDIF USE_DXGETTEXT}
  Application.Initialize;
  Application.Title := 'JVCL Installer';
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormJvclIncConfig, FormJvclIncConfig);
  Application.CreateForm(TFormCompileMessages, FormCompileMessages);
  Application.Run;
end.
