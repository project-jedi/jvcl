{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: InstallerConsts.pas, released on 2004-04-09.

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

unit InstallerConsts;

interface

resourcestring

 // JVCL3Install.pas
  SWelcomeText =
    'The JEDI Visual Component Library (JVCL) consists of a large collection (currently ca. 500)'#10 +
    'visual and non-visual components which can be instantly reused in your Delphi, Kylix and'#10 +
    'C++ Builder projects.'#10 +
    ''#10 +
    'The library is built upon code donated from the JEDI community. It is reformatted to achieve'#10 +
    'a common look-and-feel, tested, documented and merged into the library. The library is grouped'#10 +
    'into several categories such as Enhanced Standard, Visual, Non-Visual, Data Aware and many,'#10 +
    'many more. The library is released to the public under the terms of the Mozilla Public License'#10 +
    '(MPL) and as such can be freely used in both freeware, shareware, open source and commercial'#10 +
    'projects.'#10 +
    ''#10 +
    'Source code files included in the JVCL have a header which explicitly states the license (as'#10 +
    'is required). However, unless noted otherwise, all files, including those without an MPL'#10 +
    'header, are subject to the MPL license.';

  RsInstallerName = 'JVCL 3  Installation';

  RsInstallerTitle = 'JVCL 3 Installation';
  RsWelcomePageSubTitle = 'Welcome to the JVCL 3 installation program.';
  RsNoDelphiBcbInstalled = 'No Delphi or BCB is installed. The installer terminates.';
  RsDelphiBcbRunning = 'Delphi or BCB is running. Terminate the IDE and restart the installer.';

  RsInstallMode = 'New installation / Upgrade from an older version|Change package selection.';
  RsUpdateMode = 'Compile already installed packages / Update IDE|';
  RsUninstallMode = 'Uninstall JVCL';

const
  sWelcomeFilename = '%s\Install\JVCLInstall\welcome.txt';

  // sJclRootDirXx is used to find the "$(JVCL)\..\JCL" directory
  sJclRootDirName = '%s\Jcl';                                  // do not localize
  sJclRootDirFromJVCLDir = sJclRootDirName + '\Source\Common'; // do not localize

 // PgUninstall.pas
resourcestring
  RsUninstallPageTitle = 'Uninstalling JVCL 3';
  RsUninstallPageSubTitle = 'Uninstalling the JVCL 3 from the selected Delphi and BCB versions.';

 // FrmDirEditBrowse.pas
resourcestring
  RsSelectJCLDir = 'Select the directory where the JCL source is.';
  RsNoDirectoryButton = 'No directory';

 // FrmInstall.pas
resourcestring
  RsErrorOpeningFile = 'Error opening the file.';
  RsInstallError = 'An error occured.';
  RsComplete = 'Complete.';
  RsError = '%s - Error';
  RsCompiling = 'Compiling: %s';

 // FrmPackageSelection.pas
resourcestring
  RsSelectTargetIDE = 'Select an IDE';
  RsPkgInfoRequires = 'Requires:';
  RsPkgInfoContains = 'Contains:';

 // FrmUninstall.pas
resourcestring
  RsDeletingFile = 'Deleting %s';

 // PgInstall.pas
resourcestring
  RsInstallPageTitle = 'Compiling packages';
  RsInstallPageSubTitle = 'The selected packages are compiling for the selected Delphi and BCB versions.';

 // PgPackageSelection.pas
resourcestring
  RsPackageSelectionPageTitle = 'Select packages';
  RsPackageSelectionPageSubTitle = 'Select all packages for the target IDEs which should be installed.';

 // PgSummary.pas
resourcestring
  RsSummaryPageTitle = 'Summary';
  RsSummaryPageSubTitleInstall = 'The following actions will be done through installation.';
  RsSummaryPageSubTitleUpdate = 'The following actions will be done through the update.';
  RsSummaryPageSubTitleUninstall = 'The following actions will be done through uninstallation.';

  RsInstallForTarget = 'Install JVCL 3 for';
  RsInstallForFrameworks = 'Install for frameworks:';
  RsBplOutputDirectory = 'BPL output directory:';
  RsDcpOutputDirectory = 'DCP output directory:';
  RsLibOutputDirectory = 'LIB output directory:';
  RsHppOutputDirectory = 'HPP output directory:';
  RsBuildPackages = 'Build packages';
  RsCompilePackages = 'Compile packages';
  RsCleanComponentPalettes = 'Clean component palettes';
  RsAddToBrowsePath = 'Add to browse path:';
  RsAddToSearchPath = 'Add to search path:';
  RsAddToLibraryPath = 'Add to library path:';
  RsAddToIncludePath = 'Add to include path:';

  RsUninstallFromTarget = 'Uninstall from';
  RsRemove = 'Remove';
  RsUnregister = 'Unregister';
  RsJVCLPalettes = 'JVCL palettes';
  RsJVCLDirsFromPathLists = 'JVCL directories from path lists';
  RsJVCLPackages = 'JVCL 3 packages';
  RsJVCLFiles = 'JVCL 3 files';

const
  // sJVCLMacroXxx are displayed in the Summary page. They are not used by code.
  sJVCLMacroCommonDir = '$(JVCL)\common';             // do not localize
  sJVCLMacroRunDir = '$(JVCL)\run';                   // do not localize
  sJVCLMacroClxDirs = '$(JVCL)\qcommon;$(JVCL)\qrun'; // do not localize


 // PgConfig.pas
resourcestring
  RsConfigPageTitle = 'Configuration';
  RsConfigPageSubTitle = 'Choose the compilation options and global options for all targets';

 // FrmConfigPage.pas
resourcestring
  RsAllTargets = 'All versions';
  RsCannotOpen = 'Cannot open %s';
  
const
  SInstallHTM = 'help\install.htm'; // do not localize
  SBCBGuideAnchor = 'AddJVCLPathToBCB'; // do not localize

 // PgIDESelection.pas
resourcestring
  RsSelectionPageTitle = 'Choose IDE targets';
  RsSelectionPageSubTitleInstall = 'Select all target IDEs where the JVCL should be installed.';
  RsSelectionPageSubTitleUpdate = 'Select all target IDEs where the JVCL should be updated.';
  RsSelectionPageSubTitleUninstall = 'Select all target IDEs from which the JVCL should be uninstalled.';

  RsErrorInstallingJCL = 'Cannot start the JCL Installer.';
  RsDelphiBCBUpdateRequired = 'Delphi/BCB update required';
  RsDownloadUpdatesFrom = 'Download from';
  RsJCLVersionRequired = 'JCL 1.9 or higher required';
  RsDownloadOrSelectJclDir = 'Download or select a JCL directory.|' +
    'http://homepages.borland.com/jedi/jcl/'; // http://jcl.sourceforge.net
  RsInstalledJVCLVersion = 'installed JVCL version: %d';
  RsInstallJCL = 'Install JCL';
  RsNoJclVersionFound = 'No JCL 1.9 found.';

  RsJCLDirectoryCaption = '&JCL directory:';
  RsDeleteJVCLFilesCaption = '&Delete JVCL files (dcu,bpl,dcp, obj,bpi,lib,tds)';

 // Main.pas
resourcestring
  RsBtnInstall = '&Install';
  RsBtnUninstall = '&Uninstall';
  RsNoPackageInstaller = 'Application error. No PackageInstaller created.';
  RsCancelInstallation = 'Do you really want to cancel the installation?';

  RsJediHomepage = 'http://projectjedi.sourceforge.net';
  RsJVCLHomepage = 'http://jvcl.sourceforge.net';


implementation

end.
