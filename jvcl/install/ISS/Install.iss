; Script created by Andreas Hausladen (Andreas.Hausladen@gmx.de) for the JVCL
;
; CONDITIONAL COMPILATION
;    Include_Binaries    Create an installer that can install a precompiled JVCL
;    Include_Examples    Add the Examples directory to the installer (user can then select the component)
;    DEBUGGING           Development. Will only use Delphi 5 BPLs as files with a fast compression (script debugging)
;    Include_DelphiX     Include the binaries for Delphi X (X in 5..12)
;    Include_BCBX        Include the binaries for C++Builder X (X in 5..6)

#define JvclVersionStr "3.39.0.1"
#define JclVersionStr "2.0.1.3449"
#define MyAppId "Jedi Visual Component Library"
#define MyAppName "JEDI Visual Component Library"
#define MyAppVerName "JEDI Visual Component Library " + JvclVersionStr
#define MyAppPublisher "JVCL Team"
#define MyAppURL "http://jvcl.sourceforge.net"
#define downloadurl "http://jvcl.sourceforge.net/websetup/jvcl"

#define Include_Binaries
#define Include_Examples
;#define DEBUGGING

#define Include_SingleIDE
#define Include_Delphi14

#ifdef DEBUGGING
 #define Include_SingleIDE
 #define Include_Delphi14
 #undef Include_Examples
#endif

#ifdef Include_Binaries
 #ifndef Include_SingleIDE
;  #define Include_BCB6
  #define Include_Delphi6
  #define Include_Delphi7
  #define Include_Delphi9
  #define Include_Delphi10
  #define Include_Delphi11
  #define Include_Delphi12
  #define Include_Delphi14
 #endif
#endif

;---------------------------------------------------
#include "SourceDirectories.iss"

#define Delphi5Root BorlandRoot + "\Delphi5"
#define   Delphi5Bpl Delphi5Root + "\Projects\Bpl"
#define   Delphi5Dcp Delphi5Bpl
#define BCB5Root BorlandRoot + "\CBuilder5"
#define   BCB5Bpl BCB5Root + "\Projects\Bpl"
#define   BCB5Dcp BCB5Bpl
#define Delphi6Root BorlandRoot + "\Delphi6"
#define   Delphi6Bpl Delphi6Root + "\Projects\Bpl"
#define   Delphi6Dcp Delphi6Bpl
#define BCB6Root BorlandRoot + "\CBuilder6"
#define   BCB6Bpl BCB6Root + "\Projects\Bpl"
#define   BCB6Dcp BCB6Bpl
#define Delphi7Root BorlandRoot + "\Delphi7"
#define   Delphi7Bpl Delphi7Root + "\Projects\Bpl"
#define   Delphi7Dcp Delphi7Bpl
#define Delphi9Root BorlandRoot + "\BDS\3.0"
#define   Delphi9Bpl BorlandSudioProjects + "\Bpl"
#define Delphi10Root BorlandRoot + "\BDS\4.0"
#define   Delphi10Bpl BorlandSudioProjects + "\Bpl"
#define Delphi11Root CodeGearRoot + "\RAD Studio\5.0"
#define   Delphi11Bpl CommonDocs + "\RAD Studio\5.0\Bpl"
#define   Delphi11HPP CommonDocs + "\RAD Studio\5.0\HPP"
#define Delphi12Root CodeGearRoot + "\RAD Studio\6.0"
#define   Delphi12Bpl CommonDocs + "\RAD Studio\6.0\Bpl"
#define   Delphi12HPP CommonDocs + "\RAD Studio\6.0\HPP"
#define Delphi14Root EmbtRoot + "\RAD Studio\7.0"
#define   Delphi14Bpl CommonDocs + "\RAD Studio\7.0\Bpl"
#define   Delphi14HPP CommonDocs + "\RAD Studio\7.0\HPP"

[Setup]
AppId={#MyAppId}
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppVersion={#JvclVersionStr}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\DelphiComponents\JVCL
DefaultGroupName=DelphiComponents\JEDI Visual Component Library
DisableProgramGroupPage=no
;LicenseFile={#JvclRoot}\help\MPL-1.1.html
OutputBaseFilename=JVCLSetup
PrivilegesRequired=none
#ifdef DEBUGGING
Compression=zip/1
#else
Compression=lzma/ultra64
#endif
SolidCompression=yes
ShowLanguageDialog=auto
OptimizedChecks=yes
;WebSetupUpdateURL={#downloadurl}

// for skin
#define MyWizardBottomImageFile "Skin\images\wizardbottom.bmp"
#define MyWizardButtonImageFile "Skin\images\button.bmp"
#define MyWizardImageFile "wizard.bmp"
#define MyWizardSmallImageFile "wizardsmall.bmp"
WizardImageFile=Skin\images\{#MyWizardImageFile}
WizardSmallImageFile=Skin\images\{#MyWizardSmallImageFile}
#include "Skin\isxskin.iss"

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"


#ifdef Include_Binaries
[Types]
Name: "full"; Description: "Full installation"
Name: "compact"; Description: "Source only installation"
Name: "custom"; Description: "Custom installation"; Flags: iscustom
Name: "prefered"; Description: "Prefered installation"
#endif

[Components]
#ifdef Include_Examples
Name: "Examples"; Description: "Example projects"; Types: full;
#endif

#ifdef Include_Binaries
[Packages]
Name: examples; Description: "JVCL Examples"; Source: "{#downloadurl}/jvclexamples.isz"; Flags: localcopy;

Name: delphi6; Description: "Delphi 6 binary files"; Source: "{#downloadurl}/jvcldelphi6.isz"; Flags: localcopy;
Name: bcb6; Description: "C++Builder 6 binary files"; Source: "{#downloadurl}/jvclbcb6.isz"; Flags: localcopy;
Name: delphi7; Description: "Delphi 7 binary files"; Source: "{#downloadurl}/jvcldelphi7.isz"; Flags: localcopy;
Name: delphi9; Description: "Delphi 2005 binary files"; Source: "{#downloadurl}/jvcldelphi9.isz"; Flags: localcopy;
Name: delphi10; Description: "Delphi/C++Builder 2006 binary files"; Source: "{#downloadurl}/jvcldelphi10.isz"; Flags: localcopy;
Name: delphi11; Description: "Delphi/C++Builder 2007 binary files"; Source: "{#downloadurl}/jvcldelphi11.isz"; Flags: localcopy;
Name: delphi12; Description: "Delphi/C++Builder 2009 binary files"; Source: "{#downloadurl}/jvcldelphi12.isz"; Flags: localcopy;
Name: delphi14; Description: "Delphi/C++Builder 2010 binary files"; Source: "{#downloadurl}/jvcldelphi14.isz"; Flags: localcopy;

#include "IdeComponents.iss"

[Components]
; Package selection
Name: "Packages"; Description: "Register Designtime Packages"; Types: full prefered
Name: "Packages\JvCore"; Description: "Core (Property editors, AppStorage)"; Types: full prefered
Name: "Packages\JvCmp"; Description: "Non-visual Components (PageManager, DataEmbedded, StrHolder, MouseGesture)"; Types: full prefered
Name: "Packages\JvCtrls"; Description: "Controls (Buttons, ComboBox, ListBox, Labels, Splitter, Hint, Bars)"; Types: full prefered
Name: "Packages\JvCustom"; Description: "Custom Controls (Edits, Outlook bar, TabBar, Labels, TrayIcon, Timeline)"; Types: full prefered
Name: "Packages\JvStdCtrls"; Description: "Standard Controls (improved standard controls, BrowseDirDlg)"; Types: full prefered
Name: "Packages\JvPageComps"; Description: "Page Controls (Navigation Pane, PageList, PageListTreeView)"; Types: full prefered
Name: "Packages\JvSystem"; Description: "System Components (FormStorage, SimpleXML, Clipboard, Drag&Drop, MRU)"; Types: full prefered
Name: "Packages\JvValidators"; Description: "Visual Control Validator"; Types: full prefered
Name: "Packages\JvNet"; Description: "Network Components (HTTP/FTP/Local Grabber, ProgramVersionCheck)"; Types: full prefered
Name: "Packages\JvXPCtrls"; Description: "XP Controls (XPBar, Button in XP style)"; Types: full prefered
Name: "Packages\JvDocking"; Description: "Docking Components"; Types: full prefered
Name: "Packages\JvWizard"; Description: "Wizard Controls"; Types: full prefered

Name: "Packages\DB"; Description: "Database Packages"
Name: "Packages\DB\JvDB"; Description: "DB-Aware Controls (Grids, Lookups, ComboBox, TreeViews, Edits)"; Types: full prefered
Name: "Packages\DB\JvBDE"; Description: "BDE Component"; Types: full

Name: "Packages\JvAppFrm"; Description: "Application/Form Components (HotKey, FormMagnet, EmbeddedForms, Wallpaper)"; Types: full
Name: "Packages\JvMM"; Description: "Multimedia and Images (Animated Images, ID3v1, WavePlayer, WaitingProgress)"; Types: full
Name: "Packages\JvTimeframework"; Description: "Time Framework Controls"; Types: full
Name: "Packages\JvCrypt"; Description: "Encryption and Compression Components"; Types: full
Name: "Packages\JvHMI"; Description: "HMI Controls"; Types: full
Name: "Packages\JvInterpreter"; Description: "Pascal Interpreter Components"; Types: full
Name: "Packages\JvManagedThreads"; Description: "Managed Threads Components"; Types: full
Name: "Packages\JvPrintPreview"; Description: "Print Preview Components"; Types: full
Name: "Packages\JvRuntimeDesign"; Description: "Runtime Design Components"; Types: full
Name: "Packages\JvPlugin"; Description: "Plugin System"; Types: full
Name: "Packages\JvBands"; Description: "Band Objects"; Types: full
Name: "Packages\JvDlgs"; Description: "Dialog Components"; Types: full

Name: "Packages\Obsolete"; Description: "Obsolete Components"
Name: "Packages\Obsolete\JvGlobus"; Description: "Globus Components"
Name: "Packages\Obsolete\JvJans"; Description: "Jans Components"
Name: "Packages\Obsolete\JvDotNetCtrls"; Description: "DotNet Controls"

; Options
Name: "Options"; Description: "Options"; Types: full prefered custom compact; Flags: fixed
Name: "Options\RegisterGlobalDesignEditors"; Description: "Register global design editors"; Types: full prefered; Flags: dontinheritcheck

#endif

[Dirs]
Name: "{app}\bin"
Name: "{app}\dcu"
; DCU/OBJ output directories
Name: "{app}\lib\d6"
Name: "{app}\lib\d6\debug"
Name: "{app}\lib\d7"
Name: "{app}\lib\d7\debug"
Name: "{app}\lib\d9"
Name: "{app}\lib\d9\debug"
Name: "{app}\lib\d10"
Name: "{app}\lib\d10\debug"
Name: "{app}\lib\d11"
Name: "{app}\lib\d11\debug"
Name: "{app}\lib\d12"
Name: "{app}\lib\d12\debug"
Name: "{app}\lib\d14"
Name: "{app}\lib\d14\debug"


[Files]
Source: {#JvclRoot}\changelog.txt; DestDir: "{app}"; Flags: ignoreversion
Source: {#JvclRoot}\readme.htm; DestDir: "{app}"; Flags: ignoreversion
Source: {#JvclRoot}\clean.bat; DestDir: "{app}"; Flags: ignoreversion
Source: {#JvclRoot}\install.bat; DestDir: "{app}"; Flags: ignoreversion
Source: {#JvclRoot}\makemodified.bat; DestDir: "{app}"; Flags: ignoreversion
#ifndef DEBUGGING
Source: {#JvclRoot}\common\*.inc; DestDir: "{app}\common"; Flags: ignoreversion
Source: {#JvclRoot}\run\*.pas; DestDir: "{app}\run"; Flags: ignoreversion
Source: {#JvclRoot}\run\*.dfm; DestDir: "{app}\run"; Flags: ignoreversion
Source: {#JvclRoot}\design\*.pas; DestDir: "{app}\design"; Flags: ignoreversion
Source: {#JvclRoot}\design\*.dfm; DestDir: "{app}\design"; Flags: ignoreversion
Source: {#JvclRoot}\packages\*; DestDir: "{app}\packages"; Excludes: ".svn,__history,*.drc"; Flags: ignoreversion sortfilesbyextension recursesubdirs
Source: {#JvclRoot}\install\JVCLInstall\*; DestDir: "{app}\install\JVCLInstall"; Excludes: ".svn,__history"; Flags: ignoreversion recursesubdirs sortfilesbyextension
Source: {#JvclRoot}\devtools\*; DestDir: "{app}\devtools"; Excludes: ".svn,__history"; Flags: ignoreversion sortfilesbyextension recursesubdirs
Source: {#JvclRoot}\images\*; DestDir: "{app}\images"; Excludes: ".svn,__history,*.txt"; Flags: ignoreversion sortfilesbyextension
Source: {#JvclRoot}\resources\*; DestDir: "{app}\resources"; Excludes: ".svn,__history,*.txt"; Flags: ignoreversion sortfilesbyextension
Source: {#JvclRoot}\locale\*; DestDir: "{app}\locale"; Excludes: ".svn,__history"; Flags: ignoreversion recursesubdirs sortfilesbyextension
Source: {#JvclRoot}\help\*; DestDir: "{app}\help"; Excludes: ".svn,__history"; Flags: ignoreversion recursesubdirs sortfilesbyextension
Source: {#JvclRoot}\dict\*; DestDir: "{app}\dict"; Excludes: ".svn,__history"; Flags: ignoreversion recursesubdirs
Source: {#JvclRoot}\converter\*; DestDir: "{app}\converter"; Excludes: ".svn,__history"; Flags: ignoreversion
#endif

#ifdef Include_Examples
; SolidBreak
Source: {#JvclRoot}\examples\*; DestDir: "{app}\examples"; Excludes: ".svn,__history"; Components: "Examples"; Package: examples; Flags: ignoreversion recursesubdirs sortfilesbyextension solidbreak
#endif

#ifdef Include_Binaries
#ifdef Include_Delphi6
; SolidBreak; lib\Delphi 6
Source: {#JvclRoot}\lib\d6\*; DestDir: "{app}\lib\d6"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi6"; Package: delphi6; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi6Bpl}\Jv*.bpl; DestDir: "{code:GetDelphiBplDir|6}"; Components: "IDE\Delphi6"; Package: delphi6; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_BCB6
; SolidBreak; lib\C++Builder 6
Source: {#JvclRoot}\lib\c6\*; DestDir: "{app}\lib\c6"; Excludes: ".svn,__history,*.txt"; Components: "IDE\BCB6"; Package: bcb6; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#BCB6Root}\Include\Vcl\Jv*.hpp; DestDir: "{code:GetBCBDir|6}\Include\Vcl"; Components: "IDE\BCB6"; Package: bcb6; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#BCB6Bpl}\Jv*.bpl; DestDir: "{code:GetBCBBplDir|6}"; Components: "IDE\BCB6"; Package: bcb6; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi7
; SolidBreak; lib\Delphi 7
Source: {#JvclRoot}\lib\d7\*; DestDir: "{app}\lib\d7"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi7"; Package: delphi7; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi7Bpl}\Jv*7?.bpl; DestDir: "{code:GetDelphiBplDir|7}"; Components: "IDE\Delphi7"; Package: delphi7; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi9
; SolidBreak; lib\Delphi 2005
Source: {#JvclRoot}\lib\d9\*; DestDir: "{app}\lib\d9"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi9"; Package: delphi9; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi9Bpl}\Jv*9?.bpl; DestDir: "{code:GetDelphiBplDir|9}"; Components: "IDE\Delphi9"; Package: delphi9; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi10
; SolidBreak; lib\Delphi 2006
Source: {#JvclRoot}\lib\d10\*; DestDir: "{app}\lib\d10"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi10"; Package: delphi10; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi10Bpl}\Jv*10?.bpl; DestDir: "{code:GetDelphiBplDir|10}"; Components: "IDE\Delphi10"; Package: delphi10; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#Delphi10Root}\Include\Vcl\Jv*.hpp; DestDir: "{code:GetDelphiDir|10}\Include\Vcl"; Package: delphi10; Components: "IDE\Delphi10"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi11
; SolidBreak; lib\Delphi 2007
Source: {#JvclRoot}\lib\d11\*; DestDir: "{app}\lib\d11"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi11"; Package: delphi11; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi11Bpl}\Jv*11?.bpl; DestDir: "{code:GetDelphiBplDir|11}"; Components: "IDE\Delphi11"; Package: delphi11; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
;Source: {#Delphi11Root}\Include\Vcl\Jv*.hpp; DestDir: "{code:GetDelphiDir|11}\Include\Vcl"; Components: "IDE\Delphi11"; Package: delphi11; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi11HPP}\Jv*.hpp; DestDir: "{code:GetHPPDir|11}"; Components: "IDE\Delphi11"; Package: delphi11; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi12
; SolidBreak; lib\Delphi 2009
Source: {#JvclRoot}\lib\d12\*; DestDir: "{app}\lib\d12"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi12"; Package: delphi12; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi12Bpl}\Jv*12?.bpl; DestDir: "{code:GetDelphiBplDir|12}"; Components: "IDE\Delphi12"; Package: delphi12; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
;Source: {#Delphi12Root}\Include\Vcl\Jv*.hpp; DestDir: "{code:GetDelphiDir|12}\Include\Vcl"; Components: "IDE\Delphi12"; Package: delphi12; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi12HPP}\Jv*.hpp; DestDir: "{code:GetHPPDir|12}"; Components: "IDE\Delphi12"; Package: delphi12; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi14
; SolidBreak; lib\Delphi 2010
Source: {#JvclRoot}\lib\d14\*; DestDir: "{app}\lib\d14"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi14"; Package: delphi14; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi14Bpl}\Jv*140.bpl; DestDir: "{code:GetDelphiBplDir|14}"; Components: "IDE\Delphi14"; Package: delphi14; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
;Source: {#Delphi14Root}\Include\Vcl\Jv*.hpp; DestDir: "{code:GetDelphiDir|14}\Include\Vcl"; Components: "IDE\Delphi14"; Package: delphi14; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#Delphi14HPP}\Jv*.hpp; DestDir: "{code:GetHPPDir|14}"; Components: "IDE\Delphi14"; Package: delphi14; Flags: ignoreversion sortfilesbyextension
#endif
#endif

; only source code => execute JVCL Installer
[Run]
Filename: {app}\install.bat; Description: "Execute JVCL Installer"; Flags: postinstall shellexec; Check: IsSourceInstall;

#ifdef Include_Binaries
[Registry]
#ifdef Include_Delphi5
; Delphi 5
Root: HKCU; Subkey: "{code:GetDelphiRegKey|5}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|5}; Components: "IDE\Delphi5"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(5)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|5}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d5; Components: "IDE\Delphi5"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(5)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|5}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi5"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(5)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|5}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi5"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(5)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|5}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(5)
#endif
#ifdef Include_BCB5
; BCB 5
Root: HKCU; Subkey: "{code:GetBCBRegKey|5}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetBCBBplDir|5}; Components: "IDE\BCB5"; Flags: uninsdeletevalue
Root: HKCU; Subkey: "{code:GetBCBRegKey|5}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\c5; Components: "IDE\BCB5"; Flags: uninsdeletevalue
Root: HKCU; Subkey: "{code:GetBCBRegKey|5}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\BCB5"; Flags: uninsdeletevalue
Root: HKCU; Subkey: "{code:GetBCBRegKey|5}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\BCB5"; Flags: uninsdeletevalue
Root: HKCU; Subkey: "{code:GetBCBRegKey|5}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue
#endif
#ifdef Include_Delphi6
; Delphi 6
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|6}; Components: "IDE\Delphi6"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(6)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d6; Components: "IDE\Delphi6"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(6)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi6"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(6)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi6"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(6)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(6)
#endif
#ifdef Include_BCB6
; BCB 6
Root: HKCU; Subkey: "{code:GetBCBRegKey|6}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetBCBBplDir|6}; Components: "IDE\BCB6"; Flags: uninsdeletevalue
Root: HKCU; Subkey: "{code:GetBCBRegKey|6}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\c6; Components: "IDE\BCB6"; Flags: uninsdeletevalue
Root: HKCU; Subkey: "{code:GetBCBRegKey|6}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\BCB6"; Flags: uninsdeletevalue
Root: HKCU; Subkey: "{code:GetBCBRegKey|6}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\BCB6"; Flags: uninsdeletevalue
Root: HKCU; Subkey: "{code:GetBCBRegKey|6}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue
#endif
#ifdef Include_Delphi7
; Delphi 7
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|7}; Components: "IDE\Delphi7"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(7)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d7; Components: "IDE\Delphi7"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(7)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi7"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(7)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi7"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(7)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(7)
#endif
#ifdef Include_Delphi9
; Delphi 2005
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|9}; Components: "IDE\Delphi9"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(9)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d9; Components: "IDE\Delphi9"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(9)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi9"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(9)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi9"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(9)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(9)
#endif
#ifdef Include_Delphi10
; Delphi 2006
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|10}; Components: "IDE\Delphi10"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(10)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d10; Components: "IDE\Delphi10"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(10)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi10"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(10)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi10"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(10)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(10)
#endif
#ifdef Include_Delphi11
; Delphi 2007
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|11}; Components: "IDE\Delphi11"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(11)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d11; Components: "IDE\Delphi11"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(11)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi11"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(11)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi11"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(11)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(11)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi11"; Check: IsDelphiInstalled(11)
#endif
#ifdef Include_Delphi12
; Delphi 2009
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|12}; Components: "IDE\Delphi12"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(12)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d12; Components: "IDE\Delphi12"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(12)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi12"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(12)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi12"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(12)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(12)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi12"; Check: IsDelphiInstalled(12)
#endif
#ifdef Include_Delphi14
; Delphi 2010
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|14}; Components: "IDE\Delphi14"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(14)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d14; Components: "IDE\Delphi14"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(14)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi14"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(14)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi14"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(14)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue; Check: IsDelphiInstalled(14)
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi14"; Check: IsDelphiInstalled(14)
#endif
#endif


[UninstallDelete]
Type: files; Name: "{app}\dcu\*"
Type: files; Name: "{app}\bin\JVCLInstall.*"
Type: files; Name: "{app}\run\*.hpp"
Type: files; Name: "{app}\common\*.hpp"
; lib\C++Builder 6
Type: files; Name: "{app}\lib\c6\*"
Type: files; Name: "{code:GetBCBBplDir|6}\Jv*.*"
Type: files; Name: "{code:GetBCBDir|6}\Include\Vcl\Jv*.hpp"
; lib\Delphi 6
Type: files; Name: "{app}\lib\d6\*"
Type: files; Name: "{app}\lib\d6\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|6}\Jv*.*"
; lib\Delphi 7
Type: files; Name: "{app}\lib\d7\*"
Type: files; Name: "{app}\lib\d7\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|7}\Jv*.*"
; lib\Delphi 2005
Type: files; Name: "{app}\lib\d9\*"
Type: files; Name: "{app}\lib\d9\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|9}\Jv*.*"
; lib\Delphi/C++Builder 2006
Type: files; Name: "{app}\lib\d10\*"
Type: files; Name: "{app}\lib\d10\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|10}\Jv*.*"
Type: files; Name: "{code:GetDelphiDir|10}\Include\Vcl\Jv*.hpp"
; lib\Delphi/C++Builder 2007
Type: files; Name: "{app}\lib\d11\*"
Type: files; Name: "{app}\lib\d11\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|11}\Jv*.*"
Type: files; Name: "{code:GetDelphiDir|11}\Include\Vcl\Jv*.hpp"
; lib\Delphi/C++Builder 2009
Type: files; Name: "{app}\lib\d12\*"
Type: files; Name: "{app}\lib\d12\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|12}\Jv*.*"
Type: files; Name: "{code:GetDelphiDir|12}\Include\Vcl\Jv*.hpp"
; lib\Delphi/C++Builder 2010
Type: files; Name: "{app}\lib\d14\*"
Type: files; Name: "{app}\lib\d14\debug\*"
Type: files; Name: "{code:GetDelphiBplDir|14}\Jv*.*"
Type: files; Name: "{code:GetDelphiDir|14}\Include\Vcl\Jv*.hpp"

[Icons]
Name: "{group}\{cm:ProgramOnTheWeb,{#MyAppName}}"; Filename: "{#MyAppURL}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"

#include "ComponentInstallerScript.iss"

[Code]
// callbacks for the ComponentInstaller

procedure UserRegisterComponents(Components: TStrings);
begin
end;

procedure UserUnregisterComponents(Components: TStrings);
// uninstall all JVCL packages, not only the one that the user had selected
// during the installation. The user could have started the JVCLInstaller
// or have added additional designtime packages by hand.
var
  IdeList: TStrings;
  IdeIndex: Integer;
  IdeKind: TIdeKind;
  Version: Integer;
begin
{  // Uninstall from all IDEs ?
  for Version := 6 to 14 do
    UninstallDesignPackagesPrefixed(ikDelphi, Version, 'Jv');
  for Version := 6 to 6 do
    UninstallDesignPackagesPrefixed(ikBCB, Version, 'Jv');}

  IdeList := TStringList.Create;
  try
    GetSelectedList(IdeList, 'IDE', Components);
    // unregister per IDE
    for IdeIndex := 0 to IdeList.Count - 1 do
    begin
      ExtractIdeInfo(IdeList[IdeIndex], IdeKind, Version);
      UninstallDesignPackagesPrefixed(IdeKind, Version, 'Jv');
    end;
  finally
    IdeList.Free;
  end;
end;

function MapExpert(IdeKind: TIdeKind; Version: Integer; const ExpertName: string): string;
begin
  Result := '';
end;

function MapDesignPackage(IdeKind: TIdeKind; Version: Integer; const PackageName: string): string;
begin
  Result := '';
  if StartsText('Jv', PackageName) then
  begin
    case IdeKind of
      ikDelphi:
        Result := GetDelphiBplDir(IntToStr(Version)) + '\' + PackageName + 'Design' + IntToStr(Version) + '.bpl';
      ikBCB:
        Result := GetBCBBplDir(IntToStr(Version)) + '\' + PackageName + 'Design' + IntToStr(Version) + '.bpl';
    end;
  end;
end;

procedure GetSearchPaths(IdeKind: TIdeKind; Version: Integer; var SearchPaths, DebugPaths, BrowsePaths, IncludePaths: string);
var
  LibDir, AppDir, ResDir: string;
begin
  AppDir := ExpandConstant('{app}');
  SearchPaths := '';
  DebugPaths := '';
  BrowsePaths := '';
  IncludePaths := '';
  case IdeKind of
    ikDelphi:
      LibDir := AppDir + '\lib\d' + IntToStr(Version);
    ikBCB:
      LibDir := AppDir + '\lib\c' + IntToStr(Version);
  else
    Exit;
  end;
  ResDir := AppDir + '\resources';

  SearchPaths := LibDir + ';' + ResDir + ';' + AppDir + '\common';
  DebugPaths := LibDir + '\debug';
  BrowsePaths := AppDir + '\common;' + AppDir + '\run';
  if Version >= 10 then
    IncludePaths := GetHPPDir(IntToStr(Version));
end;

function IsDelphiJclInstalled(Version: Integer): Boolean;
var
  JclVersion: string;
begin
  Result := IsDelphiInstalled(Version);
  if Result then
  begin
    if RegQueryStringValue(HKEY_CURRENT_USER, GetDelphiRegKey(IntToStr(Version)) + '\Jedi\JCL', 'Version', JclVersion) then
      Result := StartsText('{#JclVersionStr}', JclVersion)
    else
      Result := False;
  end;
end;

// events

function InitializeSetup(): Boolean;
var
  Version: Integer;
begin
  Result := InitComponentInstaller;
  
  // Check for an installed JCL version that matches the {#JclVersionStr}
  if Result then
  begin
    Result := False;
    for Version := 6 to 14 do
    begin
      if IsDelphiJclInstalled(Version) then
      begin
        Result := True;
        Break;
      end;
    end;
    if not Result then
      MsgBox('No JCL installed or the installed JCL version doesn''t match "{#JclVersionStr}*".'#10#10 +
             'Please install the matching JCL first and then restart the JVCL Installation', mbError, MB_OK);
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssPostInstall  then
    RegisterComponents;
end;

function InitializeUninstall(): Boolean;
begin
  Result := InitComponentUninstaller;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  if CurUninstallStep = usUninstall then
    UnregisterComponents;
end;

// Skin

procedure CurPageChanged(CurPageID: Integer);
begin
  // update calls for skin
  UpdateButton(WizardForm.BackButton, bidBack);
  UpdateButton(WizardForm.NextButton, bidNext);
  UpdateButton(WizardForm.CancelButton, bidCancel);
end;

procedure InitializeWizard();
begin
  // initialize call for skin
  InitializeSkin;
end;
