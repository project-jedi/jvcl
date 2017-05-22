; Script created by Andreas Hausladen (Andreas.Hausladen@gmx.de) for the JVCL
;
; CONDITIONAL COMPILATION
;    Include_Binaries    Create an installer that can install a precompiled JVCL
;    Include_Examples    Add the Examples directory to the installer (user can then select the component)
;    DEBUGGING           Development. Will only use Delphi 5 BPLs as files with a fast compression (script debugging)
;    Include_DelphiX     Include the binaries for Delphi X (X in 5..12)

#ifndef CmdLineBuild
#define JvclRoot "..\.."
#define JvclLib "setupbuild\lib"
#define JvclBpl "setupbuild\bpl"
#define JvclHpp "setupbuild\hpp"
#define DEBUGGING
#endif

#define Include_SingleIDE
#define Include_Binaries
#define Include_Examples

#include "Settings.iss"
; get the JCL version
#include "..\..\..\JclInnoSetup\Settings.iss"

#define MyAppId "Jedi Visual Component Library"
#define MyAppName "JEDI Visual Component Library"
#define MyAppVerName "JEDI Visual Component Library " + JvclVersionStr
#define MyAppPublisher "JVCL Team"
#define MyAppURL "http://jvcl.sourceforge.net"

;---------------------------------------------------
; Setup the preprocessor defines for the binary files
#ifdef Include_SingleIDE

#define JvclLib6     JvclLib
#define   JvclBpl6   JvclBpl
#define JvclLib7     JvclLib
#define   JvclBpl7   JvclBpl
#define JvclLib9     JvclLib
#define   JvclBpl9   JvclBpl
#define JvclLib10    JvclLib
#define   JvclBpl10  JvclBpl
#define   JvclHpp10  JvclHpp
#define JvclLib11    JvclLib
#define   JvclBpl11  JvclBpl
#define   JvclHpp11  JvclHpp
#define JvclLib12    JvclLib
#define   JvclBpl12  JvclBpl
#define   JvclHpp12  JvclHpp
#define JvclLib14    JvclLib
#define   JvclBpl14  JvclBpl
#define   JvclHpp14  JvclHpp
#define JvclLib15    JvclLib
#define   JvclBpl15  JvclBpl
#define   JvclHpp15  JvclHpp
#define JvclLib16    JvclLib
#define   JvclBpl16  JvclBpl
#define   JvclHpp16  JvclHpp
#define JvclLib17    JvclLib
#define   JvclBpl17  JvclBpl
#define   JvclHpp17  JvclHpp

#endif

;---------------------------------------------------

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
;PrivilegesRequired=none
#ifdef DEBUGGING
Compression=zip/1
#else
Compression=lzma/ultra64
#endif
SolidCompression=yes
ShowLanguageDialog=auto
OptimizedChecks=yes

; for skin
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

#include "IdeComponents.iss"

[Components]
; Package selection
Name: "Packages"; Description: "Register Designtime Packages"; Types: full prefered
Name: "Packages\JvCore"; Description: "Core (Property editors, AppStorage)"; Types: full prefered
Name: "Packages\JvCmp"; Description: "Non-visual Components (PageManager, DataEmbedded, StrHolder, MouseGesture)"; Types: full prefered
Name: "Packages\JvControls"; Description: "Controls (Buttons, ComboBox, ListBox, Labels, Splitter, Hint, Bars)"; Types: full prefered
Name: "Packages\JvCustom"; Description: "Custom Controls (Edits, Outlook bar, TabBar, Labels, TrayIcon, Timeline)"; Types: full prefered
Name: "Packages\JvStdCtrls"; Description: "Standard Controls (improved standard controls, BrowseDirDlg)"; Types: full prefered
Name: "Packages\JvPageComps"; Description: "Page Controls (Navigation Pane, PageList, PageListTreeView)"; Types: full prefered
Name: "Packages\JvSystem"; Description: "System Components (FormStorage, SimpleXML, Clipboard, Drag&Drop, MRU)"; Types: full prefered
Name: "Packages\JvAppFrm"; Description: "Application/Form Components (HotKey, FormMagnet, EmbeddedForms, Wallpaper)"; Types: full prefered
Name: "Packages\JvValidators"; Description: "Visual Control Validator"; Types: full prefered
Name: "Packages\JvNet"; Description: "Network Components (HTTP/FTP/Local Grabber, ProgramVersionCheck)"; Types: full prefered
Name: "Packages\JvXPCtrls"; Description: "XP Controls (XPBar, Button in XP style)"; Types: full prefered
Name: "Packages\JvDocking"; Description: "Docking Components"; Types: full prefered
Name: "Packages\JvWizards"; Description: "Wizard Controls"; Types: full prefered

Name: "Packages\DB"; Description: "Database Packages"
Name: "Packages\DB\JvDB"; Description: "DB-Aware Controls (Grids, Lookups, ComboBox, TreeViews, Edits)"; Types: full prefered
Name: "Packages\DB\JvBDE"; Description: "BDE Component"; Types: full

Name: "Packages\JvMM"; Description: "Multimedia and Images (Animated Images, ID3v1, WavePlayer, WaitingProgress)"; Types: full
Name: "Packages\JvTimeFramework"; Description: "Time Framework Controls"; Types: full
Name: "Packages\JvCrypt"; Description: "Encryption and Compression Components"; Types: full
Name: "Packages\JvHMI"; Description: "HMI Controls"; Types: full
Name: "Packages\JvPascalInterpreter"; Description: "Pascal Interpreter Components"; Types: full
Name: "Packages\JvManagedThreads"; Description: "Managed Threads Components"; Types: full
Name: "Packages\JvPrintPreview"; Description: "Print Preview Components"; Types: full
Name: "Packages\JvRuntimeDesign"; Description: "Runtime Design Components"; Types: full
Name: "Packages\JvPluginSystem"; Description: "Plugin System"; Types: full
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
Name: "{app}\lib\d15"
Name: "{app}\lib\d15\debug"
Name: "{app}\lib\d16\"
Name: "{app}\lib\d16\win32"
Name: "{app}\lib\d16\win32\debug"
Name: "{app}\lib\d16\win64"
Name: "{app}\lib\d16\win64\debug"
Name: "{app}\lib\d17\win32"
Name: "{app}\lib\d17\win32\debug"
Name: "{app}\lib\d17\win64"
Name: "{app}\lib\d17\win64\debug"

[Files]
Source: {#JvclRoot}\*; DestDir: "{app}"; Flags: ignoreversion
Source: {#JvclRoot}\bin\*.csv; DestDir: "{app}\bin"; Flags: ignoreversion
Source: {#JvclRoot}\bin\*.bmp; DestDir: "{app}\bin"; Flags: ignoreversion
Source: {#JvclRoot}\bin\*.mdb; DestDir: "{app}\bin"; Flags: ignoreversion
Source: {#JvclRoot}\bin\Data\*; DestDir: "{app}\bin\Data"; Flags: ignoreversion
Source: {#JvclRoot}\common\*.inc; DestDir: "{app}\common"; Excludes: "jedi.inc"; Flags: ignoreversion
Source: {#JvclRoot}\common\jedi\*; DestDir: "{app}\common"; Flags: ignoreversion
Source: {#JvclRoot}\run\*.pas; DestDir: "{app}\run"; Flags: ignoreversion
Source: {#JvclRoot}\run\*.dfm; DestDir: "{app}\run"; Flags: ignoreversion
Source: {#JvclRoot}\design\*.pas; DestDir: "{app}\design"; Flags: ignoreversion
Source: {#JvclRoot}\design\*.dfm; DestDir: "{app}\design"; Flags: ignoreversion
Source: {#JvclRoot}\packages\*; DestDir: "{app}\packages"; Excludes: ".svn,__history,*.drc"; Flags: ignoreversion sortfilesbyextension recursesubdirs
Source: {#JvclRoot}\install\ISS\*; DestDir: "{app}\install\ISS"; Excludes: ".svn,__history"; Flags: ignoreversion sortfilesbyextension
Source: {#JvclRoot}\install\JVCLInstall\*; DestDir: "{app}\install\JVCLInstall"; Excludes: ".svn,__history"; Flags: ignoreversion recursesubdirs sortfilesbyextension
Source: {#JvclRoot}\install\release\*; DestDir: "{app}\install\release"; Excludes: ".svn,__history"; Flags: ignoreversion recursesubdirs sortfilesbyextension
Source: {#JvclRoot}\devtools\*; DestDir: "{app}\devtools"; Excludes: ".svn,__history,*.dcu,*.map,bin\*.exe"; Flags: ignoreversion sortfilesbyextension recursesubdirs
Source: {#JvclRoot}\images\*; DestDir: "{app}\images"; Excludes: ".svn,__history,*.txt"; Flags: ignoreversion sortfilesbyextension
Source: {#JvclRoot}\resources\*; DestDir: "{app}\resources"; Excludes: ".svn,__history,*.txt"; Flags: ignoreversion sortfilesbyextension
Source: {#JvclRoot}\locale\*; DestDir: "{app}\locale"; Excludes: ".svn,__history"; Flags: ignoreversion recursesubdirs sortfilesbyextension
Source: {#JvclRoot}\help\*; DestDir: "{app}\help"; Excludes: ".svn,__history"; Flags: ignoreversion recursesubdirs sortfilesbyextension
Source: {#JvclRoot}\dict\*; DestDir: "{app}\dict"; Excludes: ".svn,__history"; Flags: ignoreversion recursesubdirs
Source: {#JvclRoot}\converter\*; DestDir: "{app}\converter"; Excludes: ".svn,__history"; Flags: ignoreversion

#ifdef Include_Examples
; SolidBreak
Source: {#JvclRoot}\examples\*; DestDir: "{app}\examples"; Excludes: ".svn,__history,*.dcu,*.obj,*.exe,*.map,*.bpl,*.dcp,*.~*,*.drc,*.local"; Components: "Examples"; Flags: ignoreversion recursesubdirs sortfilesbyextension solidbreak
#endif

#ifdef Include_Binaries
#ifdef Include_Delphi6
; SolidBreak; lib\Delphi 6
Source: {#JvclLib6}\*; DestDir: "{app}\lib\d6"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi6"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JvclBpl6}\*; DestDir: "{code:GetDelphiBplDir|6}"; Components: "IDE\Delphi6"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi7
; SolidBreak; lib\Delphi 7
Source: {#JvclLib7}\*; DestDir: "{app}\lib\d7"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi7"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JvclBpl7}\*; DestDir: "{code:GetDelphiBplDir|7}"; Components: "IDE\Delphi7"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi9
; SolidBreak; lib\Delphi 2005
Source: {#JvclLib9}\*; DestDir: "{app}\lib\d9"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi9"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JvclBpl9}\*; DestDir: "{code:GetDelphiBplDir|9}"; Components: "IDE\Delphi9"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi10
; SolidBreak; lib\Delphi 2006
Source: {#JvclLib10}\*; DestDir: "{app}\lib\d10"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi10"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JvclBpl10}\*; DestDir: "{code:GetDelphiBplDir|10}"; Components: "IDE\Delphi10"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JvclHpp10}\*; DestDir: "{code:GetDelphiDir|10}\Include\Vcl"; Components: "IDE\Delphi10"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
#endif
#ifdef Include_Delphi11
; SolidBreak; lib\Delphi 2007
Source: {#JvclLib11}\*; DestDir: "{app}\lib\d11"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi11"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JvclBpl11}\*; DestDir: "{code:GetDelphiBplDir|11}"; Components: "IDE\Delphi11"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JvclHpp11}\*; DestDir: "{app}\include\d11"; Components: "IDE\Delphi11"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi12
; SolidBreak; lib\Delphi 2009
Source: {#JvclLib12}\*; DestDir: "{app}\lib\d12"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi12"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JvclBpl12}\*; DestDir: "{code:GetDelphiBplDir|12}"; Components: "IDE\Delphi12"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JvclHpp12}\*; DestDir: "{app}\include\d12"; Components: "IDE\Delphi12"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi14
; SolidBreak; lib\Delphi 2010
Source: {#JvclLib14}\*; DestDir: "{app}\lib\d14"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi14"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JvclBpl14}\*; DestDir: "{code:GetDelphiBplDir|14}"; Components: "IDE\Delphi14"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JvclHpp14}\*; DestDir: "{app}\include\d14"; Components: "IDE\Delphi14"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi15
; SolidBreak; lib\Delphi XE
Source: {#JvclLib15}\*; DestDir: "{app}\lib\d15"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi15"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JvclBpl15}\*; DestDir: "{code:GetDelphiBplDir|15}"; Components: "IDE\Delphi15"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs
Source: {#JvclHpp15}\*; DestDir: "{app}\include\d15"; Components: "IDE\Delphi15"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi16
; SolidBreak; lib\Delphi XE2
Source: {#JvclLib16}\*; DestDir: "{app}\lib\d16"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi16"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JvclBpl16}\*; DestDir: "{code:GetDelphiBplDir|16}"; Components: "IDE\Delphi16"; Flags: ignoreversion sortfilesbyextension
Source: {#JvclBpl16}\Win64\*; DestDir: "{code:GetDelphiBplDir|16}\Win64"; Components: "IDE\Delphi16"; Flags: ignoreversion sortfilesbyextension
Source: {#JvclHpp16}\*; DestDir: "{app}\include\d16"; Components: "IDE\Delphi16"; Flags: ignoreversion sortfilesbyextension
#endif
#ifdef Include_Delphi17
; SolidBreak; lib\Delphi XE3
Source: {#JvclLib17}\*; DestDir: "{app}\lib\d17"; Excludes: ".svn,__history,*.txt"; Components: "IDE\Delphi17"; Flags: ignoreversion recursesubdirs sortfilesbyextension createallsubdirs solidbreak
Source: {#JvclBpl17}\*; DestDir: "{code:GetDelphiBplDir|17}"; Components: "IDE\Delphi17"; Flags: ignoreversion sortfilesbyextension
Source: {#JvclBpl17}\Win64\*; DestDir: "{code:GetDelphiBplDir|17}\Win64"; Components: "IDE\Delphi17"; Flags: ignoreversion sortfilesbyextension
Source: {#JvclHpp17}\*; DestDir: "{app}\include\d17"; Components: "IDE\Delphi17"; Flags: ignoreversion sortfilesbyextension
#endif

#endif

; only source code => execute JVCL Installer
[Run]
Filename: {app}\install.bat; Description: "Execute JVCL Installer"; Flags: postinstall shellexec; Check: IsSourceInstall;

#ifdef Include_Binaries
[Registry]
#ifdef Include_Delphi6
; Delphi 6
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|6}; Components: "IDE\Delphi6"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d6; Components: "IDE\Delphi6"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi6"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi6"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|6}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue;
#endif
#ifdef Include_Delphi7
; Delphi 7
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|7}; Components: "IDE\Delphi7"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d7; Components: "IDE\Delphi7"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi7"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi7"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|7}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue;
#endif
#ifdef Include_Delphi9
; Delphi 2005
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|9}; Components: "IDE\Delphi9"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d9; Components: "IDE\Delphi9"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi9"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi9"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|9}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue;
#endif
#ifdef Include_Delphi10
; Delphi 2006
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|10}; Components: "IDE\Delphi10"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d10; Components: "IDE\Delphi10"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi10"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi10"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|10}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue;
#endif
#ifdef Include_Delphi11
; Delphi 2007
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|11}; Components: "IDE\Delphi11"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d11; Components: "IDE\Delphi11"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi11"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi11"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|11}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi11";
#endif
#ifdef Include_Delphi12
; Delphi 2009
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|12}; Components: "IDE\Delphi12"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d12; Components: "IDE\Delphi12"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi12"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi12"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|12}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi12";
#endif
#ifdef Include_Delphi14
; Delphi 2010
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|14}; Components: "IDE\Delphi14"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d14; Components: "IDE\Delphi14"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi14"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi14"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|14}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi14";
#endif
#ifdef Include_Delphi15
; Delphi XE
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|15}; Components: "IDE\Delphi15"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d15; Components: "IDE\Delphi15"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi15"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi15"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|15}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi15"; Check: IsDelphiInstalled(15)
#endif
#ifdef Include_Delphi16
; Delphi XE2
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|16}; Components: "IDE\Delphi16"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d16; Components: "IDE\Delphi16"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi16"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi16"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|16}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi16";
#endif
#ifdef Include_Delphi17
; Delphi XE3
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Jedi\JVCL"; ValueType: string; ValueName: "BplDir"; ValueData: {code:GetDelphiBplDir|17}; Components: "IDE\Delphi17"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Jedi\JVCL"; ValueType: string; ValueName: "DcpDir"; ValueData: {app}\lib\d17; Components: "IDE\Delphi17"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Jedi\JVCL"; ValueType: string; ValueName: "RootDir"; ValueData: {app}; Components: "IDE\Delphi17"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Jedi\JVCL"; ValueType: string; ValueName: "Version"; ValueData: {#JvclVersionStr}; Components: "IDE\Delphi17"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Jedi\JVCL\IDE"; ValueType: dword; ValueName: "RegisterGlobalDesignEditors"; ValueData: 1; Components: "Options\RegisterGlobalDesignEditors"; Flags: uninsdeletevalue;
Root: HKCU; Subkey: "{code:GetDelphiRegKey|17}\Globals"; ValueType: string; ValueName: "ForceEnvOptionsUpdate"; ValueData: "1"; Components: "IDE\Delphi17";
#endif

#endif


[UninstallDelete]
Type: files; Name: "{app}\dcu\*"
Type: files; Name: "{app}\bin\JVCLInstall.*"
Type: files; Name: "{app}\run\*.hpp"
Type: files; Name: "{app}\run\*.~*"
Type: files; Name: "{app}\run\*.dcu"
Type: files; Name: "{app}\common\*.hpp"
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
;Type: files; Name: "{code:GetDelphiDir|10}\Include\Vcl\Jv*.hpp"
; lib\Delphi/C++Builder 2007
Type: files; Name: "{app}\lib\d11\*"
Type: files; Name: "{app}\lib\d11\debug\*"
Type: files; Name: "{app}\include\d11\*"
Type: files; Name: "{code:GetDelphiBplDir|11}\Jv*.*"
;Type: files; Name: "{code:GetDelphiDir|11}\Include\Vcl\Jv*.hpp"
; lib\Delphi/C++Builder 2009
Type: files; Name: "{app}\lib\d12\*"
Type: files; Name: "{app}\lib\d12\debug\*"
Type: files; Name: "{app}\include\d12\*"
Type: files; Name: "{code:GetDelphiBplDir|12}\Jv*.*"
;Type: files; Name: "{code:GetDelphiDir|12}\Include\Vcl\Jv*.hpp"
; lib\Delphi/C++Builder 2010
Type: files; Name: "{app}\lib\d14\*"
Type: files; Name: "{app}\lib\d14\debug\*"
Type: files; Name: "{app}\include\d14\*"
Type: files; Name: "{code:GetDelphiBplDir|14}\Jv*.*"
;Type: files; Name: "{code:GetDelphiDir|14}\Include\Vcl\Jv*.hpp"
; lib\Delphi/C++Builder XE
Type: files; Name: "{app}\lib\d15\*"
Type: files; Name: "{app}\lib\d15\debug\*"
Type: files; Name: "{app}\include\d15\*"
Type: files; Name: "{code:GetDelphiBplDir|15}\Jv*.*"
; lib\Delphi/C++Builder XE2
Type: files; Name: "{app}\lib\d16\win32\*"
Type: files; Name: "{app}\lib\d16\win32\debug\*"
Type: files; Name: "{app}\lib\d16\win64\*"
Type: files; Name: "{app}\lib\d16\win64\debug\*"
Type: files; Name: "{app}\include\d16\*"
Type: files; Name: "{code:GetDelphiBplDir|16}\Jv*.*"
Type: files; Name: "{code:GetDelphiBplDir|16}\Win64\Jv*.*"
; lib\Delphi/C++Builder XE3
Type: files; Name: "{app}\lib\d17\win32\*"
Type: files; Name: "{app}\lib\d17\win32\debug\*"
Type: files; Name: "{app}\lib\d17\win64\*"
Type: files; Name: "{app}\lib\d17\win64\debug\*"
Type: files; Name: "{app}\include\d17\*"
Type: files; Name: "{code:GetDelphiBplDir|17}\Jv*.*"
Type: files; Name: "{code:GetDelphiBplDir|17}\Win64\Jv*.*"
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
  for Version := 6 to 17 do
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
        Result := GetDelphiBplDir(IntToStr(Version)) + '\' + PackageName + 'Design' + IntToStr(Version) + '0.bpl';
      ikBCB:
        Result := GetBCBBplDir(IntToStr(Version)) + '\' + PackageName + 'Design' + IntToStr(Version) + '0.bpl';
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
	  if Version >= 16 then // XE2+
	    LibDir := AppDir + '\lib\d' + IntToStr(Version) + '\$(Platform)' // is replaced by Win32/Win64 in the CompInstall.dll
	  else
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
  if Version >= 11 then
    IncludePaths := ExpandConstant('{app}') + '\include\d' + IntToStr(Version)
  else if Version = 10 then
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
    for Version := 6 to 17 do
    begin
      if IsDelphiJclInstalled(Version) then
      begin
        Result := True;
        Break;
      end;
    end;
    if not Result then
      MsgBox('No JCL is installed or the installed JCL version doesn''t match "{#JclVersionStr}*".'#10#10 +
             'Please install the matching JCL first and then restart the JVCL Installation.', mbError, MB_OK);
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
