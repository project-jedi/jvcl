; Setup script for building the JVCL setup
; NB: uses ISX and ISPP. You need both or a fairly new version of InnoSetup to compile this script
; JVCL is delivered in three versions: sources only, sources + demos and full install with JCL
; This script can compile all three. The version compiled is controlled by the JVCLRELEASE below.
; If the user selects any of the additional "Build and Install XXX Packages" tasks (these are only available if the corresponding
; compiler is installed), the installer finds the correct dcc version and builds and installs the packages for the invoked compiler
;
; TODO:
; 1. Write functions to call dcc.exe and also install the compiled packages as well as the help files into the IDE.
; 2. Add "Install Run" items that calls the build and install functions for each package
; 3. Add infobefore.txt, infoafter.txt and license.txt (MPL1.1)

;#pragma option -C-
; uncomment this line to not include any files while testing the script (mainly used for syntax-checking the Code section)
#define DEBUGNOFILES

; set up macro terminators (so we avoid those pesky escapes)
#pragma inlinestart "<$"
#pragma inlineend "$>"

;general values
#define JVCLNAMELONG "JEDI VCL Library"
#define JVCLNAMESHORT "JVCL"
#define JVCLVER "3.00"
#define JVCLURL "http://jvcl.sourceforge.net"
#define JVCLAPPID "JVCL"
#define JVCLDEFDIR "\JEDI\JVCL"
#define JCLFOLDER "..\..\JCL"

;NB: define the various versions we can build
#define SMALL "small"
#define MEDIUM "medium"
#define LARGE "large"

;define the actual release we are going to build now
#define JVCLRELEASE SMALL

;define the name of the setup file
#if JVCLRELEASE == SMALL
#define JVCLSETUPNAME JVCLNAMESHORT + JVCLVER + "SourceOnly"
#elif JVCLRELEASE == MEDIUM
#define JVCLSETUPNAME JVCLNAMESHORT + JVCLVER + "SourceExamples"
#else
#define JVCLSETUPNAME JVCLNAMESHORT + JVCLVER + "FullWithJCL"
#endif


[Setup]
AppName=<$JVCLNAMELONG$>
AppVerName=<$JVCLNAMESHORT$> <$JVCLVER$>
DefaultGroupName=<$JVCLNAMELONG$>
AppPublisher=<$JVCLNAMELONG$>
AppPublisherURL=<$JVCLURL$>
AppVersion=<$JVCLNAMESHORT$> <$JVCLVER$>
AppID=<$JVCLAPPID$>
AppSupportURL=<$JVCLURL$>
AppUpdatesURL=<$JVCLURL$>
DefaultDirName={pf}<$JVCLDEFDIR$>
OutputBaseFilename=<$JVCLSETUPNAME$>
AllowNoIcons=true
DisableStartupPrompt=true
DisableProgramGroupPage=true
UsePreviousGroup=false
OutputDir=.

[_ISTool]
EnableISX=true

[Components]
Name: SOURCEFILES; Description: Source files; Flags: fixed; Types: custom compact full
Name: CONVERTERFILES; Description: Converter files; Types: custom full

#if JVCLRELEASE != SMALL
Name: EXAMPLESFILES; Description: Examples; Types: custom full
#endif

#if JVCLRELEASE == LARGE
Name: ARCHIVEFILES; Description: Archived files; Types: custom full
Name: DEVTOOLSFILES; Description: Developer tools; Types: custom full
#endif

Name: JVCLHELPFILES; Description: JVCL Help file; Types: custom compact full
Name: JCLFILES; Description: JCL files; Types: custom full
; Delphi 5 Packages =============================================================================================================
Name: d5; Description: Delphi 5 Packages; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc; Description: Build Packages; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvCore; Description: Install Core Package; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvSystem; Description: Install System Package; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvCtrls; Description: Install Visual Controls; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvStdCtrls; Description: Install Standard Controls; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvCmp; Description: Install Non-Visual Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvCustom; Description: Install Custom Controls; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvDlgs; Description: Install Dialog Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvCrypt; Description: Install Crypt and Compress Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvNet; Description: Install Networking Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvMM; Description: Install Multimedia Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvDB; Description: Install Database Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvBDE; Description: Install BDE Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvInterpreter; Description: Install Interpreter Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvPlugin; Description: Install Plugin Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvJans; Description: Install Jans Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvGlobus; Description: Install Globus Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvPrintPreview; Description: Install Print Preview Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvPageComps; Description: Install Page List Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvValidators; Description: Install Validator Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvUIB; Description: Install Unified InterBase Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvTimeFramework; Description: Install Time Framework Components; Types: custom compact full; Check: IsDelphiInstalled(5.0)
Name: d5\dcc\JvHMI; Description: Install HMI Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d5\dcc\JvWizard; Description: Install Wizard Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)

; Delphi 6 Packages =============================================================================================================
Name: d6; Description: Delphi 6 Packages; Types: custom compact full
Name: d6\dcc; Description: Build Packages; Types: custom compact full
Name: d6\dcc\JvCore; Description: Install Core Package; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvSystem; Description: Install System Package; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvCtrls; Description: Install Visual Controls; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvStdCtrls; Description: Install Standard Controls; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvCmp; Description: Install Non-Visual Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvCustom; Description: Install Custom Controls; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvDlgs; Description: Install Dialog Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvCrypt; Description: Install Crypt and Compress Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvNet; Description: Install Networking Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvMM; Description: Install Multimedia Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvDB; Description: Install Database Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvBDE; Description: Install BDE Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvInterpreter; Description: Install Interpreter Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvPlugin; Description: Install Plugin Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvJans; Description: Install Jans Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvGlobus; Description: Install Globus Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvPrintPreview; Description: Install Print Preview Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvPageComps; Description: Install Page List Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvValidators; Description: Install Validator Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvUIB; Description: Install Unified InterBase Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvTimeFramework; Description: Install Time Framework Components; Types: custom compact full; Check: IsDelphiInstalled(6.0)
Name: d6\dcc\JvHMI; Description: Install HMI Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d6\dcc\JvWizard; Description: Install Wizard Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)

; Delphi 7 Packages =============================================================================================================
Name: d7; Description: Delphi 7 Packages; Types: custom compact full
Name: d7\dcc; Description: Build Packages; Types: custom compact full; Check: IsDelphiInstalled(8.0)
Name: d7\dcc\JvCore; Description: Install Core Package; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvSystem; Description: Install System Package; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvCtrls; Description: Install Visual Controls; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvStdCtrls; Description: Install Standard Controls; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvCmp; Description: Install Non-Visual Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvCustom; Description: Install Custom Controls; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvDlgs; Description: Install Dialog Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvCrypt; Description: Install Crypt and Compress Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvNet; Description: Install Networking Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvMM; Description: Install Multimedia Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvDB; Description: Install Database Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvBDE; Description: Install BDE Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvInterpreter; Description: Install Interpreter Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvPlugin; Description: Install Plugin Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvJans; Description: Install Jans Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvGlobus; Description: Install Globus Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvPrintPreview; Description: Install Print Preview Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvPageComps; Description: Install Page List Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvValidators; Description: Install Validator Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvUIB; Description: Install Unified InterBase Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvTimeFramework; Description: Install Time Framework Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvHMI; Description: Install HMI Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
Name: d7\dcc\JvWizard; Description: Install Wizard Components; Types: custom compact full; Check: IsDelphiInstalled(7.0)
; TODO
;Name: bcb5; Description: C++Builder 5 Packages; Types: custom compact full
;Name: bcb6; Description: C++Builder 6 Packages; Types: custom compact full
;Name: k2; Description: Kylix 2 Packages; Types: custom compact full
;Name: k3; Description: Kylix 3 Packages; Types: custom compact full
; TODO

[Files]
#ifndef DEBUGNOFILES
; SOURCEFILES
Source: ..\*.*; DestDir: {app}; Components: SOURCEFILES
Source: ..\common\*.pas; DestDir: {app}\common; Components: SOURCEFILES
Source: ..\common\*.INC; DestDir: {app}\common; Components: SOURCEFILES
Source: ..\resources\*.dcr; DestDir: {app}\resources; Components: SOURCEFILES
Source: ..\resources\*.res; DestDir: {app}\resources; Components: SOURCEFILES
Source: ..\design\*.*; DestDir: {app}\design; Components: SOURCEFILES
Source: ..\run\*.*; DestDir: {app}\run; Components: SOURCEFILES

#if JVCLRELEASE == LARGE
; ARCHIVEFILES ==================================================================================================================
Source: ..\archive\*.*; DestDir: {app}\archive; Components: ARCHIVEFILES
; CONVERTERFILES ================================================================================================================
Source: ..\converter\*.dat; DestDir: {app}\converter; Components: CONVERTERFILES DEVTOOLSFILES
; DEVTOOLSFILES =================================================================================================================
Source: ..\devtools\*.*; DestDir: {app}\devtools; Flags: recursesubdirs; Components: DEVTOOLSFILES
Source: ..\devtools\JVCLConverter\*.*; DestDir: {app}\devtools\JVCLConverter; Flags: recursesubdirs; Components: CONVERTERFILES DEVTOOLSFILES

Source: ..\images\*.*; DestDir: {app}\images; Components: DEVTOOLSFILES
#endif

#if JVCLRELEASE != SMALL
; EXAMPLESFILES =================================================================================================================
Source: ..\dict\*.dic; DestDir: {app}\dict; Components: EXAMPLESFILES
Source: ..\examples\*.*; DestDir: {app}\examples; Flags: recursesubdirs; Components: EXAMPLESFILES
#endif

; JVCLHELPFILES =================================================================================================================
Source: ..\help\*.hlp; DestDir: {app}\help; Components: JVCLHELPFILES
Source: ..\help\*.cnt; DestDir: {app}\help; Components: JVCLHELPFILES

; PACKAGES ======================================================================================================================
;Source: ..\packages\BCB5 Packages.bpg; DestDir: {app}\packages; Components: bcb5
;Source: ..\packages\BCB5\*.*; DestDir: {app}\packages\bcb5; Components: bcb5
Source: ..\packages\D5 Packages.bpg; DestDir: {app}\packages; Components: d5
Source: ..\packages\D5\*.*; DestDir: {app}\packages\d5; Components: d5
Source: ..\packages\D6 Packages.bpg; DestDir: {app}\packages; Components: d6
Source: ..\packages\D6\*.*; DestDir: {app}\packages\d6; Components: d6
Source: ..\packages\D7 Packages.bpg; DestDir: {app}\packages; Components: d7
Source: ..\packages\D7\*.*; DestDir: {app}\packages\d7; Components: d7
;Source: ..\packages\K2 Packages.bpg; DestDir: {app}\packages; Components: k2
;Source: ..\packages\K2\*.*; DestDir: {app}\packages\k2; Components: k2
;Source: ..\packages\K3 Packages.bpg; DestDir: {app}\packages; Components: k3
;Source: ..\packages\K3\*.*; DestDir: {app}\packages\k3; Components: k3

#if JVCLRELEASE == LARGE
; JCLFILES
Source: <$JCLFOLDER$>\*.*; DestDir: {app}\..\JCL; Components: JCLFILES
Source: <$JCLFOLDER$>\examples\*.*; DestDir: {app}\..\JCL\examples; Flags: recursesubdirs; Components: JCLFILES
Source: <$JCLFOLDER$>\help\*.*; DestDir: {app}\..\JCL\help; Components: JCLFILES
Source: <$JCLFOLDER$>\packages\*.*; DestDir: {app}\..\JCL\packages; Components: JCLFILES
Source: <$JCLFOLDER$>\source\*.*; DestDir: {app}\..\JCL\source; Components: JCLFILES
#endif


[Dirs]
Name: {app}\bin; Flags: uninsalwaysuninstall; Components: SOURCEFILES
Name: {app}\dcu; Flags: uninsalwaysuninstall; Components: SOURCEFILES
Name: {app}\lib\d5; Flags: uninsalwaysuninstall; Components: SOURCEFILES
Name: {app}\lib\d6; Flags: uninsalwaysuninstall; Components: SOURCEFILES
Name: {app}\lib\d7; Flags: uninsalwaysuninstall; Components: SOURCEFILES
#if JVCLRELEASE = LARGE
Name: <$JCLFOLDER$>\dcu; Flags: uninsalwaysuninstall; Components: JCLFILES
Name: <$JCLFOLDER$>\bin; Flags: uninsalwaysuninstall; Components: JCLFILES
#endif

#endif // DEBUGNOFILES

[UninstallDelete]
Name: {app}\bin; Type: filesandordirs
Name: {app}\dcu; Type: filesandordirs
Name: {app}\lib\d5; Type: filesandordirs
Name: {app}\lib\d6; Type: filesandordirs
Name: {app}\lib\d7; Type: filesandordirs
#if JVCLRELEASE != SMALL
Name: {app}\examples\bin; Type: filesandordirs
Name: {app}\examples\dcu; Type: filesandordirs
Name: {app}\devtools\bin; Type: filesandordirs
Name: {app}\devtools\dcu; Type: filesandordirs
#endif

[Registry]
; add some registry keys so we know where JVCL/JCL is installed. We don't use this yet, but might need it in the future.
Root: HKCU; Subkey: Software\JEDI\JVCL; ValueType: string; ValueName: InstallPath; ValueData: {app}; Flags: uninsdeletekey
Root: HKCU; Subkey: Software\JEDI\JCL; ValueType: string; ValueName: InstallPath; ValueData: {app}; Flags: uninsdeletekey; Components: JCLFILES

[Run]
; the JVCLPackages.iss is the unit that really handles the entire Delphi installation
Filename: {code:InstallJVCLPackages|{app}}; WorkingDir: {app}; Flags: skipifdoesntexist


[Code]
#include "JVCLPackages.iss"

[_ISToolPreCompile]
; just clean all dcu's, exe's and all other files we don't want to include before we begin
Name: clean.bat; Parameters: ; Flags: abortonerror runminimized
