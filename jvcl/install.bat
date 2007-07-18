@echo off

SET JCLVERSION=1.101

SET DELPHIVERSION=%1

:: compile installer
echo [Compiling installer...]
cd install\JVCLInstall
if EXIST JVCLInstall.cfg  del JVCLInstall.cfg
..\..\packages\bin\dcc32ex.exe --requires-jcl=%JCLVERSION% -Q -B -DJVCLINSTALLER -DUSE_DXGETTEXT -E..\..\bin -I.;..\..\common -U..\..\common;..\..\run -n..\..\dcu JVCLInstall.dpr
if ERRORLEVEL 1 goto Failed
cd ..\..

:: create mo files, if possible (msgfmt must be in the PATH environment variable)
cd locale
msgfmt --help 1> tmp1.txt 2> tmp2.txt
if errorlevel 1 goto MsgFmtNotFound

echo [Generating language files...]
call SetLanguages.bat
echo Known languages: %LANGUAGES%
for %%l in (%LANGUAGES%) do msgfmt %%l\LC_MESSAGES\jvcl.po -o %%l\LC_MESSAGES\jvcl.mo 
for %%l in (%LANGUAGES%) do msgfmt %%l\LC_MESSAGES\JVCLInstall.po -o %%l\LC_MESSAGES\JVCLInstall.mo 
goto CleanLocale

:MsgFmtNotFound
echo Warning: msgfmt not found in PATH, no language support in the installer

:CleanLocale
if exist tmp1.txt del /q tmp?.txt
cd ..

:: start installer
echo [Starting installer...]
start bin\JVCLInstall.exe %2 %3 %4 %5 %6 %7 %8 %9
if ERRORLEVEL 1 goto FailStart
goto Leave

:FailStart
bin\JVCLInstall.exe %2 %3 %4 %5 %6 %7 %8 %9
goto Leave

:Failed
cd ..\..
echo.
echo Failed to compile JVCL installer
echo.
pause

:Leave
SET DELPHIVERSION=
SET JCLVERSION=
