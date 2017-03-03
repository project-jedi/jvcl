@echo off

SETLOCAL
pushd "%~dp0"

SET JCLVERSION=1.105

SET DELPHIVERSION=%1

:: compile installer
echo [Compiling installer...]
cd install\JVCLInstall

if EXIST JVCLInstall.cfg  del JVCLInstall.cfg
..\..\packages\bin\dcc32ex.exe %INSTALL_VERBOSE% --requires-jcl=%JCLVERSION% --runtime-package-vcl -Q -B -DDEFAULT_JVCL_INC -DUSE_DXGETTEXT -E..\..\bin -I.;..\..\common -U..\..\run -R..\..\resources -n..\..\dcu JVCLInstall.dpr
if ERRORLEVEL 1 goto Failed

if EXIST JVCLCmdStarter.cfg  del JVCLCmdStarter.cfg
if EXIST bin\JVCLCmdStarter.exe goto SkipCmdStarter
..\..\packages\bin\dcc32ex.exe -Q -B -E..\..\bin -n..\..\dcu JVCLCmdStarter.dpr >NUL 2>NUL
::if ERRORLEVEL 1 goto Failed
:SkipCmdStarter
cd ..\..

:: delete the generated DCU files so that users don't get confused
del /Q dcu\*.dcu >NUL

:: create mo files, if possible (msgfmt must be in the PATH environment variable)
echo.
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
echo.
echo Warning: msgfmt not found in PATH, no language support in the installer

:CleanLocale
if exist tmp1.txt del /q tmp?.txt
cd ..

:: start installer
echo.
echo [Starting installer...]
echo bin\JVCLInstall.exe %*
if not exist bin\JVCLCmdStarter.exe goto FailStart
bin\JVCLCmdStarter.exe bin\JVCLInstall.exe %*
if ERRORLEVEL 1 goto FailStart
goto Leave

:FailStart
bin\JVCLInstall.exe %*
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

popd
ENDLOCAL
