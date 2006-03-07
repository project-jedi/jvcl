@echo off

SET DELPHIVERSION=%1

:: compile installer
cd install\JVCLInstall
del /Q *.cfg >NUL
..\..\packages\bin\dcc32ex.exe -Q -E..\..\bin -I..\..\install\JVCLInstall;..\..\common -U..\..\common;..\..\run -n..\..\dcu JVCLInstall.dpr
if ERRORLEVEL 1 goto Failed
cd ..\..

:: start installer
start bin\JVCLInstall.exe "%2" "%3" "%4" "%5" "%6" "%7" "%8" "%9"
if ERRORLEVEL 1 goto FailStart
goto Leave

:FailStart
bin\JVCLInstall.exe "%2" "%3" "%4" "%5" "%6" "%7" "%8" "%9"
goto Leave

:Failed
cd ..\..
echo.
echo Failed to compile JVCL installer
echo.
pause

:Leave
SET DELPHIVERSION=
