@echo off

set JCLDIR=%1
REM if %JCLDIR%!==! set JCLDIR=..\JCL

cd install\JVCL3Install
REM dcc32 -$O+ -Q JVCL3Install.dpr -E..\..\packages -U%JCLDIR%\source\common -U%JCLDIR%\source\windows -I%JCLDIR%\source\common 
dcc32 -$O+ -Q JVCL3Install.dpr -E..\..\packages
cd ..\..

if NOT EXIST packages\JVCL3Install.exe goto error
start packages\JVCL3Install.exe

goto end


:error
echo.
echo Error: JVCL 3 Package Installer could not be compiled.
echo.
pause

:end
SET JCLDIR=