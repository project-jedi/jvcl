@echo off

rem NB! Change this path to reflect the location of make.exe:
SET MAKE=C:\Program Files\Delphi6\bin\make.exe




SET PACKAGE=D6 Packages
SET DIR=D6

REM ------- Generic --------

if NOT EXIST "%MAKE%" SET MAKE=make

cd ..\devtools
if NOT EXIST bin\MakeDOF.exe  %MAKE% MakeDOF.exe
if NOT EXIST bin\MakeCFG.exe  %MAKE% MakeCFG.exe
if NOT EXIST bin\Bpg2Make.exe %MAKE% Bpg2Make.exe
cd bin

REM echo.
REM echo Creating .dof files
REM call makedofs.bat

echo.
echo Createing .cfg files
call makecfgs.bat

echo.
cd ..\..\packages

..\devtools\bin\Bpg2Make.exe "%PACKAGE%.bpg"

%MAKE% -f "%PACKAGE%.mak"

echo.
echo Cleaning
del "%PACKAGE%.mak"
del %DIR%\*.cfg
