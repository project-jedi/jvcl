@echo off

rem NB! Change this path to reflect the location of make.exe:
SET MAKE=C:\Program Files\Delphi7\bin\make.exe




SET PACKAGE=D7 Packages
SET DIR=D7

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
MakeCFG "..\..\packages\%DIR%\*.dpk" %DIR%packscfg.tmpl


echo.
cd ..\..\packages

..\devtools\bin\Bpg2Make.exe "%PACKAGE%.bpg"

%MAKE% -f "%PACKAGE%.mak" %1 %2 %3 %4 %5 %6 %7 %8 %9

echo.
echo Cleaning
del "%PACKAGE%.mak"
del %DIR%\*.cfg
