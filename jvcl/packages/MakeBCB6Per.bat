@echo off

rem NB! Change this path to reflect the location of make.exe:
SET ROOT=C:\Prog\CBuilder6
SET MAKE=%ROOT%\bin\make.exe




SET PACKAGE=BCB6Per Packages
SET DIR=BCB6
SET DIR2=BCB6Per

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

rem echo.
rem echo Creating .cfg files
rem MakeCFG "..\..\packages\%DIR%\*.bpk" %DIR%packscfg.tmpl
rem MakeCFG "..\..\packages\%DIR2%\*.bpk" %DIR%packscfg.tmpl


echo.
cd ..\..\packages

..\devtools\bin\Bpg2Make.exe "%PACKAGE%.bpg"

%MAKE% -f "%PACKAGE%.mak" %1 %2 %3 %4 %5 %6 %7 %8 %9



echo.
echo Cleaning
rem del "%PACKAGE%.mak"
rem del %DIR%\*.cfg
rem del %DIR2%\*.cfg
