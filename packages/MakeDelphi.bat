@echo off
: ---------------------------------------------------------------------
:    WARNING   WARNING    WARNING    WARNING    WARNING    WARNING
:
: Please read the help before using this batch file as there are
: some compulsory parameters
:
: ---------------------------------------------------------------------
cls
if %1!==! goto help

if NOT "%DELDIR%"!==""! goto next
if %2!==! goto help
SET DELDIR=%2
:next
SET DIR=%1
SET ROOT=%DELDIR%
SET DIR2=%3

SET PACKAGE=%DIR% Packages
if NOT !%DIR2%==! SET PACKAGE=%DIR2% Packages


SET DCPDIR=%ROOT%\Projects\Bpl

SET MAKE=%ROOT%\bin\make.exe
if EXIST %MAKE% goto hasmake
  SET MAKE=make
  SET ROOT=
  SET DCPDIR=
:hasmake

cd ..\devtools
if NOT EXIST bin\MakeDOF.exe  %MAKE% -s MakeDOF.exe
if NOT EXIST bin\MakeCFG.exe  %MAKE% -s MakeCFG.exe
if NOT EXIST bin\Bpg2Make.exe %MAKE% -s Bpg2Make.exe
cd bin

REM echo.
REM echo Creating .dof files
REM call makedofs.bat

echo.
echo Creating .cfg files
MakeCFG "..\..\packages\%DIR%\*.dpk" %DIR%packscfg.tmpl
if NOT !%DIR2%==! MakeCFG "..\..\packages\%DIR2%\*.dpk" %DIR%packscfg.tmpl

echo.
cd ..\..\packages

..\devtools\bin\Bpg2Make.exe "%PACKAGE%.bpg"

REM
REM This will go wrong if %MAKE% is something like:
REM   "C:\Program Files\Borland\Delphi7"\bin\make.exe
REM
REM It will work if %MAKE% is something like:
REM   C:\PROGRA~1\Borland\Delphi7\bin\make.exe
REM   "C:\Program Files\Borland\Delphi7\bin\make.exe"

%MAKE% -f "%PACKAGE%.mak" %4 %5 %6 %7 %8 %9 %TARGETS%

IF ERRORLEVEL 1 GOTO error
echo.
echo Cleaning

del /f "%PACKAGE%.mak"
del /f /q %DIR%\*.cfg
if NOT !%DIR2%==! del /f /q %DIR2%\*.cfg

echo.
echo The JVCL was successfuly built for %1
echo.
goto end

:error
echo.
echo !!!!! ERROR WHILE BUILDING THE JVCL !!!!
echo Please refer to last output for details
echo.
goto end

:help
echo MakeDelphi.bat - Builds the JVCL for Delphi
echo.
echo Usage:    MakeDelphi PackageName [DelphiDirectory] [PackageName2]
echo.
echo     PackageName       The name of the group file to use without "Packages"
echo                       e.g. "d6"
echo     DelphiDirectory   The place where Delphi is installed.
echo                       e.g. "C:\Program Files\Delphi6" (or %%DELDIR%%)
echo     PackageName2      The directory where the personal packages for the
echo                       given group are. e.g. "d6per"
echo.
echo Any additional argument (up to the 9th) will be passed to make
echo The TARGETS environment variable should contain the name of the 
echo packages to be built separated by spaces:
echo SET TARGETS=JvCoreC6R.bpl JvCoreC6D.bpl
echo If not specified, all packages will be built.
echo.
echo Delphi must be closed before running this batch file
echo.
:end

SET ROOT=
SET DIR=
SET DIR2=
SET PACKAGE=
SET DCPDIR=
