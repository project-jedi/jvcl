@echo off
: ---------------------------------------------------------------------
:    WARNING   WARNING    WARNING    WARNING    WARNING    WARNING
:
: Please read the help before using this batch file as there are
: some compulsory parameters
:
: ---------------------------------------------------------------------
if %1!==! goto help
if %2!==! goto help
if %3!==! goto help
SET PACKAGE=%1
SET DIR=%2
SET ROOT=%3

SET MAKE=%ROOT%\bin\make.exe
if NOT EXIST "%MAKE%" SET MAKE=make

SET DCPDIR=%ROOT%\Projects\Bpl

cd ..\devtools
if NOT EXIST bin\MakeDOF.exe  %MAKE% MakeDOF.exe
if NOT EXIST bin\MakeCFG.exe  %MAKE% MakeCFG.exe
if NOT EXIST bin\Bpg2Make.exe %MAKE% Bpg2Make.exe
cd bin

REM echo.
REM echo Creating .dof files
REM call makedofs.bat

echo.
echo Creating .cfg files
MakeCFG "..\..\packages\%DIR%\*.dpk" %DIR%packscfg.tmpl
MakeCFG "..\..\packages\%DIR2%\*.dpk" %DIR%packscfg.tmpl


echo.
cd ..\..\packages

..\devtools\bin\Bpg2Make.exe %PACKAGE%.bpg

%MAKE% -f %PACKAGE%.mak %4 %5 %6 %7 %8 %9

IF ERRORLEVEL 1 GOTO error
echo.
echo Cleaning
del /f %PACKAGE%.mak
del /f /q %DIR%\*.cfg
echo.
echo The JVCL was successfuly built for %2
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
echo Usage:    MakeDelphi PackageName PackageDirectory DelphiDirectory
echo.
echo     PackageName       The name of the group file to use
echo                       e.g. "Delphi6 Packages"
echo     PackageDirectory  The directory where the packages for the given
echo                       group are. e.g. "d6"
echo     DelphiDirectory   The place where Delphi is installed.
echo                       e.g. "C:\Program Files\Delphi6"
echo.
echo Any additional argument (up to the 9th) will be passed to make
echo.
:end

