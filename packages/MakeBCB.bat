@echo off
: ---------------------------------------------------------------------
:    WARNING   WARNING    WARNING    WARNING    WARNING    WARNING
:
: Please read the help before using this batch file as there are
: some compulsory parameters
:
: ---------------------------------------------------------------------
SET OLDPATH=%PATH%

if %1!==! goto help
if %2!==! goto help
if %3!==! goto help
SET PACKAGE=%1
SET DIR=%2
SET ROOT=%3

: get rid of the quotes around %ROOT%
cd ..\devtools\NoQuotes
if NOT EXIST ..\bin\NoQuotes.exe dcc32.exe NoQuotes.dpr
cd ..\bin
NoQuotes ROOT %ROOT%
Call NoQuotesBatch.bat
del /f NoQuotesBatch.bat
cd ..\..\packages

SET JCLDIR=%4

if %5!==! SET BPILIBDIR=%ROOT%\Projects\Lib
if not %5!==! SET BPILIBDIR=%5
if %6!==! SET BPLDIR=%ROOT%\Projects\Bpl
if not %6!==! SET BPLDIR=%6

SET PATH=%PATH%;%ROOT%\Projects\Bpl;%ROOT%\Projects\Lib;%ROOT%\bin

SET MAKE=%ROOT%\bin\make.exe

IF NOT %JCLDIR%!==! SET ADDFLAGS=-U%JCLDIR%\dcu

if NOT EXIST "%MAKE%" SET MAKE=make

cd ..\devtools
if NOT EXIST bin\Bpg2Make.exe %MAKE% Bpg2Make.exe
cd bin

echo.
cd ..\..\packages

..\devtools\bin\Bpg2Make.exe %PACKAGE%.bpg

%MAKE% -f %PACKAGE%.mak %7 %8 %9

IF ERRORLEVEL 1 GOTO error
echo.
echo Cleaning
del /f %PACKAGE%.mak
del /f /q %DIR%\*.mak
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
echo MakeBCB.bat - Builds the JVCL for BCB
echo.
echo Usage:    MakeBCB PackageName PackageDirectory [BCBDirectory]
echo                   [JCLDirectory] [LIBDirectory] [BPLDirectory]
echo.
echo     PackageName       The name of the group file to use
echo                       e.g. "BCB6 Packages"
echo     PackageDirectory  The directory where the packages for the given
echo                       group are. e.g. "bcb6"
echo     BCBDirectory      The place where BCB is installed.
echo                       e.g. "C:\Program Files\CBuilder6"
echo     JCLDirectory      The place where the JCL is installed. You must
echo                       specify this value if the JCL is not in ..\..\JCL
echo     LIBDirectory      The place where to put the BPI and LIB files.
echo                       Defaults to $(BCB)\Projects\Lib
echo                       You MUST ensure that this directory is in the
echo                       PATH environment variable
echo     BPLDirectory      The place where to put the BPL and TDS files.
echo                       Defaults to $(BCB)\Projects\Bpl
echo.
echo Any additional argument (up to the 9th) will be passed to make
echo.
:end

SET PATH=%OLDPATH%

SET PACKAGE=
SET DIR=
SET ROOT=
SET JCLDIR=
SET BPILIBDIR=
SET BPLDIR=
SET MAKE=
SET ADDFLAGS=
