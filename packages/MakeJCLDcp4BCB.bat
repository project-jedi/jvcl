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
SET VERSION=%1
SET ROOT=%2
SET JCLDIR=%3
SET DCPDIR=%4

if %JCLDIR%!==! set JCLDIR=..\..\Jcl
if %ROOT%!==! set ROOT=C:\program files\CBuilder%VERSION%


: get rid of the quotes around ROOT, DCPDIR and JCLDIR
cd ..\devtools\NoQuotes
if NOT EXIST ..\bin\NoQuotes.exe dcc32.exe NoQuotes.dpr
cd ..\bin
NoQuotes ROOT %ROOT%
Call NoQuotesBatch.bat
if not %DCPDIR%!==! NoQuotes DCPDIR %DCPDIR%
Call NoQuotesBatch.bat
NoQuotes JCLDIR %JCLDIR%
Call NoQuotesBatch.bat
del /f NoQuotesBatch.bat
cd ..\..\packages

if %DCPDIR%!==! set DCPDIR=%ROOT%\Projects\Bpl

SET PATH=%PATH%;%ROOT%\Projects\Bpl;%ROOT%\Projects\Lib;%ROOT%\bin

SET MAKE=%ROOT%\bin\make.exe

if NOT EXIST "%MAKE%" SET MAKE=make

: ensure we have bpg2make
cd ..\devtools
if NOT EXIST bin\Bpg2Make.exe %MAKE% Bpg2Make.exe
echo.
cd ..\packages

: ensure we have pg.exe
cd ..\devtools
if NOT EXIST bin\pg.exe %MAKE% pg.exe
echo.
cd ..\packages

: copy the required files into the JCL packages dir
mkdir "%JCLDIR%\packages\bcb%VERSION%"
copy /D /Y .\jcldcpdpk%VERSION%.tpl "%JCLDIR%\packages\bcb%VERSION%\template.dpk"
xcopy /Y "%JCLDIR%\packages\c%VERSION%\*.res" "%JCLDIR%\packages\bcb%VERSION%\"

: generate the packages from the xml files
..\devtools\bin\pg -p="%JCLDIR%\Packages" -t=c%VERSION% -r=Jcl -f=%%e%%p%%n,%%e%%p%%n%%v0

: compile the generated packages
for %%f in ("%JCLDIR%\packages\bcb%VERSION%\C*.dpk") do %ROOT%\bin\dcc32 -I"%JCLDIR%\source\common" -U"%JCLDIR%\source\common" -U"%JCLDIR%\source\windows" -U"%JCLDIR%\source\vcl" -U"%JCLDIR%\source\visclx" "%%f"

: copy the resulting files where they should go
for %%f in (*.dcp) do xcopy /y %%f "%DCPDIR%\%%f"

IF ERRORLEVEL 1 GOTO error
echo.
echo Cleaning...
del /f /q C*.dcp
del /f /q C*.bpl
rmdir /s /q "%JCLDIR%\packages\bcb%VERSION%"
echo.
echo The JCL DCP files were successfuly created for the JVCL
echo.
goto end

:error
echo.
echo !!!!! ERROR WHILE BUILDING THE JCL DPC FOR THE JVCL !!!!
echo Please refer to last output for details
echo.
goto end

:help
echo MakeJCLDcp4BCB.bat - Builds the JCL dcp files for BCB
echo.
echo Usage:    MakeJCLDcp4BCB Version [BCBDirectory]
echo                          [JCLDirectory] [DCPDirectory]
echo.
echo     Version           The version of BCB to build for (5 or 6)
echo     BCBDirectory      The place where BCB is installed.
echo                       Defaults to "C:\Program Files\CBuilder%%VERSION%%"
echo     JCLDirectory      The place where the JCL is installed.
echo                       Defaults to ..\..\JCL
echo     DCPDirectory      The place where to put the DCP file.
echo                       Defaults to $(BCB)\Projects\Bpl
echo.
:end

SET PATH=%OLDPATH%

SET VERSION=
SET JCLDIR=
SET ROOT=
SET DCPDIR=
SET MAKE=

