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

: --- Create a batch file that will pop the current directory
: --- Derived from a method written by Frank Sandy in comp.os.msdos.misc
: --- http://groups.google.com.au/groups?hl=en&lr=&ie=UTF-8&oe=UTF-8&selm=4l57gi%24pmp%40nhj.nlc.net.au&rnum=1
if %TEMP%!==! set TEMP=.
echo @echo off > %TEMP%\pop.bat
echo @prompt $N: > %TEMP%\dummy.bat
echo. >> %TEMP%\dummy.bat
echo @prompt cd $P >> %TEMP%\dummy.bat
command /c %TEMP%\dummy.bat >> %TEMP%\pop.bat
del %TEMP%\dummy.bat


: get rid of the quotes around ROOT, DCPDIR and JCLDIR
if EXIST ..\devtools\bin\NoQuotes.exe goto NoQuotesExists
cd ..\devtools\NoQuotes
dcc32.exe -e..\bin NoQuotes.dpr
cd ..\..\packages

:NoQuotesExists
..\devtools\bin\NoQuotes ROOT %ROOT%
Call NoQuotesBatch.bat
if not %DCPDIR%!==! ..\devtools\bin\NoQuotes DCPDIR %DCPDIR%
Call NoQuotesBatch.bat
..\devtools\bin\NoQuotes JCLDIR %JCLDIR%
Call NoQuotesBatch.bat
del /f NoQuotesBatch.bat

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
if EXIST ..\devtools\bin\pg.exe goto PgExists
cd ..\devtools\PackagesGenerator
SET C5PFLAGS=
if %VERSION%==5 SET C5PFLAGS=-LUvcl50
dcc32.exe -e..\bin -I"..\..\Common;%JCLDIR%\source" %C5PFLAGS% -n"..\Dcu" -U"..\..\Run;..\..\Common;%ROOT%\Lib\Obj;%JCLDIR%\source\common;%JCLDIR%\source\windows;%JCLDIR%\source\vcl;%JCLDIR%\source\visclx;..\Dcu" -q -w -h -m pg.dpr
echo.
cd ..\..\packages

:PgExists

: copy the required files into the JCL packages dir
echo Copying template...
copy /D /Y .\jcldcpdpk%VERSION%.tpl "%JCLDIR%\packages\c%VERSION%\template.dpk"
echo.

: generate the packages from the xml files

..\devtools\bin\pg -m=JCL -p="%JCLDIR%\Packages" -t=c%VERSION% -x=..\devtools\bin\pgEdit.xml
echo.

cd %JCLDIR%\packages\c%VERSION%

echo Compiling the JCL dcp files...
: compile the generated packages
echo on
for %%f in ("C*.dpk") do %ROOT%\bin\dcc32 -I"..\..\source;..\..\source\common" -U"..\..\source\common;..\..\source\windows;..\..\source\vcl;..\..\source\visclx;%ROOT%\Lib\Obj" "%%f"
@echo off
echo.

echo Copying dcp files...
: copy the resulting files where they should go
for %%f in (*.dcp) do xcopy /y %%f "%DCPDIR%\"

IF ERRORLEVEL 1 GOTO error
echo.
echo Cleaning...
del /f /q C*.dcp
del /f /q C*.bpl
del /f /q *.dpk
del /f /q *.dcu
echo.
echo The JCL DCP files were successfuly created for the JVCL
echo.
goto restoredir

:error
echo.
echo !!!!! ERROR WHILE BUILDING THE JCL DCP FOR THE JVCL !!!!
echo Please refer to last output for details
echo.
echo ERROR >..\error.dat
goto restoredir

:restoredir
: -- Restore the original directory
CALL %TEMP%\pop.bat
del %TEMP%\pop.bat
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
SET _SAVED=

