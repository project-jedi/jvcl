@echo off

SET PACKAGE=D7 Packages
SET DIR=D7

REM ------- Generic --------

cd ..\devtools
if NOT EXIST bin\MakeDOF.exe  make MakeDOF.exe
if NOT EXIST bin\MakeCFG.exe  make MakeCFG.exe
if NOT EXIST bin\Bpg2Make.exe  make Bpg2Make.exe
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

make -f "%PACKAGE%.mak"

echo.
echo Cleaning
del "%PACKAGE%.mak"
del %DIR%\*.cfg
