@echo off
: ---------------------------------------------------------------------
:    WARNING   WARNING    WARNING    WARNING    WARNING    WARNING
:
: Please read the help before using this batch file as there are
: some special parameters
:
: ---------------------------------------------------------------------
SET ROOT=%1
if %ROOT%!==! SET ROOT=C:\Program Files\Delphi7

SET MAKE=%ROOT%\bin\make.exe

if EXIST "%MAKE%" goto next
  SET MAKE=make
  SET ROOT=
:next

cd ..\devtools
if NOT EXIST bin\pg.exe %MAKE% pg.exe
cd bin

echo.

pg.exe

cd ..\..\packages

echo.
echo The packages were successfuly generated
echo.
goto end

:help
echo MakePackages.bat - Generates the packages for the JVCL
echo.
echo Usage:    MakePackages RootDir
echo.
echo     RootDir       The root directory of your Borland deveopment
echo                   environment. Make must be in RootDir\bin
echo                   Defaults to C:\Program files\Delphi7
echo.
:end

SET ROOT=
SET MAKE=
