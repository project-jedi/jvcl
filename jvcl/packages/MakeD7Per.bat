@echo off
: ---------------------------------------------------------------------
:    WARNING   WARNING    WARNING    WARNING    WARNING    WARNING
:
: Please read the help before using this batch file as there are
: some special parameters
:
: ---------------------------------------------------------------------
SET %DELDIR%=%1
if %DELDIR%!==! SET DELDIR=C:\Program Files\Delphi7

MakeDelphi "D7Per Packages" d7per %DELDIR%
del /f /q D7\*.cfg

goto end

:help
echo MakeD7Per.bat - Builds the JVCL for Delphi 7 Personal
echo.
echo Usage:    MakeD6Per [DelphiDirectory]
echo.
echo     DelphiDirectory   The place where Delphi is installed.
echo                       Defaults to "C:\Program Files\Delphi7"
echo.
echo Any additional argument will be ignored
echo.
:end
