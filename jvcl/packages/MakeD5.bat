@echo off
: ---------------------------------------------------------------------
:    WARNING   WARNING    WARNING    WARNING    WARNING    WARNING
:
: Please read the help before using this batch file as there are
: some special parameters
:
: ---------------------------------------------------------------------
SET DELDIR=%1
if "%DELDIR%!"=="!" SET DELDIR=C:\Program Files\Delphi5

call MakeDelphi d5
SET DELDIR=

goto end

:help
echo MakeD5.bat - Builds the JVCL for Delphi 5
echo.
echo Usage:    MakeD5 [DelphiDirectory]
echo.
echo     DelphiDirectory   The place where Delphi is installed.
echo                       Defaults to "C:\Program Files\Delphi5"
echo.
echo Any additional argument will be ignored
echo.
:end


