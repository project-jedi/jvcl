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

call MakeDelphi d5 x d5std
SET DELDIR=

goto end

:help
echo MakeD5Std.bat - Builds the JVCL for Delphi 5 Standard
echo.
echo Usage:    MakeD5Std [DelphiDirectory]
echo.
echo     DelphiDirectory   The place where Delphi is installed.
echo                       Defaults to "C:\Program Files\Delphi5"
echo.
echo Any additional argument will be ignored
echo.
:end
