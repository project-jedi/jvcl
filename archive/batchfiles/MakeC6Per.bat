@echo off
: ---------------------------------------------------------------------
:    WARNING   WARNING    WARNING    WARNING    WARNING    WARNING
:
: Please read the help before using this batch file as there are
: some special parameters
:
: ---------------------------------------------------------------------
SET BCBDIR=%1
if %BCBDIR%!==! SET BCBDIR=C:\Program Files\CBuilder6

MakeBCB "C6Per Packages" c6Per "%BCBDIR%" %2 %3
del /f /q c6\*.cfg

SET BCBDIR=

goto end

:help
echo MakeC6Per.bat - Builds the JVCL for C++ Builder 6 Personal Edition
echo.
echo Usage:    MakeBCB6Per [BCBDirectory] [LIBDirectory] [BPLDirectory]
echo.
echo     BCBDirectory   The place where BCB6 is installed.
echo                    Defaults to "C:\Program Files\CBuilder6"
echo     LIBDirectory   The place where to put the BPI and LIB files.
echo                    Defaults to $(BCB)\Projects\Lib
echo                    You MUST ensure that this directory is in the
echo                    PATH environment variable
echo     BPLDirectory   The place where to put the BPL and TDS files.
echo                    Defaults to $(BCB)\Projects\Bpl
echo.
echo Any additional argument will be ignored, BCB MUST be closed
echo.
:end

