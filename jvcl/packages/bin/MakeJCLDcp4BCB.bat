@echo off

if %1!==! goto help

build.exe --make=BuildJCLdcpFilesForce %1 %2 %3 %4 %5 %6 %7 %8 %9
goto end
:help
echo MakeJCLDcp4BCB.bat - Builds the JCL dcp files for BCB
echo.
echo Usage:    MakeJCLDcp4BCB Version [--jcl-path=JCLDirectory]
echo                          [--bpl-path=DCPDirectory]
echo.
echo     Version           The version of BCB to build for (c5 or c6)
echo     JCLDirectory      The place where the JCL is installed.
echo                       Defaults to ..\..\JCL
echo                       You must enclose the value AND the option
echo                       name in double quotes (")
echo     DCPDirectory      The place where to put the DCP file.
echo                       Defaults to $(BCB)\Projects\Bpl
echo                       You must enclose the value AND the option
echo                       name in double quotes (")
echo.

:end
