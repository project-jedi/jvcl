@echo off

REM --------------------------------------------------------------------------
REM Available environment variables:
REM
REM TARGETS: compile only the specified packages
REM   SET TARGETS=JvCoreD7R.bpl JvCoreD7D.bpl
REM
REM JCLROOT: root directory of the JCL
REM   SET JCLROOT=C:\folder\jedi\jcl
REM
REM HPPDIR: directory where the .hpp files for BCB should go
REM   SET HPPDIR=C:\folder\CBuilder6\Include\Vcl
REM
REM --------------------------------------------------------------------------

if EXIST xml cd bin


if "%1" == "all" goto ALL

buildtarget.exe -MAKE -s %1 %2 %3 %4 %5 %6 %7 %8 %9
goto END

:ALL
echo.

echo ################################ BCB 5 #########################################
buildtarget.exe c5 -MAKE -s %2 %3 %4
if ERRORLEVEL 1 pause
echo.

echo ################################ BCB 6 #########################################
buildtarget.exe c6 -MAKE -s %2 %3 %4
if ERRORLEVEL 1 pause
echo.

echo ############################### Delphi 5 #######################################
buildtarget.exe d5 -MAKE -s %2 %3 %4
if ERRORLEVEL 1 pause
echo.

echo ############################### Delphi 6 #######################################
buildtarget.exe d6 -MAKE -s %2 %3 %4
if ERRORLEVEL 1 pause
echo.

echo ############################### Delphi 7 #######################################
buildtarget.exe d7 -MAKE -s %2 %3 %4
if ERRORLEVEL 1 pause

echo ############################# Delphi 7 CLX #####################################
buildtarget.exe d7clx -MAKE -s %2 %3 %4
if ERRORLEVEL 1 pause

:END
