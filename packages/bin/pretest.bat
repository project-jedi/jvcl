@echo off

if EXIST build.exe GOTO FOUND

dcc32.exe -$D- -Q -I..\..\common build.dpr >NUL
if ERRORLEVEL 1 GOTO FAILED

rem ======= COMPILED =======
echo build.exe compiled. Pretest: ok

goto LEAVE

:FAILED
rem ======= FAILED =======
echo.
echo Delphi Compiler for Win32 (dpp32.exe) not found. Please add the 
echo Delphi\Bin directory to the PATH environment variable.
echo.
echo You can do this by executing 'SET PATH="C:\Program Files\Borland\Delphi7\Bin"'
echo (Adjust the directories to your installation path)
echo.


goto LEAVE

:FOUND
rem ======= FOUND =======
echo build.exe found. Pretest: ok

:LEAVE
