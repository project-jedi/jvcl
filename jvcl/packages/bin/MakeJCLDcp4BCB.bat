@echo off

if !%JCLROOT%==! SET JCLROOT=..\..\..\jcl
del "%JCLROOT%\packages\error.log" 2>NUL

echo ################################ BCB 5 #########################################
build.exe c5 --make=BuildJCLdcpFilesForce "%1" "%2" "%3" "%4" "%5" "%6" "%7" "%8" "%9"
if NOT EXIST "%JCLROOT%\packages\error.log" goto NextC6
type "%JCLROOT%\packages\error.log"
del "%JCLROOT%\packages\error.log"

:NextC6
echo.
echo ################################ BCB 6 #########################################
build.exe c6 --make=BuildJCLdcpFilesForce "%1" "%2" "%3" "%4" "%5" "%6" "%7" "%8" "%9"
if NOT EXIST "%JCLROOT%\packages\error.log" goto END
type "%JCLROOT%\packages\error.log"
del "%JCLROOT%\packages\error.log"


:END
echo.
