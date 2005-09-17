@echo off

SET DELPHIVERSION=%1
if "%1" == "" SET DELPHIVERSION=newest

cd packages\bin
SET INSTALLOPTIONS="%2" "%3" "%4" "%5" "%6" "%7" "%8" "%9"
call pretest.bat
build.exe %DELPHIVERSION% "--make=installer"
cd ..\..
SET INSTALLOPTIONS=
