@echo off
cd packages\bin
SET INSTALLOPTIONS=--autoupdate "%1" "%2" "%3" "%4" "%5" "%6" "%7" "%8" "%9"
build.exe newest "--make=installer_nomo"
cd ..\..
