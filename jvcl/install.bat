@echo off

cd install\JVCL3Install
dcc32 -$O+ -Q JVCL3Install.dpr -E..\..\packages
cd ..\..

if NOT EXIST packages\JVCL3Install.exe goto error

start packages\JVCL3Install.exe

goto end


:error
echo.
echo Error: JVCL 3 Package Installer could not be compiled.
echo.
pause

:end