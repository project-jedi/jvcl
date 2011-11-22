:start
@echo off
cls
echo. Call with the version you want to use as the first parameter
echo. If no version is given, the highest is used, just like with install.bat
echo.
rem pause
SET JVCLMAKE=make.exe -B -i -f
set DELPHIVERSION=%1 
echo. Making all examples (get some coffee, this takes a while!):
echo.
%JVCLMAKE% makefile.mak 
