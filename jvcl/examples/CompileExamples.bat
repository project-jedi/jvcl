:start
@echo off
cls
echo. Change the JVCLMAKE line in this batch file to use the make program of
echo. your choice or from a specific Delphi/BCB version (for example
echo. C:\Program Files\Borland\Delphi5\bin\make.exe
echo. to use the Delphi 5 make).
echo. Defaults to: make.exe (whatever that is on your system), always build.
echo.
rem pause
SET JVCLMAKE=make.exe -B -i -f 
echo. Making all examples (get some coffee, this takes a while!):
echo.
%JVCLMAKE% makefile.mak 
