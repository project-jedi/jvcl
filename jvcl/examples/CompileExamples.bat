:start
@echo off
cls
echo. Change the JVCLMAKE line in this batch file to use the make program of your choice 
echo. or from a specific Delphi/BCB version (f ex C:\Program Files\Borland\Delphi5\bin\make.exe 
echo. to use the Delphi 5 make).
echo. Defaults to: make.exe (whatever that is on your system), always build
pause
SET JVCLMAKE=make.exe -B -f
echo. Making all examples (get some coffeee, this takes a while!):
%JVCLMAKE% makefile.mak
