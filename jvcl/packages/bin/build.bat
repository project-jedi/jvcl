@echo off
if EXIST xml   cd bin
buildtarget.exe "--make=-s" %1 %2 %3 %4 %5 %6 %7 %8 %9
