@echo off
REM Change the line to use the make program of your choice or from a specific Delphi/BCB version
REM f ex C:\Program Files\Borland\Delphi5\bin\make.exe to use the Delphi 5 make program etc
REM Defaults to: make.exe (whatever that is on your system), always build, silent, keep temp files, cache dependency info
SET MYMAKE=make.exe -B -K -a -c -s -f
REM Make all examples:
%MYMAKE% makefile.mak

