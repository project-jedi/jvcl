@echo off
REM Change the line below to use the make program of your choice or from a specific Delphi/BCB version
REM (f ex C:\Program Files\Borland\Delphi5\bin\make.exe to use the Delphi 5 make)
REM Defaults to: make.exe (whatever that is on your system), always build
SET MYMAKE=make.exe -B -f
REM Make all examples:
cls
%MYMAKE% makefile.mak

