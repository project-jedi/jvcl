@echo off
echo erasing...
del /S *.dcu *.ddp *.dsk *.~* *.cfg *.drc *.dsm %1 %2 %3 %4 %5 %6 %7 %8 %9 >NUL
del dcu\*.obj >NUL
if NOT "%1" == "all" goto LEAVE
del /S lib\d5\*.dcp lib\d5\*.bpl lib\d5\*.dfm lib\d5\*.res >NUL
del /S lib\d6\*.dcp lib\d6\*.bpl lib\d6\*.dfm lib\d6\*.res >NUL
del /S lib\d7\*.dcp lib\d7\*.bpl lib\d7\*.dfm lib\d7\*.res >NUL
del /S lib\d7clx\*.dcp lib\d7clx\*.bpl lib\d7clx\*.xfm >NUL
del /S lib\d9\*.dcp lib\d9\*.bpl lib\d9\*.dfm lib\d9\*.res >NUL
del /S lib\c5\obj\*.obj lib\c5\obj\*.dcu lib\c5\obj\*.dfm >NUL
del /S lib\c6\obj\*.obj lib\c6\obj\*.dcu lib\c6\obj\*.dfm >NUL
del /S run\*.hpp >NUL
del /S design\*.hpp >NUL
del /S common\*.hpp >NUL
del packages\*Packages.mak >NUL

:LEAVE
