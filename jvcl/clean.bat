@echo off
echo erasing...
del /S *.dcu *.ddp *.dsk *.~* *.cfg *.drc *.dsm %1 %2 %3 %4 %5 %6 %7 %8 %9 >NUL
del dcu\*.obj >NUL
if NOT "%1" == "all" goto LEAVE

del /S lib\c5\obj\*.obj lib\c5\obj\*.dcu lib\c5\obj\*.dfm >NUL
del /S lib\c5\obj\debug\*.obj lib\c5\obj\debug\*.dcu lib\c5\obj\debug\*.dfm >NUL

del /S lib\c6\obj\*.obj lib\c6\obj\*.dcu lib\c6\obj\*.dfm >NUL
del /S lib\c6\obj\debug\*.obj lib\c6\obj\debug\*.dcu lib\c6\obj\debug\*.dfm >NUL


del /S lib\d5\*.dcp lib\d5\*.bpl lib\d5\*.dfm >NUL
del /S lib\d5\debug\*.dcp lib\d5\debug\*.bpl lib\d5\debug\*.dfm >NUL

del /S lib\d6\*.dcp lib\d6\*.bpl lib\d6\*.dfm >NUL
del /S lib\d6\debug\*.dcp lib\d6\debug\*.bpl lib\d6\debug\*.dfm >NUL

del /S lib\d7\*.dcp lib\d7\*.bpl lib\d7\*.dfm >NUL
del /S lib\d7\debug\*.dcp lib\d7\debug\*.bpl lib\d7\debug\*.dfm >NUL

del /S lib\d9\*.dcp lib\d9\*.bpl lib\d9\*.dfm >NUL
del /S lib\d9\debug\*.dcp lib\d9\debug\*.bpl lib\d9\debug\*.dfm >NUL

del /S lib\d10\*.dcp lib\d10\*.bpl lib\d10\*.dfm lib\d10\*.obj lib\d10\*.bpi lib\d10\*.lib >NUL
del /S lib\d10\debug\*.dcp lib\d10\debug\*.bpl lib\d10\debug\*.dfm lib\d10\debug\*.obj lib\d10\debug\*.bpi lib\d10\debug\*.lib >NUL

del /S lib\d11\*.dcp lib\d11\*.bpl lib\d11\*.dfm lib\d11\*.obj lib\d11\*.bpi lib\d11\*.lib >NUL
del /S lib\d11\debug\*.dcp lib\d11\debug\*.bpl lib\d11\debug\*.dfm lib\d11\debug\*.obj lib\d11\debug\*.bpi lib\d11\debug\*.lib >NUL

del /S run\*.hpp >NUL
del /S design\*.hpp >NUL
del /S common\*.hpp >NUL
del packages\*Packages.mak >NUL

:LEAVE
