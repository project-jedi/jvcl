@echo off
echo erasing...
del /S *.dcu *.ddp *.dsk *.~* *.cfg *.drc %1 %2 %3 %4 %5 %6 %7 %8 %9 >NUL
del dcu\*.obj >NUL
if NOT "%1" == "all" goto LEAVE
del lib\d5\*.dcp lib\d5\*.bpl lib\d5\*.dfm >NUL
del lib\d6\*.dcp lib\d6\*.bpl lib\d6\*.dfm >NUL
del lib\d7\*.dcp lib\d7\*.bpl lib\d7\*.dfm >NUL
del lib\d7clx\*.dcp lib\d7clx\*.bpl lib\d7clx\*.xfm >NUL
del lib\c5\obj\*.obj lib\c5\obj\*.dcu lib\c5\obj\*.dfm >NUL
del lib\c6\obj\*.obj lib\c6\obj\*.dcu lib\c6\obj\*.dfm >NUL
del /S *.hpp >NUL
del packages\*Packages.mak >NUL

:LEAVE
