@echo off
echo erasing...
del /S *.dcu *.ddp *.dsk *.~* *.cfg *.drc %1 %2 %3 %4 %5 %6 %7 %8 %9 >NUL
del dcu\*.obj >NUL
if NOT "%1" == "all" goto LEAVE
del lib\d5\*.dcp lib\d5\*.bpl >NUL
del lib\d6\*.dcp lib\d6\*.bpl >NUL
del lib\d7\*.dcp lib\d7\*.bpl >NUL
del lib\c5\obj\*.obj lib\c5\obj\*.dcu >NUL
del lib\c6\obj\*.obj lib\c6\obj\*.dcu >NUL
del packages\*Packages.mak packages\tmp.bat >/NUL
del /S packages\Make*.@@@ >NUL

: needs delete lines for k3
:LEAVE
