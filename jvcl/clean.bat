@echo off
echo erasing...
del /S *.dcu *.ddp *.dsk *.~* *.cfg *.drc %1 %2 %3 %4 %5 %6 %7 %8 %9 >NUL
del dcu\*.obj >NUL
if NOT "%1" == "all" goto LEAVE
del lib5\*.dcp lib5\*.bpl >NUL
del lib6\*.dcp lib6\*.bpl >NUL
del lib7\*.dcp lib7\*.bpl >NUL
:LEAVE
