@echo off
echo erasing...
del /S *.dcu *.ddp *.dsk *.~* *.cfg *.drc %1 %2 %3 %4 %5 %6 %7 %8 %9 >NUL
del dcu\*.obj >NUL
if NOT "%1" == "all" goto LEAVE
del lib\d5\*.dcp lib\d5\*.bpl >NUL
del lib\d6\*.dcp lib\d6\*.bpl >NUL
del lib\d7\*.dcp lib\d7\*.bpl >NUL
# (rom) needs delete lines for c5, c6 and k3
:LEAVE
