@echo off

SETLOCAL
pushd "%~dp0"

echo erasing...
del /S *.dcu *.ddp *.dsk *.~* *.cfg *.drc *.dsm *.local *.identcache %1 %2 %3 %4 %5 %6 %7 %8 %9 >NUL
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

del /S lib\d12\*.dcp lib\d12\*.bpl lib\d12\*.dfm lib\d12\*.obj lib\d12\*.bpi lib\d12\*.lib >NUL
del /S lib\d12\debug\*.dcp lib\d12\debug\*.bpl lib\d12\debug\*.dfm lib\d12\debug\*.obj lib\d12\debug\*.bpi lib\d12\debug\*.lib >NUL

del /S lib\d14\*.dcp lib\d14\*.bpl lib\d14\*.dfm lib\d14\*.obj lib\d14\*.bpi lib\d14\*.lib >NUL
del /S lib\d14\debug\*.dcp lib\d14\debug\*.bpl lib\d14\debug\*.dfm lib\d14\debug\*.obj lib\d14\debug\*.bpi lib\d14\debug\*.lib >NUL

del /S lib\d15\*.dcp lib\d15\*.bpl lib\d15\*.dfm lib\d15\*.obj lib\d15\*.bpi lib\d15\*.lib >NUL
del /S lib\d15\debug\*.dcp lib\d15\debug\*.bpl lib\d15\debug\*.dfm lib\d15\debug\*.obj lib\d15\debug\*.bpi lib\d15\debug\*.lib >NUL

del /S lib\d16\win32\*.dcp lib\d16\win32\*.bpl lib\d16\win32\*.dfm lib\d16\win32\*.obj lib\d16\win32\*.bpi lib\d16\win32\*.lib >NUL
del /S lib\d16\win32\debug\*.dcp lib\d16\win32\debug\*.bpl lib\d16\win32\debug\*.dfm lib\d16\win32\debug\*.obj lib\d16\win32\debug\*.bpi lib\d16\win32\debug\*.lib >NUL

del /S lib\d16\win64\*.dcp lib\d16\win64\*.bpl lib\d16\win64\*.dfm lib\d16\win64\*.obj lib\d16\win64\*.bpi lib\d16\win64\*.lib >NUL
del /S lib\d16\win64\debug\*.dcp lib\d16\win64\debug\*.bpl lib\d16\win64\debug\*.dfm lib\d16\win64\debug\*.obj lib\d16\win64\debug\*.bpi lib\d16\win64\debug\*.lib >NUL

del /S lib\d17\win32\*.dcp lib\d17\win32\*.bpl lib\d17\win32\*.dfm lib\d17\win32\*.obj lib\d17\win32\*.bpi lib\d17\win32\*.lib >NUL
del /S lib\d17\win32\debug\*.dcp lib\d17\win32\debug\*.bpl lib\d17\win32\debug\*.dfm lib\d17\win32\debug\*.obj lib\d17\win32\debug\*.bpi lib\d17\win32\debug\*.lib >NUL

del /S lib\d17\win64\*.dcp lib\d17\win64\*.bpl lib\d17\win64\*.dfm lib\d17\win64\*.obj lib\d17\win64\*.bpi lib\d17\win64\*.lib >NUL
del /S lib\d17\win64\debug\*.dcp lib\d17\win64\debug\*.bpl lib\d17\win64\debug\*.dfm lib\d17\win64\debug\*.obj lib\d17\win64\debug\*.bpi lib\d17\win64\debug\*.lib >NUL

del /S lib\d18\win32\*.dcp lib\d18\win32\*.bpl lib\d18\win32\*.dfm lib\d18\win32\*.obj lib\d18\win32\*.bpi lib\d18\win32\*.lib >NUL
del /S lib\d18\win32\debug\*.dcp lib\d18\win32\debug\*.bpl lib\d18\win32\debug\*.dfm lib\d18\win32\debug\*.obj lib\d18\win32\debug\*.bpi lib\d18\win32\debug\*.lib >NUL

del /S lib\d18\win64\*.dcp lib\d18\win64\*.bpl lib\d18\win64\*.dfm lib\d18\win64\*.obj lib\d18\win64\*.bpi lib\d18\win64\*.lib >NUL
del /S lib\d18\win64\debug\*.dcp lib\d18\win64\debug\*.bpl lib\d18\win64\debug\*.dfm lib\d18\win64\debug\*.obj lib\d18\win64\debug\*.bpi lib\d18\win64\debug\*.lib >NUL

del /S lib\d19\win32\*.dcp lib\d19\win32\*.bpl lib\d19\win32\*.dfm lib\d19\win32\*.obj lib\d19\win32\*.bpi lib\d19\win32\*.lib >NUL
del /S lib\d19\win32\debug\*.dcp lib\d19\win32\debug\*.bpl lib\d19\win32\debug\*.dfm lib\d19\win32\debug\*.obj lib\d19\win32\debug\*.bpi lib\d19\win32\debug\*.lib >NUL

del /S lib\d19\win64\*.dcp lib\d19\win64\*.bpl lib\d19\win64\*.dfm lib\d19\win64\*.obj lib\d19\win64\*.bpi lib\d19\win64\*.lib >NUL
del /S lib\d19\win64\debug\*.dcp lib\d19\win64\debug\*.bpl lib\d19\win64\debug\*.dfm lib\d19\win64\debug\*.obj lib\d19\win64\debug\*.bpi lib\d19\win64\debug\*.lib >NUL

del /S lib\d20\win32\*.dcp lib\d20\win32\*.bpl lib\d20\win32\*.dfm lib\d20\win32\*.obj lib\d20\win32\*.bpi lib\d20\win32\*.lib >NUL
del /S lib\d20\win32\debug\*.dcp lib\d20\win32\debug\*.bpl lib\d20\win32\debug\*.dfm lib\d20\win32\debug\*.obj lib\d20\win32\debug\*.bpi lib\d20\win32\debug\*.lib >NUL

del /S lib\d20\win64\*.dcp lib\d20\win64\*.bpl lib\d20\win64\*.dfm lib\d20\win64\*.obj lib\d20\win64\*.bpi lib\d20\win64\*.lib >NUL
del /S lib\d20\win64\debug\*.dcp lib\d20\win64\debug\*.bpl lib\d20\win64\debug\*.dfm lib\d20\win64\debug\*.obj lib\d20\win64\debug\*.bpi lib\d20\win64\debug\*.lib >NUL

del /S lib\d21\win32\*.dcp lib\d21\win32\*.bpl lib\d21\win32\*.dfm lib\d21\win32\*.obj lib\d21\win32\*.bpi lib\d21\win32\*.lib >NUL
del /S lib\d21\win32\debug\*.dcp lib\d21\win32\debug\*.bpl lib\d21\win32\debug\*.dfm lib\d21\win32\debug\*.obj lib\d21\win32\debug\*.bpi lib\d21\win32\debug\*.lib >NUL

del /S lib\d21\win64\*.dcp lib\d21\win64\*.bpl lib\d21\win64\*.dfm lib\d21\win64\*.obj lib\d21\win64\*.bpi lib\d21\win64\*.lib >NUL
del /S lib\d21\win64\debug\*.dcp lib\d21\win64\debug\*.bpl lib\d21\win64\debug\*.dfm lib\d21\win64\debug\*.obj lib\d21\win64\debug\*.bpi lib\d21\win64\debug\*.lib >NUL

del /S lib\d22\win32\*.dcp lib\d22\win32\*.bpl lib\d22\win32\*.dfm lib\d22\win32\*.obj lib\d22\win32\*.bpi lib\d22\win32\*.lib >NUL
del /S lib\d22\win32\debug\*.dcp lib\d22\win32\debug\*.bpl lib\d22\win32\debug\*.dfm lib\d22\win32\debug\*.obj lib\d22\win32\debug\*.bpi lib\d22\win32\debug\*.lib >NUL

del /S lib\d22\win64\*.dcp lib\d22\win64\*.bpl lib\d22\win64\*.dfm lib\d22\win64\*.obj lib\d22\win64\*.bpi lib\d22\win64\*.lib >NUL
del /S lib\d22\win64\debug\*.dcp lib\d22\win64\debug\*.bpl lib\d22\win64\debug\*.dfm lib\d22\win64\debug\*.obj lib\d22\win64\debug\*.bpi lib\d22\win64\debug\*.lib >NUL

del /S lib\d23\win64\*.dcp lib\d23\win64\*.bpl lib\d23\win64\*.dfm lib\d23\win64\*.obj lib\d23\win64\*.bpi lib\d23\win64\*.lib >NUL
del /S lib\d23\win64\debug\*.dcp lib\d23\win64\debug\*.bpl lib\d23\win64\debug\*.dfm lib\d23\win64\debug\*.obj lib\d23\win64\debug\*.bpi lib\d23\win64\debug\*.lib >NUL

del /S run\*.hpp >NUL
del /S design\*.hpp >NUL
del /S common\*.hpp >NUL
del packages\*Packages.mak >NUL

:LEAVE

popd
ENDLOCAL
