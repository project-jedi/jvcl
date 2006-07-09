@echo off
if "%1" == "" goto NEWEST

install.bat %1 --autoupdate %2 %3 %4 %5 %6 %7 %8 %9
goto LEAVE

:NEWEST
install.bat newest --autoupdate %2 %3 %4 %5 %6 %7 %8 %9

:LEAVE
