@echo off
cd ..
make pg.exe
cd bin
pg.exe "%1" "%2" "%3" "%4" "%5" "%6" "%7%" "%8" "%9"
