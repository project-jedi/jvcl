@echo off
echo. Run this batch to clean up the JVCL before release or if you just want to get it in "mint" condition
echo. This batch does the following:
echo. 1. Strips unwanted properties from dfm's (and converts them to text)
echo. 2. Converts all remaining dfm's to text format (if necessary)
echo. 3. Converts all files from Linux LF format to Windows CRLF format (if necessary)
echo. 4. Deletes all files created from compiling (dcu's, dsk's exe's etc)
echo. 
echo. Hit Ctrl+C NOW if you want to quit, any other key to run the batch
pause
if NOT EXIST crlf.exe goto maketools
if NOT EXIST dc.exe goto maketools
if NOT EXIST jconvert.exe goto maketools
:clean
rem Fix CRLF corruption:
crlf -s -q ..\..\..\*.pas ..\..\..\*.dfm ..\..\..\*.dpk ..\..\..\*.dpr
echo.
rem Remove unwanted properties (make D5 compatible):
dc.exe -i -s -fskiplistd5.txt ..\..\..\*.dfm
echo.
rem Convert remaining DFM's to text:
jconvert -i -t -s ..\..\..\*.dfm
echo.
rem Delete garbage:
cd ..
cd ..
del /s /q *.dcu *.ddp *.dsk *.exe .#* .cvsignore >NUL
rem Go back were we started:
cd devtools\bin
goto end

:maketools
rem Build the tools we need:
cd ..
make crlf.exe dc.exe jconvert.exe
cd bin
goto clean
:end