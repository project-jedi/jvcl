@echo off
echo. Copy this batch (and the jconvert and crlf programs from \devtools) to the folder *above JVCL* before running it
echo. This batch does the following:
echo. 1. Deletes all files created from compiling (dcu's, dsk's etc)
echo. 2. Converts all dfm's to text format (if necessary)
echo. 3. Converts all files from Linux format to Windows format (if necessary)
echo. Hit Ctrl+C now to quit
pause
del /s *.dcu *.ddp *.dsk
jconvert -i -t -s *.dfm
crlf -s -q *.pas *.dfm *.dpk *.dpr