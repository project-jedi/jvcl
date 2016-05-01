@echo off

for %%f in (*.rc) do call :process %%f

goto :eof

:process
set FILENAME=%~n1

if %FILENAME% equ template goto :eof

brcc32 -fo%FILENAME%.res %FILENAME%.rc 

goto :eof 