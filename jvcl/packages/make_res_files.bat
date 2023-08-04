@echo off
setlocal

for %%f in (%1\*.rc) do call :make_one %%f

goto :eof

:make_one

call set filename=%~n1
call set filepath=%~p1

if "%filename%" equ "template" goto :eof 

brcc32 %filepath%%filename%.rc -fo%filepath%%filename%.res 
goto :eof