@echo off

SET OUTDIR=..\..\..\run

cd src


SET FILE=.\build
if NOT "-%1" == "-" SET FILE=%1
echo Preprocessing template: %FILE%.pas
dpp.exe .\%FILE%.pas -I..\..\..\common >NUL

if "%FILE%" == ".\build" GOTO ALL

move %FILE%.i.pas %OUTDIR%\%FILE%.pas


goto LEAVE
:ALL

move JvExButtons.i.pas %OUTDIR%\JvExButtons.pas
move JvExCheckLst.i.pas %OUTDIR%\JvExCheckLst.pas
move JvExComCtrls.i.pas %OUTDIR%\JvExComCtrls.pas
move JvExControls.i.pas %OUTDIR%\JvExControls.pas
REM move JvExDBCtrls.i.pas %OUTDIR%\JvExDBCtrls.pas
move JvExDBGrids.i.pas %OUTDIR%\JvExDBGrids.pas
move JvExExtCtrls.i.pas %OUTDIR%\JvExExtCtrls.pas
move JvExForms.i.pas %OUTDIR%\JvExForms.pas
move JvExGrids.i.pas %OUTDIR%\JvExGrids.pas
move JvExMask.i.pas %OUTDIR%\JvExMask.pas
move JvExStdCtrls.i.pas %OUTDIR%\JvExStdCtrls.pas

cd ..

:LEAVE
SET FILE=
SET OUTDIR=
