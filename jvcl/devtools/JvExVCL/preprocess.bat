@echo off
cd source
dpp.exe .\build.pas

SET OUTDIR=..\..\..\run

move JvExButtons.i.pas %OUTDIR%\JvExButtons.pas
move JvExCheckLst.i.pas %OUTDIR%\JvExCheckLst.pas
move JvExComCtrls.i.pas %OUTDIR%\JvExComCtrls.pas
move JvExControls.i.pas %OUTDIR%\JvExControls.pas
REM move JvExDBCtrls.i.pas %OUTDIR%\JvExDBCtrls.pas
REM move JvExDBGrids.i.pas %OUTDIR%\JvExDBGrids.pas
move JvExExtCtrls.i.pas %OUTDIR%\JvExExtCtrls.pas
move JvExForms.i.pas %OUTDIR%\JvExForms.pas
move JvExGrids.i.pas %OUTDIR%\JvExGrids.pas
move JvExMask.i.pas %OUTDIR%\JvExMask.pas
move JvExStdCtrls.i.pas %OUTDIR%\JvExStdCtrls.pas

SET OUTDIR=

cd ..