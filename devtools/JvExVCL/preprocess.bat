echo off
cd src
dpp.exe .\build.pas -I..\..\..\common

SET OUTDIR=..

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
move JvExComboEdits.i.pas %OUTDIR%\JvExComboEdits.pas

cd ..

jpp -c -dVCL -uVisualCLX -x..\..\run\ JvExButtons.pas
jpp -c -dVCL -uVisualCLX -x..\..\run\ JvExCheckLst.pas
jpp -c -dVCL -uVisualCLX -x..\..\run\ JvExComCtrls.pas
jpp -c -dVCL -uVisualCLX -x..\..\run\ JvExControls.pas
jpp -c -dVCL -uVisualCLX -x..\..\run\ JvExDBGrids.pas
jpp -c -dVCL -uVisualCLX -x..\..\run\ JvExExtCtrls.pas
jpp -c -dVCL -uVisualCLX -x..\..\run\ JvExForms.pas
jpp -c -dVCL -uVisualCLX -x..\..\run\ JvExGrids.pas
jpp -c -dVCL -uVisualCLX -x..\..\run\ JvExMask.pas
jpp -c -dVCL -uVisualCLX -x..\..\run\ JvExStdCtrls.pas