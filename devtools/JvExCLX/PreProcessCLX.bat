@echo off
cd src
..\..\JvExVCL\src\dpp.exe .\qbuild.pas -I..\..\..\common;.

SET OUTDIR=..\..\..\qrun

move JvQExButtons.i.pas %OUTDIR%\JvQExButtons.pas
move JvQExCheckLst.i.pas %OUTDIR%\JvQExCheckLst.pas
move JvQExComboEdits.i.pas %OUTDIR%\JvQExComboEdits.pas
move JvQExComCtrls.i.pas %OUTDIR%\JvQExComCtrls.pas
move JvQExControls.i.pas %OUTDIR%\JvQExControls.pas
move JvQExDBGrids.i.pas %OUTDIR%\JvQExDBGrids.pas
move JvQExExtCtrls.i.pas %OUTDIR%\JvQExExtCtrls.pas
move JvQExForms.i.pas %OUTDIR%\JvQExForms.pas
move JvQExGrids.i.pas %OUTDIR%\JvQExGrids.pas
move JvQExMask.i.pas %OUTDIR%\JvQExMask.pas
move JvQExStdCtrls.i.pas %OUTDIR%\JvQExStdCtrls.pas

cd ..

