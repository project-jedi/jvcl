//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvTimeFrameworkC5R.res");
USEUNIT("..\..\run\JvTFWeeks.pas");
USEUNIT("..\..\run\JvTFGantt.pas");
USEUNIT("..\..\run\JvTFAlarm.pas");
USEUNIT("..\..\run\JvTFDays.pas");
USEUNIT("..\..\run\JvTFGlance.pas");
USEUNIT("..\..\run\JvTFGlanceTextViewer.pas");
USEUNIT("..\..\run\JvTFManager.pas");
USEUNIT("..\..\run\JvTFMonths.pas");
USEUNIT("..\..\run\JvTFSparseMatrix.pas");
USEUNIT("..\..\run\JvTFUtils.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("bcbsmp50.bpi");
USEPACKAGE("bcbie50.bpi");

#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------

