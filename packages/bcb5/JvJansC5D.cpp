//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvJansC5D.res");
USEUNIT("..\..\design\JvJansReg.pas");
USEUNIT("..\..\design\JvCsvBaseEditor.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("JvJansC5R.bpi");
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("vclsmp50.bpi");

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

