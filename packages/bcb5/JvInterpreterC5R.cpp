//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEPACKAGE("");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("db");
USEPACKAGE("bde");
USEPACKAGE("qrpt.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("inet.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("JvSystemC5R.bpi");
USEPACKAGE("JvCtrlsC5R.bpi");
USEPACKAGE("JvCustomC5R.bpi");
USEPACKAGE("JvStdCtrlsC5R.bpi");
//---------------------------------------------------------------------------
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
 
