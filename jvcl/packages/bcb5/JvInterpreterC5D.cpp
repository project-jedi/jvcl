//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEPACKAGE("");
USEPACKAGE("JvInterpreterC5R.bpi");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("dclstd50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("bde");
USEPACKAGE("qrpt.bpi");
USEPACKAGE("JvCustomC5R.bpi");
USEPACKAGE("JvCtrlsC5R.bpi");
USEPACKAGE("JvStdCtrlsC5R.bpi");
USEPACKAGE("JvSystemC5R.bpi");
USEPACKAGE("JvCoreC5D.bpi");
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
 
