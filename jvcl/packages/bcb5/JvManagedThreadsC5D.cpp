//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvManagedThreadsC5D.res");
USEUNIT("..\..\design\JvManagedThreadsReg.pas");
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("JvManagedThreadsC5R.bpi");

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

