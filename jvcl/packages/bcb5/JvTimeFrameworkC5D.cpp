//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvTimeFrameworkC5D.res");
USEUNIT("..\..\design\JvTimeFrameworkReg.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("JvTimeFrameworkC5R.bpi");

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

