//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

USERES("JvCryptC5D.res");
USEUNIT("..\..\design\JvCryptReg.pas");
USEPACKAGE("JvCryptC5R.bpi");
USEPACKAGE("vcl50.bpi");

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

