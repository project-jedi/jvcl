//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvUIBC5D.res");
USEUNIT("..\..\design\JvUIBReg.pas");
USEPACKAGE("JvUIBC5R.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("JvCoreC5R.bpi");

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

