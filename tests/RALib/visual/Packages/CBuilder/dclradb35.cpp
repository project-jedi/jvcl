//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("dclradb35.res");
USEPACKAGE("vcl35.bpi");
USEPACKAGE("vcldb35.bpi");
USEUNIT("JvDBReg.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
    return 1;
}
//---------------------------------------------------------------------------
