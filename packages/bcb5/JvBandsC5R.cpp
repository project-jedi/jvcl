//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvBandsC5R.res");
USEUNIT("..\..\run\JvBandObject.pas");
USEUNIT("..\..\run\JvBandForms.pas");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvCoreC5R.bpi");
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

