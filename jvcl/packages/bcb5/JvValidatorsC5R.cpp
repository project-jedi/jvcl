//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvValidatorsC5R.res");
USEUNIT("..\..\run\JvValidators.pas");
USEUNIT("..\..\run\JvErrorIndicator.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("CJCL50.bpi");
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

