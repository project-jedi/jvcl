//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("rai35.res");
USEUNIT("JvInterpreterConst.pas");
USEUNIT("JvInterpreter.pas");
USEUNIT("JvInterpreterFm.pas");
USEUNIT("JvInterpreterParser.pas");
USEPACKAGE("VCL35.bpi");
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
