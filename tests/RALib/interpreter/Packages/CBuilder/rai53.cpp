//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEUNIT("JvInterpreterConst.pas");
USEUNIT("JvInterpreter.pas");
USEUNIT("JvInterpreterFm.pas");
USEUNIT("JvInterpreterParser.pas");
USEPACKAGE("Vcl50.bpi");
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
