//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEPACKAGE("Vcl50.bpi");
USEPACKAGE("Vcldb50.bpi");
USEPACKAGE("VCLBDE50.bpi");
USEUNIT("JvDBReg.pas");
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
