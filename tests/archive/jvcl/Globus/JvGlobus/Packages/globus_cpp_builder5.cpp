//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("globus_cpp_builder5.res");
USERES("reggllib.dcr");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vcljpg50.bpi");
USEUNIT("reggllib_builder5.pas");
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
