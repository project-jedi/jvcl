//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvPluginC5R.res");
USEUNIT("..\..\run\JvPluginManager.pas");
USEUNIT("..\..\run\JvPlugin.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("JvCoreC5R.bpi");
USEPACKAGE("CJCL50.bpi");

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

