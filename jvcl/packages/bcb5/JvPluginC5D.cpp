//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvPluginC5D.res");
USEUNIT("..\..\design\JvPluginWizard.pas");
USEUNIT("..\..\design\JvPluginParamsForm.pas");
USEUNIT("..\..\design\JvPluginReg.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("JvPluginC5R.bpi");
USEPACKAGE("CJCL50.bpi");
USEPACKAGE("JvCoreC5R.bpi");
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
