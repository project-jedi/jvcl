//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\design\JvBandObjectDLLWizardForm.pas", Jvbandobjectdllwizardform, zWizardForm);
USEPACKAGE("");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("JvBandsC5R.bpi");
USEPACKAGE("CJCL50.bpi");
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
