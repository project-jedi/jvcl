//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("JvBandsC5D.res");
USEUNIT("..\..\design\JvBandsReg.pas");
USEUNIT("..\..\design\JvBandObjectDLLWizard.pas");
USEUNIT("..\..\design\JvBandObjectDLLWizardForm.pas");
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
